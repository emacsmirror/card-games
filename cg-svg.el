;;; cg-svg.el --- SVG card drawing for card games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.60
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games
;; URL: https://code.bru.st/corwin/card-game.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reusable SVG drawing for the graphical (GUI) renderers, shared by the
;; games in this package.  Console rendering remains the baseline; these
;; helpers add a prettier display when `display-graphic-p' is non-nil.
;;
;; Faces are drawn the way real cards are: a stacked rank/suit index in
;; the top-left corner (mirrored, upside-down, in the bottom-right),
;; canonical pip layouts for the number cards (with the lower pips
;; rotated 180 degrees), a large central pip for the ace, framed letters
;; for the court cards, and a distinct joker.  Backs show a dotted
;; medallion; cards cast a soft shadow; the cursor card gets a glowing
;; ring.
;;
;; The primitives are game-agnostic.  A card to draw is a "spec":
;;
;;   (RANK-STRING . SUIT)  face-up; SUIT is 0-3 or the symbol `joker';
;;                         RANK-STRING is the caller's label ("A" "10"
;;                         "K" ...).
;;   `down'                a face-down card.
;;   nil                   an empty slot / gap.
;;
;; `cg-svg-cards-svg' lays specs out in a row; `cg-svg-grid-svg' lays
;; rows out as a grid.  Both return an svg object; wrap with
;; `cg-svg-image' to insert or `cg-svg-to-string' to serialize.

;;; Code:

(require 'svg)
(require 'cg-core)

(defgroup cg-svg nil
  "SVG rendering for card games."
  :group 'card-games
  :prefix "cg-svg-")

(defcustom cg-svg-card-width 56
  "Card width in pixels."
  :type 'integer :group 'cg-svg)

(defcustom cg-svg-card-height 80
  "Card height in pixels."
  :type 'integer :group 'cg-svg)

(defcustom cg-svg-card-gap 8
  "Pixels between adjacent cards."
  :type 'integer :group 'cg-svg)

(defcustom cg-svg-card-shadow t
  "When non-nil, draw a soft drop shadow under each card."
  :type 'boolean :group 'cg-svg)

(defcustom cg-svg-font-family "Helvetica, Arial, sans-serif"
  "Font family used for ranks, pips and indices."
  :type 'string :group 'cg-svg)

(defcustom cg-svg-theme-colors t
  "Derive the highlight ring and card back from the active theme.
When non-nil, colours are taken from the active Emacs theme, with the
colour variables below as fallbacks."
  :type 'boolean :group 'cg-svg)

(defcustom cg-svg-card-back 'dots
  "Pattern drawn on a face-down card back."
  :type '(choice (const dots) (const rings) (const solid)) :group 'cg-svg)

(defcustom cg-svg-four-color nil
  "Use a four-colour deck when non-nil.
Clubs are drawn green and diamonds blue-purple; spades stay black and
hearts red."
  :type 'boolean :group 'cg-svg)

(defconst cg-svg-corner-radius 6
  "Corner radius of a drawn card.")

(defvar cg-svg-red-color   "#c0392b" "Colour for red suits.")
(defvar cg-svg-black-color "#2c3e50" "Colour for black suits.")
(defvar cg-svg-club-color "#1a8a3c" "Clubs colour in a four-colour deck.")
(defvar cg-svg-diamond-color "#3b3fb0" "Diamonds colour in a four-colour deck.")
(defvar cg-svg-joker-color "#8e44ad" "Colour for the Joker.")
(defvar cg-svg-face-color  "#fdfdfb" "Card face fill.")
(defvar cg-svg-court-color "#f6f2e8" "Court-card inner panel fill.")
(defvar cg-svg-border-color "#566573" "Card border colour.")
(defvar cg-svg-back-color  "#27496d" "Card back fill.")
(defvar cg-svg-back-trim   "#9fb3cf" "Card back inner trim/dots.")
(defvar cg-svg-highlight-color "#f1c40f" "Cursor/selection highlight.")
(defvar cg-svg-gap-color   "#95a5a6" "Empty-slot outline colour.")

(defun cg-svg--highlight ()
  "Resolve the cursor/selection ring colour (theme-aware)."
  (if cg-svg-theme-colors
      (cg-color 'region :background cg-svg-highlight-color)
    cg-svg-highlight-color))

(defun cg-svg--back-fill ()
  "Resolve the card-back fill colour (theme-aware)."
  (if cg-svg-theme-colors
      (cg-color 'mode-line :background cg-svg-back-color)
    cg-svg-back-color))

(defun cg-svg--court-fill (suit)
  "Return a faint suit-tinted fill for a court card of SUIT."
  (cond ((eq suit 'joker) "#f3eafa")
        ((cg-red-suit-p suit) "#fbeceb")
        (t "#eef2f6")))

(defvar cg-svg-hint-color "#27ae60" "Colour ringing a valid move target.")

(defun cg-svg--hint ()
  "Resolve the valid-move hint colour (theme-aware)."
  (if cg-svg-theme-colors
      (cg-color 'success :foreground cg-svg-hint-color)
    cg-svg-hint-color))

(defconst cg-svg--pip-layout
  '((1  (0.5 . 0.50))
    (2  (0.5 . 0.16) (0.5 . 0.84))
    (3  (0.5 . 0.16) (0.5 . 0.50) (0.5 . 0.84))
    (4  (0.30 . 0.16) (0.70 . 0.16) (0.30 . 0.84) (0.70 . 0.84))
    (5  (0.30 . 0.16) (0.70 . 0.16) (0.5 . 0.50) (0.30 . 0.84) (0.70 . 0.84))
    (6  (0.30 . 0.16) (0.70 . 0.16) (0.30 . 0.50) (0.70 . 0.50)
        (0.30 . 0.84) (0.70 . 0.84))
    (7  (0.30 . 0.16) (0.70 . 0.16) (0.5 . 0.33) (0.30 . 0.50) (0.70 . 0.50)
        (0.30 . 0.84) (0.70 . 0.84))
    (8  (0.30 . 0.16) (0.70 . 0.16) (0.5 . 0.33) (0.30 . 0.50) (0.70 . 0.50)
        (0.5 . 0.67) (0.30 . 0.84) (0.70 . 0.84))
    (9  (0.30 . 0.14) (0.70 . 0.14) (0.30 . 0.38) (0.70 . 0.38) (0.5 . 0.50)
        (0.30 . 0.62) (0.70 . 0.62) (0.30 . 0.86) (0.70 . 0.86))
    (10 (0.30 . 0.14) (0.70 . 0.14) (0.5 . 0.26) (0.30 . 0.38) (0.70 . 0.38)
        (0.30 . 0.62) (0.70 . 0.62) (0.5 . 0.74) (0.30 . 0.86) (0.70 . 0.86)))
  "Canonical pip positions per rank (fractions of the inner card area).
Pips with a Y fraction above 0.5 are drawn rotated 180 degrees.")

(defun cg-svg--suit-color (suit)
  "Return the ink colour for SUIT (0-3 or the symbol `joker')."
  (cond ((eq suit 'joker) cg-svg-joker-color)
        (cg-svg-four-color
         (pcase suit (0 cg-svg-black-color) (1 cg-svg-club-color)
                (2 cg-svg-diamond-color) (3 cg-svg-red-color)
                (_ cg-svg-black-color)))
        ((cg-red-suit-p suit) cg-svg-red-color)
        (t cg-svg-black-color)))

(defun cg-svg--suit-glyph (suit)
  "Return the glyph for SUIT (0-3 or the symbol `joker').
Defers to `cg-suit-glyph', so it honours `cg-symbols'."
  (cg-suit-glyph suit))

(defun cg-svg--text (svg str x y size color &optional bold transform)
  "Add centred text STR to SVG at X, Y with SIZE, COLOR, BOLD, TRANSFORM."
  (let ((args (list :x (round x) :y (round y) :font-size (round size)
                    :fill color :text-anchor "middle"
                    :font-family cg-svg-font-family)))
    (when bold (setq args (append args (list :font-weight "bold"))))
    (when transform (setq args (append args (list :transform transform))))
    (apply #'svg-text svg str args)))

(defun cg-svg--index (svg x y w h rank glyph color flip)
  "Draw a stacked RANK/GLYPH index in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size.  The index sits
top-left normally, and bottom-right and upside-down when FLIP is non-nil."
  (let* ((rs (max 8 (round (* h 0.18))))
         (gs (max 7 (round (* h 0.15))))
         (ix (+ x (round (* w 0.16))))
         (ry (+ y (round (* h 0.18))))
         (gy (+ ry (round (* gs 1.05))))
         (tr (and flip (format "rotate(180 %d %d)"
                               (round (+ x (/ w 2.0)))
                               (round (+ y (/ h 2.0)))))))
    (when (and rank (> (length rank) 0))
      (cg-svg--text svg rank ix ry rs color t tr))
    (cg-svg--text svg glyph ix gy gs color nil tr)))

(defun cg-svg--pip (svg px py size glyph color flip)
  "Draw a single pip GLYPH of SIZE in COLOR centred at PX, PY on SVG."
  (cg-svg--text svg glyph px (+ py (* size 0.36)) size color nil
                (and flip (format "rotate(180 %d %d)" (round px) (round py)))))

(defun cg-svg--draw-pips (svg x y w h n glyph color)
  "Lay out N pips of GLYPH in COLOR within the card at X, Y (W by H) on SVG."
  (let* ((mx (* w 0.24)) (my (* h 0.14))
         (iw (- w (* 2 mx))) (ih (- h (* 2 my)))
         (ps (max 9 (round (* h 0.155))))
         (layout (cdr (assq n cg-svg--pip-layout))))
    (dolist (pos layout)
      (cg-svg--pip svg (+ x mx (* (car pos) iw)) (+ y my (* (cdr pos) ih))
                   ps glyph color (> (cdr pos) 0.5)))))

(defun cg-svg--draw-ace (svg x y w h glyph color)
  "Draw a single large central pip (an ace) of GLYPH in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size."
  (cg-svg--pip svg (+ x (/ w 2.0)) (+ y (/ h 2.0)) (round (* h 0.42))
               glyph color nil))

(defun cg-svg--draw-court (svg x y w h rank glyph color suit)
  "Draw a framed court card (RANK letter + GLYPH) of SUIT in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size.  The inner panel
has a quarter-circle scallop cut into each corner; the scallop radius is
8.5% of the panel height (17% diameter)."
  (let* ((bw (round (* (- w (* 2 (round (* w 0.15)))) 0.67)))
         (bh (round (* (- h (* 2 (round (* h 0.16)))) 0.67)))
         (bx (round (+ x (/ (- w bw) 2.0))))
         (by (round (+ y (/ (- h bh) 2.0))))
         (rr (max 1 (round (* bh 0.085))))
         (d (format (concat "M %d %d L %d %d "
                            "A %d %d 0 0 0 %d %d L %d %d "
                            "A %d %d 0 0 0 %d %d L %d %d "
                            "A %d %d 0 0 0 %d %d L %d %d "
                            "A %d %d 0 0 0 %d %d Z")
                    (+ bx rr) by  (- (+ bx bw) rr) by
                    rr rr (+ bx bw) (+ by rr)
                    (+ bx bw) (- (+ by bh) rr)
                    rr rr (- (+ bx bw) rr) (+ by bh)
                    (+ bx rr) (+ by bh)
                    rr rr bx (- (+ by bh) rr)
                    bx (+ by rr)
                    rr rr (+ bx rr) by)))
    (svg-node svg 'path :d d :fill (cg-svg--court-fill suit)
              :stroke color :stroke-width 1)
    (cg-svg--text svg rank (+ x (/ w 2.0)) (+ y (* h 0.65)) (* h 0.282) color t)
    (cg-svg--text svg glyph (+ x (/ w 2.0)) (+ y (* h 0.82)) (* h 0.17) color)))

(defun cg-svg--draw-joker (svg x y w h color)
  "Draw the joker face in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size."
  (cg-svg--text svg "★" (+ x (/ w 2.0)) (+ y (* h 0.52)) (* h 0.40) color)
  (cg-svg--text svg "JOKER" (+ x (/ w 2.0)) (+ y (* h 0.74)) (* h 0.135) color t))

(defun cg-svg--draw-back (svg x y w h r)
  "Draw a face-down card back on SVG at X, Y (W by H, corner R).
The pattern is controlled by `cg-svg-card-back'."
  (svg-rectangle svg x y w h :rx r :ry r :fill (cg-svg--back-fill)
                 :stroke cg-svg-border-color :stroke-width 1)
  (svg-rectangle svg (+ x 4) (+ y 4) (- w 8) (- h 8) :rx 4 :fill "none"
                 :stroke cg-svg-back-trim :stroke-width 1)
  (pcase cg-svg-card-back
    ('solid nil)
    ('rings
     (svg-rectangle svg (+ x 8) (+ y 8) (- w 16) (- h 16) :rx 5 :fill "none"
                    :stroke cg-svg-back-trim :stroke-width 1)
     (svg-rectangle svg (+ x 12) (+ y 12) (- w 24) (- h 24) :rx 4 :fill "none"
                    :stroke cg-svg-back-trim :stroke-width 1))
    (_
     (let ((gy (+ y 10)))
       (while (< gy (- (+ y h) 8))
         (let ((gx (+ x 10)))
           (while (< gx (- (+ x w) 8))
             (svg-circle svg gx gy 1.1 :fill cg-svg-back-trim)
             (setq gx (+ gx 9))))
         (setq gy (+ gy 9)))))))

(defun cg-svg--draw-face (svg x y w h r rank suit)
  "Draw a face-up card (RANK of SUIT) on SVG at X, Y (W by H, corner R)."
  (svg-rectangle svg x y w h :rx r :ry r :fill cg-svg-face-color
                 :stroke cg-svg-border-color :stroke-width 1)
  (let ((color (cg-svg--suit-color suit))
        (glyph (cg-svg--suit-glyph suit)))
    (cg-svg--index svg x y w h rank glyph color nil)
    (cg-svg--index svg x y w h rank glyph color t)
    (cond
     ((eq suit 'joker) (cg-svg--draw-joker svg x y w h color))
     ((member rank '("J" "Q" "K")) (cg-svg--draw-court svg x y w h rank glyph color suit))
     ((equal rank "A") (cg-svg--draw-ace svg x y w h glyph color))
     (t (let ((n (truncate (string-to-number (or rank "0")))))
          (if (and (>= n 1) (<= n 10))
              (cg-svg--draw-pips svg x y w h n glyph color)
            (cg-svg--text svg glyph (+ x (/ w 2.0)) (+ y (* h 0.6))
                          (* h 0.40) color)))))))

(cl-defun cg-svg-card (svg x y &key rank suit down gap highlight hint)
  "Draw one card onto SVG with its top-left corner at X, Y.
With GAP draw an empty slot; with DOWN draw a face-down card;
otherwise draw a face card labelled RANK of SUIT (0-3 or `joker').
HIGHLIGHT draws a glowing cursor ring around the card."
  (let* ((w cg-svg-card-width)
         (h cg-svg-card-height)
         (r cg-svg-corner-radius))
    (when (and cg-svg-card-shadow (not gap))
      (svg-rectangle svg (+ x 2) (+ y 3) w h :rx r :ry r
                     :fill "black" :fill-opacity 0.16))
    (cond
     (gap
      (svg-rectangle svg x y w h :rx r :ry r :fill "black" :fill-opacity 0.05
                     :stroke cg-svg-gap-color :stroke-width 1.5
                     :stroke-dasharray "4,4"))
     (down (cg-svg--draw-back svg x y w h r))
     (t (cg-svg--draw-face svg x y w h r rank suit)))
    (when hint
      (svg-rectangle svg (- x 2) (- y 2) (+ w 4) (+ h 4) :rx (+ r 1)
                     :fill "none" :stroke (cg-svg--hint) :stroke-width 2
                     :stroke-dasharray "3,3"))
    (when highlight
      (let ((hl (cg-svg--highlight)))
        (svg-rectangle svg (- x 4) (- y 4) (+ w 8) (+ h 8) :rx (+ r 3)
                       :fill "none" :stroke hl :stroke-opacity 0.45 :stroke-width 6)
        (svg-rectangle svg (- x 3) (- y 3) (+ w 6) (+ h 6) :rx (+ r 2)
                       :fill "none" :stroke hl :stroke-width 2.5)))))

(defun cg-svg--draw-spec (svg x y spec highlight &optional hint)
  "Draw SPEC onto SVG at X, Y, with HIGHLIGHT and optional HINT ring.
SPEC is (RANK . SUIT), the symbol `down', or nil for a gap."
  (cond
   ((null spec)     (cg-svg-card svg x y :gap t :highlight highlight :hint hint))
   ((eq spec 'down) (cg-svg-card svg x y :down t :highlight highlight :hint hint))
   (t (cg-svg-card svg x y :rank (car spec) :suit (cdr spec)
                   :highlight highlight :hint hint))))

(cl-defun cg-svg-cards-svg (specs &key highlight (pad 10) (overlap 0))
  "Return an svg object drawing SPECS left to right.
SPECS is a list of card specs (see Commentary).  HIGHLIGHT is the
index of a card to ring.  OVERLAP fans cards by overlapping them by
that many pixels.  PAD is the margin around the row."
  (let* ((w cg-svg-card-width)
         (h cg-svg-card-height)
         (step (max 1 (- (+ w cg-svg-card-gap) overlap)))
         (n (length specs))
         (width (+ (* 2 pad) (if (> n 0) (+ (* (1- n) step) w) w)))
         (height (+ (* 2 pad) h))
         (svg (svg-create width height)))
    (let ((x pad) (i 0))
      (dolist (spec specs)
        (cg-svg--draw-spec svg x pad spec (and highlight (= i highlight)))
        (setq x (+ x step) i (1+ i))))
    svg))

(cl-defun cg-svg-grid-svg (rows &key cursor hints (pad 10))
  "Return an svg object drawing ROWS as a grid of cards.
ROWS is a list of rows, each a list of card specs.  CURSOR is (ROW . COL)
to highlight, or nil.  HINTS is a list of (ROW . COL) to ring as valid
targets.  PAD is the margin around the grid."
  (let* ((w cg-svg-card-width)
         (h cg-svg-card-height)
         (gx cg-svg-card-gap)
         (gy cg-svg-card-gap)
         (ncols (apply #'max 1 (mapcar #'length rows)))
         (nrows (max 1 (length rows)))
         (width (+ (* 2 pad) (* ncols w) (* (1- ncols) gx)))
         (height (+ (* 2 pad) (* nrows h) (* (1- nrows) gy)))
         (svg (svg-create width height))
         (r 0))
    (dolist (row rows)
      (let ((c 0)
            (y (+ pad (* r (+ h gy)))))
        (dolist (spec row)
          (cg-svg--draw-spec svg (+ pad (* c (+ w gx))) y spec
                             (and cursor (= r (car cursor)) (= c (cdr cursor)))
                             (and hints (member (cons r c) hints) t))
          (setq c (1+ c))))
      (setq r (1+ r)))
    svg))

(defun cg-svg-image (svg &optional scale)
  "Return an Emacs image for SVG, optionally enlarged by SCALE."
  (if (and scale (/= scale 1.0))
      (svg-image svg :scale scale)
    (svg-image svg)))

(defun cg-svg-to-string (svg)
  "Return the serialized XML string for SVG."
  (with-temp-buffer
    (svg-print svg)
    (buffer-string)))

(cl-defun cg-svg-hand-svg (specs &key cursor hints marks (overlap 0) (pad 8))
  "Return an svg drawing SPECS as a left-to-right hand.
CURSOR is the index to ring as the cursor; HINTS and MARKS are lists of
indices to ring as playable and as marked; OVERLAP fans the cards."
  (let* ((w cg-svg-card-width) (h cg-svg-card-height)
         (step (max 1 (- (+ w cg-svg-card-gap) overlap)))
         (n (length specs))
         (width (+ (* 2 pad) (if (> n 0) (+ (* (1- n) step) w) w)))
         (height (+ (* 2 pad) h))
         (svg (svg-create width height))
         (x pad) (i 0))
    (dolist (spec specs)
      (cg-svg--draw-spec svg x pad spec (eql i cursor) (and (memq i hints) t))
      (when (memq i marks)
        (svg-rectangle svg (- x 3) (- pad 3) (+ w 6) (+ h 6)
                       :rx 7 :fill "none" :stroke "#4a90d9" :stroke-width 3))
      (setq x (+ x step) i (1+ i)))
    svg))

(provide 'cg-svg)
;;; cg-svg.el ends here
