;;; card-games-svg.el --- SVG card drawing for card games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.92
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
;; `card-games-svg-cards-svg' lays specs out in a row; `card-games-svg-grid-svg' lays
;; rows out as a grid.  Both return an svg object; wrap with
;; `card-games-svg-image' to insert or `card-games-svg-to-string' to serialize.

;;; Code:

(require 'svg)
(require 'card-games-core)

(defgroup card-games-svg nil
  "SVG rendering for card games."
  :group 'card-games
  :prefix "card-games-svg-")

(defcustom card-games-svg-card-width 56
  "Card width in pixels."
  :type 'integer :group 'card-games-svg)

(defcustom card-games-svg-card-height 80
  "Card height in pixels."
  :type 'integer :group 'card-games-svg)

(defcustom card-games-svg-card-gap 8
  "Pixels between adjacent cards."
  :type 'integer :group 'card-games-svg)

(defcustom card-games-svg-card-shadow t
  "When non-nil, draw a soft drop shadow under each card."
  :type 'boolean :group 'card-games-svg)

(defcustom card-games-svg-font-family "Helvetica, Arial, sans-serif"
  "Font family used for ranks, pips and indices."
  :type 'string :group 'card-games-svg)

(defcustom card-games-svg-theme-colors t
  "Derive the card back and move-hint ring from the active theme.
When non-nil, those colours are taken from the active Emacs theme, with
the colour variables below as fallbacks.  The cursor/selection ring is
not themed this way -- see `card-games-svg--highlight' -- so it never picks up a
theme's `region' colour."
  :type 'boolean :group 'card-games-svg)

(defcustom card-games-svg-card-back 'random
  "Pattern drawn on a face-down card back.
The `emacs', `emacs-classic', `gnu' and `splash' backs stamp the card with
a logo that ships with Emacs.  `random' picks one of the concrete backs
for the session (reshuffle with `card-games-svg-shuffle-card-back')."
  :type '(choice (const dots) (const rings) (const solid)
                 (const lattice) (const waves) (const diamond)
                 (const emacs) (const emacs-classic) (const gnu) (const splash)
                 (const random))
  :group 'card-games-svg)

(defconst card-games-svg--card-backs
  '(dots rings solid lattice waves diamond emacs emacs-classic gnu splash)
  "Concrete card backs that `random' chooses among.")

(defvar card-games-svg--random-back nil
  "The concrete back currently chosen for the `random' setting.")

(defun card-games-svg--roll-back ()
  "Choose a fresh concrete back for `random' and return it."
  (setq card-games-svg--random-back
        (nth (random (length card-games-svg--card-backs)) card-games-svg--card-backs)))

;;;###autoload
(defun card-games-svg-shuffle-card-back ()
  "Pick a new random card back (used when `card-games-svg-card-back' is `random')."
  (interactive)
  (card-games-svg--roll-back)
  (when (called-interactively-p 'interactive)
    (message "Card back: %s" card-games-svg--random-back)))

(defun card-games-svg--effective-back ()
  "Return the concrete back to draw, resolving `random'."
  (if (eq card-games-svg-card-back 'random)
      (or card-games-svg--random-back (card-games-svg--roll-back))
    card-games-svg-card-back))

(defun card-games-svg--back-logo-name (back)
  "Map a logo card-back BACK to a `card-games-svg--logo-files' key."
  (pcase back ('emacs 'modern) ('emacs-classic 'classic)
         ('gnu 'gnu) ('splash 'splash)))

(defcustom card-games-svg-four-color nil
  "Use a four-colour deck when non-nil.
Clubs are drawn green and diamonds blue-purple; spades stay black and
hearts red."
  :type 'boolean :group 'card-games-svg)

(defconst card-games-svg-corner-radius 6
  "Corner radius of a drawn card.")

(defvar card-games-svg-red-color   "#c0392b" "Colour for red suits.")
(defvar card-games-svg-black-color "#2c3e50" "Colour for black suits.")
(defvar card-games-svg-club-color "#166534" "Clubs colour in a four-colour deck.")
(defvar card-games-svg-diamond-color "#3b3fb0" "Diamonds colour in a four-colour deck.")
(defvar card-games-svg-joker-color "#8e44ad" "Colour for the Joker.")
(defvar card-games-svg-face-color  "#fdfdfb" "Card face fill.")
(defvar card-games-svg-court-color "#f6f2e8" "Court-card inner panel fill.")
(defvar card-games-svg-border-color "#566573" "Card border colour.")
(defvar card-games-svg-back-color  "#27496d" "Card back fill.")
(defvar card-games-svg-back-trim   "#9fb3cf" "Card back inner trim/dots.")
(defvar card-games-svg-highlight-color "#f1c40f" "Cursor/selection highlight.")
(defvar card-games-svg-gap-color   "#cbd5e1" "Empty-slot outline colour.")

(defun card-games-svg--highlight ()
  "Resolve the cursor/selection ring colour.
Always the configured `card-games-svg-highlight-color' (a calm gold by
default).  This is deliberately not taken from the theme's `region'
face: on many themes that face is a saturated colour, and a fat ring
in it reads as an alarming frame flashing around the whole board on
each redraw.  Customize `card-games-svg-highlight-color' to change it."
  card-games-svg-highlight-color)

(defun card-games-svg--back-fill ()
  "Resolve the card-back fill colour (theme-aware)."
  (if card-games-svg-theme-colors
      (card-games-color 'mode-line :background card-games-svg-back-color)
    card-games-svg-back-color))

(defun card-games-svg--court-fill (suit)
  "Return a faint suit-tinted fill for a court card of SUIT."
  (cond ((eq suit 'joker) "#f3eafa")
        ((card-games-red-suit-p suit) "#fbeceb")
        (t "#eef2f6")))

(defvar card-games-svg-hint-color "#7cf59a" "Colour ringing a valid move target.")

(defun card-games-svg--hint ()
  "Resolve the valid-move hint colour (theme-aware)."
  (if card-games-svg-theme-colors
      (card-games-color 'success :foreground card-games-svg-hint-color)
    card-games-svg-hint-color))

(defconst card-games-svg--pip-layout
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

(defun card-games-svg--suit-color (suit)
  "Return the ink colour for SUIT (0-3 or the symbol `joker')."
  (cond ((eq suit 'joker) card-games-svg-joker-color)
        (card-games-svg-four-color
         (pcase suit (0 card-games-svg-black-color) (1 card-games-svg-club-color)
                (2 card-games-svg-diamond-color) (3 card-games-svg-red-color)
                (_ card-games-svg-black-color)))
        ((card-games-red-suit-p suit) card-games-svg-red-color)
        (t card-games-svg-black-color)))

(defun card-games-svg--suit-glyph (suit)
  "Return the glyph for SUIT (0-3 or the symbol `joker').
Defers to `card-games-suit-glyph', so it honours `card-games-symbols'."
  (card-games-suit-glyph suit))

(defun card-games-svg--text (svg str x y size color &optional bold transform)
  "Add centred text STR to SVG at X, Y with SIZE, COLOR, BOLD, TRANSFORM."
  (let ((args (list :x (round x) :y (round y) :font-size (round size)
                    :fill color :text-anchor "middle"
                    :font-family card-games-svg-font-family)))
    (when bold (setq args (append args (list :font-weight "bold"))))
    (when transform (setq args (append args (list :transform transform))))
    (apply #'svg-text svg str args)))

(defun card-games-svg--index (svg x y w h rank glyph color flip)
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
      (card-games-svg--text svg rank ix ry rs color t tr))
    (card-games-svg--text svg glyph ix gy gs color nil tr)))

(defun card-games-svg--pip (svg px py size glyph color flip)
  "Draw a single pip GLYPH of SIZE in COLOR centred at PX, PY on SVG."
  (card-games-svg--text svg glyph px (+ py (* size 0.36)) size color nil
                (and flip (format "rotate(180 %d %d)" (round px) (round py)))))

(defun card-games-svg--draw-pips (svg x y w h n glyph color)
  "Lay out N pips of GLYPH in COLOR within the card at X, Y (W by H) on SVG."
  (let* ((mx (* w 0.24)) (my (* h 0.14))
         (iw (- w (* 2 mx))) (ih (- h (* 2 my)))
         (ps (max 9 (round (* h 0.155))))
         (layout (cdr (assq n card-games-svg--pip-layout))))
    (dolist (pos layout)
      (card-games-svg--pip svg (+ x mx (* (car pos) iw)) (+ y my (* (cdr pos) ih))
                   ps glyph color (> (cdr pos) 0.5)))))

(defun card-games-svg--draw-ace (svg x y w h glyph color)
  "Draw a single large central pip (an ace) of GLYPH in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size."
  (card-games-svg--pip svg (+ x (/ w 2.0)) (+ y (/ h 2.0)) (round (* h 0.42))
               glyph color nil))

(defun card-games-svg--draw-court (svg x y w h rank glyph color suit)
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
    (svg-node svg 'path :d d :fill (card-games-svg--court-fill suit)
              :stroke color :stroke-width 1)
    (card-games-svg--text svg rank (+ x (/ w 2.0)) (+ y (* h 0.65)) (* h 0.282) color t)
    (card-games-svg--text svg glyph (+ x (/ w 2.0)) (+ y (* h 0.82)) (* h 0.17) color)))

(defun card-games-svg--draw-joker (svg x y w h color)
  "Draw the joker face in COLOR on SVG.
X, Y and W, H give the card's top-left corner and size."
  (card-games-svg--text svg "★" (+ x (/ w 2.0)) (+ y (* h 0.52)) (* h 0.40) color)
  (card-games-svg--text svg "JOKER" (+ x (/ w 2.0)) (+ y (* h 0.74)) (* h 0.135) color t))

(defun card-games-svg--back-dots (svg x y w h)
  "Draw the dotted-medallion back pattern on SVG within X, Y, W, H."
  (let ((gy (+ y 10)))
    (while (< gy (- (+ y h) 8))
      (let ((gx (+ x 10)))
        (while (< gx (- (+ x w) 8))
          (svg-circle svg gx gy 1.1 :fill card-games-svg-back-trim)
          (setq gx (+ gx 9))))
      (setq gy (+ gy 9)))))

(defun card-games-svg--back-lattice (svg x y w h)
  "Draw a small-cross lattice back pattern on SVG within X, Y, W, H."
  (let ((gy (+ y 13)))
    (while (< gy (- (+ y h) 10))
      (let ((gx (+ x 13)))
        (while (< gx (- (+ x w) 10))
          (svg-line svg (- gx 2) (- gy 2) (+ gx 2) (+ gy 2)
                    :stroke card-games-svg-back-trim :stroke-width 1)
          (svg-line svg (- gx 2) (+ gy 2) (+ gx 2) (- gy 2)
                    :stroke card-games-svg-back-trim :stroke-width 1)
          (setq gx (+ gx 11))))
      (setq gy (+ gy 11)))))

(defun card-games-svg--back-waves (svg x y w h)
  "Draw a staggered-dash (brickwork) back pattern on SVG within X, Y, W, H."
  (let ((gy (+ y 12)) (row 0))
    (while (< gy (- (+ y h) 9))
      (let ((gx (+ x (if (cl-evenp row) 9 15))))
        (while (< gx (- (+ x w) 9))
          (svg-line svg gx gy (+ gx 6) gy :stroke card-games-svg-back-trim :stroke-width 1.4)
          (setq gx (+ gx 12))))
      (setq gy (+ gy 8) row (1+ row)))))

(defun card-games-svg--back-diamond (svg x y w h)
  "Draw concentric diamonds as the back pattern on SVG within X, Y, W, H."
  (let ((cx (+ x (/ w 2.0))) (cy (+ y (/ h 2.0))))
    (dolist (f '(0.40 0.28 0.16))
      (let ((dw (* w f)) (dh (* h f)))
        (svg-polygon svg (list (cons cx (- cy dh)) (cons (+ cx dw) cy)
                               (cons cx (+ cy dh)) (cons (- cx dw) cy))
                     :fill "none" :stroke card-games-svg-back-trim :stroke-width 1)))))

(defun card-games-svg--back-logo (svg x y w h back)
  "Stamp the Emacs logo named BACK on SVG within X, Y, W, H.
Fall back to dots when the logo image is unavailable."
  (let ((file (and (fboundp 'svg-embed)
                   (card-games-svg--logo-file (card-games-svg--back-logo-name back)))))
    (if (null file)
        (card-games-svg--back-dots svg x y w h)
      (let ((size (round (* h 0.52))))
        (svg-embed svg file "image/png" nil
                   :x (round (+ x (/ (- w size) 2.0)))
                   :y (round (+ y (/ (- h size) 2.0)))
                   :width size :height size)))))

(defun card-games-svg--draw-back (svg x y w h r)
  "Draw a face-down card back on SVG at X, Y (W by H, corner R).
The pattern is controlled by `card-games-svg-card-back'."
  (svg-rectangle svg x y w h :rx r :ry r :fill (card-games-svg--back-fill)
                 :stroke card-games-svg-border-color :stroke-width 1)
  (svg-rectangle svg (+ x 4) (+ y 4) (- w 8) (- h 8) :rx 4 :fill "none"
                 :stroke card-games-svg-back-trim :stroke-width 1)
  (let ((back (card-games-svg--effective-back)))
    (pcase back
      ('solid nil)
      ('rings
       (svg-rectangle svg (+ x 8) (+ y 8) (- w 16) (- h 16) :rx 5 :fill "none"
                      :stroke card-games-svg-back-trim :stroke-width 1)
       (svg-rectangle svg (+ x 12) (+ y 12) (- w 24) (- h 24) :rx 4 :fill "none"
                      :stroke card-games-svg-back-trim :stroke-width 1))
      ('lattice (card-games-svg--back-lattice svg x y w h))
      ('waves (card-games-svg--back-waves svg x y w h))
      ('diamond (card-games-svg--back-diamond svg x y w h))
      ((or 'emacs 'emacs-classic 'gnu 'splash)
       (card-games-svg--back-logo svg x y w h back))
      (_ (card-games-svg--back-dots svg x y w h)))))

(defun card-games-svg--draw-face (svg x y w h r rank suit)
  "Draw a face-up card (RANK of SUIT) on SVG at X, Y (W by H, corner R)."
  (svg-rectangle svg x y w h :rx r :ry r :fill card-games-svg-face-color
                 :stroke card-games-svg-border-color :stroke-width 1)
  (let ((color (card-games-svg--suit-color suit))
        (glyph (card-games-svg--suit-glyph suit)))
    (card-games-svg--index svg x y w h rank glyph color nil)
    (card-games-svg--index svg x y w h rank glyph color t)
    (cond
     ((eq suit 'joker) (card-games-svg--draw-joker svg x y w h color))
     ((member rank '("J" "Q" "K")) (card-games-svg--draw-court svg x y w h rank glyph color suit))
     ((equal rank "A") (card-games-svg--draw-ace svg x y w h glyph color))
     (t (let ((n (truncate (string-to-number (or rank "0")))))
          (if (and (>= n 1) (<= n 10))
              (card-games-svg--draw-pips svg x y w h n glyph color)
            (card-games-svg--text svg glyph (+ x (/ w 2.0)) (+ y (* h 0.6))
                          (* h 0.40) color)))))))

(cl-defun card-games-svg-card (svg x y &key rank suit down gap highlight hint)
  "Draw one card onto SVG with its top-left corner at X, Y.
With GAP draw an empty slot; with DOWN draw a face-down card;
otherwise draw a face card labelled RANK of SUIT (0-3 or `joker').
HIGHLIGHT draws a glowing cursor ring around the card."
  (let* ((w card-games-svg-card-width)
         (h card-games-svg-card-height)
         (r card-games-svg-corner-radius))
    (when (and card-games-svg-card-shadow (not gap))
      (svg-rectangle svg (+ x 2) (+ y 3) w h :rx r :ry r
                     :fill "black" :fill-opacity 0.16))
    (cond
     (gap
      (svg-rectangle svg x y w h :rx r :ry r :fill "black" :fill-opacity 0.05
                     :stroke card-games-svg-gap-color :stroke-width 1.5
                     :stroke-dasharray "4,4"))
     (down (card-games-svg--draw-back svg x y w h r))
     (t (card-games-svg--draw-face svg x y w h r rank suit)))
    (when hint
      ;; Haloed dashed ring: a dark backing reads on the light card faces and
      ;; the bright ring reads on the (green) felt -- one colour cannot do
      ;; both.  Dashed (vs the solid cursor ring) keeps it distinct by shape,
      ;; not colour alone.
      (svg-rectangle svg (- x 3) (- y 3) (+ w 6) (+ h 6) :rx (+ r 2)
                     :fill "none" :stroke "#101010" :stroke-opacity 0.55
                     :stroke-width 4 :stroke-dasharray "3,3")
      (svg-rectangle svg (- x 2) (- y 2) (+ w 4) (+ h 4) :rx (+ r 1)
                     :fill "none" :stroke (card-games-svg--hint) :stroke-width 2
                     :stroke-dasharray "3,3"))
    (when highlight
      (let ((hl (card-games-svg--highlight)))
        ;; Haloed ring so the cursor reads on both the near-white card faces
        ;; (dark edges) and the dark felt (bright ring): no single colour
        ;; meets 3:1 on both, so pair a bright ring with dark edges.
        (svg-rectangle svg (- x 5) (- y 5) (+ w 10) (+ h 10) :rx (+ r 4)
                       :fill "none" :stroke "#101010" :stroke-opacity 0.6
                       :stroke-width 6)
        (svg-rectangle svg (- x 3) (- y 3) (+ w 6) (+ h 6) :rx (+ r 2)
                       :fill "none" :stroke hl :stroke-width 3.5)
        (svg-rectangle svg (- x 1) (- y 1) (+ w 2) (+ h 2) :rx (+ r 1)
                       :fill "none" :stroke "#101010" :stroke-opacity 0.55
                       :stroke-width 1.5)))))

(defun card-games-svg--draw-spec (svg x y spec highlight &optional hint)
  "Draw SPEC onto SVG at X, Y, with HIGHLIGHT and optional HINT ring.
SPEC is (RANK . SUIT), the symbol `down', or nil for a gap."
  (cond
   ((null spec)     (card-games-svg-card svg x y :gap t :highlight highlight :hint hint))
   ((eq spec 'down) (card-games-svg-card svg x y :down t :highlight highlight :hint hint))
   (t (card-games-svg-card svg x y :rank (car spec) :suit (cdr spec)
                   :highlight highlight :hint hint))))

(cl-defun card-games-svg-cards-svg (specs &key highlight (pad 10) (overlap 0))
  "Return an svg object drawing SPECS left to right.
SPECS is a list of card specs (see Commentary).  HIGHLIGHT is the
index of a card to ring.  OVERLAP fans cards by overlapping them by
that many pixels.  PAD is the margin around the row."
  (let* ((w card-games-svg-card-width)
         (h card-games-svg-card-height)
         (step (max 1 (- (+ w card-games-svg-card-gap) overlap)))
         (n (length specs))
         (width (+ (* 2 pad) (if (> n 0) (+ (* (1- n) step) w) w)))
         (height (+ (* 2 pad) h))
         (svg (svg-create width height)))
    (let ((x pad) (i 0))
      (dolist (spec specs)
        (card-games-svg--draw-spec svg x pad spec (and highlight (= i highlight)))
        (setq x (+ x step) i (1+ i))))
    svg))

(cl-defun card-games-svg-grid-svg (rows &key cursor hints (pad 10))
  "Return an svg object drawing ROWS as a grid of cards.
ROWS is a list of rows, each a list of card specs.  CURSOR is (ROW . COL)
to highlight, or nil.  HINTS is a list of (ROW . COL) to ring as valid
targets.  PAD is the margin around the grid."
  (let* ((w card-games-svg-card-width)
         (h card-games-svg-card-height)
         (gx card-games-svg-card-gap)
         (gy card-games-svg-card-gap)
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
          (card-games-svg--draw-spec svg (+ pad (* c (+ w gx))) y spec
                             (and cursor (= r (car cursor)) (= c (cdr cursor)))
                             (and hints (member (cons r c) hints) t))
          (setq c (1+ c))))
      (setq r (1+ r)))
    svg))

(defun card-games-svg-image (svg &optional scale)
  "Return an Emacs image for SVG, optionally enlarged by SCALE."
  (if (and scale (/= scale 1.0))
      (svg-image svg :scale scale)
    (svg-image svg)))

(defun card-games-svg-to-string (svg)
  "Return the serialized XML string for SVG."
  (with-temp-buffer
    (svg-print svg)
    (buffer-string)))

(cl-defun card-games-svg-hand-svg (specs &key cursor hints marks (overlap 0) (pad 8))
  "Return an svg drawing SPECS as a left-to-right hand.
CURSOR is the index to ring as the cursor; HINTS and MARKS are lists of
indices to ring as playable and as marked; OVERLAP fans the cards and
PAD insets the row."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height)
         (step (max 1 (- (+ w card-games-svg-card-gap) overlap)))
         (n (length specs))
         (width (+ (* 2 pad) (if (> n 0) (+ (* (1- n) step) w) w)))
         (height (+ (* 2 pad) h))
         (svg (svg-create width height))
         (x pad) (i 0))
    (dolist (spec specs)
      (card-games-svg--draw-spec svg x pad spec (eql i cursor) (and (memq i hints) t))
      (when (memq i marks)
        (svg-rectangle svg (- x 3) (- pad 3) (+ w 6) (+ h 6)
                       :rx 7 :fill "none" :stroke "#4a90d9" :stroke-width 3))
      (setq x (+ x step) i (1+ i)))
    svg))

(defcustom card-games-svg-slider-stops '(0.6 0.8 1.0 1.25 1.5 1.8 2.2)
  "Card-size slider stops, as scale multipliers."
  :type '(repeat number) :group 'card-games)

(defun card-games-svg-slider-width ()
  "Return the pixel width of the card-size slider."
  (+ 36 (* (length card-games-svg-slider-stops) 24) 8))

(defun card-games-svg-slider-height ()
  "Return the pixel height of the card-size slider."
  24)

(defun card-games-svg-slider-draw (svg x y current)
  "Draw a card-size slider into SVG at X, Y knobbed at CURRENT.
Return its click regions as a list of (RECT . (scale . VALUE))."
  (let* ((stops card-games-svg-slider-stops) (segw 24) (regions '()) (i 0)
         (cy (+ y 10)) (tx (+ x 36)))
    (svg-text svg "size" :x x :y (+ y 14) :font-size 9 :fill "gray55"
              :font-family card-games-svg-font-family)
    (svg-line svg tx cy (+ tx (* (length stops) segw)) cy
              :stroke "gray60" :stroke-width 2)
    (dolist (v stops)
      (let* ((px (+ tx (* i segw) (/ segw 2)))
             (near (< (abs (- v current)) 0.08)))
        (svg-circle svg px cy (if near 7 4)
                    :fill (if near (card-games-svg--highlight) "white")
                    :stroke "gray50" :stroke-width 1)
        (push (cons (list (+ tx (* i segw)) y segw 22) (cons 'scale v)) regions))
      (setq i (1+ i)))
    (nreverse regions)))

(cl-defun card-games-svg-hand-image (specs &key cursor marks hints (overlap 0) region-tag)
  "Return a propertized one-image string for a hand of card SPECS.
CURSOR is the highlighted index; MARKS and HINTS are index lists and
OVERLAP fans the cards.  With REGION-TAG non-nil, the image carries a
`card-games-regions' click map (each card as (REGION-TAG . INDEX)) and a
card-size slider beneath the row."
  (if (not region-tag)
      (propertize "*" 'display
                  (card-games-svg-image (card-games-svg-hand-svg specs :cursor cursor :hints hints
                                                 :marks marks :overlap overlap)
                                (card-games-scale)))
    (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (pad 8)
           (step (max 1 (- (+ w card-games-svg-card-gap) overlap)))
           (n (length specs))
           (cardw (if (> n 0) (+ (* (1- n) step) w) w))
           (sh (card-games-svg-slider-height))
           (width (+ (* 2 pad) (max cardw (card-games-svg-slider-width))))
           (height (+ (* 2 pad) h 8 sh))
           (svg (svg-create width height)) (regions '()) (x pad) (i 0))
      (dolist (spec specs)
        (card-games-svg--draw-spec svg x pad spec (eql i cursor) (and (memq i hints) t))
        (when (memq i marks)
          (svg-rectangle svg (- x 3) (- pad 3) (+ w 6) (+ h 6)
                         :rx 7 :fill "none" :stroke "#4a90d9" :stroke-width 3))
        (push (cons (list x pad w h) (cons region-tag i)) regions)
        (setq x (+ x step) i (1+ i)))
      (setq regions (append (nreverse regions)
                            (card-games-svg-slider-draw svg pad (+ pad h 8) card-games-card-scale)))
      (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions regions))))

(defcustom card-games-svg-emacs-logo 'modern
  "Which Emacs emblem to show in the full-window (svg-fill) games.
The image choices embed a logo that ships with Emacs, falling back to the
drawn emblem when the file is unavailable.  `drawn' is a small built-in
emblem and `none' shows nothing."
  :type '(choice (const :tag "Modern Emacs icon" modern)
                 (const :tag "Classic Emacs icon" classic)
                 (const :tag "GNU head (Gnus)" gnu)
                 (const :tag "GNU Emacs splash" splash)
                 (const :tag "Drawn emblem" drawn)
                 (const :tag "None" none))
  :group 'card-games-svg)

(defconst card-games-svg--logo-files
  '((modern  . ("images/icons/hicolor/48x48/apps/emacs.png"
                "images/icons/hicolor/128x128/apps/emacs.png"))
    (classic . ("images/icons/hicolor/48x48/apps/emacs23.png"
                "images/icons/hicolor/128x128/apps/emacs23.png"))
    (gnu     . ("images/gnus/gnus.png"))
    (splash  . ("images/splash.png")))
  "Map a logo name to candidate image files relative to `data-directory'.")

(defun card-games-svg--logo-file (name)
  "Return the first readable image file for logo NAME, or nil."
  (cl-loop for rel in (cdr (assq name card-games-svg--logo-files))
           for f = (expand-file-name rel data-directory)
           when (file-readable-p f) return f))

(defun card-games-svg--draw-logo-emblem (svg cx cy fs)
  "Draw the built-in purple GNU Emacs emblem on SVG.
Centre it at CX, CY, scaled by FS."
  (svg-gradient svg "card-games-logo" 'linear '((0 . "#8056c8") (100 . "#3f1f9e")))
  (svg-circle svg cx cy (round (* 26 fs)) :gradient "card-games-logo"
              :stroke "#2a1370" :stroke-width 2)
  (card-games-svg--text svg "e" cx (+ cy (round (* 10 fs))) (round (* 30 fs)) "#ffffff" t)
  (card-games-svg--text svg "GNU Emacs" cx (+ cy (round (* 42 fs)))
                (max 10 (round (* 11 fs))) "#c7bbe6"))

(defun card-games-svg-draw-logo (svg cx cy &optional fs)
  "Draw the configured Emacs emblem on SVG, centred at CX, CY.
The emblem is chosen by `card-games-svg-emacs-logo'; FS scales it.  Embeds a
real Emacs logo image when one is available, and otherwise draws the
built-in emblem."
  (let ((fs (or fs 1.0)) (choice card-games-svg-emacs-logo))
    (pcase choice
      ('none nil)
      ('drawn (card-games-svg--draw-logo-emblem svg cx cy fs))
      (_ (let ((file (and (fboundp 'svg-embed) (card-games-svg--logo-file choice))))
           (if (null file)
               (card-games-svg--draw-logo-emblem svg cx cy fs)
             (let ((size (round (* 56 fs))))
               (svg-embed svg file "image/png" nil
                          :x (round (- cx (/ size 2)))
                          :y (round (- cy (/ size 2)))
                          :width size :height size))))))))

(provide 'card-games-svg)
;;; card-games-svg.el ends here
