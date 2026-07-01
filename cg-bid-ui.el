;;; cg-bid-ui.el --- 500 (Bid) — console UI and commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.91
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

;; The console (UNICODE) interface and interactive commands for 500.
;; The rules engine lives in cg-bid.el.  Play with `M-x cg-bid'.

;;; Code:

(require 'cl-lib)
(require 'cg-core)
(require 'cg-bid)
(require 'cg-svg)
(require 'cg-render)
(require 'svg)
(require 'color)


;;;; Rendering

(defun cg-bid--trick-card-for (game seat)
  "Return the card SEAT has played to the current (or last) trick, or nil."
  (let ((tr (or (cg-get game :trick) (cg-get game :last-trick))))
    (cdr (assq seat tr))))

(cl-defmethod cg-render ((game cg-bid-game))
  "Return a propertized string depicting GAME."
  (let* ((contract (cg-get game :contract))
         (trump (and contract (cg-bid-trump contract)))
         (scores (cg-get game :scores))
         (tricks (cg-get game :tricks))
         (turn (cg-get game :turn))
         (phase (cg-get game :phase))
         (out (list)))
    (push (format "  500 Bid    Hand %d\n" (cg-get game :hand-no)) out)
    (push (format "  Score — You/North: %d    West/East: %d\n"
                  (car scores) (cdr scores))
          out)
    (when (eq phase 'gameover)
      (push (propertize
             (format "  *** GAME OVER — %s WIN ***\n"
                     (if (= (cg-get game :game-over) 0) "YOU/NORTH" "WEST/EAST"))
             'face 'cg-cursor)
            out))
    (push (format "  Contract: %s\n\n"
                  (if contract
                      (format "%s (%s) by %s"
                              (cg-bid-label contract) (cg-bid-name contract)
                              (aref cg-bid-seat-names (cg-get game :contractor)))
                    "—  (auction in progress)"))
          out)
    ;; opponents and partner: name, hand size (or exposed/sitting), played card
    (cl-flet ((seatline
               (seat indent)
               (let ((sit (eql seat (cg-bid--sitter game)))
                     (exp (eql seat (cg-get game :exposed))))
                 (format "%s%s%s %s   played: %s\n"
                         indent
                         (aref cg-bid-seat-names seat)
                         (if (and (eq phase 'play) (= seat turn)) "*" " ")
                         (cond
                          (sit "(sitting out)")
                          (exp (format "[%s]"
                                       (mapconcat #'cg-bid-card-string
                                                  (cg-bid-sort-hand
                                                   (cg-bid--hand game seat) trump)
                                                  " ")))
                          (t (format "[%d cards]"
                                     (length (cg-bid--hand game seat)))))
                         (cg-bid-card-string (cg-bid--trick-card-for game seat))))))
      (push (seatline 2 "            ") out)   ; North (partner)
      (push (seatline 1 "  ") out)             ; West
      (push (seatline 3 "                              ") out)) ; East
    (push (format "\n  Tricks — You/North: %d    West/East: %d\n\n"
                  (+ (aref tricks 0) (aref tricks 2))
                  (+ (aref tricks 1) (aref tricks 3)))
          out)
    ;; human hand
    (push "  Your hand (South):\n   " out)
    (let* ((hand (cg-bid-sort-display (cg-bid--hand game 0) trump))
           (cursor (cg-get game :cursor))
           (marks (cg-get game :marks))
           (led (cg-get game :led))
           (legal (and (eq phase 'play)
                       (= turn 0)
                       (cg-bid-legal-cards (cg-bid--hand game 0) led trump))))
      (cg-put game :sorted-hand hand)
      (if (null hand)
          (push "(empty)" out)
        (cl-loop for c in hand for i from 0 do
                 (let ((faces nil)
                       (str (cg-bid-card-string c)))
                   (when (cg-red-suit-p (car c)) (push 'cg-red-suit faces))
                   (when (member c marks) (setq str (concat "^" str)))
                   (when (and legal (not (member c legal)))
                     (push 'cg-gap faces)) ; dim illegal plays
                   (when (= i cursor) (push 'cg-cursor faces))
                   (push (propertize (format " %-4s" str)
                                     'face (or faces 'default)
                                     'cg-card i 'mouse-face 'highlight)
                         out))))
      (push "\n" out))
    (push (format "\n  %s\n" (cg-get game :message)) out)
    (push (cg-bid--key-help game) out)
    (apply #'concat (nreverse out))))

(defun cg-bid--key-help (game)
  "Return a context-sensitive key-help line for GAME."
  (pcase (cg-get game :phase)
    ('auction "  [b]id   [p]ass   [n]ew hand   [q]uit   ? help\n")
    ('kitty   "  [←/→] move  [RET] mark/unmark  [x] discard the 5 marked  [q]uit\n")
    ('play    "  [←/→] move   [RET] play card   [n]ew hand   [q]uit   ? help\n")
    ('done    "  [n]ext hand   [q]uit   ? help\n")
    ('gameover "  [n]ew game   [q]uit   ? help\n")
    (_        "  [n]ew hand   [q]uit   ? help\n")))


;;;; Graphical (SVG) table

(defconst cg-bid--tw 44 "Table card width.")
(defconst cg-bid--th 62 "Table card height.")
(defconst cg-bid--canvas-w 600 "Table canvas width.")
(defconst cg-bid--canvas-h 460 "Table canvas height.")

(defcustom cg-bid-felt-color "#15692f"
  "Base felt colour for the 500 table.
Set to a theme-derived colour (see `cg-color') for a table that
matches your Emacs theme."
  :type 'color :group 'cg-svg)

(defcustom cg-bid-animate t
  "When non-nil, pace AI turns so play is watchable."
  :type 'boolean :group 'cg-svg)

(defcustom cg-bid-ai-delay 0.45
  "Seconds to pause after each AI action when `cg-bid-animate' is on."
  :type 'number :group 'cg-svg)

(defcustom cg-bid-trick-pause 1.1
  "Seconds to leave a completed trick on the table before it is swept."
  :type 'number :group 'cg-svg)

(defcustom cg-bid-svg-ui nil
  "When non-nil (and on a graphical display), render 500 as a single
full-buffer SVG: the table in the centre, a status/compass/bid panel on
the left, and a scrollable message log on the right."
  :type 'boolean :group 'cg-svg)

(defcustom cg-bid-svg-fill t
  "When non-nil, size the full-SVG UI to fill the window and enlarge the
South hand, re-fitting on window changes.  Only used when `cg-bid-svg-ui'."
  :type 'boolean :group 'cg-svg)

(defcustom cg-bid-card-scale 1.0
  "Card-size multiplier for the South hand in the full-SVG 500 UI.
Driven by the on-screen card-size slider and the +/-/0 keys."
  :type 'number :group 'cg-svg)


(defun cg-bid--header-text (game)
  "Return the header lines (scores, contract, tricks) for GAME."
  (let ((scores (cg-get game :scores))
        (contract (cg-get game :contract))
        (tricks (cg-get game :tricks)))
    (concat
     (format "  500 Bid    Hand %d\n" (cg-get game :hand-no))
     (format "  Score - You/North: %d    West/East: %d\n"
             (car scores) (cdr scores))
     (if (eq (cg-get game :phase) 'gameover)
         (format "  *** GAME OVER - %s WIN ***\n"
                 (if (= (cg-get game :game-over) 0) "YOU/NORTH" "WEST/EAST"))
       "")
     (format "  Contract: %s\n"
             (if contract
                 (format "%s (%s) by %s" (cg-bid-label contract)
                         (cg-bid-name contract)
                         (aref cg-bid-seat-names (cg-get game :contractor)))
               "-  (auction in progress)"))
     (format "  Tricks - You/North: %d    West/East: %d\n"
             (+ (aref tricks 0) (aref tricks 2))
             (+ (aref tricks 1) (aref tricks 3))))))

(defun cg-bid--footer-text (game)
  "Return the footer (message and key help) for GAME."
  (concat (format "\n  %s\n" (cg-get game :message))
          (cg-bid--key-help game)))

(defun cg-bid--spec (card)
  "Return the cg-svg card spec for a 500 CARD, or nil for none."
  (cond ((null card) nil)
        ((cg-bid-joker-p card) (cons nil 'joker))
        (t (cons (aref cg-bid-ranks (cdr card)) (car card)))))

(defun cg-bid--south-layout (n)
  "Return (X0 STEP Y) for laying N South-hand cards across the canvas."
  (let* ((w cg-bid--tw)
         (maxw (- cg-bid--canvas-w 24))
         (step (if (<= n 1) 0 (min (+ w 6) (/ (- maxw w) (1- n)))))
         (total (+ w (* (max 0 (1- n)) step)))
         (x0 (/ (- cg-bid--canvas-w total) 2))
         (y (- cg-bid--canvas-h cg-bid--th 8)))
    (list x0 step y)))

(defun cg-bid--draw-backs (svg cx top n)
  "Draw a small fan of up to N face-down cards centred at CX, TOP on SVG.
Card size and fan step follow the dynamic `cg-svg-card-width'."
  (let* ((cw cg-svg-card-width)
         (k (min (max n 0) 6))
         (step (max 12 (round (* cw 0.42))))
         (total (if (> k 0) (+ cw (* (1- k) step)) 0))
         (x0 (- cx (/ total 2))))
    (dotimes (i k) (cg-svg-card svg (+ x0 (* i step)) top :down t))))

(defun cg-bid--draw-opponent (svg game seat cx top &optional fs)
  "Draw opponent SEAT (label, backs, turn marker) on SVG centred at CX, TOP.
FS scales the name pill and its fonts."
  (let* ((fs (or fs 1.0))
         (n (length (cg-bid--hand game seat)))
         (sitter (eql seat (cg-bid--sitter game)))
         (lw (round (* 104 fs))) (lh (round (* 18 fs)))
         (fsz (max 11 (round (* 13 fs)))))
    (svg-rectangle svg (- cx (/ lw 2)) (- top lh 3) lw lh :rx (round (* 9 fs))
                   :fill "#0b3d1d" :fill-opacity 0.55)
    (svg-text svg (format "%s%s" (aref cg-bid-seat-names seat)
                          (if sitter " (sitting out)" (format "  (%d)" n)))
              :x cx :y (- top (round (* 8 fs))) :font-size fsz :fill "#eaffea"
              :text-anchor "middle" :font-family "sans-serif")
    (when (and (eq (cg-get game :phase) 'play) (= seat (cg-get game :turn)))
      (svg-text svg "*" :x cx :y (- top (round (* 22 fs)))
                :font-size (round (* 18 fs)) :fill "#f1c40f"
                :text-anchor "middle" :font-family "sans-serif"))
    (unless sitter (cg-bid--draw-backs svg cx top n))))

(defun cg-bid--draw-trick (svg game)
  "Draw the cards played to the current trick around the centre of SVG."
  (let* ((W cg-bid--canvas-w) (H cg-bid--canvas-h)
         (w cg-bid--tw) (h cg-bid--th)
         (cx (/ W 2)) (cy (/ H 2))
         (spots (list (list 0 (- cx (/ w 2)) (+ cy 22))
                      (list 1 (- cx 70 w) (- cy (/ h 2)))
                      (list 2 (- cx (/ w 2)) (- cy 22 h))
                      (list 3 (+ cx 70) (- cy (/ h 2))))))
    (dolist (s spots)
      (let* ((card (cg-bid--trick-card-for game (nth 0 s)))
             (spec (cg-bid--spec card)))
        (when spec
          (cg-svg-card svg (nth 1 s) (nth 2 s)
                       :rank (car spec) :suit (cdr spec)))))))

(defun cg-bid--draw-south (svg game)
  "Draw South's hand face-up along the bottom of SVG; record sort order."
  (let* ((trump (and (cg-get game :contract)
                     (cg-bid-trump (cg-get game :contract))))
         (hand (cg-bid-sort-display (cg-bid--hand game 0) trump)))
    (cg-put game :sorted-hand hand)
    (let* ((n (length hand)) (lay (cg-bid--south-layout n))
           (x0 (nth 0 lay)) (step (nth 1 lay)) (y (nth 2 lay))
           (cursor (cg-get game :cursor)) (marks (cg-get game :marks)) (i 0))
      (svg-text svg "Your hand (South)" :x (/ cg-bid--canvas-w 2)
                :y (+ y cg-bid--th 14)
                :font-size 12 :fill "#cfeccf" :text-anchor "middle"
                :font-family "sans-serif")
      (dolist (card hand)
        (let* ((spec (cg-bid--spec card))
               (marked (and (member card marks) t))
               (hl (or (and (eq (cg-get game :phase) 'play) (= i cursor))
                       marked))
               ;; selected cards pop up out of the hand
               (cy (if marked (- y (round (* cg-bid--th 0.17))) y)))
          (cg-svg-card svg (+ x0 (* i step)) cy
                       :rank (car spec) :suit (cdr spec) :highlight hl))
        (setq i (1+ i))))))

(defun cg-bid--table-svg (game)
  "Return an svg object depicting the whole 500 table for GAME."
  (let* ((W cg-bid--canvas-w) (H cg-bid--canvas-h)
         (svg (svg-create W H)))
    (let* ((base (or cg-bid-felt-color "#15692f"))
           (lite (or (ignore-errors (color-lighten-name base 12)) base))
           (dark (or (ignore-errors (color-darken-name base 16)) base)))
      (svg-gradient svg "cg-felt" 'radial (list (cons 0 lite) (cons 100 dark)))
      (svg-rectangle svg 0 0 W H :rx 14 :gradient "cg-felt")
      (svg-ellipse svg (/ W 2) (/ H 2) 132 88 :fill "black" :fill-opacity 0.10))
    (let ((cg-svg-card-width cg-bid--tw)
          (cg-svg-card-height cg-bid--th)
          (cg-svg-card-gap 4))
      (cg-bid--draw-opponent svg game 2 (/ W 2) 34)
      (cg-bid--draw-opponent svg game 1 80 (/ H 2))
      (cg-bid--draw-opponent svg game 3 (- W 80) (/ H 2))
      (cg-bid--draw-trick svg game)
      (cg-bid--draw-south svg game))
    svg))

(defun cg-bid--insert-graphical (game)
  "Insert the GUI (SVG) depiction of GAME into the current buffer.
Folds the controls into the single action-button row (see
`cg-bid--insert-buttons'); only the status line precedes it."
  (insert (cg-bid--header-text game))
  (insert-image (cg-svg-image (cg-bid--table-svg game) (cg-scale)))
  (insert (format "\n  %s\n" (cg-get game :message))))

(defun cg-bid--south-hit (px py n)
  "Map a click at PX, PY to a South-hand index (0..N-1), or nil."
  (let* ((lay (cg-bid--south-layout n))
         (x0 (nth 0 lay)) (step (nth 1 lay)) (y (nth 2 lay)))
    (when (and (> n 0) (>= py (- y (round (* cg-bid--th 0.17)) 4))
               (<= py (+ y cg-bid--th 8)) (>= px x0))
      (let ((i (if (<= step 0) 0 (/ (- px x0) step))))
        (when (< i n) i)))))



;;;; Interaction

(defvar-local cg-bid--game nil "The `cg-bid-game' in the current buffer.")

(defun cg-bid--mode-line (game)
  "Return a mode-line status string for GAME."
  (pcase (cg-get game :phase)
    ('auction (if (= (cg-get game :bidder) 0) " [Your bid]"
                (format " [%s bidding]" (aref cg-bid-seat-names (cg-get game :bidder)))))
    ('kitty (if (cg-bid--human-p (cg-get game :contractor)) " [Discard 5]"
              (format " [%s: kitty]"
                      (aref cg-bid-seat-names (cg-get game :contractor)))))
    ('play (if (= (cg-get game :turn) 0) " [Your turn]"
             (format " [%s to play]" (aref cg-bid-seat-names (cg-get game :turn)))))
    ('done " [Hand over — n]")
    ('gameover " [Game over — n]")
    (_ "")))

(defun cg-bid--phase-text (game)
  "Return a short prompt describing what to do now in GAME."
  (pcase (cg-get game :phase)
    ('auction (if (= (cg-get game :bidder) 0)
                  "Your turn to bid — click a bid, or Pass."
                (format "Waiting for %s to bid..."
                        (aref cg-bid-seat-names (cg-get game :bidder)))))
    ('kitty (if (cg-bid--human-p (cg-get game :contractor))
                "Click 5 cards to discard, then Discard."
              (format "%s is exchanging the kitty..."
                      (aref cg-bid-seat-names (cg-get game :contractor)))))
    ('play (if (= (cg-get game :turn) 0)
               "Your turn — click a card to play."
             (format "Waiting for %s to play..."
                     (aref cg-bid-seat-names (cg-get game :turn)))))
    ('done (or (cg-get game :hand-result) (cg-get game :message)))
    ('gameover (or (cg-get game :message) "Game over — click New game."))
    (_ (cg-get game :message))))

(defun cg-bid--announce (game)
  "Echo a prompt or status describing what to do now in GAME."
  (message "%s" (cg-bid--phase-text game)))

(defun cg-bid--button (label cmd help)
  "Insert a clickable button LABEL running CMD with tooltip HELP."
  (insert-text-button label 'action (lambda (_) (call-interactively cmd))
                      'help-echo help 'follow-link t 'face 'link)
  (insert " "))

(defun cg-bid--insert-buttons (game)
  "Insert clickable buttons for the actions available now in GAME."
  (insert "  ")
  (pcase (cg-get game :phase)
    ('auction (when (= (cg-get game :bidder) 0)
                (cg-bid--button "[Bid]" #'cg-bid-make-bid "Make a bid")
                (cg-bid--button "[Pass]" #'cg-bid-pass "Pass")))
    ('kitty (when (cg-bid--human-p (cg-get game :contractor))
              (cg-bid--button "[Discard 5]" #'cg-bid-discard-marked
                              "Discard the five marked cards")))
    ('play (when (= (cg-get game :turn) 0)
             (cg-bid--button "[Play]" #'cg-bid-select
                             "Play the highlighted card"))))
  (when (memq (cg-get game :phase) '(done gameover))
    (cg-bid--button (if (eq (cg-get game :phase) 'gameover) "[New game]" "[Next hand]")
                    #'cg-bid-new "Deal the next hand / start a new game"))
  (cg-bid--button "[Help]" #'cg-bid-help "Show help")
  (insert "\n"))

(cl-defmethod cg-renderer-draw ((_renderer cg-text-renderer) (game cg-bid-game))
  "Draw the 500 GAME as UNICODE text with the action buttons."
  (insert (cg-render game))
  (cg-bid--insert-buttons game))

(cl-defmethod cg-renderer-draw ((_renderer cg-svg-renderer) (game cg-bid-game))
  "Draw the 500 GAME as an SVG table with the action buttons."
  (cg-bid--insert-graphical game)
  (cg-bid--insert-buttons game))

(cl-defmethod cg-renderer-draw ((_renderer cg-svg-fill-renderer) (game cg-bid-game))
  "Draw the 500 GAME as a frameless full-window SVG table."
  (cg-bid--insert-svg-ui game))

(defun cg-bid--treatment ()
  "Return the display treatment symbol for the current 500 buffer.
Honours `cg-bid-svg-ui' and whether the display is graphical."
  (cond ((and cg-bid-svg-ui (display-graphic-p)) 'svg-fill)
        ((display-graphic-p) 'svg)
        (t 'text)))

(defun cg-bid--redisplay ()
  "Redraw the current 500 buffer through its renderer.
The treatment is chosen by `cg-bid--treatment' and dispatched with
`cg-renderer-draw'."
  (let* ((inhibit-read-only t)
         (game cg-bid--game)
         (renderer (cg-render-set-treatment game (cg-bid--treatment))))
    (setq-local mode-line-process (cg-bid--mode-line game))
    (erase-buffer)
    (cg-renderer-draw renderer game)
    (goto-char (point-min))))

(defun cg-bid--refresh ()
  "Advance AI to the next human action, animating turns if enabled."
  (let ((game cg-bid--game))
    (if (or (not cg-bid-animate) (<= cg-bid-ai-delay 0))
        (progn (cg-bid--run game) (cg-bid--redisplay))
      (cg-bid--redisplay)
      (let ((guard 0))
        (while (and (< (cl-incf guard) 400)
                    (let ((before (cg-get game :ntricks)))
                      (when (cg-bid--ai-step game)
                        (cg-bid--redisplay)
                        (message "%s" (cg-get game :message))
                        (sit-for (if (> (cg-get game :ntricks) before)
                                     cg-bid-trick-pause
                                   cg-bid-ai-delay))
                        t)))))
      (cg-bid--redisplay))
    (cg-bid--announce game)))

(defun cg-bid-left ()
  "Move the hand cursor left."
  (interactive)
  (cg-put cg-bid--game :cursor (max 0 (1- (cg-get cg-bid--game :cursor))))
  (cg-bid--redisplay))

(defun cg-bid-right ()
  "Move the hand cursor right."
  (interactive)
  (let ((n (length (cg-get cg-bid--game :sorted-hand))))
    (cg-put cg-bid--game :cursor (min (1- n) (1+ (cg-get cg-bid--game :cursor))))
    (cg-bid--redisplay)))

(defun cg-bid--current-card ()
  "Return the card under the hand cursor."
  (nth (cg-get cg-bid--game :cursor) (cg-get cg-bid--game :sorted-hand)))

(defun cg-bid-select ()
  "Play (in play phase) or mark/unmark (in kitty phase) the current card."
  (interactive)
  (let* ((game cg-bid--game)
         (phase (cg-get game :phase))
         (card (cg-bid--current-card)))
    (pcase phase
      ('kitty
       (when (cg-bid--human-p (cg-get game :contractor))
         (let ((marks (cg-get game :marks)))
           (cond
            ((member card marks)
             (cg-put game :marks (remove card marks))
             (cg-put game :message
                     (format "%d of 5 marked for discard." (length (cg-get game :marks)))))
            ((>= (length marks) 5)
             (cg-put game :message
                     "Five already marked — click a marked card to unmark first."))
            (t (cg-put game :marks (cons card marks))
               (cg-put game :message
                       (let ((np (length (cg-get game :marks))))
                         (if (= np 5) "5 of 5 marked — press Discard."
                           (format "%d of 5 marked for discard." np))))))
           (cg-bid--redisplay))))
      ('play
       (if (/= (cg-get game :turn) 0)
           (progn (cg-put game :message "Not your turn.") (cg-bid--redisplay))
         (let ((legal (cg-bid-legal-cards (cg-bid--hand game 0)
                                          (cg-get game :led)
                                          (cg-bid-trump (cg-get game :contract)))))
           (if (not (member card legal))
               (progn (cg-put game :message "Illegal — you must follow suit.")
                      (cg-bid--redisplay))
             (cg-bid--play game 0 card)
             (cg-bid--refresh)))))
      (_ (cg-bid--redisplay)))))

(defun cg-bid-discard-marked ()
  "Discard the five marked kitty cards."
  (interactive)
  (let* ((game cg-bid--game)
         (marks (cg-get game :marks)))
    (cond
     ((not (eq (cg-get game :phase) 'kitty))
      (cg-put game :message "Nothing to discard now.") (cg-bid--redisplay))
     ((/= (length marks) 5)
      (cg-put game :message (format "Mark exactly 5 (have %d)." (length marks)))
      (cg-bid--redisplay))
     (t (cg-bid--discard game (cg-get game :contractor) marks)
        (cg-put game :marks nil)
        (cg-bid--refresh)))))

(defun cg-bid--code (bid)
  "Return a short ASCII code for BID, e.g. \"7H\", \"8NT\", \"NL\"."
  (let ((trump (cg-bid-trump bid)) (tricks (cg-bid-tricks bid)))
    (cond ((cg-bid-nullo-p bid) (cg-bid-label bid))
          ((eq trump 'nt) (format "%dNT" tricks))
          (t (format "%d%c" tricks (aref "SCDH" trump))))))

(defun cg-bid-make-bid ()
  "Prompt the human for a bid.
Type a short code such as 7H, 8NT, NL (case-insensitive)."
  (interactive)
  (let* ((game cg-bid--game))
    (if (or (not (eq (cg-get game :phase) 'auction))
            (/= (cg-get game :bidder) 0))
        (progn (cg-put game :message "Not your turn to bid.")
               (cg-bid--redisplay))
      (let* ((legal (cg-bid--legal-bids game))
             (completion-ignore-case t)
             (choices (append (mapcar (lambda (b)
                                        (cons (format "%-4s %s (%d)"
                                                      (cg-bid--code b)
                                                      (cg-bid-name b)
                                                      (cg-bid-value b))
                                              b))
                                      legal)
                              '(("Pass" . pass))))
             (pick (completing-read "Your bid (e.g. 7H, 8NT, NL; or Pass): "
                                    (mapcar #'car choices) nil t))
             (sel (cdr (assoc pick choices))))
        (cg-bid--auction-act game 0 (if (eq sel 'pass) nil sel))
        (cg-bid--refresh)))))

(defun cg-bid-pass ()
  "Pass during the auction."
  (interactive)
  (let ((game cg-bid--game))
    (if (or (not (eq (cg-get game :phase) 'auction))
            (/= (cg-get game :bidder) 0))
        (progn (cg-put game :message "Not your turn to bid.")
               (cg-bid--redisplay))
      (cg-bid--auction-act game 0 nil)
      (cg-bid--refresh))))

(defun cg-bid-new ()
  "Advance to the next hand once a hand is over, or start a fresh game at
game over.  500 is a multi-hand game with no mid-hand redeal, so a hand in
progress must be played out (unlike the solitaire games)."
  (interactive)
  (let* ((game cg-bid--game) (phase (cg-get game :phase)))
    (cond
     ((eq phase 'gameover)
      (cg-put game :scores (cons 0 0))
      (cg-put game :hand-no 0)
      (cg-put game :game-over nil)
      (cg-bid--deal game 3)
      (cg-bid--refresh))
     ((eq phase 'done)
      (cg-bid--deal game (mod (1+ (cg-get game :dealer)) 4))
      (cg-bid--refresh))
     (t (cg-put game :message "Play the hand out — 500 has no mid-hand redeal.")
        (cg-bid--redisplay)))))

(defun cg-bid-mouse (event)
  "Handle a click in the 500 buffer (SVG-UI panels, table, or text)."
  (interactive "e")
  (let ((start (event-start event)) (game cg-bid--game))
    (if (and cg-bid-svg-ui (display-graphic-p) (posn-image start))
        (cg-bid--svg-ui-click start)
      (let ((i (if (and (display-graphic-p) (posn-image start))
                   (let ((xy (posn-object-x-y start)) (s (cg-scale)))
                     (and xy (cg-bid--south-hit (round (/ (car xy) s))
                                                (round (/ (cdr xy) s))
                                                (length (cg-get game :sorted-hand)))))
                 (let ((pos (posn-point start)))
                   (and pos (get-text-property pos 'cg-card))))))
        (when i (cg-put game :cursor i) (cg-bid-select))))))

(defun cg-bid-help ()
  "Show brief help."
  (interactive)
  (message "%s" (concat "500: win the auction, exchange the kitty, take your bid "
                        "in tricks. Click the ? Help button (SVG UI) for the rules.")))

(defun cg-bid-zoom-in ()
  "Enlarge the cards."
  (interactive)
  (if (and cg-bid-svg-ui (display-graphic-p))
      (progn (setq cg-bid-card-scale (min 2.2 (+ (or cg-bid-card-scale 1.0) 0.2)))
             (cg-bid--redisplay))
    (text-scale-increase 1) (cg-bid--redisplay)))
(defun cg-bid-zoom-out ()
  "Shrink the cards."
  (interactive)
  (if (and cg-bid-svg-ui (display-graphic-p))
      (progn (setq cg-bid-card-scale (max 0.6 (- (or cg-bid-card-scale 1.0) 0.2)))
             (cg-bid--redisplay))
    (text-scale-decrease 1) (cg-bid--redisplay)))
(defun cg-bid-zoom-reset ()
  "Reset the card size."
  (interactive)
  (if (and cg-bid-svg-ui (display-graphic-p))
      (progn (setq cg-bid-card-scale 1.0) (cg-bid--redisplay))
    (text-scale-set 0) (cg-bid--redisplay)))

(defun cg-bid-redraw ()
  "Redraw the table (e.g. after a theme or frame change)."
  (interactive)
  (cg-bid--redisplay))

(defvar cg-bid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-bid-left)
    (define-key map (kbd "<right>") #'cg-bid-right)
    (define-key map (kbd "RET") #'cg-bid-select)
    (define-key map "b" #'cg-bid-make-bid)
    (define-key map "p" #'cg-bid-pass)
    (define-key map "x" #'cg-bid-discard-marked)
    (define-key map "g" #'cg-bid-redraw)
    (define-key map "n" #'cg-bid-new)
    (define-key map "?" #'cg-bid-help)
    (define-key map "+" #'cg-bid-zoom-in)
    (define-key map "=" #'cg-bid-zoom-in)
    (define-key map "-" #'cg-bid-zoom-out)
    (define-key map "0" #'cg-bid-zoom-reset)
    (define-key map (kbd "M-<up>") #'cg-bid-log-up)
    (define-key map (kbd "M-<down>") #'cg-bid-log-down)
    (define-key map [wheel-up] #'cg-bid-wheel)
    (define-key map [wheel-down] #'cg-bid-wheel)
    (define-key map [mouse-4] #'cg-bid-wheel)
    (define-key map [mouse-5] #'cg-bid-wheel)
    (define-key map "v" #'cg-bid-toggle-svg-ui)
    (define-key map [mouse-1] #'cg-bid-mouse)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-bid-mode' (Emacs style; see `cg-keys').")

(defun cg-bid--classic-keymap ()
  "Return a copy of `cg-bid-mode-map' with vi-style h/l and SPC added."
  (let ((map (copy-keymap cg-bid-mode-map)))
    (define-key map "h" #'cg-bid-left)
    (define-key map "l" #'cg-bid-right)
    (define-key map (kbd "SPC") #'cg-bid-select)
    map))

(define-derived-mode cg-bid-mode special-mode "500"
  "Major mode for playing 500 (Bid)."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type)
  (add-hook 'window-configuration-change-hook #'cg-bid--fit nil t)
  (when (eq cg-keys 'classic)
    (use-local-map (cg-bid--classic-keymap))))

;;;###autoload
(defun cg-bid ()
  "Play 500 (Bid) against three computer opponents."
  (interactive)
  (let ((buf (get-buffer-create "*500 Bid*")))
    (with-current-buffer buf
      (cg-bid-mode)
      (setq cg-bid--game (cg-bid--deal (make-instance 'cg-bid-game)))
      (cg-bid--refresh))
    (switch-to-buffer buf)))


;;;; Frameless full-SVG UI (opt-in; see `cg-bid-svg-ui')

(defconst cg-bid--ui-w 860 "Default SVG-UI canvas width.")
(defconst cg-bid--ui-h 540 "Default SVG-UI canvas height.")
(defconst cg-bid--ui-tx 210 "Left edge of the table area.")
(defconst cg-bid--ui-tw 440 "Default width of the table area.")
(defconst cg-bid--sw 58 "South-hand card width (larger, for readability).")
(defconst cg-bid--sh 82 "South-hand card height.")

(defconst cg-bid--south-minfrac 0.30
  "Minimum South-card step as a fraction of card width.
The smallest gutter that still keeps each card's rank/suit index visible.")

(defun cg-bid--south-size (w h)
  "Return (SW . SH) South-card size for a canvas W by H.
The player's cards grow with the window; height grows about twice as
fast as the window widens, so the hand compresses (cards overlap) as the
table enlarges.  Capped at 42% of canvas height; width is capped later,
per-deal, so the hand always fits the table."
  (let* ((wf (- w cg-bid--ui-w)) (hf (- h cg-bid--ui-h))
         (base (max 76 (min (round (* h 0.42))
                            (round (+ 92 (* hf 0.20) (* wf 0.40))))))
         (sh (max 50 (min (round (* h 0.60))
                          (round (* base (or cg-bid-card-scale 1.0))))))
         (sw (round (* sh 0.70))))
    (cons sw sh)))

(defvar-local cg-bid--regions nil
  "Plist of clickable SVG-UI regions for hit-testing.")
(defvar-local cg-bid--last-size nil
  "Last window pixel size used to render the SVG-UI.")

(defun cg-bid--in-rect (px py rect)
  "Return non-nil when PX,PY lie inside RECT (X Y W H)."
  (and rect (>= px (nth 0 rect)) (< px (+ (nth 0 rect) (nth 2 rect)))
       (>= py (nth 1 rect)) (< py (+ (nth 1 rect) (nth 3 rect)))))

(defun cg-bid--text-left (svg str x y size color &optional bold)
  "Draw left-anchored text STR on SVG."
  (let ((a (list :x (round x) :y (round y) :font-size (round size)
                 :fill color :text-anchor "start" :font-family cg-svg-font-family)))
    (when bold (setq a (append a (list :font-weight "bold"))))
    (apply #'svg-text svg str a)))

(defun cg-bid--ui-label (svg str x y &optional size)
  "Draw an all-caps, letter-spaced section label on SVG (font SIZE, default 10)."
  (svg-text svg (upcase str) :x (round x) :y (round y) :font-size (round (or size 10))
            :fill "#8fc79b" :text-anchor "start" :font-family cg-svg-font-family
            :font-weight "bold" :letter-spacing "2"))

(defun cg-bid--ui-divider (svg x1 x2 y)
  "Draw a faint horizontal divider on SVG."
  (svg-line svg x1 y x2 y :stroke "#1b6b35" :stroke-width 1))

(defun cg-bid--active-seat (game)
  "Return the seat whose action is pending, or nil."
  (pcase (cg-get game :phase)
    ('auction (cg-get game :bidder))
    ('kitty (cg-get game :contractor))
    ('play (cg-get game :turn))
    (_ nil)))

(defun cg-bid--hand-layout (n width xoff ybottom &optional cardw cardh)
  "Return (X0 STEP Y) for N cards across WIDTH from XOFF, bottom YBOTTOM.
CARDW/CARDH default to the table card size.  Cards overlap to fit but
keep a minimum gutter so each rank index stays visible."
  (let* ((w (or cardw cg-bid--tw))
         (hgt (or cardh cg-bid--th))
         (maxw (- width 24))
         (minstep (max 14 (round (* w cg-bid--south-minfrac))))
         (fit (if (<= n 1) 0 (/ (- maxw w) (1- n))))
         (step (if (<= n 1) 0 (max minstep (min (+ w 7) fit))))
         (total (+ w (* (max 0 (1- n)) step)))
         (x0 (+ xoff (/ (- width total) 2)))
         (y (- ybottom hgt 8)))
    (list x0 step y)))

(defun cg-bid--draw-trick-at (svg game cx cy &optional fs)
  "Draw the current trick centred at CX, CY on SVG, on a faint drop-zone.
FS scales the drop-zone, the played cards, and their spread."
  (let* ((fs (or fs 1.0))
         (r (round (* 80 fs)))
         (w (round (* cg-bid--tw fs))) (h (round (* cg-bid--th fs)))
         (off (round (* 70 fs))) (gap (round (* 22 fs)))
         (spots (list (list 0 (- cx (/ w 2)) (+ cy gap))
                      (list 1 (- cx off w) (- cy (/ h 2)))
                      (list 2 (- cx (/ w 2)) (- cy gap h))
                      (list 3 (+ cx off) (- cy (/ h 2))))))
    (svg-circle svg cx cy r :fill "#000000" :fill-opacity 0.08)
    (svg-circle svg cx cy r :fill "none" :stroke "#0e5226" :stroke-width 2)
    (let ((cg-svg-card-width w) (cg-svg-card-height h))
      (dolist (s spots)
        (let* ((card (cg-bid--trick-card-for game (nth 0 s))) (spec (cg-bid--spec card)))
          (when spec
            (cg-svg-card svg (nth 1 s) (nth 2 s) :rank (car spec) :suit (cdr spec))))))))

(defun cg-bid--draw-south-region (svg game tx tw ybottom sw sh)
  "Draw South's hand (cards SW by SH) within TX width TW bottom YBOTTOM.
Return (:hand (X0 STEP Y N SH))."
  (let* ((trump (and (cg-get game :contract) (cg-bid-trump (cg-get game :contract))))
         (hand (cg-bid-sort-display (cg-bid--hand game 0) trump)))
    (cg-put game :sorted-hand hand)
    (let* ((n (length hand))
           ;; Cap card width so N cards fit the table at the index-safe
           ;; gutter; tall cards shrink only when the table is too narrow.
           (maxsw (if (<= n 1) sw
                    (/ (- tw 24.0) (+ 1.0 (* (1- n) cg-bid--south-minfrac)))))
           (capped (and (> n 1) (> sw maxsw)))
           (sw (if capped (max 40 (round maxsw)) sw))
           (sh (if capped (round (/ sw 0.70)) sh))
           (lay (cg-bid--hand-layout n tw tx ybottom sw sh))
           (x0 (nth 0 lay)) (step (nth 1 lay)) (y (nth 2 lay))
           (cursor (cg-get game :cursor)) (marks (cg-get game :marks)) (i 0))
      (svg-rectangle svg tx (- y 6) tw (+ sh 14) :rx 10
                     :fill "#ffffff" :fill-opacity 0.05)
      (let ((cg-svg-card-width sw) (cg-svg-card-height sh)
            (legal (and (eq (cg-get game :phase) 'play) (= (cg-get game :turn) 0)
                        (cg-bid-legal-cards
                         (cg-bid--hand game 0) (cg-get game :led)
                         (and (cg-get game :contract)
                              (cg-bid-trump (cg-get game :contract)))))))
        (dolist (card hand)
          (let* ((spec (cg-bid--spec card)) (marked (and (member card marks) t))
                 (illegal (and legal (not (member card legal))))
                 (hl (or (and (eq (cg-get game :phase) 'play) (= i cursor)) marked))
                 (cxc (+ x0 (* i step)))
                 (cy (if marked (- y (round (* sh 0.17))) y)))
            (cg-svg-card svg cxc cy :rank (car spec) :suit (cdr spec) :highlight hl)
            (when illegal
              (svg-rectangle svg cxc cy sw sh :rx 6 :fill "#0a1a0c" :fill-opacity 0.55)))
          (setq i (1+ i))))
      (list :hand (list x0 step y n sh)))))

(defun cg-bid--draw-compass (svg game cx cy r &optional fs)
  "Draw a compass turn indicator centred at CX, CY radius R on SVG.
FS scales the N/S/E/W label fonts."
  (let ((active (cg-bid--active-seat game))
        (lsz (max 12 (round (* 13 (or fs 1.0))))))
    (svg-circle svg cx cy r :fill "#0d4a22" :stroke "#0a3a1a" :stroke-width 2)
    (svg-circle svg cx cy (- r 7) :fill "none" :stroke "#1b6b35" :stroke-width 1)
    (cl-flet ((lab (seat lx ly s)
                (cg-svg--text svg s lx ly lsz
                              (if (eql seat active) "#f6e27a" "#bfe0bf")
                              (eql seat active))))
      (lab 2 cx (- cy r -15) "N")
      (lab 0 cx (+ cy r -5)  "S")
      (lab 1 (- cx r -11) (+ cy 5) "W")
      (lab 3 (+ cx r -11) (+ cy 5) "E"))
    (when active
      (let* ((tip (pcase active
                    (2 (cons cx (- cy (- r 16))))
                    (0 (cons cx (+ cy (- r 16))))
                    (1 (cons (- cx (- r 16)) cy))
                    (3 (cons (+ cx (- r 16)) cy)))))
        (svg-line svg cx cy (car tip) (cdr tip) :stroke "#f1c40f" :stroke-width 2)
        (svg-circle svg (car tip) (cdr tip) 3 :fill "#f1c40f")))
    (svg-circle svg cx cy 3 :fill "#cfeccf")))

(defun cg-bid--draw-logo (svg cx cy &optional fs)
  "Draw the configured Emacs emblem centred at CX, CY on SVG, scaled by FS.
The emblem is chosen with `cg-svg-emacs-logo'."
  (cg-svg-draw-logo svg cx cy fs))

(defun cg-bid--grid-cell (bid gx gy cw ch g)
  "Return (X Y W H) for BID in a grid at GX,GY with cells CW by CH, gutter G.
Suit/NT bids occupy rows by trick count (6-10) and columns by suit;
nullo bids share the bottom row."
  (if (cg-bid-nullo-p bid)
      (let ((col (pcase (cg-bid-label bid) ("ON" 1) ("GN" 2) (_ 0))))
        (list (+ gx (* col (+ cw g))) (+ gy (* 5 (+ ch g))) cw ch))
    (let ((col (if (eq (cg-bid-trump bid) 'nt) 4 (cg-bid-trump bid)))
          (row (- (cg-bid-tricks bid) 6)))
      (list (+ gx (* col (+ cw g))) (+ gy (* row (+ ch g))) cw ch))))

(defun cg-bid--grid-pass-cell (gx gy cw ch g)
  "Return (X Y W H) for the double-width Pass button (bottom row, cols 3-4)."
  (list (+ gx (* 3 (+ cw g))) (+ gy (* 5 (+ ch g))) (+ (* 2 cw) g) ch))

(defun cg-bid--draw-left-panel (svg game h lpw fs ccy)
  "Draw the full-height left status panel; return its clickable regions.
LPW is the panel width, FS the font/element scale, CCY the compass centre
Y (also the North reference line).  All metrics scale with FS so the
panel content grows with the window."
  (let* ((scores (cg-get game :scores))
         (contract (cg-get game :contract))
         (regions nil)
         (F (lambda (n) (round (* n fs))))
         (px0 (funcall F 16)) (pxr (- lpw (funcall F 12)))
         (dl (funcall F 8)) (dr (- lpw (funcall F 8)))
         (cxp (/ lpw 2))
         (cr (funcall F 44))
         (y 0))
    (svg-rectangle svg 6 6 (- lpw 8) (- h 12) :rx 10 :fill "#0d4a22" :fill-opacity 0.9
                   :stroke "#0a3a1a" :stroke-width 1)
    (cg-bid--draw-compass svg game cxp ccy cr fs)
    (setq y (+ ccy cr (funcall F 12)))
    (cg-bid--ui-divider svg dl dr y)
    (setq y (+ y (funcall F 18)))
    (cg-bid--ui-label svg "Scores" px0 y (funcall F 10))
    (setq y (+ y (funcall F 22)))
    (cg-bid--text-left svg "You / North" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (car scores)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family cg-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 20)))
    (cg-bid--text-left svg "West / East" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (cdr scores)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family cg-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 16)))
    (cg-bid--ui-divider svg dl dr y)
    (setq y (+ y (funcall F 18)))
    (cg-bid--ui-label svg "Contract" px0 y (funcall F 10))
    (setq y (+ y (funcall F 32)))
    (cg-svg--text svg (if contract (cg-bid-label contract) "Auction…")
                  cxp y (funcall F 24) "#f1c40f" t)
    (setq y (+ y (funcall F 20)))
    (if contract
        (let ((tr (cg-get game :tricks)))
          (cg-bid--text-left svg
                             (format "%s — tricks %d:%d"
                                     (aref cg-bid-seat-names (cg-get game :contractor))
                                     (+ (aref tr 0) (aref tr 2)) (+ (aref tr 1) (aref tr 3)))
                             px0 y (funcall F 12) "#cfeccf"))
      (cg-bid--text-left svg "Bidding in progress" px0 y (funcall F 12) "#9fd0a8"))
    (setq y (+ y (funcall F 14)))
    (cg-bid--ui-divider svg dl dr y)
    (when (and (eq (cg-get game :phase) 'auction) (= (cg-get game :bidder) 0))
      (setq y (+ y (funcall F 18)))
      (cg-bid--ui-label svg "Your bid" px0 y (funcall F 10))
      ;; extra breathing room between the label and the grid
      (setq y (+ y (funcall F 16)))
      (let* ((gx px0) (gy y)
             (g (funcall F 5))
             (cw (max 24 (/ (- lpw px0 (funcall F 12) (* 4 g)) 5)))
             ;; full height when the panel is tall; shrink to fit on short windows
             (ch (min (funcall F 26)
                      (max (funcall F 10)
                           (- (/ (- (- h (funcall F 14)) gy) 6) g))))
             (legal (cg-bid--legal-bids game)) (bids nil))
        (dolist (b cg-bid-schedule)
          (when (memq b legal)
            (let* ((cell (cg-bid--grid-cell b gx gy cw ch g))
                   (x (nth 0 cell)) (cy2 (nth 1 cell)) (w (nth 2 cell)) (h2 (nth 3 cell))
                   (color (cg-svg--suit-color (pcase (cg-bid-trump b)
                                                ('nt 0) ('nullo 'joker) (n n)))))
              (svg-rectangle svg x cy2 w h2 :rx 5 :fill "#fdfdfb"
                             :stroke color :stroke-width 1)
              (cg-svg--text svg (cg-bid-label b) (+ x (/ w 2)) (+ cy2 (round (* h2 0.66)))
                            (funcall F 12) color t)
              (push (cons b cell) bids))))
        (setq regions (plist-put regions :bids bids))
        (let ((pr (cg-bid--grid-pass-cell gx gy cw ch g)))
          (svg-rectangle svg (nth 0 pr) (nth 1 pr) (nth 2 pr) (nth 3 pr)
                         :rx 5 :fill "#7f8c8d" :stroke "#566573" :stroke-width 1)
          (cg-svg--text svg "Pass" (+ (nth 0 pr) (/ (nth 2 pr) 2))
                        (+ (nth 1 pr) (round (* ch 0.66)))
                        (funcall F 12) "#ffffff" t)
          (setq regions (plist-put regions :pass pr)))))
    ;; kitty: a Discard button for the human contractor (mouse-only path)
    (when (and (eq (cg-get game :phase) 'kitty)
               (cg-bid--human-p (cg-get game :contractor)))
      (setq y (+ y (funcall F 18)))
      (cg-bid--ui-label svg "Kitty" px0 y (funcall F 10))
      (setq y (+ y (funcall F 14)))
      (let* ((nmk (length (cg-get game :marks))) (ready (= nmk 5))
             (bx px0) (by y) (bw (- lpw px0 (funcall F 12))) (bh (funcall F 30)))
        (svg-rectangle svg bx by bw bh :rx 6
                       :fill (if ready "#2e7d32" "#14401f")
                       :fill-opacity (if ready 1.0 0.6)
                       :stroke "#0a3a1a" :stroke-width 1)
        (cg-svg--text svg (format "Discard %d / 5" nmk)
                      (+ bx (/ bw 2)) (+ by (round (* bh 0.64)))
                      (funcall F 13) (if ready "#ffffff" "#9fd0a8") t)
        (setq regions (plist-put regions :discard (list bx by bw bh)))))
    regions))

(defun cg-bid--draw-log (svg game x w h fs ccy)
  "Draw the full-height right log panel (emblem + scrolling story); return regions.
FS scales the emblem and fonts; CCY aligns the divider with the compass."
  (let* ((F (lambda (n) (round (* n fs))))
         (y 6) (bottom (- h 6 (funcall F 64)))            ; reserve a control strip
         (logtop (+ ccy (funcall F 44) (funcall F 12)))   ; align with left divider
         (lh (funcall F 16))
         (list-top (+ logtop (funcall F 24)))
         (tw (funcall F 5)) (tx (+ x w (- (funcall F 10))))
         (log (cg-get game :log)) (total (max 1 (length log)))
         (scroll (or (cg-get game :log-scroll) 0))
         (vis (max 1 (/ (- bottom list-top) lh)))
         (maxch (max 12 (round (/ (- w (funcall F 22)) (* 0.62 (funcall F 11)))))))
    (svg-rectangle svg x y w (- (- h 6) y) :rx 10 :fill "#0d4a22" :fill-opacity 0.9
                   :stroke "#0a3a1a" :stroke-width 1)
    ;; emblem in the open top area, divider aligned with the left compass divider
    (cg-bid--draw-logo svg (+ x (/ w 2)) ccy fs)
    (cg-bid--ui-divider svg (+ x (funcall F 10)) (- (+ x w) (funcall F 10)) logtop)
    (cg-bid--ui-label svg "Log" (+ x (funcall F 12)) (+ logtop (funcall F 16)) (funcall F 10))
    ;; scrollbar track + proportional thumb + delicate arrows
    (svg-rectangle svg tx list-top tw (- bottom list-top) :rx 2 :fill "#0a3a1a")
    (let* ((th2 (max 16 (round (* (- bottom list-top) (min 1.0 (/ (float vis) total))))))
           (room (- (- bottom list-top) th2))
           (ty2 (+ list-top (round (* room (/ (float scroll) (max 1 (- total 1)))))))
           (up (list tx (- list-top 11) tw 9)) (dn (list tx (+ bottom 2) tw 9)))
      (svg-rectangle svg tx ty2 tw th2 :rx 2 :fill "#7fae8a")
      (svg-polygon svg (list (cons (+ tx 2) (nth 1 up)) (cons (- tx 1) (+ (nth 1 up) 7))
                             (cons (+ tx 5) (+ (nth 1 up) 7))) :fill "#9fd0a8")
      (svg-polygon svg (list (cons (- tx 1) (nth 1 dn)) (cons (+ tx 5) (nth 1 dn))
                             (cons (+ tx 2) (+ (nth 1 dn) 7))) :fill "#9fd0a8")
      ;; entries: newest first; top item gets ceremony; alternating stripes
      (let ((yy (+ list-top (funcall F 12))) (ents (nthcdr scroll log)) (k 0))
        (while (and ents (< k vis))
          (let* ((sline (car ents)) (top? (= k 0)))
            (when (cl-oddp k)
              (svg-rectangle svg (+ x (funcall F 6)) (- yy (funcall F 12))
                             (- w (funcall F 22)) lh :fill "#ffffff" :fill-opacity 0.05))
            (when (> (length sline) maxch)
              (setq sline (concat (substring sline 0 (1- maxch)) "…")))
            (cg-bid--text-left svg sline (+ x (funcall F 10)) yy
                               (if top? (funcall F 13) (funcall F 11))
                               (if top? "#f4faf4" "#cfe3cf") top?)
            (setq yy (+ yy (if top? (funcall F 22) lh))))
          (setq ents (cdr ents) k (1+ k))))
      ;; global controls (Help, card size) in the reserved bottom strip
      (cg-bid--ui-divider svg (+ x (funcall F 10)) (- (+ x w) (funcall F 10))
                          (+ bottom (funcall F 4)))
      (let* ((cz (+ bottom (funcall F 12)))
             (hx (+ x (funcall F 12))) (hw (- w (funcall F 24))) (hh (funcall F 24))
             (sy (+ cz hh (funcall F 14)))
             (lx (+ x (funcall F 36))) (rx2 (- (+ x w) (funcall F 14)))
             (stops cg-svg-slider-stops)
             (segw (/ (float (- rx2 lx)) (max 1 (1- (length stops)))))
             (srs nil) (k 0))
        (svg-rectangle svg hx cz hw hh :rx (funcall F 6)
                       :fill "#14401f" :stroke "#2e7d32" :stroke-width 1)
        (cg-svg--text svg "? Help / Rules" (+ hx (/ hw 2)) (+ cz (round (* hh 0.66)))
                      (funcall F 12) "#cfe3cf" t)
        (cg-bid--ui-label svg "Size" (+ x (funcall F 12)) (+ sy (funcall F 4)) (funcall F 9))
        (svg-line svg lx sy rx2 sy :stroke "#1b6b35" :stroke-width 2)
        (dolist (v stops)
          (let* ((cxk (round (+ lx (* k segw))))
                 (near (< (abs (- v (or cg-bid-card-scale 1.0))) 0.08)))
            (svg-circle svg cxk sy (if near 7 4)
                        :fill (if near "#f1c40f" "#eaffea")
                        :stroke "#0a3a1a" :stroke-width 1)
            (push (cons (list (- cxk (round (/ segw 2))) (- sy 10)
                              (max 12 (round segw)) 20) (cons 'scale v)) srs))
          (setq k (1+ k)))
        (list :scroll-up up :scroll-down dn :log-region (list x y w (- bottom y))
              :help (list hx cz hw hh) :sizer (nreverse srs))))))

(defun cg-bid--draw-banner (svg game tx tw ty fs)
  "Draw the phase-prompt banner across the top of the table."
  (let* ((txt (cg-bid--phase-text game))
         (by (+ ty (round (* 6 fs)))) (bh (round (* 30 fs)))
         (bw (min (- tw (round (* 90 fs)))
                  (max (round (* 240 fs)) (* (length txt) (round (* 8 fs))))))
         (bx (+ tx (/ (- tw bw) 2))))
    (svg-rectangle svg bx by bw bh :rx (round (* 15 fs))
                   :fill "#0d2c17" :fill-opacity 0.88 :stroke "#2e7d32" :stroke-width 1)
    (cg-svg--text svg txt (+ bx (/ bw 2)) (+ by (round (* bh 0.66)))
                  (round (* 14 fs)) "#f4faf4" t)))

(defun cg-bid--draw-help-overlay (svg _game tx ty tw th fs)
  "Draw the rules/legend overlay over the table; return its regions."
  (let* ((F (lambda (n) (round (* n fs))))
         (m (funcall F 26))
         (ox (+ tx m)) (oy (+ ty m)) (ow (- tw (* 2 m))) (oh (- th (* 2 m)))
         (lx (+ ox (funcall F 22))) (y (+ oy (funcall F 36)))
         (lines '("How to play 500"
                  ""
                  "You (South) + North are partners vs West + East."
                  "1. AUCTION — bid how many tricks your side will take,"
                  "   or Pass.  Click a bid in the left panel; high bid wins."
                  "2. KITTY — the winner takes 5 hidden cards, then clicks"
                  "   5 to throw away (the Discard button turns green at 5)."
                  "3. PLAY — take turns clicking a card; follow the led suit."
                  "   Take at least as many tricks as you bid to score."
                  ""
                  "Bids:  7♠ = take 7 tricks, spades trump.   NT = no-trump."
                  "       NL / ON / GN = misère bids (try to take none)."
                  "Trump rank: Joker, right & left bowers, A K Q 10 9 ... 4."))
         (regions nil))
    (svg-rectangle svg ox oy ow oh :rx (funcall F 14)
                   :fill "#08200f" :fill-opacity 0.97 :stroke "#2e7d32" :stroke-width 2)
    (dolist (ln lines)
      (let ((title (string-prefix-p "How to" ln)))
        (cg-bid--text-left svg ln lx y (if title (funcall F 18) (funcall F 13))
                           (if title "#f1c40f" "#eaffea") title))
      (setq y (+ y (funcall F 22))))
    (let* ((by (- (+ oy oh) (funcall F 44)))
           (bw (funcall F 130)) (bh (funcall F 30)) (g (funcall F 14)) (bx lx))
      (cl-flet ((btn (label key fill)
                  (svg-rectangle svg bx by bw bh :rx (funcall F 6) :fill fill
                                 :stroke "#0a3a1a" :stroke-width 1)
                  (cg-svg--text svg label (+ bx (/ bw 2)) (+ by (round (* bh 0.64)))
                                (funcall F 13) "#ffffff" t)
                  (setq regions (plist-put regions key (list bx by bw bh)))
                  (setq bx (+ bx bw g))))
        (btn "Close" :help-close "#2e7d32")
        (btn "Classic view" :help-classic "#34495e")
        (btn "Quit" :help-quit "#7f3b3b")))
    regions))

(defun cg-bid--ui-svg (game &optional w h)
  "Return (SVG . REGIONS) for the full-buffer SVG-UI of GAME (W by H).
Everything scales proportionally with the canvas: FS drives fonts and
table cards, PSCALE the side-panel widths."
  (let* ((W (or w cg-bid--ui-w)) (H (or h cg-bid--ui-h))
         (svg (svg-create W H)) (regions nil)
         ;; master scales relative to the base 860x540 canvas
         (fs (max 1.0 (min 2.0 (/ (+ (/ (float W) cg-bid--ui-w)
                                     (/ (float H) cg-bid--ui-h)) 2.0))))
         (pscale (max 1.0 (min 1.7 (/ (float W) cg-bid--ui-w))))
         (lpw (round (* 196 pscale)))
         (rp-w (round (* 206 pscale)))
         (rp-x (- W rp-w))
         (tx (+ lpw 14)) (tw (max 320 (- rp-x tx 8)))
         (ty 8) (th (- H 16))
         (cx (+ tx (/ tw 2))) (cy (+ ty (/ th 2)))
         ;; opponent/trick card size grows up to ~2x
         (otw (round (* cg-bid--tw fs))) (oth (round (* cg-bid--th fs)))
         ;; compass-centre line; North sits just below it, its name just above
         (ccy (max 56 (round (* H 0.12)))))
    (let* ((base (or cg-bid-felt-color "#15692f"))
           (lite (or (ignore-errors (color-lighten-name base 12)) base))
           (dark (or (ignore-errors (color-darken-name base 18)) base)))
      (svg-gradient svg "cg-felt2" 'radial (list (cons 0 lite) (cons 100 dark)))
      (svg-rectangle svg 0 0 W H :rx 14 :gradient "cg-felt2")
      (svg-rectangle svg (- tx 6) 8 (+ tw 12) (- H 16) :rx 12
                     :fill "none" :stroke "#0e5226" :stroke-width 2))
    (let ((cg-svg-card-width otw) (cg-svg-card-height oth)
          (cg-svg-card-gap (max 2 (round (* 4 fs))))
          (inset (round (* 70 fs))))
      ;; North: cards just below the compass line, name just above it
      (cg-bid--draw-opponent svg game 2 cx (+ ccy (round (* 4 fs))) fs)
      ;; West/East: vertically centred on the table midline
      (cg-bid--draw-opponent svg game 1 (+ tx inset) (- cy (/ oth 2)) fs)
      (cg-bid--draw-opponent svg game 3 (- (+ tx tw) inset) (- cy (/ oth 2)) fs)
      (cg-bid--draw-trick-at svg game cx (- cy 24) fs))
    (let ((ss (cg-bid--south-size W H)))
      (setq regions (append regions
                            (cg-bid--draw-south-region svg game tx tw (+ ty th)
                                                       (car ss) (cdr ss)))))
    (when (memq (cg-get game :phase) '(done gameover))
      (let* ((hy (nth 2 (plist-get regions :hand)))
             (bw (round (* 120 fs))) (bh (round (* 26 fs)))
             (bx (- cx (/ bw 2))) (by (- hy bh (round (* 8 fs)))))
        (svg-rectangle svg bx by bw bh :rx 6 :fill "#2e7d32"
                       :stroke "#0a3a1a" :stroke-width 1)
        (cg-svg--text svg "Next hand" (+ bx (/ bw 2)) (+ by (round (* bh 0.66)))
                      (round (* 14 fs)) "#ffffff" t)
        (setq regions (plist-put regions :next (list bx by bw bh)))))
    (setq regions (append regions (cg-bid--draw-left-panel svg game H lpw fs ccy)))
    (setq regions (append regions (cg-bid--draw-log svg game rp-x rp-w H fs ccy)))
    (cg-bid--draw-banner svg game tx tw ty fs)
    (when (cg-get game :help-open)
      (setq regions (append regions
                            (cg-bid--draw-help-overlay svg game tx ty tw th fs))))
    (cons svg regions)))

(defun cg-bid--insert-svg-ui (game)
  "Insert the full-buffer SVG-UI for GAME and record its regions.
When `cg-bid-svg-fill', size the canvas to fill the window."
  (let* ((win (get-buffer-window (current-buffer)))
         (fill (and cg-bid-svg-fill win))
         (w (if fill (max 720 (window-body-width win t)) cg-bid--ui-w))
         (h (if fill (max 470 (- (window-body-height win t) 4)) cg-bid--ui-h))
         (sr (cg-bid--ui-svg game w h)))
    (when fill (setq cg-bid--last-size (cons (window-body-width win t)
                                             (window-body-height win t))))
    (setq cg-bid--regions (cdr sr))
    (insert-image (cg-svg-image (car sr) (if fill 1.0 (cg-scale))))
    (put-text-property (point-min) (point-max) 'pointer 'hand)))

(defun cg-bid--fit (&rest _)
  "Re-render the SVG-UI to fit the window after a configuration change."
  (when (and cg-bid--game cg-bid-svg-ui cg-bid-svg-fill
             (eq major-mode 'cg-bid-mode))
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (let ((sz (cons (window-body-width win t) (window-body-height win t))))
          (unless (equal sz cg-bid--last-size)
            (setq cg-bid--last-size sz)
            (cg-bid--redisplay)))))))

(defun cg-bid-log-up ()
  "Scroll the SVG-UI message log towards older entries."
  (interactive)
  (let* ((game cg-bid--game) (max (max 0 (1- (length (cg-get game :log))))))
    (cg-put game :log-scroll (min max (1+ (or (cg-get game :log-scroll) 0))))
    (cg-bid--redisplay)))

(defun cg-bid-log-down ()
  "Scroll the SVG-UI message log towards newer entries."
  (interactive)
  (let ((game cg-bid--game))
    (cg-put game :log-scroll (max 0 (1- (or (cg-get game :log-scroll) 0))))
    (cg-bid--redisplay)))

(defun cg-bid-wheel (event)
  "Scroll the message log when the wheel turns over the log area.
Elsewhere, fall back to normal buffer scrolling."
  (interactive "e")
  (let ((start (event-start event)) (rg cg-bid--regions) (handled nil))
    (when (and cg-bid-svg-ui (display-graphic-p) (posn-image start))
      (let* ((xy (posn-object-x-y start)) (s (cg-scale))
             (px (round (/ (car xy) s))) (py (round (/ (cdr xy) s))))
        (when (cg-bid--in-rect px py (plist-get rg :log-region))
          (setq handled t)
          (pcase (event-basic-type event)
            ((or 'wheel-up 'mouse-4) (cg-bid-log-up))
            ((or 'wheel-down 'mouse-5) (cg-bid-log-down))))))
    (unless handled
      (ignore-errors (require 'mwheel) (mwheel-scroll event)))))

(defun cg-bid--region-bid (px py rg)
  "Return the bid whose button rect contains PX,PY in REGIONS RG, or nil."
  (cl-some (lambda (e) (and (cg-bid--in-rect px py (cdr e)) (car e)))
           (plist-get rg :bids)))

(defun cg-bid--region-hand (px py hl)
  "Return the South-hand index at PX,PY given hand layout HL, or nil."
  (when hl
    (let ((x0 (nth 0 hl)) (step (nth 1 hl)) (y (nth 2 hl)) (n (nth 3 hl))
          (sh (or (nth 4 hl) cg-bid--sh)))
      (when (and (> n 0) (>= py (- y (round (* sh 0.17)) 4))
                 (<= py (+ y sh 8)) (>= px x0))
        (let ((i (if (<= step 0) 0 (/ (- px x0) step)))) (when (< i n) i))))))

(defun cg-bid--sizer-hit (px py rg)
  "If PX,PY lands on a card-size slider stop in RG, apply it; return non-nil."
  (let ((hit (cl-some (lambda (e) (and (cg-bid--in-rect px py (car e)) (cdr e)))
                      (plist-get rg :sizer))))
    (when (and (consp hit) (eq (car hit) 'scale))
      (setq cg-bid-card-scale (cdr hit))
      (cg-bid--refresh)
      t)))

(defun cg-bid--svg-ui-click (start)
  "Dispatch a click at posn START within the SVG-UI."
  (let* ((xy (posn-object-x-y start)) (s (cg-scale))
         (px (round (/ (car xy) s))) (py (round (/ (cdr xy) s)))
         (game cg-bid--game) (rg cg-bid--regions) bid)
    (if (cg-get game :help-open)
        (cond
         ((cg-bid--in-rect px py (plist-get rg :help-classic)) (cg-bid-toggle-svg-ui))
         ((cg-bid--in-rect px py (plist-get rg :help-quit)) (quit-window))
         (t (cg-put game :help-open nil) (cg-bid--redisplay)))
      (cond
       ((cg-bid--in-rect px py (plist-get rg :help))
        (cg-put game :help-open t) (cg-bid--redisplay))
       ((cg-bid--in-rect px py (plist-get rg :scroll-up)) (cg-bid-log-up))
       ((cg-bid--in-rect px py (plist-get rg :scroll-down)) (cg-bid-log-down))
       ((cg-bid--in-rect px py (plist-get rg :next)) (cg-bid-new))
       ((cg-bid--in-rect px py (plist-get rg :discard)) (cg-bid-discard-marked))
       ((cg-bid--sizer-hit px py rg))
       ((and (cg-bid--in-rect px py (plist-get rg :pass))
             (eq (cg-get game :phase) 'auction) (= (cg-get game :bidder) 0))
        (cg-bid--auction-act game 0 nil) (cg-bid--refresh))
       ((setq bid (cg-bid--region-bid px py rg))
        (cg-bid--auction-act game 0 bid) (cg-bid--refresh))
       (t (let ((i (cg-bid--region-hand px py (plist-get rg :hand))))
            (when i (cg-put game :cursor i) (cg-bid-select))))))))

(defun cg-bid-toggle-svg-ui ()
  "Toggle the full-buffer SVG UI for 500."
  (interactive)
  (setq cg-bid-svg-ui (not cg-bid-svg-ui))
  (setq cg-bid--last-size nil)
  (cg-bid--redisplay)
  (message "Full-SVG UI %s" (if cg-bid-svg-ui "enabled" "disabled")))

(provide 'cg-bid-ui)
;;; cg-bid-ui.el ends here
