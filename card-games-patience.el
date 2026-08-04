;;; card-games-patience.el --- Pile solitaires (Golf, TriPeaks, Pyramid)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.91
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

;; Three "pile" solitaires that clear a fixed layout of cards rather than
;; building tableau columns:
;;
;;   `card-games-golf'     -- move an exposed card to the waste when it is one rank
;;                    above or below the waste's top card; deal when stuck.
;;   `card-games-tripeaks' -- the same, on three overlapping peaks, with Ace-King
;;                    wrapping so long chains are possible.
;;   `card-games-pyramid'  -- remove pairs of exposed cards whose ranks sum to 13
;;                    (Kings go alone); deal from the stock to help.
;;
;; A board is a vector of card slots; each slot lists the slots that cover
;; it, and a slot is "exposed" (playable) once all its coverers are gone.
;; Cards are the package-standard cons (SUIT . RANK) with RANK 0 Ace .. 12
;; King; a rank's value for the sum-of-13 rule is RANK + 1.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-pat-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0 (Ace) .. 12 (King).")

(defun card-games-pat-card-string (card)
  "Return a short string for CARD, or a dot for an empty slot."
  (if (null card) "·"
    (concat (aref card-games-pat-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defsubst card-games-pat-red-p (card) "Return non-nil when CARD is red." (and card (card-games-red-suit-p (car card))))

(defun card-games-pat--deck () "Return a fresh shuffled 52-card deck." (card-games-shuffle (cl-loop for s below 4 append
                                            (cl-loop for r below 13 collect (cons s r)))))

;;;; Classes

(defclass card-games-patience-game (card-games-game)
  ((mode :initform 'build :documentation "Play mode: build (waste) or sum13.")
   (wrap :initform nil :documentation "Whether Ace-King wrap in build mode.")
   (vname :initform "Patience"))
  "Abstract base for the pile solitaires."
  :abstract t)

(defclass card-games-golf-game (card-games-patience-game)
  ((mode :initform 'build) (wrap :initform nil) (vname :initform "Golf")))
(defclass card-games-tripeaks-game (card-games-patience-game)
  ((mode :initform 'build) (wrap :initform t) (vname :initform "TriPeaks")))
(defclass card-games-pyramid-game (card-games-patience-game)
  ((mode :initform 'sum13) (vname :initform "Pyramid")))

;;;; Layouts -- return (CARDS-VECTOR COVER-VECTOR ROWS), ROWS for display.

(cl-defgeneric card-games-pat--layout (game deck)
  "Build GAME's board from DECK; return (CARDS COVER ROWS STOCK WASTE).")

(cl-defmethod card-games-pat--layout ((_ card-games-golf-game) deck)
  "Lay out DECK as a Golf board."
  (let ((cards (make-vector 35 nil)) (cover (make-vector 35 nil)) (rows nil))
    (dotimes (c 7) (dotimes (r 5)
      (let ((i (+ (* c 5) r)))
        (aset cards i (pop deck))
        (when (< r 4) (aset cover i (list (+ i 1)))))))
    (dotimes (r 5) (push (cl-loop for c below 7 collect (+ (* c 5) r)) rows))
    (let ((waste (list (pop deck))))
      (list cards cover (nreverse rows) deck waste))))

(cl-defmethod card-games-pat--layout ((_ card-games-tripeaks-game) deck)
  "Lay out DECK as a TriPeaks board."
  (let ((cards (make-vector 28 nil))
        (cover (vector '(3 4) '(5 6) '(7 8)
                       '(9 10) '(10 11) '(12 13) '(13 14) '(15 16) '(16 17)
                       '(18 19) '(19 20) '(20 21) '(21 22) '(22 23) '(23 24)
                       '(24 25) '(25 26) '(26 27)
                       nil nil nil nil nil nil nil nil nil nil))
        (rows (list '(0 1 2) '(3 4 5 6 7 8)
                    '(9 10 11 12 13 14 15 16 17)
                    '(18 19 20 21 22 23 24 25 26 27))))
    (dotimes (i 28) (aset cards i (pop deck)))
    (let ((waste (list (pop deck))))
      (list cards cover rows deck waste))))

(cl-defmethod card-games-pat--layout ((_ card-games-pyramid-game) deck)
  "Lay out DECK as a Pyramid board."
  (let ((cards (make-vector 28 nil)) (cover (make-vector 28 nil)) (rows nil))
    (dotimes (r 7)
      (let ((start (/ (* r (1+ r)) 2)) (row nil))
        (dotimes (i (1+ r))
          (let ((idx (+ start i)))
            (aset cards idx (pop deck))
            (push idx row)
            (when (< r 6)
              (let ((below (/ (* (1+ r) (+ r 2)) 2)))
                (aset cover idx (list (+ below i) (+ below i 1)))))))
        (push (nreverse row) rows)))
    (list cards cover (nreverse rows) deck nil)))

;;;; Engine

(cl-defmethod card-games-pat--deal ((game card-games-patience-game))
  "Deal a fresh board into GAME."
  (random t)
  (cl-destructuring-bind (cards cover rows stock waste) (card-games-pat--layout game (card-games-pat--deck))
    (card-games-put game :cards cards)
    (card-games-put game :cover cover)
    (card-games-put game :rows rows)
    (card-games-put game :stock stock)
    (card-games-put game :waste waste)
    (card-games-put game :marks nil)
    (card-games-put game :cursor 0)
    (card-games-put game :moves 0)
    (card-games-put game :history nil)
    (card-games-put game :message
            (if (eq (oref game mode) 'sum13)
                "Remove pairs summing to 13; Kings go alone.  RET marks, stock deals."
              "Move a card one rank from the waste top.  RET plays; stock deals."))
    game))

(defun card-games-pat--exposed-p (game i)
  "Return non-nil when GAME board slot I is present and uncovered."
  (let ((cards (card-games-get game :cards)))
    (and (aref cards i)
         (cl-every (lambda (j) (null (aref cards j))) (aref (card-games-get game :cover) i)))))

(defun card-games-pat--exposed (game)
  "Return GAME's list of exposed board slot indices."
  (cl-loop for i below (length (card-games-get game :cards))
           when (card-games-pat--exposed-p game i) collect i))

(defun card-games-pat--spots (game)
  "Return the ordered spots the cursor can visit in GAME."
  (append (mapcar (lambda (i) (cons 'slot i)) (card-games-pat--exposed game))
          '((waste . 0) (stock . 0))))

(defun card-games-pat--waste-top (game) "Return the top card of GAME's waste pile." (car (last (card-games-get game :waste))))

(defun card-games-pat--board-empty-p (game)
  "Return non-nil when every GAME board slot has been cleared."
  (cl-every #'null (append (card-games-get game :cards) nil)))

(cl-defmethod card-games-won-p ((game card-games-patience-game))
  "Return non-nil when GAME's board has been cleared."
  (card-games-pat--board-empty-p game))

(defun card-games-pat--adjacent (a b wrap)
  "Return non-nil when ranks A and B differ by one, or (with WRAP) Ace-King."
  (let ((d (abs (- a b)))) (or (= d 1) (and wrap (= d 12)))))

(defun card-games-pat--snapshot (game)
  "Record an undo snapshot of GAME."
  (card-games-put game :history
          (cons (list (copy-sequence (card-games-get game :cards))
                      (copy-sequence (card-games-get game :stock))
                      (copy-sequence (card-games-get game :waste))
                      (card-games-get game :moves))
                (card-games-get game :history))))

(defun card-games-pat--restore (game)
  "Undo the last move of GAME, if any."
  (let ((h (card-games-get game :history)))
    (when h
      (cl-destructuring-bind (cards stock waste moves) (car h)
        (card-games-put game :cards cards) (card-games-put game :stock stock)
        (card-games-put game :waste waste) (card-games-put game :moves moves))
      (card-games-put game :history (cdr h))
      (card-games-put game :marks nil)
      t)))

(defun card-games-pat--deal-stock (game)
  "Turn one of GAME's stock cards to the waste."
  (let ((stock (card-games-get game :stock)))
    (if (null stock)
        (card-games-put game :message "The stock is empty.")
      (card-games-pat--snapshot game)
      (card-games-put game :waste (append (card-games-get game :waste) (last stock 1)))
      (card-games-put game :stock (butlast stock 1))
      (card-games-put game :marks nil)
      (card-games-put game :message "Dealt a card."))))

(defun card-games-pat--value (card) "Sum-of-13 value of CARD." (1+ (cdr card)))

(defun card-games-pat--remove-slot (game i)
  "Clear GAME board slot I."
  (aset (card-games-get game :cards) i nil))

;;;; Interaction

(defvar-local card-games-pat--game nil "The pile-solitaire game in the current buffer.")

(defun card-games-pat--cur-spot (game)
  "Return the spot the cursor is on in GAME."
  (let ((spots (card-games-pat--spots game)))
    (nth (min (card-games-get game :cursor) (1- (length spots))) spots)))

(defun card-games-pat-act ()
  "Play the spot under the cursor (build move, sum-13 mark, or deal)."
  (interactive)
  (let* ((game card-games-pat--game) (spot (card-games-pat--cur-spot game)))
    (pcase (car spot)
      ('stock (card-games-pat--deal-stock game))
      ('waste (when (eq (oref game mode) 'sum13) (card-games-pat--toggle-mark game (cons 'waste 0))))
      ('slot
       (let* ((i (cdr spot)) (card (aref (card-games-get game :cards) i)))
         (if (eq (oref game mode) 'build)
             (let ((top (card-games-pat--waste-top game)))
               (if (and top (card-games-pat--adjacent (cdr card) (cdr top) (oref game wrap)))
                   (progn (card-games-pat--snapshot game)
                          (card-games-put game :waste (append (card-games-get game :waste) (list card)))
                          (card-games-pat--remove-slot game i)
                          (card-games-put game :moves (1+ (card-games-get game :moves)))
                          (card-games-put game :message "Played."))
                 (card-games-put game :message "That card is not adjacent to the waste top.")))
           ;; sum13
           (if (= 13 (card-games-pat--value card))
               (progn (card-games-pat--snapshot game) (card-games-pat--remove-slot game i)
                      (card-games-put game :moves (1+ (card-games-get game :moves)))
                      (card-games-put game :marks nil)
                      (card-games-put game :message "King removed."))
             (card-games-pat--toggle-mark game (cons 'slot i)))))))
    (card-games-pat--after game)))

(defun card-games-pat--mark-value (game m)
  "Return the card value of GAME mark M (a slot or the waste)."
  (pcase (car m)
    ('slot (card-games-pat--value (aref (card-games-get game :cards) (cdr m))))
    ('waste (let ((w (card-games-pat--waste-top game))) (and w (card-games-pat--value w))))))

(defun card-games-pat--toggle-mark (game m)
  "Toggle GAME mark M; when two marked slots sum to 13, remove both."
  (if (member m (card-games-get game :marks))
      (card-games-put game :marks (remove m (card-games-get game :marks)))
    (card-games-put game :marks (cons m (card-games-get game :marks))))
  (let ((marks (card-games-get game :marks)))
    (when (= 2 (length marks))
      (if (= 13 (+ (card-games-pat--mark-value game (nth 0 marks))
                   (card-games-pat--mark-value game (nth 1 marks))))
          (progn (card-games-pat--snapshot game)
                 (dolist (mm marks)
                   (pcase (car mm)
                     ('slot (card-games-pat--remove-slot game (cdr mm)))
                     ('waste (card-games-put game :waste (butlast (card-games-get game :waste) 1)))))
                 (card-games-put game :moves (1+ (card-games-get game :moves)))
                 (card-games-put game :marks nil)
                 (card-games-put game :message "Pair removed."))
        (card-games-put game :marks nil)
        (card-games-put game :message "Those do not sum to 13.")))))

(defun card-games-pat--after (game)
  "Redisplay GAME and announce a win."
  (card-games-pat--redisplay)
  (when (card-games-won-p game)
    (card-games-put game :message "Board cleared -- you won!  Press n for a new game.")
    (card-games-pat--redisplay)
    (message "Solved!")))

(defun card-games-pat--move (delta)
  "Move the cursor by DELTA spots."
  (let* ((game card-games-pat--game) (n (length (card-games-pat--spots game))))
    (card-games-put game :cursor (mod (+ (card-games-get game :cursor) delta) n))
    (card-games-pat--redisplay)))

(defun card-games-pat-left () "Cursor left." (interactive) (card-games-pat--move -1))
(defun card-games-pat-right () "Cursor right." (interactive) (card-games-pat--move 1))
(defun card-games-pat-undo () "Undo." (interactive)
  (let ((game card-games-pat--game))
    (card-games-put game :message (if (card-games-pat--restore game) "Undid a move." "Nothing to undo."))
    (card-games-pat--redisplay)))
(defun card-games-pat-new () "New deal." (interactive)
  (card-games-pat--deal card-games-pat--game) (card-games-pat--redisplay))
(defun card-games-pat-redraw () "Redraw." (interactive) (card-games-pat--redisplay))
(defun card-games-pat-help () "Controls." (interactive)
  (message "Arrows or click: move/play  RET: play/mark/deal  u: undo  +/-: size  n: new"))

;;;; Rendering

(defun card-games-pat--render-card (card &optional exposed marked cursor)
  "Return CARD's display text, flagged by EXPOSED, MARKED, and CURSOR."
  (let ((s (card-games-pat-card-string card)) (faces nil))
    (when (card-games-pat-red-p card) (push 'card-games-red-suit faces))
    (when (and card (not exposed)) (push 'card-games-gap faces))
    (when marked (push 'card-games-hint faces))
    (when cursor (push 'card-games-cursor faces))
    (propertize (format "%4s" s) 'face (or faces 'default))))

(defcustom card-games-pat-svg-cards t
  "When non-nil, draw the patience board as SVG on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-pat--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD, or nil."
  (and card (cons (aref card-games-pat-ranks (cdr card)) (car card))))

(defun card-games-pat--svg (game)
  "Return a propertized, clickable one-image SVG board for patience GAME.
Exposed slots, the waste, and the stock each carry a click region (the
matching spot); a card-size slider sits below."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (pad 12) (gap card-games-svg-card-gap)
         (rowstep 30) (rows (card-games-get game :rows)) (cur (card-games-pat--cur-spot game))
         (marks (card-games-get game :marks)) (lc (card-games-color 'shadow :foreground "gray40"))
         (maxlen (apply #'max 1 (mapcar #'length rows))) (nrows (length rows))
         (sh (card-games-svg-slider-height))
         (width (+ (* 2 pad) (max (* maxlen (+ w gap)) (card-games-svg-slider-width))))
         (boardh (+ (* (1- nrows) rowstep) h)) (bottom-y (+ pad boardh 26))
         (slider-y (+ bottom-y h 10))
         (height (+ slider-y sh pad)) (svg (svg-create width height))
         (r 0) (regions '()))
    (dolist (row rows)
      (let* ((len (length row)) (x0 (/ (- width (* len (+ w gap))) 2))
             (y (+ pad (* r rowstep))) (c 0))
        (dolist (i row)
          (let* ((card (aref (card-games-get game :cards) i)) (x (+ x0 (* c (+ w gap)))))
            (when card
              (card-games-svg-card svg x y :rank (car (card-games-pat--spec card))
                           :suit (cdr (card-games-pat--spec card))
                           :highlight (equal cur (cons 'slot i))
                           :hint (and (member (cons 'slot i) marks) t))
              (when (card-games-pat--exposed-p game i)
                (push (cons (list x y w h) (cons 'slot i)) regions))))
          (setq c (1+ c))))
      (setq r (1+ r)))
    (svg-text svg "Waste" :x pad :y (- bottom-y 3) :font-size 11 :fill lc
              :font-family card-games-svg-font-family)
    (let ((wt (card-games-pat--waste-top game)))
      (if wt (card-games-svg-card svg pad bottom-y :rank (car (card-games-pat--spec wt))
                          :suit (cdr (card-games-pat--spec wt))
                          :highlight (equal cur '(waste . 0))
                          :hint (and (member '(waste . 0) marks) t))
        (card-games-svg-card svg pad bottom-y :gap t :highlight (equal cur '(waste . 0)))))
    (push (cons (list pad bottom-y w h) (cons 'waste 0)) regions)
    (svg-text svg (format "Stock(%d)" (length (card-games-get game :stock)))
              :x (+ pad w gap) :y (- bottom-y 3) :font-size 11 :fill lc
              :font-family card-games-svg-font-family)
    (if (card-games-get game :stock)
        (card-games-svg-card svg (+ pad w gap) bottom-y :down t :highlight (equal cur '(stock . 0)))
      (card-games-svg-card svg (+ pad w gap) bottom-y :gap t :highlight (equal cur '(stock . 0))))
    (push (cons (list (+ pad w gap) bottom-y w h) (cons 'stock 0)) regions)
    (setq regions (append (nreverse regions)
                          (card-games-svg-slider-draw svg pad slider-y card-games-card-scale)))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions regions)))

(cl-defmethod card-games-render-apply ((g card-games-patience-game) action)
  "Apply a click ACTION (a board spot) to GAME G: select that spot and play."
  (pcase action
    ((or `(slot . ,_) `(waste . ,_) `(stock . ,_))
     (let ((idx (cl-position action (card-games-pat--spots g) :test #'equal)))
       (when idx (card-games-put g :cursor idx) (card-games-pat-act))))
    (_ (cl-call-next-method))))

(cl-defmethod card-games-render ((game card-games-patience-game))
  "Return a propertized depiction of GAME (SVG on a graphical display)."
  (if (and card-games-pat-svg-cards (display-graphic-p))
      (card-games-pat--svg game)
    (card-games-pat--render-text game)))

(defun card-games-pat--render-text (game)
  "Return a plain-text depiction of patience GAME."
  (let* ((cur (card-games-pat--cur-spot game)) (marks (card-games-get game :marks)) (out (list)))
    (push (format "  %s   Moves: %d\n\n" (oref game vname) (card-games-get game :moves)) out)
    (dolist (row (card-games-get game :rows))
      (push "  " out)
      (dolist (i row)
        (let* ((card (aref (card-games-get game :cards) i))
               (exp (card-games-pat--exposed-p game i))
               (mk (member (cons 'slot i) marks))
               (cz (equal cur (cons 'slot i))))
          (push (if card (card-games-pat--render-card card exp mk cz) "    ") out)))
      (push "\n" out))
    (push (format "\n  Waste: %s    Stock: %d\n"
                  (let ((w (card-games-pat--waste-top game)))
                    (card-games-pat--render-card w t (member '(waste . 0) marks)
                                         (equal cur '(waste . 0))))
                  (length (card-games-get game :stock)))
          out)
    (push (format "  %s\n" (if (equal cur '(stock . 0))
                               (propertize "[stock]" 'face 'card-games-cursor) "")) out)
    (push (format "\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun card-games-pat--redisplay ()
  "Redraw the current patience-game buffer."
  (let ((game card-games-pat--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-pat--redisplay)
    (setq-local mode-line-process (format " [%s]" (if (card-games-won-p game) "solved" "playing")))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

;;;; Mode and commands

(defvar card-games-pat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>") #'card-games-pat-left)
    (define-key map (kbd "<right>") #'card-games-pat-right)
    (define-key map (kbd "<up>") #'card-games-pat-left)
    (define-key map (kbd "<down>") #'card-games-pat-right)
    (define-key map (kbd "RET") #'card-games-pat-act)
    (define-key map (kbd "SPC") #'card-games-pat-act)
    (define-key map "u" #'card-games-pat-undo)
    (define-key map "n" #'card-games-pat-new)
    (define-key map "g" #'card-games-pat-redraw)
    (define-key map "?" #'card-games-pat-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-pat-mode'.")

(define-derived-mode card-games-pat-mode special-mode "Patience"
  "Major mode for the pile solitaires."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

(defun card-games-pat--play (class)
  "Start a patience game of CLASS."
  (let* ((game (card-games-pat--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (card-games-pat-mode) (setq card-games-pat--game game) (card-games-pat--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun card-games-golf () "Play Golf solitaire." (interactive) (card-games-pat--play 'card-games-golf-game))
;;;###autoload
(defun card-games-tripeaks () "Play TriPeaks solitaire." (interactive) (card-games-pat--play 'card-games-tripeaks-game))
;;;###autoload
(defun card-games-pyramid () "Play Pyramid solitaire." (interactive) (card-games-pat--play 'card-games-pyramid-game))

(provide 'card-games-patience)
;;; card-games-patience.el ends here
