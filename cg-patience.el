;;; cg-patience.el --- Pile solitaires (Golf, TriPeaks, Pyramid)  -*- lexical-binding: t; -*-

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

;; Three "pile" solitaires that clear a fixed layout of cards rather than
;; building tableau columns:
;;
;;   `cg-golf'     -- move an exposed card to the waste when it is one rank
;;                    above or below the waste's top card; deal when stuck.
;;   `cg-tripeaks' -- the same, on three overlapping peaks, with Ace-King
;;                    wrapping so long chains are possible.
;;   `cg-pyramid'  -- remove pairs of exposed cards whose ranks sum to 13
;;                    (Kings go alone); deal from the stock to help.
;;
;; A board is a vector of card slots; each slot lists the slots that cover
;; it, and a slot is "exposed" (playable) once all its coverers are gone.
;; Cards are the package-standard cons (SUIT . RANK) with RANK 0 Ace .. 12
;; King; a rank's value for the sum-of-13 rule is RANK + 1.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)

(defconst cg-pat-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0 (Ace) .. 12 (King).")

(defun cg-pat-card-string (card)
  "Return a short string for CARD, or a dot for an empty slot."
  (if (null card) "·"
    (concat (aref cg-pat-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defsubst cg-pat-red-p (card) (and card (cg-red-suit-p (car card))))

(defun cg-pat--deck () (cg-shuffle (cl-loop for s below 4 append
                                            (cl-loop for r below 13 collect (cons s r)))))

;;;; Classes

(defclass cg-patience-game (cg-game)
  ((mode :initform 'build :documentation "Play mode: build (waste) or sum13.")
   (wrap :initform nil :documentation "Whether Ace-King wrap in build mode.")
   (vname :initform "Patience"))
  "Abstract base for the pile solitaires."
  :abstract t)

(defclass cg-golf-game (cg-patience-game)
  ((mode :initform 'build) (wrap :initform nil) (vname :initform "Golf")))
(defclass cg-tripeaks-game (cg-patience-game)
  ((mode :initform 'build) (wrap :initform t) (vname :initform "TriPeaks")))
(defclass cg-pyramid-game (cg-patience-game)
  ((mode :initform 'sum13) (vname :initform "Pyramid")))

;;;; Layouts -- return (CARDS-VECTOR COVER-VECTOR ROWS), ROWS for display.

(cl-defgeneric cg-pat--layout (game deck)
  "Build GAME's board from DECK; return (CARDS COVER ROWS STOCK WASTE).")

(cl-defmethod cg-pat--layout ((_ cg-golf-game) deck)
  (let ((cards (make-vector 35 nil)) (cover (make-vector 35 nil)) (rows nil))
    (dotimes (c 7) (dotimes (r 5)
      (let ((i (+ (* c 5) r)))
        (aset cards i (pop deck))
        (when (< r 4) (aset cover i (list (+ i 1)))))))
    (dotimes (r 5) (push (cl-loop for c below 7 collect (+ (* c 5) r)) rows))
    (let ((waste (list (pop deck))))
      (list cards cover (nreverse rows) deck waste))))

(cl-defmethod cg-pat--layout ((_ cg-tripeaks-game) deck)
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

(cl-defmethod cg-pat--layout ((_ cg-pyramid-game) deck)
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

(cl-defmethod cg-pat--deal ((game cg-patience-game))
  "Deal a fresh board into GAME."
  (random t)
  (cl-destructuring-bind (cards cover rows stock waste) (cg-pat--layout game (cg-pat--deck))
    (cg-put game :cards cards)
    (cg-put game :cover cover)
    (cg-put game :rows rows)
    (cg-put game :stock stock)
    (cg-put game :waste waste)
    (cg-put game :marks nil)
    (cg-put game :cursor 0)
    (cg-put game :moves 0)
    (cg-put game :history nil)
    (cg-put game :message
            (if (eq (oref game mode) 'sum13)
                "Remove pairs summing to 13; Kings go alone.  RET marks, stock deals."
              "Move a card one rank from the waste top.  RET plays; stock deals."))
    game))

(defun cg-pat--exposed-p (game i)
  "Return non-nil when board slot I is present and uncovered."
  (let ((cards (cg-get game :cards)))
    (and (aref cards i)
         (cl-every (lambda (j) (null (aref cards j))) (aref (cg-get game :cover) i)))))

(defun cg-pat--exposed (game)
  "Return the list of exposed board slot indices."
  (cl-loop for i below (length (cg-get game :cards))
           when (cg-pat--exposed-p game i) collect i))

(defun cg-pat--spots (game)
  "Return the ordered spots the cursor can visit."
  (append (mapcar (lambda (i) (cons 'slot i)) (cg-pat--exposed game))
          '((waste . 0) (stock . 0))))

(defun cg-pat--waste-top (game) (car (last (cg-get game :waste))))

(defun cg-pat--board-empty-p (game)
  "Return non-nil when every board slot has been cleared."
  (cl-every #'null (append (cg-get game :cards) nil)))

(cl-defmethod cg-won-p ((game cg-patience-game))
  "Return non-nil when the board has been cleared."
  (cg-pat--board-empty-p game))

(defun cg-pat--adjacent (a b wrap)
  "Return non-nil when ranks A and B differ by one (or wrap Ace-King)."
  (let ((d (abs (- a b)))) (or (= d 1) (and wrap (= d 12)))))

(defun cg-pat--snapshot (game)
  "Record an undo snapshot of GAME."
  (cg-put game :history
          (cons (list (copy-sequence (cg-get game :cards))
                      (copy-sequence (cg-get game :stock))
                      (copy-sequence (cg-get game :waste))
                      (cg-get game :moves))
                (cg-get game :history))))

(defun cg-pat--restore (game)
  "Undo the last move of GAME, if any."
  (let ((h (cg-get game :history)))
    (when h
      (cl-destructuring-bind (cards stock waste moves) (car h)
        (cg-put game :cards cards) (cg-put game :stock stock)
        (cg-put game :waste waste) (cg-put game :moves moves))
      (cg-put game :history (cdr h))
      (cg-put game :marks nil)
      t)))

(defun cg-pat--deal-stock (game)
  "Turn one card from the stock to the waste."
  (let ((stock (cg-get game :stock)))
    (if (null stock)
        (cg-put game :message "The stock is empty.")
      (cg-pat--snapshot game)
      (cg-put game :waste (append (cg-get game :waste) (last stock 1)))
      (cg-put game :stock (butlast stock 1))
      (cg-put game :marks nil)
      (cg-put game :message "Dealt a card."))))

(defun cg-pat--value (card) "Sum-of-13 value of CARD." (1+ (cdr card)))

(defun cg-pat--remove-slot (game i)
  "Clear board slot I."
  (aset (cg-get game :cards) i nil))

;;;; Interaction

(defvar-local cg-pat--game nil "The pile-solitaire game in the current buffer.")

(defun cg-pat--cur-spot (game)
  (let ((spots (cg-pat--spots game)))
    (nth (min (cg-get game :cursor) (1- (length spots))) spots)))

(defun cg-pat-act ()
  "Play the spot under the cursor (build move, sum-13 mark, or deal)."
  (interactive)
  (let* ((game cg-pat--game) (spot (cg-pat--cur-spot game)))
    (pcase (car spot)
      ('stock (cg-pat--deal-stock game))
      ('waste (when (eq (oref game mode) 'sum13) (cg-pat--toggle-mark game (cons 'waste 0))))
      ('slot
       (let* ((i (cdr spot)) (card (aref (cg-get game :cards) i)))
         (if (eq (oref game mode) 'build)
             (let ((top (cg-pat--waste-top game)))
               (if (and top (cg-pat--adjacent (cdr card) (cdr top) (oref game wrap)))
                   (progn (cg-pat--snapshot game)
                          (cg-put game :waste (append (cg-get game :waste) (list card)))
                          (cg-pat--remove-slot game i)
                          (cg-put game :moves (1+ (cg-get game :moves)))
                          (cg-put game :message "Played."))
                 (cg-put game :message "That card is not adjacent to the waste top.")))
           ;; sum13
           (if (= 13 (cg-pat--value card))
               (progn (cg-pat--snapshot game) (cg-pat--remove-slot game i)
                      (cg-put game :moves (1+ (cg-get game :moves)))
                      (cg-put game :marks nil)
                      (cg-put game :message "King removed."))
             (cg-pat--toggle-mark game (cons 'slot i)))))))
    (cg-pat--after game)))

(defun cg-pat--mark-value (game m)
  "Return the card value of mark M (a slot or the waste)."
  (pcase (car m)
    ('slot (cg-pat--value (aref (cg-get game :cards) (cdr m))))
    ('waste (let ((w (cg-pat--waste-top game))) (and w (cg-pat--value w))))))

(defun cg-pat--toggle-mark (game m)
  "Toggle mark M; when two marks sum to 13, remove both."
  (if (member m (cg-get game :marks))
      (cg-put game :marks (remove m (cg-get game :marks)))
    (cg-put game :marks (cons m (cg-get game :marks))))
  (let ((marks (cg-get game :marks)))
    (when (= 2 (length marks))
      (if (= 13 (+ (cg-pat--mark-value game (nth 0 marks))
                   (cg-pat--mark-value game (nth 1 marks))))
          (progn (cg-pat--snapshot game)
                 (dolist (mm marks)
                   (pcase (car mm)
                     ('slot (cg-pat--remove-slot game (cdr mm)))
                     ('waste (cg-put game :waste (butlast (cg-get game :waste) 1)))))
                 (cg-put game :moves (1+ (cg-get game :moves)))
                 (cg-put game :marks nil)
                 (cg-put game :message "Pair removed."))
        (cg-put game :marks nil)
        (cg-put game :message "Those do not sum to 13.")))))

(defun cg-pat--after (game)
  "Redisplay GAME and announce a win."
  (cg-pat--redisplay)
  (when (cg-won-p game)
    (cg-put game :message "Board cleared -- you won!  Press n for a new game.")
    (cg-pat--redisplay)
    (message "Solved!")))

(defun cg-pat--move (delta)
  (let* ((game cg-pat--game) (n (length (cg-pat--spots game))))
    (cg-put game :cursor (mod (+ (cg-get game :cursor) delta) n))
    (cg-pat--redisplay)))

(defun cg-pat-left () "Cursor left." (interactive) (cg-pat--move -1))
(defun cg-pat-right () "Cursor right." (interactive) (cg-pat--move 1))
(defun cg-pat-undo () "Undo." (interactive)
  (let ((game cg-pat--game))
    (cg-put game :message (if (cg-pat--restore game) "Undid a move." "Nothing to undo."))
    (cg-pat--redisplay)))
(defun cg-pat-new () "New deal." (interactive)
  (cg-pat--deal cg-pat--game) (cg-pat--redisplay))
(defun cg-pat-redraw () "Redraw." (interactive) (cg-pat--redisplay))
(defun cg-pat-help () "Controls." (interactive)
  (message "Arrows: move  RET: play/mark/deal  u: undo  n: new  g: redraw"))

;;;; Rendering

(defun cg-pat--render-card (card &optional exposed marked cursor)
  (let ((s (cg-pat-card-string card)) (faces nil))
    (when (cg-pat-red-p card) (push 'cg-red-suit faces))
    (when (and card (not exposed)) (push 'cg-gap faces))
    (when marked (push 'cg-hint faces))
    (when cursor (push 'cg-cursor faces))
    (propertize (format "%4s" s) 'face (or faces 'default))))

(cl-defmethod cg-render ((game cg-patience-game))
  "Return a propertized string depicting GAME for a text display."
  (let* ((cur (cg-pat--cur-spot game)) (marks (cg-get game :marks)) (out (list)))
    (push (format "  %s   Moves: %d\n\n" (oref game vname) (cg-get game :moves)) out)
    (dolist (row (cg-get game :rows))
      (push "  " out)
      (dolist (i row)
        (let* ((card (aref (cg-get game :cards) i))
               (exp (cg-pat--exposed-p game i))
               (mk (member (cons 'slot i) marks))
               (cz (equal cur (cons 'slot i))))
          (push (if card (cg-pat--render-card card exp mk cz) "    ") out)))
      (push "\n" out))
    (push (format "\n  Waste: %s    Stock: %d\n"
                  (let ((w (cg-pat--waste-top game)))
                    (cg-pat--render-card w t (member '(waste . 0) marks)
                                         (equal cur '(waste . 0))))
                  (length (cg-get game :stock)))
          out)
    (push (format "  %s\n" (if (equal cur '(stock . 0))
                               (propertize "[stock]" 'face 'cg-cursor) "")) out)
    (push (format "\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-pat--redisplay ()
  (let ((game cg-pat--game) (inhibit-read-only t))
    (setq-local mode-line-process (format " [%s]" (if (cg-won-p game) "solved" "playing")))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

;;;; Mode and commands

(defvar cg-pat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'cg-pat-left)
    (define-key map (kbd "<right>") #'cg-pat-right)
    (define-key map (kbd "<up>") #'cg-pat-left)
    (define-key map (kbd "<down>") #'cg-pat-right)
    (define-key map (kbd "RET") #'cg-pat-act)
    (define-key map (kbd "SPC") #'cg-pat-act)
    (define-key map "u" #'cg-pat-undo)
    (define-key map "n" #'cg-pat-new)
    (define-key map "g" #'cg-pat-redraw)
    (define-key map "?" #'cg-pat-help)
    map)
  "Keymap for `cg-pat-mode'.")

(define-derived-mode cg-pat-mode special-mode "Patience"
  "Major mode for the pile solitaires."
  (setq-local truncate-lines t))

(defun cg-pat--play (class)
  (let* ((game (cg-pat--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (cg-pat-mode) (setq cg-pat--game game) (cg-pat--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun cg-golf () "Play Golf solitaire." (interactive) (cg-pat--play 'cg-golf-game))
;;;###autoload
(defun cg-tripeaks () "Play TriPeaks solitaire." (interactive) (cg-pat--play 'cg-tripeaks-game))
;;;###autoload
(defun cg-pyramid () "Play Pyramid solitaire." (interactive) (cg-pat--play 'cg-pyramid-game))

(provide 'cg-patience)
;;; cg-patience.el ends here
