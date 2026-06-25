;;; cg-rum500.el --- Basic Rummy and Rummy 500  -*- lexical-binding: t; -*-

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

;; Two table-meld rummy games sharing one engine, built on the meld
;; finder in cg-rummy.el.
;;
;; In a table-meld game you draw a card, lay melds face-up on the table,
;; lay single cards off onto melds already there, and end your turn by
;; discarding.  Empty your hand to go out.
;;
;;   `cg-rummy-basic' -- plain Rummy: the first player to meld their whole
;;     hand wins the deal and scores the cards left in the others' hands.
;;   `cg-rum500' -- Rummy 500: you score the cards you lay down and lose
;;     the cards left in your hand; first past 500 wins.
;;
;; You are the South player (seat 0); the rest are simple AI.  To meld,
;; mark cards with SPC and press m; to lay a card off, put the cursor on
;; it and press l.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)
(require 'cg-rummy)

(defclass cg-tablemeld-game (cg-rummy-game)
  ((nplayers  :initarg :nplayers  :initform 2)
   (hand-size :initarg :hand-size :initform 10)
   (ace-15    :initarg :ace-15    :initform nil)
   (ace-high  :initarg :ace-high  :initform nil)
   (target    :initarg :target    :initform 100)
   (score-style :initarg :score-style :initform 'go-out))
  "Abstract base for table-meld rummy games (Basic Rummy, Rummy 500)."
  :abstract t)

;;;; Engine

(cl-defmethod cg-tm--deal ((game cg-tablemeld-game))
  "Deal a fresh hand into GAME."
  (let* ((n (oref game nplayers))
         (deck (cg-rummy-deck))
         (per (oref game hand-size))
         (hands (make-vector n nil)))
    (dotimes (s n)
      (aset hands s (cg-rummy-sort-hand (cl-loop repeat per collect (pop deck)))))
    (cg-put game :hands hands)
    (cg-put game :nplayers n)
    (cg-put game :discard (list (pop deck)))
    (cg-put game :stock deck)
    (cg-put game :table nil)
    (cg-put game :laid (make-vector n 0))
    (cg-put game :turn 0)
    (cg-put game :step 'draw)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (cg-put game :marks nil)
    (unless (cg-get game :scores) (cg-put game :scores (make-vector n 0)))
    (cg-put game :message "Your turn: s draws from stock, t takes the discard.")
    game))

(defun cg-tm--ace-high (game) (oref game ace-high))
(defun cg-tm--ace-15 (game) (oref game ace-15))

(defun cg-tm--draw (game s)
  "Move one stock card to seat S's hand; return it or nil if stock empty."
  (let ((stock (cg-get game :stock)))
    (when stock
      (let ((c (pop stock)))
        (cg-put game :stock stock)
        (cg-rummy--set-hand game s (cg-rummy-sort-hand
                                    (cons c (cg-rummy--hand game s))))
        c))))

(defun cg-tm--take-top (game s)
  "Move the discard top to seat S's hand and return it."
  (let ((c (cg-rummy--top game)))
    (when c
      (cg-put game :discard (cdr (cg-get game :discard)))
      (cg-rummy--set-hand game s (cg-rummy-sort-hand
                                  (cons c (cg-rummy--hand game s))))
      c)))

(defun cg-tm--meld-value (game cards)
  "Return the total point value of CARDS for GAME's scoring."
  (apply #'+ (mapcar (lambda (c) (cg-rummy-value c (cg-tm--ace-15 game))) cards)))

(cl-defmethod cg-tm--meld ((game cg-tablemeld-game) s cards)
  "Have seat S meld CARDS onto the table.  Return non-nil on success."
  (when (cg-rummy-meld-p cards :min 3 :ace-high (cg-tm--ace-high game)
                         :distinct-suits t)
    (dolist (c cards) (cg-rummy--set-hand game s (remove c (cg-rummy--hand game s))))
    (cg-put game :table (append (cg-get game :table)
                                (list (cons s (copy-sequence cards)))))
    (let ((laid (cg-get game :laid)))
      (aset laid s (+ (aref laid s) (cg-tm--meld-value game cards))))
    t))

(cl-defmethod cg-tm--layoff ((game cg-tablemeld-game) s card)
  "Have seat S lay CARD off onto a matching table meld.  Return non-nil on success."
  (let ((rec (cl-find-if
              (lambda (r) (cg-rummy-meld-p (cons card (cdr r)) :min 3
                                           :ace-high (cg-tm--ace-high game)))
              (cg-get game :table))))
    (when rec
      (cg-rummy--set-hand game s (remove card (cg-rummy--hand game s)))
      (setcdr rec (cg-rummy-sort-hand (cons card (cdr rec))))
      (let ((laid (cg-get game :laid)))
        (aset laid s (+ (aref laid s) (cg-rummy-value card (cg-tm--ace-15 game)))))
      t)))

(cl-defmethod cg-tm--score-hand ((game cg-tablemeld-game) outseat)
  "Score the hand ended by OUTSEAT (or nil for a washed-out hand)."
  (let* ((n (cg-get game :nplayers)) (scores (cg-get game :scores))
         (style (oref game score-style)))
    (cond
     ((eq style 'go-out)
      (when outseat
        (let ((sum 0))
          (dotimes (s n)
            (unless (= s outseat)
              (dolist (c (cg-rummy--hand game s))
                (setq sum (+ sum (cg-rummy-value c))))))
          (aset scores outseat (+ (aref scores outseat) sum)))))
     ((eq style 'meld-points)
      (let ((laid (cg-get game :laid)))
        (dotimes (s n)
          (let ((rem (apply #'+ (mapcar (lambda (c)
                                          (cg-rummy-value c (cg-tm--ace-15 game)))
                                        (cg-rummy--hand game s)))))
            (aset scores s (+ (aref scores s) (- (aref laid s) rem))))))))
    ;; decide if the game is over
    (let ((win nil) (best most-negative-fixnum))
      (dotimes (s n)
        (when (and (>= (aref scores s) (oref game target))
                   (> (aref scores s) best))
          (setq win s best (aref scores s))))
      (cg-put game :phase (if win 'game-over 'hand-over))
      (cg-put game :winner (or win outseat))
      (cg-put game :reveal t)
      (cg-put game :message
              (if win
                  (format "%s wins the game with %d!  (n: new game)"
                          (cg-tm--who win) (aref scores win))
                (concat (if outseat (format "%s goes out.  " (cg-tm--who outseat))
                          "Stock exhausted.  ")
                        (format "Scores: %s.  (n: next hand)"
                                (cg-tm--scores-string game))))))))

(defun cg-tm--who (s) (if (= s 0) "You" (format "Player %d" s)))

(defun cg-tm--scores-string (game)
  "Return a compact \"You N · P1 N ...\" score line for GAME."
  (let ((scores (cg-get game :scores)) (parts '()))
    (dotimes (s (cg-get game :nplayers))
      (push (format "%s %d" (if (= s 0) "You" (format "P%d" s)) (aref scores s))
            parts))
    (mapconcat #'identity (nreverse parts) " · ")))

(cl-defmethod cg-tm--end-turn ((game cg-tablemeld-game) s)
  "Finish seat S's turn: go out if the hand is empty, else advance."
  (if (null (cg-rummy--hand game s))
      (cg-tm--score-hand game s)
    (cg-put game :turn (mod (1+ s) (cg-get game :nplayers)))
    (cg-put game :step 'draw)))

(cl-defmethod cg-tm--discard ((game cg-tablemeld-game) s card)
  "Discard CARD from seat S and finish the turn."
  (cg-rummy--set-hand game s (remove card (cg-rummy--hand game s)))
  (cg-put game :discard (cons card (cg-get game :discard)))
  (cg-tm--end-turn game s))

;;;; AI

(defun cg-tm--ai-melds (game s)
  "Lay down every meld seat S can, keeping a card back to discard.
Return non-nil if any meld was laid."
  (let ((did nil) (again t))
    (while again
      (setq again nil)
      (let* ((hand (cg-rummy--hand game s))
             (p (cg-rummy-best-partition hand :ace-high (cg-tm--ace-high game)
                                         :ace-15 (cg-tm--ace-15 game)))
             (melds (plist-get p :melds))
             ;; keep one card to discard: skip a meld if it would empty the hand
             (melded (apply #'+ (mapcar #'length melds))))
        (when (and melds (= melded (length hand)))
          (setq melds (cdr (sort melds (lambda (a b) (< (length a) (length b)))))))
        (when melds
          (cg-tm--meld game s (car melds))
          (setq did t again t))))
    did))

(defun cg-tm--ai-layoffs (game s)
  "Lay off every fitting card from seat S, keeping a card back to discard."
  (let ((again t))
    (while again
      (setq again nil)
      (when (> (length (cg-rummy--hand game s)) 1)
        (let ((card (cl-find-if
                     (lambda (c)
                       (cl-find-if
                        (lambda (r) (cg-rummy-meld-p (cons c (cdr r)) :min 3
                                                     :ace-high (cg-tm--ace-high game)))
                        (cg-get game :table)))
                     (cg-rummy--hand game s))))
          (when card (cg-tm--layoff game s card) (setq again t)))))))

(defun cg-tm--ai-discard-card (game s)
  "Return the best card for seat S to discard (highest deadwood)."
  (let* ((hand (cg-rummy--hand game s))
         (p (cg-rummy-best-partition hand :ace-high (cg-tm--ace-high game)
                                     :ace-15 (cg-tm--ace-15 game)))
         (dead (or (plist-get p :deadwood) hand))
         (best (car dead)) (bestv -1))
    (dolist (c dead best)
      (let ((v (cg-rummy-value c (cg-tm--ace-15 game))))
        (when (> v bestv) (setq best c bestv v))))))

(cl-defmethod cg-tm--ai-turn ((game cg-tablemeld-game) s)
  "Play seat S's whole turn."
  (let* ((hand (cg-rummy--hand game s))
         (up (cg-rummy--top game))
         (cur (cg-rummy-deadwood hand (cg-tm--ace-high game) (cg-tm--ace-15 game)))
         (with (and up (cg-rummy-deadwood (cons up hand)
                                          (cg-tm--ace-high game) (cg-tm--ace-15 game))))
         (drew (if (and up with (< with cur))
                   (cg-tm--take-top game s)
                 (cg-tm--draw game s))))
    (if (not drew)
        (cg-tm--score-hand game nil)
      (cg-tm--ai-melds game s)
      (cg-tm--ai-layoffs game s)
      (when (eq (cg-get game :phase) 'play)
        (if (null (cg-rummy--hand game s))
            (cg-tm--end-turn game s)          ; melded out, no discard needed
          (cg-tm--discard game s (cg-tm--ai-discard-card game s)))))))

(defun cg-tm--run (game)
  "Advance AI seats until it is the human's turn or the hand ends."
  (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0))
    (cg-tm--ai-turn game (cg-get game :turn))))

;;;; UI

(defvar-local cg-tm--game nil "The table-meld game in the current buffer.")

(defun cg-tm--layoff-hint (game)
  "Return a predicate marking cards that can be laid off in GAME now."
  (lambda (c)
    (cl-find-if (lambda (r) (cg-rummy-meld-p (cons c (cdr r)) :min 3
                                             :ace-high (cg-tm--ace-high game)))
                (cg-get game :table))))

(cl-defmethod cg-render ((game cg-tablemeld-game))
  "Return a propertized depiction of the table-meld GAME."
  (let* ((out '()) (scores (cg-get game :scores))
         (laid (cg-get game :laid)) (meldp (oref game score-style))
         (hand (cg-rummy--hand game 0)) (cursor (cg-get game :cursor)))
    (push (format "  %s        target %d\n\n" (oref game vname) (oref game target)) out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   score %d%s\n"
                      s (length (cg-rummy--hand game s)) (aref scores s)
                      (if (eq meldp 'meld-points)
                          (format "  (laid %d)" (aref laid s)) ""))
              out)))
    (push "\n  Table:\n" out)
    (if (cg-get game :table)
        (dolist (rec (cg-get game :table))
          (push (format "    [%s] %s\n" (if (= (car rec) 0) "you" (format "P%d" (car rec)))
                        (mapconcat #'cg-rummy-card-string (cdr rec) " "))
                out))
      (push "    (empty)\n" out))
    (push (format "\n  Discard: %s     Stock: %d\n\n"
                  (let ((cs (cg-rummy-card-string (cg-rummy--top game))) (tp (cg-rummy--top game)))
                    (if (and tp (cg-red-suit-p (car tp))) (propertize cs 'face 'cg-red-suit) cs))
                  (length (cg-get game :stock)))
          out)
    (push (format "  Your hand%s:\n   "
                  (if (eq meldp 'meld-points) (format "  (laid %d, score %d)"
                                                       (aref laid 0) (aref scores 0))
                    (format "  (score %d)" (aref scores 0))))
          out)
    (push (cg-rummy--render-cards hand cursor (cg-get game :marks)
                                  (cg-tm--layoff-hint game))
          out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-tm--redisplay ()
  "Redraw the table-meld buffer."
  (let ((game cg-tm--game) (inhibit-read-only t))
    (setq-local mode-line-process
                (format " [%s]" (or (cg-get game :step) (cg-get game :phase))))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-tm--clamp-cursor (g)
  "Keep G's cursor within the hand and drop stale marks."
  (let ((n (length (cg-rummy--hand g 0))))
    (cg-put g :cursor (if (> n 0) (min (cg-get g :cursor) (1- n)) 0))
    (cg-put g :marks (cl-remove-if (lambda (i) (>= i n)) (cg-get g :marks)))))

(defun cg-tm--my-turn-p (g)
  (and (eq (cg-get g :phase) 'play) (= (cg-get g :turn) 0)))

(defun cg-tm-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g cg-tm--game) (n (length (cg-rummy--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-tm--redisplay)))

(defun cg-tm-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g cg-tm--game) (n (length (cg-rummy--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-tm--redisplay)))

(defun cg-tm-mark ()
  "Toggle a mark on the card under the cursor (for melding)."
  (interactive)
  (let* ((g cg-tm--game) (i (cg-get g :cursor)) (marks (cg-get g :marks)))
    (cg-put g :marks (if (memq i marks) (delq i marks) (cons i marks)))
    (cg-tm--redisplay)))

(defun cg-tm--marked-cards (g)
  "Return the cards currently marked in G's hand."
  (let ((hand (cg-rummy--hand g 0)))
    (mapcar (lambda (i) (nth i hand)) (sort (copy-sequence (cg-get g :marks)) #'<))))

(defun cg-tm-meld ()
  "Meld the marked cards onto the table."
  (interactive)
  (let* ((g cg-tm--game) (cards (cg-tm--marked-cards g)))
    (cond
     ((not (cg-tm--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s or t)."))
     ((< (length cards) 3) (cg-put g :message "Mark at least three cards (SPC), then m."))
     ((cg-tm--meld g 0 cards)
      (cg-put g :marks nil) (cg-tm--clamp-cursor g)
      (cg-put g :message "Melded.  Lay off with l, meld more, or discard (RET)."))
     (t (cg-put g :message "Those cards are not a valid set or run.")))
    (cg-tm--redisplay)))

(defun cg-tm-layoff ()
  "Lay the cursor card (or marked cards) off onto a table meld."
  (interactive)
  (let* ((g cg-tm--game) (marks (cg-tm--marked-cards g)))
    (cond
     ((not (cg-tm--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s or t)."))
     (t (let ((cards (or marks (list (nth (cg-get g :cursor) (cg-rummy--hand g 0)))))
              (any nil))
          (dolist (c cards) (when (and c (cg-tm--layoff g 0 c)) (setq any t)))
          (cg-put g :marks nil) (cg-tm--clamp-cursor g)
          (cg-put g :message (if any "Laid off." "That card fits no meld on the table.")))))
    (cg-tm--redisplay)))

(defun cg-tm-draw-stock ()
  "Draw the top stock card."
  (interactive)
  (let ((g cg-tm--game))
    (cond
     ((not (cg-tm--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((not (eq (cg-get g :step) 'draw)) (cg-put g :message "You already drew."))
     ((cg-tm--draw g 0)
      (cg-put g :step 'play) (cg-tm--clamp-cursor g)
      (cg-put g :message "Meld (m), lay off (l), then discard (RET)."))
     (t (cg-tm--score-hand g nil)))
    (cg-tm--redisplay)))

(defun cg-tm-take ()
  "Take the discard top into your hand."
  (interactive)
  (let ((g cg-tm--game))
    (cond
     ((not (cg-tm--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((not (eq (cg-get g :step) 'draw)) (cg-put g :message "You already drew."))
     ((null (cg-rummy--top g)) (cg-put g :message "The discard pile is empty."))
     (t (let ((c (cg-tm--take-top g 0)))
          (cg-put g :step 'play) (cg-tm--clamp-cursor g)
          (cg-put g :message (format "Took %s.  Meld (m), lay off (l), discard (RET)."
                                     (cg-rummy-card-string c))))))
    (cg-tm--redisplay)))

(defun cg-tm-discard ()
  "Discard the cursor card and end your turn."
  (interactive)
  (let* ((g cg-tm--game) (card (nth (cg-get g :cursor) (cg-rummy--hand g 0))))
    (cond
     ((not (cg-tm--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s or t)."))
     ((null card) (cg-put g :message "No card selected."))
     (t (cg-tm--discard g 0 card)
        (cg-put g :marks nil)
        (when (eq (cg-get g :phase) 'play)
          (cg-put g :message "You discarded.")
          (cg-tm--run g))))
    (cg-tm--redisplay)))

(defun cg-tm-new ()
  "Deal a fresh hand, or a new game when one is over."
  (interactive)
  (let ((g cg-tm--game))
    (when (eq (cg-get g :phase) 'game-over)
      (cg-put g :scores (make-vector (oref g nplayers) 0)))
    (cg-put g :reveal nil)
    (cg-tm--deal g)
    (cg-tm--run g)
    (cg-tm--redisplay)))

(defun cg-tm-redraw () "Redraw the board." (interactive) (cg-tm--redisplay))

(defun cg-tm-help ()
  "Describe the table-meld controls."
  (interactive)
  (message "Arrows: choose  SPC: mark  m: meld  l: lay off  s: draw  t: take  RET: discard  n: new"))

(defvar cg-tm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-tm-left)
    (define-key map (kbd "<right>") #'cg-tm-right)
    (define-key map (kbd "SPC") #'cg-tm-mark)
    (define-key map "m" #'cg-tm-meld)
    (define-key map "l" #'cg-tm-layoff)
    (define-key map "s" #'cg-tm-draw-stock)
    (define-key map "t" #'cg-tm-take)
    (define-key map (kbd "RET") #'cg-tm-discard)
    (define-key map "n" #'cg-tm-new)
    (define-key map "g" #'cg-tm-redraw)
    (define-key map "?" #'cg-tm-help)
    map)
  "Keymap for `cg-tm-mode'.")

(define-derived-mode cg-tm-mode special-mode "Rummy"
  "Major mode for the table-meld rummy games."
  (setq-local truncate-lines t))

(defun cg-tm--start (game buffer-name)
  "Start GAME in a buffer named BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (cg-tm-mode)
      (setq cg-tm--game game)
      (cg-tm--deal game)
      (cg-tm--run game)
      (cg-tm--redisplay))
    (switch-to-buffer buf)))

;;;; The two games

(defcustom cg-rummy-basic-players 2
  "Number of players in Basic Rummy, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defclass cg-rummy-basic-game (cg-tablemeld-game)
  ((vname :initform "Rummy")
   (score-style :initform 'go-out)
   (target :initform 100))
  "A game of plain Rummy.")

;;;###autoload
(defun cg-rummy-basic ()
  "Play Basic Rummy against the computer."
  (interactive)
  (let ((n (max 2 (min 4 cg-rummy-basic-players))))
    (cg-tm--start (cg-rummy-basic-game :nplayers n :hand-size (if (= n 2) 10 7))
                  "*Rummy*")))

(defcustom cg-rum500-players 3
  "Number of players in Rummy 500, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defclass cg-rum500-game (cg-tablemeld-game)
  ((vname :initform "Rummy 500")
   (score-style :initform 'meld-points)
   (ace-15 :initform t)
   (ace-high :initform t)
   (target :initform 500))
  "A game of Rummy 500.")

;;;###autoload
(defun cg-rum500 ()
  "Play Rummy 500 against the computer."
  (interactive)
  (let ((n (max 2 (min 4 cg-rum500-players))))
    (cg-tm--start (cg-rum500-game :nplayers n :hand-size (if (= n 2) 13 7))
                  "*Rummy 500*")))

;;;###autoload
(defalias 'cg-rummy-500 #'cg-rum500)

(provide 'cg-rum500)
;;; 