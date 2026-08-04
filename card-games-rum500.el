;;; card-games-rum500.el --- Basic Rummy and Rummy 500  -*- lexical-binding: t; -*-

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

;; Two table-meld rummy games sharing one engine, built on the meld
;; finder in card-games-rummy.el.
;;
;; In a table-meld game you draw a card, lay melds face-up on the table,
;; lay single cards off onto melds already there, and end your turn by
;; discarding.  Empty your hand to go out.
;;
;;   `card-games-rummy-basic' -- plain Rummy: the first player to meld their whole
;;     hand wins the deal and scores the cards left in the others' hands.
;;   `card-games-rum500' -- Rummy 500: you score the cards you lay down and lose
;;     the cards left in your hand; first past 500 wins.  In Rummy 500 you
;;     may take a card from anywhere in the discard pile (T): you take that
;;     card and everything above it, and the chosen card is melded at once.
;;
;; You are the South player (seat 0); the rest are simple AI.  To meld,
;; mark cards with SPC and press m; to lay a card off, put the cursor on
;; it and press l.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-rummy)

(defclass card-games-tablemeld-game (card-games-rummy-game)
  ((nplayers  :initarg :nplayers  :initform 2)
   (hand-size :initarg :hand-size :initform 10)
   (ace-15    :initarg :ace-15    :initform nil)
   (ace-high  :initarg :ace-high  :initform nil)
   (target    :initarg :target    :initform 100)
   (score-style :initarg :score-style :initform 'go-out)
   (deep-pickup :initarg :deep-pickup :initform nil))
  "Abstract base for table-meld rummy games (Basic Rummy, Rummy 500)."
  :abstract t)

;;;; Engine

(cl-defmethod card-games-tm--deal ((game card-games-tablemeld-game))
  "Deal a fresh hand into GAME."
  (let* ((n (oref game nplayers))
         (deck (card-games-rummy-deck))
         (per (oref game hand-size))
         (hands (make-vector n nil)))
    (dotimes (s n)
      (aset hands s (card-games-rummy-sort-hand (cl-loop repeat per collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :nplayers n)
    (card-games-put game :discard (list (pop deck)))
    (card-games-put game :stock deck)
    (card-games-put game :table nil)
    (card-games-put game :laid (make-vector n 0))
    (card-games-put game :turn 0)
    (card-games-put game :step 'draw)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (card-games-put game :marks nil)
    (unless (card-games-get game :scores) (card-games-put game :scores (make-vector n 0)))
    (card-games-put game :message "Your turn: s draws from stock, t takes the discard.")
    game))

(defun card-games-tm--ace-high (game) (oref game ace-high))
(defun card-games-tm--ace-15 (game) (oref game ace-15))
(defun card-games-tm--deep-pickup (game) (oref game deep-pickup))

(defun card-games-tm--draw (game s)
  "Move one stock card to seat S's hand; return it or nil if stock empty."
  (let ((stock (card-games-get game :stock)))
    (when stock
      (let ((c (pop stock)))
        (card-games-put game :stock stock)
        (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                    (cons c (card-games-rummy--hand game s))))
        c))))

(defun card-games-tm--take-top (game s)
  "Move the discard top to seat S's hand and return it."
  (let ((c (card-games-rummy--top game)))
    (when c
      (card-games-put game :discard (cdr (card-games-get game :discard)))
      (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                  (cons c (card-games-rummy--hand game s))))
      c)))

(defun card-games-tm--meld-value (game cards)
  "Return the total point value of CARDS for GAME's scoring."
  (apply #'+ (mapcar (lambda (c) (card-games-rummy-value c (card-games-tm--ace-15 game))) cards)))

(cl-defmethod card-games-tm--meld ((game card-games-tablemeld-game) s cards)
  "Have seat S meld CARDS onto the table.  Return non-nil on success."
  (when (card-games-rummy-meld-p cards :min 3 :ace-high (card-games-tm--ace-high game)
                         :distinct-suits t)
    (dolist (c cards) (card-games-rummy--set-hand game s (remove c (card-games-rummy--hand game s))))
    (card-games-put game :table (append (card-games-get game :table)
                                (list (cons s (copy-sequence cards)))))
    (let ((laid (card-games-get game :laid)))
      (aset laid s (+ (aref laid s) (card-games-tm--meld-value game cards))))
    t))

(cl-defmethod card-games-tm--layoff ((game card-games-tablemeld-game) s card)
  "Have seat S lay CARD off onto a matching table meld.  Return non-nil on success."
  (let ((rec (cl-find-if
              (lambda (r) (card-games-rummy-meld-p (cons card (cdr r)) :min 3
                                           :ace-high (card-games-tm--ace-high game)))
              (card-games-get game :table))))
    (when rec
      (card-games-rummy--set-hand game s (remove card (card-games-rummy--hand game s)))
      (setcdr rec (card-games-rummy-sort-hand (cons card (cdr rec))))
      (let ((laid (card-games-get game :laid)))
        (aset laid s (+ (aref laid s) (card-games-rummy-value card (card-games-tm--ace-15 game)))))
      t)))

(cl-defmethod card-games-tm--score-hand ((game card-games-tablemeld-game) outseat)
  "Score the hand ended by OUTSEAT (or nil for a washed-out hand)."
  (let* ((n (card-games-get game :nplayers)) (scores (card-games-get game :scores))
         (style (oref game score-style)))
    (cond
     ((eq style 'go-out)
      (when outseat
        (let ((sum 0))
          (dotimes (s n)
            (unless (= s outseat)
              (dolist (c (card-games-rummy--hand game s))
                (setq sum (+ sum (card-games-rummy-value c))))))
          (aset scores outseat (+ (aref scores outseat) sum)))))
     ((eq style 'meld-points)
      (let ((laid (card-games-get game :laid)))
        (dotimes (s n)
          (let ((rem (apply #'+ (mapcar (lambda (c)
                                          (card-games-rummy-value c (card-games-tm--ace-15 game)))
                                        (card-games-rummy--hand game s)))))
            (aset scores s (+ (aref scores s) (- (aref laid s) rem))))))))
    ;; decide if the game is over
    (let ((win nil) (best most-negative-fixnum))
      (dotimes (s n)
        (when (and (>= (aref scores s) (oref game target))
                   (> (aref scores s) best))
          (setq win s best (aref scores s))))
      (card-games-put game :phase (if win 'game-over 'hand-over))
      (card-games-put game :winner (or win outseat))
      (card-games-put game :reveal t)
      (card-games-put game :message
              (if win
                  (format "%s wins the game with %d!  (n: new game)"
                          (card-games-tm--who win) (aref scores win))
                (concat (if outseat (format "%s goes out.  " (card-games-tm--who outseat))
                          "Stock exhausted.  ")
                        (format "Scores: %s.  (n: next hand)"
                                (card-games-tm--scores-string game))))))))

(defun card-games-tm--who (s) (if (= s 0) "You" (format "Player %d" s)))

(defun card-games-tm--scores-string (game)
  "Return a compact \"You N · P1 N ...\" score line for GAME."
  (let ((scores (card-games-get game :scores)) (parts '()))
    (dotimes (s (card-games-get game :nplayers))
      (push (format "%s %d" (if (= s 0) "You" (format "P%d" s)) (aref scores s))
            parts))
    (mapconcat #'identity (nreverse parts) " · ")))

(cl-defmethod card-games-tm--end-turn ((game card-games-tablemeld-game) s)
  "Finish seat S's turn: go out if the hand is empty, else advance."
  (if (null (card-games-rummy--hand game s))
      (card-games-tm--score-hand game s)
    (card-games-put game :turn (mod (1+ s) (card-games-get game :nplayers)))
    (card-games-put game :step 'draw)))

(cl-defmethod card-games-tm--discard ((game card-games-tablemeld-game) s card)
  "Discard CARD from seat S and finish the turn."
  (card-games-rummy--set-hand game s (remove card (card-games-rummy--hand game s)))
  (card-games-put game :discard (cons card (card-games-get game :discard)))
  (card-games-tm--end-turn game s))

;;;; AI

(defun card-games-tm--ai-melds (game s)
  "Lay down every meld seat S can, keeping a card back to discard.
Return non-nil if any meld was laid."
  (let ((did nil) (again t))
    (while again
      (setq again nil)
      (let* ((hand (card-games-rummy--hand game s))
             (p (card-games-rummy-best-partition hand :ace-high (card-games-tm--ace-high game)
                                         :ace-15 (card-games-tm--ace-15 game)))
             (melds (plist-get p :melds))
             ;; keep one card to discard: skip a meld if it would empty the hand
             (melded (apply #'+ (mapcar #'length melds))))
        (when (and melds (= melded (length hand)))
          (setq melds (cdr (sort melds (lambda (a b) (< (length a) (length b)))))))
        (when melds
          (card-games-tm--meld game s (car melds))
          (setq did t again t))))
    did))

(defun card-games-tm--ai-layoffs (game s)
  "Lay off every fitting card from seat S, keeping a card back to discard."
  (let ((again t))
    (while again
      (setq again nil)
      (when (> (length (card-games-rummy--hand game s)) 1)
        (let ((card (cl-find-if
                     (lambda (c)
                       (cl-find-if
                        (lambda (r) (card-games-rummy-meld-p (cons c (cdr r)) :min 3
                                                     :ace-high (card-games-tm--ace-high game)))
                        (card-games-get game :table)))
                     (card-games-rummy--hand game s))))
          (when card (card-games-tm--layoff game s card) (setq again t)))))))

(defun card-games-tm--ai-discard-card (game s)
  "Return the best card for seat S to discard (highest deadwood)."
  (let* ((hand (card-games-rummy--hand game s))
         (p (card-games-rummy-best-partition hand :ace-high (card-games-tm--ace-high game)
                                     :ace-15 (card-games-tm--ace-15 game)))
         (dead (or (plist-get p :deadwood) hand))
         (best (car dead)) (bestv -1))
    (dolist (c dead best)
      (let ((v (card-games-rummy-value c (card-games-tm--ace-15 game))))
        (when (> v bestv) (setq best c bestv v))))))

(defun card-games-tm--meld-for-target (game cards target)
  "Return a minimal valid meld (card list) containing TARGET drawn from CARDS.
Return nil when TARGET cannot join a set or run with the other CARDS."
  (let* ((ace-high (card-games-tm--ace-high game))
         (pool (cons target cards))
         (cands (card-games-rummy--candidate-melds pool :ace-high ace-high))
         (vec (vconcat pool))
         (withtgt (cl-remove-if-not (lambda (m) (memq 0 m)) cands)))
    (when withtgt
      (setq withtgt (sort withtgt (lambda (a b) (< (length a) (length b)))))
      (mapcar (lambda (i) (aref vec i)) (car withtgt)))))

(defun card-games-tm--take-deep (game s depth)
  "Seat S takes the card DEPTH-deep in the discard pile, plus all above it.
The chosen card is melded or laid off at once, as Rummy 500 requires; the
rest enter the hand.  Return a status string, or nil when the move is not
legal (the chosen card cannot be used immediately)."
  (let* ((pile (card-games-get game :discard)) (n (length pile)))
    (when (and (card-games-tm--deep-pickup game) (>= depth 0) (< depth n))
      (let* ((target (nth depth pile))
             (above (cl-subseq pile 0 depth))
             (avail (append (card-games-rummy--hand game s) above))
             (lay (cl-find-if
                   (lambda (r)
                     (card-games-rummy-meld-p (cons target (cdr r)) :min 3
                                      :ace-high (card-games-tm--ace-high game)))
                   (card-games-get game :table)))
             (meld (unless lay (card-games-tm--meld-for-target game avail target))))
        (when (or lay meld)
          (let ((taken (cl-subseq pile 0 (1+ depth))))
            (card-games-put game :discard (nthcdr (1+ depth) pile))
            (dolist (c taken)
              (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                          (cons c (card-games-rummy--hand game s))))))
          (if lay (card-games-tm--layoff game s target) (card-games-tm--meld game s meld))
          (format "Took %d card%s and used %s."
                  (1+ depth) (if (= depth 0) "" "s")
                  (card-games-rummy-card-string target)))))))

(defun card-games-tm--ai-deep-pickup (game s)
  "Try a worthwhile below-the-top discard pickup for seat S.
Return non-nil when one was taken."
  (when (card-games-tm--deep-pickup game)
    (let* ((pile (card-games-get game :discard)) (n (length pile))
           (limit (min n 7)) (hand (card-games-rummy--hand game s)) (chosen nil))
      (cl-loop for d from 1 below limit
               for target = (nth d pile)
               for above = (cl-subseq pile 0 d)
               when (card-games-tm--meld-for-target game (append hand above) target)
               do (setq chosen d) (cl-return))
      (when chosen (card-games-tm--take-deep game s chosen)))))

(cl-defmethod card-games-tm--ai-turn ((game card-games-tablemeld-game) s)
  "Play seat S's whole turn."
  (let* ((deep (card-games-tm--ai-deep-pickup game s))
         (drew (if deep t
                 (let* ((hand (card-games-rummy--hand game s))
                        (up (card-games-rummy--top game))
                        (cur (card-games-rummy-deadwood hand (card-games-tm--ace-high game)
                                                (card-games-tm--ace-15 game)))
                        (with (and up (card-games-rummy-deadwood
                                       (cons up hand)
                                       (card-games-tm--ace-high game) (card-games-tm--ace-15 game)))))
                   (if (and up with (< with cur))
                       (card-games-tm--take-top game s)
                     (card-games-tm--draw game s))))))
    (if (not drew)
        (card-games-tm--score-hand game nil)
      (card-games-tm--ai-melds game s)
      (card-games-tm--ai-layoffs game s)
      (when (eq (card-games-get game :phase) 'play)
        (if (null (card-games-rummy--hand game s))
            (card-games-tm--end-turn game s)          ; melded out, no discard needed
          (card-games-tm--discard game s (card-games-tm--ai-discard-card game s)))))))

(defun card-games-tm--run (game)
  "Advance AI seats until it is the human's turn or the hand ends."
  (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0))
    (card-games-tm--ai-turn game (card-games-get game :turn))))

;;;; UI

(defvar-local card-games-tm--game nil "The table-meld game in the current buffer.")

(defun card-games-tm--discard-string (game)
  "Return the discard-pile display line for GAME.
Deep-pickup games show the whole pile with depth indices (0 = top)."
  (cl-flet ((paint (c)
              (let ((cs (card-games-rummy-card-string c)))
                (if (and c (not (card-games-rummy-joker-p c)) (card-games-red-suit-p (car c)))
                    (propertize cs 'face 'card-games-red-suit) cs))))
    (let ((pile (card-games-get game :discard)))
      (if (and (card-games-tm--deep-pickup game) (cdr pile))
          (concat "Discard (0=top): "
                  (let ((i -1))
                    (mapconcat
                     (lambda (c) (setq i (1+ i)) (format "%d:%s" i (paint c)))
                     (cl-subseq pile 0 (min (length pile) 12)) " ")))
        (concat "Discard: " (paint (card-games-rummy--top game)))))))

(defun card-games-tm--layoff-hint (game)
  "Return a predicate marking cards that can be laid off in GAME now."
  (lambda (c)
    (cl-find-if (lambda (r) (card-games-rummy-meld-p (cons c (cdr r)) :min 3
                                             :ace-high (card-games-tm--ace-high game)))
                (card-games-get game :table))))

(defun card-games-tm--svg (game)
  "Return an SVG board for the table-meld GAME."
  (let* ((scores (card-games-get game :scores)) (laid (card-games-get game :laid))
         (meldp (oref game score-style)))
    (card-games-rummy--board-svg
     :title (format "%s   (target %d)" (oref game vname) (oref game target))
     :infos (let (out)
              (dotimes (s (card-games-get game :nplayers))
                (unless (= s 0)
                  (push (format "Player %d: %d cards   score %d%s"
                                s (length (card-games-rummy--hand game s)) (aref scores s)
                                (if (eq meldp 'meld-points)
                                    (format "   laid %d" (aref laid s)) ""))
                        out)))
              (nreverse out))
     :melds (mapcar (lambda (rec)
                      (cons (if (= (car rec) 0) "you" (format "P%d" (car rec)))
                            (cdr rec)))
                    (card-games-get game :table))
     :discard (card-games-rummy--top game) :stock (length (card-games-get game :stock))
     :hand (card-games-rummy--hand game 0) :cursor (card-games-get game :cursor)
     :marks (card-games-get game :marks) :hint-fn (card-games-tm--layoff-hint game)
     :message (card-games-get game :message))))

(cl-defmethod card-games-render ((game card-games-tablemeld-game))
  "Return a depiction of the table-meld GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-tm--svg game)
    (card-games-tm--render-text game)))

(defun card-games-tm--render-text (game)
  "Return a plain-text depiction of the table-meld GAME."
  (let* ((out '()) (scores (card-games-get game :scores))
         (laid (card-games-get game :laid)) (meldp (oref game score-style))
         (hand (card-games-rummy--hand game 0)) (cursor (card-games-get game :cursor)))
    (push (format "  %s        target %d\n\n" (oref game vname) (oref game target)) out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   score %d%s\n"
                      s (length (card-games-rummy--hand game s)) (aref scores s)
                      (if (eq meldp 'meld-points)
                          (format "  (laid %d)" (aref laid s)) ""))
              out)))
    (push "\n  Table:\n" out)
    (if (card-games-get game :table)
        (dolist (rec (card-games-get game :table))
          (push (format "    [%s] %s\n" (if (= (car rec) 0) "you" (format "P%d" (car rec)))
                        (mapconcat #'card-games-rummy-card-string (cdr rec) " "))
                out))
      (push "    (empty)\n" out))
    (push (format "\n  %s     Stock: %d\n\n"
                  (card-games-tm--discard-string game)
                  (length (card-games-get game :stock)))
          out)
    (push (format "  Your hand%s:\n   "
                  (if (eq meldp 'meld-points) (format "  (laid %d, score %d)"
                                                       (aref laid 0) (aref scores 0))
                    (format "  (score %d)" (aref scores 0))))
          out)
    (push (card-games-rummy--render-cards hand cursor (card-games-get game :marks)
                                  (card-games-tm--layoff-hint game) 'hand)
          out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-tablemeld-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i))
    (_ (cl-call-next-method))))

(defun card-games-tm--redisplay ()
  "Redraw the table-meld buffer."
  (let ((game card-games-tm--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-tm--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (card-games-get game :step) (card-games-get game :phase))))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-tm--clamp-cursor (g)
  "Keep G's cursor within the hand and drop stale marks."
  (let ((n (length (card-games-rummy--hand g 0))))
    (card-games-put g :cursor (if (> n 0) (min (card-games-get g :cursor) (1- n)) 0))
    (card-games-put g :marks (cl-remove-if (lambda (i) (>= i n)) (card-games-get g :marks)))))

(defun card-games-tm--my-turn-p (g)
  (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0)))

(defun card-games-tm-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-tm--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-tm--redisplay)))

(defun card-games-tm-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-tm--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-tm--redisplay)))

(defun card-games-tm-mark ()
  "Toggle a mark on the card under the cursor (for melding)."
  (interactive)
  (let* ((g card-games-tm--game) (i (card-games-get g :cursor)) (marks (card-games-get g :marks)))
    (card-games-put g :marks (if (memq i marks) (delq i marks) (cons i marks)))
    (card-games-tm--redisplay)))

(defun card-games-tm--marked-cards (g)
  "Return the cards currently marked in G's hand."
  (let ((hand (card-games-rummy--hand g 0)))
    (mapcar (lambda (i) (nth i hand)) (sort (copy-sequence (card-games-get g :marks)) #'<))))

(defun card-games-tm-meld ()
  "Meld the marked cards onto the table."
  (interactive)
  (let* ((g card-games-tm--game) (cards (card-games-tm--marked-cards g)))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s or t)."))
     ((< (length cards) 3) (card-games-put g :message "Mark at least three cards (SPC), then m."))
     ((card-games-tm--meld g 0 cards)
      (card-games-put g :marks nil) (card-games-tm--clamp-cursor g)
      (card-games-put g :message "Melded.  Lay off with l, meld more, or discard (RET)."))
     (t (card-games-put g :message "Those cards are not a valid set or run.")))
    (card-games-tm--redisplay)))

(defun card-games-tm-layoff ()
  "Lay the cursor card (or marked cards) off onto a table meld."
  (interactive)
  (let* ((g card-games-tm--game) (marks (card-games-tm--marked-cards g)))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s or t)."))
     (t (let ((cards (or marks (list (nth (card-games-get g :cursor) (card-games-rummy--hand g 0)))))
              (any nil))
          (dolist (c cards) (when (and c (card-games-tm--layoff g 0 c)) (setq any t)))
          (card-games-put g :marks nil) (card-games-tm--clamp-cursor g)
          (card-games-put g :message (if any "Laid off." "That card fits no meld on the table.")))))
    (card-games-tm--redisplay)))

(defun card-games-tm-draw-stock ()
  "Draw the top stock card."
  (interactive)
  (let ((g card-games-tm--game))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "You already drew."))
     ((card-games-tm--draw g 0)
      (card-games-put g :step 'play) (card-games-tm--clamp-cursor g)
      (card-games-put g :message "Meld (m), lay off (l), then discard (RET)."))
     (t (card-games-tm--score-hand g nil)))
    (card-games-tm--redisplay)))

(defun card-games-tm-take ()
  "Take the discard top into your hand."
  (interactive)
  (let ((g card-games-tm--game))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "You already drew."))
     ((null (card-games-rummy--top g)) (card-games-put g :message "The discard pile is empty."))
     (t (let ((c (card-games-tm--take-top g 0)))
          (card-games-put g :step 'play) (card-games-tm--clamp-cursor g)
          (card-games-put g :message (format "Took %s.  Meld (m), lay off (l), discard (RET)."
                                     (card-games-rummy-card-string c))))))
    (card-games-tm--redisplay)))

(defun card-games-tm-take-deep ()
  "Take a card from below the top of the discard pile (Rummy 500).
You take that card and every card lying on top of it; the chosen card is
melded or laid off at once, the rest go into your hand."
  (interactive)
  (let* ((g card-games-tm--game) (pile (card-games-get g :discard)) (n (length pile)))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (card-games-tm--deep-pickup g))
      (card-games-put g :message "This game lets you take only the top discard (t)."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "You already drew."))
     ((< n 1) (card-games-put g :message "The discard pile is empty."))
     (t (let ((depth (read-number
                      (format "Take how deep?  0=top .. %d (you must meld that card): "
                              (1- n)) 0)))
          (if (and (integerp depth) (>= depth 0) (< depth n))
              (let ((desc (card-games-tm--take-deep g 0 depth)))
                (if desc
                    (progn (card-games-put g :step 'play) (card-games-tm--clamp-cursor g)
                           (card-games-put g :message
                                   (concat desc "  Meld, lay off, or discard (RET).")))
                  (card-games-put g :message
                          "You can't use that card right now -- choose another.")))
            (card-games-put g :message "No card at that depth.")))))
    (card-games-tm--redisplay)))

(defun card-games-tm-discard ()
  "Discard the cursor card and end your turn."
  (interactive)
  (let* ((g card-games-tm--game) (card (nth (card-games-get g :cursor) (card-games-rummy--hand g 0))))
    (cond
     ((not (card-games-tm--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s or t)."))
     ((null card) (card-games-put g :message "No card selected."))
     (t (card-games-tm--discard g 0 card)
        (card-games-put g :marks nil)
        (when (eq (card-games-get g :phase) 'play)
          (card-games-put g :message "You discarded.")
          (card-games-tm--run g))))
    (card-games-tm--redisplay)))

(defun card-games-tm-new ()
  "Deal a fresh hand, or a new game when one is over."
  (interactive)
  (let ((g card-games-tm--game))
    (when (eq (card-games-get g :phase) 'game-over)
      (card-games-put g :scores (make-vector (oref g nplayers) 0)))
    (card-games-put g :reveal nil)
    (card-games-tm--deal g)
    (card-games-tm--run g)
    (card-games-tm--redisplay)))

(defun card-games-tm-redraw () "Redraw the board." (interactive) (card-games-tm--redisplay))

(defun card-games-tm-help ()
  "Describe the table-meld controls."
  (interactive)
  (message "Arrows: choose  SPC: mark  m: meld  l: lay off  s: draw  t: take  T: deep take  RET: discard  n: new"))

(defvar card-games-tm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-tm-left)
    (define-key map (kbd "<right>") #'card-games-tm-right)
    (define-key map (kbd "SPC") #'card-games-tm-mark)
    (define-key map "m" #'card-games-tm-meld)
    (define-key map "l" #'card-games-tm-layoff)
    (define-key map "s" #'card-games-tm-draw-stock)
    (define-key map "t" #'card-games-tm-take)
    (define-key map "T" #'card-games-tm-take-deep)
    (define-key map (kbd "RET") #'card-games-tm-discard)
    (define-key map "n" #'card-games-tm-new)
    (define-key map "g" #'card-games-tm-redraw)
    (define-key map "?" #'card-games-tm-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-tm-mode'.")

(define-derived-mode card-games-tm-mode special-mode "Rummy"
  "Major mode for the table-meld rummy games."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

(defun card-games-tm--start (game buffer-name)
  "Start GAME in a buffer named BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (card-games-tm-mode)
      (setq card-games-tm--game game)
      (card-games-tm--deal game)
      (card-games-tm--run game)
      (card-games-tm--redisplay))
    (switch-to-buffer buf)))

;;;; The two games

(defcustom card-games-rummy-basic-players 2
  "Number of players in Basic Rummy, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defclass card-games-rummy-basic-game (card-games-tablemeld-game)
  ((vname :initform "Rummy")
   (score-style :initform 'go-out)
   (target :initform 100))
  "A game of plain Rummy.")

;;;###autoload
(defun card-games-rummy-basic ()
  "Play Basic Rummy against the computer."
  (interactive)
  (let ((n (max 2 (min 4 card-games-rummy-basic-players))))
    (card-games-tm--start (card-games-rummy-basic-game :nplayers n :hand-size (if (= n 2) 10 7))
                  "*Rummy*")))

(defcustom card-games-rum500-players 3
  "Number of players in Rummy 500, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defclass card-games-rum500-game (card-games-tablemeld-game)
  ((vname :initform "Rummy 500")
   (score-style :initform 'meld-points)
   (ace-15 :initform t)
   (ace-high :initform t)
   (deep-pickup :initform t)
   (target :initform 500))
  "A game of Rummy 500.")

;;;###autoload
(defun card-games-rum500 ()
  "Play Rummy 500 against the computer."
  (interactive)
  (let ((n (max 2 (min 4 card-games-rum500-players))))
    (card-games-tm--start (card-games-rum500-game :nplayers n :hand-size (if (= n 2) 13 7))
                  "*Rummy 500*")))

;;;###autoload
(defalias 'card-games-rummy-500 #'card-games-rum500)

(provide 'card-games-rum500)
;;; card-games-rum500.el ends here
