;;; cg-scopa.el --- Scopa and Casino, capturing games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.90
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

;; Two capturing ("fishing") games on a shared engine.  You play a card
;; from your hand to capture cards from the table: either a single card of
;; equal value or a combination that sums to it.  Clear the whole table
;; for a sweep.
;;
;;   `cg-scopa' -- Scopa.  The Italian classic on a 40-card deck; score
;;     for cards, coins (diamonds), the sette bello (seven of diamonds),
;;     primiera, and each sweep ("scopa").  Game to 11.
;;   `cg-casino' -- Casino.  The English cousin on the full deck; score for
;;     cards, spades, big casino (ten of diamonds), little casino (two of
;;     spades), each ace, and each sweep.  Game to 21.
;;
;; You are the first player against the computer.  Captures are resolved
;; automatically (a single equal card if there is one, otherwise the
;; combination taking the most cards).  This Casino omits builds and
;; multiple captures from a single card.  Cards use the package cons
;; (SUIT . RANK), RANK 0 (Ace) .. 12 (King); suit 2 is diamonds.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)
(require 'cg-rummy)

(defclass cg-fish-game (cg-game)
  ((nplayers  :initarg :nplayers  :initform 2)
   (hand-size :initarg :hand-size :initform 3)
   (target    :initarg :target    :initform 11))
  "Abstract base for the capturing games Scopa and Casino."
  :abstract t)

(cl-defgeneric cg-fish--value (game card)
  "Return CARD's capture value in GAME, or nil if it captures only by rank.")
(cl-defgeneric cg-fish--deck (game)
  "Return a fresh shuffled deck for GAME.")
(cl-defgeneric cg-fish--face-pair-p (game card)
  "Return non-nil when CARD captures only equal-rank cards (no sums).")
(cl-defmethod cg-fish--face-pair-p ((_game cg-fish-game) _card) nil)
(cl-defgeneric cg-fish--score-round (game)
  "Add this round's points to GAME's running scores.")

(defsubst cg-fish--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-fish--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-fish--captured (game s) (aref (cg-get game :captured) s))

(defun cg-fish--who (s) (if (= s 0) "You" "Computer"))

;;;; Capture search

(defun cg-fish--best-subset (cards target valfn)
  "Return the largest subset of CARDS whose values (via VALFN) sum to TARGET.
Only subsets of two or more cards are considered.  Return nil if none."
  (let ((best nil) (vec (vconcat cards)) (n (length cards)))
    (dotimes (mask (ash 1 n))
      (let ((sum 0) (sub '()) (cnt 0))
        (dotimes (i n)
          (when (/= 0 (logand mask (ash 1 i)))
            (let ((v (funcall valfn (aref vec i))))
              (when v (setq sum (+ sum v) sub (cons (aref vec i) sub) cnt (1+ cnt))))))
        (when (and (>= cnt 2) (= sum target) (> cnt (length best)))
          (setq best sub))))
    best))

(defun cg-fish--capture (game card)
  "Return the table cards CARD would capture in GAME, or nil."
  (let ((table (cg-get game :table)))
    (if (cg-fish--face-pair-p game card)
        (let ((same (cl-remove-if-not (lambda (c) (= (cdr c) (cdr card))) table)))
          (and same (list (car same))))
      (let ((v (cg-fish--value game card)))
        (and v (let ((single (cl-find-if (lambda (c) (eql (cg-fish--value game c) v))
                                         table)))
                 (if single (list single)
                   (cg-fish--best-subset table v
                                         (lambda (c) (cg-fish--value game c))))))))))

;;;; Flow

(cl-defmethod cg-fish--deal-round ((game cg-fish-game))
  "Start a fresh round: shuffle, deal the table and the first hands."
  (let* ((n (oref game nplayers)) (deck (cg-fish--deck game))
         (hands (make-vector n nil)) (table '()))
    (dotimes (_ 4) (push (pop deck) table))
    (dotimes (s n)
      (aset hands s (cg-rummy-sort-hand (cl-loop repeat (oref game hand-size)
                                                 collect (pop deck)))))
    (cg-put game :hands hands)
    (cg-put game :table table)
    (cg-put game :deck deck)
    (cg-put game :captured (make-vector n nil))
    (cg-put game :sweeps (make-vector n 0))
    (cg-put game :nplayers n)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (cg-put game :last-capturer nil)
    (unless (cg-get game :scores) (cg-put game :scores (make-vector n 0)))
    (cg-put game :message "Play a card to capture by value, or trail it on the table.")
    game))

(defun cg-fish--refill (game)
  "Deal fresh hands from the deck when every hand is empty."
  (when (and (cl-every #'null (append (cg-get game :hands) nil)) (cg-get game :deck))
    (let ((deck (cg-get game :deck)))
      (dotimes (s (cg-get game :nplayers))
        (cg-fish--set-hand game s
                           (cg-rummy-sort-hand
                            (cl-loop repeat (oref game hand-size)
                                     while deck collect (pop deck)))))
      (cg-put game :deck deck))))

(defun cg-fish--round-over-p (game)
  (and (null (cg-get game :deck))
       (cl-every #'null (append (cg-get game :hands) nil))))

(cl-defmethod cg-fish--play ((game cg-fish-game) s card)
  "Seat S plays CARD: capture if possible, else trail it on the table."
  (cg-fish--set-hand game s (cl-remove card (cg-fish--hand game s) :test #'equal :count 1))
  (let ((cap (cg-fish--capture game card)))
    (if cap
        (progn
          (dolist (c cap)
            (cg-put game :table (cl-remove c (cg-get game :table) :test #'equal :count 1)))
          (aset (cg-get game :captured) s (append (cons card cap) (cg-fish--captured game s)))
          (cg-put game :last-capturer s)
          (when (and (null (cg-get game :table)) (not (cg-fish--round-over-p game)))
            (aset (cg-get game :sweeps) s (1+ (aref (cg-get game :sweeps) s))))
          (cg-put game :message
                  (format "%s captured %d card%s with %s.%s" (cg-fish--who s)
                          (length cap) (if (> (length cap) 1) "s" "")
                          (cg-rummy-card-string card)
                          (if (null (cg-get game :table)) "  Sweep!" ""))))
      (cg-put game :table (cons card (cg-get game :table)))
      (cg-put game :message (format "%s trailed %s." (cg-fish--who s)
                                    (cg-rummy-card-string card))))
    (cg-put game :turn (mod (1+ s) (cg-get game :nplayers)))
    (cg-fish--refill game)
    (when (cg-fish--round-over-p game) (cg-fish--finish-round game))))

(cl-defmethod cg-fish--finish-round ((game cg-fish-game))
  "Award leftover table cards to the last capturer and score the round."
  (when (and (cg-get game :table) (cg-get game :last-capturer))
    (let ((s (cg-get game :last-capturer)))
      (aset (cg-get game :captured) s
            (append (cg-get game :table) (cg-fish--captured game s)))
      (cg-put game :table nil)))
  (cg-fish--score-round game)
  (let ((win nil) (n (cg-get game :nplayers)) (best most-negative-fixnum))
    (dotimes (s n)
      (when (and (>= (aref (cg-get game :scores) s) (oref game target))
                 (> (aref (cg-get game :scores) s) best))
        (setq win s best (aref (cg-get game :scores) s))))
    (cg-put game :phase (if win 'game-over 'round-over))
    (cg-put game :winner win)
    (cg-put game :message
            (format "Round over.  Scores: You %d, Computer %d.  %s"
                    (aref (cg-get game :scores) 0) (aref (cg-get game :scores) 1)
                    (if win (format "%s wins!  (n: new game)" (cg-fish--who win))
                      "(n: next round)")))))

(defun cg-fish--award-most (game suit-pred pts)
  "Give PTS to whoever captured more cards satisfying SUIT-PRED."
  (let ((c0 (cl-count-if suit-pred (cg-fish--captured game 0)))
        (c1 (cl-count-if suit-pred (cg-fish--captured game 1))))
    (cond ((> c0 c1) (aset (cg-get game :scores) 0 (+ (aref (cg-get game :scores) 0) pts)))
          ((> c1 c0) (aset (cg-get game :scores) 1 (+ (aref (cg-get game :scores) 1) pts))))))

(cl-defmethod cg-fish--ai-play ((game cg-fish-game) s)
  "Have AI seat S capture the most it can, else trail its lowest card."
  (let ((hand (cg-fish--hand game s)) (best nil) (bestn -1) (sweep nil))
    (dolist (c hand)
      (let* ((cap (cg-fish--capture game c))
             (nn (length cap))
             (sw (and cap (= nn (length (cg-get game :table))))))
        (when (or (and sw (not sweep))
                  (and (eq (and sw t) (and sweep t)) (> nn bestn)))
          (setq best c bestn nn sweep sw))))
    (unless best   ; nothing captures: trail the lowest-value card
      (setq best (car (sort (copy-sequence hand)
                            (lambda (a b) (< (or (cg-fish--value game a) 99)
                                             (or (cg-fish--value game b) 99)))))))
    (cg-fish--play game s best)))

(defun cg-fish--run (game)
  "Advance AI seats until it is your turn or the round ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0) (< guard 200))
      (setq guard (1+ guard))
      (cg-fish--ai-play game (cg-get game :turn)))))

;;;; UI

(defvar-local cg-fish--game nil "The fishing game in the current buffer.")

(cl-defmethod cg-render ((game cg-fish-game))
  "Return a propertized depiction of the fishing GAME."
  (let* ((out '()) (cursor (cg-get game :cursor)))
    (push (format "  %s      to %d\n\n" (oref game vname) (oref game target)) out)
    (push (format "  Computer: %d cards   captured %d   (score %d)\n"
                  (length (cg-fish--hand game 1)) (length (cg-fish--captured game 1))
                  (aref (cg-get game :scores) 1)) out)
    (push (format "  Deck: %d        Your captured: %d   (score %d)\n\n"
                  (length (cg-get game :deck)) (length (cg-fish--captured game 0))
                  (aref (cg-get game :scores) 0)) out)
    (push "  Table:\n   " out)
    (push (if (cg-get game :table)
              (cg-rummy--render-cards (cg-rummy-sort-hand (cg-get game :table)) -1 nil)
            "(empty)")
          out)
    (push "\n\n  Your hand:\n   " out)
    (push (cg-rummy--render-cards (cg-fish--hand game 0) cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-fish-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i)
      (cg-fish-play))
    (_ (cl-call-next-method))))

(defun cg-fish--redisplay ()
  (let ((game cg-fish--game) (inhibit-read-only t))
        (setq cg-current-game game cg-redisplay-function #'cg-fish--redisplay)
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-fish-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g cg-fish--game) (n (length (cg-fish--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-fish--redisplay)))

(defun cg-fish-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g cg-fish--game) (n (length (cg-fish--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-fish--redisplay)))

(defun cg-fish-play ()
  "Play the card under the cursor."
  (interactive)
  (let* ((g cg-fish--game) (card (nth (cg-get g :cursor) (cg-fish--hand g 0))))
    (cond
     ((not (eq (cg-get g :phase) 'play)) (cg-put g :message "Press n to continue."))
     ((/= (cg-get g :turn) 0) (cg-put g :message "Not your turn."))
     ((null card) (cg-put g :message "No card selected."))
     (t (cg-fish--play g 0 card)
        (cg-put g :cursor 0)
        (when (eq (cg-get g :phase) 'play) (cg-fish--run g))))
    (cg-fish--redisplay)))

(defun cg-fish-new ()
  "Start the next round, or a new game when one is over."
  (interactive)
  (let ((g cg-fish--game))
    (when (eq (cg-get g :phase) 'game-over)
      (cg-put g :scores (make-vector (oref g nplayers) 0)))
    (cg-fish--deal-round g)
    (cg-fish--run g)
    (cg-fish--redisplay)))

(defun cg-fish-redraw () "Redraw." (interactive) (cg-fish--redisplay))
(defun cg-fish-help () "Describe the controls." (interactive)
       (message "Arrows: choose  RET: play the card  n: next round / new game  g: redraw"))

(defvar cg-fish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-fish-left)
    (define-key map (kbd "<right>") #'cg-fish-right)
    (define-key map (kbd "RET") #'cg-fish-play)
    (define-key map "n" #'cg-fish-new)
    (define-key map "g" #'cg-fish-redraw)
    (define-key map "?" #'cg-fish-help)
    map)
  "Keymap for `cg-fish-mode'.")

(define-derived-mode cg-fish-mode special-mode "Fish"
  "Major mode for the capturing games Scopa and Casino."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

(defun cg-fish--start (game buffer-name)
  "Start GAME in a buffer named BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (cg-fish-mode)
      (setq cg-fish--game game)
      (cg-fish--deal-round game)
      (cg-fish--run game)
      (cg-fish--redisplay))
    (switch-to-buffer buf)))

;;;; Scopa

(defclass cg-scopa-game (cg-fish-game)
  ((vname :initform "Scopa") (hand-size :initform 3) (target :initform 11))
  "A game of Scopa.")

(cl-defmethod cg-fish--value ((_game cg-scopa-game) card)
  "Return CARD's Scopa value (Ace 1 .. 7, Jack 8, Queen 9, King 10)."
  (let ((r (cdr card)))
    (cond ((<= r 6) (1+ r)) ((= r 10) 8) ((= r 11) 9) ((= r 12) 10))))

(cl-defmethod cg-fish--deck ((_game cg-scopa-game))
  "Return a shuffled 40-card Scopa deck (no eights, nines, or tens)."
  (random t)
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13
                                unless (memq r '(7 8 9)) collect (cons s r)))))

(defun cg-scopa--prime (card)
  "Return the primiera prime value of CARD."
  (pcase (cdr card)
    (6 21) (5 18) (0 16) (4 15) (3 14) (2 13) (1 12) (_ 10)))

(cl-defmethod cg-fish--score-round ((game cg-scopa-game))
  "Score a Scopa round: cards, coins, sette bello, primiera, sweeps."
  (let ((scores (cg-get game :scores)))
    (cg-fish--award-most game (lambda (_c) t) 1)                 ; most cards
    (cg-fish--award-most game (lambda (c) (= (car c) 2)) 1)      ; most coins (diamonds)
    ;; sette bello: 7 of diamonds
    (dotimes (s 2)
      (when (cl-find '(2 . 6) (cg-fish--captured game s) :test #'equal)
        (aset scores s (1+ (aref scores s)))))
    ;; primiera: best prime total across suits
    (let ((p (vector 0 0)))
      (dotimes (s 2)
        (let ((bysuit (make-vector 4 0)))
          (dolist (c (cg-fish--captured game s))
            (aset bysuit (car c) (max (aref bysuit (car c)) (cg-scopa--prime c))))
          (aset p s (apply #'+ (append bysuit nil)))))
      (cond ((> (aref p 0) (aref p 1)) (aset scores 0 (1+ (aref scores 0))))
            ((> (aref p 1) (aref p 0)) (aset scores 1 (1+ (aref scores 1))))))
    ;; sweeps
    (dotimes (s 2) (aset scores s (+ (aref scores s) (aref (cg-get game :sweeps) s))))))

;;;###autoload
(defun cg-scopa ()
  "Play Scopa against the computer."
  (interactive)
  (cg-fish--start (cg-scopa-game) "*Scopa*"))

;;;; Casino

(defclass cg-casino-game (cg-fish-game)
  ((vname :initform "Casino") (hand-size :initform 4) (target :initform 21))
  "A game of Casino.")

(cl-defmethod cg-fish--value ((_game cg-casino-game) card)
  "Return CARD's Casino value (Ace 1, pips 2-10, faces nil)."
  (let ((r (cdr card)))
    (cond ((= r 0) 1) ((<= r 9) (1+ r)) (t nil))))

(cl-defmethod cg-fish--face-pair-p ((_game cg-casino-game) card)
  "Return non-nil when CARD is a face card (captures only by matching rank)."
  (>= (cdr card) 10))

(cl-defmethod cg-fish--deck ((_game cg-casino-game))
  "Return a shuffled 52-card deck for Casino."
  (cg-rummy-deck))

(cl-defmethod cg-fish--score-round ((game cg-casino-game))
  "Score a Casino round: cards, spades, casinos, aces, sweeps."
  (let ((scores (cg-get game :scores)))
    (cg-fish--award-most game (lambda (_c) t) 3)                 ; most cards
    (cg-fish--award-most game (lambda (c) (= (car c) 0)) 1)      ; most spades
    (dotimes (s 2)
      (let ((caps (cg-fish--captured game s)))
        (when (cl-find '(2 . 9) caps :test #'equal)             ; big casino 10D
          (aset scores s (+ (aref scores s) 2)))
        (when (cl-find '(0 . 1) caps :test #'equal)             ; little casino 2S
          (aset scores s (+ (aref scores s) 1)))
        (aset scores s (+ (aref scores s) (cl-count 0 caps :key #'cdr)))  ; aces
        (aset scores s (+ (aref scores s) (aref (cg-get game :sweeps) s)))))))

;;;###autoload
(defun cg-casino ()
  "Play Casino against the computer."
  (interactive)
  (cg-fish--start (cg-casino-game) "*Casino*"))

(provide 'cg-scopa)
;;; cg-scopa.el ends here
