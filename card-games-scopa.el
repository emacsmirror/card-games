;;; card-games-scopa.el --- Scopa and Casino, capturing games  -*- lexical-binding: t; -*-

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

;; Two capturing ("fishing") games on a shared engine.  You play a card
;; from your hand to capture cards from the table: either a single card of
;; equal value or a combination that sums to it.  Clear the whole table
;; for a sweep.
;;
;;   `card-games-scopa' -- Scopa.  The Italian classic on a 40-card deck; score
;;     for cards, coins (diamonds), the sette bello (seven of diamonds),
;;     primiera, and each sweep ("scopa").  Game to 11.
;;   `card-games-casino' -- Casino.  The English cousin on the full deck; score for
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
(require 'card-games-core)
(require 'card-games-rummy)

(defclass card-games-fish-game (card-games-game)
  ((nplayers  :initarg :nplayers  :initform 2)
   (hand-size :initarg :hand-size :initform 3)
   (target    :initarg :target    :initform 11))
  "Abstract base for the capturing games Scopa and Casino."
  :abstract t)

(cl-defgeneric card-games-fish--value (game card)
  "Return CARD's capture value in GAME, or nil if it captures only by rank.")
(cl-defgeneric card-games-fish--deck (game)
  "Return a fresh shuffled deck for GAME.")
(cl-defgeneric card-games-fish--face-pair-p (game card)
  "Return non-nil when CARD in GAME captures only equal-rank cards (no sums).")
(cl-defmethod card-games-fish--face-pair-p ((_game card-games-fish-game) _card) "Base fishing games capture rank pairs only, never sums." nil)
(cl-defgeneric card-games-fish--score-round (game)
  "Add this round's points to GAME's running scores.")

(defsubst card-games-fish--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-fish--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))
(defsubst card-games-fish--captured (game s) "Return the cards seat S has captured in GAME." (aref (card-games-get game :captured) s))

(defun card-games-fish--who (s) "Return the display name of seat S." (if (= s 0) "You" "Computer"))

;;;; Capture search

(defun card-games-fish--best-subset (cards target valfn)
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

(defun card-games-fish--capture (game card)
  "Return the table cards CARD would capture in GAME, or nil."
  (let ((table (card-games-get game :table)))
    (if (card-games-fish--face-pair-p game card)
        (let ((same (cl-remove-if-not (lambda (c) (= (cdr c) (cdr card))) table)))
          (and same (list (car same))))
      (let ((v (card-games-fish--value game card)))
        (and v (let ((single (cl-find-if (lambda (c) (eql (card-games-fish--value game c) v))
                                         table)))
                 (if single (list single)
                   (card-games-fish--best-subset table v
                                         (lambda (c) (card-games-fish--value game c))))))))))

;;;; Flow

(cl-defmethod card-games-fish--deal-round ((game card-games-fish-game))
  "Start a fresh round of GAME: shuffle, deal the table and first hands."
  (let* ((n (oref game nplayers)) (deck (card-games-fish--deck game))
         (hands (make-vector n nil)) (table '()))
    (dotimes (_ 4) (push (pop deck) table))
    (dotimes (s n)
      (aset hands s (card-games-rummy-sort-hand (cl-loop repeat (oref game hand-size)
                                                 collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :table table)
    (card-games-put game :deck deck)
    (card-games-put game :captured (make-vector n nil))
    (card-games-put game :sweeps (make-vector n 0))
    (card-games-put game :nplayers n)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (card-games-put game :last-capturer nil)
    (unless (card-games-get game :scores) (card-games-put game :scores (make-vector n 0)))
    (card-games-put game :message "Play a card to capture by value, or trail it on the table.")
    game))

(defun card-games-fish--refill (game)
  "Deal GAME fresh hands from the deck when every hand is empty."
  (when (and (cl-every #'null (append (card-games-get game :hands) nil)) (card-games-get game :deck))
    (let ((deck (card-games-get game :deck)))
      (dotimes (s (card-games-get game :nplayers))
        (card-games-fish--set-hand game s
                           (card-games-rummy-sort-hand
                            (cl-loop repeat (oref game hand-size)
                                     while deck collect (pop deck)))))
      (card-games-put game :deck deck))))

(defun card-games-fish--round-over-p (game)
  "Return non-nil when GAME's deck and every hand are empty."
  (and (null (card-games-get game :deck))
       (cl-every #'null (append (card-games-get game :hands) nil))))

(cl-defmethod card-games-fish--play ((game card-games-fish-game) s card)
  "Have seat S play CARD in GAME: capture if possible, else trail it."
  (card-games-fish--set-hand game s (cl-remove card (card-games-fish--hand game s) :test #'equal :count 1))
  (let ((cap (card-games-fish--capture game card)))
    (if cap
        (progn
          (dolist (c cap)
            (card-games-put game :table (cl-remove c (card-games-get game :table) :test #'equal :count 1)))
          (aset (card-games-get game :captured) s (append (cons card cap) (card-games-fish--captured game s)))
          (card-games-put game :last-capturer s)
          (when (and (null (card-games-get game :table)) (not (card-games-fish--round-over-p game)))
            (aset (card-games-get game :sweeps) s (1+ (aref (card-games-get game :sweeps) s))))
          (card-games-put game :message
                  (format "%s captured %d card%s with %s.%s" (card-games-fish--who s)
                          (length cap) (if (> (length cap) 1) "s" "")
                          (card-games-rummy-card-string card)
                          (if (null (card-games-get game :table)) "  Sweep!" ""))))
      (card-games-put game :table (cons card (card-games-get game :table)))
      (card-games-put game :message (format "%s trailed %s." (card-games-fish--who s)
                                    (card-games-rummy-card-string card))))
    (card-games-put game :turn (mod (1+ s) (card-games-get game :nplayers)))
    (card-games-fish--refill game)
    (when (card-games-fish--round-over-p game) (card-games-fish--finish-round game))))

(cl-defmethod card-games-fish--finish-round ((game card-games-fish-game))
  "Award GAME's leftover table cards to the last capturer and score the round."
  (when (and (card-games-get game :table) (card-games-get game :last-capturer))
    (let ((s (card-games-get game :last-capturer)))
      (aset (card-games-get game :captured) s
            (append (card-games-get game :table) (card-games-fish--captured game s)))
      (card-games-put game :table nil)))
  (card-games-fish--score-round game)
  (let ((win nil) (n (card-games-get game :nplayers)) (best most-negative-fixnum))
    (dotimes (s n)
      (when (and (>= (aref (card-games-get game :scores) s) (oref game target))
                 (> (aref (card-games-get game :scores) s) best))
        (setq win s best (aref (card-games-get game :scores) s))))
    (card-games-put game :phase (if win 'game-over 'round-over))
    (card-games-put game :winner win)
    (card-games-put game :message
            (format "Round over.  Scores: You %d, Computer %d.  %s"
                    (aref (card-games-get game :scores) 0) (aref (card-games-get game :scores) 1)
                    (if win (format "%s wins!  (n: new game)" (card-games-fish--who win))
                      "(n: next round)")))))

(defun card-games-fish--award-most (game suit-pred pts)
  "Give PTS to whoever captured more of GAME's cards satisfying SUIT-PRED."
  (let ((c0 (cl-count-if suit-pred (card-games-fish--captured game 0)))
        (c1 (cl-count-if suit-pred (card-games-fish--captured game 1))))
    (cond ((> c0 c1) (aset (card-games-get game :scores) 0 (+ (aref (card-games-get game :scores) 0) pts)))
          ((> c1 c0) (aset (card-games-get game :scores) 1 (+ (aref (card-games-get game :scores) 1) pts))))))

(cl-defmethod card-games-fish--ai-play ((game card-games-fish-game) s)
  "Have GAME AI seat S capture the most it can, else trail its lowest card."
  (let ((hand (card-games-fish--hand game s)) (best nil) (bestn -1) (sweep nil))
    (dolist (c hand)
      (let* ((cap (card-games-fish--capture game c))
             (nn (length cap))
             (sw (and cap (= nn (length (card-games-get game :table))))))
        (when (or (and sw (not sweep))
                  (and (eq (and sw t) (and sweep t)) (> nn bestn)))
          (setq best c bestn nn sweep sw))))
    (unless best   ; nothing captures: trail the lowest-value card
      (setq best (car (sort (copy-sequence hand)
                            (lambda (a b) (< (or (card-games-fish--value game a) 99)
                                             (or (card-games-fish--value game b) 99)))))))
    (card-games-fish--play game s best)))

(defun card-games-fish--run (game)
  "Advance GAME's AI seats until your turn or the round ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0) (< guard 200))
      (setq guard (1+ guard))
      (card-games-fish--ai-play game (card-games-get game :turn)))))

;;;; UI

(defvar-local card-games-fish--game nil "The fishing game in the current buffer.")

(defun card-games-fish--svg (game)
  "Return an SVG board for the fishing GAME."
  (card-games-rummy--board-svg
   :title (format "%s   (to %d)" (oref game vname) (oref game target))
   :infos (list (format "Computer: %d cards   captured %d   (score %d)"
                        (length (card-games-fish--hand game 1)) (length (card-games-fish--captured game 1))
                        (aref (card-games-get game :scores) 1))
                (format "Your captured: %d   (score %d)"
                        (length (card-games-fish--captured game 0)) (aref (card-games-get game :scores) 0)))
   :stock-label "Deck" :stock (length (card-games-get game :deck)) :discard 'none
   :melds (list (cons "Table" (card-games-rummy-sort-hand (card-games-get game :table))))
   :hand (card-games-fish--hand game 0) :cursor (card-games-get game :cursor)
   :message (card-games-get game :message)))

(cl-defmethod card-games-render ((game card-games-fish-game))
  "Return a depiction of the fishing GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-fish--svg game)
    (card-games-fish--render-text game)))

(defun card-games-fish--render-text (game)
  "Return a plain-text depiction of the fishing GAME."
  (let* ((out '()) (cursor (card-games-get game :cursor)))
    (push (format "  %s      to %d\n\n" (oref game vname) (oref game target)) out)
    (push (format "  Computer: %d cards   captured %d   (score %d)\n"
                  (length (card-games-fish--hand game 1)) (length (card-games-fish--captured game 1))
                  (aref (card-games-get game :scores) 1)) out)
    (push (format "  Deck: %d        Your captured: %d   (score %d)\n\n"
                  (length (card-games-get game :deck)) (length (card-games-fish--captured game 0))
                  (aref (card-games-get game :scores) 0)) out)
    (push "  Table:\n   " out)
    (push (if (card-games-get game :table)
              (card-games-rummy--render-cards (card-games-rummy-sort-hand (card-games-get game :table)) -1 nil)
            "(empty)")
          out)
    (push "\n\n  Your hand:\n   " out)
    (push (card-games-rummy--render-cards (card-games-fish--hand game 0) cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-fish-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i)
      (card-games-fish-play))
    (_ (cl-call-next-method))))

(defun card-games-fish--redisplay ()
  "Redraw the current fishing-family game buffer."
  (let ((game card-games-fish--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-fish--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-fish-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-fish--game) (n (length (card-games-fish--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-fish--redisplay)))

(defun card-games-fish-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-fish--game) (n (length (card-games-fish--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-fish--redisplay)))

(defun card-games-fish-play ()
  "Play the card under the cursor."
  (interactive)
  (let* ((g card-games-fish--game) (card (nth (card-games-get g :cursor) (card-games-fish--hand g 0))))
    (cond
     ((not (eq (card-games-get g :phase) 'play)) (card-games-put g :message "Press n to continue."))
     ((/= (card-games-get g :turn) 0) (card-games-put g :message "Not your turn."))
     ((null card) (card-games-put g :message "No card selected."))
     (t (card-games-fish--play g 0 card)
        (card-games-put g :cursor 0)
        (when (eq (card-games-get g :phase) 'play) (card-games-fish--run g))))
    (card-games-fish--redisplay)))

(defun card-games-fish-new ()
  "Start the next round, or a new game when one is over."
  (interactive)
  (let ((g card-games-fish--game))
    (when (eq (card-games-get g :phase) 'game-over)
      (card-games-put g :scores (make-vector (oref g nplayers) 0)))
    (card-games-fish--deal-round g)
    (card-games-fish--run g)
    (card-games-fish--redisplay)))

(defun card-games-fish-redraw () "Redraw." (interactive) (card-games-fish--redisplay))
(defun card-games-fish-help () "Describe the controls." (interactive)
       (message "Arrows: choose  RET: play the card  n: next round / new game  g: redraw"))

(defvar card-games-fish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-fish-left)
    (define-key map (kbd "<right>") #'card-games-fish-right)
    (define-key map (kbd "RET") #'card-games-fish-play)
    (define-key map "n" #'card-games-fish-new)
    (define-key map "g" #'card-games-fish-redraw)
    (define-key map "?" #'card-games-fish-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-fish-mode'.")

(define-derived-mode card-games-fish-mode special-mode "Fish"
  "Major mode for the capturing games Scopa and Casino."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

(defun card-games-fish--start (game buffer-name)
  "Start GAME in a buffer named BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (card-games-fish-mode)
      (setq card-games-fish--game game)
      (card-games-fish--deal-round game)
      (card-games-fish--run game)
      (card-games-fish--redisplay))
    (switch-to-buffer buf)))

;;;; Scopa

(defclass card-games-scopa-game (card-games-fish-game)
  ((vname :initform "Scopa") (hand-size :initform 3) (target :initform 11))
  "A game of Scopa.")

(cl-defmethod card-games-fish--value ((_game card-games-scopa-game) card)
  "Return CARD's Scopa value (Ace 1 .. 7, Jack 8, Queen 9, King 10)."
  (let ((r (cdr card)))
    (cond ((<= r 6) (1+ r)) ((= r 10) 8) ((= r 11) 9) ((= r 12) 10))))

(cl-defmethod card-games-fish--deck ((_game card-games-scopa-game))
  "Return a shuffled 40-card Scopa deck (no eights, nines, or tens)."
  (random t)
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13
                                unless (memq r '(7 8 9)) collect (cons s r)))))

(defun card-games-scopa--prime (card)
  "Return the primiera prime value of CARD."
  (pcase (cdr card)
    (6 21) (5 18) (0 16) (4 15) (3 14) (2 13) (1 12) (_ 10)))

(cl-defmethod card-games-fish--score-round ((game card-games-scopa-game))
  "Score a Scopa round for GAME: cards, coins, sette bello, primiera, sweeps."
  (let ((scores (card-games-get game :scores)))
    (card-games-fish--award-most game (lambda (_c) t) 1)                 ; most cards
    (card-games-fish--award-most game (lambda (c) (= (car c) 2)) 1)      ; most coins (diamonds)
    ;; sette bello: 7 of diamonds
    (dotimes (s 2)
      (when (cl-find '(2 . 6) (card-games-fish--captured game s) :test #'equal)
        (aset scores s (1+ (aref scores s)))))
    ;; primiera: best prime total across suits
    (let ((p (vector 0 0)))
      (dotimes (s 2)
        (let ((bysuit (make-vector 4 0)))
          (dolist (c (card-games-fish--captured game s))
            (aset bysuit (car c) (max (aref bysuit (car c)) (card-games-scopa--prime c))))
          (aset p s (apply #'+ (append bysuit nil)))))
      (cond ((> (aref p 0) (aref p 1)) (aset scores 0 (1+ (aref scores 0))))
            ((> (aref p 1) (aref p 0)) (aset scores 1 (1+ (aref scores 1))))))
    ;; sweeps
    (dotimes (s 2) (aset scores s (+ (aref scores s) (aref (card-games-get game :sweeps) s))))))

;;;###autoload
(defun card-games-scopa ()
  "Play Scopa against the computer."
  (interactive)
  (card-games-fish--start (card-games-scopa-game) "*Scopa*"))

;;;; Casino

(defclass card-games-casino-game (card-games-fish-game)
  ((vname :initform "Casino") (hand-size :initform 4) (target :initform 21))
  "A game of Casino.")

(cl-defmethod card-games-fish--value ((_game card-games-casino-game) card)
  "Return CARD's Casino value (Ace 1, pips 2-10, faces nil)."
  (let ((r (cdr card)))
    (cond ((= r 0) 1) ((<= r 9) (1+ r)) (t nil))))

(cl-defmethod card-games-fish--face-pair-p ((_game card-games-casino-game) card)
  "Return non-nil when CARD is a face card (captures only by matching rank)."
  (>= (cdr card) 10))

(cl-defmethod card-games-fish--deck ((_game card-games-casino-game))
  "Return a shuffled 52-card deck for Casino."
  (card-games-rummy-deck))

(cl-defmethod card-games-fish--score-round ((game card-games-casino-game))
  "Score a Casino round for GAME: cards, spades, casinos, aces, sweeps."
  (let ((scores (card-games-get game :scores)))
    (card-games-fish--award-most game (lambda (_c) t) 3)                 ; most cards
    (card-games-fish--award-most game (lambda (c) (= (car c) 0)) 1)      ; most spades
    (dotimes (s 2)
      (let ((caps (card-games-fish--captured game s)))
        (when (cl-find '(2 . 9) caps :test #'equal)             ; big casino 10D
          (aset scores s (+ (aref scores s) 2)))
        (when (cl-find '(0 . 1) caps :test #'equal)             ; little casino 2S
          (aset scores s (+ (aref scores s) 1)))
        (aset scores s (+ (aref scores s) (cl-count 0 caps :key #'cdr)))  ; aces
        (aset scores s (+ (aref scores s) (aref (card-games-get game :sweeps) s)))))))

;;;###autoload
(defun card-games-casino ()
  "Play Casino against the computer."
  (interactive)
  (card-games-fish--start (card-games-casino-game) "*Casino*"))

(provide 'card-games-scopa)
;;; card-games-scopa.el ends here
