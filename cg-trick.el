;;; cg-trick.el --- Four-hand trick-taking games (Hearts, Spades)  -*- lexical-binding: t; -*-

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

;; A small four-handed trick-taking engine and two games built on it:
;;
;;   `cg-hearts' -- the classic avoidance game; dodge every heart and the
;;                  Queen of Spades, or take them all to "shoot the moon".
;;   `cg-spades' -- partnership bidding; spades are always trump; make your
;;                  side's combined bid, beware of bags, and dare a nil.
;;
;; You sit South (seat 0); the other three seats are played by simple but
;; legal AI.  Cards are the package-standard cons (SUIT . RANK) with SUIT
;; 0 spades, 1 clubs, 2 diamonds, 3 hearts and RANK 0 (the Two) .. 12 (the
;; Ace); within a suit the higher rank wins, with the trump suit beating
;; every plain suit.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)
(require 'cg-svg)

;;;; Cards

(defconst cg-trick-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst cg-trick-seat-names ["South" "West" "North" "East"]
  "Seat names indexed 0..3, going clockwise from the human player.")

(defun cg-trick-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref cg-trick-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defsubst cg-trick-red-p (card) (and card (cg-red-suit-p (car card))))

(defun cg-trick--full-deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defun cg-trick--sort (cards)
  "Return CARDS sorted by suit then rank for display."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (car a) (car b)) (< (cdr a) (cdr b))
                        (< (car a) (car b))))))

;;;; Classes

(defclass cg-trick-game (cg-game)
  ((trump :initform nil :documentation "Trump suit index, or nil for none.")
   (restricted :initform 3 :documentation "Suit that cannot be led until broken.")
   (target :initform 100 :documentation "Score that ends the game.")
   (hand-size :initform 13 :documentation "Cards dealt to each seat per hand.")
   (vname  :initform "Trick game"))
  "Abstract base for four-handed trick-taking games."
  :abstract t)

(defclass cg-hearts-game (cg-trick-game)
  ((trump :initform nil) (restricted :initform 3) (target :initform 100)
   (vname :initform "Hearts"))
  "Hearts: no trump; avoid hearts and the Queen of Spades.")

(defclass cg-spades-game (cg-trick-game)
  ((trump :initform 0) (restricted :initform 0) (target :initform 500)
   (vname :initform "Spades"))
  "Spades: spades are trump; partnership bidding to 500.")

;;;; Dealing

(cl-defmethod cg-trick--deal ((game cg-trick-game))
  "Deal a fresh hand into GAME."
  (let ((deck (cg-trick--full-deck))
        (hands (make-vector 4 nil))
        (hs (oref game hand-size))
        (last nil))
    (dotimes (s 4)
      (let ((h nil))
        (dotimes (_ hs) (setq last (pop deck)) (push last h))
        (aset hands s (cg-trick--sort h))))
    (cg-put game :hands hands)
    (cg-put game :deck deck)
    (cg-put game :last-card last)
    (cg-put game :trick nil)
    (cg-put game :tricks (make-vector 4 0))
    (cg-put game :taken (make-vector 4 nil))
    (cg-put game :broken nil)
    (cg-put game :trick-no 0)
    game))

(defsubst cg-trick--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-trick--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-trick--partner (s) (mod (+ s 2) 4))
(defsubst cg-trick--team (s) (mod s 2))

;;;; Trick mechanics

(defun cg-trick--led-suit (game)
  "Return the suit led to the current trick, or nil if none yet."
  (let ((tr (cg-get game :trick)))
    (and tr (car (cdr (car (last tr)))))))   ; first entry played

(defun cg-trick--first-play (game)
  "Return the (SEAT . CARD) led to the current trick, or nil."
  (car (last (cg-get game :trick))))

(cl-defmethod cg-trick--has-points-only-p ((_ cg-trick-game) _hand) nil)

(cl-defmethod cg-trick--legal-p ((game cg-trick-game) seat card)
  "Return non-nil when SEAT may legally play CARD now."
  (let* ((hand (cg-trick--hand game seat))
         (trick (cg-get game :trick))
         (restricted (oref game restricted))
         (broken (cg-get game :broken)))
    (and (member card hand)
         (if trick
             ;; following: must follow the led suit if able
             (let ((led (cg-trick--led-suit game)))
               (if (cl-some (lambda (c) (= (car c) led)) hand)
                   (= (car card) led)
                 t))
           ;; leading: cannot lead the restricted suit until broken,
           ;; unless the hand holds nothing else
           (if (and (= (car card) restricted) (not broken))
               (cl-every (lambda (c) (= (car c) restricted)) hand)
             t)))))

(defun cg-trick--legal-moves (game seat)
  "Return the list of cards SEAT may legally play now."
  (cl-remove-if-not (lambda (c) (cg-trick--legal-p game seat c))
                    (cg-trick--hand game seat)))

(cl-defmethod cg-trick--winner ((game cg-trick-game))
  "Return the seat that wins the now-complete current trick."
  (let* ((trick (reverse (cg-get game :trick)))   ; play order
         (led (car (cdr (car trick))))
         (trump (oref game trump))
         (best (car trick)))
    (dolist (play (cdr trick))
      (let ((bc (cdr best)) (pc (cdr play)))
        (cond
         ((and trump (= (car pc) trump) (not (= (car bc) trump)))
          (setq best play))
         ((and (= (car pc) (car bc)) (> (cdr pc) (cdr bc)))
          (setq best play))
         ((and trump (not (= (car bc) trump)) (= (car pc) led)
               (> (cdr pc) (cdr bc)))
          (setq best play)))))
    (car best)))

(cl-defmethod cg-trick--play ((game cg-trick-game) seat card)
  "Have SEAT play CARD, resolving the trick when it completes."
  (cg-trick--set-hand game seat (remove card (cg-trick--hand game seat)))
  (when (= (car card) (oref game restricted)) (cg-put game :broken t))
  (cg-put game :trick (cons (cons seat card) (cg-get game :trick)))
  (if (= 4 (length (cg-get game :trick)))
      (let* ((w (cg-trick--winner game))
             (cards (mapcar #'cdr (cg-get game :trick))))
        (aset (cg-get game :tricks) w (1+ (aref (cg-get game :tricks) w)))
        (aset (cg-get game :taken) w (append cards (aref (cg-get game :taken) w)))
        (cg-put game :trick nil)
        (cg-put game :trick-no (1+ (cg-get game :trick-no)))
        (cg-put game :leader w)
        (cg-put game :turn w)
        w)
    (cg-put game :turn (mod (1+ seat) 4))
    nil))

(defun cg-trick--hand-over-p (game)
  "Return non-nil when all 13 tricks of the hand have been played."
  (and (null (cg-get game :trick))
       (cl-every #'null (append (cg-get game :hands) nil))))

;;;; Hearts specifics

(defun cg-hearts--card-points (card)
  "Return the penalty points for CARD in Hearts."
  (cond ((equal card '(0 . 10)) 13)   ; Queen of Spades
        ((= (car card) 3) 1)          ; any heart
        (t 0)))

(cl-defmethod cg-trick--legal-p ((game cg-hearts-game) seat card)
  "Hearts legality, adding the first-trick rules to the base."
  (and (cl-call-next-method)
       (let ((trick (cg-get game :trick))
             (hand (cg-trick--hand game seat))
             (first (= 0 (cg-get game :trick-no))))
         (cond
          ;; the very first card of the hand must be the Two of Clubs
          ((and first (null trick))
           (equal card '(1 . 0)))
          ;; no points on the first trick unless that is all one holds
          ((and first trick (> (cg-hearts--card-points card) 0))
           (cl-every (lambda (c) (> (cg-hearts--card-points c) 0)) hand))
          (t t)))))

(cl-defmethod cg-trick--leader-init ((game cg-hearts-game))
  "Hearts: the holder of the Two of Clubs leads first."
  (let (seat)
    (dotimes (s 4)
      (when (member '(1 . 0) (cg-trick--hand game s)) (setq seat s)))
    (cg-put game :leader seat) (cg-put game :turn seat)))

(cl-defmethod cg-trick--leader-init ((game cg-spades-game))
  "Spades: the player left of the dealer leads first."
  (let ((s (mod (1+ (or (cg-get game :dealer) 3)) 4)))
    (cg-put game :leader s) (cg-put game :turn s)))

(cl-defmethod cg-trick--score-hand ((game cg-hearts-game))
  "Score a finished Hearts hand into the cumulative scores."
  (let ((pts (make-vector 4 0)) (scores (cg-get game :scores)))
    (dotimes (s 4)
      (aset pts s (apply #'+ (mapcar #'cg-hearts--card-points
                                     (aref (cg-get game :taken) s)))))
    ;; shooting the moon
    (let ((moon (cl-position 26 (append pts nil))))
      (if moon
          (dotimes (s 4) (unless (= s moon)
                           (aset scores s (+ (aref scores s) 26))))
        (dotimes (s 4) (aset scores s (+ (aref scores s) (aref pts s))))))
    (cg-put game :last-points pts)))

(cl-defmethod cg-trick--game-over-p ((game cg-hearts-game))
  "Hearts ends when any score reaches the target."
  (cl-some (lambda (s) (>= s (oref game target))) (append (cg-get game :scores) nil)))

(cl-defmethod cg-trick--winner-seat ((game cg-hearts-game))
  "Return the winning seat (lowest score) for a finished Hearts game."
  (let ((best 0))
    (dotimes (s 4) (when (< (aref (cg-get game :scores) s)
                            (aref (cg-get game :scores) best))
                     (setq best s)))
    best))

;;;; Spades specifics

(cl-defmethod cg-trick--score-hand ((game cg-spades-game))
  "Score a finished Spades hand into the cumulative team scores."
  (let ((scores (cg-get game :scores))
        (bags (cg-get game :bags))
        (bids (cg-get game :bids))
        (tricks (cg-get game :tricks)))
    (dotimes (team 2)
      (let* ((a team) (b (+ team 2))
             (teambid 0) (teamtricks (+ (aref tricks a) (aref tricks b)))
             (delta 0))
        ;; nil bids handled per player
        (dolist (s (list a b))
          (if (= (aref bids s) 0)
              (setq delta (+ delta (if (= (aref tricks s) 0) 100 -100)))
            (setq teambid (+ teambid (aref bids s)))))
        (if (>= teamtricks teambid)
            (let ((over (- teamtricks teambid)))
              ;; overtricks beyond nil winners count as bags
              (setq delta (+ delta (* 10 teambid) over))
              (aset bags team (+ (aref bags team) over))
              (when (>= (aref bags team) 10)
                (setq delta (- delta 100))
                (aset bags team (- (aref bags team) 10))))
          (setq delta (- delta (* 10 teambid))))
        (aset scores a (+ (aref scores a) delta))
        (aset scores b (aref scores a))))
    (cg-put game :scores scores)))

(cl-defmethod cg-trick--game-over-p ((game cg-spades-game))
  "Spades ends when a team reaches the target (or falls badly behind)."
  (cl-some (lambda (s) (>= s (oref game target))) (append (cg-get game :scores) nil)))

(cl-defmethod cg-trick--winner-seat ((game cg-spades-game))
  "Return a member seat of the winning team for a finished Spades game."
  (if (>= (aref (cg-get game :scores) 0) (aref (cg-get game :scores) 1)) 0 1))

;;;; AI

(cl-defmethod cg-trick--ai-bid ((game cg-spades-game) seat)
  "Return a simple trick estimate (bid) for SEAT in Spades."
  (let ((hand (cg-trick--hand game seat)) (bid 0))
    (dolist (c hand)
      (cond
       ((= (cdr c) 12) (setq bid (1+ bid)))                 ; aces
       ((and (= (cdr c) 11)) (setq bid (1+ bid)))           ; kings
       ((and (= (car c) 0) (>= (cdr c) 9)) (setq bid (1+ bid))))) ; high spades
    ;; long spades add tricks
    (let ((nsp (cl-count-if (lambda (c) (= (car c) 0)) hand)))
      (when (> nsp 4) (setq bid (+ bid (- nsp 4)))))
    (max 1 (min 13 bid))))

(cl-defmethod cg-trick--ai-play ((game cg-hearts-game) seat)
  "Choose a legal Hearts card for SEAT, avoiding points."
  (let* ((moves (cg-trick--legal-moves game seat))
         (trick (cg-get game :trick)))
    (or
     (if (null trick)
         ;; leading: play a low non-point card
         (car (sort (copy-sequence moves)
                    (lambda (a b) (< (+ (* 4 (cg-hearts--card-points a)) (cdr a))
                                     (+ (* 4 (cg-hearts--card-points b)) (cdr b))))))
       ;; following: if we can duck under the current winner, play highest
       ;; safe card; else dump the most dangerous card
       (let* ((led (cg-trick--led-suit game))
              (winrank (apply #'max (cons -1 (mapcar (lambda (p) (if (= (car (cdr p)) led)
                                                                     (cdr (cdr p)) -1))
                                                     trick))))
              (under (cl-remove-if-not (lambda (c) (and (= (car c) led)
                                                        (< (cdr c) winrank)))
                                       moves)))
         (cond
          (under (car (last (cg-trick--sort under))))      ; highest still safe
          ((cl-some (lambda (c) (/= (car c) led)) moves)   ; void: dump worst
           (car (sort (copy-sequence moves)
                      (lambda (a b) (> (+ (* 4 (cg-hearts--card-points a)) (cdr a))
                                       (+ (* 4 (cg-hearts--card-points b)) (cdr b)))))))
          (t (car (sort (copy-sequence moves)        ; must follow & take: lowest
                        (lambda (a b) (< (cdr a) (cdr b)))))))))
     (car moves))))

(cl-defmethod cg-trick--ai-play ((game cg-spades-game) seat)
  "Choose a legal Spades card for SEAT."
  (let* ((moves (cg-trick--legal-moves game seat))
         (trick (cg-get game :trick))
         (trump (oref game trump)))
    (or
     (if (null trick)
         ;; lead a high non-spade if possible, else lowest
         (let ((non (cl-remove-if (lambda (c) (= (car c) trump)) moves)))
           (if non (car (last (cg-trick--sort non)))
             (car (cg-trick--sort moves))))
       (let* ((led (cg-trick--led-suit game))
              (cur (cg-get game :trick))
              ;; current winning play
              (winner (cg-trick--winner-of game cur))
              (partner-winning (and winner (= (cg-trick--team winner)
                                              (cg-trick--team seat)))))
         (if partner-winning
             (car (cg-trick--sort moves))   ; let partner have it: play low
           ;; try to win cheaply
           (let* ((followers (cl-remove-if-not (lambda (c) (= (car c) led)) moves)))
             (or (car (cg-trick--sort followers))
                 (car (cg-trick--sort moves)))))))
     (car moves))))

(defun cg-trick--winner-of (game trick)
  "Return the seat currently winning the partial TRICK of GAME."
  (when trick
    (let* ((order (reverse trick))
           (led (car (cdr (car order))))
           (trump (oref game trump))
           (best (car order)))
      (dolist (play (cdr order))
        (let ((bc (cdr best)) (pc (cdr play)))
          (cond
           ((and trump (= (car pc) trump) (not (= (car bc) trump))) (setq best play))
           ((and (= (car pc) (car bc)) (> (cdr pc) (cdr bc))) (setq best play))
           ((and trump (not (= (car bc) trump)) (= (car pc) led)
                 (> (cdr pc) (cdr bc))) (setq best play)))))
      (car best))))

;;;; Game driver (logic; UI layered on top)

(cl-defmethod cg-trick--start-hand ((game cg-trick-game))
  "Deal and prepare a new hand, leaving GAME ready for the first lead."
  (cg-trick--deal game)
  (cg-trick--leader-init game)
  game)

(defun cg-trick--simulate-hand (game)
  "Play a whole hand with AI for every seat (used by tests)."
  (while (not (cg-trick--hand-over-p game))
    (let ((seat (cg-get game :turn)))
      (cg-trick--play game seat
                      (if (eq cg-ai-level 'easy)
                          (let ((moves (cg-trick--legal-moves game seat)))
                            (nth (random (length moves)) moves))
                        (cg-trick--ai-play game seat)))))
  (cg-trick--score-hand game))

;;;; New-game / hand lifecycle

(defvar-local cg-trick--game nil "The trick-taking game in the current buffer.")

(defconst cg-trick--pass-dirs [1 3 2 0]
  "Pass directions by hand: left, right, across, hold (then repeat).")

(defun cg-trick--dir-name (dir)
  "Return a human label for pass direction DIR."
  (pcase dir (1 "left") (3 "right") (2 "across") (_ "hold")))

(cl-defgeneric cg-trick--begin-hand (game)
  "Deal and set up a new hand of GAME, then run AI up to the human's turn.")

(cl-defmethod cg-trick--begin-hand ((game cg-hearts-game))
  (cg-trick--deal game)
  (cg-put game :hand-no (1+ (or (cg-get game :hand-no) 0)))
  (cg-put game :cursor 0) (cg-put game :marks nil)
  (let ((dir (aref cg-trick--pass-dirs (mod (1- (cg-get game :hand-no)) 4))))
    (cg-put game :pass-dir dir)
    (if (= dir 0)
        (progn (cg-trick--leader-init game)
               (cg-put game :phase 'play)
               (cg-put game :message "No passing this hand.  Play begins.")
               (cg-trick--run game))
      (cg-put game :phase 'pass)
      (cg-put game :message
              (format "Pass three cards %s.  RET marks a card; p sends them."
                      (cg-trick--dir-name dir))))))

(cl-defmethod cg-trick--begin-hand ((game cg-spades-game))
  (cg-trick--deal game)
  (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
  (cg-put game :cursor 0)
  (let ((bids (make-vector 4 0)))
    (dotimes (s 4) (unless (= s 0) (aset bids s (cg-trick--ai-bid game s))))
    (aset bids 0 (if noninteractive (cg-trick--ai-bid game 0)
                   (let ((sug (cg-trick--ai-bid game 0)))
                     (max 0 (min 13 (read-number
                                     (format "Your bid (0 = nil) [suggest %d]: " sug)
                                     sug))))))
    (cg-put game :bids bids))
  (cg-trick--leader-init game)
  (cg-put game :phase 'play)
  (cg-put game :message
          (format "You bid %d.  Make your side's combined bid."
                  (aref (cg-get game :bids) 0)))
  (cg-trick--run game))

(defun cg-trick--new (game)
  "Initialise GAME for a fresh match and deal the first hand."
  (cg-put game :scores (make-vector 4 0))
  (cg-put game :bags (make-vector 2 0))
  (cg-put game :dealer 3)
  (cg-put game :hand-no 0)
  (cg-put game :round 0)
  (cg-trick--begin-hand game)
  game)

(defun cg-trick--run (game)
  "Advance AI seats until it is the human's turn or the hand ends."
  (while (and (eq (cg-get game :phase) 'play)
              (not (cg-trick--hand-over-p game))
              (/= (cg-get game :turn) 0))
    (let ((s (cg-get game :turn)))
      (cg-trick--play game s (cg-trick--ai-play game s))))
  (when (and (eq (cg-get game :phase) 'play) (cg-trick--hand-over-p game))
    (cg-trick--finish-hand game)))

(defun cg-trick--finish-hand (game)
  "Score the finished hand of GAME and start the next, or end the match."
  (cg-trick--score-hand game)
  (if (cg-trick--game-over-p game)
      (progn (cg-put game :phase 'game-over)
             (cg-put game :message
                     (format "Game over.  %s.  Press n for a new match."
                             (cg-trick--result-string game))))
    (cg-trick--begin-hand game)))

(cl-defmethod cg-trick--result-string ((game cg-hearts-game))
  (format "%s wins with the lowest score"
          (aref cg-trick-seat-names (cg-trick--winner-seat game))))

(cl-defmethod cg-trick--result-string ((game cg-spades-game))
  (let ((w (cg-trick--winner-seat game)))
    (format "%s win" (if (= w 0) "You and North" "West and East"))))

;;;; AI passing

(cl-defmethod cg-trick--ai-pass ((_ cg-hearts-game) hand)
  "Return three cards to pass from HAND (shed the most dangerous)."
  (let ((danger (lambda (c) (+ (* 6 (cg-hearts--card-points c))
                               (if (and (= (car c) 0) (>= (cdr c) 10)) 5 0)
                               (cdr c)))))
    (cl-subseq (sort (copy-sequence hand)
                     (lambda (a b) (> (funcall danger a) (funcall danger b))))
               0 3)))

(defun cg-trick--do-pass (game)
  "Exchange the chosen passing cards among the four seats of GAME."
  (let* ((dir (cg-get game :pass-dir))
         (sel (make-vector 4 nil))
         (kept (make-vector 4 nil)))
    (aset sel 0 (copy-sequence (cg-get game :marks)))
    (dotimes (s 4)
      (unless (= s 0)
        (aset sel s (copy-sequence (cg-trick--ai-pass game (cg-trick--hand game s))))))
    ;; what each seat keeps (its hand minus the cards it gives away)
    (dotimes (s 4)
      (aset kept s (cl-remove-if (lambda (c) (member c (aref sel s)))
                                 (cg-trick--hand game s))))
    ;; deal each seat's three cards to the seat DIR places along
    (dotimes (s 4)
      (let ((r (mod (+ s dir) 4)))
        (aset kept r (append (aref kept r) (aref sel s)))))
    (dotimes (s 4)
      (cg-trick--set-hand game s (cg-trick--sort (aref kept s))))
    (cg-trick--leader-init game)
    (cg-put game :phase 'play)
    (cg-put game :marks nil)
    (cg-put game :message "Cards passed.  Play begins.")
    (cg-trick--run game)))

(defun cg-trick--seat-line (game s)
  "Return a status line for opponent seat S of GAME."
  (let* ((n (length (cg-trick--hand game s)))
         (bid (and (cg-get game :bids) (aref (cg-get game :bids) s)))
         (won (and (cg-get game :tricks) (aref (cg-get game :tricks) s))))
    (format "  %-6s %2d cards%s%s\n" (aref cg-trick-seat-names s) n
            (if bid (format "  bid %d" bid) "")
            (if won (format "  won %d" won) ""))))

(defcustom cg-trick-svg-cards t
  "When non-nil, draw cards as SVG images on a graphical display."
  :type 'boolean :group 'card-games)

(defun cg-trick--spec (card)
  "Return the cg-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref cg-trick-ranks (cdr card)) (car card)))

(cl-defun cg-trick--svg-row (cards &key cursor marks hints region-tag)
  "Return a one-image SVG row for CARDS (clickable + sliderful when REGION-TAG)."
  (cg-svg-hand-image (mapcar #'cg-trick--spec cards)
                     :cursor cursor :marks marks :hints hints
                     :overlap (if (> (length cards) 11)
                                  (max 0 (- cg-svg-card-width 24)) 0)
                     :region-tag region-tag))

(defun cg-trick--draw-backs (svg x y n)
  "Draw up to three overlapped face-down backs at X, Y for a hand of N cards."
  (let ((k (min (max n 0) 3)) (xx x))
    (dotimes (_ k)
      (cg-svg-card svg xx y :down t)
      (setq xx (+ xx 16)))))

(defun cg-trick--svg (game)
  "Return a propertized full-table SVG depiction of trick GAME.
The South hand carries clickable (hand . INDEX) regions."
  (let* ((w cg-svg-card-width) (h cg-svg-card-height) (gap cg-svg-card-gap)
         (pad 16)
         (hand (cg-trick--sort (cg-trick--hand game 0)))
         (n (length hand))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (width (max (+ fanw (* 2 pad)) 720))
         (cx (/ width 2))
         (scores (cg-get game :scores))
         (trick (cg-get game :trick))
         (turn (cg-get game :turn))
         (phase (cg-get game :phase))
         (marks (cg-get game :marks))
         (cursor (cg-get game :cursor))
         (bids (cg-get game :bids))
         (tks (cg-get game :tricks))
         (y-title 6) (y-north 26)
         (y-tn (+ y-north h 22))
         (cy (+ y-tn (round (* h 0.55))))
         (y-ts (+ cy (round (* h 0.15))))
         (y-south (+ y-ts h 34))
         (height (+ y-south h 30))
         (svg (svg-create width height))
         (lc (cg-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels
        ((txt (str x y &optional sz bold anchor)
              (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                     :font-family cg-svg-font-family
                     (append (and bold '(:font-weight "bold"))
                             (and anchor (list :text-anchor anchor)))))
         (seat (s x y)
               (cg-trick--draw-backs svg x (+ y 6) (length (cg-trick--hand game s)))
               (txt (format "%s%s%s%s" (aref cg-trick-seat-names s)
                            (if (and bids (aref bids s)) (format "  bid %d" (aref bids s)) "")
                            (if (and tks (> (aref tks s) 0)) (format "  won %d" (aref tks s)) "")
                            (if (= turn s) "  <-" ""))
                    x y 11))
         (trick-card (s x y)
                     (let ((play (assq s trick)))
                       (when play
                         (let ((sp (cg-trick--spec (cdr play))))
                           (cg-svg-card svg x y :rank (car sp) :suit (cdr sp)))))))
      (txt (format "%s -- %s" (oref game vname)
                   (pcase phase
                     ('pass "pass three cards") ('bid "bidding")
                     ('play (if (= turn 0) "your turn" "opponents playing"))
                     (_ "play")))
           pad (+ y-title 12) 13 t)
      (when scores
        (txt (format "S %d  W %d  N %d  E %d"
                     (aref scores 0) (aref scores 1) (aref scores 2) (aref scores 3))
             (- width pad) (+ y-title 12) 12 nil "end"))
      (seat 2 (- cx 40) y-north)
      (seat 1 pad cy)
      (seat 3 (- width pad 100) cy)
      (trick-card 2 (- cx (/ w 2)) y-tn)
      (trick-card 0 (- cx (/ w 2)) y-ts)
      (trick-card 1 (- cx w (round (* w 0.4))) (round (- cy (* h 0.25))))
      (trick-card 3 (+ cx (round (* w 0.4))) (round (- cy (* h 0.25))))
      (txt (format "Your hand (South)%s"
                   (if (eq phase 'pass) (format " -- marked %d/3" (length marks)) ""))
           pad (- y-south 6) 11)
      (let ((x (- cx (/ fanw 2))) (i 0)
            (legalp (and (eq phase 'play) (= turn 0))))
        (dolist (c hand)
          (let ((sp (cg-trick--spec c))
                (curp (= i cursor))
                (markp (member c marks))
                (hintp (and legalp (cg-trick--legal-p game 0 c))))
            (cg-svg-card svg x y-south :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (when markp
              (svg-rectangle svg (- x 3) (- y-south 3) (+ w 6) (+ h 6)
                             :rx 8 :fill "none" :stroke "#4a90d9" :stroke-width 3))
            (push (cons (list x y-south (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (cg-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (cg-svg-image svg (cg-scale))
                'cg-regions (nreverse regions))))

(defun cg-trick--render-text (game)
  "Return a plain-text depiction of trick GAME."
  (let* ((out (list))
         (scores (cg-get game :scores))
         (marks (cg-get game :marks))
         (cursor (cg-get game :cursor))
         (hand (cg-trick--sort (cg-trick--hand game 0))))
    (push (format "  %s\n" (oref game vname)) out)
    (when scores
      (push (format "  Scores:  South %d  West %d  North %d  East %d\n\n"
                    (aref scores 0) (aref scores 1) (aref scores 2) (aref scores 3))
            out))
    (dolist (s '(2 1 3))
      (push (cg-trick--seat-line game s) out))
    (push "\n  Trick: " out)
    (if (null (cg-get game :trick))
        (push "(empty)" out)
      (dolist (play (reverse (cg-get game :trick)))
        (push (format "%s:%s  " (aref cg-trick-seat-names (car play))
                      (let ((cs (cg-trick-card-string (cdr play))))
                        (if (cg-trick-red-p (cdr play))
                            (propertize cs 'face 'cg-red-suit) cs)))
              out)))
    (push "\n\n  Your hand (South):\n   " out)
    (let ((i 0))
      (dolist (c hand)
        (let ((cs (cg-trick-card-string c)) (faces nil))
          (when (cg-trick-red-p c) (push 'cg-red-suit faces))
          (when (member c marks) (push 'cg-hint faces))
          (when (= i cursor) (push 'cg-cursor faces))
          (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
        (setq i (1+ i))))
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render ((game cg-trick-game))
  "Return a depiction of GAME: a full SVG table on a graphical display,
else a plain-text board."
  (if (and cg-trick-svg-cards (display-graphic-p))
      (cg-trick--svg game)
    (cg-trick--render-text game)))

(cl-defmethod cg-render-apply ((g cg-trick-game) action)
  "Apply a click ACTION on the hand: select that card and play it."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i) (cg-trick-act))
    (_ (cl-call-next-method))))

(defun cg-trick--redisplay ()
  "Redraw the current trick-game buffer."
  (let ((game cg-trick--game) (inhibit-read-only t))
    (setq cg-current-game game cg-redisplay-function #'cg-trick--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (cg-get game :phase) "play")))
    (erase-buffer)
    (insert (cg-render game))
    (goto-char (point-min))))

;;;; Commands

(defun cg-trick--cursor-card (game)
  "Return the South card currently under the cursor."
  (nth (cg-get game :cursor) (cg-trick--sort (cg-trick--hand game 0))))

(defun cg-trick-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((game cg-trick--game) (n (length (cg-trick--hand game 0))))
    (when (> n 0) (cg-put game :cursor (mod (1- (cg-get game :cursor)) n)))
    (cg-trick--redisplay)))

(defun cg-trick-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((game cg-trick--game) (n (length (cg-trick--hand game 0))))
    (when (> n 0) (cg-put game :cursor (mod (1+ (cg-get game :cursor)) n)))
    (cg-trick--redisplay)))

(defun cg-trick-act ()
  "Play, or (during the Hearts pass) mark, the selected card."
  (interactive)
  (let* ((game cg-trick--game)
         (phase (cg-get game :phase))
         (card (cg-trick--cursor-card game)))
    (pcase phase
      ('play
       (cond
        ((/= (cg-get game :turn) 0) (cg-put game :message "Not your turn."))
        ((not (cg-trick--legal-p game 0 card))
         (cg-put game :message "Illegal play — you must follow suit."))
        (t (cg-trick--play game 0 card)
           (cg-put game :cursor (max 0 (min (cg-get game :cursor)
                                            (1- (length (cg-trick--hand game 0))))))
           (cg-trick--run game))))
      ('pass
       (if (member card (cg-get game :marks))
           (cg-put game :marks (remove card (cg-get game :marks)))
         (if (>= (length (cg-get game :marks)) 3)
             (cg-put game :message "Three already marked — press p to pass.")
           (cg-put game :marks (cons card (cg-get game :marks))))))
      (_ (cg-put game :message "Press n for a new match.")))
    (cg-trick--redisplay)))

(defun cg-trick-pass ()
  "Confirm the Hearts pass once three cards are marked."
  (interactive)
  (let ((game cg-trick--game))
    (if (and (eq (cg-get game :phase) 'pass) (= 3 (length (cg-get game :marks))))
        (cg-trick--do-pass game)
      (cg-put game :message "Mark exactly three cards first."))
    (cg-trick--redisplay)))

(defun cg-trick-new ()
  "Start a fresh match in this buffer."
  (interactive)
  (cg-trick--new cg-trick--game)
  (cg-trick--redisplay))

(defun cg-trick-redraw () "Redraw the table." (interactive) (cg-trick--redisplay))

(defun cg-trick-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows: choose card  RET: play/mark  p: pass (Hearts)  n: new  g: redraw"))

(defvar cg-trick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-trick-left)
    (define-key map (kbd "<right>") #'cg-trick-right)
    (define-key map (kbd "RET") #'cg-trick-act)
    (define-key map (kbd "SPC") #'cg-trick-act)
    (define-key map "p" #'cg-trick-pass)
    (define-key map "n" #'cg-trick-new)
    (define-key map "g" #'cg-trick-redraw)
    (define-key map "?" #'cg-trick-help)
    map)
  "Keymap for `cg-trick-mode'.")

(define-derived-mode cg-trick-mode special-mode "Trick"
  "Major mode for the four-handed trick-taking games."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

(defun cg-trick--play-game (class)
  "Start a trick game of CLASS in its own buffer."
  (let* ((game (make-instance class))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (cg-trick-mode)
      (setq cg-trick--game game)
      (cg-trick--new game)
      (cg-trick--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun cg-hearts ()
  "Play Hearts against three computer opponents."
  (interactive) (cg-trick--play-game 'cg-hearts-game))

;;;###autoload
(defun cg-spades ()
  "Play Spades (partnership) against three computer opponents."
  (interactive) (cg-trick--play-game 'cg-spades-game))


;;;; Whist and Oh Hell

(defclass cg-whist-game (cg-trick-game)
  ((restricted :initform -1) (target :initform 5) (vname :initform "Whist"))
  "Whist: trump set by the turned card, no bidding, score tricks over six.")

(defclass cg-ohhell-game (cg-trick-game)
  ((restricted :initform -1) (target :initform 0) (vname :initform "Oh Hell"))
  "Oh Hell: hand size shrinks each round; bid the exact tricks you will take.")

(cl-defmethod cg-trick--leader-init ((game cg-whist-game))
  (let ((s (mod (1+ (or (cg-get game :dealer) 3)) 4)))
    (cg-put game :leader s) (cg-put game :turn s)))
(cl-defmethod cg-trick--leader-init ((game cg-ohhell-game))
  (let ((s (mod (1+ (or (cg-get game :dealer) 3)) 4)))
    (cg-put game :leader s) (cg-put game :turn s)))

(defun cg-trick--ai-trump-play (game seat)
  "A generic legal trump-game play for SEAT: follow and win cheaply, else low."
  (let* ((moves (cg-trick--legal-moves game seat))
         (trick (cg-get game :trick)))
    (or
     (if (null trick)
         (car (last (cg-trick--sort moves)))
       (let* ((winner (cg-trick--winner-of game trick))
              (partner-winning (and winner (= (cg-trick--team winner)
                                              (cg-trick--team seat))))
              (led (cg-trick--led-suit game)))
         (if partner-winning
             (car (cg-trick--sort moves))
           (let ((follow (cl-remove-if-not (lambda (c) (= (car c) led)) moves)))
             (or (car (last (cg-trick--sort follow)))
                 (car (cg-trick--sort moves)))))))
     (car moves))))

(cl-defmethod cg-trick--ai-play ((game cg-whist-game) seat)
  (cg-trick--ai-trump-play game seat))
(cl-defmethod cg-trick--ai-play ((game cg-ohhell-game) seat)
  (cg-trick--ai-trump-play game seat))

;; Whist
(cl-defmethod cg-trick--begin-hand ((game cg-whist-game))
  (cg-trick--deal game)
  (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
  (oset game trump (car (cg-get game :last-card)))   ; dealer's last card turns trump
  (cg-put game :cursor 0)
  (cg-trick--leader-init game)
  (cg-put game :phase 'play)
  (cg-put game :message (format "Trump is %s.  Take tricks past the book of six."
                                (cg-suit-glyph (oref game trump))))
  (cg-trick--run game))

(cl-defmethod cg-trick--score-hand ((game cg-whist-game))
  (let ((scores (cg-get game :scores)) (tricks (cg-get game :tricks)))
    (dotimes (team 2)
      (let ((over (max 0 (- (+ (aref tricks team) (aref tricks (+ team 2))) 6))))
        (aset scores team (+ (aref scores team) over))
        (aset scores (+ team 2) (aref scores team))))
    (cg-put game :scores scores)))

(cl-defmethod cg-trick--game-over-p ((game cg-whist-game))
  (cl-some (lambda (s) (>= s (oref game target))) (append (cg-get game :scores) nil)))
(cl-defmethod cg-trick--winner-seat ((game cg-whist-game))
  (if (>= (aref (cg-get game :scores) 0) (aref (cg-get game :scores) 1)) 0 1))
(cl-defmethod cg-trick--result-string ((game cg-whist-game))
  (format "%s win" (if (= 0 (cg-trick--winner-seat game)) "You and North" "West and East")))

;; Oh Hell
(defconst cg-ohhell--sizes [7 6 5 4 3 2 1]
  "Hand sizes dealt in successive Oh Hell rounds.")

(cl-defmethod cg-trick--ai-bid ((game cg-ohhell-game) seat)
  (let ((hand (cg-trick--hand game seat)) (trump (oref game trump)) (bid 0))
    (dolist (c hand)
      (cond ((= (cdr c) 12) (cl-incf bid))
            ((and (= (car c) trump) (>= (cdr c) 9)) (cl-incf bid))))
    (min bid (length hand))))

(cl-defmethod cg-trick--begin-hand ((game cg-ohhell-game))
  (let* ((round (or (cg-get game :round) 0))
         (hs (aref cg-ohhell--sizes (min round (1- (length cg-ohhell--sizes))))))
    (oset game hand-size hs)
    (cg-trick--deal game)
    (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
    (let ((up (car (cg-get game :deck))))
      (oset game trump (if up (car up) 0)))
    (cg-put game :cursor 0)
    (let ((bids (make-vector 4 0)))
      (dotimes (s 4) (unless (= s 0) (aset bids s (cg-trick--ai-bid game s))))
      (aset bids 0 (if noninteractive (cg-trick--ai-bid game 0)
                     (max 0 (min hs (read-number
                                     (format "Round %d (trump %s) -- your bid (0-%d): "
                                             (1+ round) (cg-suit-glyph (oref game trump)) hs)
                                     (cg-trick--ai-bid game 0))))))
      (cg-put game :bids bids))
    (cg-trick--leader-init game)
    (cg-put game :phase 'play)
    (cg-put game :message (format "Round %d: make EXACTLY your bid (trump %s)."
                                  (1+ round) (cg-suit-glyph (oref game trump))))
    (cg-trick--run game)))

(cl-defmethod cg-trick--score-hand ((game cg-ohhell-game))
  (let ((scores (cg-get game :scores)) (bids (cg-get game :bids))
        (tricks (cg-get game :tricks)))
    (dotimes (s 4)
      (when (= (aref tricks s) (aref bids s))
        (aset scores s (+ (aref scores s) 10 (aref bids s)))))
    (cg-put game :scores scores)
    (cg-put game :round (1+ (or (cg-get game :round) 0)))))

(cl-defmethod cg-trick--game-over-p ((game cg-ohhell-game))
  (>= (or (cg-get game :round) 0) (length cg-ohhell--sizes)))
(cl-defmethod cg-trick--winner-seat ((game cg-ohhell-game))
  (let ((best 0)) (dotimes (s 4) (when (> (aref (cg-get game :scores) s)
                                          (aref (cg-get game :scores) best)) (setq best s)))
    best))
(cl-defmethod cg-trick--result-string ((game cg-ohhell-game))
  (format "%s wins" (aref cg-trick-seat-names (cg-trick--winner-seat game))))

;;;###autoload
(defun cg-whist () "Play Whist against three computer opponents."
  (interactive) (cg-trick--play-game 'cg-whist-game))
;;;###autoload
(defun cg-ohhell () "Play Oh Hell against three computer opponents."
  (interactive) (cg-trick--play-game 'cg-ohhell-game))

(provide 'cg-trick)
;;; cg-trick.el ends here
