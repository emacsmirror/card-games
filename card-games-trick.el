;;; card-games-trick.el --- Four-hand trick-taking games (Hearts, Spades)  -*- lexical-binding: t; -*-

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

;; A small four-handed trick-taking engine and two games built on it:
;;
;;   `card-games-hearts' -- the classic avoidance game; dodge every heart and the
;;                  Queen of Spades, or take them all to "shoot the moon".
;;   `card-games-spades' -- partnership bidding; spades are always trump; make your
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
(require 'card-games-core)
(require 'card-games-svg)

;;;; Cards

(defconst card-games-trick-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst card-games-trick-seat-names ["South" "West" "North" "East"]
  "Seat names indexed 0..3, going clockwise from the human player.")

(defun card-games-trick-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref card-games-trick-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defsubst card-games-trick-red-p (card) (and card (card-games-red-suit-p (car card))))

(defun card-games-trick--full-deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defun card-games-trick--sort (cards)
  "Return CARDS sorted by suit then rank for display."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (car a) (car b)) (< (cdr a) (cdr b))
                        (< (car a) (car b))))))

;;;; Classes

(defclass card-games-trick-game (card-games-game)
  ((trump :initform nil :documentation "Trump suit index, or nil for none.")
   (restricted :initform 3 :documentation "Suit that cannot be led until broken.")
   (target :initform 100 :documentation "Score that ends the game.")
   (hand-size :initform 13 :documentation "Cards dealt to each seat per hand.")
   (vname  :initform "Trick game"))
  "Abstract base for four-handed trick-taking games."
  :abstract t)

(defclass card-games-hearts-game (card-games-trick-game)
  ((trump :initform nil) (restricted :initform 3) (target :initform 100)
   (vname :initform "Hearts"))
  "Hearts: no trump; avoid hearts and the Queen of Spades.")

(defclass card-games-spades-game (card-games-trick-game)
  ((trump :initform 0) (restricted :initform 0) (target :initform 500)
   (vname :initform "Spades"))
  "Spades: spades are trump; partnership bidding to 500.")

;;;; Dealing

(cl-defmethod card-games-trick--deal ((game card-games-trick-game))
  "Deal a fresh hand into GAME."
  (let ((deck (card-games-trick--full-deck))
        (hands (make-vector 4 nil))
        (hs (oref game hand-size))
        (last nil))
    (dotimes (s 4)
      (let ((h nil))
        (dotimes (_ hs) (setq last (pop deck)) (push last h))
        (aset hands s (card-games-trick--sort h))))
    (card-games-put game :hands hands)
    (card-games-put game :deck deck)
    (card-games-put game :last-card last)
    (card-games-put game :trick nil)
    (card-games-put game :tricks (make-vector 4 0))
    (card-games-put game :taken (make-vector 4 nil))
    (card-games-put game :broken nil)
    (card-games-put game :trick-no 0)
    game))

(defsubst card-games-trick--hand (game s) (aref (card-games-get game :hands) s))
(defsubst card-games-trick--set-hand (game s v) (aset (card-games-get game :hands) s v))
(defsubst card-games-trick--partner (s) (mod (+ s 2) 4))
(defsubst card-games-trick--team (s) (mod s 2))

;;;; Trick mechanics

(defun card-games-trick--led-suit (game)
  "Return the suit led to the current trick, or nil if none yet."
  (let ((tr (card-games-get game :trick)))
    (and tr (car (cdr (car (last tr)))))))   ; first entry played

(defun card-games-trick--first-play (game)
  "Return the (SEAT . CARD) led to the current trick, or nil."
  (car (last (card-games-get game :trick))))

(cl-defmethod card-games-trick--has-points-only-p ((_ card-games-trick-game) _hand) nil)

(cl-defmethod card-games-trick--legal-p ((game card-games-trick-game) seat card)
  "Return non-nil when SEAT may legally play CARD now."
  (let* ((hand (card-games-trick--hand game seat))
         (trick (card-games-get game :trick))
         (restricted (oref game restricted))
         (broken (card-games-get game :broken)))
    (and (member card hand)
         (if trick
             ;; following: must follow the led suit if able
             (let ((led (card-games-trick--led-suit game)))
               (if (cl-some (lambda (c) (= (car c) led)) hand)
                   (= (car card) led)
                 t))
           ;; leading: cannot lead the restricted suit until broken,
           ;; unless the hand holds nothing else
           (if (and (= (car card) restricted) (not broken))
               (cl-every (lambda (c) (= (car c) restricted)) hand)
             t)))))

(defun card-games-trick--legal-moves (game seat)
  "Return the list of cards SEAT may legally play now."
  (cl-remove-if-not (lambda (c) (card-games-trick--legal-p game seat c))
                    (card-games-trick--hand game seat)))

(cl-defmethod card-games-trick--winner ((game card-games-trick-game))
  "Return the seat that wins the now-complete current trick."
  (let* ((trick (reverse (card-games-get game :trick)))   ; play order
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

(cl-defmethod card-games-trick--play ((game card-games-trick-game) seat card)
  "Have SEAT play CARD, resolving the trick when it completes."
  (card-games-trick--set-hand game seat (remove card (card-games-trick--hand game seat)))
  (when (= (car card) (oref game restricted)) (card-games-put game :broken t))
  (card-games-put game :trick (cons (cons seat card) (card-games-get game :trick)))
  (if (= 4 (length (card-games-get game :trick)))
      (let* ((w (card-games-trick--winner game))
             (cards (mapcar #'cdr (card-games-get game :trick))))
        (aset (card-games-get game :tricks) w (1+ (aref (card-games-get game :tricks) w)))
        (aset (card-games-get game :taken) w (append cards (aref (card-games-get game :taken) w)))
        (card-games-put game :trick nil)
        (card-games-put game :trick-no (1+ (card-games-get game :trick-no)))
        (card-games-put game :leader w)
        (card-games-put game :turn w)
        w)
    (card-games-put game :turn (mod (1+ seat) 4))
    nil))

(defun card-games-trick--hand-over-p (game)
  "Return non-nil when all 13 tricks of the hand have been played."
  (and (null (card-games-get game :trick))
       (cl-every #'null (append (card-games-get game :hands) nil))))

;;;; Hearts specifics

(defun card-games-hearts--card-points (card)
  "Return the penalty points for CARD in Hearts."
  (cond ((equal card '(0 . 10)) 13)   ; Queen of Spades
        ((= (car card) 3) 1)          ; any heart
        (t 0)))

(cl-defmethod card-games-trick--legal-p ((game card-games-hearts-game) seat card)
  "Hearts legality, adding the first-trick rules to the base."
  (and (cl-call-next-method)
       (let ((trick (card-games-get game :trick))
             (hand (card-games-trick--hand game seat))
             (first (= 0 (card-games-get game :trick-no))))
         (cond
          ;; the very first card of the hand must be the Two of Clubs
          ((and first (null trick))
           (equal card '(1 . 0)))
          ;; no points on the first trick unless that is all one holds
          ((and first trick (> (card-games-hearts--card-points card) 0))
           (cl-every (lambda (c) (> (card-games-hearts--card-points c) 0)) hand))
          (t t)))))

(cl-defmethod card-games-trick--leader-init ((game card-games-hearts-game))
  "Hearts: the holder of the Two of Clubs leads first."
  (let (seat)
    (dotimes (s 4)
      (when (member '(1 . 0) (card-games-trick--hand game s)) (setq seat s)))
    (card-games-put game :leader seat) (card-games-put game :turn seat)))

(cl-defmethod card-games-trick--leader-init ((game card-games-spades-game))
  "Spades: the player left of the dealer leads first."
  (let ((s (mod (1+ (or (card-games-get game :dealer) 3)) 4)))
    (card-games-put game :leader s) (card-games-put game :turn s)))

(cl-defmethod card-games-trick--score-hand ((game card-games-hearts-game))
  "Score a finished Hearts hand into the cumulative scores."
  (let ((pts (make-vector 4 0)) (scores (card-games-get game :scores)))
    (dotimes (s 4)
      (aset pts s (apply #'+ (mapcar #'card-games-hearts--card-points
                                     (aref (card-games-get game :taken) s)))))
    ;; shooting the moon
    (let ((moon (cl-position 26 (append pts nil))))
      (if moon
          (dotimes (s 4) (unless (= s moon)
                           (aset scores s (+ (aref scores s) 26))))
        (dotimes (s 4) (aset scores s (+ (aref scores s) (aref pts s))))))
    (card-games-put game :last-points pts)))

(cl-defmethod card-games-trick--game-over-p ((game card-games-hearts-game))
  "Hearts ends when any score reaches the target."
  (cl-some (lambda (s) (>= s (oref game target))) (append (card-games-get game :scores) nil)))

(cl-defmethod card-games-trick--winner-seat ((game card-games-hearts-game))
  "Return the winning seat (lowest score) for a finished Hearts game."
  (let ((best 0))
    (dotimes (s 4) (when (< (aref (card-games-get game :scores) s)
                            (aref (card-games-get game :scores) best))
                     (setq best s)))
    best))

;;;; Spades specifics

(cl-defmethod card-games-trick--score-hand ((game card-games-spades-game))
  "Score a finished Spades hand into the cumulative team scores."
  (let ((scores (card-games-get game :scores))
        (bags (card-games-get game :bags))
        (bids (card-games-get game :bids))
        (tricks (card-games-get game :tricks)))
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
    (card-games-put game :scores scores)))

(cl-defmethod card-games-trick--game-over-p ((game card-games-spades-game))
  "Spades ends when a team reaches the target (or falls badly behind)."
  (cl-some (lambda (s) (>= s (oref game target))) (append (card-games-get game :scores) nil)))

(cl-defmethod card-games-trick--winner-seat ((game card-games-spades-game))
  "Return a member seat of the winning team for a finished Spades game."
  (if (>= (aref (card-games-get game :scores) 0) (aref (card-games-get game :scores) 1)) 0 1))

;;;; AI

(cl-defmethod card-games-trick--ai-bid ((game card-games-spades-game) seat)
  "Return a simple trick estimate (bid) for SEAT in Spades."
  (let ((hand (card-games-trick--hand game seat)) (bid 0))
    (dolist (c hand)
      (cond
       ((= (cdr c) 12) (setq bid (1+ bid)))                 ; aces
       ((and (= (cdr c) 11)) (setq bid (1+ bid)))           ; kings
       ((and (= (car c) 0) (>= (cdr c) 9)) (setq bid (1+ bid))))) ; high spades
    ;; long spades add tricks
    (let ((nsp (cl-count-if (lambda (c) (= (car c) 0)) hand)))
      (when (> nsp 4) (setq bid (+ bid (- nsp 4)))))
    (max 1 (min 13 bid))))

(cl-defmethod card-games-trick--ai-play ((game card-games-hearts-game) seat)
  "Choose a legal Hearts card for SEAT, avoiding points."
  (let* ((moves (card-games-trick--legal-moves game seat))
         (trick (card-games-get game :trick)))
    (or
     (if (null trick)
         ;; leading: play a low non-point card
         (car (sort (copy-sequence moves)
                    (lambda (a b) (< (+ (* 4 (card-games-hearts--card-points a)) (cdr a))
                                     (+ (* 4 (card-games-hearts--card-points b)) (cdr b))))))
       ;; following: if we can duck under the current winner, play highest
       ;; safe card; else dump the most dangerous card
       (let* ((led (card-games-trick--led-suit game))
              (winrank (apply #'max (cons -1 (mapcar (lambda (p) (if (= (car (cdr p)) led)
                                                                     (cdr (cdr p)) -1))
                                                     trick))))
              (under (cl-remove-if-not (lambda (c) (and (= (car c) led)
                                                        (< (cdr c) winrank)))
                                       moves)))
         (cond
          (under (car (last (card-games-trick--sort under))))      ; highest still safe
          ((cl-some (lambda (c) (/= (car c) led)) moves)   ; void: dump worst
           (car (sort (copy-sequence moves)
                      (lambda (a b) (> (+ (* 4 (card-games-hearts--card-points a)) (cdr a))
                                       (+ (* 4 (card-games-hearts--card-points b)) (cdr b)))))))
          (t (car (sort (copy-sequence moves)        ; must follow & take: lowest
                        (lambda (a b) (< (cdr a) (cdr b)))))))))
     (car moves))))

(cl-defmethod card-games-trick--ai-play ((game card-games-spades-game) seat)
  "Choose a legal Spades card for SEAT."
  (let* ((moves (card-games-trick--legal-moves game seat))
         (trick (card-games-get game :trick))
         (trump (oref game trump)))
    (or
     (if (null trick)
         ;; lead a high non-spade if possible, else lowest
         (let ((non (cl-remove-if (lambda (c) (= (car c) trump)) moves)))
           (if non (car (last (card-games-trick--sort non)))
             (car (card-games-trick--sort moves))))
       (let* ((led (card-games-trick--led-suit game))
              (cur (card-games-get game :trick))
              ;; current winning play
              (winner (card-games-trick--winner-of game cur))
              (partner-winning (and winner (= (card-games-trick--team winner)
                                              (card-games-trick--team seat)))))
         (if partner-winning
             (car (card-games-trick--sort moves))   ; let partner have it: play low
           ;; try to win cheaply
           (let* ((followers (cl-remove-if-not (lambda (c) (= (car c) led)) moves)))
             (or (car (card-games-trick--sort followers))
                 (car (card-games-trick--sort moves)))))))
     (car moves))))

(defun card-games-trick--winner-of (game trick)
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

(cl-defmethod card-games-trick--start-hand ((game card-games-trick-game))
  "Deal and prepare a new hand, leaving GAME ready for the first lead."
  (card-games-trick--deal game)
  (card-games-trick--leader-init game)
  game)

(defun card-games-trick--simulate-hand (game)
  "Play a whole hand with AI for every seat (used by tests)."
  (while (not (card-games-trick--hand-over-p game))
    (let ((seat (card-games-get game :turn)))
      (card-games-trick--play game seat
                      (if (eq card-games-ai-level 'easy)
                          (let ((moves (card-games-trick--legal-moves game seat)))
                            (nth (random (length moves)) moves))
                        (card-games-trick--ai-play game seat)))))
  (card-games-trick--score-hand game))

;;;; New-game / hand lifecycle

(defvar-local card-games-trick--game nil "The trick-taking game in the current buffer.")

(defconst card-games-trick--pass-dirs [1 3 2 0]
  "Pass directions by hand: left, right, across, hold (then repeat).")

(defun card-games-trick--dir-name (dir)
  "Return a human label for pass direction DIR."
  (pcase dir (1 "left") (3 "right") (2 "across") (_ "hold")))

(cl-defgeneric card-games-trick--begin-hand (game)
  "Deal and set up a new hand of GAME, then run AI up to the human's turn.")

(cl-defmethod card-games-trick--begin-hand ((game card-games-hearts-game))
  (card-games-trick--deal game)
  (card-games-put game :hand-no (1+ (or (card-games-get game :hand-no) 0)))
  (card-games-put game :cursor 0) (card-games-put game :marks nil)
  (let ((dir (aref card-games-trick--pass-dirs (mod (1- (card-games-get game :hand-no)) 4))))
    (card-games-put game :pass-dir dir)
    (if (= dir 0)
        (progn (card-games-trick--leader-init game)
               (card-games-put game :phase 'play)
               (card-games-put game :message "No passing this hand.  Play begins.")
               (card-games-trick--run game))
      (card-games-put game :phase 'pass)
      (card-games-put game :message
              (format "Pass three cards %s.  RET marks a card; p sends them."
                      (card-games-trick--dir-name dir))))))

(cl-defmethod card-games-trick--begin-hand ((game card-games-spades-game))
  (card-games-trick--deal game)
  (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
  (card-games-put game :cursor 0)
  (let ((bids (make-vector 4 0)))
    (dotimes (s 4) (unless (= s 0) (aset bids s (card-games-trick--ai-bid game s))))
    (aset bids 0 (if noninteractive (card-games-trick--ai-bid game 0)
                   (let ((sug (card-games-trick--ai-bid game 0)))
                     (max 0 (min 13 (read-number
                                     (format "Your bid (0 = nil) [suggest %d]: " sug)
                                     sug))))))
    (card-games-put game :bids bids))
  (card-games-trick--leader-init game)
  (card-games-put game :phase 'play)
  (card-games-put game :message
          (format "You bid %d.  Make your side's combined bid."
                  (aref (card-games-get game :bids) 0)))
  (card-games-trick--run game))

(defun card-games-trick--new (game)
  "Initialise GAME for a fresh match and deal the first hand."
  (card-games-put game :scores (make-vector 4 0))
  (card-games-put game :bags (make-vector 2 0))
  (card-games-put game :dealer 3)
  (card-games-put game :hand-no 0)
  (card-games-put game :round 0)
  (card-games-trick--begin-hand game)
  game)

(defun card-games-trick--run (game)
  "Advance AI seats until it is the human's turn or the hand ends."
  (while (and (eq (card-games-get game :phase) 'play)
              (not (card-games-trick--hand-over-p game))
              (/= (card-games-get game :turn) 0))
    (let ((s (card-games-get game :turn)))
      (card-games-trick--play game s (card-games-trick--ai-play game s))))
  (when (and (eq (card-games-get game :phase) 'play) (card-games-trick--hand-over-p game))
    (card-games-trick--finish-hand game)))

(defun card-games-trick--finish-hand (game)
  "Score the finished hand of GAME and start the next, or end the match."
  (card-games-trick--score-hand game)
  (if (card-games-trick--game-over-p game)
      (progn (card-games-put game :phase 'game-over)
             (card-games-put game :message
                     (format "Game over.  %s.  Press n for a new match."
                             (card-games-trick--result-string game))))
    (card-games-trick--begin-hand game)))

(cl-defmethod card-games-trick--result-string ((game card-games-hearts-game))
  (format "%s wins with the lowest score"
          (aref card-games-trick-seat-names (card-games-trick--winner-seat game))))

(cl-defmethod card-games-trick--result-string ((game card-games-spades-game))
  (let ((w (card-games-trick--winner-seat game)))
    (format "%s win" (if (= w 0) "You and North" "West and East"))))

;;;; AI passing

(cl-defmethod card-games-trick--ai-pass ((_ card-games-hearts-game) hand)
  "Return three cards to pass from HAND (shed the most dangerous)."
  (let ((danger (lambda (c) (+ (* 6 (card-games-hearts--card-points c))
                               (if (and (= (car c) 0) (>= (cdr c) 10)) 5 0)
                               (cdr c)))))
    (cl-subseq (sort (copy-sequence hand)
                     (lambda (a b) (> (funcall danger a) (funcall danger b))))
               0 3)))

(defun card-games-trick--do-pass (game)
  "Exchange the chosen passing cards among the four seats of GAME."
  (let* ((dir (card-games-get game :pass-dir))
         (sel (make-vector 4 nil))
         (kept (make-vector 4 nil)))
    (aset sel 0 (copy-sequence (card-games-get game :marks)))
    (dotimes (s 4)
      (unless (= s 0)
        (aset sel s (copy-sequence (card-games-trick--ai-pass game (card-games-trick--hand game s))))))
    ;; what each seat keeps (its hand minus the cards it gives away)
    (dotimes (s 4)
      (aset kept s (cl-remove-if (lambda (c) (member c (aref sel s)))
                                 (card-games-trick--hand game s))))
    ;; deal each seat's three cards to the seat DIR places along
    (dotimes (s 4)
      (let ((r (mod (+ s dir) 4)))
        (aset kept r (append (aref kept r) (aref sel s)))))
    (dotimes (s 4)
      (card-games-trick--set-hand game s (card-games-trick--sort (aref kept s))))
    (card-games-trick--leader-init game)
    (card-games-put game :phase 'play)
    (card-games-put game :marks nil)
    (card-games-put game :message "Cards passed.  Play begins.")
    (card-games-trick--run game)))

(defun card-games-trick--seat-line (game s)
  "Return a status line for opponent seat S of GAME."
  (let* ((n (length (card-games-trick--hand game s)))
         (bid (and (card-games-get game :bids) (aref (card-games-get game :bids) s)))
         (won (and (card-games-get game :tricks) (aref (card-games-get game :tricks) s))))
    (format "  %-6s %2d cards%s%s\n" (aref card-games-trick-seat-names s) n
            (if bid (format "  bid %d" bid) "")
            (if won (format "  won %d" won) ""))))

(defcustom card-games-trick-svg-cards t
  "When non-nil, draw cards as SVG images on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-trick--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref card-games-trick-ranks (cdr card)) (car card)))

(cl-defun card-games-trick--svg-row (cards &key cursor marks hints region-tag)
  "Return a one-image SVG row for CARDS (clickable + sliderful when REGION-TAG)."
  (card-games-svg-hand-image (mapcar #'card-games-trick--spec cards)
                     :cursor cursor :marks marks :hints hints
                     :overlap (if (> (length cards) 11)
                                  (max 0 (- card-games-svg-card-width 24)) 0)
                     :region-tag region-tag))

(defun card-games-trick--draw-backs (svg x y n)
  "Draw up to three overlapped face-down backs at X, Y for a hand of N cards."
  (let ((k (min (max n 0) 3)) (xx x))
    (dotimes (_ k)
      (card-games-svg-card svg xx y :down t)
      (setq xx (+ xx 16)))))

(defun card-games-trick--svg (game)
  "Return a propertized full-table SVG depiction of trick GAME.
The South hand carries clickable (hand . INDEX) regions."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap)
         (pad 16)
         (hand (card-games-trick--sort (card-games-trick--hand game 0)))
         (n (length hand))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (width (max (+ fanw (* 2 pad)) 720))
         (cx (/ width 2))
         (scores (card-games-get game :scores))
         (trick (card-games-get game :trick))
         (turn (card-games-get game :turn))
         (phase (card-games-get game :phase))
         (marks (card-games-get game :marks))
         (cursor (card-games-get game :cursor))
         (bids (card-games-get game :bids))
         (tks (card-games-get game :tricks))
         (y-title 6) (y-north 26)
         (y-tn (+ y-north h 22))
         (cy (+ y-tn (round (* h 0.55))))
         (y-ts (+ cy (round (* h 0.15))))
         (y-south (+ y-ts h 34))
         (height (+ y-south h 30))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels
        ((txt (str x y &optional sz bold anchor)
              (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                     :font-family card-games-svg-font-family
                     (append (and bold '(:font-weight "bold"))
                             (and anchor (list :text-anchor anchor)))))
         (seat (s x y)
               (card-games-trick--draw-backs svg x (+ y 6) (length (card-games-trick--hand game s)))
               (txt (format "%s%s%s%s" (aref card-games-trick-seat-names s)
                            (if (and bids (aref bids s)) (format "  bid %d" (aref bids s)) "")
                            (if (and tks (> (aref tks s) 0)) (format "  won %d" (aref tks s)) "")
                            (if (= turn s) "  <-" ""))
                    x y 11))
         (trick-card (s x y)
                     (let ((play (assq s trick)))
                       (when play
                         (let ((sp (card-games-trick--spec (cdr play))))
                           (card-games-svg-card svg x y :rank (car sp) :suit (cdr sp)))))))
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
          (let ((sp (card-games-trick--spec c))
                (curp (= i cursor))
                (markp (member c marks))
                (hintp (and legalp (card-games-trick--legal-p game 0 c))))
            (card-games-svg-card svg x y-south :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (when markp
              (svg-rectangle svg (- x 3) (- y-south 3) (+ w 6) (+ h 6)
                             :rx 8 :fill "none" :stroke "#4a90d9" :stroke-width 3))
            (push (cons (list x y-south (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale))
                'card-games-regions (nreverse regions))))

(defun card-games-trick--render-text (game)
  "Return a plain-text depiction of trick GAME."
  (let* ((out (list))
         (scores (card-games-get game :scores))
         (marks (card-games-get game :marks))
         (cursor (card-games-get game :cursor))
         (hand (card-games-trick--sort (card-games-trick--hand game 0))))
    (push (format "  %s\n" (oref game vname)) out)
    (when scores
      (push (format "  Scores:  South %d  West %d  North %d  East %d\n\n"
                    (aref scores 0) (aref scores 1) (aref scores 2) (aref scores 3))
            out))
    (dolist (s '(2 1 3))
      (push (card-games-trick--seat-line game s) out))
    (push "\n  Trick: " out)
    (if (null (card-games-get game :trick))
        (push "(empty)" out)
      (dolist (play (reverse (card-games-get game :trick)))
        (push (format "%s:%s  " (aref card-games-trick-seat-names (car play))
                      (let ((cs (card-games-trick-card-string (cdr play))))
                        (if (card-games-trick-red-p (cdr play))
                            (propertize cs 'face 'card-games-red-suit) cs)))
              out)))
    (push "\n\n  Your hand (South):\n   " out)
    (let ((i 0))
      (dolist (c hand)
        (let ((cs (card-games-trick-card-string c)) (faces nil))
          (when (card-games-trick-red-p c) (push 'card-games-red-suit faces))
          (when (member c marks) (push 'card-games-hint faces))
          (when (= i cursor) (push 'card-games-cursor faces))
          (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
        (setq i (1+ i))))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render ((game card-games-trick-game))
  "Return a depiction of GAME: a full SVG table on a graphical display,
else a plain-text board."
  (if (and card-games-trick-svg-cards (display-graphic-p))
      (card-games-trick--svg game)
    (card-games-trick--render-text game)))

(cl-defmethod card-games-render-apply ((g card-games-trick-game) action)
  "Apply a click ACTION on the hand: select that card and play it."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i) (card-games-trick-act))
    (_ (cl-call-next-method))))

(defun card-games-trick--redisplay ()
  "Redraw the current trick-game buffer."
  (let ((game card-games-trick--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-trick--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (card-games-get game :phase) "play")))
    (erase-buffer)
    (insert (card-games-render game))
    (goto-char (point-min))))

;;;; Commands

(defun card-games-trick--cursor-card (game)
  "Return the South card currently under the cursor."
  (nth (card-games-get game :cursor) (card-games-trick--sort (card-games-trick--hand game 0))))

(defun card-games-trick-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((game card-games-trick--game) (n (length (card-games-trick--hand game 0))))
    (when (> n 0) (card-games-put game :cursor (mod (1- (card-games-get game :cursor)) n)))
    (card-games-trick--redisplay)))

(defun card-games-trick-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((game card-games-trick--game) (n (length (card-games-trick--hand game 0))))
    (when (> n 0) (card-games-put game :cursor (mod (1+ (card-games-get game :cursor)) n)))
    (card-games-trick--redisplay)))

(defun card-games-trick-act ()
  "Play, or (during the Hearts pass) mark, the selected card."
  (interactive)
  (let* ((game card-games-trick--game)
         (phase (card-games-get game :phase))
         (card (card-games-trick--cursor-card game)))
    (pcase phase
      ('play
       (cond
        ((/= (card-games-get game :turn) 0) (card-games-put game :message "Not your turn."))
        ((not (card-games-trick--legal-p game 0 card))
         (card-games-put game :message "Illegal play — you must follow suit."))
        (t (card-games-trick--play game 0 card)
           (card-games-put game :cursor (max 0 (min (card-games-get game :cursor)
                                            (1- (length (card-games-trick--hand game 0))))))
           (card-games-trick--run game))))
      ('pass
       (if (member card (card-games-get game :marks))
           (card-games-put game :marks (remove card (card-games-get game :marks)))
         (if (>= (length (card-games-get game :marks)) 3)
             (card-games-put game :message "Three already marked — press p to pass.")
           (card-games-put game :marks (cons card (card-games-get game :marks))))))
      (_ (card-games-put game :message "Press n for a new match.")))
    (card-games-trick--redisplay)))

(defun card-games-trick-pass ()
  "Confirm the Hearts pass once three cards are marked."
  (interactive)
  (let ((game card-games-trick--game))
    (if (and (eq (card-games-get game :phase) 'pass) (= 3 (length (card-games-get game :marks))))
        (card-games-trick--do-pass game)
      (card-games-put game :message "Mark exactly three cards first."))
    (card-games-trick--redisplay)))

(defun card-games-trick-new ()
  "Start a fresh match in this buffer."
  (interactive)
  (card-games-trick--new card-games-trick--game)
  (card-games-trick--redisplay))

(defun card-games-trick-redraw () "Redraw the table." (interactive) (card-games-trick--redisplay))

(defun card-games-trick-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows: choose card  RET: play/mark  p: pass (Hearts)  n: new  g: redraw"))

(defvar card-games-trick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-trick-left)
    (define-key map (kbd "<right>") #'card-games-trick-right)
    (define-key map (kbd "RET") #'card-games-trick-act)
    (define-key map (kbd "SPC") #'card-games-trick-act)
    (define-key map "p" #'card-games-trick-pass)
    (define-key map "n" #'card-games-trick-new)
    (define-key map "g" #'card-games-trick-redraw)
    (define-key map "?" #'card-games-trick-help)
    map)
  "Keymap for `card-games-trick-mode'.")

(define-derived-mode card-games-trick-mode special-mode "Trick"
  "Major mode for the four-handed trick-taking games."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

(defun card-games-trick--play-game (class)
  "Start a trick game of CLASS in its own buffer."
  (let* ((game (make-instance class))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (card-games-trick-mode)
      (setq card-games-trick--game game)
      (card-games-trick--new game)
      (card-games-trick--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun card-games-hearts ()
  "Play Hearts against three computer opponents."
  (interactive) (card-games-trick--play-game 'card-games-hearts-game))

;;;###autoload
(defun card-games-spades ()
  "Play Spades (partnership) against three computer opponents."
  (interactive) (card-games-trick--play-game 'card-games-spades-game))


;;;; Whist and Oh Hell

(defclass card-games-whist-game (card-games-trick-game)
  ((restricted :initform -1) (target :initform 5) (vname :initform "Whist"))
  "Whist: trump set by the turned card, no bidding, score tricks over six.")

(defclass card-games-ohhell-game (card-games-trick-game)
  ((restricted :initform -1) (target :initform 0) (vname :initform "Oh Hell"))
  "Oh Hell: hand size shrinks each round; bid the exact tricks you will take.")

(cl-defmethod card-games-trick--leader-init ((game card-games-whist-game))
  (let ((s (mod (1+ (or (card-games-get game :dealer) 3)) 4)))
    (card-games-put game :leader s) (card-games-put game :turn s)))
(cl-defmethod card-games-trick--leader-init ((game card-games-ohhell-game))
  (let ((s (mod (1+ (or (card-games-get game :dealer) 3)) 4)))
    (card-games-put game :leader s) (card-games-put game :turn s)))

(defun card-games-trick--ai-trump-play (game seat)
  "A generic legal trump-game play for SEAT: follow and win cheaply, else low."
  (let* ((moves (card-games-trick--legal-moves game seat))
         (trick (card-games-get game :trick)))
    (or
     (if (null trick)
         (car (last (card-games-trick--sort moves)))
       (let* ((winner (card-games-trick--winner-of game trick))
              (partner-winning (and winner (= (card-games-trick--team winner)
                                              (card-games-trick--team seat))))
              (led (card-games-trick--led-suit game)))
         (if partner-winning
             (car (card-games-trick--sort moves))
           (let ((follow (cl-remove-if-not (lambda (c) (= (car c) led)) moves)))
             (or (car (last (card-games-trick--sort follow)))
                 (car (card-games-trick--sort moves)))))))
     (car moves))))

(cl-defmethod card-games-trick--ai-play ((game card-games-whist-game) seat)
  (card-games-trick--ai-trump-play game seat))
(cl-defmethod card-games-trick--ai-play ((game card-games-ohhell-game) seat)
  (card-games-trick--ai-trump-play game seat))

;; Whist
(cl-defmethod card-games-trick--begin-hand ((game card-games-whist-game))
  (card-games-trick--deal game)
  (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
  (oset game trump (car (card-games-get game :last-card)))   ; dealer's last card turns trump
  (card-games-put game :cursor 0)
  (card-games-trick--leader-init game)
  (card-games-put game :phase 'play)
  (card-games-put game :message (format "Trump is %s.  Take tricks past the book of six."
                                (card-games-suit-glyph (oref game trump))))
  (card-games-trick--run game))

(cl-defmethod card-games-trick--score-hand ((game card-games-whist-game))
  (let ((scores (card-games-get game :scores)) (tricks (card-games-get game :tricks)))
    (dotimes (team 2)
      (let ((over (max 0 (- (+ (aref tricks team) (aref tricks (+ team 2))) 6))))
        (aset scores team (+ (aref scores team) over))
        (aset scores (+ team 2) (aref scores team))))
    (card-games-put game :scores scores)))

(cl-defmethod card-games-trick--game-over-p ((game card-games-whist-game))
  (cl-some (lambda (s) (>= s (oref game target))) (append (card-games-get game :scores) nil)))
(cl-defmethod card-games-trick--winner-seat ((game card-games-whist-game))
  (if (>= (aref (card-games-get game :scores) 0) (aref (card-games-get game :scores) 1)) 0 1))
(cl-defmethod card-games-trick--result-string ((game card-games-whist-game))
  (format "%s win" (if (= 0 (card-games-trick--winner-seat game)) "You and North" "West and East")))

;; Oh Hell
(defconst card-games-ohhell--sizes [7 6 5 4 3 2 1]
  "Hand sizes dealt in successive Oh Hell rounds.")

(cl-defmethod card-games-trick--ai-bid ((game card-games-ohhell-game) seat)
  (let ((hand (card-games-trick--hand game seat)) (trump (oref game trump)) (bid 0))
    (dolist (c hand)
      (cond ((= (cdr c) 12) (cl-incf bid))
            ((and (= (car c) trump) (>= (cdr c) 9)) (cl-incf bid))))
    (min bid (length hand))))

(cl-defmethod card-games-trick--begin-hand ((game card-games-ohhell-game))
  (let* ((round (or (card-games-get game :round) 0))
         (hs (aref card-games-ohhell--sizes (min round (1- (length card-games-ohhell--sizes))))))
    (oset game hand-size hs)
    (card-games-trick--deal game)
    (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
    (let ((up (car (card-games-get game :deck))))
      (oset game trump (if up (car up) 0)))
    (card-games-put game :cursor 0)
    (let ((bids (make-vector 4 0)))
      (dotimes (s 4) (unless (= s 0) (aset bids s (card-games-trick--ai-bid game s))))
      (aset bids 0 (if noninteractive (card-games-trick--ai-bid game 0)
                     (max 0 (min hs (read-number
                                     (format "Round %d (trump %s) -- your bid (0-%d): "
                                             (1+ round) (card-games-suit-glyph (oref game trump)) hs)
                                     (card-games-trick--ai-bid game 0))))))
      (card-games-put game :bids bids))
    (card-games-trick--leader-init game)
    (card-games-put game :phase 'play)
    (card-games-put game :message (format "Round %d: make EXACTLY your bid (trump %s)."
                                  (1+ round) (card-games-suit-glyph (oref game trump))))
    (card-games-trick--run game)))

(cl-defmethod card-games-trick--score-hand ((game card-games-ohhell-game))
  (let ((scores (card-games-get game :scores)) (bids (card-games-get game :bids))
        (tricks (card-games-get game :tricks)))
    (dotimes (s 4)
      (when (= (aref tricks s) (aref bids s))
        (aset scores s (+ (aref scores s) 10 (aref bids s)))))
    (card-games-put game :scores scores)
    (card-games-put game :round (1+ (or (card-games-get game :round) 0)))))

(cl-defmethod card-games-trick--game-over-p ((game card-games-ohhell-game))
  (>= (or (card-games-get game :round) 0) (length card-games-ohhell--sizes)))
(cl-defmethod card-games-trick--winner-seat ((game card-games-ohhell-game))
  (let ((best 0)) (dotimes (s 4) (when (> (aref (card-games-get game :scores) s)
                                          (aref (card-games-get game :scores) best)) (setq best s)))
    best))
(cl-defmethod card-games-trick--result-string ((game card-games-ohhell-game))
  (format "%s wins" (aref card-games-trick-seat-names (card-games-trick--winner-seat game))))

;;;###autoload
(defun card-games-whist () "Play Whist against three computer opponents."
  (interactive) (card-games-trick--play-game 'card-games-whist-game))
;;;###autoload
(defun card-games-ohhell () "Play Oh Hell against three computer opponents."
  (interactive) (card-games-trick--play-game 'card-games-ohhell-game))

(provide 'card-games-trick)
;;; card-games-trick.el ends here
