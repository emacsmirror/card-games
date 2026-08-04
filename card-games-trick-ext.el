;;; card-games-trick-ext.el --- Euchre, Pitch and Briscola  -*- lexical-binding: t; -*-

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

;; Three more trick-taking games built on the engine in card-games-trick.el, each
;; a four-handed game against three AI opponents (you are South).
;;
;;   `card-games-euchre' -- Euchre.  A 24-card deck, the Jack of trump (right
;;     bower) and its same-colour Jack (left bower) outranking everything;
;;     order up or call trump, then take three tricks.  Partnership to 10.
;;   `card-games-pitch' -- Auction Pitch (All Fours).  Bid for the privilege of
;;     pitching; the first card led sets trump.  Score High, Low, Jack and
;;     Game.  First to 7.
;;   `card-games-briscola' -- Briscola.  A 40-card deck, a fixed trump turned from
;;     the deal, and no need to follow suit; capture the Aces and Threes.
;;     Partnership race to 61 of the 120 points.
;;
;; Cards use the package cons (SUIT . RANK), RANK 0 (Two) .. 12 (Ace) as
;; in card-games-trick.el.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-trick)

;;;; Shared helpers

(defun card-games-tx--deck (ranks)
  "Return a shuffled deck holding only the RANKS (a list of rank indices)."
  (random t)
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r in ranks collect (cons s r)))))

(defun card-games-tx--deal (game deck hs)
  "Deal HS cards each from DECK into GAME, in the card-games-trick layout."
  (let ((hands (make-vector 4 nil)) (last nil) (d deck))
    (dotimes (s 4)
      (let ((h nil))
        (dotimes (_ hs) (setq last (pop d)) (push last h))
        (aset hands s (card-games-trick--sort h))))
    (card-games-put game :hands hands)
    (card-games-put game :deck d)
    (card-games-put game :last-card last)
    (card-games-put game :trick nil)
    (card-games-put game :tricks (make-vector 4 0))
    (card-games-put game :taken (make-vector 4 nil))
    (card-games-put game :broken t)
    (card-games-put game :trick-no 0)
    game))

(defun card-games-tx--winner (plays trump powerfn ledfn)
  "Return the winning seat of PLAYS (a list of (SEAT . CARD), play order).
TRUMP is the trump suit; POWERFN and LEDFN rank cards for this game."
  (let* ((led (funcall ledfn (cdr (car plays)) trump))
         (best (car plays))
         (bestp (funcall powerfn (cdr (car plays)) trump led)))
    (dolist (p (cdr plays))
      (let ((pp (funcall powerfn (cdr p) trump led)))
        (when (> pp bestp) (setq best p bestp pp))))
    (car best)))

(defun card-games-tx--ai (game seat powerfn ledfn valuefn)
  "Pick a card for GAME SEAT: win cheaply if leading, else shed the cheapest.
POWERFN, LEDFN rank cards; VALUEFN gives a card's point worth."
  (let* ((legal (card-games-trick--legal-moves game seat))
         (trick (card-games-get game :trick)) (trump (oref game trump)))
    (if (null trick)
        (car (sort (copy-sequence legal)
                   (lambda (a b) (< (funcall valuefn a) (funcall valuefn b)))))
      (let* ((order (reverse trick))
             (led (funcall ledfn (cdr (car order)) trump))
             (winners '()) (losers '()))
        (dolist (c legal)
          (if (= seat (card-games-tx--winner (append order (list (cons seat c)))
                                     trump powerfn ledfn))
              (push c winners) (push c losers)))
        (if winners
            (car (sort winners (lambda (a b) (< (funcall powerfn a trump led)
                                                (funcall powerfn b trump led)))))
          (car (sort (or losers legal)
                     (lambda (a b) (< (funcall valuefn a) (funcall valuefn b))))))))))

(defun card-games-tx--plain-led (card _trump) "Return CARD's suit (the plain led suit)." (car card))


;;;; Briscola

(defconst card-games-briscola--ranks '(0 1 2 3 4 5 9 10 11 12)
  "Rank indices in a 40-card Briscola deck (no 8, 9, or 10).")

(defclass card-games-briscola-game (card-games-trick-game)
  ((trump :initform nil) (target :initform 61) (hand-size :initform 10)
   (vname :initform "Briscola"))
  "Briscola: fixed trump, no follow, capture the points.")

(defun card-games-bris--power (card _trump _led)
  "Return CARD's rank power within its suit for Briscola."
  (- 10 (or (cl-position (cdr card) '(12 1 11 10 9 5 4 3 2 0)) 10)))

(defun card-games-bris--points (card)
  "Return CARD's Briscola point value."
  (pcase (cdr card) (12 11) (1 10) (11 4) (10 3) (9 2) (_ 0)))

(defun card-games-bris--win-power (card trump led)
  "Return CARD's Briscola power given TRUMP and LED (trump dominates)."
  (cond ((= (car card) trump) (+ 200 (card-games-bris--power card trump led)))
        ((= (car card) led) (+ 100 (card-games-bris--power card trump led)))
        (t 0)))

(cl-defmethod card-games-trick--legal-p ((game card-games-briscola-game) seat card)
  "In GAME Briscola, SEAT may play any CARD (following suit is optional)."
  (and (member card (card-games-trick--hand game seat)) t))

(cl-defmethod card-games-trick--winner ((game card-games-briscola-game))
  "Return the winning seat of GAME's current Briscola trick."
  (card-games-tx--winner (reverse (card-games-get game :trick)) (oref game trump)
                 #'card-games-bris--win-power #'card-games-tx--plain-led))

(cl-defmethod card-games-trick--ai-play ((game card-games-briscola-game) seat)
  "Return a card for AI SEAT in Briscola GAME."
  (card-games-tx--ai game seat #'card-games-bris--win-power #'card-games-tx--plain-led #'card-games-bris--points))

(cl-defmethod card-games-trick--begin-hand ((game card-games-briscola-game))
  "Begin a Briscola hand in GAME: deal and turn the trump."
  (card-games-tx--deal game (card-games-tx--deck card-games-briscola--ranks) 10)
  (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
  (oset game trump (car (card-games-get game :last-card)))
  (card-games-put game :cursor 0)
  (let ((lead (mod (1+ (card-games-get game :dealer)) 4)))
    (card-games-put game :leader lead) (card-games-put game :turn lead))
  (card-games-put game :phase 'play)
  (card-games-put game :message
          (format "Trump is %s.  No need to follow suit."
                  (card-games-suit-glyph (oref game trump))))
  (card-games-trick--run game))

(cl-defmethod card-games-trick--score-hand ((game card-games-briscola-game))
  "Score GAME's completed Briscola hand."
  (let ((scores (card-games-get game :scores)) (tp (make-vector 2 0)))
    (dotimes (s 4)
      (aset tp (card-games-trick--team s)
            (+ (aref tp (card-games-trick--team s))
               (apply #'+ (mapcar #'card-games-bris--points (aref (card-games-get game :taken) s))))))
    (dotimes (s 4) (aset scores s (+ (aref scores s) (aref tp (card-games-trick--team s)))))))

(cl-defmethod card-games-trick--game-over-p ((game card-games-briscola-game))
  "Return non-nil when GAME's Briscola game is over."
  (or (>= (aref (card-games-get game :scores) 0) (oref game target))
      (>= (aref (card-games-get game :scores) 1) (oref game target))))

(cl-defmethod card-games-trick--winner-seat ((game card-games-briscola-game))
  "Return the winning seat of GAME's Briscola game."
  (if (>= (aref (card-games-get game :scores) 0) (aref (card-games-get game :scores) 1)) 0 1))

(cl-defmethod card-games-trick--result-string ((game card-games-briscola-game))
  "Return GAME's Briscola game-over summary."
  (let ((w (card-games-trick--winner-seat game)))
    (format "%s win (%d points)" (if (= w 0) "You and North" "West and East")
            (aref (card-games-get game :scores) w))))

;;;###autoload
(defun card-games-briscola ()
  "Play Briscola against three AI opponents."
  (interactive)
  (card-games-trick--play-game 'card-games-briscola-game))


;;;; Auction Pitch

(defclass card-games-pitch-game (card-games-trick-game)
  ((trump :initform nil) (target :initform 7) (hand-size :initform 6)
   (vname :initform "Pitch"))
  "Auction Pitch: bid, pitch to set trump, score High/Low/Jack/Game.")

(defun card-games-pitch--pip (rank)
  "Return the Game-point pip value of RANK."
  (pcase rank (12 4) (11 3) (10 2) (9 1) (8 10) (_ 0)))

(defun card-games-pitch--power (card trump led)
  "Rank CARD for a Pitch trick under TRUMP given the LED suit."
  (cond ((and trump (= (car card) trump)) (+ 100 (cdr card)))
        ((= (car card) led) (+ 50 (cdr card)))
        (t (cdr card))))

(cl-defmethod card-games-trick--legal-p ((game card-games-pitch-game) seat card)
  "In GAME Pitch, SEAT follows the led suit if able but may always trump CARD."
  (let ((hand (card-games-trick--hand game seat)) (trick (card-games-get game :trick))
        (trump (oref game trump)))
    (and (member card hand)
         (or (null trick)
             (let ((led (card-games-trick--led-suit game)))
               (cond ((= (car card) led) t)
                     ((and trump (= (car card) trump)) t)
                     ((cl-some (lambda (c) (= (car c) led)) hand) nil)
                     (t t)))))))

(cl-defmethod card-games-trick--play ((game card-games-pitch-game) seat card)
  "In GAME, set trump from the pitcher's first lead (SEAT plays CARD), then play on."
  (when (and (null (oref game trump)) (null (card-games-get game :trick)))
    (oset game trump (car card))
    (card-games-put game :message
            (format "%s leads %s -- %s is trump."
                    (aref card-games-trick-seat-names seat) (card-games-trick-card-string card)
                    (card-games-suit-glyph (car card)))))
  (cl-call-next-method))

(cl-defmethod card-games-trick--ai-play ((game card-games-pitch-game) seat)
  "Return a card for AI SEAT in Pitch GAME."
  (if (and (null (oref game trump)) (= seat (card-games-get game :leader)))
      ;; pitcher's opening lead: lead high from the strongest suit
      (let ((best nil) (bestv -1))
        (dotimes (s 4)
          (let ((v (card-games-pitch--suit-strength game seat s)))
            (when (> v bestv) (setq bestv v best s))))
        (car (sort (cl-remove-if-not (lambda (c) (= (car c) best))
                                     (card-games-trick--hand game seat))
                   (lambda (a b) (> (cdr a) (cdr b))))))
    (card-games-tx--ai game seat #'card-games-pitch--power #'card-games-tx--plain-led
               (lambda (c) (card-games-pitch--pip (cdr c))))))

(defun card-games-pitch--suit-strength (game seat suit)
  "Estimate GAME SEAT's strength if SUIT were trump."
  (let ((v 0))
    (dolist (c (card-games-trick--hand game seat))
      (when (= (car c) suit)
        (setq v (+ v 2 (pcase (cdr c) (12 4) (11 3) (9 3) (_ 1))))))
    v))

(cl-defmethod card-games-trick--ai-bid ((game card-games-pitch-game) seat)
  "Return GAME SEAT's Pitch bid (0 to pass, else 2..4), only what is makeable."
  (let ((bid 0))
    (dotimes (s 4)
      (let* ((cards (cl-remove-if-not (lambda (c) (= (car c) s))
                                      (card-games-trick--hand game seat)))
             (n (length cards))
             (hasa (cl-find 12 cards :key #'cdr))
             (hask (cl-find 11 cards :key #'cdr))
             (hasj (cl-find 9 cards :key #'cdr))
             (b (cond ((and (>= n 4) hasa hasj) 4)
                      ((and (>= n 3) hasa (or hask hasj)) 3)
                      ((and (>= n 3) hasa) 2)
                      ((and (>= n 2) hasa hask) 2)
                      (t 0))))
        (setq bid (max bid b))))
    bid))

(defun card-games-pitch--read-bid (game high)
  "Prompt you for a GAME Pitch bid that must beat HIGH (or 0 to pass)."
  (let ((sug (card-games-trick--ai-bid game 0)))
    (max 0 (min 4 (read-number
                   (format "Your bid (0 pass, else %d-4) [suggest %d]: "
                           (max 2 (1+ high)) sug)
                   sug)))))

(cl-defmethod card-games-trick--begin-hand ((game card-games-pitch-game))
  "Begin a Pitch hand in GAME: deal and run the bidding."
  (card-games-tx--deal game (card-games-tx--deck (number-sequence 0 12)) 6)
  (oset game trump nil)
  (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
  (card-games-put game :cursor 0)
  (let ((high 0) (bidder nil))
    (dotimes (k 4)
      (let* ((s (mod (+ (card-games-get game :dealer) 1 k) 4))
             (b (if (= s 0)
                    (if noninteractive (card-games-trick--ai-bid game 0) (card-games-pitch--read-bid game high))
                  (card-games-trick--ai-bid game s))))
        (when (and (>= b 2) (> b high)) (setq high b bidder s))))
    (unless bidder (setq bidder (card-games-get game :dealer) high 2)) ; stuck dealer pitches 2
    (card-games-put game :bidder bidder) (card-games-put game :bid high)
    (card-games-put game :leader bidder) (card-games-put game :turn bidder)
    (card-games-put game :phase 'play)
    (card-games-put game :message
            (format "%s pitches (bid %d).  Their first card sets trump."
                    (aref card-games-trick-seat-names bidder) high))
    (card-games-trick--run game)))

(cl-defmethod card-games-trick--score-hand ((game card-games-pitch-game))
  "Score GAME's completed Pitch hand."
  (let* ((trump (oref game trump)) (scores (card-games-get game :scores))
         (earned (make-vector 4 0)) (game-pts (make-vector 4 0))
         (bidder (card-games-get game :bidder)) (bid (card-games-get game :bid))
         (hi nil) (hiseat nil) (lo nil) (loseat nil) (jackseat nil))
    (dotimes (s 4)
      (dolist (c (aref (card-games-get game :taken) s))
        (when (= (car c) trump)
          (when (or (null hi) (> (cdr c) hi)) (setq hi (cdr c) hiseat s))
          (when (or (null lo) (< (cdr c) lo)) (setq lo (cdr c) loseat s))
          (when (= (cdr c) 9) (setq jackseat s)))
        (aset game-pts s (+ (aref game-pts s) (card-games-pitch--pip (cdr c))))))
    (when hiseat (aset earned hiseat (1+ (aref earned hiseat))))
    (when loseat (aset earned loseat (1+ (aref earned loseat))))
    (when jackseat (aset earned jackseat (1+ (aref earned jackseat))))
    (let ((best -1) (bs nil) (tie nil))
      (dotimes (s 4)
        (cond ((> (aref game-pts s) best) (setq best (aref game-pts s) bs s tie nil))
              ((= (aref game-pts s) best) (setq tie t))))
      (when (and bs (not tie) (> best 0)) (aset earned bs (1+ (aref earned bs)))))
    (dotimes (s 4)
      (if (= s bidder)
          (if (>= (aref earned s) bid)
              (aset scores s (+ (aref scores s) (aref earned s)))
            (aset scores s (- (aref scores s) bid)))
        (aset scores s (+ (aref scores s) (aref earned s)))))
    (card-games-put game :last-earned earned)))

(cl-defmethod card-games-trick--game-over-p ((game card-games-pitch-game))
  "Return non-nil when GAME's Pitch game is over."
  (cl-some (lambda (s) (>= s (oref game target))) (append (card-games-get game :scores) nil)))

(cl-defmethod card-games-trick--winner-seat ((game card-games-pitch-game))
  "Return the winning seat of GAME's Pitch game."
  (let ((best 0)) (dotimes (s 4)
                    (when (> (aref (card-games-get game :scores) s)
                             (aref (card-games-get game :scores) best)) (setq best s)))
       best))

(cl-defmethod card-games-trick--result-string ((game card-games-pitch-game))
  "Return GAME's Pitch game-over summary."
  (format "%s wins" (aref card-games-trick-seat-names (card-games-trick--winner-seat game))))

;;;###autoload
(defun card-games-pitch ()
  "Play Auction Pitch against three AI opponents."
  (interactive)
  (card-games-trick--play-game 'card-games-pitch-game))


;;;; Euchre

(defclass card-games-euchre-game (card-games-trick-game)
  ((trump :initform nil) (target :initform 10) (hand-size :initform 5)
   (vname :initform "Euchre"))
  "Euchre: 24 cards, bowers, order up or call trump, partnership to 10.")

(defun card-games-euchre--right-bower-p (card trump)
  "Return non-nil when CARD is the right bower for TRUMP."
  (and (= (cdr card) 9) (= (car card) trump)))

(defun card-games-euchre--left-bower-p (card trump)
  "Return non-nil when CARD is the left bower for TRUMP."
  (and (= (cdr card) 9) (= (car card) (card-games-sister-suit trump))))

(defun card-games-euchre--eff-suit (card trump)
  "Return CARD's effective suit (the left bower belongs to TRUMP)."
  (if (card-games-euchre--left-bower-p card trump) trump (car card)))

(defun card-games-euchre--power (card trump led)
  "Rank CARD for a Euchre trick under TRUMP given the effective LED suit."
  (cond ((card-games-euchre--right-bower-p card trump) 1000)
        ((card-games-euchre--left-bower-p card trump) 999)
        ((= (card-games-euchre--eff-suit card trump) trump) (+ 900 (cdr card)))
        ((= (card-games-euchre--eff-suit card trump) led) (+ 100 (cdr card)))
        (t (cdr card))))

(defun card-games-euchre--eff-led (card trump) "Return CARD's effective led suit under TRUMP." (card-games-euchre--eff-suit card trump))

(cl-defmethod card-games-trick--legal-p ((game card-games-euchre-game) seat card)
  "In GAME Euchre, SEAT must follow the effective led suit with CARD if able."
  (let ((hand (card-games-trick--hand game seat)) (trick (card-games-get game :trick))
        (trump (oref game trump)))
    (and (member card hand)
         (or (null trick)
             (let ((led (card-games-euchre--eff-suit (cdr (card-games-trick--first-play game)) trump)))
               (if (cl-some (lambda (c) (= (card-games-euchre--eff-suit c trump) led)) hand)
                   (= (card-games-euchre--eff-suit card trump) led)
                 t))))))

(cl-defmethod card-games-trick--winner ((game card-games-euchre-game))
  "Return the winning seat of GAME's current Euchre trick."
  (card-games-tx--winner (reverse (card-games-get game :trick)) (oref game trump)
                 #'card-games-euchre--power #'card-games-euchre--eff-led))

(cl-defmethod card-games-trick--ai-play ((game card-games-euchre-game) seat)
  "Return a card for AI SEAT in Euchre GAME."
  (card-games-tx--ai game seat #'card-games-euchre--power #'card-games-euchre--eff-led
             (lambda (c) (card-games-euchre--power c (oref game trump) -1))))

(defun card-games-euchre--strength (game seat suit)
  "Estimate GAME SEAT's trump strength if SUIT were trump."
  (let ((v 0))
    (dolist (c (card-games-trick--hand game seat))
      (cond ((card-games-euchre--right-bower-p c suit) (setq v (+ v 4)))
            ((card-games-euchre--left-bower-p c suit) (setq v (+ v 3)))
            ((= (card-games-euchre--eff-suit c suit) suit) (setq v (+ v 2)))
            ((= (cdr c) 12) (setq v (+ v 1)))))   ; off-ace
    v))

(defun card-games-euchre--ai-order (game seat upsuit)
  "Return non-nil if GAME SEAT orders up the UPSUIT."
  (>= (card-games-euchre--strength game seat upsuit) 6))

(defun card-games-euchre--ai-call (game seat upsuit)
  "Return a suit GAME SEAT names in round two after UPSUIT, or nil to pass."
  (let ((best nil) (bestv 0))
    (dotimes (s 4)
      (unless (= s upsuit)
        (let ((v (card-games-euchre--strength game seat s)))
          (when (> v bestv) (setq bestv v best s)))))
    (and (>= bestv 6) best)))

(defun card-games-euchre--best-suit (game seat upsuit)
  "Return GAME SEAT's strongest suit other than UPSUIT (for a stuck dealer)."
  (let ((best (mod (1+ upsuit) 4)) (bestv -1))
    (dotimes (s 4)
      (unless (= s upsuit)
        (let ((v (card-games-euchre--strength game seat s)))
          (when (> v bestv) (setq bestv v best s)))))
    best))

(defun card-games-euchre--dealer-pickup (game up)
  "In GAME, the dealer takes the UP card and discards their weakest."
  (let* ((d (card-games-get game :dealer)) (trump (car up))
         (hand (cons up (card-games-trick--hand game d)))
         (worst (car (sort (copy-sequence hand)
                           (lambda (a b) (< (card-games-euchre--power a trump -1)
                                            (card-games-euchre--power b trump -1)))))))
    (card-games-trick--set-hand game d (card-games-trick--sort (remove worst hand)))))

(cl-defmethod card-games-trick--begin-hand ((game card-games-euchre-game))
  "Begin a Euchre hand in GAME: deal, turn up, run the bidding."
  (card-games-tx--deal game (card-games-tx--deck '(7 8 9 10 11 12)) 5)
  (oset game trump nil)
  (card-games-put game :dealer (mod (1+ (or (card-games-get game :dealer) 3)) 4))
  (card-games-put game :cursor 0)
  (let* ((up (car (card-games-get game :deck))) (upsuit (car up))
         (dealer (card-games-get game :dealer)) (maker nil) (chosen nil))
    (card-games-put game :up up)
    (cl-block bid
      (dotimes (k 4)
        (let ((s (mod (+ dealer 1 k) 4)))
          (when (if (= s 0)
                    (if noninteractive (card-games-euchre--ai-order game 0 upsuit)
                      (y-or-n-p (format "Order up %s as trump? " (card-games-suit-glyph upsuit))))
                  (card-games-euchre--ai-order game s upsuit))
            (setq maker s chosen upsuit)
            (card-games-euchre--dealer-pickup game up)
            (cl-return-from bid))))
      (dotimes (k 4)
        (let* ((s (mod (+ dealer 1 k) 4))
               (suit (if (= s 0)
                         (if noninteractive (card-games-euchre--ai-call game 0 upsuit)
                           (card-games-euchre--human-call upsuit))
                       (card-games-euchre--ai-call game s upsuit))))
          (when suit (setq maker s chosen suit) (cl-return-from bid)))))
    (unless chosen
      (setq maker dealer chosen (card-games-euchre--best-suit game dealer upsuit)))
    (oset game trump chosen)
    (card-games-put game :maker maker)
    (let ((lead (mod (1+ dealer) 4)))
      (card-games-put game :leader lead) (card-games-put game :turn lead))
    (card-games-put game :phase 'play)
    (card-games-put game :message
            (format "%s makes %s trump." (aref card-games-trick-seat-names maker)
                    (card-games-suit-glyph chosen)))
    (card-games-trick--run game)))

(defun card-games-euchre--human-call (upsuit)
  "Prompt you to name a trump suit other than UPSUIT, or pass."
  (let* ((choices (cl-loop for s below 4 unless (= s upsuit)
                           collect (cons (aref card-games-suit-names s) s)))
         (pick (completing-read "Call trump (or RET to pass): "
                                (mapcar #'car choices) nil t)))
    (cdr (assoc pick choices))))

(cl-defmethod card-games-trick--score-hand ((game card-games-euchre-game))
  "Score GAME's completed Euchre hand."
  (let* ((scores (card-games-get game :scores))
         (mteam (card-games-trick--team (card-games-get game :maker)))
         (mt (+ (aref (card-games-get game :tricks) mteam)
                (aref (card-games-get game :tricks) (+ mteam 2))))
         (oteam (- 1 mteam)))
    (cl-flet ((award (team n) (dolist (s (list team (+ team 2)))
                                (aset scores s (+ (aref scores s) n)))))
      (cond ((>= mt 5) (award mteam 2))
            ((>= mt 3) (award mteam 1))
            (t (award oteam 2))))))

(cl-defmethod card-games-trick--game-over-p ((game card-games-euchre-game))
  "Return non-nil when GAME's Euchre game is over."
  (or (>= (aref (card-games-get game :scores) 0) (oref game target))
      (>= (aref (card-games-get game :scores) 1) (oref game target))))

(cl-defmethod card-games-trick--winner-seat ((game card-games-euchre-game))
  "Return the winning seat of GAME's Euchre game."
  (if (>= (aref (card-games-get game :scores) 0) (aref (card-games-get game :scores) 1)) 0 1))

(cl-defmethod card-games-trick--result-string ((game card-games-euchre-game))
  "Return GAME's Euchre game-over summary."
  (let ((w (card-games-trick--winner-seat game)))
    (format "%s win" (if (= w 0) "You and North" "West and East"))))

;;;###autoload
(defun card-games-euchre ()
  "Play Euchre against three AI opponents."
  (interactive)
  (card-games-trick--play-game 'card-games-euchre-game))

(provide 'card-games-trick-ext)
;;; card-games-trick-ext.el ends here
