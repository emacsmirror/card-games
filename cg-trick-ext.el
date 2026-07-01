;;; cg-trick-ext.el --- Euchre, Pitch and Briscola  -*- lexical-binding: t; -*-

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

;; Three more trick-taking games built on the engine in cg-trick.el, each
;; a four-handed game against three AI opponents (you are South).
;;
;;   `cg-euchre' -- Euchre.  A 24-card deck, the Jack of trump (right
;;     bower) and its same-colour Jack (left bower) outranking everything;
;;     order up or call trump, then take three tricks.  Partnership to 10.
;;   `cg-pitch' -- Auction Pitch (All Fours).  Bid for the privilege of
;;     pitching; the first card led sets trump.  Score High, Low, Jack and
;;     Game.  First to 7.
;;   `cg-briscola' -- Briscola.  A 40-card deck, a fixed trump turned from
;;     the deal, and no need to follow suit; capture the Aces and Threes.
;;     Partnership race to 61 of the 120 points.
;;
;; Cards use the package cons (SUIT . RANK), RANK 0 (Two) .. 12 (Ace) as
;; in cg-trick.el.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)
(require 'cg-trick)

;;;; Shared helpers

(defun cg-tx--deck (ranks)
  "Return a shuffled deck holding only the RANKS (a list of rank indices)."
  (random t)
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r in ranks collect (cons s r)))))

(defun cg-tx--deal (game deck hs)
  "Deal HS cards each from DECK into GAME, in the cg-trick layout."
  (let ((hands (make-vector 4 nil)) (last nil) (d deck))
    (dotimes (s 4)
      (let ((h nil))
        (dotimes (_ hs) (setq last (pop d)) (push last h))
        (aset hands s (cg-trick--sort h))))
    (cg-put game :hands hands)
    (cg-put game :deck d)
    (cg-put game :last-card last)
    (cg-put game :trick nil)
    (cg-put game :tricks (make-vector 4 0))
    (cg-put game :taken (make-vector 4 nil))
    (cg-put game :broken t)
    (cg-put game :trick-no 0)
    game))

(defun cg-tx--winner (plays trump powerfn ledfn)
  "Return the winning seat of PLAYS (a list of (SEAT . CARD), play order).
TRUMP is the trump suit; POWERFN and LEDFN rank cards for this game."
  (let* ((led (funcall ledfn (cdr (car plays)) trump))
         (best (car plays))
         (bestp (funcall powerfn (cdr (car plays)) trump led)))
    (dolist (p (cdr plays))
      (let ((pp (funcall powerfn (cdr p) trump led)))
        (when (> pp bestp) (setq best p bestp pp))))
    (car best)))

(defun cg-tx--ai (game seat powerfn ledfn valuefn)
  "Pick a card for SEAT: win cheaply if leading, else shed the cheapest.
POWERFN, LEDFN rank cards; VALUEFN gives a card's point worth."
  (let* ((legal (cg-trick--legal-moves game seat))
         (trick (cg-get game :trick)) (trump (oref game trump)))
    (if (null trick)
        (car (sort (copy-sequence legal)
                   (lambda (a b) (< (funcall valuefn a) (funcall valuefn b)))))
      (let* ((order (reverse trick))
             (led (funcall ledfn (cdr (car order)) trump))
             (winners '()) (losers '()))
        (dolist (c legal)
          (if (= seat (cg-tx--winner (append order (list (cons seat c)))
                                     trump powerfn ledfn))
              (push c winners) (push c losers)))
        (if winners
            (car (sort winners (lambda (a b) (< (funcall powerfn a trump led)
                                                (funcall powerfn b trump led)))))
          (car (sort (or losers legal)
                     (lambda (a b) (< (funcall valuefn a) (funcall valuefn b))))))))))

(defun cg-tx--plain-led (card _trump) (car card))


;;;; Briscola

(defconst cg-briscola--ranks '(0 1 2 3 4 5 9 10 11 12)
  "Rank indices in a 40-card Briscola deck (no 8, 9, or 10).")

(defclass cg-briscola-game (cg-trick-game)
  ((trump :initform nil) (target :initform 61) (hand-size :initform 10)
   (vname :initform "Briscola"))
  "Briscola: fixed trump, no follow, capture the points.")

(defun cg-bris--power (card _trump _led)
  "Return CARD's rank power within its suit for Briscola."
  (- 10 (or (cl-position (cdr card) '(12 1 11 10 9 5 4 3 2 0)) 10)))

(defun cg-bris--points (card)
  "Return CARD's Briscola point value."
  (pcase (cdr card) (12 11) (1 10) (11 4) (10 3) (9 2) (_ 0)))

(defun cg-bris--win-power (card trump led)
  "Power with trump dominance, for resolving a Briscola trick."
  (cond ((= (car card) trump) (+ 200 (cg-bris--power card trump led)))
        ((= (car card) led) (+ 100 (cg-bris--power card trump led)))
        (t 0)))

(cl-defmethod cg-trick--legal-p ((game cg-briscola-game) seat card)
  "Briscola has no obligation to follow suit."
  (and (member card (cg-trick--hand game seat)) t))

(cl-defmethod cg-trick--winner ((game cg-briscola-game))
  (cg-tx--winner (reverse (cg-get game :trick)) (oref game trump)
                 #'cg-bris--win-power #'cg-tx--plain-led))

(cl-defmethod cg-trick--ai-play ((game cg-briscola-game) seat)
  (cg-tx--ai game seat #'cg-bris--win-power #'cg-tx--plain-led #'cg-bris--points))

(cl-defmethod cg-trick--begin-hand ((game cg-briscola-game))
  (cg-tx--deal game (cg-tx--deck cg-briscola--ranks) 10)
  (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
  (oset game trump (car (cg-get game :last-card)))
  (cg-put game :cursor 0)
  (let ((lead (mod (1+ (cg-get game :dealer)) 4)))
    (cg-put game :leader lead) (cg-put game :turn lead))
  (cg-put game :phase 'play)
  (cg-put game :message
          (format "Trump is %s.  No need to follow suit."
                  (cg-suit-glyph (oref game trump))))
  (cg-trick--run game))

(cl-defmethod cg-trick--score-hand ((game cg-briscola-game))
  (let ((scores (cg-get game :scores)) (tp (make-vector 2 0)))
    (dotimes (s 4)
      (aset tp (cg-trick--team s)
            (+ (aref tp (cg-trick--team s))
               (apply #'+ (mapcar #'cg-bris--points (aref (cg-get game :taken) s))))))
    (dotimes (s 4) (aset scores s (+ (aref scores s) (aref tp (cg-trick--team s)))))))

(cl-defmethod cg-trick--game-over-p ((game cg-briscola-game))
  (or (>= (aref (cg-get game :scores) 0) (oref game target))
      (>= (aref (cg-get game :scores) 1) (oref game target))))

(cl-defmethod cg-trick--winner-seat ((game cg-briscola-game))
  (if (>= (aref (cg-get game :scores) 0) (aref (cg-get game :scores) 1)) 0 1))

(cl-defmethod cg-trick--result-string ((game cg-briscola-game))
  (let ((w (cg-trick--winner-seat game)))
    (format "%s win (%d points)" (if (= w 0) "You and North" "West and East")
            (aref (cg-get game :scores) w))))

;;;###autoload
(defun cg-briscola ()
  "Play Briscola against three AI opponents."
  (interactive)
  (cg-trick--play-game 'cg-briscola-game))


;;;; Auction Pitch

(defclass cg-pitch-game (cg-trick-game)
  ((trump :initform nil) (target :initform 7) (hand-size :initform 6)
   (vname :initform "Pitch"))
  "Auction Pitch: bid, pitch to set trump, score High/Low/Jack/Game.")

(defun cg-pitch--pip (rank)
  "Return the Game-point pip value of RANK."
  (pcase rank (12 4) (11 3) (10 2) (9 1) (8 10) (_ 0)))

(defun cg-pitch--power (card trump led)
  "Rank CARD for a Pitch trick under TRUMP given the LED suit."
  (cond ((and trump (= (car card) trump)) (+ 100 (cdr card)))
        ((= (car card) led) (+ 50 (cdr card)))
        (t (cdr card))))

(cl-defmethod cg-trick--legal-p ((game cg-pitch-game) seat card)
  "Pitch: follow the led suit if able, but you may always trump."
  (let ((hand (cg-trick--hand game seat)) (trick (cg-get game :trick))
        (trump (oref game trump)))
    (and (member card hand)
         (or (null trick)
             (let ((led (cg-trick--led-suit game)))
               (cond ((= (car card) led) t)
                     ((and trump (= (car card) trump)) t)
                     ((cl-some (lambda (c) (= (car c) led)) hand) nil)
                     (t t)))))))

(cl-defmethod cg-trick--play ((game cg-pitch-game) seat card)
  "Set trump from the pitcher's first lead, then play normally."
  (when (and (null (oref game trump)) (null (cg-get game :trick)))
    (oset game trump (car card))
    (cg-put game :message
            (format "%s leads %s -- %s is trump."
                    (aref cg-trick-seat-names seat) (cg-trick-card-string card)
                    (cg-suit-glyph (car card)))))
  (cl-call-next-method))

(cl-defmethod cg-trick--ai-play ((game cg-pitch-game) seat)
  (if (and (null (oref game trump)) (= seat (cg-get game :leader)))
      ;; pitcher's opening lead: lead high from the strongest suit
      (let ((best nil) (bestv -1))
        (dotimes (s 4)
          (let ((v (cg-pitch--suit-strength game seat s)))
            (when (> v bestv) (setq bestv v best s))))
        (car (sort (cl-remove-if-not (lambda (c) (= (car c) best))
                                     (cg-trick--hand game seat))
                   (lambda (a b) (> (cdr a) (cdr b))))))
    (cg-tx--ai game seat #'cg-pitch--power #'cg-tx--plain-led
               (lambda (c) (cg-pitch--pip (cdr c))))))

(defun cg-pitch--suit-strength (game seat suit)
  "Estimate SEAT's strength if SUIT were trump."
  (let ((v 0))
    (dolist (c (cg-trick--hand game seat))
      (when (= (car c) suit)
        (setq v (+ v 2 (pcase (cdr c) (12 4) (11 3) (9 3) (_ 1))))))
    v))

(cl-defmethod cg-trick--ai-bid ((game cg-pitch-game) seat)
  "Return SEAT's Pitch bid (0 to pass, else 2..4), bidding only what is makeable."
  (let ((bid 0))
    (dotimes (s 4)
      (let* ((cards (cl-remove-if-not (lambda (c) (= (car c) s))
                                      (cg-trick--hand game seat)))
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

(defun cg-pitch--read-bid (game high)
  "Prompt you for a Pitch bid that must beat HIGH (or 0 to pass)."
  (let ((sug (cg-trick--ai-bid game 0)))
    (max 0 (min 4 (read-number
                   (format "Your bid (0 pass, else %d-4) [suggest %d]: "
                           (max 2 (1+ high)) sug)
                   sug)))))

(cl-defmethod cg-trick--begin-hand ((game cg-pitch-game))
  (cg-tx--deal game (cg-tx--deck (number-sequence 0 12)) 6)
  (oset game trump nil)
  (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
  (cg-put game :cursor 0)
  (let ((high 0) (bidder nil))
    (dotimes (k 4)
      (let* ((s (mod (+ (cg-get game :dealer) 1 k) 4))
             (b (if (= s 0)
                    (if noninteractive (cg-trick--ai-bid game 0) (cg-pitch--read-bid game high))
                  (cg-trick--ai-bid game s))))
        (when (and (>= b 2) (> b high)) (setq high b bidder s))))
    (unless bidder (setq bidder (cg-get game :dealer) high 2)) ; stuck dealer pitches 2
    (cg-put game :bidder bidder) (cg-put game :bid high)
    (cg-put game :leader bidder) (cg-put game :turn bidder)
    (cg-put game :phase 'play)
    (cg-put game :message
            (format "%s pitches (bid %d).  Their first card sets trump."
                    (aref cg-trick-seat-names bidder) high))
    (cg-trick--run game)))

(cl-defmethod cg-trick--score-hand ((game cg-pitch-game))
  (let* ((trump (oref game trump)) (scores (cg-get game :scores))
         (earned (make-vector 4 0)) (game-pts (make-vector 4 0))
         (bidder (cg-get game :bidder)) (bid (cg-get game :bid))
         (hi nil) (hiseat nil) (lo nil) (loseat nil) (jackseat nil))
    (dotimes (s 4)
      (dolist (c (aref (cg-get game :taken) s))
        (when (= (car c) trump)
          (when (or (null hi) (> (cdr c) hi)) (setq hi (cdr c) hiseat s))
          (when (or (null lo) (< (cdr c) lo)) (setq lo (cdr c) loseat s))
          (when (= (cdr c) 9) (setq jackseat s)))
        (aset game-pts s (+ (aref game-pts s) (cg-pitch--pip (cdr c))))))
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
    (cg-put game :last-earned earned)))

(cl-defmethod cg-trick--game-over-p ((game cg-pitch-game))
  (cl-some (lambda (s) (>= s (oref game target))) (append (cg-get game :scores) nil)))

(cl-defmethod cg-trick--winner-seat ((game cg-pitch-game))
  (let ((best 0)) (dotimes (s 4)
                    (when (> (aref (cg-get game :scores) s)
                             (aref (cg-get game :scores) best)) (setq best s)))
       best))

(cl-defmethod cg-trick--result-string ((game cg-pitch-game))
  (format "%s wins" (aref cg-trick-seat-names (cg-trick--winner-seat game))))

;;;###autoload
(defun cg-pitch ()
  "Play Auction Pitch against three AI opponents."
  (interactive)
  (cg-trick--play-game 'cg-pitch-game))


;;;; Euchre

(defclass cg-euchre-game (cg-trick-game)
  ((trump :initform nil) (target :initform 10) (hand-size :initform 5)
   (vname :initform "Euchre"))
  "Euchre: 24 cards, bowers, order up or call trump, partnership to 10.")

(defun cg-euchre--right-bower-p (card trump)
  (and (= (cdr card) 9) (= (car card) trump)))

(defun cg-euchre--left-bower-p (card trump)
  (and (= (cdr card) 9) (= (car card) (cg-sister-suit trump))))

(defun cg-euchre--eff-suit (card trump)
  "Return CARD's effective suit (the left bower belongs to TRUMP)."
  (if (cg-euchre--left-bower-p card trump) trump (car card)))

(defun cg-euchre--power (card trump led)
  "Rank CARD for a Euchre trick under TRUMP given the effective LED suit."
  (cond ((cg-euchre--right-bower-p card trump) 1000)
        ((cg-euchre--left-bower-p card trump) 999)
        ((= (cg-euchre--eff-suit card trump) trump) (+ 900 (cdr card)))
        ((= (cg-euchre--eff-suit card trump) led) (+ 100 (cdr card)))
        (t (cdr card))))

(defun cg-euchre--eff-led (card trump) (cg-euchre--eff-suit card trump))

(cl-defmethod cg-trick--legal-p ((game cg-euchre-game) seat card)
  "Euchre: follow the effective led suit if able (left bower is trump)."
  (let ((hand (cg-trick--hand game seat)) (trick (cg-get game :trick))
        (trump (oref game trump)))
    (and (member card hand)
         (or (null trick)
             (let ((led (cg-euchre--eff-suit (cdr (cg-trick--first-play game)) trump)))
               (if (cl-some (lambda (c) (= (cg-euchre--eff-suit c trump) led)) hand)
                   (= (cg-euchre--eff-suit card trump) led)
                 t))))))

(cl-defmethod cg-trick--winner ((game cg-euchre-game))
  (cg-tx--winner (reverse (cg-get game :trick)) (oref game trump)
                 #'cg-euchre--power #'cg-euchre--eff-led))

(cl-defmethod cg-trick--ai-play ((game cg-euchre-game) seat)
  (cg-tx--ai game seat #'cg-euchre--power #'cg-euchre--eff-led
             (lambda (c) (cg-euchre--power c (oref game trump) -1))))

(defun cg-euchre--strength (game seat suit)
  "Estimate SEAT's trump strength if SUIT were trump."
  (let ((v 0))
    (dolist (c (cg-trick--hand game seat))
      (cond ((cg-euchre--right-bower-p c suit) (setq v (+ v 4)))
            ((cg-euchre--left-bower-p c suit) (setq v (+ v 3)))
            ((= (cg-euchre--eff-suit c suit) suit) (setq v (+ v 2)))
            ((= (cdr c) 12) (setq v (+ v 1)))))   ; off-ace
    v))

(defun cg-euchre--ai-order (game seat upsuit)
  "Return non-nil if SEAT orders up the UPSUIT."
  (>= (cg-euchre--strength game seat upsuit) 6))

(defun cg-euchre--ai-call (game seat upsuit)
  "Return a suit SEAT calls in round two, or nil to pass."
  (let ((best nil) (bestv 0))
    (dotimes (s 4)
      (unless (= s upsuit)
        (let ((v (cg-euchre--strength game seat s)))
          (when (> v bestv) (setq bestv v best s)))))
    (and (>= bestv 6) best)))

(defun cg-euchre--best-suit (game seat upsuit)
  "Return SEAT's strongest suit other than UPSUIT (for a stuck dealer)."
  (let ((best (mod (1+ upsuit) 4)) (bestv -1))
    (dotimes (s 4)
      (unless (= s upsuit)
        (let ((v (cg-euchre--strength game seat s)))
          (when (> v bestv) (setq bestv v best s)))))
    best))

(defun cg-euchre--dealer-pickup (game up)
  "Dealer takes the UP card and discards their weakest card."
  (let* ((d (cg-get game :dealer)) (trump (car up))
         (hand (cons up (cg-trick--hand game d)))
         (worst (car (sort (copy-sequence hand)
                           (lambda (a b) (< (cg-euchre--power a trump -1)
                                            (cg-euchre--power b trump -1)))))))
    (cg-trick--set-hand game d (cg-trick--sort (remove worst hand)))))

(cl-defmethod cg-trick--begin-hand ((game cg-euchre-game))
  (cg-tx--deal game (cg-tx--deck '(7 8 9 10 11 12)) 5)
  (oset game trump nil)
  (cg-put game :dealer (mod (1+ (or (cg-get game :dealer) 3)) 4))
  (cg-put game :cursor 0)
  (let* ((up (car (cg-get game :deck))) (upsuit (car up))
         (dealer (cg-get game :dealer)) (maker nil) (chosen nil))
    (cg-put game :up up)
    (cl-block bid
      (dotimes (k 4)
        (let ((s (mod (+ dealer 1 k) 4)))
          (when (if (= s 0)
                    (if noninteractive (cg-euchre--ai-order game 0 upsuit)
                      (y-or-n-p (format "Order up %s as trump? " (cg-suit-glyph upsuit))))
                  (cg-euchre--ai-order game s upsuit))
            (setq maker s chosen upsuit)
            (cg-euchre--dealer-pickup game up)
            (cl-return-from bid))))
      (dotimes (k 4)
        (let* ((s (mod (+ dealer 1 k) 4))
               (suit (if (= s 0)
                         (if noninteractive (cg-euchre--ai-call game 0 upsuit)
                           (cg-euchre--human-call upsuit))
                       (cg-euchre--ai-call game s upsuit))))
          (when suit (setq maker s chosen suit) (cl-return-from bid)))))
    (unless chosen
      (setq maker dealer chosen (cg-euchre--best-suit game dealer upsuit)))
    (oset game trump chosen)
    (cg-put game :maker maker)
    (let ((lead (mod (1+ dealer) 4)))
      (cg-put game :leader lead) (cg-put game :turn lead))
    (cg-put game :phase 'play)
    (cg-put game :message
            (format "%s makes %s trump." (aref cg-trick-seat-names maker)
                    (cg-suit-glyph chosen)))
    (cg-trick--run game)))

(defun cg-euchre--human-call (upsuit)
  "Prompt you to name a trump suit other than UPSUIT, or pass."
  (let* ((choices (cl-loop for s below 4 unless (= s upsuit)
                           collect (cons (aref cg-suit-names s) s)))
         (pick (completing-read "Call trump (or RET to pass): "
                                (mapcar #'car choices) nil t)))
    (cdr (assoc pick choices))))

(cl-defmethod cg-trick--score-hand ((game cg-euchre-game))
  (let* ((scores (cg-get game :scores))
         (mteam (cg-trick--team (cg-get game :maker)))
         (mt (+ (aref (cg-get game :tricks) mteam)
                (aref (cg-get game :tricks) (+ mteam 2))))
         (oteam (- 1 mteam)))
    (cl-flet ((award (team n) (dolist (s (list team (+ team 2)))
                                (aset scores s (+ (aref scores s) n)))))
      (cond ((>= mt 5) (award mteam 2))
            ((>= mt 3) (award mteam 1))
            (t (award oteam 2))))))

(cl-defmethod cg-trick--game-over-p ((game cg-euchre-game))
  (or (>= (aref (cg-get game :scores) 0) (oref game target))
      (>= (aref (cg-get game :scores) 1) (oref game target))))

(cl-defmethod cg-trick--winner-seat ((game cg-euchre-game))
  (if (>= (aref (cg-get game :scores) 0) (aref (cg-get game :scores) 1)) 0 1))

(cl-defmethod cg-trick--result-string ((game cg-euchre-game))
  (let ((w (cg-trick--winner-seat game)))
    (format "%s win" (if (= w 0) "You and North" "West and East"))))

;;;###autoload
(defun cg-euchre ()
  "Play Euchre against three AI opponents."
  (interactive)
  (cg-trick--play-game 'cg-euchre-game))

(provide 'cg-trick-ext)
;;; cg-trick-ext.el ends here
