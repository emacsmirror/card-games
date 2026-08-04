;;; card-games-bridge.el --- Contract Bridge with rubber scoring  -*- lexical-binding: t; -*-

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

;; Contract Bridge: you are South, partnered with North, against East and
;; West.  Each deal has an auction -- bids of a level (1-7) and a strain
;; (clubs, diamonds, hearts, spades, or no-trump), plus Pass, Double, and
;; Redouble -- followed by the play of thirteen tricks with the dummy (the
;; declarer's partner) exposed.  Scoring is the classic rubber game: trick
;; points below the line race toward game, and bonuses, overtricks, and
;; penalties go above; two games win the rubber.
;;
;; When you are declarer you play both your own hand and the dummy; when
;; you defend you play your own cards and the computer plays the rest.
;;
;; The bidding AI is a deliberately small natural system (it opens on
;; about twelve points, raises to game with a fit, and overcalls a good
;; long suit); it reaches sensible contracts but is no expert.  Cards use
;; the package cons (SUIT . RANK), SUIT 0 spades, 1 clubs, 2 diamonds,
;; 3 hearts, RANK 0 (Two) .. 12 (Ace).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-bridge-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst card-games-bridge-strains ["♣" "♦" "♥" "♠" "NT"]
  "Strain glyphs indexed 0 clubs, 1 diamonds, 2 hearts, 3 spades, 4 no-trump.")

(defconst card-games-bridge--strain-suit [1 2 3 0 nil]
  "Map a strain index to its trump suit index (nil for no-trump).")

(defconst card-games-bridge--suit-strain [3 0 1 2]
  "Map a suit index (0 S,1 C,2 D,3 H) to its strain index.")

(defconst card-games-bridge-seat-names ["South" "West" "North" "East"]
  "Seat names indexed 0..3 clockwise from the human.")

(defclass card-games-bridge-game (card-games-game)
  ((vname :initform "Bridge"))
  "A game of contract Bridge.")

(defun card-games-bridge-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref card-games-bridge-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defun card-games-bridge--sort (cards)
  "Return CARDS sorted by suit then rank (high first) for display."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (car a) (car b)) (> (cdr a) (cdr b)) (< (car a) (car b))))))

(defun card-games-bridge--deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

;;;; Hand evaluation

(defun card-games-bridge--hcp (hand)
  "Return the high-card points of HAND (A=4 K=3 Q=2 J=1)."
  (let ((p 0))
    (dolist (c hand p)
      (setq p (+ p (pcase (cdr c) (12 4) (11 3) (10 2) (9 1) (_ 0)))))))

(defun card-games-bridge--suit-len (hand suit)
  "Return how many cards of SUIT are in HAND."
  (cl-count suit hand :key #'car))

(defun card-games-bridge--longest (hand)
  "Return the suit index HAND has most of (ties prefer majors, then spades)."
  (let ((best 0) (bestn -1))
    ;; check in order hearts, diamonds, clubs, spades so spades win ties last
    (dolist (s '(3 2 1 0))
      (let ((n (card-games-bridge--suit-len hand s)))
        (when (>= n bestn) (setq bestn n best s))))
    best))

(defun card-games-bridge--balanced-p (hand)
  "Return non-nil when HAND has a balanced shape (no void/singleton)."
  (let ((doubletons 0) (ok t))
    (dotimes (s 4)
      (let ((n (card-games-bridge--suit-len hand s)))
        (when (< n 2) (setq ok nil))
        (when (= n 2) (setq doubletons (1+ doubletons)))))
    (and ok (<= doubletons 1))))

;;;; Auction mechanics

(defsubst card-games-bridge--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-bridge--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))
(defsubst card-games-bridge--side (s) "Return the partnership side (0 or 1) of seat S." (mod s 2))

(cl-defmethod card-games-bridge--deal ((game card-games-bridge-game))
  "Deal a fresh Bridge hand into GAME, leaving it ready for the auction."
  (let ((deck (card-games-bridge--deck)) (hands (make-vector 4 nil)))
    (dotimes (s 4)
      (aset hands s (card-games-bridge--sort (cl-loop repeat 13 collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :calls nil)            ; list of (SEAT . CALL), newest first
    (card-games-put game :contract nil)         ; (LEVEL . STRAIN)
    (card-games-put game :declarer nil)
    (card-games-put game :doubled 0)
    (card-games-put game :dealer (or (card-games-get game :dealer) 0))
    (card-games-put game :bidder (card-games-get game :dealer))
    (card-games-put game :phase 'auction)
    (card-games-put game :cursor 0)
    (card-games-put game :bid-level 1)          ; UI: level being composed
    (card-games-put game :bid-strain 0)
    (card-games-put game :trick nil)
    (card-games-put game :tricks 0)             ; declarer-side tricks won
    (card-games-put game :dummy nil)
    (card-games-put game :exposed nil)
    (unless (card-games-get game :below) (card-games-put game :below (make-vector 2 0)))
    (unless (card-games-get game :above) (card-games-put game :above (make-vector 2 0)))
    (unless (card-games-get game :games) (card-games-put game :games (make-vector 2 0)))
    (unless (card-games-get game :vul) (card-games-put game :vul (make-vector 2 nil)))
    (card-games-put game :message "Auction: compose a bid and press RET, or p/d to pass/double.")
    game))

(defun card-games-bridge--high-bid (game)
  "Return GAME's highest (LEVEL . STRAIN) bid so far, or nil."
  (cl-loop for (_s . call) in (card-games-get game :calls)
           when (consp call) return call))

(defun card-games-bridge--high-bidder (game)
  "Return the GAME seat that made the highest bid, or nil."
  (cl-loop for (s . call) in (card-games-get game :calls)
           when (consp call) return s))

(defun card-games-bridge--call> (a b)
  "Return non-nil when bid A is higher than bid B (each (LEVEL . STRAIN))."
  (or (null b)
      (> (car a) (car b))
      (and (= (car a) (car b)) (> (cdr a) (cdr b)))))

(defun card-games-bridge--legal-call-p (game call)
  "Return non-nil when CALL is legal now in GAME."
  (let ((high (card-games-bridge--high-bid game))
        (hb (card-games-bridge--high-bidder game)))
    (pcase call
      ('pass t)
      ('double (and high (/= (card-games-bridge--side hb) (card-games-bridge--side (card-games-get game :bidder)))
                    (= (card-games-get game :doubled) 0)))
      ('redouble (and high (= (card-games-bridge--side hb) (card-games-bridge--side (card-games-get game :bidder)))
                      (= (card-games-get game :doubled) 1)))
      (_ (and (consp call) (>= (car call) 1) (<= (car call) 7)
              (card-games-bridge--call> call high))))))

(defun card-games-bridge--apply-call (game seat call)
  "Record CALL by SEAT in GAME and update doubling state."
  (card-games-put game :calls (cons (cons seat call) (card-games-get game :calls)))
  (pcase call
    ('double (card-games-put game :doubled 1))
    ('redouble (card-games-put game :doubled 2))
    ((pred consp) (card-games-put game :doubled 0)))
  (card-games-put game :bidder (mod (1+ seat) 4)))

(defun card-games-bridge--auction-done-p (game)
  "Return non-nil when GAME's auction has ended.
Sets up the contract (or a pass-out) as a side effect."
  (let* ((calls (card-games-get game :calls)) (n (length calls)))
    (cond
     ;; four passes with no bid: passed out
     ((and (= n 4) (cl-every (lambda (c) (eq (cdr c) 'pass)) calls))
      (card-games-put game :phase 'passed-out) t)
     ;; a bid then three passes
     ((and (card-games-bridge--high-bid game)
           (>= n 3)
           (cl-every (lambda (c) (eq (cdr c) 'pass))
                     (cl-subseq calls 0 3)))
      (card-games-bridge--establish-contract game) t)
     (t nil))))

(defun card-games-bridge--establish-contract (game)
  "Set GAME's contract, declarer, and start of play from the auction."
  (let* ((bid (card-games-bridge--high-bid game))
         (side (card-games-bridge--side (card-games-bridge--high-bidder game)))
         (strain (cdr bid))
         (declarer
          ;; first player of SIDE to have named STRAIN
          (cl-loop for (s . call) in (reverse (card-games-get game :calls))
                   when (and (consp call) (= (cdr call) strain)
                             (= (card-games-bridge--side s) side))
                   return s)))
    (card-games-put game :contract bid)
    (card-games-put game :declarer declarer)
    (card-games-put game :dummy (mod (+ declarer 2) 4))
    (card-games-put game :phase 'play)
    (card-games-put game :leader (mod (1+ declarer) 4))
    (card-games-put game :turn (mod (1+ declarer) 4))
    (card-games-put game :trick nil)
    (card-games-put game :tricks 0)
    (card-games-put game :cursor 0)
    (card-games-put game :message
            (format "Contract: %s by %s.  %s leads."
                    (card-games-bridge--contract-string game)
                    (aref card-games-bridge-seat-names declarer)
                    (aref card-games-bridge-seat-names (card-games-get game :leader))))))

(defun card-games-bridge--contract-string (game)
  "Return a label for GAME's contract, e.g. \"4NT x\"."
  (let ((c (card-games-get game :contract)) (d (card-games-get game :doubled)))
    (if (null c) "passed out"
      (format "%d%s%s" (car c) (aref card-games-bridge-strains (cdr c))
              (pcase d (1 " x") (2 " xx") (_ ""))))))

;;;; Play mechanics

(defun card-games-bridge--trump (game)
  "Return the trump suit index for GAME, or nil for no-trump."
  (and (card-games-get game :contract) (aref card-games-bridge--strain-suit (cdr (card-games-get game :contract)))))

(defun card-games-bridge--led-suit (game)
  "Return the suit led to GAME's current trick, or nil."
  (let ((tr (card-games-get game :trick)))
    (and tr (car (cdr (car (last tr)))))))

(defun card-games-bridge--legal-play-p (game seat card)
  "Return non-nil when SEAT may play CARD in GAME now (follow suit if able)."
  (let ((hand (card-games-bridge--hand game seat)) (led (card-games-bridge--led-suit game)))
    (and (member card hand)
         (or (null led)
             (= (car card) led)
             (not (cl-some (lambda (c) (= (car c) led)) hand))))))

(defun card-games-bridge--legal-plays (game seat)
  "Return SEAT's legal cards in GAME now."
  (cl-remove-if-not (lambda (c) (card-games-bridge--legal-play-p game seat c))
                    (card-games-bridge--hand game seat)))

(defun card-games-bridge--trick-winner (plays trump)
  "Return the winning seat of complete PLAYS, given TRUMP ((SEAT . CARD) order)."
  (let ((best (car plays)))
    (dolist (p (cdr plays))
      (let ((bc (cdr best)) (pc (cdr p)))
        (cond
         ((and trump (= (car pc) trump) (/= (car bc) trump)) (setq best p))
         ((and (= (car pc) (car bc)) (> (cdr pc) (cdr bc))) (setq best p)))))
    (car best)))

(defun card-games-bridge--play-card (game seat card)
  "Have SEAT play CARD in GAME; resolve and score the trick when complete."
  (card-games-bridge--set-hand game seat (remove card (card-games-bridge--hand game seat)))
  (card-games-put game :trick (cons (cons seat card) (card-games-get game :trick)))
  ;; expose the dummy after the opening lead
  (unless (card-games-get game :exposed)
    (card-games-put game :exposed t))
  (if (= 4 (length (card-games-get game :trick)))
      (let ((w (card-games-bridge--trick-winner (reverse (card-games-get game :trick))
                                        (card-games-bridge--trump game))))
        (when (= (card-games-bridge--side w) (card-games-bridge--side (card-games-get game :declarer)))
          (card-games-put game :tricks (1+ (card-games-get game :tricks))))
        (card-games-put game :trick nil)
        (card-games-put game :leader w)
        (card-games-put game :turn w)
        (card-games-put game :last-winner w)
        (when (cl-every #'null (append (card-games-get game :hands) nil))
          (card-games-bridge--score-deal game))
        w)
    (card-games-put game :turn (mod (1+ seat) 4))
    nil))

;;;; Scoring (rubber)

(defun card-games-bridge--undertrick-points (n doubled vul)
  "Return defender points for N undertricks at DOUBLED level and VUL state."
  (cond
   ((= doubled 0) (* n (if vul 100 50)))
   (t (let ((mult (if (= doubled 2) 2 1)) (sum 0))
        (dotimes (i n)
          (setq sum (+ sum (* mult (if vul (if (= i 0) 200 300)
                                     (cond ((= i 0) 100) ((< i 3) 200) (t 300)))))))
        sum))))

(defun card-games-bridge--deal-score (level strain doubled vul tricks)
  "Return a plist scoring a contract result.
LEVEL/STRAIN/DOUBLED describe the contract, VUL the declarer's
vulnerability, and TRICKS the declarer side's trick count.  Keys:
:below contracted points, :datk declarer bonus points above the line,
:defend defender points, :result tricks over/under the contract."
  (let* ((need (+ 6 level)) (result (- tricks need))
         (mult (pcase doubled (0 1) (1 2) (2 4)))
         (below 0) (datk 0) (defend 0))
    (if (>= result 0)
        (progn
          (setq below (* mult (if (= strain 4) (+ 40 (* 30 (1- level)))
                                (* (if (<= strain 1) 20 30) level))))
          (when (> result 0)
            (setq datk (+ datk (if (= doubled 0)
                                   (* result (if (= strain 4) 30 (if (<= strain 1) 20 30)))
                                 (* result (* (if (= doubled 2) 2 1) (if vul 200 100)))))))
          (when (> doubled 0) (setq datk (+ datk (if (= doubled 2) 100 50))))
          (cond ((= level 6) (setq datk (+ datk (if vul 750 500))))
                ((= level 7) (setq datk (+ datk (if vul 1500 1000))))))
      (setq defend (card-games-bridge--undertrick-points (- result) doubled vul)))
    (list :below below :datk datk :defend defend :result result)))

(defun card-games-bridge--score-deal (game)
  "Score the finished deal into GAME's rubber state."
  (let* ((c (card-games-get game :contract)) (level (car c)) (strain (cdr c))
         (decl (card-games-get game :declarer)) (side (card-games-bridge--side decl))
         (opp (- 1 side)) (doubled (card-games-get game :doubled))
         (vul (aref (card-games-get game :vul) side))
         (sc (card-games-bridge--deal-score level strain doubled vul (card-games-get game :tricks)))
         (below (card-games-get game :below)) (above (card-games-get game :above)))
    (aset below side (+ (aref below side) (plist-get sc :below)))
    (aset above side (+ (aref above side) (plist-get sc :datk)))
    (aset above opp (+ (aref above opp) (plist-get sc :defend)))
    (card-games-put game :deal-result sc)
    ;; game / rubber bookkeeping
    (when (>= (aref below side) 100)
      (let ((games (card-games-get game :games)))
        (aset games side (1+ (aref games side)))
        (aset (card-games-get game :vul) side t)
        (aset below 0 0) (aset below 1 0)
        (when (>= (aref games side) 2)
          (aset above side (+ (aref above side)
                              (if (>= (aref games opp) 1) 500 700)))
          (card-games-put game :rubber-winner side))))
    (card-games-put game :phase 'scored)
    (card-games-put game :message
            (format "%s: %s.  %s"
                    (card-games-bridge--contract-string game)
                    (let ((r (plist-get sc :result)))
                      (cond ((>= r 0) (format "made +%d" r))
                            (t (format "down %d" (- r)))))
                    (if (card-games-get game :rubber-winner)
                        (format "%s win the rubber!  (n: new rubber)"
                                (if (= side 0) "You and North" "East and West"))
                      "(n: next deal)")))))

;;;; AI -- bidding

(cl-defmethod card-games-bridge--ai-call ((game card-games-bridge-game) seat)
  "Return a call for GAME AI SEAT from a small natural system."
  (let* ((hand (card-games-bridge--hand game seat)) (hcp (card-games-bridge--hcp hand))
         (high (card-games-bridge--high-bid game)) (hb (card-games-bridge--high-bidder game))
         (ours (and high (= (card-games-bridge--side hb) (card-games-bridge--side seat)))))
    (cond
     ((null high)                       ; opening
      (cond ((and (card-games-bridge--balanced-p hand) (>= hcp 15) (<= hcp 17)) (cons 1 4))
            ((and (card-games-bridge--balanced-p hand) (>= hcp 20) (<= hcp 21)) (cons 2 4))
            ((>= hcp 12)
             (let ((suit (card-games-bridge--longest hand)))
               (cons 1 (aref card-games-bridge--suit-strain suit))))
            (t 'pass)))
     (ours                              ; partner has the contract
      (let* ((est (+ hcp 13)) (hl (car high)) (hs (cdr high))
             (fit (or (= hs 4)
                      (>= (card-games-bridge--suit-len
                           hand (aref card-games-bridge--strain-suit hs)) 3))))
        (if (and fit (>= est 26) (< hl 4)
                 (card-games-bridge--legal-call-p
                  game (cond ((= hs 4) (cons 3 4))
                             ((>= hs 2) (cons 4 hs))
                             (t (cons 5 hs)))))
            (cond ((= hs 4) (cons 3 4)) ((>= hs 2) (cons 4 hs)) (t (cons 5 hs)))
          'pass)))
     (t                                 ; opponents have the contract
      (let* ((suit (card-games-bridge--longest hand))
             (len (card-games-bridge--suit-len hand suit))
             (st (aref card-games-bridge--suit-strain suit))
             (cand (if (> st (cdr high)) (cons (car high) st)
                     (cons (1+ (car high)) st))))
        (if (and (>= hcp 11) (>= len 5) (<= (car cand) 3)
                 (card-games-bridge--legal-call-p game cand))
            cand 'pass))))))

;;;; AI -- play

(cl-defmethod card-games-bridge--ai-play ((game card-games-bridge-game) seat)
  "Return a card for GAME AI SEAT: win cheaply or shed low."
  (let* ((legal (card-games-bridge--legal-plays game seat))
         (trump (card-games-bridge--trump game)) (trick (card-games-get game :trick)))
    (if (null trick)
        ;; leading: low from the longest non-trump suit, else lowest
        (car (sort (copy-sequence legal)
                   (lambda (a b) (< (cdr a) (cdr b)))))
      (let* ((order (reverse trick))
             (cur (card-games-bridge--trick-winner order trump))
             (partner (= (card-games-bridge--side cur) (card-games-bridge--side seat)))
             (winners (cl-remove-if-not
                       (lambda (c) (= seat (card-games-bridge--trick-winner
                                            (append order (list (cons seat c))) trump)))
                       legal)))
        (cond
         ;; partner already winning: throw the lowest card
         ((and partner (>= (length trick) 1))
          (car (sort (copy-sequence legal) (lambda (a b) (< (cdr a) (cdr b))))))
         ;; can win: take it with the cheapest winner
         (winners (car (sort winners (lambda (a b) (< (cdr a) (cdr b))))))
         ;; cannot win: discard lowest
         (t (car (sort (copy-sequence legal) (lambda (a b) (< (cdr a) (cdr b)))))))))))

(defun card-games-bridge--controls (game)
  "Return the GAME seats the human controls during play."
  (let ((decl (card-games-get game :declarer)))
    (cond ((null decl) nil)
          ((= decl 0) '(0 2))           ; South declares: play hand + dummy
          ((= decl 2) nil)              ; North declares: AI plays both
          (t '(0)))))                   ; South defends

(defun card-games-bridge--auto-seat-p (game seat)
  "Return non-nil when SEAT is played automatically (by AI) in GAME."
  (not (memq seat (card-games-bridge--controls game))))

(defun card-games-bridge--run-play (game)
  "Advance GAME's AI plays until a human seat must act or the deal ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play)
                (card-games-bridge--auto-seat-p game (card-games-get game :turn))
                (< guard 60))
      (setq guard (1+ guard))
      (card-games-bridge--play-card game (card-games-get game :turn)
                            (card-games-bridge--ai-play game (card-games-get game :turn))))))

(defun card-games-bridge--run-auction (game)
  "Advance GAME's auction through AI seats until South must call or it ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'auction) (/= (card-games-get game :bidder) 0)
                (< guard 40))
      (setq guard (1+ guard))
      (let* ((s (card-games-get game :bidder)) (call (card-games-bridge--ai-call game s)))
        (unless (card-games-bridge--legal-call-p game call) (setq call 'pass))
        (card-games-bridge--apply-call game s call)
        (card-games-bridge--auction-done-p game)))
    (when (eq (card-games-get game :phase) 'play) (card-games-bridge--run-play game))))

;;;; UI

(defvar-local card-games-bridge--game nil "The Bridge game in the current buffer.")

(defun card-games-bridge--hand-by-suit (cards)
  "Return CARDS grouped into four lines by suit, as a string."
  (let ((out '()))
    (dolist (s '(0 3 2 1))              ; S H D C
      (let ((in (card-games-bridge--sort (cl-remove-if-not (lambda (c) (= (car c) s)) cards))))
        (push (format "    %s %s\n" (card-games-suit-glyph s)
                      (if in (mapconcat (lambda (c) (aref card-games-bridge-ranks (cdr c))) in " ")
                        "--"))
              out)))
    (apply #'concat (nreverse out))))

(defun card-games-bridge--auction-string (game)
  "Return a compact record of GAME's auction so far."
  (let ((calls (reverse (card-games-get game :calls))) (out '()))
    (dolist (sc calls)
      (push (format "%s:%s" (aref card-games-bridge-seat-names (car sc))
                    (pcase (cdr sc)
                      ('pass "pass") ('double "X") ('redouble "XX")
                      (c (format "%d%s" (car c) (aref card-games-bridge-strains (cdr c))))))
            out))
    (if out (mapconcat #'identity (nreverse out) "  ") "(no calls yet)")))

(defcustom card-games-bridge-svg-cards t
  "When non-nil, draw cards as SVG images on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-bridge--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref card-games-bridge-ranks (cdr card)) (car card)))

(cl-defun card-games-bridge--svg-row (cards &key cursor hints region-tag)
  "Return an SVG row for CARDS with CURSOR and HINTS, clickable via REGION-TAG."
  (card-games-svg-hand-image (mapcar #'card-games-bridge--spec cards)
                     :cursor cursor :hints hints
                     :overlap (if (> (length cards) 11)
                                  (max 0 (- card-games-svg-card-width 26)) 0)
                     :region-tag region-tag))

(defun card-games-bridge--draw-backs (svg x y n)
  "Draw up to three overlapped backs on SVG at X, Y for a hand of N cards."
  (let ((k (min (max n 0) 3)) (xx x))
    (dotimes (_ k) (card-games-svg-card svg xx y :down t) (setq xx (+ xx 16)))))

(defun card-games-bridge--svg (game)
  "Return an SVG board for the Bridge GAME (four seats, dummy exposed)."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap) (pad 16)
         (phase (card-games-get game :phase)) (cursor (card-games-get game :cursor))
         (turn (card-games-get game :turn)) (dummy (card-games-get game :dummy))
         (exposed (card-games-get game :exposed)) (trick (card-games-get game :trick))
         (act (if (and (eq phase 'play) (memq turn (card-games-bridge--controls game))) turn 0))
         (ahand (card-games-bridge--sort (card-games-bridge--hand game act)))
         (n (length ahand))
         (overlap (cond ((> n 11) (- w 26)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (width (max (+ fanw (* 2 pad)) 760))
         (cx (/ width 2))
         (y-title 6) (y-info 24) (y-north 62)
         (y-tn (+ y-north h 20))
         (cyc (+ y-tn (round (* h 0.5))))
         (y-ts (+ cyc (round (* h 0.15))))
         (y-hand (+ y-ts h 42))
         (height (+ y-hand h 30))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels
        ((txt (str x y &optional sz bold)
              (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                     :font-family card-games-svg-font-family (and bold '(:font-weight "bold"))))
         (seat (s x y)
               (if (and exposed (eql s dummy) (/= s act))
                   (let ((cs (card-games-bridge--sort (card-games-bridge--hand game s))) (xx x))
                     (dolist (c cs)
                       (let ((sp (card-games-bridge--spec c)))
                         (card-games-svg-card svg xx y :rank (car sp) :suit (cdr sp)))
                       (setq xx (+ xx 15))))
                 (card-games-bridge--draw-backs svg x (+ y 6) (length (card-games-bridge--hand game s))))
               (txt (format "%s%s%s" (aref card-games-bridge-seat-names s)
                            (if (eql s dummy) " (dummy)" "")
                            (if (= turn s) "  <-" ""))
                    x y 11))
         (trick-card (s x y)
                     (let ((play (assq s trick)))
                       (when play
                         (let ((sp (card-games-bridge--spec (cdr play))))
                           (card-games-svg-card svg x y :rank (car sp) :suit (cdr sp)))))))
      (txt "Bridge" pad (+ y-title 12) 13 t)
      (txt (format "Games  N-S %d  E-W %d    Below %d/%d   Above %d/%d"
                   (aref (card-games-get game :games) 0) (aref (card-games-get game :games) 1)
                   (aref (card-games-get game :below) 0) (aref (card-games-get game :below) 1)
                   (aref (card-games-get game :above) 0) (aref (card-games-get game :above) 1))
           pad (+ y-info 8) 11)
      (pcase phase
        ('auction
         (txt (format "Auction: %s" (card-games-bridge--auction-string game)) pad (+ y-info 24) 11)
         (txt (format "Your bid: %d %s   (arrows compose, RET bids)"
                      (card-games-get game :bid-level)
                      (aref card-games-bridge-strains (card-games-get game :bid-strain)))
              pad (+ y-info 40) 11))
        ((or 'play 'scored 'passed-out)
         (txt (format "Contract: %s by %s    Declarer tricks: %d"
                      (card-games-bridge--contract-string game)
                      (if (card-games-get game :declarer)
                          (aref card-games-bridge-seat-names (card-games-get game :declarer)) "--")
                      (card-games-get game :tricks))
              pad (+ y-info 24) 11)))
      (seat 2 (- cx 40) y-north)
      (seat 1 pad cyc)
      (seat 3 (- width pad 110) cyc)
      (when (eq phase 'play)
        (trick-card 2 (- cx (/ w 2)) y-tn)
        (trick-card 0 (- cx (/ w 2)) y-ts)
        (trick-card 1 (- cx w (round (* w 0.4))) (round (- cyc (* h 0.25))))
        (trick-card 3 (+ cx (round (* w 0.4))) (round (- cyc (* h 0.25)))))
      (txt (format "%s%s" (aref card-games-bridge-seat-names act)
                   (cond ((eq phase 'auction) " (you)")
                         ((= act 0) " (you)")
                         (t " (dummy -- you play)")))
           pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0)
            (legalp (and (eq phase 'play) (= turn act))))
        (dolist (c ahand)
          (let ((sp (card-games-bridge--spec c)) (curp (= i cursor))
                (hintp (and legalp (card-games-bridge--legal-play-p game act c))))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions (nreverse regions))))

(cl-defmethod card-games-render ((game card-games-bridge-game))
  "Return a depiction of the Bridge GAME: SVG board if graphical, else text."
  (if (and card-games-bridge-svg-cards (display-graphic-p))
      (card-games-bridge--svg game)
    (card-games-bridge--render-text game)))

(defun card-games-bridge--render-text (game)
  "Return a plain-text depiction of the Bridge GAME."
  (let* ((out '()) (phase (card-games-get game :phase)) (cursor (card-games-get game :cursor)))
    (push "  Bridge\n" out)
    (push (format "  Rubber:  You/North games %d  East/West games %d%s\n"
                  (aref (card-games-get game :games) 0) (aref (card-games-get game :games) 1)
                  (let ((v (card-games-get game :vul)))
                    (format "   (vul: %s)"
                            (cond ((and (aref v 0) (aref v 1)) "both")
                                  ((aref v 0) "N-S") ((aref v 1) "E-W") (t "none")))))
          out)
    (push (format "  Below: You/N %d  E/W %d    Above: You/N %d  E/W %d\n\n"
                  (aref (card-games-get game :below) 0) (aref (card-games-get game :below) 1)
                  (aref (card-games-get game :above) 0) (aref (card-games-get game :above) 1))
          out)
    (pcase phase
      ('auction
       (push (format "  Auction so far: %s\n\n" (card-games-bridge--auction-string game)) out)
       (push (format "  Compose:  %d %s   (Up/Down level, Left/Right strain)\n\n"
                     (card-games-get game :bid-level)
                     (aref card-games-bridge-strains (card-games-get game :bid-strain)))
             out))
      ((or 'play 'scored 'passed-out)
       (push (format "  Contract: %s by %s    Declarer tricks: %d\n"
                     (card-games-bridge--contract-string game)
                     (if (card-games-get game :declarer)
                         (aref card-games-bridge-seat-names (card-games-get game :declarer)) "--")
                     (card-games-get game :tricks))
             out)
       (when (and (card-games-get game :exposed) (card-games-get game :dummy))
         (push (format "\n  Dummy (%s):\n   "
                       (aref card-games-bridge-seat-names (card-games-get game :dummy))) out)
         (if (and card-games-bridge-svg-cards (display-graphic-p))
             (push (card-games-bridge--svg-row
                    (card-games-bridge--sort (card-games-bridge--hand game (card-games-get game :dummy)))) out)
           (push (card-games-bridge--hand-by-suit (card-games-bridge--hand game (card-games-get game :dummy))) out)))
       (push "\n  Trick: " out)
       (cond
        ((null (card-games-get game :trick)) (push "(empty)" out))
        ((and card-games-bridge-svg-cards (display-graphic-p))
         (push (concat (mapconcat (lambda (p) (aref card-games-bridge-seat-names (car p)))
                                  (reverse (card-games-get game :trick)) "  ") "   ") out)
         (push (card-games-bridge--svg-row (mapcar #'cdr (reverse (card-games-get game :trick)))) out))
        (t (dolist (p (reverse (card-games-get game :trick)))
             (push (format "%s:%s  " (aref card-games-bridge-seat-names (car p))
                           (card-games-bridge-card-string (cdr p))) out))))
       (push "\n" out)))
    ;; the human's hand (South), or the seat being played from when it is dummy
    (let* ((act (if (and (eq phase 'play) (memq (card-games-get game :turn)
                                                (card-games-bridge--controls game)))
                    (card-games-get game :turn) 0))
           (hand (card-games-bridge--sort (card-games-bridge--hand game act))))
      (push (format "\n  %s%s:\n   "
                    (aref card-games-bridge-seat-names act)
                    (cond ((eq phase 'auction) " (you)")
                          ((= act 0) " (you)")
                          (t " (dummy, you play)")))
            out)
      (cond
       ((and (eq phase 'play) card-games-bridge-svg-cards (display-graphic-p))
        (let ((hi '()) (i 0))
          (dolist (c hand)
            (when (and (= (card-games-get game :turn) act) (card-games-bridge--legal-play-p game act c))
              (push i hi))
            (setq i (1+ i)))
          (push (card-games-bridge--svg-row hand :cursor cursor :hints hi :region-tag 'hand) out)))
       ((eq phase 'play)
        (let ((i 0))
          (dolist (c hand)
            (let ((cs (card-games-bridge-card-string c)) (faces nil))
              (when (card-games-red-suit-p (car c)) (push 'card-games-red-suit faces))
              (when (and (= (card-games-get game :turn) act)
                         (card-games-bridge--legal-play-p game act c)) (push 'card-games-hint faces))
              (when (= i cursor) (push 'card-games-cursor faces))
              (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
            (setq i (1+ i)))))
       (t (push (card-games-bridge--hand-by-suit hand) out))))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-bridge-game) action)
  "Apply click ACTION on G's hand: select that card and play it."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i) (card-games-bridge-play))
    (_ (cl-call-next-method))))

(defun card-games-bridge--redisplay ()
  "Redraw the current Bridge buffer."
  (let ((game card-games-bridge--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-bridge--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

;;;; Auction commands

(defun card-games-bridge-bid-level-up ()
  "Raise the level being composed."
  (interactive)
  (let ((g card-games-bridge--game))
    (card-games-put g :bid-level (min 7 (1+ (card-games-get g :bid-level))))
    (card-games-bridge--redisplay)))

(defun card-games-bridge-bid-level-down ()
  "Lower the level being composed."
  (interactive)
  (let ((g card-games-bridge--game))
    (card-games-put g :bid-level (max 1 (1- (card-games-get g :bid-level))))
    (card-games-bridge--redisplay)))

(defun card-games-bridge-bid-strain-left ()
  "Move the composed strain down (toward clubs)."
  (interactive)
  (let ((g card-games-bridge--game))
    (card-games-put g :bid-strain (max 0 (1- (card-games-get g :bid-strain))))
    (card-games-bridge--redisplay)))

(defun card-games-bridge-bid-strain-right ()
  "Move the composed strain up (toward no-trump)."
  (interactive)
  (let ((g card-games-bridge--game))
    (card-games-put g :bid-strain (min 4 (1+ (card-games-get g :bid-strain))))
    (card-games-bridge--redisplay)))

(defun card-games-bridge--after-call (g)
  "Resolve the end of the auction and run AI after South's call in G."
  (unless (card-games-bridge--auction-done-p g)
    (card-games-bridge--run-auction g))
  (when (eq (card-games-get g :phase) 'play) (card-games-bridge--run-play g))
  (card-games-bridge--redisplay))

(defun card-games-bridge-bid ()
  "Make the composed bid."
  (interactive)
  (let* ((g card-games-bridge--game)
         (call (cons (card-games-get g :bid-level) (card-games-get g :bid-strain))))
    (cond
     ((not (eq (card-games-get g :phase) 'auction)) (card-games-put g :message "Not bidding now."))
     ((/= (card-games-get g :bidder) 0) (card-games-put g :message "Not your turn."))
     ((not (card-games-bridge--legal-call-p g call))
      (card-games-put g :message "That bid is too low."))
     (t (card-games-bridge--apply-call g 0 call) (card-games-bridge--after-call g)))
    (card-games-bridge--redisplay)))

(defun card-games-bridge-pass ()
  "Pass in the auction."
  (interactive)
  (let ((g card-games-bridge--game))
    (if (and (eq (card-games-get g :phase) 'auction) (= (card-games-get g :bidder) 0))
        (progn (card-games-bridge--apply-call g 0 'pass) (card-games-bridge--after-call g))
      (card-games-put g :message "Nothing to pass on."))
    (card-games-bridge--redisplay)))

(defun card-games-bridge-double ()
  "Double (or redouble) in the auction."
  (interactive)
  (let* ((g card-games-bridge--game)
         (call (if (= (card-games-get g :doubled) 1) 'redouble 'double)))
    (if (and (eq (card-games-get g :phase) 'auction) (= (card-games-get g :bidder) 0)
             (card-games-bridge--legal-call-p g call))
        (progn (card-games-bridge--apply-call g 0 call) (card-games-bridge--after-call g))
      (card-games-put g :message "You cannot double now."))
    (card-games-bridge--redisplay)))

;;;; Play commands

(defun card-games-bridge--act-hand (g)
  "Return the hand G's cursor currently indexes (the seat to act)."
  (let ((act (if (memq (card-games-get g :turn) (card-games-bridge--controls g)) (card-games-get g :turn) 0)))
    (card-games-bridge--sort (card-games-bridge--hand g act))))

(defun card-games-bridge-left ()
  "Move the cursor left."
  (interactive)
  (let* ((g card-games-bridge--game) (n (length (card-games-bridge--act-hand g))))
    (cond ((eq (card-games-get g :phase) 'auction) (card-games-bridge-bid-strain-left))
          (t (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
             (card-games-bridge--redisplay)))))

(defun card-games-bridge-right ()
  "Move the cursor right."
  (interactive)
  (let* ((g card-games-bridge--game) (n (length (card-games-bridge--act-hand g))))
    (cond ((eq (card-games-get g :phase) 'auction) (card-games-bridge-bid-strain-right))
          (t (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
             (card-games-bridge--redisplay)))))

(defun card-games-bridge-up ()
  "Raise the bid level (auction only)."
  (interactive)
  (if (eq (card-games-get card-games-bridge--game :phase) 'auction) (card-games-bridge-bid-level-up)
    (card-games-bridge--redisplay)))

(defun card-games-bridge-down ()
  "Lower the bid level (auction only)."
  (interactive)
  (if (eq (card-games-get card-games-bridge--game :phase) 'auction) (card-games-bridge-bid-level-down)
    (card-games-bridge--redisplay)))

(defun card-games-bridge-play ()
  "Play the cursor card, or make the composed bid during the auction."
  (interactive)
  (let ((g card-games-bridge--game))
    (if (eq (card-games-get g :phase) 'auction)
        (card-games-bridge-bid)
      (let* ((turn (card-games-get g :turn)))
        (cond
         ((not (eq (card-games-get g :phase) 'play)) (card-games-put g :message "Press n to continue."))
         ((not (memq turn (card-games-bridge--controls g)))
          (card-games-put g :message "Wait for your turn."))
         (t (let ((card (nth (card-games-get g :cursor) (card-games-bridge--sort (card-games-bridge--hand g turn)))))
              (if (or (null card) (not (card-games-bridge--legal-play-p g turn card)))
                  (card-games-put g :message "You must follow suit.")
                (card-games-bridge--play-card g turn card)
                (card-games-put g :cursor 0)
                (when (eq (card-games-get g :phase) 'play) (card-games-bridge--run-play g))))))
        (card-games-bridge--redisplay)))))

(defun card-games-bridge-new ()
  "Deal the next hand, or a fresh rubber when one is over."
  (interactive)
  (let ((g card-games-bridge--game))
    (when (or (card-games-get g :rubber-winner))
      (card-games-put g :below (make-vector 2 0)) (card-games-put g :above (make-vector 2 0))
      (card-games-put g :games (make-vector 2 0)) (card-games-put g :vul (make-vector 2 nil))
      (card-games-put g :rubber-winner nil))
    (card-games-put g :dealer (mod (1+ (or (card-games-get g :dealer) 0)) 4))
    (card-games-bridge--deal g)
    (card-games-bridge--run-auction g)
    (card-games-bridge--redisplay)))

(defun card-games-bridge-redraw () "Redraw." (interactive) (card-games-bridge--redisplay))
(defun card-games-bridge-help () "Describe the controls." (interactive)
       (message "Auction: Up/Down level, Left/Right strain, RET bid, p pass, d double.  Play: arrows + RET.  n: next"))

(defvar card-games-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-bridge-left)
    (define-key map (kbd "<right>") #'card-games-bridge-right)
    (define-key map (kbd "<up>")    #'card-games-bridge-up)
    (define-key map (kbd "<down>")  #'card-games-bridge-down)
    (define-key map (kbd "RET") #'card-games-bridge-play)
    (define-key map "p" #'card-games-bridge-pass)
    (define-key map "d" #'card-games-bridge-double)
    (define-key map "n" #'card-games-bridge-new)
    (define-key map "g" #'card-games-bridge-redraw)
    (define-key map "?" #'card-games-bridge-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-bridge-mode'.")

(define-derived-mode card-games-bridge-mode special-mode "Bridge"
  "Major mode for contract Bridge."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-bridge ()
  "Play contract Bridge against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Bridge*")))
    (with-current-buffer buf
      (card-games-bridge-mode)
      (setq card-games-bridge--game (card-games-bridge-game))
      (card-games-put card-games-bridge--game :dealer 0)
      (card-games-bridge--deal card-games-bridge--game)
      (card-games-bridge--run-auction card-games-bridge--game)
      (card-games-bridge--redisplay))
    (switch-to-buffer buf)))

(provide 'card-games-bridge)
;;; card-games-bridge.el ends here
