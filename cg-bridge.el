;;; cg-bridge.el --- Contract Bridge with rubber scoring  -*- lexical-binding: t; -*-

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
(require 'cg-core)
(require 'cg-svg)

(defconst cg-bridge-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst cg-bridge-strains ["♣" "♦" "♥" "♠" "NT"]
  "Strain glyphs indexed 0 clubs, 1 diamonds, 2 hearts, 3 spades, 4 no-trump.")

(defconst cg-bridge--strain-suit [1 2 3 0 nil]
  "Map a strain index to its trump suit index (nil for no-trump).")

(defconst cg-bridge--suit-strain [3 0 1 2]
  "Map a suit index (0 S,1 C,2 D,3 H) to its strain index.")

(defconst cg-bridge-seat-names ["South" "West" "North" "East"]
  "Seat names indexed 0..3 clockwise from the human.")

(defclass cg-bridge-game (cg-game)
  ((vname :initform "Bridge"))
  "A game of contract Bridge.")

(defun cg-bridge-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref cg-bridge-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defun cg-bridge--sort (cards)
  "Return CARDS sorted by suit then rank (high first) for display."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (car a) (car b)) (> (cdr a) (cdr b)) (< (car a) (car b))))))

(defun cg-bridge--deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

;;;; Hand evaluation

(defun cg-bridge--hcp (hand)
  "Return the high-card points of HAND (A=4 K=3 Q=2 J=1)."
  (let ((p 0))
    (dolist (c hand p)
      (setq p (+ p (pcase (cdr c) (12 4) (11 3) (10 2) (9 1) (_ 0)))))))

(defun cg-bridge--suit-len (hand suit)
  "Return how many cards of SUIT are in HAND."
  (cl-count suit hand :key #'car))

(defun cg-bridge--longest (hand)
  "Return the suit index HAND holds most of (ties prefer majors, then spades)."
  (let ((best 0) (bestn -1))
    ;; check in order hearts, diamonds, clubs, spades so spades win ties last
    (dolist (s '(3 2 1 0))
      (let ((n (cg-bridge--suit-len hand s)))
        (when (>= n bestn) (setq bestn n best s))))
    best))

(defun cg-bridge--balanced-p (hand)
  "Return non-nil when HAND has a balanced shape (no void/singleton)."
  (let ((doubletons 0) (ok t))
    (dotimes (s 4)
      (let ((n (cg-bridge--suit-len hand s)))
        (when (< n 2) (setq ok nil))
        (when (= n 2) (setq doubletons (1+ doubletons)))))
    (and ok (<= doubletons 1))))

;;;; Auction mechanics

(defsubst cg-bridge--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-bridge--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-bridge--side (s) (mod s 2))

(cl-defmethod cg-bridge--deal ((game cg-bridge-game))
  "Deal a fresh Bridge hand into GAME, leaving it ready for the auction."
  (let ((deck (cg-bridge--deck)) (hands (make-vector 4 nil)))
    (dotimes (s 4)
      (aset hands s (cg-bridge--sort (cl-loop repeat 13 collect (pop deck)))))
    (cg-put game :hands hands)
    (cg-put game :calls nil)            ; list of (SEAT . CALL), newest first
    (cg-put game :contract nil)         ; (LEVEL . STRAIN)
    (cg-put game :declarer nil)
    (cg-put game :doubled 0)
    (cg-put game :dealer (or (cg-get game :dealer) 0))
    (cg-put game :bidder (cg-get game :dealer))
    (cg-put game :phase 'auction)
    (cg-put game :cursor 0)
    (cg-put game :bid-level 1)          ; UI: level being composed
    (cg-put game :bid-strain 0)
    (cg-put game :trick nil)
    (cg-put game :tricks 0)             ; declarer-side tricks won
    (cg-put game :dummy nil)
    (cg-put game :exposed nil)
    (unless (cg-get game :below) (cg-put game :below (make-vector 2 0)))
    (unless (cg-get game :above) (cg-put game :above (make-vector 2 0)))
    (unless (cg-get game :games) (cg-put game :games (make-vector 2 0)))
    (unless (cg-get game :vul) (cg-put game :vul (make-vector 2 nil)))
    (cg-put game :message "Auction: compose a bid and press RET, or p/d to pass/double.")
    game))

(defun cg-bridge--high-bid (game)
  "Return the highest (LEVEL . STRAIN) bid so far, or nil."
  (cl-loop for (_s . call) in (cg-get game :calls)
           when (consp call) return call))

(defun cg-bridge--high-bidder (game)
  "Return the seat that made the highest bid, or nil."
  (cl-loop for (s . call) in (cg-get game :calls)
           when (consp call) return s))

(defun cg-bridge--call> (a b)
  "Return non-nil when bid A is higher than bid B (each (LEVEL . STRAIN))."
  (or (null b)
      (> (car a) (car b))
      (and (= (car a) (car b)) (> (cdr a) (cdr b)))))

(defun cg-bridge--legal-call-p (game call)
  "Return non-nil when CALL is legal now in GAME."
  (let ((high (cg-bridge--high-bid game))
        (hb (cg-bridge--high-bidder game)))
    (pcase call
      ('pass t)
      ('double (and high (/= (cg-bridge--side hb) (cg-bridge--side (cg-get game :bidder)))
                    (= (cg-get game :doubled) 0)))
      ('redouble (and high (= (cg-bridge--side hb) (cg-bridge--side (cg-get game :bidder)))
                      (= (cg-get game :doubled) 1)))
      (_ (and (consp call) (>= (car call) 1) (<= (car call) 7)
              (cg-bridge--call> call high))))))

(defun cg-bridge--apply-call (game seat call)
  "Record CALL by SEAT and update doubling state."
  (cg-put game :calls (cons (cons seat call) (cg-get game :calls)))
  (pcase call
    ('double (cg-put game :doubled 1))
    ('redouble (cg-put game :doubled 2))
    ((pred consp) (cg-put game :doubled 0)))
  (cg-put game :bidder (mod (1+ seat) 4)))

(defun cg-bridge--auction-done-p (game)
  "Return non-nil when the auction has ended.
Sets up the contract (or a pass-out) as a side effect."
  (let* ((calls (cg-get game :calls)) (n (length calls)))
    (cond
     ;; four passes with no bid: passed out
     ((and (= n 4) (cl-every (lambda (c) (eq (cdr c) 'pass)) calls))
      (cg-put game :phase 'passed-out) t)
     ;; a bid then three passes
     ((and (cg-bridge--high-bid game)
           (>= n 3)
           (cl-every (lambda (c) (eq (cdr c) 'pass))
                     (cl-subseq calls 0 3)))
      (cg-bridge--establish-contract game) t)
     (t nil))))

(defun cg-bridge--establish-contract (game)
  "Set the contract, declarer, and start of play from the finished auction."
  (let* ((bid (cg-bridge--high-bid game))
         (side (cg-bridge--side (cg-bridge--high-bidder game)))
         (strain (cdr bid))
         (declarer
          ;; first player of SIDE to have named STRAIN
          (cl-loop for (s . call) in (reverse (cg-get game :calls))
                   when (and (consp call) (= (cdr call) strain)
                             (= (cg-bridge--side s) side))
                   return s)))
    (cg-put game :contract bid)
    (cg-put game :declarer declarer)
    (cg-put game :dummy (mod (+ declarer 2) 4))
    (cg-put game :phase 'play)
    (cg-put game :leader (mod (1+ declarer) 4))
    (cg-put game :turn (mod (1+ declarer) 4))
    (cg-put game :trick nil)
    (cg-put game :tricks 0)
    (cg-put game :cursor 0)
    (cg-put game :message
            (format "Contract: %s by %s.  %s leads."
                    (cg-bridge--contract-string game)
                    (aref cg-bridge-seat-names declarer)
                    (aref cg-bridge-seat-names (cg-get game :leader))))))

(defun cg-bridge--contract-string (game)
  "Return a label for GAME's contract, e.g. \"4NT x\"."
  (let ((c (cg-get game :contract)) (d (cg-get game :doubled)))
    (if (null c) "passed out"
      (format "%d%s%s" (car c) (aref cg-bridge-strains (cdr c))
              (pcase d (1 " x") (2 " xx") (_ ""))))))

;;;; Play mechanics

(defun cg-bridge--trump (game)
  "Return the trump suit index for GAME, or nil for no-trump."
  (and (cg-get game :contract) (aref cg-bridge--strain-suit (cdr (cg-get game :contract)))))

(defun cg-bridge--led-suit (game)
  "Return the suit led to the current trick, or nil."
  (let ((tr (cg-get game :trick)))
    (and tr (car (cdr (car (last tr)))))))

(defun cg-bridge--legal-play-p (game seat card)
  "Return non-nil when SEAT may play CARD now (follow suit if able)."
  (let ((hand (cg-bridge--hand game seat)) (led (cg-bridge--led-suit game)))
    (and (member card hand)
         (or (null led)
             (= (car card) led)
             (not (cl-some (lambda (c) (= (car c) led)) hand))))))

(defun cg-bridge--legal-plays (game seat)
  "Return SEAT's legal cards now."
  (cl-remove-if-not (lambda (c) (cg-bridge--legal-play-p game seat c))
                    (cg-bridge--hand game seat)))

(defun cg-bridge--trick-winner (plays trump)
  "Return the winning seat of complete PLAYS ((SEAT . CARD), play order)."
  (let ((best (car plays)))
    (dolist (p (cdr plays))
      (let ((bc (cdr best)) (pc (cdr p)))
        (cond
         ((and trump (= (car pc) trump) (/= (car bc) trump)) (setq best p))
         ((and (= (car pc) (car bc)) (> (cdr pc) (cdr bc))) (setq best p)))))
    (car best)))

(defun cg-bridge--play-card (game seat card)
  "Have SEAT play CARD; resolve and score the trick when it completes."
  (cg-bridge--set-hand game seat (remove card (cg-bridge--hand game seat)))
  (cg-put game :trick (cons (cons seat card) (cg-get game :trick)))
  ;; expose the dummy after the opening lead
  (unless (cg-get game :exposed)
    (cg-put game :exposed t))
  (if (= 4 (length (cg-get game :trick)))
      (let ((w (cg-bridge--trick-winner (reverse (cg-get game :trick))
                                        (cg-bridge--trump game))))
        (when (= (cg-bridge--side w) (cg-bridge--side (cg-get game :declarer)))
          (cg-put game :tricks (1+ (cg-get game :tricks))))
        (cg-put game :trick nil)
        (cg-put game :leader w)
        (cg-put game :turn w)
        (cg-put game :last-winner w)
        (when (cl-every #'null (append (cg-get game :hands) nil))
          (cg-bridge--score-deal game))
        w)
    (cg-put game :turn (mod (1+ seat) 4))
    nil))

;;;; Scoring (rubber)

(defun cg-bridge--undertrick-points (n doubled vul)
  "Return defender points for N undertricks at DOUBLED level and VUL state."
  (cond
   ((= doubled 0) (* n (if vul 100 50)))
   (t (let ((mult (if (= doubled 2) 2 1)) (sum 0))
        (dotimes (i n)
          (setq sum (+ sum (* mult (if vul (if (= i 0) 200 300)
                                     (cond ((= i 0) 100) ((< i 3) 200) (t 300)))))))
        sum))))

(defun cg-bridge--deal-score (level strain doubled vul tricks)
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
      (setq defend (cg-bridge--undertrick-points (- result) doubled vul)))
    (list :below below :datk datk :defend defend :result result)))

(defun cg-bridge--score-deal (game)
  "Score the finished deal into GAME's rubber state."
  (let* ((c (cg-get game :contract)) (level (car c)) (strain (cdr c))
         (decl (cg-get game :declarer)) (side (cg-bridge--side decl))
         (opp (- 1 side)) (doubled (cg-get game :doubled))
         (vul (aref (cg-get game :vul) side))
         (sc (cg-bridge--deal-score level strain doubled vul (cg-get game :tricks)))
         (below (cg-get game :below)) (above (cg-get game :above)))
    (aset below side (+ (aref below side) (plist-get sc :below)))
    (aset above side (+ (aref above side) (plist-get sc :datk)))
    (aset above opp (+ (aref above opp) (plist-get sc :defend)))
    (cg-put game :deal-result sc)
    ;; game / rubber bookkeeping
    (when (>= (aref below side) 100)
      (let ((games (cg-get game :games)))
        (aset games side (1+ (aref games side)))
        (aset (cg-get game :vul) side t)
        (aset below 0 0) (aset below 1 0)
        (when (>= (aref games side) 2)
          (aset above side (+ (aref above side)
                              (if (>= (aref games opp) 1) 500 700)))
          (cg-put game :rubber-winner side))))
    (cg-put game :phase 'scored)
    (cg-put game :message
            (format "%s: %s.  %s"
                    (cg-bridge--contract-string game)
                    (let ((r (plist-get sc :result)))
                      (cond ((>= r 0) (format "made +%d" r))
                            (t (format "down %d" (- r)))))
                    (if (cg-get game :rubber-winner)
                        (format "%s win the rubber!  (n: new rubber)"
                                (if (= side 0) "You and North" "East and West"))
                      "(n: next deal)")))))

;;;; AI -- bidding

(cl-defmethod cg-bridge--ai-call ((game cg-bridge-game) seat)
  "Return a call for AI SEAT from a small natural system."
  (let* ((hand (cg-bridge--hand game seat)) (hcp (cg-bridge--hcp hand))
         (high (cg-bridge--high-bid game)) (hb (cg-bridge--high-bidder game))
         (ours (and high (= (cg-bridge--side hb) (cg-bridge--side seat)))))
    (cond
     ((null high)                       ; opening
      (cond ((and (cg-bridge--balanced-p hand) (>= hcp 15) (<= hcp 17)) (cons 1 4))
            ((and (cg-bridge--balanced-p hand) (>= hcp 20) (<= hcp 21)) (cons 2 4))
            ((>= hcp 12)
             (let ((suit (cg-bridge--longest hand)))
               (cons 1 (aref cg-bridge--suit-strain suit))))
            (t 'pass)))
     (ours                              ; partner has the contract
      (let* ((est (+ hcp 13)) (hl (car high)) (hs (cdr high))
             (fit (or (= hs 4)
                      (>= (cg-bridge--suit-len
                           hand (aref cg-bridge--strain-suit hs)) 3))))
        (if (and fit (>= est 26) (< hl 4)
                 (cg-bridge--legal-call-p
                  game (cond ((= hs 4) (cons 3 4))
                             ((>= hs 2) (cons 4 hs))
                             (t (cons 5 hs)))))
            (cond ((= hs 4) (cons 3 4)) ((>= hs 2) (cons 4 hs)) (t (cons 5 hs)))
          'pass)))
     (t                                 ; opponents have the contract
      (let* ((suit (cg-bridge--longest hand))
             (len (cg-bridge--suit-len hand suit))
             (st (aref cg-bridge--suit-strain suit))
             (cand (if (> st (cdr high)) (cons (car high) st)
                     (cons (1+ (car high)) st))))
        (if (and (>= hcp 11) (>= len 5) (<= (car cand) 3)
                 (cg-bridge--legal-call-p game cand))
            cand 'pass))))))

;;;; AI -- play

(cl-defmethod cg-bridge--ai-play ((game cg-bridge-game) seat)
  "Return a card for AI SEAT: win cheaply or shed low."
  (let* ((legal (cg-bridge--legal-plays game seat))
         (trump (cg-bridge--trump game)) (trick (cg-get game :trick)))
    (if (null trick)
        ;; leading: low from the longest non-trump suit, else lowest
        (car (sort (copy-sequence legal)
                   (lambda (a b) (< (cdr a) (cdr b)))))
      (let* ((order (reverse trick))
             (cur (cg-bridge--trick-winner order trump))
             (partner (= (cg-bridge--side cur) (cg-bridge--side seat)))
             (winners (cl-remove-if-not
                       (lambda (c) (= seat (cg-bridge--trick-winner
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

(defun cg-bridge--controls (game)
  "Return the list of seats the human controls during play."
  (let ((decl (cg-get game :declarer)))
    (cond ((null decl) nil)
          ((= decl 0) '(0 2))           ; South declares: play hand + dummy
          ((= decl 2) nil)              ; North declares: AI plays both
          (t '(0)))))                   ; South defends

(defun cg-bridge--auto-seat-p (game seat)
  "Return non-nil when SEAT is played automatically (by AI) in GAME."
  (not (memq seat (cg-bridge--controls game))))

(defun cg-bridge--run-play (game)
  "Advance AI plays until a human-controlled seat must act or the deal ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play)
                (cg-bridge--auto-seat-p game (cg-get game :turn))
                (< guard 60))
      (setq guard (1+ guard))
      (cg-bridge--play-card game (cg-get game :turn)
                            (cg-bridge--ai-play game (cg-get game :turn))))))

(defun cg-bridge--run-auction (game)
  "Advance the auction through AI seats until South must call or it ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'auction) (/= (cg-get game :bidder) 0)
                (< guard 40))
      (setq guard (1+ guard))
      (let* ((s (cg-get game :bidder)) (call (cg-bridge--ai-call game s)))
        (unless (cg-bridge--legal-call-p game call) (setq call 'pass))
        (cg-bridge--apply-call game s call)
        (cg-bridge--auction-done-p game)))
    (when (eq (cg-get game :phase) 'play) (cg-bridge--run-play game))))

;;;; UI

(defvar-local cg-bridge--game nil "The Bridge game in the current buffer.")

(defun cg-bridge--hand-by-suit (cards)
  "Return CARDS grouped into four lines by suit, as a string."
  (let ((out '()))
    (dolist (s '(0 3 2 1))              ; S H D C
      (let ((in (cg-bridge--sort (cl-remove-if-not (lambda (c) (= (car c) s)) cards))))
        (push (format "    %s %s\n" (cg-suit-glyph s)
                      (if in (mapconcat (lambda (c) (aref cg-bridge-ranks (cdr c))) in " ")
                        "--"))
              out)))
    (apply #'concat (nreverse out))))

(defun cg-bridge--auction-string (game)
  "Return a compact record of the auction so far."
  (let ((calls (reverse (cg-get game :calls))) (out '()))
    (dolist (sc calls)
      (push (format "%s:%s" (aref cg-bridge-seat-names (car sc))
                    (pcase (cdr sc)
                      ('pass "pass") ('double "X") ('redouble "XX")
                      (c (format "%d%s" (car c) (aref cg-bridge-strains (cdr c))))))
            out))
    (if out (mapconcat #'identity (nreverse out) "  ") "(no calls yet)")))

(defcustom cg-bridge-svg-cards t
  "When non-nil, draw cards as SVG images on a graphical display."
  :type 'boolean :group 'card-games)

(defun cg-bridge--spec (card)
  "Return the cg-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref cg-bridge-ranks (cdr card)) (car card)))

(cl-defun cg-bridge--svg-row (cards &key cursor hints)
  "Return a one-image SVG row for CARDS with CURSOR and HINTS indices."
  (propertize "*" 'display
              (cg-svg-image
               (cg-svg-hand-svg (mapcar #'cg-bridge--spec cards)
                                :cursor cursor :hints hints
                                :overlap (if (> (length cards) 11)
                                             (max 0 (- cg-svg-card-width 26)) 0))
               (cg-scale))))

(cl-defmethod cg-render ((game cg-bridge-game))
  "Return a propertized depiction of the Bridge GAME."
  (let* ((out '()) (phase (cg-get game :phase)) (cursor (cg-get game :cursor)))
    (push "  Bridge\n" out)
    (push (format "  Rubber:  You/North games %d  East/West games %d%s\n"
                  (aref (cg-get game :games) 0) (aref (cg-get game :games) 1)
                  (let ((v (cg-get game :vul)))
                    (format "   (vul: %s)"
                            (cond ((and (aref v 0) (aref v 1)) "both")
                                  ((aref v 0) "N-S") ((aref v 1) "E-W") (t "none")))))
          out)
    (push (format "  Below: You/N %d  E/W %d    Above: You/N %d  E/W %d\n\n"
                  (aref (cg-get game :below) 0) (aref (cg-get game :below) 1)
                  (aref (cg-get game :above) 0) (aref (cg-get game :above) 1))
          out)
    (pcase phase
      ('auction
       (push (format "  Auction so far: %s\n\n" (cg-bridge--auction-string game)) out)
       (push (format "  Compose:  %d %s   (Up/Down level, Left/Right strain)\n\n"
                     (cg-get game :bid-level)
                     (aref cg-bridge-strains (cg-get game :bid-strain)))
             out))
      ((or 'play 'scored 'passed-out)
       (push (format "  Contract: %s by %s    Declarer tricks: %d\n"
                     (cg-bridge--contract-string game)
                     (if (cg-get game :declarer)
                         (aref cg-bridge-seat-names (cg-get game :declarer)) "--")
                     (cg-get game :tricks))
             out)
       (when (and (cg-get game :exposed) (cg-get game :dummy))
         (push (format "\n  Dummy (%s):\n   "
                       (aref cg-bridge-seat-names (cg-get game :dummy))) out)
         (if (and cg-bridge-svg-cards (display-graphic-p))
             (push (cg-bridge--svg-row
                    (cg-bridge--sort (cg-bridge--hand game (cg-get game :dummy)))) out)
           (push (cg-bridge--hand-by-suit (cg-bridge--hand game (cg-get game :dummy))) out)))
       (push "\n  Trick: " out)
       (cond
        ((null (cg-get game :trick)) (push "(empty)" out))
        ((and cg-bridge-svg-cards (display-graphic-p))
         (push (concat (mapconcat (lambda (p) (aref cg-bridge-seat-names (car p)))
                                  (reverse (cg-get game :trick)) "  ") "   ") out)
         (push (cg-bridge--svg-row (mapcar #'cdr (reverse (cg-get game :trick)))) out))
        (t (dolist (p (reverse (cg-get game :trick)))
             (push (format "%s:%s  " (aref cg-bridge-seat-names (car p))
                           (cg-bridge-card-string (cdr p))) out))))
       (push "\n" out)))
    ;; the human's hand (South), or the seat being played from when it is dummy
    (let* ((act (if (and (eq phase 'play) (memq (cg-get game :turn)
                                                (cg-bridge--controls game)))
                    (cg-get game :turn) 0))
           (hand (cg-bridge--sort (cg-bridge--hand game act))))
      (push (format "\n  %s%s:\n   "
                    (aref cg-bridge-seat-names act)
                    (cond ((eq phase 'auction) " (you)")
                          ((= act 0) " (you)")
                          (t " (dummy, you play)")))
            out)
      (cond
       ((and (eq phase 'play) cg-bridge-svg-cards (display-graphic-p))
        (let ((hi '()) (i 0))
          (dolist (c hand)
            (when (and (= (cg-get game :turn) act) (cg-bridge--legal-play-p game act c))
              (push i hi))
            (setq i (1+ i)))
          (push (cg-bridge--svg-row hand :cursor cursor :hints hi) out)))
       ((eq phase 'play)
        (let ((i 0))
          (dolist (c hand)
            (let ((cs (cg-bridge-card-string c)) (faces nil))
              (when (cg-red-suit-p (car c)) (push 'cg-red-suit faces))
              (when (and (= (cg-get game :turn) act)
                         (cg-bridge--legal-play-p game act c)) (push 'cg-hint faces))
              (when (= i cursor) (push 'cg-cursor faces))
              (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
            (setq i (1+ i)))))
       (t (push (cg-bridge--hand-by-suit hand) out))))
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-bridge--redisplay ()
  (let ((game cg-bridge--game) (inhibit-read-only t))
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

;;;; Auction commands

(defun cg-bridge-bid-level-up ()
  "Raise the level being composed."
  (interactive)
  (let ((g cg-bridge--game))
    (cg-put g :bid-level (min 7 (1+ (cg-get g :bid-level))))
    (cg-bridge--redisplay)))

(defun cg-bridge-bid-level-down ()
  "Lower the level being composed."
  (interactive)
  (let ((g cg-bridge--game))
    (cg-put g :bid-level (max 1 (1- (cg-get g :bid-level))))
    (cg-bridge--redisplay)))

(defun cg-bridge-bid-strain-left ()
  "Move the composed strain down (toward clubs)."
  (interactive)
  (let ((g cg-bridge--game))
    (cg-put g :bid-strain (max 0 (1- (cg-get g :bid-strain))))
    (cg-bridge--redisplay)))

(defun cg-bridge-bid-strain-right ()
  "Move the composed strain up (toward no-trump)."
  (interactive)
  (let ((g cg-bridge--game))
    (cg-put g :bid-strain (min 4 (1+ (cg-get g :bid-strain))))
    (cg-bridge--redisplay)))

(defun cg-bridge--after-call (g)
  "Resolve end-of-auction and run AI after South calls in G."
  (unless (cg-bridge--auction-done-p g)
    (cg-bridge--run-auction g))
  (when (eq (cg-get g :phase) 'play) (cg-bridge--run-play g))
  (cg-bridge--redisplay))

(defun cg-bridge-bid ()
  "Make the composed bid."
  (interactive)
  (let* ((g cg-bridge--game)
         (call (cons (cg-get g :bid-level) (cg-get g :bid-strain))))
    (cond
     ((not (eq (cg-get g :phase) 'auction)) (cg-put g :message "Not bidding now."))
     ((/= (cg-get g :bidder) 0) (cg-put g :message "Not your turn."))
     ((not (cg-bridge--legal-call-p g call))
      (cg-put g :message "That bid is too low."))
     (t (cg-bridge--apply-call g 0 call) (cg-bridge--after-call g)))
    (cg-bridge--redisplay)))

(defun cg-bridge-pass ()
  "Pass in the auction."
  (interactive)
  (let ((g cg-bridge--game))
    (if (and (eq (cg-get g :phase) 'auction) (= (cg-get g :bidder) 0))
        (progn (cg-bridge--apply-call g 0 'pass) (cg-bridge--after-call g))
      (cg-put g :message "Nothing to pass on."))
    (cg-bridge--redisplay)))

(defun cg-bridge-double ()
  "Double (or redouble) in the auction."
  (interactive)
  (let* ((g cg-bridge--game)
         (call (if (= (cg-get g :doubled) 1) 'redouble 'double)))
    (if (and (eq (cg-get g :phase) 'auction) (= (cg-get g :bidder) 0)
             (cg-bridge--legal-call-p g call))
        (progn (cg-bridge--apply-call g 0 call) (cg-bridge--after-call g))
      (cg-put g :message "You cannot double now."))
    (cg-bridge--redisplay)))

;;;; Play commands

(defun cg-bridge--act-hand (g)
  "Return the hand the cursor currently indexes (the seat to act)."
  (let ((act (if (memq (cg-get g :turn) (cg-bridge--controls g)) (cg-get g :turn) 0)))
    (cg-bridge--sort (cg-bridge--hand g act))))

(defun cg-bridge-left ()
  "Move the cursor left."
  (interactive)
  (let* ((g cg-bridge--game) (n (length (cg-bridge--act-hand g))))
    (cond ((eq (cg-get g :phase) 'auction) (cg-bridge-bid-strain-left))
          (t (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
             (cg-bridge--redisplay)))))

(defun cg-bridge-right ()
  "Move the cursor right."
  (interactive)
  (let* ((g cg-bridge--game) (n (length (cg-bridge--act-hand g))))
    (cond ((eq (cg-get g :phase) 'auction) (cg-bridge-bid-strain-right))
          (t (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
             (cg-bridge--redisplay)))))

(defun cg-bridge-up ()
  "Raise the bid level (auction only)."
  (interactive)
  (if (eq (cg-get cg-bridge--game :phase) 'auction) (cg-bridge-bid-level-up)
    (cg-bridge--redisplay)))

(defun cg-bridge-down ()
  "Lower the bid level (auction only)."
  (interactive)
  (if (eq (cg-get cg-bridge--game :phase) 'auction) (cg-bridge-bid-level-down)
    (cg-bridge--redisplay)))

(defun cg-bridge-play ()
  "Play the cursor card, or make the composed bid during the auction."
  (interactive)
  (let ((g cg-bridge--game))
    (if (eq (cg-get g :phase) 'auction)
        (cg-bridge-bid)
      (let* ((turn (cg-get g :turn)))
        (cond
         ((not (eq (cg-get g :phase) 'play)) (cg-put g :message "Press n to continue."))
         ((not (memq turn (cg-bridge--controls g)))
          (cg-put g :message "Wait for your turn."))
         (t (let ((card (nth (cg-get g :cursor) (cg-bridge--sort (cg-bridge--hand g turn)))))
              (if (or (null card) (not (cg-bridge--legal-play-p g turn card)))
                  (cg-put g :message "You must follow suit.")
                (cg-bridge--play-card g turn card)
                (cg-put g :cursor 0)
                (when (eq (cg-get g :phase) 'play) (cg-bridge--run-play g))))))
        (cg-bridge--redisplay)))))

(defun cg-bridge-new ()
  "Deal the next hand, or a fresh rubber when one is over."
  (interactive)
  (let ((g cg-bridge--game))
    (when (or (cg-get g :rubber-winner))
      (cg-put g :below (make-vector 2 0)) (cg-put g :above (make-vector 2 0))
      (cg-put g :games (make-vector 2 0)) (cg-put g :vul (make-vector 2 nil))
      (cg-put g :rubber-winner nil))
    (cg-put g :dealer (mod (1+ (or (cg-get g :dealer) 0)) 4))
    (cg-bridge--deal g)
    (cg-bridge--run-auction g)
    (cg-bridge--redisplay)))

(defun cg-bridge-redraw () "Redraw." (interactive) (cg-bridge--redisplay))
(defun cg-bridge-help () "Describe the controls." (interactive)
       (message "Auction: Up/Down level, Left/Right strain, RET bid, p pass, d double.  Play: arrows + RET.  n: next"))

(defvar cg-bridge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-bridge-left)
    (define-key map (kbd "<right>") #'cg-bridge-right)
    (define-key map (kbd "<up>")    #'cg-bridge-up)
    (define-key map (kbd "<down>")  #'cg-bridge-down)
    (define-key map (kbd "RET") #'cg-bridge-play)
    (define-key map "p" #'cg-bridge-pass)
    (define-key map "d" #'cg-bridge-double)
    (define-key map "n" #'cg-bridge-new)
    (define-key map "g" #'cg-bridge-redraw)
    (define-key map "?" #'cg-bridge-help)
    map)
  "Keymap for `cg-bridge-mode'.")

(define-derived-mode cg-bridge-mode special-mode "Bridge"
  "Major mode for contract Bridge."
  (setq-local truncate-lines t))

;;;###autoload
(defun cg-bridge ()
  "Play contract Bridge against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Bridge*")))
    (with-current-buffer buf
      (cg-bridge-mode)
      (setq cg-bridge--game (cg-bridge-game))
      (cg-put cg-bridge--game :dealer 0)
      (cg-bridge--deal cg-bridge--game)
      (cg-bridge--run-auction cg-bridge--game)
      (cg-bridge--redisplay))
    (switch-to-buffer buf)))

(provide 'cg-bridge)
;;; cg-bridge.el ends here
