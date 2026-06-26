;;; cg-bid.el --- 500 (Bid) — game logic  -*- lexical-binding: t; -*-

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

;; 500 ("Bid"): the four-handed partnership trick-taking game.  This
;; file holds the rules engine (deck, auction, kitty, trick play,
;; scoring, the full game to 500, and the basic AI).  The console UI and
;; the `cg-bid' command live in cg-bid-ui.el.
;;
;; You sit South (seat 0); partner North (2); West (1) and East (3)
;; oppose.  A side wins only by reaching 500 on a contract it made (the
;; "front door"); a side that sinks to -500 loses ("back door").
;;
;; Deck (Corwin's 45-card variant): Four..Ace in all four suits plus a
;; single Joker; ten cards each and a five-card kitty.  With a trump
;; suit the order is Joker, right bower (jack of trumps), left bower
;; (other jack of the trump colour), then A K Q 10 9 8 7 6 5 4.  In
;; no-trumps the Joker is the only trump and is highest.  Misère/Nullo
;; is "own hand": the contractor's partner sits out and the contractor
;; tries to take no tricks; Open Nullo exposes the hand after trick one.

;;; Code:

(require 'cl-lib)
(require 'cg-core)


;;;; Cards specific to 500

(defconst cg-bid-ranks
  ["4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0..10 (Four through Ace).  Index 7 is the Jack.")

(defconst cg-bid-jack 7  "Rank index of the Jack.")
(defconst cg-bid-joker '(4 . 0) "Canonical Joker card; suit index 4.")

(defsubst cg-bid-joker-p (card)
  "Return non-nil when CARD is the Joker."
  (and (consp card) (= (car card) 4)))

(defun cg-bid-card-string (card)
  "Return a short label for CARD."
  (cond
   ((null card) "--")
   ((cg-bid-joker-p card) "Jk")
   (t (concat (aref cg-bid-ranks (cdr card)) (cg-suit-glyph (car card))))))

(defun cg-bid--full-deck ()
  "Return the 45-card deck as a list of cards."
  (cons (cons 4 0)
        (cl-loop for s below 4
                 append (cl-loop for r below 11 collect (cons s r)))))


;;;; The Avondale-style bid schedule

(defconst cg-bid-schedule
  ;; (LABEL NAME VALUE TRICKS TRUMP OPEN)
  ;; TRUMP: 0-3 suit, nt, or nullo.
  '(("6♠"  "Six Spades"        40  6 0)
    ("6♣"  "Six Clubs"         60  6 1)
    ("6♦"  "Six Diamonds"      80  6 2)
    ("6♥"  "Six Hearts"       100  6 3)
    ("6NT" "Six No Trump"     120  6 nt)
    ("7♠"  "Seven Spades"     140  7 0)
    ("7♣"  "Seven Clubs"      160  7 1)
    ("7♦"  "Seven Diamonds"   180  7 2)
    ("7♥"  "Seven Hearts"     200  7 3)
    ("7NT" "Seven No Trump"   220  7 nt)
    ("8♠"  "Eight Spades"     240  8 0)
    ("NL"  "Nullo"            250  0 nullo)
    ("8♣"  "Eight Clubs"      260  8 1)
    ("8♦"  "Eight Diamonds"   280  8 2)
    ("8♥"  "Eight Hearts"     300  8 3)
    ("8NT" "Eight No Trump"   320  8 nt)
    ("9♠"  "Nine Spades"      340  9 0)
    ("ON"  "Open Nullo"       350  0 nullo t)
    ("9♣"  "Nine Clubs"       360  9 1)
    ("9♦"  "Nine Diamonds"    380  9 2)
    ("9♥"  "Nine Hearts"      400  9 3)
    ("9NT" "Nine No Trump"    420  9 nt)
    ("10♠" "Ten Spades"       440 10 0)
    ("GN"  "Grand Nullo"      450  0 nullo t)
    ("10♣" "Ten Clubs"        460 10 1)
    ("10♦" "Ten Diamonds"     480 10 2)
    ("10♥" "Ten Hearts"       500 10 3)
    ("10NT" "Ten No Trump"    520 10 nt))
  "Bidding schedule, ascending by value.
Each entry is (LABEL NAME VALUE TRICKS TRUMP [OPEN]).")

(defsubst cg-bid-label  (bid) (nth 0 bid))
(defsubst cg-bid-name   (bid) (nth 1 bid))
(defsubst cg-bid-value  (bid) (nth 2 bid))
(defsubst cg-bid-tricks (bid) (nth 3 bid))
(defsubst cg-bid-trump  (bid) (nth 4 bid))
(defsubst cg-bid-open-p (bid) (nth 5 bid))
(defsubst cg-bid-nullo-p (bid) (eq (cg-bid-trump bid) 'nullo))


;;;; Card power and trick logic

(defun cg-bid-effective-suit (card trump)
  "Return the suit CARD belongs to for following, given TRUMP.
TRUMP is a suit index 0-3, or the symbol `nt' or `nullo'.
The left bower counts as the trump suit; the Joker counts as
trump (or as its own suit `joker' when there is no trump suit)."
  (cond
   ((cg-bid-joker-p card) (if (memq trump '(nt nullo)) 'joker trump))
   ((and (numberp trump)
         (= (cdr card) cg-bid-jack)
         (= (car card) (cg-sister-suit trump)))
    trump)
   (t (car card))))

(defun cg-bid-power (card trump led)
  "Return an integer strength for CARD given TRUMP and the LED suit.
Higher wins.  Cards that are neither trump nor of the led suit score
below 100 and so can never win a trick."
  (let ((es (cg-bid-effective-suit card trump)))
    (cond
     ((cg-bid-joker-p card) 1000)
     ((and (numberp trump) (eq es trump))
      (cond
       ((and (= (cdr card) cg-bid-jack) (= (car card) trump)) 900) ; right bower
       ((= (cdr card) cg-bid-jack) 899)                            ; left bower
       (t (+ 800 (cdr card)))))
     ((eq es led) (+ 100 (cdr card)))
     (t (cdr card)))))

(defun cg-bid-trick-winner (plays trump led)
  "Return the seat that wins a trick.
PLAYS is a list of (SEAT . CARD); TRUMP and LED as in `cg-bid-power'."
  (car (cl-reduce
        (lambda (best p)
          (if (> (cg-bid-power (cdr p) trump led)
                 (cg-bid-power (cdr best) trump led))
              p best))
        plays)))

(defun cg-bid-legal-cards (hand led trump)
  "Return the legal subset of HAND given the LED suit and TRUMP.
When LED is nil (leading) every card is legal.  Otherwise a player
must follow the led suit if able."
  (if (null led)
      hand
    (let ((follow (cl-remove-if-not
                   (lambda (c) (eq (cg-bid-effective-suit c trump) led))
                   hand)))
      (or follow hand))))

(defun cg-bid-sort-hand (hand trump)
  "Return HAND sorted for display: trumps first (by power), then by suit."
  (sort (copy-sequence hand)
        (lambda (a b)
          (let* (( at (and (numberp trump)
                           (eq (cg-bid-effective-suit a trump) trump)))
                 (bt (and (numberp trump)
                          (eq (cg-bid-effective-suit b trump) trump)))
                 (aj (cg-bid-joker-p a))
                 (bj (cg-bid-joker-p b)))
            (cond
             ((or aj bt) (and (not bj) (or aj bt) t))
             (t
              (let ((ak (if (or aj at) -1 (car a)))
                    (bk (if (or bj bt) -1 (car b))))
                (if (/= ak bk) (< ak bk)
                  (> (cg-bid-power a (or trump 'nt) nil)
                     (cg-bid-power b (or trump 'nt) nil))))))))))


(defun cg-bid--display-key (card trump)
  "Return an ascending sort key for CARD to group a hand for display.
Trumps (and the Joker) sort first, strongest first; the remaining
suits are grouped spades, hearts, clubs, diamonds, high rank first."
  (cond
   ((cg-bid-joker-p card) 0)
   ((and (numberp trump) (eq (cg-bid-effective-suit card trump) trump))
    (- 1000 (cg-bid-power card trump trump)))
   (t (let ((si (cl-position (car card) [0 3 1 2])))
        (+ 2000 (* (or si 0) 100) (- 12 (cdr card)))))))

(defun cg-bid-sort-display (hand trump)
  "Return HAND sorted for display under TRUMP.
Trumps lead (strongest first), then each side suit runs high to low."
  (sort (copy-sequence hand)
        (lambda (a b) (< (cg-bid--display-key a trump)
                         (cg-bid--display-key b trump)))))


;;;; Game object and dealing

(defclass cg-bid-game (cg-game)
  ((name :initform "500 Bid"))
  "The partnership trick-taking game 500.")

(defvar cg-bid--human-seats '(0)
  "List of seats controlled by a human player.  South is seat 0.")

(defconst cg-bid-seat-names ["South" "West" "North" "East"]
  "Seat labels; partners sit opposite (0/2 and 1/3).")

(defsubst cg-bid--human-p (seat)
  "Return non-nil when SEAT is played by a human."
  (memq seat cg-bid--human-seats))

(defsubst cg-bid--partner (seat) (mod (+ seat 2) 4))
(defsubst cg-bid--team (seat) (mod seat 2)) ; 0 -> team 0 (S/N), 1 -> team 1 (W/E)

(cl-defmethod cg-bid--deal ((game cg-bid-game) &optional dealer)
  "Deal a fresh hand into GAME.  DEALER defaults to East so South bids first."
  (random t)
  (let ((deck (cg-shuffle (cg-bid--full-deck)))
        (hands (make-vector 4 nil))
        (dealer (or dealer 3)))
    (dotimes (s 4)
      (aset hands s (cl-loop repeat 10 collect (pop deck))))
    (cg-put game :hands hands)
    (cg-put game :kitty deck)              ; remaining 5 cards
    (cg-put game :dealer dealer)
    (cg-put game :phase 'auction)
    (cg-put game :passed (make-vector 4 nil))
    (cg-put game :high-bid nil)
    (cg-put game :high-bidder nil)
    (cg-put game :bidder (mod (1+ dealer) 4)) ; left of dealer bids first
    (cg-put game :contract nil)
    (cg-put game :contractor nil)
    (cg-put game :trick nil)
    (cg-put game :last-trick nil)
    (cg-put game :led nil)
    (cg-put game :leader nil)
    (cg-put game :tricks (make-vector 4 0))
    (cg-put game :ntricks 0)
    (cg-put game :exposed nil)
    (cg-put game :cursor 0)
    (cg-put game :marks nil)
    (cg-put game :hand-result nil)
    (unless (cg-get game :scores) (cg-put game :scores (cons 0 0)))
    (unless (plist-member (oref game env) :game-over)
      (cg-put game :game-over nil))
    (unless (cg-get game :hand-no) (cg-put game :hand-no 0))
    (cg-put game :hand-no (1+ (cg-get game :hand-no)))
    (cg-bid--note game "— Hand %d —" (cg-get game :hand-no))
    (cg-put game :message
            (format "Auction: %s to bid." (aref cg-bid-seat-names
                                                 (cg-get game :bidder))))
    game))

(defun cg-bid--hand (game seat) (aref (cg-get game :hands) seat))
(defun cg-bid--set-hand (game seat cards) (aset (cg-get game :hands) seat cards))


;;;; Auction

(defun cg-bid--legal-bids (game)
  "Return the schedule entries that outbid the current high bid."
  (let ((hv (if (cg-get game :high-bid)
                (cg-bid-value (cg-get game :high-bid)) 0)))
    (cl-remove-if-not (lambda (b) (> (cg-bid-value b) hv)) cg-bid-schedule)))

(defun cg-bid--active-seats (game)
  "Return the seats that have not passed."
  (cl-loop for s below 4
           unless (aref (cg-get game :passed) s) collect s))

(defun cg-bid--next-bidder (game from)
  "Return the next non-passed seat after FROM, or nil if none."
  (cl-loop for i from 1 to 4
           for s = (mod (+ from i) 4)
           unless (aref (cg-get game :passed) s) return s))

(defun cg-bid--note (game fmt &rest args)
  "Append a narrative line (FMT with ARGS) to GAME's message log."
  (cg-put game :log (cons (apply #'format fmt args) (cg-get game :log)))
  (cg-put game :log-scroll 0))

(cl-defmethod cg-bid--auction-act ((game cg-bid-game) seat bid)
  "Record SEAT's action: BID is a schedule entry, or nil to pass."
  (if bid
      (progn (cg-put game :high-bid bid)
             (cg-put game :high-bidder seat)
             (cg-put game :message
                     (format "%s bids %s."
                             (aref cg-bid-seat-names seat) (cg-bid-label bid)))
             (cg-bid--note game "%s bids %s."
                           (aref cg-bid-seat-names seat) (cg-bid-label bid)))
    (aset (cg-get game :passed) seat t)
    (cg-put game :message (format "%s passes." (aref cg-bid-seat-names seat)))
    (cg-bid--note game "%s passes." (aref cg-bid-seat-names seat)))
  (let ((active (cg-bid--active-seats game)))
    (cond
     ;; everyone passed with no bid -> throw in
     ((and (null (cg-get game :high-bid)) (null active))
      (cg-bid--deal game (mod (1+ (cg-get game :dealer)) 4))
      (cg-put game :message "All passed — redeal."))
     ;; one bidder left standing -> contract is set
     ((and (cg-get game :high-bid) (= (length active) 1))
      (cg-bid--begin-contract game))
     (t
      (cg-put game :bidder (cg-bid--next-bidder game seat))))))

(cl-defmethod cg-bid--begin-contract ((game cg-bid-game))
  "Set the winning contract and move to the kitty phase."
  (let* ((contractor (cg-get game :high-bidder))
         (bid (cg-get game :high-bid)))
    (cg-put game :contractor contractor)
    (cg-put game :contract bid)
    (cg-put game :phase 'kitty)
    ;; contractor takes the kitty into hand
    (cg-bid--set-hand game contractor
                      (append (cg-bid--hand game contractor)
                              (cg-get game :kitty)))
    (cg-put game :kitty nil)
    (cg-put game :leader contractor)
    (cg-put game :turn contractor)
    (cg-put game :message
            (format "%s won the auction with %s (%s). Kitty taken."
                    (aref cg-bid-seat-names contractor)
                    (cg-bid-label bid) (cg-bid-name bid)))
    (cg-bid--note game "%s won the bid: %s."
                  (aref cg-bid-seat-names contractor) (cg-bid-label bid))))


;;;; Kitty discard

(cl-defmethod cg-bid--discard ((game cg-bid-game) seat cards)
  "Have SEAT discard CARDS (a list of 5) and start play."
  (cg-bid--set-hand game seat
                    (cl-set-difference (cg-bid--hand game seat) cards
                                       :test #'equal))
  (cg-put game :phase 'play)
  (cg-put game :turn (cg-get game :contractor))
  (cg-put game :leader (cg-get game :contractor))
  (cg-put game :led nil)
  (cg-put game :trick nil)
  (cg-put game :cursor 0)
  (cg-put game :message
          (format "Play! %s leads."
                  (aref cg-bid-seat-names (cg-get game :contractor)))))


;;;; Seat order (a partner sits out during a misère)

(defun cg-bid--misere-p (game)
  "Return non-nil when the current contract is a nullo/misère."
  (let ((c (cg-get game :contract))) (and c (cg-bid-nullo-p c))))

(defun cg-bid--sitter (game)
  "Return the seat sitting out (contractor's partner) in a misère, else nil."
  (and (cg-bid--misere-p game)
       (cg-bid--partner (cg-get game :contractor))))

(defun cg-bid--in-play-p (game seat)
  "Return non-nil when SEAT takes part in the current hand's play."
  (not (eql seat (cg-bid--sitter game))))

(defun cg-bid--num-players (game)
  "Return the number of seats playing to each trick (3 in misère, else 4)."
  (if (cg-bid--misere-p game) 3 4))

(defun cg-bid--next-seat (game seat)
  "Return the next in-play seat clockwise from SEAT."
  (let ((n (mod (1+ seat) 4)))
    (if (cg-bid--in-play-p game n) n (mod (1+ n) 4))))


;;;; Trick play

(cl-defmethod cg-bid--play ((game cg-bid-game) seat card)
  "Have SEAT play CARD into the current trick and advance."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (led (cg-get game :led))
         (lead-p (null (cg-get game :trick))))
    ;; remove card from hand
    (cg-bid--set-hand game seat
                      (cl-remove card (cg-bid--hand game seat)
                                 :test #'equal :count 1))
    (cg-put game :trick (append (cg-get game :trick) (list (cons seat card))))
    (cg-bid--note game "%s %s the %s."
                  (aref cg-bid-seat-names seat)
                  (if lead-p "leads" "plays")
                  (cg-bid-card-string card))
    ;; establish led suit
    (unless led
      (setq led (cg-bid-effective-suit card trump))
      ;; joker led in no-trump nominates a suit
      (when (and (eq led 'joker))
        (setq led (cg-bid--nominate-suit game seat)))
      (cg-put game :led led))
    (if (= (length (cg-get game :trick)) (cg-bid--num-players game))
        (cg-bid--finish-trick game)
      (cg-put game :turn (cg-bid--next-seat game seat)))))

(defun cg-bid--nominate-suit (game seat)
  "Choose the suit nominated when the Joker leads under no-trump."
  (let ((hand (cg-bid--hand game seat)))
    (if (cg-bid--human-p seat)
        (let ((ch (read-char-choice
                   "Joker leads — nominate a suit [s]pades [c]lubs [d]iamonds [h]earts: "
                   '(?s ?c ?d ?h))))
          (cdr (assq ch '((?s . 0) (?c . 1) (?d . 2) (?h . 3)))))
      ;; AI: nominate its longest non-joker suit
      (let ((counts (make-vector 4 0)))
        (dolist (c hand)
          (unless (cg-bid-joker-p c) (cl-incf (aref counts (car c)))))
        (let ((best 0))
          (dotimes (s 4) (when (> (aref counts s) (aref counts best))
                           (setq best s)))
          best)))))

(cl-defmethod cg-bid--finish-trick ((game cg-bid-game))
  "Resolve the completed trick, award it, and set up the next."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (led (cg-get game :led))
         (plays (cg-get game :trick))
         (winner (cg-bid-trick-winner plays trump led)))
    (cl-incf (aref (cg-get game :tricks) winner))
    (cg-put game :ntricks (1+ (cg-get game :ntricks)))
    (cg-put game :last-trick plays)
    (cg-put game :trick nil)
    (cg-put game :led nil)
    (cg-put game :leader winner)
    (cg-put game :turn winner)
    (cg-put game :message
            (format "%s wins the trick (%s)."
                    (aref cg-bid-seat-names winner)
                    (mapconcat (lambda (p) (cg-bid-card-string (cdr p))) plays " ")))
    (cg-bid--note game "%s wins the trick." (aref cg-bid-seat-names winner))
    ;; open misère: expose the contractor's hand after the first trick
    (when (and (cg-bid-open-p (cg-get game :contract))
               (= (cg-get game :ntricks) 1))
      (cg-put game :exposed (cg-get game :contractor)))
    ;; hand over after ten tricks
    (when (= (cg-get game :ntricks) 10)
      (cg-bid--score-hand game))))


;;;; Scoring

(cl-defmethod cg-bid--score-hand ((game cg-bid-game))
  "Score the completed hand per the Avondale schedule."
  (let* ((bid (cg-get game :contract))
         (contractor (cg-get game :contractor))
         (cteam (cg-bid--team contractor))
         (tricks (cg-get game :tricks))
         (side (+ (aref tricks contractor)
                  (aref tricks (cg-bid--partner contractor))))
         (opp (- 10 side))
         (scores (cg-get game :scores))
         (delta-c 0) (delta-o 0) (made nil) result)
    (cond
     ((cg-bid-nullo-p bid)
      ;; contractor alone must take no tricks (own-hand misère)
      (setq made (zerop (aref tricks contractor)))
      (setq delta-c (if made (cg-bid-value bid) (- (cg-bid-value bid)))))
     (t
      (setq made (>= side (cg-bid-tricks bid)))
      (if made
          (setq delta-c (if (and (= side 10) (< (cg-bid-value bid) 250))
                            250 (cg-bid-value bid)))
        (setq delta-c (- (cg-bid-value bid))))
      (setq delta-o (* 10 opp))))
    ;; apply to teams
    (let ((c (if (= cteam 0) (cons delta-c delta-o) (cons delta-o delta-c))))
      (cg-put game :scores (cons (+ (car scores) (car c))
                                 (+ (cdr scores) (cdr c)))))
    (setq result
          (format "%s %s %s: %s/%s took %d trick%s. %s %+d%s"
                  (aref cg-bid-seat-names contractor)
                  (if made "MADE" "was SET on")
                  (cg-bid-label bid)
                  (aref cg-bid-seat-names contractor)
                  (aref cg-bid-seat-names (cg-bid--partner contractor))
                  side (if (= side 1) "" "s")
                  (if (= cteam 0) "You/North" "West/East")
                  delta-c
                  (if (and (not (cg-bid-nullo-p bid)) (> delta-o 0))
                      (format ", opponents +%d" delta-o) "")))
    (cg-put game :phase 'done)
    (cg-put game :hand-result result)
    (cg-bid--note game "%s" result)
    (let ((winner (cg-bid--check-gameover game made cteam)))
      (cg-put game :message
              (if winner
                  (format "%s  —  GAME OVER: %s WIN!  Final — You/North %d, West/East %d.  Press n for a new game."
                          result
                          (if (= winner 0) "You/North" "West/East")
                          (car (cg-get game :scores)) (cdr (cg-get game :scores)))
                (concat result "  —  press n for the next hand."))))))

(cl-defmethod cg-bid--check-gameover ((game cg-bid-game) made cteam)
  "End the game if a side has won (front door) or lost (back door).
Return the winning team, or nil.  MADE and CTEAM describe the hand
just scored: a side wins only by reaching 500 on a made contract;
a side that sinks to -500 loses."
  (let* ((sc (cg-get game :scores))
         (t0 (car sc)) (t1 (cdr sc))
         (winner
          (cond
           ((and made (>= (if (= cteam 0) t0 t1) 500)) cteam)
           ((<= t0 -500) 1)
           ((<= t1 -500) 0)
           (t nil))))
    (when winner
      (cg-put game :game-over winner)
      (cg-put game :phase 'gameover))
    winner))


;;;; Basic AI

(defvar cg-bid-ai-policies (vector 'smart 'smart 'smart 'smart)
  "Per-seat AI policy vector; each element is `smart' or `basic'.")

(defvar cg-bid-ai-partner-help 1.0
  "Tricks the smart bidder assumes its partner will contribute.")

(defun cg-bid--policy (seat)
  "Return the AI policy symbol for SEAT."
  (aref cg-bid-ai-policies seat))

;;; shared helpers

(defun cg-bid--lowest (cards trump led)
  "Return the weakest of CARDS given TRUMP and LED."
  (car (sort (copy-sequence cards)
             (lambda (a b) (< (cg-bid-power a trump led)
                              (cg-bid-power b trump led))))))

(defun cg-bid--highest (cards trump led)
  "Return the strongest of CARDS given TRUMP and LED."
  (car (sort (copy-sequence cards)
             (lambda (a b) (> (cg-bid-power a trump led)
                              (cg-bid-power b trump led))))))

(defun cg-bid--trump-cards (hand trump)
  "Return the cards of HAND that are trumps under TRUMP (incl. Joker, bowers)."
  (cl-remove-if-not
   (lambda (c) (or (cg-bid-joker-p c)
                   (and (numberp trump) (eq (cg-bid-effective-suit c trump) trump))))
   hand))

(defun cg-bid--suit-cards (hand suit trump)
  "Return non-Joker cards of HAND whose effective suit is SUIT under TRUMP."
  (cl-remove-if-not
   (lambda (c) (and (not (cg-bid-joker-p c))
                    (eq (cg-bid-effective-suit c trump) suit)))
   hand))

;;; basic policy (original heuristics)

(defun cg-bid--ai-estimate (hand trump)
  "Rough trick estimate for HAND if TRUMP (0-3 or `nt') were the contract."
  (let ((joker (cl-some #'cg-bid-joker-p hand))
        (aces 0) (kings 0) (trumps 0))
    (dolist (c hand)
      (unless (cg-bid-joker-p c)
        (cond
         ((and (numberp trump) (eq (cg-bid-effective-suit c trump) trump))
          (cl-incf trumps))
         ((= (cdr c) 10) (cl-incf aces))
         ((= (cdr c) 9) (cl-incf kings)))))
    (floor (+ trumps aces (* 0.5 kings) (if joker 1 0)))))

(defun cg-bid--ai-best-contract (hand)
  "Return (TRUMP . EST) for the strongest contract HAND suggests (basic)."
  (let ((best (cons 'nt (cg-bid--ai-estimate hand 'nt))))
    (dotimes (s 4)
      (let ((e (cg-bid--ai-estimate hand s)))
        (when (> e (cdr best)) (setq best (cons s e)))))
    best))

(defun cg-bid--ai-bid-basic (game seat)
  "Pick and record a bid (or pass) for AI SEAT using the basic estimate."
  (let* ((hand (cg-bid--hand game seat))
         (best (cg-bid--ai-best-contract hand))
         (trump (car best))
         (est (min 10 (cdr best)))
         (maxval (cl-loop for b in cg-bid-schedule
                          when (and (eq (cg-bid-trump b) trump)
                                    (= (cg-bid-tricks b) est))
                          return (cg-bid-value b)))
         (legal (cg-bid--legal-bids game))
         (choice (and maxval (>= est 6)
                      (car (cl-remove-if-not
                            (lambda (b) (<= (cg-bid-value b) maxval))
                            legal)))))
    (cg-bid--auction-act game seat choice)))

(defun cg-bid--ai-discard-basic (game seat)
  "Discard SEAT's five weakest cards (basic)."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (sorted (cg-bid-sort-hand (cg-bid--hand game seat) trump))
         (discard (last sorted 5)))
    (cg-bid--discard game seat discard)))

(defun cg-bid--ai-play-positive (game seat)
  "Trick-play for AI SEAT under a suit or no-trump contract (basic)."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (led (cg-get game :led))
         (hand (cg-bid--hand game seat))
         (legal (cg-bid-legal-cards hand led trump))
         (plays (cg-get game :trick))
         card)
    (if (null plays)
        (setq card (or (cl-find-if
                        (lambda (c) (and (not (cg-bid-joker-p c)) (= (cdr c) 10)
                                         (or (not (numberp trump))
                                             (/= (cg-bid-effective-suit c trump) trump))))
                        legal)
                       (cg-bid--lowest legal trump led)))
      (let* ((winner (cg-bid-trick-winner plays trump led))
             (partner-winning (= (cg-bid--partner seat) winner))
             (best-power (cg-bid-power (cdr (assq winner plays)) trump led)))
        (if partner-winning
            (setq card (cg-bid--lowest legal trump led))
          (let ((winners (cl-remove-if-not
                          (lambda (c) (> (cg-bid-power c trump led) best-power))
                          legal)))
            (setq card (if winners
                           (cg-bid--lowest winners trump led)
                         (cg-bid--lowest legal trump led)))))))
    (cg-bid--play game seat card)))

;;; smart policy

(defun cg-bid--eval-suit (hand trump)
  "Estimate tricks (float) for a suit TRUMP contract from HAND."
  (let* ((trumps (cg-bid--trump-cards hand trump))
         (nt (length trumps))
         (high (cl-count-if (lambda (c) (>= (cg-bid-power c trump trump) 809)) trumps))
         (trump-tricks (+ high (max 0 (- nt 4))))
         (side 0.0) (ruffs 0.0))
    (dotimes (s 4)
      (unless (= s trump)
        (let* ((cs (cg-bid--suit-cards hand s trump))
               (len (length cs))
               (ranks (mapcar #'cdr cs)))
          (when (memql 10 ranks) (cl-incf side 1.0))
          (when (memql 9 ranks) (cl-incf side (if (>= len 2) 0.5 0.25)))
          (cond ((= len 0) (cl-incf ruffs 1.0))
                ((and (= len 1) (not (memql 10 ranks))) (cl-incf ruffs 0.5))))))
    (min 10.0 (+ trump-tricks side (min ruffs (float nt))))))

(defun cg-bid--eval-nt (hand)
  "Estimate tricks (float) for a no-trump contract from HAND."
  (let ((est (if (cl-some #'cg-bid-joker-p hand) 1.0 0.0)))
    (dotimes (s 4)
      (let* ((cs (cg-bid--suit-cards hand s 'nt))
             (len (length cs))
             (ranks (mapcar #'cdr cs)))
        (when (memql 10 ranks) (cl-incf est 1.0))
        (when (memql 9 ranks) (cl-incf est (if (>= len 2) 0.5 0.25)))
        (when (>= len 5) (cl-incf est (* 0.5 (- len 4))))))
    (min 10.0 est)))

(defun cg-bid--best-smart (hand)
  "Return (TRUMP . EST-float) for the best contract HAND suggests (smart)."
  (let ((best (cons 'nt (cg-bid--eval-nt hand))))
    (dotimes (s 4)
      (let ((e (cg-bid--eval-suit hand s)))
        (when (> e (cdr best)) (setq best (cons s e)))))
    best))

(defun cg-bid--ai-bid-smart (game seat)
  "Pick and record a bid (or pass) for AI SEAT using the smart evaluation."
  (let* ((hand (cg-bid--hand game seat))
         (best (cg-bid--best-smart hand))
         (trump (car best))
         (est (min 10 (floor (+ (cdr best) cg-bid-ai-partner-help))))
         (maxval (cl-loop for b in cg-bid-schedule
                          when (and (eq (cg-bid-trump b) trump)
                                    (= (cg-bid-tricks b) est))
                          return (cg-bid-value b)))
         (legal (cg-bid--legal-bids game))
         (choice (and maxval (>= est 6)
                      (car (cl-remove-if-not
                            (lambda (b) (<= (cg-bid-value b) maxval))
                            legal)))))
    (cg-bid--auction-act game seat choice)))

(defun cg-bid--ai-discard-smart (game seat)
  "Discard to keep trumps and aces and to void short side suits for ruffs."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (hand (cg-bid--hand game seat))
         (cand '()))
    (dolist (c hand)
      (unless (or (cg-bid-joker-p c) (= (cdr c) 10)
                  (and (numberp trump) (eq (cg-bid-effective-suit c trump) trump)))
        (push c cand)))
    (let ((bysuit (make-vector 4 0)))
      (dolist (c cand) (cl-incf (aref bysuit (car c))))
      (setq cand (sort cand
                       (lambda (a b)
                         (if (/= (aref bysuit (car a)) (aref bysuit (car b)))
                             (< (aref bysuit (car a)) (aref bysuit (car b)))
                           (< (cdr a) (cdr b))))))
      (let ((discard (if (>= (length cand) 5)
                         (cl-subseq cand 0 5)
                       (last (cg-bid-sort-hand hand trump) 5))))
        (cg-bid--discard game seat discard)))))

(defun cg-bid--lead-low-long (hand trump legal)
  "Lead the lowest card of the player's longest side suit, from LEGAL."
  (let ((best-suit nil) (best-len -1))
    (dotimes (s 4)
      (unless (and (numberp trump) (= s trump))
        (let ((len (length (cg-bid--suit-cards hand s trump))))
          (when (> len best-len) (setq best-len len best-suit s)))))
    (let ((cs (and best-suit
                   (cl-remove-if-not
                    (lambda (c) (and (not (cg-bid-joker-p c))
                                     (eq (cg-bid-effective-suit c trump) best-suit)))
                    legal))))
      (cg-bid--lowest (or cs legal) trump nil))))

(defun cg-bid--ai-play-smart (game seat)
  "Trick-play for AI SEAT under a suit/NT contract with simple tactics:
declarer draws trumps and cashes aces; everyone wins as cheaply as
possible and never overtakes a partner who is already winning."
  (let* ((trump (cg-bid-trump (cg-get game :contract)))
         (led (cg-get game :led))
         (hand (cg-bid--hand game seat))
         (legal (cg-bid-legal-cards hand led trump))
         (plays (cg-get game :trick))
         (contractor (cg-get game :contractor))
         (declarer-side (= (cg-bid--team seat) (cg-bid--team contractor)))
         card)
    (cond
     ((null plays)
      (let* ((trumps (and (numberp trump) (cg-bid--trump-cards hand trump)))
             (hi (cl-count-if (lambda (c) (>= (cg-bid-power c trump trump) 809))
                              (or trumps '()))))
        (setq card
              (cond
               ((and declarer-side (numberp trump) trumps
                     (or (>= (length trumps) 4) (>= hi 2)))
                (cg-bid--highest trumps trump trump))
               ((cl-find-if (lambda (c)
                              (and (not (cg-bid-joker-p c)) (= (cdr c) 10)
                                   (or (not (numberp trump))
                                       (/= (cg-bid-effective-suit c trump) trump))))
                            legal))
               (t (cg-bid--lead-low-long hand trump legal))))))
     (t
      (let* ((winner (cg-bid-trick-winner plays trump led))
             (partner-winning (= (cg-bid--partner seat) winner))
             (best-power (cg-bid-power (cdr (assq winner plays)) trump led)))
        (setq card
              (if partner-winning
                  (cg-bid--lowest legal trump led)
                (let ((winners (cl-remove-if-not
                                (lambda (c) (> (cg-bid-power c trump led) best-power))
                                legal)))
                  (if winners (cg-bid--lowest winners trump led)
                    (cg-bid--lowest legal trump led))))))))
    (cg-bid--play game seat card)))

;;; dispatch

(cl-defmethod cg-bid--ai-bid ((game cg-bid-game) seat)
  "Pick and record a bid for AI SEAT per its policy."
  (if (eq (cg-bid--policy seat) 'smart)
      (cg-bid--ai-bid-smart game seat)
    (cg-bid--ai-bid-basic game seat)))

(cl-defmethod cg-bid--ai-discard ((game cg-bid-game) seat)
  "Have AI SEAT exchange the kitty per its policy."
  (if (eq (cg-bid--policy seat) 'smart)
      (cg-bid--ai-discard-smart game seat)
    (cg-bid--ai-discard-basic game seat)))

(cl-defmethod cg-bid--ai-play ((game cg-bid-game) seat)
  "Choose and play a card for AI SEAT per its policy."
  (cond ((cg-bid--misere-p game) (cg-bid--ai-play-misere game seat))
        ((eq (cg-bid--policy seat) 'smart) (cg-bid--ai-play-smart game seat))
        (t (cg-bid--ai-play-positive game seat))))

(defun cg-bid--ai-play-misere (game seat)
  "Trick-play for AI SEAT during a misère.
The contractor sheds its highest card that still loses (or ducks
lowest when leading); defenders simply play low."
  (let* ((trump 'nullo)
         (led (cg-get game :led))
         (hand (cg-bid--hand game seat))
         (legal (cg-bid-legal-cards hand led trump))
         (plays (cg-get game :trick))
         (contractor (cg-get game :contractor))
         card)
    (cond
     ((/= seat contractor)
      (setq card (cg-bid--lowest legal trump led)))
     ((null plays)
      (setq card (cg-bid--lowest legal trump led)))
     (t
      (let* ((wseat (cg-bid-trick-winner plays trump led))
             (bestp (cg-bid-power (cdr (assq wseat plays)) trump led))
             (losers (cl-remove-if-not
                      (lambda (c) (< (cg-bid-power c trump led) bestp)) legal)))
        (setq card (cg-bid--highest (or losers legal) trump led)))))
    (cg-bid--play game seat card)))


;;;; Driver: run AI until the human must act

(defun cg-bid--ai-step (game)
  "Perform one pending AI action in GAME.  Return non-nil if it acted."
  (pcase (cg-get game :phase)
    ('auction (unless (cg-bid--human-p (cg-get game :bidder))
                (cg-bid--ai-bid game (cg-get game :bidder)) t))
    ('kitty   (unless (cg-bid--human-p (cg-get game :contractor))
                (cg-bid--ai-discard game (cg-get game :contractor)) t))
    ('play    (unless (cg-bid--human-p (cg-get game :turn))
                (cg-bid--ai-play game (cg-get game :turn)) t))
    (_ nil)))

(cl-defmethod cg-bid--run ((game cg-bid-game))
  "Advance GAME through AI actions until a human is needed or it ends."
  (let ((guard 0))
    (while (and (< (cl-incf guard) 400) (cg-bid--ai-step game)))))

(provide 'cg-bid)
;;; cg-bid.el ends here
