;;; card-games-handfoot.el --- Hand and Foot, a partnership rummy  -*- lexical-binding: t; -*-

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

;; Hand & Foot, a Canasta cousin played in partnerships.  Each player is
;; dealt two packets: a "hand" played first and a "foot" taken up once the
;; hand is gone.  Partners build *books* -- three or more cards of one
;; rank, suits ignored -- on the table; a book of seven is complete (a
;; "pile"), clean if it holds no wild card and dirty if it does.  Twos and
;; Jokers are wild.  You go out, ending the round, once your side owns at
;; least two complete books and you can empty your foot.
;;
;; You partner the North player against East and West, all three of them
;; computer opponents.  Mark cards with SPC, meld them with m, lay off onto
;; a book with l, and discard with RET.
;;
;; This Hand & Foot includes the round-by-round go-down minimum (50, 90,
;; 120, then 150), the red-three bonus (100 each, or 200 each for all four),
;; and picking up the discard pile -- meld its top card with two matching
;; naturals (`p') to take the top card plus several cards beneath it.
;; Cards use the package cons
;; (SUIT . RANK), RANK 0 (Ace) .. 12 (King), with jokers as (joker . 0).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-rummy)

(defcustom card-games-handfoot-target 5000
  "Points a partnership needs to win Hand & Foot."
  :type 'integer :group 'card-games)

(defcustom card-games-handfoot-pickup-count 7
  "Cards taken (top included) when picking up the discard pile."
  :type 'integer :group 'card-games)

(defconst card-games-handfoot--minimums [50 90 120 150]
  "Initial go-down minimum by round, the last value repeating thereafter.")

(defconst card-games-handfoot--names ["You" "West" "North" "East"]
  "Seat labels; North is your partner.")

(defclass card-games-handfoot-game (card-games-rummy-game)
  ((vname :initform "Hand & Foot"))
  "A game of Hand & Foot.")

;;;; Cards

(defun card-games-hf--wild-p (card)
  "Return non-nil when CARD is wild (a Joker or a Two)."
  (or (card-games-rummy-joker-p card) (= (cdr card) 1)))

(defun card-games-hf--three-p (card)
  "Return non-nil when CARD is a three (never meldable)."
  (and (not (card-games-rummy-joker-p card)) (= (cdr card) 2)))

(defun card-games-hf--red-three-p (card)
  "Return non-nil when CARD is a red three (a bonus card)."
  (and (not (card-games-rummy-joker-p card)) (= (cdr card) 2) (card-games-red-suit-p (car card))))

(defun card-games-hf-value (card)
  "Return the Hand & Foot point value of CARD."
  (cond ((card-games-rummy-joker-p card) 50)
        (t (let ((r (cdr card)))
             (cond ((= r 1) 20)        ; Two (wild)
                   ((= r 0) 20)        ; Ace
                   ((= r 2) 5)         ; Three
                   ((<= r 6) 5)        ; 4 5 6 7
                   (t 10))))))         ; 8 9 10 J Q K

(defun card-games-hf--book-rank (cards)
  "Return the natural rank shared by CARDS, or nil if invalid."
  (let ((nats (cl-remove-if #'card-games-hf--wild-p cards)))
    (and nats
         (let ((r (cdr (car nats))))
           (and (cl-every (lambda (c) (= (cdr c) r)) nats)
                (/= r 2)
                r)))))

(defun card-games-hf--book-valid-p (cards)
  "Return non-nil when CARDS form a legal book."
  (let* ((nats (cl-remove-if #'card-games-hf--wild-p cards))
         (wilds (cl-remove-if-not #'card-games-hf--wild-p cards)))
    (and (>= (length cards) 3)
         (card-games-hf--book-rank cards)
         (>= (length nats) 2)
         (<= (length wilds) 3)
         (<= (length wilds) (length nats)))))

(defun card-games-hf--book-complete-p (cards) "Return non-nil when CARDS form a complete (7+ card) book." (>= (length cards) 7))
(defun card-games-hf--book-clean-p (cards) "Return non-nil when CARDS is a clean book (no wilds)." (not (cl-some #'card-games-hf--wild-p cards)))

;;;; Setup

(defun card-games-hf--team (game s) "Return the team index of seat S in GAME." (mod s (card-games-get game :nteams)))

(cl-defmethod card-games-hf--deal ((game card-games-handfoot-game))
  "Deal a fresh round into GAME."
  (let* ((n (card-games-get game :nplayers))
         (decks (1+ n))
         (deck (card-games-rummy-deck decks 2))
         (hands (make-vector n nil)) (feet (make-vector n nil))
         (stage (make-vector n 0)))
    (dotimes (s n)
      (aset hands s (card-games-rummy-sort-hand (cl-loop repeat 11 collect (pop deck))))
      (aset feet s (card-games-rummy-sort-hand (cl-loop repeat 11 collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :feet feet)
    (card-games-put game :stage stage)
    (card-games-put game :books (make-vector (card-games-get game :nteams) nil))
    (card-games-put game :round (1+ (or (card-games-get game :round) -1)))
    (card-games-put game :down (make-vector (card-games-get game :nteams) nil))
    (card-games-put game :redthrees (make-vector (card-games-get game :nteams) nil))
    (card-games-put game :discard (list (pop deck)))
    (card-games-put game :stock deck)
    (dotimes (s n) (card-games-hf--collect-red-threes game s))
    (card-games-put game :turn 0)
    (card-games-put game :step 'draw)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (card-games-put game :marks nil)
    (card-games-put game :message "Your turn: s draws two cards.")
    game))

(defun card-games-hf--books (game team) "Return TEAM's books in GAME." (aref (card-games-get game :books) team))
(defun card-games-hf--set-books (game team v) "Set TEAM's books in GAME to V." (aset (card-games-get game :books) team v))

(defun card-games-hf--down-p (game team)
  "Return non-nil when GAME TEAM has met this round's go-down minimum."
  (aref (card-games-get game :down) team))

(defun card-games-hf--min-for-round (game)
  "Return the go-down minimum for GAME's current round."
  (let ((r (or (card-games-get game :round) 0)))
    (aref card-games-handfoot--minimums
          (min r (1- (length card-games-handfoot--minimums))))))

(defun card-games-hf--collect-red-threes (game s)
  "Move GAME seat S's red threes to its team pile, drawing replacements.
Return the number collected."
  (let ((team (card-games-hf--team game s)) (moved 0) (again t))
    (while again
      (setq again nil)
      (let ((rt (cl-find-if #'card-games-hf--red-three-p (card-games-rummy--hand game s))))
        (when rt
          (card-games-rummy--set-hand game s (card-games-rummy--remove1 rt (card-games-rummy--hand game s)))
          (aset (card-games-get game :redthrees) team
                (cons rt (aref (card-games-get game :redthrees) team)))
          (setq moved (1+ moved))
          (let ((stock (card-games-get game :stock)))
            (when stock
              (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                          (cons (car stock) (card-games-rummy--hand game s))))
              (card-games-put game :stock (cdr stock))))
          (setq again t))))
    moved))

(defun card-games-hf--take-foot (game s)
  "Move GAME seat S onto its foot, collecting any red threes it has."
  (aset (card-games-get game :stage) s 1)
  (card-games-rummy--set-hand game s (aref (card-games-get game :feet) s))
  (card-games-hf--collect-red-threes game s))

(defun card-games-hf--pickup-eligible (game s)
  "Return non-nil when GAME seat S may pick up the discard pile.
That needs two natural cards in hand matching a meldable top discard."
  (let ((top (card-games-rummy--top game)))
    (and top (not (card-games-hf--wild-p top)) (not (card-games-hf--three-p top))
         (>= (cl-count-if (lambda (c) (and (not (card-games-hf--wild-p c))
                                           (= (cdr c) (cdr top))))
                          (card-games-rummy--hand game s))
             2))))

(defun card-games-hf--pickup (game s)
  "Have GAME seat S pick up the discard pile, melding its top card.
Take the top card plus up to `card-games-handfoot-pickup-count' - 1 cards
beneath it into hand, melding the top with two matching naturals.  Return
the top card, or nil if ineligible."
  (when (card-games-hf--pickup-eligible game s)
    (let* ((pile (card-games-get game :discard)) (top (car pile)) (rank (cdr top))
           (team (card-games-hf--team game s)) (books (card-games-hf--books game team))
           (nats (cl-remove-if-not
                  (lambda (c) (and (not (card-games-hf--wild-p c)) (= (cdr c) rank)))
                  (card-games-rummy--hand game s)))
           (two (list (nth 0 nats) (nth 1 nats)))
           (existing (cl-find-if
                      (lambda (bk) (and (not (card-games-hf--book-complete-p bk))
                                        (equal (card-games-hf--book-rank bk) rank)))
                      books))
           (rest (cdr pile))
           (ntake (min (1- card-games-handfoot-pickup-count) (length rest)))
           (take (cl-subseq rest 0 ntake))
           (remain (nthcdr ntake rest)))
      (dolist (c two)
        (card-games-rummy--set-hand game s (card-games-rummy--remove1 c (card-games-rummy--hand game s))))
      (if existing
          (setcar (memq existing books)
                  (card-games-rummy-sort-hand (append (list top) two existing)))
        (card-games-hf--set-books game team
                          (append books (list (card-games-rummy-sort-hand (cons top two))))))
      (card-games-put game :discard remain)
      (dolist (c take)
        (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                    (cons c (card-games-rummy--hand game s)))))
      (card-games-hf--collect-red-threes game s)
      top)))

(defun card-games-hf--partition-books (cards)
  "Partition CARDS into valid books, or nil if they can't all be used.
Naturals group by rank (each rank needs two), and wilds fill the groups."
  (if (or (null cards) (cl-some #'card-games-hf--three-p cards)) nil
    (let ((wilds (cl-remove-if-not #'card-games-hf--wild-p cards))
          (byrank (make-hash-table :test 'eql)) (groups '()) (ok t))
      (dolist (c cards)
        (unless (card-games-hf--wild-p c) (push c (gethash (cdr c) byrank))))
      (maphash (lambda (_r cs) (push cs groups)) byrank)
      (when (or (null groups) (cl-some (lambda (g) (< (length g) 2)) groups))
        (setq ok nil))
      (when ok
        (let ((w (copy-sequence wilds)) (books '()))
          (dolist (g (sort groups (lambda (a b) (< (length a) (length b)))))
            (let ((bk (copy-sequence g)))
              (while (and (< (length bk) 3) w) (push (pop w) bk))
              (push bk books)))
          (dolist (wcard w)
            (let ((tgt (cl-find-if
                        (lambda (bk)
                          (and (< (length bk) 7)
                               (< (cl-count-if #'card-games-hf--wild-p bk) 3)
                               (< (cl-count-if #'card-games-hf--wild-p bk)
                                  (cl-count-if-not #'card-games-hf--wild-p bk))))
                        books)))
              (if tgt (setcar (memq tgt books) (cons wcard tgt)) (setq ok nil))))
          (if (and ok (cl-every #'card-games-hf--book-valid-p books)) books nil))))))

(defun card-games-hf--initial-meld (game s cards)
  "Lay CARDS as GAME seat S's initial meld, meeting the round minimum.
Return non-nil when the team goes down."
  (let* ((books (card-games-hf--partition-books cards))
         (team (card-games-hf--team game s)))
    (when (and books
               (cl-subsetp cards (card-games-rummy--hand game s) :test #'equal)
               (>= (apply #'+ (mapcar #'card-games-hf-value cards))
                   (card-games-hf--min-for-round game)))
      (dolist (c cards)
        (card-games-rummy--set-hand game s (card-games-rummy--remove1 c (card-games-rummy--hand game s))))
      (card-games-hf--set-books game team
                        (append (card-games-hf--books game team)
                                (mapcar #'card-games-rummy-sort-hand books)))
      (aset (card-games-get game :down) team t)
      t)))

(defun card-games-hf--ai-go-down (game s)
  "Try to lay GAME seat S's initial meld meeting the round minimum.
Return non-nil when the team goes down."
  (let* ((hand (card-games-rummy--hand game s))
         (byrank (make-hash-table :test 'eql))
         (wilds (cl-remove-if-not #'card-games-hf--wild-p hand)) (cards '()))
    (dolist (c hand)
      (unless (or (card-games-hf--wild-p c) (card-games-hf--three-p c))
        (push c (gethash (cdr c) byrank))))
    (let ((w (copy-sequence wilds)))
      (maphash (lambda (_r cs)
                 (cond ((>= (length cs) 3) (setq cards (append cs cards)))
                       ((and (= (length cs) 2) w)
                        (setq cards (append cs (list (pop w)) cards)))))
               byrank))
    (when (and cards (>= (apply #'+ (mapcar #'card-games-hf-value cards))
                         (card-games-hf--min-for-round game)))
      (card-games-hf--initial-meld game s cards))))

(defun card-games-hf--ai-meld (game s)
  "Meld for GAME seat S, going down only when the round minimum is met."
  (let ((team (card-games-hf--team game s)))
    (unless (card-games-hf--down-p game team) (card-games-hf--ai-go-down game s))
    (when (card-games-hf--down-p game team) (card-games-hf--ai-extend game s))))

;;;; Engine

(defun card-games-hf--draw2 (game s)
  "Draw two of GAME's stock cards into seat S's hand; nil if stock is empty."
  (let ((ok t))
    (dotimes (_ 2)
      (let ((stock (card-games-get game :stock)))
        (if (null stock) (setq ok nil)
          (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                                      (cons (car stock) (card-games-rummy--hand game s))))
          (card-games-put game :stock (cdr stock)))))
    ok))

(defun card-games-hf--meld (game s cards)
  "Have GAME seat S lay CARDS as a new team book; non-nil on win."
  (when (and (card-games-hf--book-valid-p cards)
             (cl-subsetp cards (card-games-rummy--hand game s) :test #'equal))
    (let ((team (card-games-hf--team game s)))
      (dolist (c cards)
        (card-games-rummy--set-hand game s (card-games-rummy--remove1 c (card-games-rummy--hand game s))))
      (card-games-hf--set-books game team
                        (append (card-games-hf--books game team)
                                (list (card-games-rummy-sort-hand (copy-sequence cards)))))
      t)))

(defun card-games-rummy--remove1 (card list)
  "Return LIST with one copy of CARD (an `equal' match) removed."
  (let ((seen nil))
    (cl-remove-if (lambda (c) (and (not seen) (equal c card) (setq seen t))) list)))

(defun card-games-hf--layoff (game s card)
  "Lay CARD off from seat S onto a GAME team book; non-nil on success."
  (let* ((team (card-games-hf--team game s)) (books (card-games-hf--books game team)) (done nil))
    (catch 'hit
      (dolist (bk books)
        (unless (card-games-hf--book-complete-p bk)
          (let ((cand (cons card bk)))
            (when (card-games-hf--book-valid-p cand)
              (card-games-rummy--set-hand game s (card-games-rummy--remove1 card (card-games-rummy--hand game s)))
              (setcar (memq bk books) (card-games-rummy-sort-hand cand))
              (setq done t)
              (throw 'hit t))))))
    done))

(defun card-games-hf--advance (game s)
  "After a play by GAME seat S, take up the foot or finish, then pass the turn."
  (let ((stage (card-games-get game :stage)))
    (when (and (= (aref stage s) 0) (null (card-games-rummy--hand game s)))
      ;; hand exhausted: pick up the foot
      (card-games-hf--take-foot game s))
    (if (and (= (aref stage s) 1) (null (card-games-rummy--hand game s))
             (card-games-hf--can-go-out-p game (card-games-hf--team game s)))
        (card-games-hf--score-round game s)
      (card-games-put game :turn (mod (1+ s) (card-games-get game :nplayers)))
      (card-games-put game :step 'draw))))

(defun card-games-hf--can-go-out-p (game team)
  "Return non-nil when GAME TEAM owns at least two complete books."
  (>= (cl-count-if #'card-games-hf--book-complete-p (card-games-hf--books game team)) 2))

(defun card-games-hf--discard (game s card)
  "Discard CARD from GAME seat S and end the play portion of the turn."
  (card-games-rummy--set-hand game s (card-games-rummy--remove1 card (card-games-rummy--hand game s)))
  (card-games-put game :discard (cons card (card-games-get game :discard)))
  (card-games-hf--advance game s))

(cl-defmethod card-games-hf--score-round ((game card-games-handfoot-game) outseat)
  "Score GAME's round (OUTSEAT went out, or nil if the stock ran dry)."
  (let* ((nt (card-games-get game :nteams)) (scores (card-games-get game :scores)))
    (dotimes (team nt)
      (let ((pts 0))
        (dolist (bk (card-games-hf--books game team))
          (dolist (c bk) (setq pts (+ pts (card-games-hf-value c))))
          (when (card-games-hf--book-complete-p bk)
            (setq pts (+ pts (if (card-games-hf--book-clean-p bk) 500 300)))))
        (when (and outseat (= (card-games-hf--team game outseat) team))
          (setq pts (+ pts 100)))         ; going-out bonus
        (let ((k (length (aref (card-games-get game :redthrees) team))))
          (setq pts (+ pts (* k (if (>= k 4) 200 100)))))  ; red threes
        ;; subtract cards left in members' hands and feet
        (dotimes (s (card-games-get game :nplayers))
          (when (= (card-games-hf--team game s) team)
            (dolist (c (card-games-rummy--hand game s)) (setq pts (- pts (card-games-hf-value c))))
            (when (= (aref (card-games-get game :stage) s) 0)
              (dolist (c (aref (card-games-get game :feet) s))
                (setq pts (- pts (card-games-hf-value c)))))))
        (aset scores team (+ (aref scores team) pts))))
    (let ((win nil) (best most-negative-fixnum))
      (dotimes (team nt)
        (when (and (>= (aref scores team) card-games-handfoot-target)
                   (> (aref scores team) best))
          (setq win team best (aref scores team))))
      (card-games-put game :phase (if win 'game-over 'round-over))
      (card-games-put game :winner (or win (and outseat (card-games-hf--team game outseat))))
      (card-games-put game :reveal t)
      (card-games-put game :message
              (concat
               (if outseat (format "%s goes out!  " (aref card-games-handfoot--names outseat))
                 "Stock exhausted.  ")
               (if win (format "Team %d wins the game!  (n: new game)" win)
                 (format "Round over.  Scores: %s.  (n: next round)"
                         (card-games-hf--scores-string game))))))))

(defun card-games-hf--scores-string (game)
  "Return a compact per-team score line for GAME."
  (let ((scores (card-games-get game :scores)) (parts '()))
    (dotimes (team (card-games-get game :nteams))
      (push (format "Team %d %d" team (aref scores team)) parts))
    (mapconcat #'identity (nreverse parts) " · ")))

;;;; AI

(defun card-games-hf--ai-extend (game s)
  "Extend and add books for GAME seat S once the team is down."
  ;; lay off naturals onto existing incomplete team books
  (let ((again t))
    (while again
      (setq again nil)
      (let ((card (cl-find-if
                   (lambda (c)
                     (and (not (card-games-hf--wild-p c)) (not (card-games-hf--three-p c))
                          (cl-find-if
                           (lambda (bk) (and (not (card-games-hf--book-complete-p bk))
                                             (equal (card-games-hf--book-rank bk) (cdr c))))
                           (card-games-hf--books game (card-games-hf--team game s)))))
                   (card-games-rummy--hand game s))))
        (when card (card-games-hf--layoff game s card) (setq again t)))))
  ;; start new books from ranks with >=3 naturals in hand
  (let ((again t))
    (while again
      (setq again nil)
      (let* ((hand (card-games-rummy--hand game s))
             (byrank (make-hash-table :test 'eql)) (target nil))
        (dolist (c hand)
          (unless (or (card-games-hf--wild-p c) (card-games-hf--three-p c))
            (push c (gethash (cdr c) byrank))))
        (maphash (lambda (_r cs) (when (and (not target) (>= (length cs) 3))
                                   (setq target cs)))
                 byrank)
        (when target
          (card-games-hf--meld game s (cl-subseq target 0 (min 7 (length target))))
          (setq again t)))))
  ;; push a nearly-complete book to seven with a spare wild
  (let ((again t))
    (while again
      (setq again nil)
      (let ((wild (cl-find-if #'card-games-hf--wild-p (card-games-rummy--hand game s)))
            (team (card-games-hf--team game s)))
        (when wild
          (let ((bk (cl-find-if
                     (lambda (b)
                       (and (not (card-games-hf--book-complete-p b))
                            (>= (length b) 5)
                            (> (cl-count-if-not #'card-games-hf--wild-p b)
                               (cl-count-if #'card-games-hf--wild-p b))
                            (< (cl-count-if #'card-games-hf--wild-p b) 3)))
                     (card-games-hf--books game team))))
            (when bk
              (card-games-rummy--set-hand game s (card-games-rummy--remove1 wild (card-games-rummy--hand game s)))
              (setcar (memq bk (card-games-hf--books game team))
                      (card-games-rummy-sort-hand (cons wild bk)))
              (setq again t))))))))

(defun card-games-hf--ai-discard-card (game s)
  "Return the card GAME seat S should discard."
  (let ((hand (card-games-rummy--hand game s)))
    (or (cl-find-if #'card-games-hf--three-p hand)
        ;; a high singleton, else the first card
        (let ((byrank (make-hash-table :test 'eql)) (best (car hand)) (bestv -1))
          (dolist (c hand) (unless (card-games-hf--wild-p c)
                             (push c (gethash (cdr c) byrank))))
          (maphash (lambda (_r cs)
                     (when (= (length cs) 1)
                       (let ((v (card-games-hf-value (car cs))))
                         (when (> v bestv) (setq best (car cs) bestv v)))))
                   byrank)
          best))))

(cl-defmethod card-games-hf--ai-turn ((game card-games-handfoot-game) s)
  "Play GAME seat S's whole turn."
  (let ((got (or (and (> (length (card-games-get game :stock)) 30)
                      (card-games-hf--pickup-eligible game s)
                      (card-games-hf--pickup game s))
                 (card-games-hf--draw2 game s))))
    (if (not got)
        (card-games-hf--score-round game nil)
      (card-games-hf--ai-meld game s)
      (when (eq (card-games-get game :phase) 'play)
        ;; if the hand emptied through melding, pick up the foot and meld again
        (when (and (= (aref (card-games-get game :stage) s) 0) (null (card-games-rummy--hand game s)))
          (card-games-hf--take-foot game s)
          (card-games-hf--ai-meld game s))
        (when (eq (card-games-get game :phase) 'play)
          (if (card-games-rummy--hand game s)
              (card-games-hf--discard game s (card-games-hf--ai-discard-card game s))
            (card-games-hf--advance game s)))))))

(defun card-games-hf--run (game)
  "Advance GAME's AI seats until your turn or the round ends."
  (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0))
    (card-games-hf--ai-turn game (card-games-get game :turn))))

;;;; UI

(defvar-local card-games-hf--game nil "The Hand & Foot game in the current buffer.")

(defun card-games-hf--svg (game)
  "Return an SVG board for the Hand & Foot GAME."
  (let* ((scores (card-games-get game :scores)) (infos '()) (melds '()))
    (dotimes (team (card-games-get game :nteams))
      (push (format "Team %d  (score %d)%s%s" team (aref scores team)
                    (if (card-games-hf--down-p game team) "  down"
                      (format "  needs %d" (card-games-hf--min-for-round game)))
                    (let ((k (length (aref (card-games-get game :redthrees) team))))
                      (if (> k 0) (format "  red3:%d" k) "")))
            infos)
      (dolist (bk (card-games-hf--books game team))
        (push (cons (format "T%d%s" team
                            (if (card-games-hf--book-complete-p bk)
                                (if (card-games-hf--book-clean-p bk) " clean" " dirty") ""))
                    bk)
              melds)))
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "%s: %d in hand%s" (aref card-games-handfoot--names s)
                      (length (card-games-rummy--hand game s))
                      (if (= (aref (card-games-get game :stage) s) 1) " (on foot)" ""))
              infos)))
    (card-games-rummy--board-svg
     :title (format "Hand & Foot   target %d   round %d   (min %d)"
                    card-games-handfoot-target (1+ (or (card-games-get game :round) 0))
                    (card-games-hf--min-for-round game))
     :infos (nreverse infos) :melds (nreverse melds)
     :discard (card-games-rummy--top game) :stock (length (card-games-get game :stock))
     :hand (card-games-rummy--hand game 0) :cursor (card-games-get game :cursor)
     :marks (card-games-get game :marks) :message (card-games-get game :message))))

(cl-defmethod card-games-render ((game card-games-handfoot-game))
  "Return a depiction of the Hand & Foot GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-hf--svg game)
    (card-games-hf--render-text game)))

(defun card-games-hf--render-text (game)
  "Return a plain-text depiction of the Hand & Foot GAME."
  (let* ((out '()) (scores (card-games-get game :scores))
         (hand (card-games-rummy--hand game 0)) (cursor (card-games-get game :cursor)))
    (push (format "  Hand & Foot   target %d   round %d  (go-down minimum %d)\n\n"
                  card-games-handfoot-target (1+ (or (card-games-get game :round) 0))
                  (card-games-hf--min-for-round game)) out)
    (dotimes (team (card-games-get game :nteams))
      (push (format "  Team %d  (score %d)%s%s:\n" team (aref scores team)
                    (if (card-games-hf--down-p game team) "  down"
                      (format "  needs %d to go down" (card-games-hf--min-for-round game)))
                    (let ((k (length (aref (card-games-get game :redthrees) team))))
                      (if (> k 0) (format "  red3:%d" k) ""))) out)
      (let ((bks (card-games-hf--books game team)))
        (if bks
            (dolist (bk bks)
              (push (format "    %s%s\n"
                            (mapconcat #'card-games-rummy-card-string bk " ")
                            (cond ((card-games-hf--book-complete-p bk)
                                   (if (card-games-hf--book-clean-p bk) "  [clean pile]" "  [dirty pile]"))
                                  (t "")))
                    out))
          (push "    (no books yet)\n" out))))
    (push "\n" out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  %-6s %d in hand%s\n" (aref card-games-handfoot--names s)
                      (length (card-games-rummy--hand game s))
                      (if (= (aref (card-games-get game :stage) s) 1) " (on foot)" ""))
              out)))
    (push (format "\n  Discard: %s (pile %d)     Stock: %d\n\n"
                  (let ((cs (card-games-rummy-card-string (card-games-rummy--top game))) (tp (card-games-rummy--top game)))
                    (if (and tp (not (card-games-rummy-joker-p tp)) (card-games-red-suit-p (car tp)))
                        (propertize cs 'face 'card-games-red-suit) cs))
                  (length (card-games-get game :discard))
                  (length (card-games-get game :stock)))
          out)
    (push (format "  Your %s:\n   " (if (= (aref (card-games-get game :stage) 0) 1) "foot" "hand")) out)
    (push (card-games-rummy--render-cards hand cursor (card-games-get game :marks) nil 'hand) out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-handfoot-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i))
    (_ (cl-call-next-method))))

(defun card-games-hf--redisplay ()
  "Redraw the Hand & Foot buffer."
  (let ((game card-games-hf--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-hf--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (card-games-get game :step) (card-games-get game :phase))))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-hf--clamp (g)
  "Keep G's cursor in range and drop stale marked cards."
  (let ((n (length (card-games-rummy--hand g 0))))
    (card-games-put g :cursor (if (> n 0) (min (card-games-get g :cursor) (1- n)) 0))
    (card-games-put g :marks (cl-remove-if (lambda (i) (>= i n)) (card-games-get g :marks)))))

(defun card-games-hf--my-turn-p (g)
  "Return non-nil when it is your turn in G."
  (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0)))

(defun card-games-hf-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-hf--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-hf--redisplay)))

(defun card-games-hf-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-hf--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-hf--redisplay)))

(defun card-games-hf-mark ()
  "Toggle a mark on the card under the cursor."
  (interactive)
  (let* ((g card-games-hf--game) (i (card-games-get g :cursor)) (marks (card-games-get g :marks)))
    (card-games-put g :marks (if (memq i marks) (delq i marks) (cons i marks)))
    (card-games-hf--redisplay)))

(defun card-games-hf--marked (g)
  "Return the marked cards in G's hand."
  (let ((hand (card-games-rummy--hand g 0)))
    (mapcar (lambda (i) (nth i hand)) (sort (copy-sequence (card-games-get g :marks)) #'<))))

(defun card-games-hf-meld ()
  "Meld the marked cards.
Until your team is down you must mark a complete initial meld -- one or
more valid books totalling at least the round minimum -- and lay it in one
action.  After that, mark single books as usual."
  (interactive)
  (let* ((g card-games-hf--game) (cards (card-games-hf--marked g)) (team (card-games-hf--team g 0)))
    (cond
     ((not (card-games-hf--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s)."))
     ((not (card-games-hf--down-p g team))
      (if (card-games-hf--initial-meld g 0 cards)
          (progn
            (card-games-put g :marks nil) (card-games-hf--clamp g)
            (when (and (= (aref (card-games-get g :stage) 0) 0) (null (card-games-rummy--hand g 0)))
              (card-games-hf--take-foot g 0) (card-games-hf--clamp g))
            (card-games-put g :message
                    (format "You're down!  (met the %d minimum.)  Meld more, lay off, or discard."
                            (card-games-hf--min-for-round g))))
        (card-games-put g :message
                (format "Initial meld must be valid books totalling >= %d; mark them all, then m."
                        (card-games-hf--min-for-round g)))))
     ((not (card-games-hf--book-valid-p cards))
      (card-games-put g :message "Not a legal book: 3+ of a rank, 2+ natural, wilds <= naturals."))
     ((card-games-hf--meld g 0 cards)
      (card-games-put g :marks nil) (card-games-hf--clamp g)
      (if (and (= (aref (card-games-get g :stage) 0) 0) (null (card-games-rummy--hand g 0)))
          (progn (card-games-hf--take-foot g 0) (card-games-hf--clamp g)
                 (card-games-put g :message "Hand cleared -- foot picked up!  Keep melding or discard (RET)."))
        (unless (and (= (aref (card-games-get g :stage) 0) 1) (null (card-games-rummy--hand g 0)))
          (card-games-put g :message "Booked.  Meld more, lay off (l), or discard (RET)."))))
     (t (card-games-put g :message "Could not meld those cards.")))
    (card-games-hf--redisplay)))

(defun card-games-hf-layoff ()
  "Lay the cursor card (or marked cards) off onto a team book."
  (interactive)
  (let* ((g card-games-hf--game) (marks (card-games-hf--marked g)))
    (cond
     ((not (card-games-hf--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s)."))
     (t (let ((cards (or marks (list (nth (card-games-get g :cursor) (card-games-rummy--hand g 0)))))
              (any nil))
          (dolist (c cards) (when (and c (card-games-hf--layoff g 0 c)) (setq any t)))
          (card-games-put g :marks nil) (card-games-hf--clamp g)
          (card-games-put g :message (if any "Laid off." "That card fits none of your books.")))))
    (card-games-hf--redisplay)))

(defun card-games-hf-draw ()
  "Draw two cards from the stock."
  (interactive)
  (let ((g card-games-hf--game))
    (cond
     ((not (card-games-hf--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "You already drew."))
     ((card-games-hf--draw2 g 0)
      (card-games-hf--collect-red-threes g 0)
      (card-games-put g :step 'play) (card-games-hf--clamp g)
      (card-games-put g :message "Meld (m), lay off (l), then discard (RET)."))
     (t (card-games-hf--score-round g nil)))
    (card-games-hf--redisplay)))

(defun card-games-hf-pickup ()
  "Pick up the discard pile by melding its top card (Hand & Foot)."
  (interactive)
  (let ((g card-games-hf--game))
    (cond
     ((not (card-games-hf--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "You already drew."))
     ((not (card-games-hf--pickup-eligible g 0))
      (card-games-put g :message
              "Can't pick up: you need two natural cards matching the top discard."))
     (t (let ((top (card-games-hf--pickup g 0)))
          (card-games-put g :step 'play) (card-games-hf--clamp g)
          (card-games-put g :message
                  (format "Picked up the pile, melding %s.  Meld more, lay off, or discard."
                          (card-games-rummy-card-string top))))))
    (card-games-hf--redisplay)))

(defun card-games-hf-discard ()
  "Discard the cursor card and end your turn."
  (interactive)
  (let* ((g card-games-hf--game) (card (nth (card-games-get g :cursor) (card-games-rummy--hand g 0))))
    (cond
     ((not (card-games-hf--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((eq (card-games-get g :step) 'draw) (card-games-put g :message "Draw first (s)."))
     ((null card) (card-games-put g :message "No card selected."))
     (t (card-games-hf--discard g 0 card)
        (card-games-put g :marks nil)
        (when (memq (card-games-get g :phase) '(play))
          (card-games-put g :message "You discarded.")
          (card-games-hf--run g))))
    (card-games-hf--redisplay)))

(defun card-games-hf-new ()
  "Deal a fresh round, or a new game when one is over."
  (interactive)
  (let ((g card-games-hf--game))
    (when (eq (card-games-get g :phase) 'game-over)
      (card-games-put g :scores (make-vector (card-games-get g :nteams) 0))
      (card-games-put g :round -1))
    (card-games-put g :reveal nil)
    (card-games-hf--deal g)
    (card-games-hf--run g)
    (card-games-hf--redisplay)))

(defun card-games-hf-redraw () "Redraw the board." (interactive) (card-games-hf--redisplay))

(defun card-games-hf-help ()
  "Describe the Hand & Foot controls."
  (interactive)
  (message "Arrows: choose  SPC: mark  m: meld  l: lay off  s: draw 2  p: pick up pile  RET: discard  n: new"))

(defvar card-games-handfoot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-hf-left)
    (define-key map (kbd "<right>") #'card-games-hf-right)
    (define-key map (kbd "SPC") #'card-games-hf-mark)
    (define-key map "m" #'card-games-hf-meld)
    (define-key map "l" #'card-games-hf-layoff)
    (define-key map "s" #'card-games-hf-draw)
    (define-key map "p" #'card-games-hf-pickup)
    (define-key map (kbd "RET") #'card-games-hf-discard)
    (define-key map "n" #'card-games-hf-new)
    (define-key map "g" #'card-games-hf-redraw)
    (define-key map "?" #'card-games-hf-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-handfoot-mode'.")

(define-derived-mode card-games-handfoot-mode special-mode "Hand&Foot"
  "Major mode for Hand & Foot."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-handfoot ()
  "Play Hand & Foot, partnering North against two AI opponents."
  (interactive)
  (let ((buf (get-buffer-create "*Hand & Foot*")))
    (with-current-buffer buf
      (card-games-handfoot-mode)
      (setq card-games-hf--game (card-games-handfoot-game))
      (card-games-put card-games-hf--game :nplayers 4)
      (card-games-put card-games-hf--game :nteams 2)
      (card-games-put card-games-hf--game :scores (make-vector 2 0))
      (card-games-hf--deal card-games-hf--game)
      (card-games-hf--run card-games-hf--game)
      (card-games-hf--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'card-games-hand-and-foot #'card-games-handfoot)

(provide 'card-games-handfoot)
;;; card-games-handfoot.el ends here
