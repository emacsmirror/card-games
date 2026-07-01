;;; cg-handfoot.el --- Hand and Foot, a partnership rummy  -*- lexical-binding: t; -*-

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
(require 'cg-core)
(require 'cg-rummy)

(defcustom cg-handfoot-target 5000
  "Points a partnership needs to win Hand & Foot."
  :type 'integer :group 'card-games)

(defcustom cg-handfoot-pickup-count 7
  "Cards taken (top included) when picking up the discard pile."
  :type 'integer :group 'card-games)

(defconst cg-handfoot--minimums [50 90 120 150]
  "Initial go-down minimum by round, the last value repeating thereafter.")

(defconst cg-handfoot--names ["You" "West" "North" "East"]
  "Seat labels; North is your partner.")

(defclass cg-handfoot-game (cg-rummy-game)
  ((vname :initform "Hand & Foot"))
  "A game of Hand & Foot.")

;;;; Cards

(defun cg-hf--wild-p (card)
  "Return non-nil when CARD is wild (a Joker or a Two)."
  (or (cg-rummy-joker-p card) (= (cdr card) 1)))

(defun cg-hf--three-p (card)
  "Return non-nil when CARD is a three (never meldable)."
  (and (not (cg-rummy-joker-p card)) (= (cdr card) 2)))

(defun cg-hf--red-three-p (card)
  "Return non-nil when CARD is a red three (a bonus card)."
  (and (not (cg-rummy-joker-p card)) (= (cdr card) 2) (cg-red-suit-p (car card))))

(defun cg-hf-value (card)
  "Return the Hand & Foot point value of CARD."
  (cond ((cg-rummy-joker-p card) 50)
        (t (let ((r (cdr card)))
             (cond ((= r 1) 20)        ; Two (wild)
                   ((= r 0) 20)        ; Ace
                   ((= r 2) 5)         ; Three
                   ((<= r 6) 5)        ; 4 5 6 7
                   (t 10))))))         ; 8 9 10 J Q K

(defun cg-hf--book-rank (cards)
  "Return the natural rank shared by CARDS, or nil if invalid."
  (let ((nats (cl-remove-if #'cg-hf--wild-p cards)))
    (and nats
         (let ((r (cdr (car nats))))
           (and (cl-every (lambda (c) (= (cdr c) r)) nats)
                (/= r 2)
                r)))))

(defun cg-hf--book-valid-p (cards)
  "Return non-nil when CARDS form a legal book."
  (let* ((nats (cl-remove-if #'cg-hf--wild-p cards))
         (wilds (cl-remove-if-not #'cg-hf--wild-p cards)))
    (and (>= (length cards) 3)
         (cg-hf--book-rank cards)
         (>= (length nats) 2)
         (<= (length wilds) 3)
         (<= (length wilds) (length nats)))))

(defun cg-hf--book-complete-p (cards) (>= (length cards) 7))
(defun cg-hf--book-clean-p (cards) (not (cl-some #'cg-hf--wild-p cards)))

;;;; Setup

(defun cg-hf--team (game s) (mod s (cg-get game :nteams)))

(cl-defmethod cg-hf--deal ((game cg-handfoot-game))
  "Deal a fresh round into GAME."
  (let* ((n (cg-get game :nplayers))
         (decks (1+ n))
         (deck (cg-rummy-deck decks 2))
         (hands (make-vector n nil)) (feet (make-vector n nil))
         (stage (make-vector n 0)))
    (dotimes (s n)
      (aset hands s (cg-rummy-sort-hand (cl-loop repeat 11 collect (pop deck))))
      (aset feet s (cg-rummy-sort-hand (cl-loop repeat 11 collect (pop deck)))))
    (cg-put game :hands hands)
    (cg-put game :feet feet)
    (cg-put game :stage stage)
    (cg-put game :books (make-vector (cg-get game :nteams) nil))
    (cg-put game :round (1+ (or (cg-get game :round) -1)))
    (cg-put game :down (make-vector (cg-get game :nteams) nil))
    (cg-put game :redthrees (make-vector (cg-get game :nteams) nil))
    (cg-put game :discard (list (pop deck)))
    (cg-put game :stock deck)
    (dotimes (s n) (cg-hf--collect-red-threes game s))
    (cg-put game :turn 0)
    (cg-put game :step 'draw)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (cg-put game :marks nil)
    (cg-put game :message "Your turn: s draws two cards.")
    game))

(defun cg-hf--books (game team) (aref (cg-get game :books) team))
(defun cg-hf--set-books (game team v) (aset (cg-get game :books) team v))

(defun cg-hf--down-p (game team)
  "Return non-nil when TEAM has met this round's go-down minimum."
  (aref (cg-get game :down) team))

(defun cg-hf--min-for-round (game)
  "Return the go-down minimum for GAME's current round."
  (let ((r (or (cg-get game :round) 0)))
    (aref cg-handfoot--minimums
          (min r (1- (length cg-handfoot--minimums))))))

(defun cg-hf--collect-red-threes (game s)
  "Move seat S's red threes to its team pile, drawing replacements.
Return the number collected."
  (let ((team (cg-hf--team game s)) (moved 0) (again t))
    (while again
      (setq again nil)
      (let ((rt (cl-find-if #'cg-hf--red-three-p (cg-rummy--hand game s))))
        (when rt
          (cg-rummy--set-hand game s (cg-rummy--remove1 rt (cg-rummy--hand game s)))
          (aset (cg-get game :redthrees) team
                (cons rt (aref (cg-get game :redthrees) team)))
          (setq moved (1+ moved))
          (let ((stock (cg-get game :stock)))
            (when stock
              (cg-rummy--set-hand game s (cg-rummy-sort-hand
                                          (cons (car stock) (cg-rummy--hand game s))))
              (cg-put game :stock (cdr stock))))
          (setq again t))))
    moved))

(defun cg-hf--take-foot (game s)
  "Move seat S onto its foot, collecting any red threes it holds."
  (aset (cg-get game :stage) s 1)
  (cg-rummy--set-hand game s (aref (cg-get game :feet) s))
  (cg-hf--collect-red-threes game s))

(defun cg-hf--pickup-eligible (game s)
  "Return non-nil when seat S may pick up the discard pile.
That needs two natural cards in hand matching a meldable top discard."
  (let ((top (cg-rummy--top game)))
    (and top (not (cg-hf--wild-p top)) (not (cg-hf--three-p top))
         (>= (cl-count-if (lambda (c) (and (not (cg-hf--wild-p c))
                                           (= (cdr c) (cdr top))))
                          (cg-rummy--hand game s))
             2))))

(defun cg-hf--pickup (game s)
  "Seat S picks up the discard pile, melding its top card.
Take the top card plus up to `cg-handfoot-pickup-count' - 1 cards beneath
it into hand, melding the top with two matching naturals.  Return the top
card, or nil if ineligible."
  (when (cg-hf--pickup-eligible game s)
    (let* ((pile (cg-get game :discard)) (top (car pile)) (rank (cdr top))
           (team (cg-hf--team game s)) (books (cg-hf--books game team))
           (nats (cl-remove-if-not
                  (lambda (c) (and (not (cg-hf--wild-p c)) (= (cdr c) rank)))
                  (cg-rummy--hand game s)))
           (two (list (nth 0 nats) (nth 1 nats)))
           (existing (cl-find-if
                      (lambda (bk) (and (not (cg-hf--book-complete-p bk))
                                        (equal (cg-hf--book-rank bk) rank)))
                      books))
           (rest (cdr pile))
           (ntake (min (1- cg-handfoot-pickup-count) (length rest)))
           (take (cl-subseq rest 0 ntake))
           (remain (nthcdr ntake rest)))
      (dolist (c two)
        (cg-rummy--set-hand game s (cg-rummy--remove1 c (cg-rummy--hand game s))))
      (if existing
          (setcar (memq existing books)
                  (cg-rummy-sort-hand (append (list top) two existing)))
        (cg-hf--set-books game team
                          (append books (list (cg-rummy-sort-hand (cons top two))))))
      (cg-put game :discard remain)
      (dolist (c take)
        (cg-rummy--set-hand game s (cg-rummy-sort-hand
                                    (cons c (cg-rummy--hand game s)))))
      (cg-hf--collect-red-threes game s)
      top)))

(defun cg-hf--partition-books (cards)
  "Partition CARDS into valid books, or nil if they can't all be used.
Naturals group by rank (each rank needs two), and wilds fill the groups."
  (if (or (null cards) (cl-some #'cg-hf--three-p cards)) nil
    (let ((wilds (cl-remove-if-not #'cg-hf--wild-p cards))
          (byrank (make-hash-table :test 'eql)) (groups '()) (ok t))
      (dolist (c cards)
        (unless (cg-hf--wild-p c) (push c (gethash (cdr c) byrank))))
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
                               (< (cl-count-if #'cg-hf--wild-p bk) 3)
                               (< (cl-count-if #'cg-hf--wild-p bk)
                                  (cl-count-if-not #'cg-hf--wild-p bk))))
                        books)))
              (if tgt (setcar (memq tgt books) (cons wcard tgt)) (setq ok nil))))
          (if (and ok (cl-every #'cg-hf--book-valid-p books)) books nil))))))

(defun cg-hf--initial-meld (game s cards)
  "Lay CARDS as seat S's initial meld, meeting the round minimum.
Return non-nil when the team goes down."
  (let* ((books (cg-hf--partition-books cards))
         (team (cg-hf--team game s)))
    (when (and books
               (cl-subsetp cards (cg-rummy--hand game s) :test #'equal)
               (>= (apply #'+ (mapcar #'cg-hf-value cards))
                   (cg-hf--min-for-round game)))
      (dolist (c cards)
        (cg-rummy--set-hand game s (cg-rummy--remove1 c (cg-rummy--hand game s))))
      (cg-hf--set-books game team
                        (append (cg-hf--books game team)
                                (mapcar #'cg-rummy-sort-hand books)))
      (aset (cg-get game :down) team t)
      t)))

(defun cg-hf--ai-go-down (game s)
  "Try to lay seat S's initial meld meeting the round minimum.
Return non-nil when the team goes down."
  (let* ((hand (cg-rummy--hand game s))
         (byrank (make-hash-table :test 'eql))
         (wilds (cl-remove-if-not #'cg-hf--wild-p hand)) (cards '()))
    (dolist (c hand)
      (unless (or (cg-hf--wild-p c) (cg-hf--three-p c))
        (push c (gethash (cdr c) byrank))))
    (let ((w (copy-sequence wilds)))
      (maphash (lambda (_r cs)
                 (cond ((>= (length cs) 3) (setq cards (append cs cards)))
                       ((and (= (length cs) 2) w)
                        (setq cards (append cs (list (pop w)) cards)))))
               byrank))
    (when (and cards (>= (apply #'+ (mapcar #'cg-hf-value cards))
                         (cg-hf--min-for-round game)))
      (cg-hf--initial-meld game s cards))))

(defun cg-hf--ai-meld (game s)
  "Meld for seat S, going down only when the round minimum is met."
  (let ((team (cg-hf--team game s)))
    (unless (cg-hf--down-p game team) (cg-hf--ai-go-down game s))
    (when (cg-hf--down-p game team) (cg-hf--ai-extend game s))))

;;;; Engine

(defun cg-hf--draw2 (game s)
  "Draw two stock cards into seat S's hand.  Return nil if stock runs out."
  (let ((ok t))
    (dotimes (_ 2)
      (let ((stock (cg-get game :stock)))
        (if (null stock) (setq ok nil)
          (cg-rummy--set-hand game s (cg-rummy-sort-hand
                                      (cons (car stock) (cg-rummy--hand game s))))
          (cg-put game :stock (cdr stock)))))
    ok))

(defun cg-hf--meld (game s cards)
  "Have seat S lay CARDS as a new book for their team.  Return non-nil on win."
  (when (and (cg-hf--book-valid-p cards)
             (cl-subsetp cards (cg-rummy--hand game s) :test #'equal))
    (let ((team (cg-hf--team game s)))
      (dolist (c cards)
        (cg-rummy--set-hand game s (cg-rummy--remove1 c (cg-rummy--hand game s))))
      (cg-hf--set-books game team
                        (append (cg-hf--books game team)
                                (list (cg-rummy-sort-hand (copy-sequence cards)))))
      t)))

(defun cg-rummy--remove1 (card list)
  "Return LIST with one copy of CARD (an `equal' match) removed."
  (let ((seen nil))
    (cl-remove-if (lambda (c) (and (not seen) (equal c card) (setq seen t))) list)))

(defun cg-hf--layoff (game s card)
  "Lay CARD off onto a team book it fits.  Return non-nil on success."
  (let* ((team (cg-hf--team game s)) (books (cg-hf--books game team)) (done nil))
    (catch 'hit
      (dolist (bk books)
        (unless (cg-hf--book-complete-p bk)
          (let ((cand (cons card bk)))
            (when (cg-hf--book-valid-p cand)
              (cg-rummy--set-hand game s (cg-rummy--remove1 card (cg-rummy--hand game s)))
              (setcar (memq bk books) (cg-rummy-sort-hand cand))
              (setq done t)
              (throw 'hit t))))))
    done))

(defun cg-hf--advance (game s)
  "After a play, take up the foot or finish, then pass the turn."
  (let ((stage (cg-get game :stage)))
    (when (and (= (aref stage s) 0) (null (cg-rummy--hand game s)))
      ;; hand exhausted: pick up the foot
      (cg-hf--take-foot game s))
    (if (and (= (aref stage s) 1) (null (cg-rummy--hand game s))
             (cg-hf--can-go-out-p game (cg-hf--team game s)))
        (cg-hf--score-round game s)
      (cg-put game :turn (mod (1+ s) (cg-get game :nplayers)))
      (cg-put game :step 'draw))))

(defun cg-hf--can-go-out-p (game team)
  "Return non-nil when TEAM owns at least two complete books."
  (>= (cl-count-if #'cg-hf--book-complete-p (cg-hf--books game team)) 2))

(defun cg-hf--discard (game s card)
  "Discard CARD from seat S and end the play portion of the turn."
  (cg-rummy--set-hand game s (cg-rummy--remove1 card (cg-rummy--hand game s)))
  (cg-put game :discard (cons card (cg-get game :discard)))
  (cg-hf--advance game s))

(cl-defmethod cg-hf--score-round ((game cg-handfoot-game) outseat)
  "Score the round (OUTSEAT went out, or nil if the stock ran dry)."
  (let* ((nt (cg-get game :nteams)) (scores (cg-get game :scores)))
    (dotimes (team nt)
      (let ((pts 0))
        (dolist (bk (cg-hf--books game team))
          (dolist (c bk) (setq pts (+ pts (cg-hf-value c))))
          (when (cg-hf--book-complete-p bk)
            (setq pts (+ pts (if (cg-hf--book-clean-p bk) 500 300)))))
        (when (and outseat (= (cg-hf--team game outseat) team))
          (setq pts (+ pts 100)))         ; going-out bonus
        (let ((k (length (aref (cg-get game :redthrees) team))))
          (setq pts (+ pts (* k (if (>= k 4) 200 100)))))  ; red threes
        ;; subtract cards left in members' hands and feet
        (dotimes (s (cg-get game :nplayers))
          (when (= (cg-hf--team game s) team)
            (dolist (c (cg-rummy--hand game s)) (setq pts (- pts (cg-hf-value c))))
            (when (= (aref (cg-get game :stage) s) 0)
              (dolist (c (aref (cg-get game :feet) s))
                (setq pts (- pts (cg-hf-value c)))))))
        (aset scores team (+ (aref scores team) pts))))
    (let ((win nil) (best most-negative-fixnum))
      (dotimes (team nt)
        (when (and (>= (aref scores team) cg-handfoot-target)
                   (> (aref scores team) best))
          (setq win team best (aref scores team))))
      (cg-put game :phase (if win 'game-over 'round-over))
      (cg-put game :winner (or win (and outseat (cg-hf--team game outseat))))
      (cg-put game :reveal t)
      (cg-put game :message
              (concat
               (if outseat (format "%s goes out!  " (aref cg-handfoot--names outseat))
                 "Stock exhausted.  ")
               (if win (format "Team %d wins the game!  (n: new game)" win)
                 (format "Round over.  Scores: %s.  (n: next round)"
                         (cg-hf--scores-string game))))))))

(defun cg-hf--scores-string (game)
  "Return a compact per-team score line for GAME."
  (let ((scores (cg-get game :scores)) (parts '()))
    (dotimes (team (cg-get game :nteams))
      (push (format "Team %d %d" team (aref scores team)) parts))
    (mapconcat #'identity (nreverse parts) " · ")))

;;;; AI

(defun cg-hf--ai-extend (game s)
  "Extend and add books for seat S once the team is down."
  ;; lay off naturals onto existing incomplete team books
  (let ((again t))
    (while again
      (setq again nil)
      (let ((card (cl-find-if
                   (lambda (c)
                     (and (not (cg-hf--wild-p c)) (not (cg-hf--three-p c))
                          (cl-find-if
                           (lambda (bk) (and (not (cg-hf--book-complete-p bk))
                                             (equal (cg-hf--book-rank bk) (cdr c))))
                           (cg-hf--books game (cg-hf--team game s)))))
                   (cg-rummy--hand game s))))
        (when card (cg-hf--layoff game s card) (setq again t)))))
  ;; start new books from ranks with >=3 naturals in hand
  (let ((again t))
    (while again
      (setq again nil)
      (let* ((hand (cg-rummy--hand game s))
             (byrank (make-hash-table :test 'eql)) (target nil))
        (dolist (c hand)
          (unless (or (cg-hf--wild-p c) (cg-hf--three-p c))
            (push c (gethash (cdr c) byrank))))
        (maphash (lambda (_r cs) (when (and (not target) (>= (length cs) 3))
                                   (setq target cs)))
                 byrank)
        (when target
          (cg-hf--meld game s (cl-subseq target 0 (min 7 (length target))))
          (setq again t)))))
  ;; push a nearly-complete book to seven with a spare wild
  (let ((again t))
    (while again
      (setq again nil)
      (let ((wild (cl-find-if #'cg-hf--wild-p (cg-rummy--hand game s)))
            (team (cg-hf--team game s)))
        (when wild
          (let ((bk (cl-find-if
                     (lambda (b)
                       (and (not (cg-hf--book-complete-p b))
                            (>= (length b) 5)
                            (> (cl-count-if-not #'cg-hf--wild-p b)
                               (cl-count-if #'cg-hf--wild-p b))
                            (< (cl-count-if #'cg-hf--wild-p b) 3)))
                     (cg-hf--books game team))))
            (when bk
              (cg-rummy--set-hand game s (cg-rummy--remove1 wild (cg-rummy--hand game s)))
              (setcar (memq bk (cg-hf--books game team))
                      (cg-rummy-sort-hand (cons wild bk)))
              (setq again t))))))))

(defun cg-hf--ai-discard-card (game s)
  "Return the card seat S should discard."
  (let ((hand (cg-rummy--hand game s)))
    (or (cl-find-if #'cg-hf--three-p hand)
        ;; a high singleton, else the first card
        (let ((byrank (make-hash-table :test 'eql)) (best (car hand)) (bestv -1))
          (dolist (c hand) (unless (cg-hf--wild-p c)
                             (push c (gethash (cdr c) byrank))))
          (maphash (lambda (_r cs)
                     (when (= (length cs) 1)
                       (let ((v (cg-hf-value (car cs))))
                         (when (> v bestv) (setq best (car cs) bestv v)))))
                   byrank)
          best))))

(cl-defmethod cg-hf--ai-turn ((game cg-handfoot-game) s)
  "Play seat S's whole turn."
  (let ((got (or (and (> (length (cg-get game :stock)) 30)
                      (cg-hf--pickup-eligible game s)
                      (cg-hf--pickup game s))
                 (cg-hf--draw2 game s))))
    (if (not got)
        (cg-hf--score-round game nil)
      (cg-hf--ai-meld game s)
      (when (eq (cg-get game :phase) 'play)
        ;; if the hand emptied through melding, pick up the foot and meld again
        (when (and (= (aref (cg-get game :stage) s) 0) (null (cg-rummy--hand game s)))
          (cg-hf--take-foot game s)
          (cg-hf--ai-meld game s))
        (when (eq (cg-get game :phase) 'play)
          (if (cg-rummy--hand game s)
              (cg-hf--discard game s (cg-hf--ai-discard-card game s))
            (cg-hf--advance game s)))))))

(defun cg-hf--run (game)
  "Advance AI seats until it is your turn or the round ends."
  (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0))
    (cg-hf--ai-turn game (cg-get game :turn))))

;;;; UI

(defvar-local cg-hf--game nil "The Hand & Foot game in the current buffer.")

(defun cg-hf--svg (game)
  "Return an SVG board for the Hand & Foot GAME."
  (let* ((scores (cg-get game :scores)) (infos '()) (melds '()))
    (dotimes (team (cg-get game :nteams))
      (push (format "Team %d  (score %d)%s%s" team (aref scores team)
                    (if (cg-hf--down-p game team) "  down"
                      (format "  needs %d" (cg-hf--min-for-round game)))
                    (let ((k (length (aref (cg-get game :redthrees) team))))
                      (if (> k 0) (format "  red3:%d" k) "")))
            infos)
      (dolist (bk (cg-hf--books game team))
        (push (cons (format "T%d%s" team
                            (if (cg-hf--book-complete-p bk)
                                (if (cg-hf--book-clean-p bk) " clean" " dirty") ""))
                    bk)
              melds)))
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "%s: %d in hand%s" (aref cg-handfoot--names s)
                      (length (cg-rummy--hand game s))
                      (if (= (aref (cg-get game :stage) s) 1) " (on foot)" ""))
              infos)))
    (cg-rummy--board-svg
     :title (format "Hand & Foot   target %d   round %d   (min %d)"
                    cg-handfoot-target (1+ (or (cg-get game :round) 0))
                    (cg-hf--min-for-round game))
     :infos (nreverse infos) :melds (nreverse melds)
     :discard (cg-rummy--top game) :stock (length (cg-get game :stock))
     :hand (cg-rummy--hand game 0) :cursor (cg-get game :cursor)
     :marks (cg-get game :marks) :message (cg-get game :message))))

(cl-defmethod cg-render ((game cg-handfoot-game))
  "Return a depiction of the Hand & Foot GAME: SVG board if graphical, else text."
  (if (and cg-rummy-svg-cards (display-graphic-p))
      (cg-hf--svg game)
    (cg-hf--render-text game)))

(defun cg-hf--render-text (game)
  "Return a plain-text depiction of the Hand & Foot GAME."
  (let* ((out '()) (scores (cg-get game :scores))
         (hand (cg-rummy--hand game 0)) (cursor (cg-get game :cursor)))
    (push (format "  Hand & Foot   target %d   round %d  (go-down minimum %d)\n\n"
                  cg-handfoot-target (1+ (or (cg-get game :round) 0))
                  (cg-hf--min-for-round game)) out)
    (dotimes (team (cg-get game :nteams))
      (push (format "  Team %d  (score %d)%s%s:\n" team (aref scores team)
                    (if (cg-hf--down-p game team) "  down"
                      (format "  needs %d to go down" (cg-hf--min-for-round game)))
                    (let ((k (length (aref (cg-get game :redthrees) team))))
                      (if (> k 0) (format "  red3:%d" k) ""))) out)
      (let ((bks (cg-hf--books game team)))
        (if bks
            (dolist (bk bks)
              (push (format "    %s%s\n"
                            (mapconcat #'cg-rummy-card-string bk " ")
                            (cond ((cg-hf--book-complete-p bk)
                                   (if (cg-hf--book-clean-p bk) "  [clean pile]" "  [dirty pile]"))
                                  (t "")))
                    out))
          (push "    (no books yet)\n" out))))
    (push "\n" out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  %-6s %d in hand%s\n" (aref cg-handfoot--names s)
                      (length (cg-rummy--hand game s))
                      (if (= (aref (cg-get game :stage) s) 1) " (on foot)" ""))
              out)))
    (push (format "\n  Discard: %s (pile %d)     Stock: %d\n\n"
                  (let ((cs (cg-rummy-card-string (cg-rummy--top game))) (tp (cg-rummy--top game)))
                    (if (and tp (not (cg-rummy-joker-p tp)) (cg-red-suit-p (car tp)))
                        (propertize cs 'face 'cg-red-suit) cs))
                  (length (cg-get game :discard))
                  (length (cg-get game :stock)))
          out)
    (push (format "  Your %s:\n   " (if (= (aref (cg-get game :stage) 0) 1) "foot" "hand")) out)
    (push (cg-rummy--render-cards hand cursor (cg-get game :marks) nil 'hand) out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-handfoot-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i))
    (_ (cl-call-next-method))))

(defun cg-hf--redisplay ()
  "Redraw the Hand & Foot buffer."
  (let ((game cg-hf--game) (inhibit-read-only t))
        (setq cg-current-game game cg-redisplay-function #'cg-hf--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (cg-get game :step) (cg-get game :phase))))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-hf--clamp (g)
  "Keep G's cursor in range and drop stale marks."
  (let ((n (length (cg-rummy--hand g 0))))
    (cg-put g :cursor (if (> n 0) (min (cg-get g :cursor) (1- n)) 0))
    (cg-put g :marks (cl-remove-if (lambda (i) (>= i n)) (cg-get g :marks)))))

(defun cg-hf--my-turn-p (g)
  (and (eq (cg-get g :phase) 'play) (= (cg-get g :turn) 0)))

(defun cg-hf-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g cg-hf--game) (n (length (cg-rummy--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-hf--redisplay)))

(defun cg-hf-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g cg-hf--game) (n (length (cg-rummy--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-hf--redisplay)))

(defun cg-hf-mark ()
  "Toggle a mark on the card under the cursor."
  (interactive)
  (let* ((g cg-hf--game) (i (cg-get g :cursor)) (marks (cg-get g :marks)))
    (cg-put g :marks (if (memq i marks) (delq i marks) (cons i marks)))
    (cg-hf--redisplay)))

(defun cg-hf--marked (g)
  "Return the marked cards in G's hand."
  (let ((hand (cg-rummy--hand g 0)))
    (mapcar (lambda (i) (nth i hand)) (sort (copy-sequence (cg-get g :marks)) #'<))))

(defun cg-hf-meld ()
  "Meld the marked cards.
Until your team is down you must mark a complete initial meld -- one or
more valid books totalling at least the round minimum -- and lay it in one
action.  After that, mark single books as usual."
  (interactive)
  (let* ((g cg-hf--game) (cards (cg-hf--marked g)) (team (cg-hf--team g 0)))
    (cond
     ((not (cg-hf--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s)."))
     ((not (cg-hf--down-p g team))
      (if (cg-hf--initial-meld g 0 cards)
          (progn
            (cg-put g :marks nil) (cg-hf--clamp g)
            (when (and (= (aref (cg-get g :stage) 0) 0) (null (cg-rummy--hand g 0)))
              (cg-hf--take-foot g 0) (cg-hf--clamp g))
            (cg-put g :message
                    (format "You're down!  (met the %d minimum.)  Meld more, lay off, or discard."
                            (cg-hf--min-for-round g))))
        (cg-put g :message
                (format "Initial meld must be valid books totalling >= %d; mark them all, then m."
                        (cg-hf--min-for-round g)))))
     ((not (cg-hf--book-valid-p cards))
      (cg-put g :message "Not a legal book: 3+ of a rank, 2+ natural, wilds <= naturals."))
     ((cg-hf--meld g 0 cards)
      (cg-put g :marks nil) (cg-hf--clamp g)
      (if (and (= (aref (cg-get g :stage) 0) 0) (null (cg-rummy--hand g 0)))
          (progn (cg-hf--take-foot g 0) (cg-hf--clamp g)
                 (cg-put g :message "Hand cleared -- foot picked up!  Keep melding or discard (RET)."))
        (unless (and (= (aref (cg-get g :stage) 0) 1) (null (cg-rummy--hand g 0)))
          (cg-put g :message "Booked.  Meld more, lay off (l), or discard (RET)."))))
     (t (cg-put g :message "Could not meld those cards.")))
    (cg-hf--redisplay)))

(defun cg-hf-layoff ()
  "Lay the cursor card (or marked cards) off onto a team book."
  (interactive)
  (let* ((g cg-hf--game) (marks (cg-hf--marked g)))
    (cond
     ((not (cg-hf--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s)."))
     (t (let ((cards (or marks (list (nth (cg-get g :cursor) (cg-rummy--hand g 0)))))
              (any nil))
          (dolist (c cards) (when (and c (cg-hf--layoff g 0 c)) (setq any t)))
          (cg-put g :marks nil) (cg-hf--clamp g)
          (cg-put g :message (if any "Laid off." "That card fits none of your books.")))))
    (cg-hf--redisplay)))

(defun cg-hf-draw ()
  "Draw two cards from the stock."
  (interactive)
  (let ((g cg-hf--game))
    (cond
     ((not (cg-hf--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((not (eq (cg-get g :step) 'draw)) (cg-put g :message "You already drew."))
     ((cg-hf--draw2 g 0)
      (cg-hf--collect-red-threes g 0)
      (cg-put g :step 'play) (cg-hf--clamp g)
      (cg-put g :message "Meld (m), lay off (l), then discard (RET)."))
     (t (cg-hf--score-round g nil)))
    (cg-hf--redisplay)))

(defun cg-hf-pickup ()
  "Pick up the discard pile by melding its top card (Hand & Foot)."
  (interactive)
  (let ((g cg-hf--game))
    (cond
     ((not (cg-hf--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((not (eq (cg-get g :step) 'draw)) (cg-put g :message "You already drew."))
     ((not (cg-hf--pickup-eligible g 0))
      (cg-put g :message
              "Can't pick up: you need two natural cards matching the top discard."))
     (t (let ((top (cg-hf--pickup g 0)))
          (cg-put g :step 'play) (cg-hf--clamp g)
          (cg-put g :message
                  (format "Picked up the pile, melding %s.  Meld more, lay off, or discard."
                          (cg-rummy-card-string top))))))
    (cg-hf--redisplay)))

(defun cg-hf-discard ()
  "Discard the cursor card and end your turn."
  (interactive)
  (let* ((g cg-hf--game) (card (nth (cg-get g :cursor) (cg-rummy--hand g 0))))
    (cond
     ((not (cg-hf--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((eq (cg-get g :step) 'draw) (cg-put g :message "Draw first (s)."))
     ((null card) (cg-put g :message "No card selected."))
     (t (cg-hf--discard g 0 card)
        (cg-put g :marks nil)
        (when (memq (cg-get g :phase) '(play))
          (cg-put g :message "You discarded.")
          (cg-hf--run g))))
    (cg-hf--redisplay)))

(defun cg-hf-new ()
  "Deal a fresh round, or a new game when one is over."
  (interactive)
  (let ((g cg-hf--game))
    (when (eq (cg-get g :phase) 'game-over)
      (cg-put g :scores (make-vector (cg-get g :nteams) 0))
      (cg-put g :round -1))
    (cg-put g :reveal nil)
    (cg-hf--deal g)
    (cg-hf--run g)
    (cg-hf--redisplay)))

(defun cg-hf-redraw () "Redraw the board." (interactive) (cg-hf--redisplay))

(defun cg-hf-help ()
  "Describe the Hand & Foot controls."
  (interactive)
  (message "Arrows: choose  SPC: mark  m: meld  l: lay off  s: draw 2  p: pick up pile  RET: discard  n: new"))

(defvar cg-handfoot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-hf-left)
    (define-key map (kbd "<right>") #'cg-hf-right)
    (define-key map (kbd "SPC") #'cg-hf-mark)
    (define-key map "m" #'cg-hf-meld)
    (define-key map "l" #'cg-hf-layoff)
    (define-key map "s" #'cg-hf-draw)
    (define-key map "p" #'cg-hf-pickup)
    (define-key map (kbd "RET") #'cg-hf-discard)
    (define-key map "n" #'cg-hf-new)
    (define-key map "g" #'cg-hf-redraw)
    (define-key map "?" #'cg-hf-help)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-handfoot-mode'.")

(define-derived-mode cg-handfoot-mode special-mode "Hand&Foot"
  "Major mode for Hand & Foot."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-handfoot ()
  "Play Hand & Foot, partnering North against two AI opponents."
  (interactive)
  (let ((buf (get-buffer-create "*Hand & Foot*")))
    (with-current-buffer buf
      (cg-handfoot-mode)
      (setq cg-hf--game (cg-handfoot-game))
      (cg-put cg-hf--game :nplayers 4)
      (cg-put cg-hf--game :nteams 2)
      (cg-put cg-hf--game :scores (make-vector 2 0))
      (cg-hf--deal cg-hf--game)
      (cg-hf--run cg-hf--game)
      (cg-hf--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'cg-hand-and-foot #'cg-handfoot)

(provide 'cg-handfoot)
;;; cg-handfoot.el ends here
