;;; card-games-cribbage.el --- Cribbage, with pegging and the show  -*- lexical-binding: t; -*-

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

;; Two-handed Cribbage to 121, against the computer.
;;
;; Each deal you lay two cards away to the crib (which belongs to the
;; dealer), cut a starter, then play the pegging round -- adding cards
;; toward 31 and scoring fifteens, pairs, runs, and the go.  Then comes
;; the show, where both hands and the crib are counted for fifteens,
;; pairs, runs, flushes, and his nobs.  The deal alternates.
;;
;; Cards use the package cons (SUIT . RANK), RANK 0 (Ace) .. 12 (King);
;; for counting, an Ace is one, face cards ten, the rest their pip value.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-rummy)

(defcustom card-games-cribbage-target 121
  "Points needed to win a game of Cribbage."
  :type 'integer :group 'card-games)

(defclass card-games-cribbage-game (card-games-game)
  ((vname :initform "Cribbage"))
  "A two-handed game of Cribbage.")

;;;; Scoring primitives

(defun card-games-crib--val (card)
  "Return CARD's counting value (Ace 1, faces 10, else pip)."
  (let ((r (cdr card))) (if (<= r 8) (1+ r) 10)))

(defun card-games-crib--count-15s (cards)
  "Return points for all subsets of CARDS summing to fifteen."
  (let ((n (length cards)) (vec (vconcat cards)) (count 0))
    (dotimes (mask (ash 1 n))
      (let ((sum 0))
        (dotimes (i n)
          (when (/= 0 (logand mask (ash 1 i)))
            (setq sum (+ sum (card-games-crib--val (aref vec i))))))
        (when (= sum 15) (setq count (+ count 2)))))
    count))

(defun card-games-crib--count-pairs (cards)
  "Return points for all pairs in CARDS."
  (let ((cnt (make-vector 13 0)) (tot 0))
    (dolist (c cards) (aset cnt (cdr c) (1+ (aref cnt (cdr c)))))
    (dotimes (r 13)
      (let ((k (aref cnt r))) (setq tot (+ tot (* k (1- k))))))  ; 2*C(k,2)=k*(k-1)
    tot))

(defun card-games-crib--count-runs (cards)
  "Return points for every run of three or more in CARDS (with multiplicity)."
  (let ((cnt (make-vector 13 0)) (total 0) (r 0))
    (dolist (c cards) (aset cnt (cdr c) (1+ (aref cnt (cdr c)))))
    (while (< r 13)
      (if (= 0 (aref cnt r)) (setq r (1+ r))
        (let ((len 0) (mult 1))
          (while (and (< r 13) (> (aref cnt r) 0))
            (setq len (1+ len) mult (* mult (aref cnt r)) r (1+ r)))
          (when (>= len 3) (setq total (+ total (* len mult)))))))
    total))

(defun card-games-crib--flush (hand starter is-crib)
  "Return flush points for the four-card HAND with STARTER.
A crib (IS-CRIB) flush must include the starter."
  (let ((s (car (car hand))))
    (cond ((not (cl-every (lambda (c) (= (car c) s)) hand)) 0)
          ((= (car starter) s) 5)
          (is-crib 0)
          (t 4))))

(defun card-games-crib--nobs (hand starter)
  "Return 1 when HAND has the Jack of the STARTER's suit, else 0."
  (if (cl-find-if (lambda (c) (and (= (cdr c) 10) (= (car c) (car starter)))) hand)
      1 0))

(defun card-games-crib--score-show (hand starter &optional is-crib)
  "Return the show score of the four-card HAND with STARTER.
IS-CRIB applies the stricter crib flush rule."
  (let ((all (cons starter hand)))
    (+ (card-games-crib--count-15s all)
       (card-games-crib--count-pairs all)
       (card-games-crib--count-runs all)
       (card-games-crib--flush hand starter is-crib)
       (card-games-crib--nobs hand starter))))

(defun card-games-crib--peg-score (seq total)
  "Return pegging points for the play whose sequence is SEQ (newest first).
TOTAL is the running count after the play."
  (let ((pts 0))
    (when (= total 15) (setq pts (+ pts 2)))
    (when (= total 31) (setq pts (+ pts 2)))
    ;; pairs: leading same-rank run in SEQ
    (let ((r (cdr (car seq))) (m 0) (lst seq) (stop nil))
      (while (and lst (not stop))
        (if (= (cdr (car lst)) r) (setq m (1+ m) lst (cdr lst)) (setq stop t)))
      (setq pts (+ pts (pcase m (2 2) (3 6) (4 12) (_ 0)))))
    ;; runs: largest k>=3 whose last k cards form a consecutive run
    (let ((best 0) (k (length seq)))
      (while (>= k 3)
        (let* ((lastk (cl-subseq seq 0 k))
               (ranks (sort (mapcar #'cdr lastk) #'<)))
          (when (and (= (length ranks) (length (delete-dups (copy-sequence ranks))))
                     (= (- (car (last ranks)) (car ranks)) (1- k)))
            (setq best (max best k))))
        (setq k (1- k)))
      (setq pts (+ pts best)))
    pts))

;;;; Setup and flow

(defsubst card-games-crib--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-crib--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))
(defsubst card-games-crib--play (game s) "Return seat S's pegging-play cards in GAME." (aref (card-games-get game :play) s))
(defsubst card-games-crib--set-play (game s v) "Set seat S's pegging-play cards in GAME to V." (aset (card-games-get game :play) s v))

(defun card-games-crib--who (s) "Return the display name of seat S." (if (= s 0) "You" "Computer"))

(cl-defmethod card-games-crib--deal ((game card-games-cribbage-game))
  "Deal a fresh Cribbage hand into GAME."
  (let ((deck (card-games-rummy-deck)) (hands (make-vector 2 nil)))
    (dotimes (s 2)
      (aset hands s (card-games-rummy-sort-hand (cl-loop repeat 6 collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :deck deck)
    (card-games-put game :crib nil)
    (card-games-put game :starter nil)
    (card-games-put game :phase 'discard)
    (card-games-put game :cursor 0)
    (card-games-put game :marks nil)
    (unless (card-games-get game :scores) (card-games-put game :scores (make-vector 2 0)))
    (unless (integerp (card-games-get game :dealer)) (card-games-put game :dealer 1))
    (card-games-put game :message "Discard two cards to the crib: SPC marks, m confirms.")
    game))

(defun card-games-crib--add (game s pts)
  "Add PTS to GAME seat S and end the game if it reaches the target."
  (when (> pts 0)
    (aset (card-games-get game :scores) s (+ (aref (card-games-get game :scores) s) pts))
    (when (>= (aref (card-games-get game :scores) s) card-games-cribbage-target)
      (card-games-put game :phase 'game-over)
      (card-games-put game :winner s))))

(defun card-games-crib--ai-discard (game s)
  "Return the two cards GAME seat S should lay away (keep the best four)."
  (let* ((hand (card-games-crib--hand game s)) (best nil) (bestv -1))
    (dolist (combo (card-games-rummy--combinations hand 4))
      (let ((v (card-games-crib--score-show combo '(0 . 0))))   ; rough: no starter
        (when (> v bestv) (setq bestv v best combo))))
    (cl-set-difference hand best :test #'equal)))

(cl-defmethod card-games-crib--start-play ((game card-games-cribbage-game))
  "Cut GAME's starter and begin the pegging round."
  (let* ((deck (card-games-get game :deck))
         (starter (nth (random (length deck)) deck))
         (dealer (card-games-get game :dealer)))
    (card-games-put game :starter starter)
    (when (= (cdr starter) 10)            ; his heels: starter is a Jack
      (card-games-crib--add game dealer 2))
    (card-games-put game :play (vector (card-games-crib--hand game 0) (card-games-crib--hand game 1)))
    (card-games-put game :seq nil)
    (card-games-put game :total 0)
    (card-games-put game :go nil)
    (card-games-put game :last-player nil)
    (card-games-put game :pturn (- 1 dealer))     ; non-dealer leads
    (card-games-put game :phase (if (eq (card-games-get game :phase) 'game-over) 'game-over 'play))
    (card-games-put game :cursor 0)
    (card-games-put game :message
            (format "Pegging: %s leads.  Starter is %s."
                    (card-games-crib--who (- 1 dealer)) (card-games-rummy-card-string starter)))))

(defun card-games-crib--legal (game s)
  "Return GAME seat S's play-cards that fit under 31."
  (cl-remove-if (lambda (c) (> (card-games-crib--val c) (- 31 (card-games-get game :total))))
                (card-games-crib--play game s)))

(defun card-games-crib--peg-play (game s card)
  "Have GAME seat S play CARD into the pegging round, pegging any points."
  (card-games-crib--set-play game s (cl-remove card (card-games-crib--play game s) :test #'equal :count 1))
  (card-games-put game :seq (cons card (card-games-get game :seq)))
  (card-games-put game :total (+ (card-games-get game :total) (card-games-crib--val card)))
  (card-games-put game :last-player s)
  (card-games-put game :go nil)
  (let ((pts (card-games-crib--peg-score (card-games-get game :seq) (card-games-get game :total))))
    (card-games-crib--add game s pts)
    (card-games-put game :message
            (format "%s played %s (count %d)%s."
                    (card-games-crib--who s) (card-games-rummy-card-string card) (card-games-get game :total)
                    (if (> pts 0) (format " for %d" pts) ""))))
  (if (= (card-games-get game :total) 31)
      (card-games-crib--peg-reset game)
    (card-games-put game :pturn (- 1 s))))

(defun card-games-crib--peg-reset (game)
  "Reset GAME's running count; the player after the last to play leads."
  (card-games-put game :seq nil)
  (card-games-put game :total 0)
  (card-games-put game :go nil)
  (card-games-put game :pturn (- 1 (card-games-get game :last-player))))

(defun card-games-crib--peg-over-p (game)
  "Return non-nil when both GAME players have played out their cards."
  (and (null (card-games-crib--play game 0)) (null (card-games-crib--play game 1))))

(defun card-games-crib--peg-go (game s)
  "Handle GAME seat S being unable to play (a go)."
  (let ((other (- 1 s)))
    (if (card-games-crib--legal game other)
        (card-games-put game :pturn other)          ; opponent plays on
      ;; neither can play: last player pegs one for the go, then reset
      (when (card-games-get game :last-player)
        (card-games-crib--add game (card-games-get game :last-player) 1)
        (card-games-put game :message
                (format "%s pegs 1 for the go." (card-games-crib--who (card-games-get game :last-player)))))
      (card-games-crib--peg-reset game))))

(cl-defmethod card-games-crib--ai-play ((game card-games-cribbage-game) s)
  "Have GAME AI seat S play its best pegging card or declare a go."
  (let ((legal (card-games-crib--legal game s)))
    (if (null legal) (card-games-crib--peg-go game s)
      (let ((best (car legal)) (bestv -1))
        (dolist (c legal)
          (let* ((seq (cons c (card-games-get game :seq)))
                 (tot (+ (card-games-get game :total) (card-games-crib--val c)))
                 (v (card-games-crib--peg-score seq tot)))
            ;; prefer points; tie-break toward keeping count off 5 and 21
            (when (or (> v bestv)
                      (and (= v bestv) (> (card-games-crib--val c) (card-games-crib--val best))))
              (setq best c bestv v))))
        (card-games-crib--peg-play game s best)))))

(defun card-games-crib--peg-advance (game)
  "Run GAME's AI pegging until your turn or the round ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (not (card-games-crib--peg-over-p game))
                (/= (card-games-get game :pturn) 0) (< guard 200))
      (setq guard (1+ guard))
      (card-games-crib--ai-play game (card-games-get game :pturn))))
  ;; if it is your turn but you have no legal play, auto-go
  (when (and (eq (card-games-get game :phase) 'play) (not (card-games-crib--peg-over-p game))
             (= (card-games-get game :pturn) 0) (null (card-games-crib--legal game 0)))
    (card-games-crib--peg-go game 0)
    (card-games-crib--peg-advance game))
  (when (and (eq (card-games-get game :phase) 'play) (card-games-crib--peg-over-p game))
    (card-games-crib--show game)))

(cl-defmethod card-games-crib--show ((game card-games-cribbage-game))
  "Count GAME's hands and crib, then set up the next deal."
  (let* ((starter (card-games-get game :starter))
         (dealer (card-games-get game :dealer)) (pone (- 1 dealer))
         (h-pone (card-games-crib--hand game pone)) (h-dealer (card-games-crib--hand game dealer))
         (crib (card-games-get game :crib))
         (s-pone (card-games-crib--score-show h-pone starter))
         (s-dealer (card-games-crib--score-show h-dealer starter))
         (s-crib (card-games-crib--score-show crib starter t)))
    ;; count in order: non-dealer, dealer, crib (a player may win mid-count)
    (card-games-crib--add game pone s-pone)
    (when (not (eq (card-games-get game :phase) 'game-over)) (card-games-crib--add game dealer s-dealer))
    (when (not (eq (card-games-get game :phase) 'game-over)) (card-games-crib--add game dealer s-crib))
    (card-games-put game :show (list :pone s-pone :dealer s-dealer :crib s-crib))
    (unless (eq (card-games-get game :phase) 'game-over) (card-games-put game :phase 'show))
    (card-games-put game :message
            (format "Show: %s %d, %s %d, crib %d.  %s"
                    (card-games-crib--who pone) s-pone (card-games-crib--who dealer) s-dealer s-crib
                    (if (eq (card-games-get game :phase) 'game-over)
                        (format "%s wins!  (n: new game)" (card-games-crib--who (card-games-get game :winner)))
                      "(n: next deal)")))))

;;;; UI

(defvar-local card-games-crib--game nil "The Cribbage game in the current buffer.")

(defun card-games-crib--svg (game)
  "Return an SVG board for the Cribbage GAME (with a peg-track)."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap) (pad 16)
         (phase (card-games-get game :phase)) (scores (card-games-get game :scores))
         (hand (if (eq phase 'play) (card-games-crib--play game 0) (card-games-crib--hand game 0)))
         (n (length hand)) (cursor (card-games-get game :cursor)) (marks (card-games-get game :marks))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (target card-games-cribbage-target) (barw 220) (peg-h 14) (peg-gap 8)
         (y-title 6) (y-peg 26)
         (y-mid (+ y-peg (* 2 (+ peg-h peg-gap)) 18))
         (y-hand (+ y-mid h 44))
         (height (+ y-hand h 30))
         (width (max (+ fanw (* 2 pad)) (+ pad 90 barw 120) 620))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family card-games-svg-font-family (and bold '(:font-weight "bold"))))
                (card-games-crib--peg (label sc y)
                     (txt label pad (+ y 11) 12)
                     (let ((bx (+ pad 90)))
                       (svg-rectangle svg bx y barw peg-h :rx 4 :fill "none"
                                      :stroke lc :stroke-width 1)
                       (svg-rectangle svg bx y
                                      (round (* barw (/ (float (min sc target)) target)))
                                      peg-h :rx 4 :fill "#3aa15a")
                       (txt (format "%d" sc) (+ bx barw 8) (+ y 11) 12)))
                (crow (cards x y)
                      (let ((xx x))
                        (dolist (c cards)
                          (let ((sp (card-games-rummy--card-spec c)))
                            (card-games-svg-card svg xx y :rank (car sp) :suit (cdr sp)))
                          (setq xx (+ xx (round (* w 0.5))))))))
      (txt (format "Cribbage   (to %d)" target) pad (+ y-title 12) 13 t)
      (card-games-crib--peg "You" (aref scores 0) y-peg)
      (card-games-crib--peg "Computer" (aref scores 1) (+ y-peg peg-h peg-gap))
      (txt (format "%s deals" (card-games-crib--who (card-games-get game :dealer)))
           (+ pad 90 barw 60) (+ y-peg 11) 11)
      (let ((mx pad))
        (when (card-games-get game :starter)
          (let ((sp (card-games-rummy--card-spec (card-games-get game :starter))))
            (card-games-svg-card svg mx y-mid :rank (car sp) :suit (cdr sp))
            (txt "Starter" mx (+ y-mid h 13) 11)
            (setq mx (+ mx w gap 24))))
        (cond
         ((eq phase 'play)
          (txt (format "Count: %d" (card-games-get game :total)) mx (- y-mid 4) 12)
          (crow (reverse (card-games-get game :seq)) mx y-mid))
         ((memq phase '(show game-over))
          (when (card-games-get game :crib)
            (txt (format "Crib (%s)" (card-games-crib--who (card-games-get game :dealer))) mx (- y-mid 4) 11)
            (crow (card-games-get game :crib) mx y-mid)))))
      (txt (format "Your %s" (if (eq phase 'play) "cards" "hand")) pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (card-games-rummy--card-spec c)) (curp (= i cursor))
                (markp (and marks (memq i marks))))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp) :highlight curp)
            (when markp
              (svg-rectangle svg (- x 3) (- y-hand 3) (+ w 6) (+ h 6)
                             :rx 8 :fill "none" :stroke "#4a90d9" :stroke-width 3))
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions (nreverse regions))))

(cl-defmethod card-games-render ((game card-games-cribbage-game))
  "Return a depiction of the Cribbage GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-crib--svg game)
    (card-games-crib--render-text game)))

(defun card-games-crib--render-text (game)
  "Return a plain-text depiction of the Cribbage GAME."
  (let* ((out '()) (scores (card-games-get game :scores)) (phase (card-games-get game :phase))
         (cursor (card-games-get game :cursor)))
    (push (format "  Cribbage      to %d\n\n" card-games-cribbage-target) out)
    (push (format "  You %d        Computer %d        %s deals\n\n"
                  (aref scores 0) (aref scores 1) (card-games-crib--who (card-games-get game :dealer)))
          out)
    (when (card-games-get game :starter)
      (push (format "  Starter: %s\n" (card-games-rummy-card-string (card-games-get game :starter))) out))
    (when (eq phase 'play)
      (push (format "  Count: %d\n  Played: %s\n" (card-games-get game :total)
                    (mapconcat #'card-games-rummy-card-string (reverse (card-games-get game :seq)) " "))
            out))
    (when (memq phase '(show game-over))
      (let ((sh (card-games-get game :show)))
        (when sh
          (push (format "  Crib (%s): %s\n"
                        (card-games-crib--who (card-games-get game :dealer))
                        (mapconcat #'card-games-rummy-card-string (card-games-get game :crib) " ")) out))))
    (let* ((hand (if (eq phase 'play) (card-games-crib--play game 0) (card-games-crib--hand game 0))))
      (push (format "\n  Your %s:\n   " (if (eq phase 'play) "cards" "hand")) out)
      (push (card-games-rummy--render-cards hand cursor (card-games-get game :marks) nil 'hand) out))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-cribbage-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i))
    (_ (cl-call-next-method))))

(defun card-games-crib--redisplay ()
  "Redraw the current Cribbage buffer."
  (let ((game card-games-crib--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-crib--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-crib--cur-list (g)
  "Return seat 0's current card list in G (pegging plays or hand)."
  (if (eq (card-games-get g :phase) 'play) (card-games-crib--play g 0) (card-games-crib--hand g 0)))

(defun card-games-crib-left ()
  "Move the cursor left."
  (interactive)
  (let* ((g card-games-crib--game) (n (length (card-games-crib--cur-list g))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-crib--redisplay)))

(defun card-games-crib-right ()
  "Move the cursor right."
  (interactive)
  (let* ((g card-games-crib--game) (n (length (card-games-crib--cur-list g))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-crib--redisplay)))

(defun card-games-crib-mark ()
  "Toggle a discard mark on the cursor card (discard phase)."
  (interactive)
  (let* ((g card-games-crib--game) (i (card-games-get g :cursor)) (marks (card-games-get g :marks)))
    (when (eq (card-games-get g :phase) 'discard)
      (card-games-put g :marks (if (memq i marks) (delq i marks)
                         (if (>= (length marks) 2) marks (cons i marks)))))
    (card-games-crib--redisplay)))

(defun card-games-crib-confirm ()
  "Confirm your two crib discards and start play."
  (interactive)
  (let* ((g card-games-crib--game) (hand (card-games-crib--hand g 0))
         (marks (card-games-get g :marks)))
    (if (or (not (eq (card-games-get g :phase) 'discard)) (/= (length marks) 2))
        (progn (card-games-put g :message "Mark exactly two cards (SPC), then m.")
               (card-games-crib--redisplay))
      (let ((mine (mapcar (lambda (i) (nth i hand)) marks))
            (ai (card-games-crib--ai-discard g 1)))
        (card-games-crib--set-hand g 0 (cl-set-difference hand mine :test #'equal))
        (card-games-crib--set-hand g 1 (cl-set-difference (card-games-crib--hand g 1) ai :test #'equal))
        (card-games-put g :crib (append mine ai))
        (card-games-put g :marks nil) (card-games-put g :cursor 0)
        (card-games-crib--start-play g)
        (card-games-crib--peg-advance g)
        (card-games-crib--redisplay)))))

(defun card-games-crib-play ()
  "Play the cursor card in pegging, or declare a go if you cannot."
  (interactive)
  (let* ((g card-games-crib--game))
    (cond
     ((not (eq (card-games-get g :phase) 'play)) (card-games-put g :message "Not the pegging round."))
     ((/= (card-games-get g :pturn) 0) (card-games-put g :message "Not your turn."))
     ((null (card-games-crib--legal g 0))
      (card-games-crib--peg-go g 0) (card-games-crib--peg-advance g))
     (t (let ((card (nth (card-games-get g :cursor) (card-games-crib--play g 0))))
          (if (or (null card) (> (card-games-crib--val card) (- 31 (card-games-get g :total))))
              (card-games-put g :message "That card would go over 31 -- choose another.")
            (card-games-crib--peg-play g 0 card)
            (card-games-put g :cursor 0)
            (card-games-crib--peg-advance g)))))
    (card-games-crib--redisplay)))

(defun card-games-crib-new ()
  "Start the next deal, or a new game when one is over."
  (interactive)
  (let ((g card-games-crib--game))
    (when (eq (card-games-get g :phase) 'game-over)
      (card-games-put g :scores (make-vector 2 0)) (card-games-put g :dealer 1))
    (card-games-put g :dealer (- 1 (card-games-get g :dealer)))   ; alternate the deal
    (card-games-put g :show nil)
    (card-games-crib--deal g)
    (card-games-crib--redisplay)))

(defun card-games-crib-redraw () "Redraw." (interactive) (card-games-crib--redisplay))
(defun card-games-crib-help () "Describe the controls." (interactive)
       (message "Arrows: choose  SPC: mark (discard)  m: confirm crib  RET: play/go  n: next  g: redraw"))

(defvar card-games-cribbage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-crib-left)
    (define-key map (kbd "<right>") #'card-games-crib-right)
    (define-key map (kbd "SPC") #'card-games-crib-mark)
    (define-key map "m" #'card-games-crib-confirm)
    (define-key map (kbd "RET") #'card-games-crib-play)
    (define-key map "n" #'card-games-crib-new)
    (define-key map "g" #'card-games-crib-redraw)
    (define-key map "?" #'card-games-crib-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-cribbage-mode'.")

(define-derived-mode card-games-cribbage-mode special-mode "Cribbage"
  "Major mode for Cribbage."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-cribbage ()
  "Play two-handed Cribbage against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Cribbage*")))
    (with-current-buffer buf
      (card-games-cribbage-mode)
      (setq card-games-crib--game (card-games-cribbage-game))
      (card-games-crib--deal card-games-crib--game)
      (card-games-crib--redisplay))
    (switch-to-buffer buf)))

(provide 'card-games-cribbage)
;;; card-games-cribbage.el ends here
