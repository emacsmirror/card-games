;;; cg-cribbage.el --- Cribbage, with pegging and the show  -*- lexical-binding: t; -*-

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
(require 'cg-core)
(require 'cg-rummy)

(defcustom cg-cribbage-target 121
  "Points needed to win a game of Cribbage."
  :type 'integer :group 'card-games)

(defclass cg-cribbage-game (cg-game)
  ((vname :initform "Cribbage"))
  "A two-handed game of Cribbage.")

;;;; Scoring primitives

(defun cg-crib--val (card)
  "Return CARD's counting value (Ace 1, faces 10, else pip)."
  (let ((r (cdr card))) (if (<= r 8) (1+ r) 10)))

(defun cg-crib--count-15s (cards)
  "Return points for all subsets of CARDS summing to fifteen."
  (let ((n (length cards)) (vec (vconcat cards)) (count 0))
    (dotimes (mask (ash 1 n))
      (let ((sum 0))
        (dotimes (i n)
          (when (/= 0 (logand mask (ash 1 i)))
            (setq sum (+ sum (cg-crib--val (aref vec i))))))
        (when (= sum 15) (setq count (+ count 2)))))
    count))

(defun cg-crib--count-pairs (cards)
  "Return points for all pairs in CARDS."
  (let ((cnt (make-vector 13 0)) (tot 0))
    (dolist (c cards) (aset cnt (cdr c) (1+ (aref cnt (cdr c)))))
    (dotimes (r 13)
      (let ((k (aref cnt r))) (setq tot (+ tot (* k (1- k))))))  ; 2*C(k,2)=k*(k-1)
    tot))

(defun cg-crib--count-runs (cards)
  "Return points for all runs of three or more in CARDS (with multiplicity)."
  (let ((cnt (make-vector 13 0)) (total 0) (r 0))
    (dolist (c cards) (aset cnt (cdr c) (1+ (aref cnt (cdr c)))))
    (while (< r 13)
      (if (= 0 (aref cnt r)) (setq r (1+ r))
        (let ((len 0) (mult 1))
          (while (and (< r 13) (> (aref cnt r) 0))
            (setq len (1+ len) mult (* mult (aref cnt r)) r (1+ r)))
          (when (>= len 3) (setq total (+ total (* len mult)))))))
    total))

(defun cg-crib--flush (hand starter is-crib)
  "Return flush points for the four-card HAND with STARTER.
A crib (IS-CRIB) flush must include the starter."
  (let ((s (car (car hand))))
    (cond ((not (cl-every (lambda (c) (= (car c) s)) hand)) 0)
          ((= (car starter) s) 5)
          (is-crib 0)
          (t 4))))

(defun cg-crib--nobs (hand starter)
  "Return 1 when HAND holds the Jack of the STARTER's suit, else 0."
  (if (cl-find-if (lambda (c) (and (= (cdr c) 10) (= (car c) (car starter)))) hand)
      1 0))

(defun cg-crib--score-show (hand starter &optional is-crib)
  "Return the show score of the four-card HAND with STARTER.
IS-CRIB applies the stricter crib flush rule."
  (let ((all (cons starter hand)))
    (+ (cg-crib--count-15s all)
       (cg-crib--count-pairs all)
       (cg-crib--count-runs all)
       (cg-crib--flush hand starter is-crib)
       (cg-crib--nobs hand starter))))

(defun cg-crib--peg-score (seq total)
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

(defsubst cg-crib--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-crib--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-crib--play (game s) (aref (cg-get game :play) s))
(defsubst cg-crib--set-play (game s v) (aset (cg-get game :play) s v))

(defun cg-crib--who (s) (if (= s 0) "You" "Computer"))

(cl-defmethod cg-crib--deal ((game cg-cribbage-game))
  "Deal a fresh Cribbage hand into GAME."
  (let ((deck (cg-rummy-deck)) (hands (make-vector 2 nil)))
    (dotimes (s 2)
      (aset hands s (cg-rummy-sort-hand (cl-loop repeat 6 collect (pop deck)))))
    (cg-put game :hands hands)
    (cg-put game :deck deck)
    (cg-put game :crib nil)
    (cg-put game :starter nil)
    (cg-put game :phase 'discard)
    (cg-put game :cursor 0)
    (cg-put game :marks nil)
    (unless (cg-get game :scores) (cg-put game :scores (make-vector 2 0)))
    (unless (integerp (cg-get game :dealer)) (cg-put game :dealer 1))
    (cg-put game :message "Discard two cards to the crib: SPC marks, m confirms.")
    game))

(defun cg-crib--add (game s pts)
  "Add PTS to seat S and end the game if it reaches the target."
  (when (> pts 0)
    (aset (cg-get game :scores) s (+ (aref (cg-get game :scores) s) pts))
    (when (>= (aref (cg-get game :scores) s) cg-cribbage-target)
      (cg-put game :phase 'game-over)
      (cg-put game :winner s))))

(defun cg-crib--ai-discard (game s)
  "Return the two cards seat S should lay away (keep the best four)."
  (let* ((hand (cg-crib--hand game s)) (best nil) (bestv -1))
    (dolist (combo (cg-rummy--combinations hand 4))
      (let ((v (cg-crib--score-show combo '(0 . 0))))   ; rough: no starter
        (when (> v bestv) (setq bestv v best combo))))
    (cl-set-difference hand best :test #'equal)))

(cl-defmethod cg-crib--start-play ((game cg-cribbage-game))
  "Cut the starter and begin the pegging round."
  (let* ((deck (cg-get game :deck))
         (starter (nth (random (length deck)) deck))
         (dealer (cg-get game :dealer)))
    (cg-put game :starter starter)
    (when (= (cdr starter) 10)            ; his heels: starter is a Jack
      (cg-crib--add game dealer 2))
    (cg-put game :play (vector (cg-crib--hand game 0) (cg-crib--hand game 1)))
    (cg-put game :seq nil)
    (cg-put game :total 0)
    (cg-put game :go nil)
    (cg-put game :last-player nil)
    (cg-put game :pturn (- 1 dealer))     ; non-dealer leads
    (cg-put game :phase (if (eq (cg-get game :phase) 'game-over) 'game-over 'play))
    (cg-put game :cursor 0)
    (cg-put game :message
            (format "Pegging: %s leads.  Starter is %s."
                    (cg-crib--who (- 1 dealer)) (cg-rummy-card-string starter)))))

(defun cg-crib--legal (game s)
  "Return seat S's play-cards that fit under 31."
  (cl-remove-if (lambda (c) (> (cg-crib--val c) (- 31 (cg-get game :total))))
                (cg-crib--play game s)))

(defun cg-crib--peg-play (game s card)
  "Seat S plays CARD into the pegging round and pegs any points."
  (cg-crib--set-play game s (cl-remove card (cg-crib--play game s) :test #'equal :count 1))
  (cg-put game :seq (cons card (cg-get game :seq)))
  (cg-put game :total (+ (cg-get game :total) (cg-crib--val card)))
  (cg-put game :last-player s)
  (cg-put game :go nil)
  (let ((pts (cg-crib--peg-score (cg-get game :seq) (cg-get game :total))))
    (cg-crib--add game s pts)
    (cg-put game :message
            (format "%s played %s (count %d)%s."
                    (cg-crib--who s) (cg-rummy-card-string card) (cg-get game :total)
                    (if (> pts 0) (format " for %d" pts) ""))))
  (if (= (cg-get game :total) 31)
      (cg-crib--peg-reset game)
    (cg-put game :pturn (- 1 s))))

(defun cg-crib--peg-reset (game)
  "Reset the running count; the player after the last to play leads."
  (cg-put game :seq nil)
  (cg-put game :total 0)
  (cg-put game :go nil)
  (cg-put game :pturn (- 1 (cg-get game :last-player))))

(defun cg-crib--peg-over-p (game)
  "Return non-nil when both players have played out their cards."
  (and (null (cg-crib--play game 0)) (null (cg-crib--play game 1))))

(defun cg-crib--peg-go (game s)
  "Handle seat S being unable to play (a go)."
  (let ((other (- 1 s)))
    (if (cg-crib--legal game other)
        (cg-put game :pturn other)          ; opponent plays on
      ;; neither can play: last player pegs one for the go, then reset
      (when (cg-get game :last-player)
        (cg-crib--add game (cg-get game :last-player) 1)
        (cg-put game :message
                (format "%s pegs 1 for the go." (cg-crib--who (cg-get game :last-player)))))
      (cg-crib--peg-reset game))))

(cl-defmethod cg-crib--ai-play ((game cg-cribbage-game) s)
  "Have AI seat S either play its best pegging card or declare a go."
  (let ((legal (cg-crib--legal game s)))
    (if (null legal) (cg-crib--peg-go game s)
      (let ((best (car legal)) (bestv -1))
        (dolist (c legal)
          (let* ((seq (cons c (cg-get game :seq)))
                 (tot (+ (cg-get game :total) (cg-crib--val c)))
                 (v (cg-crib--peg-score seq tot)))
            ;; prefer points; tie-break toward keeping count off 5 and 21
            (when (or (> v bestv)
                      (and (= v bestv) (> (cg-crib--val c) (cg-crib--val best))))
              (setq best c bestv v))))
        (cg-crib--peg-play game s best)))))

(defun cg-crib--peg-advance (game)
  "Run AI pegging turns until it is your turn or the round ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (not (cg-crib--peg-over-p game))
                (/= (cg-get game :pturn) 0) (< guard 200))
      (setq guard (1+ guard))
      (cg-crib--ai-play game (cg-get game :pturn))))
  ;; if it is your turn but you have no legal play, auto-go
  (when (and (eq (cg-get game :phase) 'play) (not (cg-crib--peg-over-p game))
             (= (cg-get game :pturn) 0) (null (cg-crib--legal game 0)))
    (cg-crib--peg-go game 0)
    (cg-crib--peg-advance game))
  (when (and (eq (cg-get game :phase) 'play) (cg-crib--peg-over-p game))
    (cg-crib--show game)))

(cl-defmethod cg-crib--show ((game cg-cribbage-game))
  "Count the hands and the crib, then set up the next deal."
  (let* ((starter (cg-get game :starter))
         (dealer (cg-get game :dealer)) (pone (- 1 dealer))
         (h-pone (cg-crib--hand game pone)) (h-dealer (cg-crib--hand game dealer))
         (crib (cg-get game :crib))
         (s-pone (cg-crib--score-show h-pone starter))
         (s-dealer (cg-crib--score-show h-dealer starter))
         (s-crib (cg-crib--score-show crib starter t)))
    ;; count in order: non-dealer, dealer, crib (a player may win mid-count)
    (cg-crib--add game pone s-pone)
    (when (not (eq (cg-get game :phase) 'game-over)) (cg-crib--add game dealer s-dealer))
    (when (not (eq (cg-get game :phase) 'game-over)) (cg-crib--add game dealer s-crib))
    (cg-put game :show (list :pone s-pone :dealer s-dealer :crib s-crib))
    (unless (eq (cg-get game :phase) 'game-over) (cg-put game :phase 'show))
    (cg-put game :message
            (format "Show: %s %d, %s %d, crib %d.  %s"
                    (cg-crib--who pone) s-pone (cg-crib--who dealer) s-dealer s-crib
                    (if (eq (cg-get game :phase) 'game-over)
                        (format "%s wins!  (n: new game)" (cg-crib--who (cg-get game :winner)))
                      "(n: next deal)")))))

;;;; UI

(defvar-local cg-crib--game nil "The Cribbage game in the current buffer.")

(cl-defmethod cg-render ((game cg-cribbage-game))
  "Return a propertized depiction of the Cribbage GAME."
  (let* ((out '()) (scores (cg-get game :scores)) (phase (cg-get game :phase))
         (cursor (cg-get game :cursor)))
    (push (format "  Cribbage      to %d\n\n" cg-cribbage-target) out)
    (push (format "  You %d        Computer %d        %s deals\n\n"
                  (aref scores 0) (aref scores 1) (cg-crib--who (cg-get game :dealer)))
          out)
    (when (cg-get game :starter)
      (push (format "  Starter: %s\n" (cg-rummy-card-string (cg-get game :starter))) out))
    (when (eq phase 'play)
      (push (format "  Count: %d\n  Played: %s\n" (cg-get game :total)
                    (mapconcat #'cg-rummy-card-string (reverse (cg-get game :seq)) " "))
            out))
    (when (memq phase '(show game-over))
      (let ((sh (cg-get game :show)))
        (when sh
          (push (format "  Crib (%s): %s\n"
                        (cg-crib--who (cg-get game :dealer))
                        (mapconcat #'cg-rummy-card-string (cg-get game :crib) " ")) out))))
    (let* ((hand (if (eq phase 'play) (cg-crib--play game 0) (cg-crib--hand game 0))))
      (push (format "\n  Your %s:\n   " (if (eq phase 'play) "cards" "hand")) out)
      (push (cg-rummy--render-cards hand cursor (cg-get game :marks) nil 'hand) out))
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-cribbage-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i))
    (_ (cl-call-next-method))))

(defun cg-crib--redisplay ()
  (let ((game cg-crib--game) (inhibit-read-only t))
        (setq cg-current-game game cg-redisplay-function #'cg-crib--redisplay)
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-crib--cur-list (g)
  (if (eq (cg-get g :phase) 'play) (cg-crib--play g 0) (cg-crib--hand g 0)))

(defun cg-crib-left ()
  "Move the cursor left."
  (interactive)
  (let* ((g cg-crib--game) (n (length (cg-crib--cur-list g))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-crib--redisplay)))

(defun cg-crib-right ()
  "Move the cursor right."
  (interactive)
  (let* ((g cg-crib--game) (n (length (cg-crib--cur-list g))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-crib--redisplay)))

(defun cg-crib-mark ()
  "Toggle a discard mark on the cursor card (discard phase)."
  (interactive)
  (let* ((g cg-crib--game) (i (cg-get g :cursor)) (marks (cg-get g :marks)))
    (when (eq (cg-get g :phase) 'discard)
      (cg-put g :marks (if (memq i marks) (delq i marks)
                         (if (>= (length marks) 2) marks (cons i marks)))))
    (cg-crib--redisplay)))

(defun cg-crib-confirm ()
  "Confirm your two crib discards and start play."
  (interactive)
  (let* ((g cg-crib--game) (hand (cg-crib--hand g 0))
         (marks (cg-get g :marks)))
    (if (or (not (eq (cg-get g :phase) 'discard)) (/= (length marks) 2))
        (progn (cg-put g :message "Mark exactly two cards (SPC), then m.")
               (cg-crib--redisplay))
      (let ((mine (mapcar (lambda (i) (nth i hand)) marks))
            (ai (cg-crib--ai-discard g 1)))
        (cg-crib--set-hand g 0 (cl-set-difference hand mine :test #'equal))
        (cg-crib--set-hand g 1 (cl-set-difference (cg-crib--hand g 1) ai :test #'equal))
        (cg-put g :crib (append mine ai))
        (cg-put g :marks nil) (cg-put g :cursor 0)
        (cg-crib--start-play g)
        (cg-crib--peg-advance g)
        (cg-crib--redisplay)))))

(defun cg-crib-play ()
  "Play the cursor card in pegging, or declare a go if you cannot."
  (interactive)
  (let* ((g cg-crib--game))
    (cond
     ((not (eq (cg-get g :phase) 'play)) (cg-put g :message "Not the pegging round."))
     ((/= (cg-get g :pturn) 0) (cg-put g :message "Not your turn."))
     ((null (cg-crib--legal g 0))
      (cg-crib--peg-go g 0) (cg-crib--peg-advance g))
     (t (let ((card (nth (cg-get g :cursor) (cg-crib--play g 0))))
          (if (or (null card) (> (cg-crib--val card) (- 31 (cg-get g :total))))
              (cg-put g :message "That card would go over 31 -- choose another.")
            (cg-crib--peg-play g 0 card)
            (cg-put g :cursor 0)
            (cg-crib--peg-advance g)))))
    (cg-crib--redisplay)))

(defun cg-crib-new ()
  "Start the next deal, or a new game when one is over."
  (interactive)
  (let ((g cg-crib--game))
    (when (eq (cg-get g :phase) 'game-over)
      (cg-put g :scores (make-vector 2 0)) (cg-put g :dealer 1))
    (cg-put g :dealer (- 1 (cg-get g :dealer)))   ; alternate the deal
    (cg-put g :show nil)
    (cg-crib--deal g)
    (cg-crib--redisplay)))

(defun cg-crib-redraw () "Redraw." (interactive) (cg-crib--redisplay))
(defun cg-crib-help () "Describe the controls." (interactive)
       (message "Arrows: choose  SPC: mark (discard)  m: confirm crib  RET: play/go  n: next  g: redraw"))

(defvar cg-cribbage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-crib-left)
    (define-key map (kbd "<right>") #'cg-crib-right)
    (define-key map (kbd "SPC") #'cg-crib-mark)
    (define-key map "m" #'cg-crib-confirm)
    (define-key map (kbd "RET") #'cg-crib-play)
    (define-key map "n" #'cg-crib-new)
    (define-key map "g" #'cg-crib-redraw)
    (define-key map "?" #'cg-crib-help)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-cribbage-mode'.")

(define-derived-mode cg-cribbage-mode special-mode "Cribbage"
  "Major mode for Cribbage."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-cribbage ()
  "Play two-handed Cribbage against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Cribbage*")))
    (with-current-buffer buf
      (cg-cribbage-mode)
      (setq cg-crib--game (cg-cribbage-game))
      (cg-crib--deal cg-crib--game)
      (cg-crib--redisplay))
    (switch-to-buffer buf)))

(provide 'cg-cribbage)
;;; cg-cribbage.el ends here
