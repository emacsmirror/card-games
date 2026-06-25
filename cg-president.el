;;; cg-president.el --- President (Scum), a climbing card game  -*- lexical-binding: t; -*-

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

;; President (also Scum, Asshole, Daihinmin): a climbing/shedding game.
;; The leader plays one to four cards of a single rank; each player in turn
;; must beat it with the same number of a higher rank or pass.  Once all but
;; one have passed, the pile clears and the last player to play leads again.
;; The first player out is President, the last is Scum; on the next deal the
;; Scum hands the President their two best cards and gets two junk cards back.
;;
;; You are the South player (seat 0); the rest are simple AI.  Card power
;; runs 3 (low) .. K, A, then the Two (highest).  Cards are the package
;; cons (SUIT . RANK) with RANK 0 (the Two) .. 12 (the Ace).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)

(defconst cg-pres-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst cg-pres-titles ["President" "Vice-President" "Citizen"
                          "Vice-Scum" "Scum"]
  "Finishing titles from first out to last.")

(defcustom cg-president-players 4
  "Number of players in President, including you (3-6)."
  :type 'integer :group 'card-games)

(defun cg-pres--power (rank)
  "Return the climbing power of RANK; the Two (RANK 0) is highest."
  (if (= rank 0) 13 rank))

(defun cg-pres-card-string (card)
  (if (null card) "·"
    (concat (aref cg-pres-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defsubst cg-pres-red-p (card) (and card (cg-red-suit-p (car card))))

(defun cg-pres--deck ()
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defun cg-pres--sort (cards)
  "Sort CARDS by climbing power then suit."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (cg-pres--power (cdr a)) (cg-pres--power (cdr b)))
                          (< (car a) (car b))
                        (< (cg-pres--power (cdr a)) (cg-pres--power (cdr b)))))))

(defclass cg-president-game (cg-game)
  ((vname :initform "President"))
  "A game of President (Scum).")

(defsubst cg-pres--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-pres--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-pres--name (_game s)
  (if (= s 0) "You" (format "Player %d" s)))

;;;; Dealing and the inter-game exchange

(cl-defmethod cg-pres--deal ((game cg-president-game))
  (let* ((n (max 3 (min 6 cg-president-players)))
         (deck (cg-pres--deck))
         (hands (make-vector n nil))
         (s 0))
    (while deck
      (push (pop deck) (aref hands (mod s n)))
      (cl-incf s))
    (dotimes (i n) (aset hands i (cg-pres--sort (aref hands i))))
    (cg-put game :hands hands)
    (cg-put game :nplayers n)
    (cg-pres--exchange game)            ; carry out roles from the last deal
    (cg-put game :count 0)
    (cg-put game :top -1)
    (cg-put game :passed (make-vector n nil))
    (cg-put game :out nil)
    (cg-put game :last-player nil)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (unless (cg-get game :games) (cg-put game :games 0))
    (cg-put game :message
            "Lead any rank; others beat it with a higher one or pass.  p passes.")
    game))

(defun cg-pres--best (hand k) "The K highest-power cards of HAND." (last (cg-pres--sort hand) k))
(defun cg-pres--worst (hand k) "The K lowest-power cards of HAND." (cl-subseq (cg-pres--sort hand) 0 k))

(cl-defmethod cg-pres--exchange ((game cg-president-game))
  "Trade cards by rank from the previous deal's finishing order, if any."
  (let ((order (cg-get game :order)) (n (cg-get game :nplayers)))
    (when (and order (= (length order) n) (>= n 4))
      (let* ((prez (nth 0 order)) (scum (nth (1- n) order))
             (vp (nth 1 order)) (vice (nth (- n 2) order)))
        (cg-pres--give game scum prez 2)   ; scum's 2 best -> president
        (cg-pres--give game prez scum 2 t) ; president's 2 worst -> scum
        (cg-pres--give game vice vp 1)
        (cg-pres--give game vp vice 1 t)))))

(defun cg-pres--give (game from to k &optional worst)
  "Move K cards (best, or WORST) from seat FROM to seat TO."
  (let* ((cards (if worst (cg-pres--worst (cg-pres--hand game from) k)
                  (cg-pres--best (cg-pres--hand game from) k))))
    (cg-pres--set-hand game from
                       (cl-set-difference (cg-pres--hand game from) cards :test #'equal))
    (cg-pres--set-hand game to
                       (cg-pres--sort (append (cg-pres--hand game to) cards)))))

;;;; Move logic

(defun cg-pres--rank-counts (game s)
  "Return an alist (RANK . COUNT) for seat S's hand."
  (let ((tbl nil))
    (dolist (c (cg-pres--hand game s))
      (setf (alist-get (cdr c) tbl 0) (1+ (alist-get (cdr c) tbl 0))))
    tbl))

(defun cg-pres--legal-ranks (game s)
  "Return the ranks seat S may legally play now."
  (let ((cnt (cg-get game :count)) (top (cg-get game :top)))
    (cl-loop for (r . c) in (cg-pres--rank-counts game s)
             when (if (= cnt 0) t (and (>= c cnt) (> (cg-pres--power r) top)))
             collect r)))

(defun cg-pres--remove-n (hand rank n)
  "Remove N cards of RANK from HAND."
  (let ((out nil) (left n))
    (dolist (c hand) (if (and (> left 0) (= (cdr c) rank))
                         (cl-decf left) (push c out)))
    (nreverse out)))

(defun cg-pres--in-game (game)
  "Seats that still hold cards."
  (cl-loop for s below (cg-get game :nplayers)
           unless (memq s (cg-get game :out)) collect s))

(defun cg-pres--round-active (game)
  "Seats that can still act on the current pile."
  (cl-loop for s below (cg-get game :nplayers)
           unless (or (memq s (cg-get game :out)) (aref (cg-get game :passed) s))
           collect s))

(defun cg-pres--next (game from)
  "Next seat after FROM that is still in the round."
  (let ((n (cg-get game :nplayers)) (s from) (res nil))
    (dotimes (_ n)
      (setq s (mod (1+ s) n))
      (when (and (not res)
                 (not (memq s (cg-get game :out)))
                 (not (aref (cg-get game :passed) s)))
        (setq res s)))
    (or res from)))

(defun cg-pres--clear (game)
  "Clear the pile; the last player to play leads (or the next active seat)."
  (cg-put game :count 0) (cg-put game :top -1)
  (cg-put game :passed (make-vector (cg-get game :nplayers) nil))
  (let ((last (cg-get game :last-player)))
    (cg-put game :turn (if (and last (not (memq last (cg-get game :out)))) last
                         (cg-pres--next game (or last 0)))))
  (cg-put game :message "Pile cleared."))

(defun cg-pres--check-finish (game)
  "End the game when only one player still holds cards (the Scum)."
  (let ((in (cg-pres--in-game game)))
    (when (<= (length in) 1)
      (when in (cg-put game :out (append (cg-get game :out) in)))
      (cg-put game :order (cg-get game :out))
      (cg-put game :games (1+ (or (cg-get game :games) 0)))
      (cg-put game :phase 'game-over)
      (cg-put game :message (cg-pres--result game))
      t)))

(defun cg-pres--advance (game)
  "Decide the next turn or clear the pile after a move."
  (unless (cg-pres--check-finish game)
    (let* ((active (cg-pres--round-active game))
           (last (cg-get game :last-player))
           (others (and last (cl-remove last active))))
      (if (and (> (cg-get game :count) 0) (null others))
          (cg-pres--clear game)
        (cg-put game :turn (cg-pres--next game (cg-get game :turn)))))))

(defun cg-pres--play (game seat rank n)
  "Seat SEAT plays N cards of RANK."
  (cg-pres--set-hand game seat (cg-pres--remove-n (cg-pres--hand game seat) rank n))
  (cg-put game :count n) (cg-put game :top (cg-pres--power rank))
  (cg-put game :last-player seat)
  (when (null (cg-pres--hand game seat))
    (cg-put game :out (append (cg-get game :out) (list seat))))
  (cg-put game :message (format "%s plays %d × %s" (cg-pres--name game seat)
                                n (aref cg-pres-ranks rank)))
  (cg-pres--advance game))

(defun cg-pres--pass (game seat)
  "Seat SEAT passes for the current pile."
  (aset (cg-get game :passed) seat t)
  (cg-put game :message (format "%s passes." (cg-pres--name game seat)))
  (cg-pres--advance game))

(defun cg-pres--ai-move (game seat)
  "Make seat SEAT's move: lead low, beat low, or pass."
  (let* ((cnt (cg-get game :count)) (top (cg-get game :top))
         (counts (cg-pres--rank-counts game seat)))
    (if (= cnt 0)
        (let ((r (caar (cl-sort counts #'< :key (lambda (x) (cg-pres--power (car x)))))))
          (cg-pres--play game seat r 1))
      (let ((cand (cl-loop for (r . c) in counts
                           when (and (>= c cnt) (> (cg-pres--power r) top)) collect r)))
        (if cand
            (cg-pres--play game seat
                           (car (cl-sort cand #'< :key #'cg-pres--power)) cnt)
          (cg-pres--pass game seat))))))

(defun cg-pres--result (game)
  "Return a finishing summary string."
  (let* ((order (cg-get game :order)) (n (length order)) (parts nil))
    (dotimes (i n)
      (let ((title (cond ((= i 0) "President") ((= i (1- n)) "Scum")
                         ((= i 1) "Vice-President") ((= i (- n 2)) "Vice-Scum")
                         (t "Citizen"))))
        (push (format "%s: %s" title (cg-pres--name game (nth i order))) parts)))
    (concat "Game over -- " (mapconcat #'identity (nreverse parts) ", ")
            ".  Press n for the next deal.")))

;;;; UI

(defvar-local cg-pres--game nil "The President game in the current buffer.")

(defun cg-pres--run (game)
  "Advance AI seats until it is the human's turn or the game ends."
  (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0))
    (cg-pres--ai-move game (cg-get game :turn))))

(defun cg-pres--hand-ranks (game)
  "Distinct ranks in seat 0's hand, ordered by power."
  (let ((rs (delete-dups (mapcar #'cdr (cg-pres--hand game 0)))))
    (cl-sort rs #'< :key #'cg-pres--power)))

(defun cg-pres-act (&optional count)
  "Play the selected rank.  With prefix COUNT, lead that many of it."
  (interactive "P")
  (let* ((game cg-pres--game)
         (ranks (cg-pres--hand-ranks game)))
    (cond
     ((not (eq (cg-get game :phase) 'play)) (cg-put game :message "Press n for a new deal."))
     ((/= (cg-get game :turn) 0) (cg-put game :message "Not your turn."))
     ((null ranks) (cg-put game :message "You are out."))
     (t (let* ((rank (nth (min (cg-get game :cursor) (1- (length ranks))) ranks))
               (have (cl-count rank (mapcar #'cdr (cg-pres--hand game 0))))
               (need (cg-get game :count)))
          (if (= need 0)
              (let ((n (min have (max 1 (prefix-numeric-value (or count 1))))))
                (cg-pres--play game 0 rank n)
                (cg-put game :cursor 0)
                (cg-pres--run game))
            (if (and (>= have need) (> (cg-pres--power rank) (cg-get game :top)))
                (progn (cg-pres--play game 0 rank need)
                       (cg-put game :cursor 0)
                       (cg-pres--run game))
              (cg-put game :message
                      (format "Need %d of a rank higher than the pile." need)))))))
    (cg-pres--redisplay)))

(defun cg-pres-pass ()
  "Pass for the current pile."
  (interactive)
  (let ((game cg-pres--game))
    (cond
     ((/= (cg-get game :turn) 0) (cg-put game :message "Not your turn."))
     ((= (cg-get game :count) 0) (cg-put game :message "You lead -- you must play."))
     (t (cg-pres--pass game 0) (cg-pres--run game)))
    (cg-pres--redisplay)))

(defun cg-pres-left () "Cursor left." (interactive)
  (let* ((g cg-pres--game) (n (length (cg-pres--hand-ranks g))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n))) (cg-pres--redisplay)))
(defun cg-pres-right () "Cursor right." (interactive)
  (let* ((g cg-pres--game) (n (length (cg-pres--hand-ranks g))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n))) (cg-pres--redisplay)))
(defun cg-pres-new () "New deal." (interactive)
  (cg-pres--deal cg-pres--game) (cg-pres--run cg-pres--game) (cg-pres--redisplay))
(defun cg-pres-redraw () "Redraw." (interactive) (cg-pres--redisplay))
(defun cg-pres-help () "Controls." (interactive)
  (message "Arrows: choose rank  RET: play (C-u N to lead N)  p: pass  n: new  g: redraw"))

(cl-defmethod cg-render ((game cg-president-game))
  "Return a propertized string depicting GAME for a text display."
  (let* ((out (list)) (ranks (cg-pres--hand-ranks game))
         (cur (cg-get game :cursor)))
    (push (format "  President\n\n") out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards%s\n" s (length (cg-pres--hand game s))
                      (if (memq s (cg-get game :out)) "  (out)" "")) out)))
    (push (format "\n  Pile: %s\n\n"
                  (if (> (cg-get game :count) 0)
                      (format "%d × power-%d (last: %s)"
                              (cg-get game :count) (cg-get game :top)
                              (cg-pres--name game (cg-get game :last-player)))
                    "empty -- your lead"))
          out)
    (push "  Your hand (by rank):\n   " out)
    (let ((i 0))
      (dolist (r ranks)
        (let* ((cnt (cl-count r (mapcar #'cdr (cg-pres--hand game 0))))
               (str (format "%s×%d" (aref cg-pres-ranks r) cnt))
               (faces nil))
          (when (= i cur) (push 'cg-cursor faces))
          (push (propertize (format "%6s" str) 'face (or faces 'default)) out))
        (cl-incf i)))
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-pres--redisplay ()
  (let ((game cg-pres--game) (inhibit-read-only t))
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defvar cg-pres-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'cg-pres-left)
    (define-key map (kbd "<right>") #'cg-pres-right)
    (define-key map (kbd "RET") #'cg-pres-act)
    (define-key map (kbd "SPC") #'cg-pres-act)
    (define-key map "p" #'cg-pres-pass)
    (define-key map "n" #'cg-pres-new)
    (define-key map "g" #'cg-pres-redraw)
    (define-key map "?" #'cg-pres-help)
    map)
  "Keymap for `cg-pres-mode'.")

(define-derived-mode cg-pres-mode special-mode "President"
  "Major mode for President."
  (setq-local truncate-lines t))

;;;###autoload
(defun cg-president ()
  "Play President (Scum) against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*President*")))
    (with-current-buffer buf
      (cg-pres-mode)
      (setq cg-pres--game (cg-president-game))
      (cg-pres--deal cg-pres--game)
      (cg-pres--run cg-pres--game)
      (cg-pres--redisplay))
    (switch-to-buffer buf)))

(provide 'cg-president)
;;; cg-president.el ends here
