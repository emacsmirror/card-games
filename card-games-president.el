;;; card-games-president.el --- President (Scum), a climbing card game  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.92
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
(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-pres-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst card-games-pres-titles ["President" "Vice-President" "Citizen"
                          "Vice-Scum" "Scum"]
  "Finishing titles from first out to last.")

(defcustom card-games-president-players 4
  "Number of players in President, including you (3-6)."
  :type 'integer :group 'card-games)

(defun card-games-pres--power (rank)
  "Return the climbing power of RANK; the Two (RANK 0) is highest."
  (if (= rank 0) 13 rank))

(defun card-games-pres-card-string (card)
  "Return the display string for CARD (a middot for nil)."
  (if (null card) "·"
    (concat (aref card-games-pres-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defsubst card-games-pres-red-p (card) "Return non-nil when CARD is red." (and card (card-games-red-suit-p (car card))))

(defun card-games-pres--deck ()
  "Return a fresh shuffled 52-card deck."
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defun card-games-pres--sort (cards)
  "Sort CARDS by climbing power then suit."
  (sort (copy-sequence cards)
        (lambda (a b) (if (= (card-games-pres--power (cdr a)) (card-games-pres--power (cdr b)))
                          (< (car a) (car b))
                        (< (card-games-pres--power (cdr a)) (card-games-pres--power (cdr b)))))))

(defclass card-games-president-game (card-games-game)
  ((vname :initform "President"))
  "A game of President (Scum).")

(defsubst card-games-pres--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-pres--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))
(defsubst card-games-pres--name (_game s)
  "Return the display name of seat S."
  (if (= s 0) "You" (format "Player %d" s)))

;;;; Dealing and the inter-game exchange

(cl-defmethod card-games-pres--deal ((game card-games-president-game))
  "Deal a new round into GAME (3-6 players)."
  (let* ((n (max 3 (min 6 card-games-president-players)))
         (deck (card-games-pres--deck))
         (hands (make-vector n nil))
         (s 0))
    (while deck
      (push (pop deck) (aref hands (mod s n)))
      (cl-incf s))
    (dotimes (i n) (aset hands i (card-games-pres--sort (aref hands i))))
    (card-games-put game :hands hands)
    (card-games-put game :nplayers n)
    (card-games-pres--exchange game)            ; carry out roles from the last deal
    (card-games-put game :count 0)
    (card-games-put game :top -1)
    (card-games-put game :passed (make-vector n nil))
    (card-games-put game :out nil)
    (card-games-put game :last-player nil)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (unless (card-games-get game :games) (card-games-put game :games 0))
    (card-games-put game :message
            "Lead any rank; others beat it with a higher one or pass.  p passes.")
    game))

(defun card-games-pres--best (hand k) "The K highest-power cards of HAND." (last (card-games-pres--sort hand) k))
(defun card-games-pres--worst (hand k) "The K lowest-power cards of HAND." (cl-subseq (card-games-pres--sort hand) 0 k))

(cl-defmethod card-games-pres--exchange ((game card-games-president-game))
  "Trade GAME cards by rank from the previous deal's finishing order, if any."
  (let ((order (card-games-get game :order)) (n (card-games-get game :nplayers)))
    (when (and order (= (length order) n) (>= n 4))
      (let* ((prez (nth 0 order)) (scum (nth (1- n) order))
             (vp (nth 1 order)) (vice (nth (- n 2) order)))
        (card-games-pres--give game scum prez 2)   ; scum's 2 best -> president
        (card-games-pres--give game prez scum 2 t) ; president's 2 worst -> scum
        (card-games-pres--give game vice vp 1)
        (card-games-pres--give game vp vice 1 t)))))

(defun card-games-pres--give (game from to k &optional worst)
  "Move K cards (best, or WORST) from GAME seat FROM to seat TO."
  (let* ((cards (if worst (card-games-pres--worst (card-games-pres--hand game from) k)
                  (card-games-pres--best (card-games-pres--hand game from) k))))
    (card-games-pres--set-hand game from
                       (cl-set-difference (card-games-pres--hand game from) cards :test #'equal))
    (card-games-pres--set-hand game to
                       (card-games-pres--sort (append (card-games-pres--hand game to) cards)))))

;;;; Move logic

(defun card-games-pres--rank-counts (game s)
  "Return an alist (RANK . COUNT) for GAME seat S's hand."
  (let ((tbl nil))
    (dolist (c (card-games-pres--hand game s))
      (setf (alist-get (cdr c) tbl 0) (1+ (alist-get (cdr c) tbl 0))))
    tbl))

(defun card-games-pres--legal-ranks (game s)
  "Return the ranks GAME seat S may legally play now."
  (let ((cnt (card-games-get game :count)) (top (card-games-get game :top)))
    (cl-loop for (r . c) in (card-games-pres--rank-counts game s)
             when (if (= cnt 0) t (and (>= c cnt) (> (card-games-pres--power r) top)))
             collect r)))

(defun card-games-pres--remove-n (hand rank n)
  "Remove N cards of RANK from HAND."
  (let ((out nil) (left n))
    (dolist (c hand) (if (and (> left 0) (= (cdr c) rank))
                         (cl-decf left) (push c out)))
    (nreverse out)))

(defun card-games-pres--in-game (game)
  "Return GAME seats that still hold cards."
  (cl-loop for s below (card-games-get game :nplayers)
           unless (memq s (card-games-get game :out)) collect s))

(defun card-games-pres--round-active (game)
  "Return GAME seats that can still act on the current pile."
  (cl-loop for s below (card-games-get game :nplayers)
           unless (or (memq s (card-games-get game :out)) (aref (card-games-get game :passed) s))
           collect s))

(defun card-games-pres--next (game from)
  "Return the next GAME seat after FROM still in the round."
  (let ((n (card-games-get game :nplayers)) (s from) (res nil))
    (dotimes (_ n)
      (setq s (mod (1+ s) n))
      (when (and (not res)
                 (not (memq s (card-games-get game :out)))
                 (not (aref (card-games-get game :passed) s)))
        (setq res s)))
    (or res from)))

(defun card-games-pres--clear (game)
  "Clear GAME's pile; the last player to play leads, else the next active seat."
  (card-games-put game :count 0) (card-games-put game :top -1)
  (card-games-put game :passed (make-vector (card-games-get game :nplayers) nil))
  (let ((last (card-games-get game :last-player)))
    (card-games-put game :turn (if (and last (not (memq last (card-games-get game :out)))) last
                         (card-games-pres--next game (or last 0)))))
  (card-games-put game :message "Pile cleared."))

(defun card-games-pres--check-finish (game)
  "End GAME when only one player still has cards (the Scum)."
  (let ((in (card-games-pres--in-game game)))
    (when (<= (length in) 1)
      (when in (card-games-put game :out (append (card-games-get game :out) in)))
      (card-games-put game :order (card-games-get game :out))
      (card-games-put game :games (1+ (or (card-games-get game :games) 0)))
      (card-games-put game :phase 'game-over)
      (card-games-put game :message (card-games-pres--result game))
      t)))

(defun card-games-pres--advance (game)
  "Decide GAME's next turn or clear the pile after a move."
  (unless (card-games-pres--check-finish game)
    (let* ((active (card-games-pres--round-active game))
           (last (card-games-get game :last-player))
           (others (and last (cl-remove last active))))
      (if (and (> (card-games-get game :count) 0) (null others))
          (card-games-pres--clear game)
        (card-games-put game :turn (card-games-pres--next game (card-games-get game :turn)))))))

(defun card-games-pres--play (game seat rank n)
  "Have GAME seat SEAT play N cards of RANK."
  (card-games-pres--set-hand game seat (card-games-pres--remove-n (card-games-pres--hand game seat) rank n))
  (card-games-put game :count n) (card-games-put game :top (card-games-pres--power rank))
  (card-games-put game :last-player seat)
  (when (null (card-games-pres--hand game seat))
    (card-games-put game :out (append (card-games-get game :out) (list seat))))
  (card-games-put game :message (format "%s plays %d × %s" (card-games-pres--name game seat)
                                n (aref card-games-pres-ranks rank)))
  (card-games-pres--advance game))

(defun card-games-pres--pass (game seat)
  "Have GAME seat SEAT pass on the current pile."
  (aset (card-games-get game :passed) seat t)
  (card-games-put game :message (format "%s passes." (card-games-pres--name game seat)))
  (card-games-pres--advance game))

(defun card-games-pres--ai-move (game seat)
  "Make GAME seat SEAT's move: lead low, beat low, or pass."
  (let* ((cnt (card-games-get game :count)) (top (card-games-get game :top))
         (counts (card-games-pres--rank-counts game seat)))
    (if (= cnt 0)
        (let ((r (caar (cl-sort counts #'< :key (lambda (x) (card-games-pres--power (car x)))))))
          (card-games-pres--play game seat r 1))
      (let ((cand (cl-loop for (r . c) in counts
                           when (and (>= c cnt) (> (card-games-pres--power r) top)) collect r)))
        (if cand
            (card-games-pres--play game seat
                           (car (cl-sort cand #'< :key #'card-games-pres--power)) cnt)
          (card-games-pres--pass game seat))))))

(defun card-games-pres--result (game)
  "Return a finishing summary string for GAME."
  (let* ((order (card-games-get game :order)) (n (length order)) (parts nil))
    (dotimes (i n)
      (let ((title (cond ((= i 0) "President") ((= i (1- n)) "Scum")
                         ((= i 1) "Vice-President") ((= i (- n 2)) "Vice-Scum")
                         (t "Citizen"))))
        (push (format "%s: %s" title (card-games-pres--name game (nth i order))) parts)))
    (concat "Game over -- " (mapconcat #'identity (nreverse parts) ", ")
            ".  Press n for the next deal.")))

;;;; UI

(defvar-local card-games-pres--game nil "The President game in the current buffer.")

(defun card-games-pres--run (game)
  "Advance GAME's AI seats until the human's turn or the game ends."
  (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0))
    (card-games-pres--ai-move game (card-games-get game :turn))))

(defun card-games-pres--hand-ranks (game)
  "Return the distinct ranks in GAME seat 0's hand, ordered by power."
  (let ((rs (delete-dups (mapcar #'cdr (card-games-pres--hand game 0)))))
    (cl-sort rs #'< :key #'card-games-pres--power)))

(defun card-games-pres-act (&optional count)
  "Play the selected rank.  With prefix COUNT, lead that many of it."
  (interactive "P")
  (let* ((game card-games-pres--game)
         (ranks (card-games-pres--hand-ranks game)))
    (cond
     ((not (eq (card-games-get game :phase) 'play)) (card-games-put game :message "Press n for a new deal."))
     ((/= (card-games-get game :turn) 0) (card-games-put game :message "Not your turn."))
     ((null ranks) (card-games-put game :message "You are out."))
     (t (let* ((rank (nth (min (card-games-get game :cursor) (1- (length ranks))) ranks))
               (have (cl-count rank (mapcar #'cdr (card-games-pres--hand game 0))))
               (need (card-games-get game :count)))
          (if (= need 0)
              (let ((n (min have (max 1 (prefix-numeric-value (or count 1))))))
                (card-games-pres--play game 0 rank n)
                (card-games-put game :cursor 0)
                (card-games-pres--run game))
            (if (and (>= have need) (> (card-games-pres--power rank) (card-games-get game :top)))
                (progn (card-games-pres--play game 0 rank need)
                       (card-games-put game :cursor 0)
                       (card-games-pres--run game))
              (card-games-put game :message
                      (format "Need %d of a rank higher than the pile." need)))))))
    (card-games-pres--redisplay)))

(defun card-games-pres-pass ()
  "Pass for the current pile."
  (interactive)
  (let ((game card-games-pres--game))
    (cond
     ((/= (card-games-get game :turn) 0) (card-games-put game :message "Not your turn."))
     ((= (card-games-get game :count) 0) (card-games-put game :message "You lead -- you must play."))
     (t (card-games-pres--pass game 0) (card-games-pres--run game)))
    (card-games-pres--redisplay)))

(defun card-games-pres-left () "Cursor left." (interactive)
  (let* ((g card-games-pres--game) (n (length (card-games-pres--hand-ranks g))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n))) (card-games-pres--redisplay)))
(defun card-games-pres-right () "Cursor right." (interactive)
  (let* ((g card-games-pres--game) (n (length (card-games-pres--hand-ranks g))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n))) (card-games-pres--redisplay)))
(defun card-games-pres-new () "New deal." (interactive)
  (card-games-pres--deal card-games-pres--game) (card-games-pres--run card-games-pres--game) (card-games-pres--redisplay))
(defun card-games-pres-redraw () "Redraw." (interactive) (card-games-pres--redisplay))
(defun card-games-pres-help () "Controls." (interactive)
  (message "Arrows or click: choose/play  RET: play (C-u N to lead N)  p: pass  +/-: size  n: new"))

(defcustom card-games-pres-svg-cards t
  "When non-nil, draw the hand as SVG on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-pres--svg (game)
  "Return a propertized, clickable SVG row of GAME's hand: one card per rank.
Each rank maps to a (hand . INDEX) region and a card-size slider sits below."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (pad 10)
         (gap (+ card-games-svg-card-gap 8)) (ranks (card-games-pres--hand-ranks game))
         (cur (card-games-get game :cursor)) (hand (card-games-pres--hand game 0))
         (n (length ranks)) (lc (card-games-color 'shadow :foreground "gray40"))
         (sh (card-games-svg-slider-height)) (slider-y (+ pad h 22))
         (width (+ (* 2 pad) (max (+ w gap) (* n (+ w gap)) (card-games-svg-slider-width))))
         (height (+ slider-y sh pad)) (svg (svg-create width height))
         (x pad) (i 0) (regions '()))
    (dolist (r ranks)
      (let* ((cnt (cl-count r (mapcar #'cdr hand)))
             (suit (car (cl-find r hand :key #'cdr))))
        (card-games-svg-card svg x pad :rank (aref card-games-pres-ranks r) :suit suit
                     :highlight (= i cur))
        (svg-text svg (format "x%d" cnt) :x (+ x 3) :y (+ pad h 15)
                  :font-size 13 :fill lc :font-family card-games-svg-font-family)
        (push (cons (list x pad w h) (cons 'hand i)) regions))
      (setq x (+ x w gap) i (1+ i)))
    (setq regions (append (nreverse regions)
                          (card-games-svg-slider-draw svg pad slider-y card-games-card-scale)))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions regions)))

(cl-defmethod card-games-render ((game card-games-president-game))
  "Return a propertized string depicting GAME for a text display."
  (let* ((out (list)) (ranks (card-games-pres--hand-ranks game))
         (cur (card-games-get game :cursor)))
    (push (format "  President\n\n") out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards%s\n" s (length (card-games-pres--hand game s))
                      (if (memq s (card-games-get game :out)) "  (out)" "")) out)))
    (push (format "\n  Pile: %s\n\n"
                  (if (> (card-games-get game :count) 0)
                      (format "%d × power-%d (last: %s)"
                              (card-games-get game :count) (card-games-get game :top)
                              (card-games-pres--name game (card-games-get game :last-player)))
                    "empty -- your lead"))
          out)
    (push "  Your hand (by rank):\n   " out)
    (if (and card-games-pres-svg-cards (display-graphic-p))
        (push (card-games-pres--svg game) out)
      (let ((i 0))
        (dolist (r ranks)
          (let* ((cnt (cl-count r (mapcar #'cdr (card-games-pres--hand game 0))))
                 (str (format "%s×%d" (aref card-games-pres-ranks r) cnt))
                 (faces nil))
            (when (= i cur) (push 'card-games-cursor faces))
            (push (propertize (format "%6s" str) 'face (or faces 'default)) out))
          (cl-incf i))))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-president-game) action)
  "Apply a click ACTION on the rank row to GAME G (a click also plays)."
  (pcase action
    (`(hand . ,i)
     (card-games-put g :cursor i)
     (when (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0))
       (card-games-pres-act)))
    (_ (cl-call-next-method))))

(defun card-games-pres--redisplay ()
  "Redraw the current President buffer."
  (let ((game card-games-pres--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-pres--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defvar card-games-pres-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>") #'card-games-pres-left)
    (define-key map (kbd "<right>") #'card-games-pres-right)
    (define-key map (kbd "RET") #'card-games-pres-act)
    (define-key map (kbd "SPC") #'card-games-pres-act)
    (define-key map "p" #'card-games-pres-pass)
    (define-key map "n" #'card-games-pres-new)
    (define-key map "g" #'card-games-pres-redraw)
    (define-key map "?" #'card-games-pres-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-pres-mode'.")

(define-derived-mode card-games-pres-mode special-mode "President"
  "Major mode for President."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-president ()
  "Play President (Scum) against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*President*")))
    (with-current-buffer buf
      (card-games-pres-mode)
      (setq card-games-pres--game (card-games-president-game))
      (card-games-pres--deal card-games-pres--game)
      (card-games-pres--run card-games-pres--game)
      (card-games-pres--redisplay))
    (switch-to-buffer buf)))

(provide 'card-games-president)
;;; card-games-president.el ends here
