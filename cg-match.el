;;; cg-match.el --- Go Fish and Old Maid  -*- lexical-binding: t; -*-

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

;; Two children's classics that turn on matching ranks rather than melding.
;;
;;   `cg-go-fish' -- Go Fish.  On your turn ask another player for a rank
;;     you already hold; collect all four of a rank to lay down a book.
;;     Whoever lays down the most books wins.
;;   `cg-old-maid' -- Old Maid.  One Queen is removed, so one stays
;;     unpaired.  Discard pairs, then draw blind from your neighbour; do
;;     not be the one left holding the odd Queen.
;;
;; You are the first player; the rest are computer opponents.  Cards use
;; the package cons (SUIT . RANK), RANK 0 (Ace) .. 12 (King).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'cg-core)
(require 'cg-rummy)

;;;; Go Fish

(defcustom cg-go-fish-players 3
  "Number of players in Go Fish, including you (2-5)."
  :type '(choice (const 2) (const 3) (const 4) (const 5)) :group 'card-games)

(defclass cg-go-fish-game (cg-game)
  ((vname :initform "Go Fish"))
  "A game of Go Fish.")

(defsubst cg-gf--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-gf--set-hand (game s v) (aset (cg-get game :hands) s v))

(defun cg-gf--books (game s) (aref (cg-get game :books) s))

(defun cg-gf--rank-count (hand rank)
  "Return how many cards of RANK are in HAND."
  (cl-count rank hand :key #'cdr))

(defun cg-gf--check-books (game s)
  "Lay down any completed four-of-a-kind books from seat S's hand."
  (dotimes (r 13)
    (when (>= (cg-gf--rank-count (cg-gf--hand game s) r) 4)
      (cg-gf--set-hand game s (cl-remove r (cg-gf--hand game s) :key #'cdr))
      (aset (cg-get game :books) s (1+ (aref (cg-get game :books) s))))))

(cl-defmethod cg-gf--deal ((game cg-go-fish-game))
  "Deal a fresh Go Fish game into GAME."
  (let* ((n (max 2 (min 5 cg-go-fish-players)))
         (deck (cg-rummy-deck)) (per (if (<= n 3) 7 5))
         (hands (make-vector n nil)))
    (dotimes (s n) (aset hands s (cl-loop repeat per collect (pop deck))))
    (cg-put game :hands hands)
    (cg-put game :books (make-vector n 0))
    (cg-put game :nplayers n)
    (cg-put game :stock deck)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (dotimes (s n)
      (cg-gf--set-hand game s (cg-rummy-sort-hand (cg-gf--hand game s)))
      (cg-gf--check-books game s))
    (cg-put game :message "Pick a card, then press 1-4 to ask that player for its rank.")
    game))

(defun cg-gf--draw (game s)
  "Draw one stock card into seat S's hand.  Return it, or nil if empty."
  (let ((stock (cg-get game :stock)))
    (when stock
      (cg-gf--set-hand game s (cg-rummy-sort-hand (cons (car stock) (cg-gf--hand game s))))
      (cg-put game :stock (cdr stock))
      (car stock))))

(defun cg-gf--total-books (game)
  (let ((sum 0)) (dotimes (s (cg-get game :nplayers))
                   (setq sum (+ sum (cg-gf--books game s))))
       sum))

(defun cg-gf--maybe-over (game)
  "End the game when all thirteen books are made."
  (when (>= (cg-gf--total-books game) 13)
    (let ((best 0))
      (dotimes (s (cg-get game :nplayers))
        (when (> (cg-gf--books game s) (cg-gf--books game best)) (setq best s)))
      (cg-put game :phase 'game-over)
      (cg-put game :winner best)
      (cg-put game :message
              (format "Game over.  %s wins with %d books!  (n: new game)"
                      (cg-gf--who best) (cg-gf--books game best))))))

(defun cg-gf--who (s) (if (= s 0) "You" (format "Player %d" s)))

(cl-defmethod cg-gf--ask ((game cg-go-fish-game) asker target rank)
  "ASKER asks TARGET for RANK.  Return non-nil if ASKER keeps the turn."
  (let* ((got (cl-remove-if-not (lambda (c) (= (cdr c) rank)) (cg-gf--hand game target)))
         (keep nil))
    (if got
        (progn
          (cg-gf--set-hand game target (cl-remove rank (cg-gf--hand game target) :key #'cdr))
          (cg-gf--set-hand game asker
                           (cg-rummy-sort-hand (append got (cg-gf--hand game asker))))
          (cg-put game :message
                  (format "%s took %d %s%s from %s."
                          (cg-gf--who asker) (length got)
                          (aref cg-rummy-ranks rank) (if (> (length got) 1) "s" "")
                          (cg-gf--who target)))
          (setq keep t))
      ;; go fish
      (let ((drawn (cg-gf--draw game asker)))
        (cg-put game :message
                (format "%s asked %s for %ss -- go fish!%s"
                        (cg-gf--who asker) (cg-gf--who target) (aref cg-rummy-ranks rank)
                        (cond ((null drawn) "  (stock empty)")
                              ((= (cdr drawn) rank) "  Fished it -- go again!")
                              (t ""))))
        (when (and drawn (= (cdr drawn) rank)) (setq keep t))))
    (cg-gf--check-books game asker)
    ;; refill an empty hand from the stock if possible
    (when (and (null (cg-gf--hand game asker)) (cg-get game :stock))
      (cg-gf--draw game asker))
    (cg-gf--maybe-over game)
    (when (and (eq (cg-get game :phase) 'play) (not keep))
      (cg-put game :turn (cg-gf--next game asker)))
    keep))

(defun cg-gf--next (game s)
  "Return the next seat after S that still has cards (or stock to draw)."
  (let ((n (cg-get game :nplayers)) (i (mod (1+ s) (cg-get game :nplayers))) (tries 0))
    (while (and (< tries n) (null (cg-gf--hand game i)) (null (cg-get game :stock)))
      (setq i (mod (1+ i) n) tries (1+ tries)))
    i))

(defun cg-gf--start-turn (game s)
  "Ready seat S to act: draw up if empty; pass the turn if it cannot ask.
Return non-nil when S can ask."
  (when (and (null (cg-gf--hand game s)) (cg-get game :stock))
    (cg-gf--draw game s))
  (cg-gf--maybe-over game)
  (cond ((not (eq (cg-get game :phase) 'play)) nil)
        ((cg-gf--hand game s) t)
        (t (cg-put game :turn (cg-gf--next game s)) nil)))

(cl-defmethod cg-gf--ai-turn ((game cg-go-fish-game) s)
  "Take seat S's whole AI turn (it may keep asking)."
  (when (cg-gf--start-turn game s)
    (let ((guard 0))
      (while (and (= (cg-get game :turn) s) (eq (cg-get game :phase) 'play)
                  (cg-gf--hand game s) (< guard 40))
        (setq guard (1+ guard))
        (let* ((hand (cg-gf--hand game s))
               (counts (make-vector 13 0)) (rank (cdr (car hand))))
          (dolist (c hand) (aset counts (cdr c) (1+ (aref counts (cdr c)))))
          (dotimes (r 13) (when (> (aref counts r) (aref counts rank)) (setq rank r)))
          (let* ((others (cl-loop for o below (cg-get game :nplayers)
                                  unless (= o s) when (cg-gf--hand game o) collect o))
                 (target (and others (nth (random (length others)) others))))
            (if target (cg-gf--ask game s target rank)
              (cg-put game :turn (cg-gf--next game s)))))))))

(defun cg-gf--run (game)
  "Advance AI seats until it is your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0) (< guard 1000))
      (setq guard (1+ guard))
      (cg-gf--ai-turn game (cg-get game :turn))))
  (when (and (eq (cg-get game :phase) 'play) (= (cg-get game :turn) 0))
    (unless (cg-gf--start-turn game 0)
      (when (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0))
        (cg-gf--run game)))))

;;;; Go Fish UI

(defvar-local cg-gf--game nil "The Go Fish game in the current buffer.")

(cl-defmethod cg-render ((game cg-go-fish-game))
  "Return a propertized depiction of the Go Fish GAME."
  (let* ((out '()) (hand (cg-gf--hand game 0)) (cursor (cg-get game :cursor)))
    (push "  Go Fish\n\n" out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   books %d\n"
                      s (length (cg-gf--hand game s)) (cg-gf--books game s)) out)))
    (push (format "\n  Stock: %d        Your books: %d\n\n"
                  (length (cg-get game :stock)) (cg-gf--books game 0)) out)
    (push "  Your hand:\n   " out)
    (push (cg-rummy--render-cards hand cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-go-fish-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i))
    (_ (cl-call-next-method))))

(defun cg-gf--redisplay ()
  (let ((game cg-gf--game) (inhibit-read-only t))
        (setq cg-current-game game cg-redisplay-function #'cg-gf--redisplay)
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-gf-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g cg-gf--game) (n (length (cg-gf--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-gf--redisplay)))

(defun cg-gf-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g cg-gf--game) (n (length (cg-gf--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-gf--redisplay)))

(defun cg-gf-ask ()
  "Ask the player whose number you pressed for the cursor card's rank."
  (interactive)
  (let* ((g cg-gf--game)
         (target (- last-command-event ?0))
         (card (nth (cg-get g :cursor) (cg-gf--hand g 0))))
    (cond
     ((not (eq (cg-get g :phase) 'play)) (cg-put g :message "Press n for a new game."))
     ((/= (cg-get g :turn) 0) (cg-put g :message "Not your turn."))
     ((null card) (cg-put g :message "Pick a card first."))
     ((or (< target 1) (>= target (cg-get g :nplayers)))
      (cg-put g :message "No such player to ask."))
     ((null (cg-gf--hand g target)) (cg-put g :message "That player has no cards."))
     (t (cg-gf--ask g 0 target (cdr card))
        (cg-put g :cursor 0)
        (unless (= (cg-get g :turn) 0) (cg-gf--run g))))
    (cg-gf--redisplay)))

(defun cg-gf-new () "Deal a new Go Fish game." (interactive)
       (cg-gf--deal cg-gf--game) (cg-gf--redisplay))
(defun cg-gf-redraw () "Redraw." (interactive) (cg-gf--redisplay))
(defun cg-gf-help () "Describe the controls." (interactive)
       (message "Arrows: choose a rank  1-4: ask that player  n: new  g: redraw"))

(defvar cg-go-fish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-gf-left)
    (define-key map (kbd "<right>") #'cg-gf-right)
    (dolist (k '("1" "2" "3" "4")) (define-key map k #'cg-gf-ask))
    (define-key map "n" #'cg-gf-new)
    (define-key map "g" #'cg-gf-redraw)
    (define-key map "?" #'cg-gf-help)
    map)
  "Keymap for `cg-go-fish-mode'.")

(define-derived-mode cg-go-fish-mode special-mode "GoFish"
  "Major mode for Go Fish."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-go-fish ()
  "Play Go Fish against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Go Fish*")))
    (with-current-buffer buf
      (cg-go-fish-mode)
      (setq cg-gf--game (cg-go-fish-game))
      (cg-gf--deal cg-gf--game)
      (cg-gf--redisplay))
    (switch-to-buffer buf)))


;;;; Old Maid

(defcustom cg-old-maid-players 3
  "Number of players in Old Maid, including you (2-5)."
  :type '(choice (const 2) (const 3) (const 4) (const 5)) :group 'card-games)

(defclass cg-old-maid-game (cg-game)
  ((vname :initform "Old Maid"))
  "A game of Old Maid.")

(defsubst cg-om--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-om--set-hand (game s v) (aset (cg-get game :hands) s v))

(defun cg-om--discard-pairs (hand)
  "Return HAND with every matched pair of ranks removed."
  (let ((out '()) (byrank (make-hash-table :test 'eql)))
    (dolist (c hand) (push c (gethash (cdr c) byrank)))
    (maphash (lambda (_r cs)
               (when (cl-oddp (length cs)) (push (car cs) out)))
             byrank)
    (cg-rummy-sort-hand out)))

(cl-defmethod cg-om--deal ((game cg-old-maid-game))
  "Deal a fresh Old Maid game into GAME (one Queen removed)."
  (let* ((n (max 2 (min 5 cg-old-maid-players)))
         (deck (cl-remove (cons 0 11) (cg-rummy-deck) :test #'equal :count 1))
         (hands (make-vector n nil)) (i 0))
    (dolist (c deck)
      (aset hands i (cons c (aref hands i)))
      (setq i (mod (1+ i) n)))
    (dotimes (s n) (aset hands s (cg-om--discard-pairs (aref hands s))))
    (cg-put game :hands hands)
    (cg-put game :nplayers n)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :pick 0)
    (cg-put game :message "Draw a card from the next player: arrows pick, RET draws.")
    (cg-om--skip-empty game)
    game))

(defun cg-om--active (game)
  "Return the list of seats still holding cards."
  (cl-loop for s below (cg-get game :nplayers)
           when (cg-om--hand game s) collect s))

(defun cg-om--target (game s)
  "Return the next active seat after S to draw from."
  (let ((n (cg-get game :nplayers)) (i (mod (1+ s) (cg-get game :nplayers))) (tries 0))
    (while (and (< tries n) (or (= i s) (null (cg-om--hand game i))))
      (setq i (mod (1+ i) n) tries (1+ tries)))
    (and (cg-om--hand game i) i)))

(defun cg-om--skip-empty (game)
  "Advance the turn past any seat that has run out of cards."
  (let ((n (cg-get game :nplayers)) (tries 0))
    (while (and (< tries n) (null (cg-om--hand game (cg-get game :turn))))
      (cg-put game :turn (mod (1+ (cg-get game :turn)) n))
      (setq tries (1+ tries)))))

(defun cg-om--total (game)
  (let ((sum 0)) (dotimes (s (cg-get game :nplayers))
                   (setq sum (+ sum (length (cg-om--hand game s)))))
       sum))

(cl-defmethod cg-om--draw ((game cg-old-maid-game) drawer idx)
  "DRAWER takes card IDX from the next active hand, then discards a pair."
  (let ((target (cg-om--target game drawer)))
    (when target
      (let* ((thand (cg-om--hand game target))
             (card (nth (min idx (1- (length thand))) thand)))
        (cg-om--set-hand game target (cl-remove card thand :test #'equal :count 1))
        (cg-om--set-hand game drawer
                         (cg-om--discard-pairs (cons card (cg-om--hand game drawer))))
        (cg-put game :message
                (format "%s drew from %s."
                        (if (= drawer 0) "You" (format "Player %d" drawer))
                        (if (= target 0) "you" (format "Player %d" target))))))
    (if (<= (cg-om--total game) 1)
        (cg-om--finish game)
      (cg-put game :turn (mod (1+ drawer) (cg-get game :nplayers)))
      (cg-put game :pick 0)
      (cg-om--skip-empty game))))

(cl-defmethod cg-om--finish ((game cg-old-maid-game))
  "End the game; whoever holds the last card is the Old Maid."
  (let ((loser (car (cg-om--active game))))
    (cg-put game :phase 'game-over)
    (cg-put game :winner loser)
    (cg-put game :message
            (if loser
                (format "%s is left holding the Old Maid!  (n: new game)"
                        (if (= loser 0) "You are" (format "Player %d is" loser)))
              "All paired off -- a draw!  (n: new game)"))))

(defun cg-om--ai-turn (game s)
  "Take seat S's AI turn: draw a random card from the next hand."
  (let ((target (cg-om--target game s)))
    (if (null target) (cg-om--finish game)
      (cg-om--draw game s (random (length (cg-om--hand game target)))))))

(defun cg-om--run (game)
  "Advance AI seats until it is your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (/= (cg-get game :turn) 0) (< guard 500))
      (setq guard (1+ guard))
      (cg-om--ai-turn game (cg-get game :turn)))))

;;;; Old Maid UI

(defvar-local cg-om--game nil "The Old Maid game in the current buffer.")

(cl-defmethod cg-render ((game cg-old-maid-game))
  "Return a propertized depiction of the Old Maid GAME."
  (let* ((out '()) (target (cg-om--target game 0)))
    (push "  Old Maid\n\n" out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards%s\n" s (length (cg-om--hand game s))
                      (if (eql s target) "   <- you draw from here" "")) out)))
    (when (and target (eq (cg-get game :phase) 'play) (= (cg-get game :turn) 0))
      (push (format "\n  Player %d's cards (pick one to draw):\n   " target) out)
      (let ((np (length (cg-om--hand game target))) (pk (cg-get game :pick)))
        (dotimes (i np)
          (push (propertize " ##" 'face (if (= i pk) 'cg-cursor 'cg-gap)) out))))
    (push "\n\n  Your hand:\n   " out)
    (push (cg-rummy--render-cards (cg-om--hand game 0) -1 nil) out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-om--redisplay ()
  (let ((game cg-om--game) (inhibit-read-only t))
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-om-left ()
  "Move the pick cursor left over the target's cards."
  (interactive)
  (let* ((g cg-om--game) (target (cg-om--target g 0))
         (np (and target (length (cg-om--hand g target)))))
    (when (and np (> np 0)) (cg-put g :pick (mod (1- (cg-get g :pick)) np)))
    (cg-om--redisplay)))

(defun cg-om-right ()
  "Move the pick cursor right over the target's cards."
  (interactive)
  (let* ((g cg-om--game) (target (cg-om--target g 0))
         (np (and target (length (cg-om--hand g target)))))
    (when (and np (> np 0)) (cg-put g :pick (mod (1+ (cg-get g :pick)) np)))
    (cg-om--redisplay)))

(defun cg-om-draw ()
  "Draw the selected card from the next player."
  (interactive)
  (let ((g cg-om--game))
    (cond
     ((not (eq (cg-get g :phase) 'play)) (cg-put g :message "Press n for a new game."))
     ((/= (cg-get g :turn) 0) (cg-put g :message "Not your turn."))
     (t (cg-om--draw g 0 (cg-get g :pick))
        (unless (= (cg-get g :turn) 0) (cg-om--run g))))
    (cg-om--redisplay)))

(defun cg-om-new () "Deal a new Old Maid game." (interactive)
       (cg-om--deal cg-om--game) (cg-om--redisplay))
(defun cg-om-redraw () "Redraw." (interactive) (cg-om--redisplay))
(defun cg-om-help () "Describe the controls." (interactive)
       (message "Arrows: pick a card from the next player  RET: draw it  n: new  g: redraw"))

(defvar cg-old-maid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-om-left)
    (define-key map (kbd "<right>") #'cg-om-right)
    (define-key map (kbd "RET") #'cg-om-draw)
    (define-key map "n" #'cg-om-new)
    (define-key map "g" #'cg-om-redraw)
    (define-key map "?" #'cg-om-help)
    map)
  "Keymap for `cg-old-maid-mode'.")

(define-derived-mode cg-old-maid-mode special-mode "OldMaid"
  "Major mode for Old Maid."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-old-maid ()
  "Play Old Maid against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Old Maid*")))
    (with-current-buffer buf
      (cg-old-maid-mode)
      (setq cg-om--game (cg-old-maid-game))
      (cg-om--deal cg-om--game)
      (cg-om--redisplay))
    (switch-to-buffer buf)))

(provide 'cg-match)
;;; cg-match.el ends here
