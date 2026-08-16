;;; card-games-match.el --- Go Fish and Old Maid  -*- lexical-binding: t; -*-

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

;; Two children's classics that turn on matching ranks rather than melding.
;;
;;   `card-games-go-fish' -- Go Fish.  On your turn ask another player for a rank
;;     you already hold; collect all four of a rank to lay down a book.
;;     Whoever lays down the most books wins.
;;   `card-games-old-maid' -- Old Maid.  One Queen is removed, so one stays
;;     unpaired.  Discard pairs, then draw blind from your neighbour; do
;;     not be the one left holding the odd Queen.
;;
;; You are the first player; the rest are computer opponents.  Cards use
;; the package cons (SUIT . RANK), RANK 0 (Ace) .. 12 (King).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-rummy)

;;;; Go Fish

(defcustom card-games-go-fish-players 3
  "Number of players in Go Fish, including you (2-5)."
  :type '(choice (const 2) (const 3) (const 4) (const 5)) :group 'card-games)

(defclass card-games-go-fish-game (card-games-game)
  ((vname :initform "Go Fish"))
  "A game of Go Fish.")

(defsubst card-games-gf--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-gf--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))

(defun card-games-gf--books (game s) "Return seat S's laid-down books in GAME." (aref (card-games-get game :books) s))

(defun card-games-gf--rank-count (hand rank)
  "Return how many cards of RANK are in HAND."
  (cl-count rank hand :key #'cdr))

(defun card-games-gf--check-books (game s)
  "Lay down any completed four-of-a-kind books from GAME seat S's hand."
  (dotimes (r 13)
    (when (>= (card-games-gf--rank-count (card-games-gf--hand game s) r) 4)
      (card-games-gf--set-hand game s (cl-remove r (card-games-gf--hand game s) :key #'cdr))
      (aset (card-games-get game :books) s (1+ (aref (card-games-get game :books) s))))))

(cl-defmethod card-games-gf--deal ((game card-games-go-fish-game))
  "Deal a fresh Go Fish game into GAME."
  (let* ((n (max 2 (min 5 card-games-go-fish-players)))
         (deck (card-games-rummy-deck)) (per (if (<= n 3) 7 5))
         (hands (make-vector n nil)))
    (dotimes (s n) (aset hands s (cl-loop repeat per collect (pop deck))))
    (card-games-put game :hands hands)
    (card-games-put game :books (make-vector n 0))
    (card-games-put game :nplayers n)
    (card-games-put game :stock deck)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (dotimes (s n)
      (card-games-gf--set-hand game s (card-games-rummy-sort-hand (card-games-gf--hand game s)))
      (card-games-gf--check-books game s))
    (card-games-put game :message "Pick a card, then press 1-4 to ask that player for its rank.")
    game))

(defun card-games-gf--draw (game s)
  "Draw one stock card into GAME seat S's hand.  Return it, or nil if empty."
  (let ((stock (card-games-get game :stock)))
    (when stock
      (card-games-gf--set-hand game s (card-games-rummy-sort-hand (cons (car stock) (card-games-gf--hand game s))))
      (card-games-put game :stock (cdr stock))
      (car stock))))

(defun card-games-gf--total-books (game)
  "Return the total number of books laid down in GAME."
  (let ((sum 0)) (dotimes (s (card-games-get game :nplayers))
                   (setq sum (+ sum (card-games-gf--books game s))))
       sum))

(defun card-games-gf--maybe-over (game)
  "End GAME when all thirteen books are made."
  (when (>= (card-games-gf--total-books game) 13)
    (let ((best 0))
      (dotimes (s (card-games-get game :nplayers))
        (when (> (card-games-gf--books game s) (card-games-gf--books game best)) (setq best s)))
      (card-games-put game :phase 'game-over)
      (card-games-put game :winner best)
      (card-games-put game :message
              (format "Game over.  %s wins with %d books!  (n: new game)"
                      (card-games-gf--who best) (card-games-gf--books game best))))))

(defun card-games-gf--who (s) "Return the display name of seat S." (if (= s 0) "You" (format "Player %d" s)))

(cl-defmethod card-games-gf--ask ((game card-games-go-fish-game) asker target rank)
  "In GAME, ASKER asks TARGET for RANK; return non-nil for another turn."
  (let* ((got (cl-remove-if-not (lambda (c) (= (cdr c) rank)) (card-games-gf--hand game target)))
         (keep nil))
    (if got
        (progn
          (card-games-gf--set-hand game target (cl-remove rank (card-games-gf--hand game target) :key #'cdr))
          (card-games-gf--set-hand game asker
                           (card-games-rummy-sort-hand (append got (card-games-gf--hand game asker))))
          (card-games-put game :message
                  (format "%s took %d %s%s from %s."
                          (card-games-gf--who asker) (length got)
                          (aref card-games-rummy-ranks rank) (if (> (length got) 1) "s" "")
                          (card-games-gf--who target)))
          (setq keep t))
      ;; go fish
      (let ((drawn (card-games-gf--draw game asker)))
        (card-games-put game :message
                (format "%s asked %s for %ss -- go fish!%s"
                        (card-games-gf--who asker) (card-games-gf--who target) (aref card-games-rummy-ranks rank)
                        (cond ((null drawn) "  (stock empty)")
                              ((= (cdr drawn) rank) "  Fished it -- go again!")
                              (t ""))))
        (when (and drawn (= (cdr drawn) rank)) (setq keep t))))
    (card-games-gf--check-books game asker)
    ;; refill an empty hand from the stock if possible
    (when (and (null (card-games-gf--hand game asker)) (card-games-get game :stock))
      (card-games-gf--draw game asker))
    (card-games-gf--maybe-over game)
    (when (and (eq (card-games-get game :phase) 'play) (not keep))
      (card-games-put game :turn (card-games-gf--next game asker)))
    keep))

(defun card-games-gf--next (game s)
  "Return the next GAME seat after S that still has cards (or stock to draw)."
  (let ((n (card-games-get game :nplayers)) (i (mod (1+ s) (card-games-get game :nplayers))) (tries 0))
    (while (and (< tries n) (null (card-games-gf--hand game i)) (null (card-games-get game :stock)))
      (setq i (mod (1+ i) n) tries (1+ tries)))
    i))

(defun card-games-gf--start-turn (game s)
  "Ready GAME seat S to act: draw up if empty; pass if it cannot ask.
Return non-nil when S can ask."
  (when (and (null (card-games-gf--hand game s)) (card-games-get game :stock))
    (card-games-gf--draw game s))
  (card-games-gf--maybe-over game)
  (cond ((not (eq (card-games-get game :phase) 'play)) nil)
        ((card-games-gf--hand game s) t)
        (t (card-games-put game :turn (card-games-gf--next game s)) nil)))

(cl-defmethod card-games-gf--ai-turn ((game card-games-go-fish-game) s)
  "Take GAME seat S's whole AI turn (it may keep asking)."
  (when (card-games-gf--start-turn game s)
    (let ((guard 0))
      (while (and (= (card-games-get game :turn) s) (eq (card-games-get game :phase) 'play)
                  (card-games-gf--hand game s) (< guard 40))
        (setq guard (1+ guard))
        (let* ((hand (card-games-gf--hand game s))
               (counts (make-vector 13 0)) (rank (cdr (car hand))))
          (dolist (c hand) (aset counts (cdr c) (1+ (aref counts (cdr c)))))
          (dotimes (r 13) (when (> (aref counts r) (aref counts rank)) (setq rank r)))
          (let* ((others (cl-loop for o below (card-games-get game :nplayers)
                                  unless (= o s) when (card-games-gf--hand game o) collect o))
                 (target (and others (nth (random (length others)) others))))
            (if target (card-games-gf--ask game s target rank)
              (card-games-put game :turn (card-games-gf--next game s)))))))))

(defun card-games-gf--run (game)
  "Advance GAME's AI seats until your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0) (< guard 1000))
      (setq guard (1+ guard))
      (card-games-gf--ai-turn game (card-games-get game :turn))))
  (when (and (eq (card-games-get game :phase) 'play) (= (card-games-get game :turn) 0))
    (unless (card-games-gf--start-turn game 0)
      (when (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0))
        (card-games-gf--run game)))))

;;;; Go Fish UI

(defvar-local card-games-gf--game nil "The Go Fish game in the current buffer.")

(cl-defmethod card-games-render ((game card-games-go-fish-game))
  "Return a propertized depiction of the Go Fish GAME."
  (let* ((out '()) (hand (card-games-gf--hand game 0)) (cursor (card-games-get game :cursor)))
    (push "  Go Fish\n\n" out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   books %d\n"
                      s (length (card-games-gf--hand game s)) (card-games-gf--books game s)) out)))
    (push (format "\n  Stock: %d        Your books: %d\n\n"
                  (length (card-games-get game :stock)) (card-games-gf--books game 0)) out)
    (push "  Your hand:\n   " out)
    (push (card-games-rummy--render-cards hand cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-go-fish-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i))
    (_ (cl-call-next-method))))

(defun card-games-gf--hand-ranks (game)
  "Return the distinct ranks in your GAME hand, low to high (Ace..King)."
  (let ((seen (make-vector 13 nil)) (out '()))
    (dolist (c (card-games-gf--hand game 0)) (aset seen (cdr c) t))
    (dotimes (r 13) (when (aref seen r) (push r out)))
    (nreverse out)))

(defun card-games-gf--pick-rank (button)
  "Select the rank stored on BUTTON, ready to ask a player for it.
Moves the hand cursor to a card of that rank so the existing
1-4 player keys ask for it."
  (let* ((g card-games-gf--game) (rank (button-get button 'card-games-gf-rank))
         (i (cl-position rank (card-games-gf--hand g 0) :key #'cdr)))
    (when i (card-games-put g :cursor i))
    (card-games-put g :message
            (format "Ask which player for %s?  Press 1-%d."
                    (aref card-games-rummy-ranks rank) (1- (card-games-get g :nplayers))))
    (card-games-gf--redisplay)))

(defun card-games-gf--insert-rank-picker (game)
  "Insert clickable rank buttons for the ranks in your GAME hand.
Each rank is a large, easy target, so you pick what to ask for by rank
instead of hunting for one overlapped card in a big hand."
  (let* ((ranks (card-games-gf--hand-ranks game))
         (cur (nth (card-games-get game :cursor) (card-games-gf--hand game 0)))
         (cur-rank (and cur (cdr cur))))
    (when ranks
      (insert "\n  Ask for:  ")
      (dolist (r ranks)
        (insert-text-button
         (format " %s " (aref card-games-rummy-ranks r))
         'face (if (eql r cur-rank) 'card-games-hint 'link)
         'mouse-face 'highlight 'follow-link t
         'help-echo (format "Ask a player for %ss" (aref card-games-rummy-ranks r))
         'card-games-gf-rank r
         'action #'card-games-gf--pick-rank)
        (insert " "))
      (insert "\n"))))

(defun card-games-gf--redisplay ()
  "Redraw the current Go Fish buffer."
  (let ((game card-games-gf--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-gf--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer)
    (insert (card-games-render game))
    (card-games-gf--insert-rank-picker game)
    (card-games-insert-legend "click a rank (or arrows) · 1-4 ask that player · n new · q menu · ? help")
    (goto-char (point-min))))

(defun card-games-gf-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-gf--game) (n (length (card-games-gf--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-gf--redisplay)))

(defun card-games-gf-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-gf--game) (n (length (card-games-gf--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-gf--redisplay)))

(defun card-games-gf-ask ()
  "Ask the player whose number you pressed for the cursor card's rank."
  (interactive)
  (let* ((g card-games-gf--game)
         (target (- last-command-event ?0))
         (card (nth (card-games-get g :cursor) (card-games-gf--hand g 0))))
    (cond
     ((not (eq (card-games-get g :phase) 'play)) (card-games-put g :message "Press n for a new game."))
     ((/= (card-games-get g :turn) 0) (card-games-put g :message "Not your turn."))
     ((null card) (card-games-put g :message "Pick a card first."))
     ((or (< target 1) (>= target (card-games-get g :nplayers)))
      (card-games-put g :message "No such player to ask."))
     ((null (card-games-gf--hand g target)) (card-games-put g :message "That player has no cards."))
     (t (card-games-gf--ask g 0 target (cdr card))
        (card-games-put g :cursor 0)
        (unless (= (card-games-get g :turn) 0) (card-games-gf--run g))))
    (card-games-gf--redisplay)))

(defun card-games-gf-new () "Deal a new Go Fish game." (interactive)
       (card-games-gf--deal card-games-gf--game) (card-games-gf--redisplay))
(defun card-games-gf-redraw () "Redraw." (interactive) (card-games-gf--redisplay))
(defun card-games-gf-help () "Describe the controls." (interactive)
       (message
        "Click a rank (or arrows) to choose  1-4: ask that player  n: new  q: menu"))

(defvar card-games-go-fish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-gf-left)
    (define-key map (kbd "<right>") #'card-games-gf-right)
    (dolist (k '("1" "2" "3" "4")) (define-key map k #'card-games-gf-ask))
    (define-key map "n" #'card-games-gf-new)
    (define-key map "g" #'card-games-gf-redraw)
    (define-key map "?" #'card-games-gf-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-go-fish-mode'.")

(define-derived-mode card-games-go-fish-mode special-mode "GoFish"
  "Major mode for Go Fish."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-go-fish ()
  "Play Go Fish against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Go Fish*")))
    (with-current-buffer buf
      (card-games-go-fish-mode)
      (setq card-games-gf--game (card-games-go-fish-game))
      (card-games-gf--deal card-games-gf--game)
      (card-games-gf--redisplay))
    (switch-to-buffer buf)))


;;;; Old Maid

(defcustom card-games-old-maid-players 3
  "Number of players in Old Maid, including you (2-5)."
  :type '(choice (const 2) (const 3) (const 4) (const 5)) :group 'card-games)

(defclass card-games-old-maid-game (card-games-game)
  ((vname :initform "Old Maid"))
  "A game of Old Maid.")

(defsubst card-games-om--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-om--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))

(defun card-games-om--discard-pairs (hand)
  "Return HAND with every matched pair of ranks removed."
  (let ((out '()) (byrank (make-hash-table :test 'eql)))
    (dolist (c hand) (push c (gethash (cdr c) byrank)))
    (maphash (lambda (_r cs)
               (when (cl-oddp (length cs)) (push (car cs) out)))
             byrank)
    (card-games-rummy-sort-hand out)))

(cl-defmethod card-games-om--deal ((game card-games-old-maid-game))
  "Deal a fresh Old Maid game into GAME (one Queen removed)."
  (let* ((n (max 2 (min 5 card-games-old-maid-players)))
         (deck (cl-remove (cons 0 11) (card-games-rummy-deck) :test #'equal :count 1))
         (hands (make-vector n nil)) (i 0))
    (dolist (c deck)
      (aset hands i (cons c (aref hands i)))
      (setq i (mod (1+ i) n)))
    (dotimes (s n) (aset hands s (card-games-om--discard-pairs (aref hands s))))
    (card-games-put game :hands hands)
    (card-games-put game :nplayers n)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :pick 0)
    (card-games-put game :message "Draw a card from the next player: arrows pick, RET draws.")
    (card-games-om--skip-empty game)
    game))

(defun card-games-om--active (game)
  "Return GAME's seats still holding cards."
  (cl-loop for s below (card-games-get game :nplayers)
           when (card-games-om--hand game s) collect s))

(defun card-games-om--target (game s)
  "Return the next active GAME seat after S to draw from."
  (let ((n (card-games-get game :nplayers)) (i (mod (1+ s) (card-games-get game :nplayers))) (tries 0))
    (while (and (< tries n) (or (= i s) (null (card-games-om--hand game i))))
      (setq i (mod (1+ i) n) tries (1+ tries)))
    (and (card-games-om--hand game i) i)))

(defun card-games-om--skip-empty (game)
  "Advance GAME's turn past any seat that has run out of cards."
  (let ((n (card-games-get game :nplayers)) (tries 0))
    (while (and (< tries n) (null (card-games-om--hand game (card-games-get game :turn))))
      (card-games-put game :turn (mod (1+ (card-games-get game :turn)) n))
      (setq tries (1+ tries)))))

(defun card-games-om--total (game)
  "Return the total cards remaining in GAME."
  (let ((sum 0)) (dotimes (s (card-games-get game :nplayers))
                   (setq sum (+ sum (length (card-games-om--hand game s)))))
       sum))

(cl-defmethod card-games-om--draw ((game card-games-old-maid-game) drawer idx)
  "In GAME, DRAWER takes card IDX from the next hand, then discards a pair."
  (let ((target (card-games-om--target game drawer)))
    (when target
      (let* ((thand (card-games-om--hand game target))
             (card (nth (min idx (1- (length thand))) thand)))
        (card-games-om--set-hand game target (cl-remove card thand :test #'equal :count 1))
        (card-games-om--set-hand game drawer
                         (card-games-om--discard-pairs (cons card (card-games-om--hand game drawer))))
        (card-games-put game :message
                (format "%s drew from %s."
                        (if (= drawer 0) "You" (format "Player %d" drawer))
                        (if (= target 0) "you" (format "Player %d" target))))))
    (if (<= (card-games-om--total game) 1)
        (card-games-om--finish game)
      (card-games-put game :turn (mod (1+ drawer) (card-games-get game :nplayers)))
      (card-games-put game :pick 0)
      (card-games-om--skip-empty game))))

(cl-defmethod card-games-om--finish ((game card-games-old-maid-game))
  "End GAME; whoever has the last card is the Old Maid."
  (let ((loser (car (card-games-om--active game))))
    (card-games-put game :phase 'game-over)
    (card-games-put game :winner loser)
    (card-games-put game :message
            (if loser
                (format "%s is left holding the Old Maid!  (n: new game)"
                        (if (= loser 0) "You are" (format "Player %d is" loser)))
              "All paired off -- a draw!  (n: new game)"))))

(defun card-games-om--ai-turn (game s)
  "Take GAME seat S's AI turn: draw a random card from the next hand."
  (let ((target (card-games-om--target game s)))
    (if (null target) (card-games-om--finish game)
      (card-games-om--draw game s (random (length (card-games-om--hand game target)))))))

(defun card-games-om--run (game)
  "Advance GAME's AI seats until your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (/= (card-games-get game :turn) 0) (< guard 500))
      (setq guard (1+ guard))
      (card-games-om--ai-turn game (card-games-get game :turn)))))

;;;; Old Maid UI

(defvar-local card-games-om--game nil "The Old Maid game in the current buffer.")

(defun card-games-om--svg (game)
  "Return an SVG board for the Old Maid GAME."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap) (pad 16)
         (hand (card-games-om--hand game 0)) (n (length hand))
         (target (card-games-om--target game 0)) (pick (or (card-games-get game :pick) 0))
         (yourp (and target (eq (card-games-get game :phase) 'play) (= (card-games-get game :turn) 0)))
         (np (card-games-get game :nplayers))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (bstep 20)
         (tn (and target (length (card-games-om--hand game target))))
         (y-title 6) (y-info 26)
         (y-target (+ y-info (* (1- np) 16) 18))
         (y-hand (+ y-target h 42))
         (targetw (if (and yourp tn (> tn 0)) (+ (* (1- tn) bstep) w) 0))
         (height (+ y-hand h 30))
         (width (max (+ fanw (* 2 pad)) (+ targetw (* 2 pad)) 560))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family card-games-svg-font-family (and bold '(:font-weight "bold")))))
      (txt "Old Maid" pad (+ y-title 12) 13 t)
      (let ((yy (+ y-info 4)))
        (dotimes (s np)
          (unless (= s 0)
            (txt (format "Player %d: %d cards%s" s (length (card-games-om--hand game s))
                         (if (eql s target) "   <- draw from here" ""))
                 pad yy 12)
            (setq yy (+ yy 16)))))
      (when (and yourp tn (> tn 0))
        (txt (format "Pick a card from Player %d:" target) pad (- y-target 6) 11)
        (let ((x pad))
          (dotimes (i tn)
            (card-games-svg-card svg x y-target :down t :highlight (= i pick))
            (push (cons (list x y-target (if (= i (1- tn)) w bstep) h) (cons 'pick i))
                  regions)
            (setq x (+ x bstep)))))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))))
        (dolist (c hand)
          (let ((sp (card-games-rummy--card-spec c)))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp)))
          (setq x (+ x step))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions (nreverse regions))))

(cl-defmethod card-games-render-apply ((g card-games-old-maid-game) action)
  "Apply click ACTION on G: pick that card from the target and draw it."
  (pcase action
    (`(pick . ,i) (card-games-put g :pick i) (card-games-om-draw))
    (_ (cl-call-next-method))))

(cl-defmethod card-games-render ((game card-games-old-maid-game))
  "Return a depiction of the Old Maid GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-om--svg game)
    (card-games-om--render-text game)))

(defun card-games-om--render-text (game)
  "Return a plain-text depiction of the Old Maid GAME."
  (let* ((out '()) (target (card-games-om--target game 0)))
    (push "  Old Maid\n\n" out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards%s\n" s (length (card-games-om--hand game s))
                      (if (eql s target) "   <- you draw from here" "")) out)))
    (when (and target (eq (card-games-get game :phase) 'play) (= (card-games-get game :turn) 0))
      (push (format "\n  Player %d's cards (pick one to draw):\n   " target) out)
      (let ((np (length (card-games-om--hand game target))) (pk (card-games-get game :pick)))
        (dotimes (i np)
          (push (propertize " ##" 'face (if (= i pk) 'card-games-cursor 'card-games-gap)) out))))
    (push "\n\n  Your hand:\n   " out)
    (push (card-games-rummy--render-cards (card-games-om--hand game 0) -1 nil) out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun card-games-om--redisplay ()
  "Redraw the current Old Maid buffer."
  (let ((game card-games-om--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-om--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-om-left ()
  "Move the pick cursor left over the target's cards."
  (interactive)
  (let* ((g card-games-om--game) (target (card-games-om--target g 0))
         (np (and target (length (card-games-om--hand g target)))))
    (when (and np (> np 0)) (card-games-put g :pick (mod (1- (card-games-get g :pick)) np)))
    (card-games-om--redisplay)))

(defun card-games-om-right ()
  "Move the pick cursor right over the target's cards."
  (interactive)
  (let* ((g card-games-om--game) (target (card-games-om--target g 0))
         (np (and target (length (card-games-om--hand g target)))))
    (when (and np (> np 0)) (card-games-put g :pick (mod (1+ (card-games-get g :pick)) np)))
    (card-games-om--redisplay)))

(defun card-games-om-draw ()
  "Draw the selected card from the next player."
  (interactive)
  (let ((g card-games-om--game))
    (cond
     ((not (eq (card-games-get g :phase) 'play)) (card-games-put g :message "Press n for a new game."))
     ((/= (card-games-get g :turn) 0) (card-games-put g :message "Not your turn."))
     (t (card-games-om--draw g 0 (card-games-get g :pick))
        (unless (= (card-games-get g :turn) 0) (card-games-om--run g))))
    (card-games-om--redisplay)))

(defun card-games-om-new () "Deal a new Old Maid game." (interactive)
       (card-games-om--deal card-games-om--game) (card-games-om--redisplay))
(defun card-games-om-redraw () "Redraw." (interactive) (card-games-om--redisplay))
(defun card-games-om-help () "Describe the controls." (interactive)
       (message "Arrows: pick a card from the next player  RET: draw it  n: new  g: redraw"))

(defvar card-games-old-maid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-om-left)
    (define-key map (kbd "<right>") #'card-games-om-right)
    (define-key map (kbd "RET") #'card-games-om-draw)
    (define-key map "n" #'card-games-om-new)
    (define-key map "g" #'card-games-om-redraw)
    (define-key map "?" #'card-games-om-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-old-maid-mode'.")

(define-derived-mode card-games-old-maid-mode special-mode "OldMaid"
  "Major mode for Old Maid."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-old-maid ()
  "Play Old Maid against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Old Maid*")))
    (with-current-buffer buf
      (card-games-old-maid-mode)
      (setq card-games-om--game (card-games-old-maid-game))
      (card-games-om--deal card-games-om--game)
      (card-games-om--redisplay))
    (switch-to-buffer buf)))

(provide 'card-games-match)
;;; card-games-match.el ends here
