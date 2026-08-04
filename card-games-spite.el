;;; card-games-spite.el --- Spite and Malice, a competitive patience  -*- lexical-binding: t; -*-

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

;; Spite & Malice (also called Cat & Mouse): a race between you and the
;; computer to empty a face-down goal pile.  Play cards onto up to four
;; shared centre piles, which build up from Ace to Queen regardless of
;; suit; a pile that reaches a Queen is cleared away.  Kings are wild and
;; stand for whatever rank a pile needs next.
;;
;; On your turn, draw your hand up to five, then play from the top of your
;; goal pile, your hand, or the tops of your four discard piles.  Playing
;; your goal card is how you win, so take every chance to.  End your turn
;; by discarding one card to a discard pile.
;;
;; Targets are chosen automatically (the first centre pile a card fits).
;; Cards use the package cons (SUIT . RANK), RANK 0 (Ace) .. 12 (King);
;; the build order runs Ace(0) up to Queen(11), and the King(12) is wild.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-rummy)

(defcustom card-games-spite-goal-size 20
  "Number of cards in each player's goal pile."
  :type 'integer :group 'card-games)

(defclass card-games-spite-game (card-games-game)
  ((vname :initform "Spite & Malice"))
  "A game of Spite & Malice.")

(defun card-games-spite--wild-p (card) "Return non-nil when CARD (a King) is wild."
  (= (cdr card) 12))

(defun card-games-spite--nat (card) "Return CARD's natural build rank, or nil if wild."
  (if (card-games-spite--wild-p card) nil (cdr card)))

(defun card-games-spite--deck ()
  "Return two shuffled standard decks (104 cards)."
  (random t)
  (card-games-shuffle (cl-loop repeat 2 append
                       (cl-loop for s below 4 append
                                (cl-loop for r below 13 collect (cons s r))))))

;;;; Accessors

(defsubst card-games-spite--goal (game s) (aref (card-games-get game :goal) s))
(defsubst card-games-spite--set-goal (game s v) (aset (card-games-get game :goal) s v))
(defsubst card-games-spite--hand (game s) (aref (card-games-get game :hand) s))
(defsubst card-games-spite--set-hand (game s v) (aset (card-games-get game :hand) s v))
(defsubst card-games-spite--disc (game s) (aref (card-games-get game :disc) s))   ; vector of 4 lists

(defun card-games-spite--who (s) (if (= s 0) "You" "Computer"))

(cl-defmethod card-games-spite--deal ((game card-games-spite-game))
  "Deal a fresh Spite & Malice game into GAME."
  (let ((deck (card-games-spite--deck)) (goal (make-vector 2 nil))
        (hand (make-vector 2 nil)) (disc (vector nil nil)))
    (dotimes (s 2)
      (aset goal s (cl-loop repeat card-games-spite-goal-size collect (pop deck)))
      (aset hand s (card-games-rummy-sort-hand (cl-loop repeat 5 collect (pop deck))))
      (aset disc s (make-vector 4 nil)))
    (card-games-put game :goal goal)
    (card-games-put game :hand hand)
    (card-games-put game :disc disc)
    (card-games-put game :center (make-vector 4 nil))   ; each nil or (TOPRANK . CARDS)
    (card-games-put game :muck nil)
    (card-games-put game :stock deck)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (card-games-put game :message "Your turn.  RET plays a hand card; G plays your goal card.")
    game))

;;;; Stock and centre piles

(defun card-games-spite--draw-stock (game)
  "Pop one card from the stock, recycling the muck when the stock is empty."
  (when (and (null (card-games-get game :stock)) (card-games-get game :muck))
    (card-games-put game :stock (card-games-shuffle (card-games-get game :muck)))
    (card-games-put game :muck nil))
  (let ((stock (card-games-get game :stock)))
    (when stock (card-games-put game :stock (cdr stock)) (car stock))))

(defun card-games-spite--refill (game s)
  "Draw seat S's hand back up to five cards."
  (while (and (< (length (card-games-spite--hand game s)) 5) (or (card-games-get game :stock)
                                                         (card-games-get game :muck)))
    (let ((c (card-games-spite--draw-stock game)))
      (when c (card-games-spite--set-hand game s (card-games-rummy-sort-hand
                                          (cons c (card-games-spite--hand game s))))))))

(defun card-games-spite--needed (game i)
  "Return the rank the centre pile I needs next (0 for an empty slot)."
  (let ((p (aref (card-games-get game :center) i)))
    (if p (1+ (car p)) 0)))

(defun card-games-spite--legal-center (game card)
  "Return the index of the first centre pile CARD may be played on, or nil."
  (let ((found nil))
    (dotimes (i 4)
      (let ((need (card-games-spite--needed game i)))
        (when (and (null found) (<= need 11)
                   (or (card-games-spite--wild-p card) (eql (card-games-spite--nat card) need)))
          (setq found i))))
    found))

(defun card-games-spite--put-center (game card i)
  "Place CARD on centre pile I; clear the pile if it reaches a Queen."
  (let* ((need (card-games-spite--needed game i))
         (p (aref (card-games-get game :center) i))
         (cards (cons card (and p (cdr p)))))
    (if (= need 11)                        ; completed Ace..Queen
        (progn (card-games-put game :muck (append cards (card-games-get game :muck)))
               (aset (card-games-get game :center) i nil))
      (aset (card-games-get game :center) i (cons need cards)))))

;;;; Plays

(defun card-games-spite--play-hand (game s card i)
  "Seat S plays hand CARD onto centre pile I."
  (card-games-spite--set-hand game s (cl-remove card (card-games-spite--hand game s) :test #'equal :count 1))
  (card-games-spite--put-center game card i)
  (when (null (card-games-spite--hand game s)) (card-games-spite--refill game s)))

(defun card-games-spite--play-goal (game s i)
  "Seat S plays the top of their goal pile onto centre pile I."
  (let ((card (car (card-games-spite--goal game s))))
    (card-games-spite--set-goal game s (cdr (card-games-spite--goal game s)))
    (card-games-spite--put-center game card i)
    (when (null (card-games-spite--goal game s))
      (card-games-put game :phase 'game-over) (card-games-put game :winner s))))

(defun card-games-spite--play-disc (game s d i)
  "Seat S plays the top of discard pile D onto centre pile I."
  (let* ((pile (aref (card-games-spite--disc game s) d)) (card (car pile)))
    (aset (card-games-spite--disc game s) d (cdr pile))
    (card-games-spite--put-center game card i)))

(defun card-games-spite--discard (game s card d)
  "Seat S discards CARD from hand onto discard pile D, ending the turn."
  (card-games-spite--set-hand game s (cl-remove card (card-games-spite--hand game s) :test #'equal :count 1))
  (aset (card-games-spite--disc game s) d (cons card (aref (card-games-spite--disc game s) d)))
  (card-games-put game :turn (- 1 s)))

;;;; AI

(defun card-games-spite--ai-one (game s)
  "Make one beneficial play for seat S; return non-nil if a play was made."
  (let ((goal (car (card-games-spite--goal game s))) (done nil))
    (cond
     ;; 1. advance the goal card (a wild goal card plays anywhere)
     ((and goal (card-games-spite--legal-center game goal))
      (card-games-spite--play-goal game s (card-games-spite--legal-center game goal)) (setq done t))
     ;; 2. a non-wild hand card that fits
     ((cl-find-if (lambda (c) (and (not (card-games-spite--wild-p c))
                                   (card-games-spite--legal-center game c)))
                  (card-games-spite--hand game s))
      (let ((card (cl-find-if (lambda (c) (and (not (card-games-spite--wild-p c))
                                               (card-games-spite--legal-center game c)))
                              (card-games-spite--hand game s))))
        (card-games-spite--play-hand game s card (card-games-spite--legal-center game card))
        (setq done t)))
     (t
      ;; 3. a non-wild discard top that fits
      (catch 'hit
        (dotimes (d 4)
          (let ((top (car (aref (card-games-spite--disc game s) d))))
            (when (and top (not (card-games-spite--wild-p top)) (card-games-spite--legal-center game top))
              (card-games-spite--play-disc game s d (card-games-spite--legal-center game top))
              (setq done t) (throw 'hit t))))
        ;; 4. use a wild King: bridge to the goal card if possible, else
        ;;    advance the most-built pile to keep cards flowing
        (let ((king (cl-find-if #'card-games-spite--wild-p (card-games-spite--hand game s))))
          (when king
            (let* ((gr (and goal (card-games-spite--nat goal))) (target nil))
              (when gr
                (dotimes (i 4)
                  (let ((need (card-games-spite--needed game i)))
                    (when (and (null target) (<= need 11) (= need (1- gr)))
                      (setq target i)))))
              (unless target
                (let ((bestneed -1))
                  (dotimes (i 4)
                    (let ((need (card-games-spite--needed game i)))
                      (when (and (<= need 11) (> need bestneed))
                        (setq bestneed need target i))))))
              (when target
                (card-games-spite--play-hand game s king target) (setq done t))))))))
    done))

(defun card-games-spite--ai-turn (game s)
  "Take seat S's whole AI turn: play what helps, then discard."
  (card-games-spite--refill game s)
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (< guard 300)
                (card-games-spite--ai-one game s))
      (setq guard (1+ guard))))
  (when (eq (card-games-get game :phase) 'play)
    (let ((hand (card-games-spite--hand game s)))
      (if (null hand)
          (card-games-put game :turn (- 1 s))       ; played out, nothing to discard
        ;; discard the highest non-wild card; keep Kings (wild)
        (let* ((nonk (cl-remove-if #'card-games-spite--wild-p hand))
               (card (car (sort (copy-sequence (or nonk hand))
                                (lambda (a b) (> (cdr a) (cdr b))))))
               (d (card-games-spite--ai-disc-pile game s card)))
          (card-games-spite--discard game s card d))))))

(defun card-games-spite--ai-disc-pile (game s card)
  "Choose a discard pile for CARD: an empty one, else the one topped just above."
  (let ((disc (card-games-spite--disc game s)) (empty nil) (best nil) (bestv 99))
    (dotimes (d 4)
      (let ((top (car (aref disc d))))
        (cond ((null top) (unless empty (setq empty d)))
              ((and (not (card-games-spite--wild-p top)) (>= (cdr top) (cdr card))
                    (< (- (cdr top) (cdr card)) bestv))
               (setq best d bestv (- (cdr top) (cdr card)))))))
    (or best empty 0)))

(defun card-games-spite--run (game)
  "Let the computer (seat 1) take its turns until it is your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (card-games-get game :phase) 'play) (= (card-games-get game :turn) 1) (< guard 200))
      (setq guard (1+ guard))
      (card-games-spite--ai-turn game 1))))

;;;; UI

(defvar-local card-games-spite--game nil "The Spite & Malice game in the current buffer.")

(defun card-games-spite--center-string (game)
  "Return a one-line depiction of the centre piles."
  (let ((parts '()))
    (dotimes (i 4)
      (let ((p (aref (card-games-get game :center) i)))
        (push (if p (format "[%s->%s]" (length (cdr p))
                            (aref card-games-rummy-ranks (car p)))
                "[ -- ]")
              parts)))
    (mapconcat #'identity (nreverse parts) "  ")))

(defun card-games-spite--disc-string (game s)
  "Return a depiction of seat S's four discard-pile tops."
  (let ((parts '()))
    (dotimes (d 4)
      (let ((top (car (aref (card-games-spite--disc game s) d))))
        (push (format "%d:%s" (1+ d) (if top (card-games-rummy-card-string top) "--")) parts)))
    (mapconcat #'identity (nreverse parts) "  ")))

(defun card-games-spite--board-svg (game)
  "Return an SVG board for the Spite & Malice GAME."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap) (pad 16)
         (hand (card-games-spite--hand game 0)) (n (length hand))
         (cursor (card-games-get game :cursor)) (center (card-games-get game :center))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (colstep (+ w 14))
         (y-title 6) (y-opp 26)
         (y-center (+ y-opp 16))
         (y-sm (+ y-center h 16))
         (y-ylabel (+ y-sm 12))
         (y-yours (+ y-ylabel 6))
         (y-hand (+ y-yours h 42))
         (height (+ y-hand h 30))
         (width (max (+ fanw (* 2 pad)) (+ (* 5 colstep) (* 2 pad)) 620))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family card-games-svg-font-family (and bold '(:font-weight "bold"))))
                (pilecard (spec x y)
                          (if spec (card-games-svg-card svg x y :rank (car spec) :suit (cdr spec))
                            (card-games-svg-card svg x y :gap t))))
      (txt (format "Spite & Malice   (goal %d)" card-games-spite-goal-size) pad (+ y-title 12) 13 t)
      (txt (format "Computer:  goal %d left   hand %d   discards %s"
                   (length (card-games-spite--goal game 1)) (length (card-games-spite--hand game 1))
                   (card-games-spite--disc-string game 1))
           pad (+ y-opp 4) 12)
      (txt "Centre (build A..Q; King is wild)" pad (- y-center 4) 11)
      (dotimes (i 4)
        (let* ((x (+ pad (* i colstep))) (pp (aref center i))
               (spec (and pp (cons (aref card-games-rummy-ranks (car pp)) (car (cadr pp))))))
          (pilecard spec x y-center)))
      (txt (format "Stock %d    Muck %d"
                   (length (card-games-get game :stock)) (length (card-games-get game :muck)))
           pad y-sm 11)
      (let* ((gtop (car (card-games-spite--goal game 0)))
             (gspec (and gtop (card-games-rummy--card-spec gtop))))
        (txt (format "Your goal (%d left)" (length (card-games-spite--goal game 0)))
             pad y-ylabel 11)
        (txt "Discards" (+ pad colstep) y-ylabel 11)
        (pilecard gspec pad y-yours)
        (dotimes (d 4)
          (let* ((x (+ pad colstep (* d colstep)))
                 (dtop (car (aref (card-games-spite--disc game 0) d)))
                 (dspec (and dtop (card-games-rummy--card-spec dtop))))
            (pilecard dspec x y-yours))))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (card-games-rummy--card-spec c)) (curp (= i cursor)))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp) :highlight curp)
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions (nreverse regions))))

(cl-defmethod card-games-render ((game card-games-spite-game))
  "Return a depiction of the GAME: an SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-spite--board-svg game)
    (card-games-spite--render-text game)))

(defun card-games-spite--render-text (game)
  "Return a plain-text depiction of the Spite & Malice GAME."
  (let* ((out '()) (cursor (card-games-get game :cursor)))
    (push "  Spite & Malice\n\n" out)
    (push (format "  Computer goal: %d left    hand: %d    discards: %s\n\n"
                  (length (card-games-spite--goal game 1)) (length (card-games-spite--hand game 1))
                  (card-games-spite--disc-string game 1))
          out)
    (push (format "  Centre:  %s\n" (card-games-spite--center-string game)) out)
    (push (format "  Stock: %d   Muck: %d\n\n"
                  (length (card-games-get game :stock)) (length (card-games-get game :muck))) out)
    (push (format "  Your goal: %s  (%d left)\n"
                  (let ((g (car (card-games-spite--goal game 0))))
                    (if g (card-games-rummy-card-string g) "--"))
                  (length (card-games-spite--goal game 0)))
          out)
    (push (format "  Your discards: %s\n\n" (card-games-spite--disc-string game 0)) out)
    (push "  Your hand:\n   " out)
    (push (card-games-rummy--render-cards (card-games-spite--hand game 0) cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-spite-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i)
      (card-games-spite-play))
    (_ (cl-call-next-method))))

(defun card-games-spite--redisplay ()
  (let ((game card-games-spite--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-spite--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-spite--my-turn-p (g)
  (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0)))

(defun card-games-spite-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-spite--game) (n (length (card-games-spite--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-spite--redisplay)))

(defun card-games-spite-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-spite--game) (n (length (card-games-spite--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-spite--redisplay)))

(defun card-games-spite--ensure-hand (g)
  "Draw your hand up to five at the start of your turn."
  (card-games-spite--refill g 0))

(defun card-games-spite-play ()
  "Play the cursor hand card onto the first centre pile it fits."
  (interactive)
  (let* ((g card-games-spite--game) (card (nth (card-games-get g :cursor) (card-games-spite--hand g 0))))
    (cond
     ((not (card-games-spite--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((null card) (card-games-put g :message "No card selected."))
     (t (let ((i (card-games-spite--legal-center g card)))
          (if (null i) (card-games-put g :message "That card fits no centre pile.")
            (card-games-spite--play-hand g 0 card i)
            (card-games-put g :cursor 0)
            (card-games-put g :message "Played.  Keep going, or d to discard and end turn.")))))
    (card-games-spite--redisplay)))

(defun card-games-spite-goal ()
  "Play your goal-pile top onto the first centre pile it fits."
  (interactive)
  (let* ((g card-games-spite--game) (card (car (card-games-spite--goal g 0))))
    (cond
     ((not (card-games-spite--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((null card) (card-games-put g :message "Your goal pile is empty."))
     (t (let ((i (card-games-spite--legal-center g card)))
          (if (null i) (card-games-put g :message "Your goal card fits no centre pile.")
            (card-games-spite--play-goal g 0 i)
            (if (eq (card-games-get g :phase) 'game-over)
                (card-games-put g :message "You emptied your goal -- you win!  (n: new game)")
              (card-games-put g :message "Goal card played!"))))))
    (card-games-spite--redisplay)))

(defun card-games-spite-play-disc ()
  "Play the top of the discard pile whose number you pressed."
  (interactive)
  (let* ((g card-games-spite--game) (d (- last-command-event ?1))
         (top (and (>= d 0) (< d 4) (car (aref (card-games-spite--disc g 0) d)))))
    (cond
     ((not (card-games-spite--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((null top) (card-games-put g :message "That discard pile is empty."))
     (t (let ((i (card-games-spite--legal-center g top)))
          (if (null i) (card-games-put g :message "That card fits no centre pile.")
            (card-games-spite--play-disc g 0 d i)
            (card-games-put g :message "Played from a discard pile.")))))
    (card-games-spite--redisplay)))

(defun card-games-spite-discard ()
  "Discard the cursor card to a discard pile and end your turn."
  (interactive)
  (let* ((g card-games-spite--game) (card (nth (card-games-get g :cursor) (card-games-spite--hand g 0))))
    (cond
     ((not (card-games-spite--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((null card) (card-games-put g :message "No card to discard."))
     (t (card-games-spite--discard g 0 card (card-games-spite--ai-disc-pile g 0 card))
        (card-games-put g :cursor 0)
        (card-games-spite--run g)
        (when (eq (card-games-get g :phase) 'play)
          (card-games-spite--ensure-hand g)
          (card-games-put g :message "Your turn."))))
    (card-games-spite--redisplay)))

(defun card-games-spite-new ()
  "Deal a fresh game."
  (interactive)
  (card-games-spite--deal card-games-spite--game)
  (card-games-spite--redisplay))

(defun card-games-spite-redraw () "Redraw." (interactive) (card-games-spite--redisplay))
(defun card-games-spite-help () "Describe the controls." (interactive)
       (message "Arrows: choose  RET: play hand card  G: play goal  1-4: play discard top  d: discard/end  n: new"))

(defvar card-games-spite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-spite-left)
    (define-key map (kbd "<right>") #'card-games-spite-right)
    (define-key map (kbd "RET") #'card-games-spite-play)
    (define-key map "G" #'card-games-spite-goal)
    (dolist (k '("1" "2" "3" "4")) (define-key map k #'card-games-spite-play-disc))
    (define-key map "d" #'card-games-spite-discard)
    (define-key map "n" #'card-games-spite-new)
    (define-key map "g" #'card-games-spite-redraw)
    (define-key map "?" #'card-games-spite-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-spite-mode'.")

(define-derived-mode card-games-spite-mode special-mode "Spite"
  "Major mode for Spite & Malice."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-spite ()
  "Play Spite & Malice against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Spite & Malice*")))
    (with-current-buffer buf
      (card-games-spite-mode)
      (setq card-games-spite--game (card-games-spite-game))
      (card-games-spite--deal card-games-spite--game)
      (card-games-spite--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'card-games-cat-and-mouse #'card-games-spite)

(provide 'card-games-spite)
;;; card-games-spite.el ends here
