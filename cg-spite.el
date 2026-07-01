;;; cg-spite.el --- Spite and Malice, a competitive patience  -*- lexical-binding: t; -*-

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
(require 'cg-core)
(require 'cg-rummy)

(defcustom cg-spite-goal-size 20
  "Number of cards in each player's goal pile."
  :type 'integer :group 'card-games)

(defclass cg-spite-game (cg-game)
  ((vname :initform "Spite & Malice"))
  "A game of Spite & Malice.")

(defun cg-spite--wild-p (card) "Return non-nil when CARD (a King) is wild."
  (= (cdr card) 12))

(defun cg-spite--nat (card) "Return CARD's natural build rank, or nil if wild."
  (if (cg-spite--wild-p card) nil (cdr card)))

(defun cg-spite--deck ()
  "Return two shuffled standard decks (104 cards)."
  (random t)
  (cg-shuffle (cl-loop repeat 2 append
                       (cl-loop for s below 4 append
                                (cl-loop for r below 13 collect (cons s r))))))

;;;; Accessors

(defsubst cg-spite--goal (game s) (aref (cg-get game :goal) s))
(defsubst cg-spite--set-goal (game s v) (aset (cg-get game :goal) s v))
(defsubst cg-spite--hand (game s) (aref (cg-get game :hand) s))
(defsubst cg-spite--set-hand (game s v) (aset (cg-get game :hand) s v))
(defsubst cg-spite--disc (game s) (aref (cg-get game :disc) s))   ; vector of 4 lists

(defun cg-spite--who (s) (if (= s 0) "You" "Computer"))

(cl-defmethod cg-spite--deal ((game cg-spite-game))
  "Deal a fresh Spite & Malice game into GAME."
  (let ((deck (cg-spite--deck)) (goal (make-vector 2 nil))
        (hand (make-vector 2 nil)) (disc (vector nil nil)))
    (dotimes (s 2)
      (aset goal s (cl-loop repeat cg-spite-goal-size collect (pop deck)))
      (aset hand s (cg-rummy-sort-hand (cl-loop repeat 5 collect (pop deck))))
      (aset disc s (make-vector 4 nil)))
    (cg-put game :goal goal)
    (cg-put game :hand hand)
    (cg-put game :disc disc)
    (cg-put game :center (make-vector 4 nil))   ; each nil or (TOPRANK . CARDS)
    (cg-put game :muck nil)
    (cg-put game :stock deck)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :cursor 0)
    (cg-put game :message "Your turn.  RET plays a hand card; G plays your goal card.")
    game))

;;;; Stock and centre piles

(defun cg-spite--draw-stock (game)
  "Pop one card from the stock, recycling the muck when the stock is empty."
  (when (and (null (cg-get game :stock)) (cg-get game :muck))
    (cg-put game :stock (cg-shuffle (cg-get game :muck)))
    (cg-put game :muck nil))
  (let ((stock (cg-get game :stock)))
    (when stock (cg-put game :stock (cdr stock)) (car stock))))

(defun cg-spite--refill (game s)
  "Draw seat S's hand back up to five cards."
  (while (and (< (length (cg-spite--hand game s)) 5) (or (cg-get game :stock)
                                                         (cg-get game :muck)))
    (let ((c (cg-spite--draw-stock game)))
      (when c (cg-spite--set-hand game s (cg-rummy-sort-hand
                                          (cons c (cg-spite--hand game s))))))))

(defun cg-spite--needed (game i)
  "Return the rank the centre pile I needs next (0 for an empty slot)."
  (let ((p (aref (cg-get game :center) i)))
    (if p (1+ (car p)) 0)))

(defun cg-spite--legal-center (game card)
  "Return the index of the first centre pile CARD may be played on, or nil."
  (let ((found nil))
    (dotimes (i 4)
      (let ((need (cg-spite--needed game i)))
        (when (and (null found) (<= need 11)
                   (or (cg-spite--wild-p card) (eql (cg-spite--nat card) need)))
          (setq found i))))
    found))

(defun cg-spite--put-center (game card i)
  "Place CARD on centre pile I; clear the pile if it reaches a Queen."
  (let* ((need (cg-spite--needed game i))
         (p (aref (cg-get game :center) i))
         (cards (cons card (and p (cdr p)))))
    (if (= need 11)                        ; completed Ace..Queen
        (progn (cg-put game :muck (append cards (cg-get game :muck)))
               (aset (cg-get game :center) i nil))
      (aset (cg-get game :center) i (cons need cards)))))

;;;; Plays

(defun cg-spite--play-hand (game s card i)
  "Seat S plays hand CARD onto centre pile I."
  (cg-spite--set-hand game s (cl-remove card (cg-spite--hand game s) :test #'equal :count 1))
  (cg-spite--put-center game card i)
  (when (null (cg-spite--hand game s)) (cg-spite--refill game s)))

(defun cg-spite--play-goal (game s i)
  "Seat S plays the top of their goal pile onto centre pile I."
  (let ((card (car (cg-spite--goal game s))))
    (cg-spite--set-goal game s (cdr (cg-spite--goal game s)))
    (cg-spite--put-center game card i)
    (when (null (cg-spite--goal game s))
      (cg-put game :phase 'game-over) (cg-put game :winner s))))

(defun cg-spite--play-disc (game s d i)
  "Seat S plays the top of discard pile D onto centre pile I."
  (let* ((pile (aref (cg-spite--disc game s) d)) (card (car pile)))
    (aset (cg-spite--disc game s) d (cdr pile))
    (cg-spite--put-center game card i)))

(defun cg-spite--discard (game s card d)
  "Seat S discards CARD from hand onto discard pile D, ending the turn."
  (cg-spite--set-hand game s (cl-remove card (cg-spite--hand game s) :test #'equal :count 1))
  (aset (cg-spite--disc game s) d (cons card (aref (cg-spite--disc game s) d)))
  (cg-put game :turn (- 1 s)))

;;;; AI

(defun cg-spite--ai-one (game s)
  "Make one beneficial play for seat S; return non-nil if a play was made."
  (let ((goal (car (cg-spite--goal game s))) (done nil))
    (cond
     ;; 1. advance the goal card (a wild goal card plays anywhere)
     ((and goal (cg-spite--legal-center game goal))
      (cg-spite--play-goal game s (cg-spite--legal-center game goal)) (setq done t))
     ;; 2. a non-wild hand card that fits
     ((cl-find-if (lambda (c) (and (not (cg-spite--wild-p c))
                                   (cg-spite--legal-center game c)))
                  (cg-spite--hand game s))
      (let ((card (cl-find-if (lambda (c) (and (not (cg-spite--wild-p c))
                                               (cg-spite--legal-center game c)))
                              (cg-spite--hand game s))))
        (cg-spite--play-hand game s card (cg-spite--legal-center game card))
        (setq done t)))
     (t
      ;; 3. a non-wild discard top that fits
      (catch 'hit
        (dotimes (d 4)
          (let ((top (car (aref (cg-spite--disc game s) d))))
            (when (and top (not (cg-spite--wild-p top)) (cg-spite--legal-center game top))
              (cg-spite--play-disc game s d (cg-spite--legal-center game top))
              (setq done t) (throw 'hit t))))
        ;; 4. use a wild King: bridge to the goal card if possible, else
        ;;    advance the most-built pile to keep cards flowing
        (let ((king (cl-find-if #'cg-spite--wild-p (cg-spite--hand game s))))
          (when king
            (let* ((gr (and goal (cg-spite--nat goal))) (target nil))
              (when gr
                (dotimes (i 4)
                  (let ((need (cg-spite--needed game i)))
                    (when (and (null target) (<= need 11) (= need (1- gr)))
                      (setq target i)))))
              (unless target
                (let ((bestneed -1))
                  (dotimes (i 4)
                    (let ((need (cg-spite--needed game i)))
                      (when (and (<= need 11) (> need bestneed))
                        (setq bestneed need target i))))))
              (when target
                (cg-spite--play-hand game s king target) (setq done t))))))))
    done))

(defun cg-spite--ai-turn (game s)
  "Take seat S's whole AI turn: play what helps, then discard."
  (cg-spite--refill game s)
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (< guard 300)
                (cg-spite--ai-one game s))
      (setq guard (1+ guard))))
  (when (eq (cg-get game :phase) 'play)
    (let ((hand (cg-spite--hand game s)))
      (if (null hand)
          (cg-put game :turn (- 1 s))       ; played out, nothing to discard
        ;; discard the highest non-wild card; keep Kings (wild)
        (let* ((nonk (cl-remove-if #'cg-spite--wild-p hand))
               (card (car (sort (copy-sequence (or nonk hand))
                                (lambda (a b) (> (cdr a) (cdr b))))))
               (d (cg-spite--ai-disc-pile game s card)))
          (cg-spite--discard game s card d))))))

(defun cg-spite--ai-disc-pile (game s card)
  "Choose a discard pile for CARD: an empty one, else the one topped just above."
  (let ((disc (cg-spite--disc game s)) (empty nil) (best nil) (bestv 99))
    (dotimes (d 4)
      (let ((top (car (aref disc d))))
        (cond ((null top) (unless empty (setq empty d)))
              ((and (not (cg-spite--wild-p top)) (>= (cdr top) (cdr card))
                    (< (- (cdr top) (cdr card)) bestv))
               (setq best d bestv (- (cdr top) (cdr card)))))))
    (or best empty 0)))

(defun cg-spite--run (game)
  "Let the computer (seat 1) take its turns until it is your turn or the game ends."
  (let ((guard 0))
    (while (and (eq (cg-get game :phase) 'play) (= (cg-get game :turn) 1) (< guard 200))
      (setq guard (1+ guard))
      (cg-spite--ai-turn game 1))))

;;;; UI

(defvar-local cg-spite--game nil "The Spite & Malice game in the current buffer.")

(defun cg-spite--center-string (game)
  "Return a one-line depiction of the centre piles."
  (let ((parts '()))
    (dotimes (i 4)
      (let ((p (aref (cg-get game :center) i)))
        (push (if p (format "[%s->%s]" (length (cdr p))
                            (aref cg-rummy-ranks (car p)))
                "[ -- ]")
              parts)))
    (mapconcat #'identity (nreverse parts) "  ")))

(defun cg-spite--disc-string (game s)
  "Return a depiction of seat S's four discard-pile tops."
  (let ((parts '()))
    (dotimes (d 4)
      (let ((top (car (aref (cg-spite--disc game s) d))))
        (push (format "%d:%s" (1+ d) (if top (cg-rummy-card-string top) "--")) parts)))
    (mapconcat #'identity (nreverse parts) "  ")))

(defun cg-spite--board-svg (game)
  "Return an SVG board for the Spite & Malice GAME."
  (let* ((w cg-svg-card-width) (h cg-svg-card-height) (gap cg-svg-card-gap) (pad 16)
         (hand (cg-spite--hand game 0)) (n (length hand))
         (cursor (cg-get game :cursor)) (center (cg-get game :center))
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
         (lc (cg-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family cg-svg-font-family (and bold '(:font-weight "bold"))))
                (pilecard (spec x y)
                          (if spec (cg-svg-card svg x y :rank (car spec) :suit (cdr spec))
                            (cg-svg-card svg x y :gap t))))
      (txt (format "Spite & Malice   (goal %d)" cg-spite-goal-size) pad (+ y-title 12) 13 t)
      (txt (format "Computer:  goal %d left   hand %d   discards %s"
                   (length (cg-spite--goal game 1)) (length (cg-spite--hand game 1))
                   (cg-spite--disc-string game 1))
           pad (+ y-opp 4) 12)
      (txt "Centre (build A..Q; King is wild)" pad (- y-center 4) 11)
      (dotimes (i 4)
        (let* ((x (+ pad (* i colstep))) (pp (aref center i))
               (spec (and pp (cons (aref cg-rummy-ranks (car pp)) (car (cadr pp))))))
          (pilecard spec x y-center)))
      (txt (format "Stock %d    Muck %d"
                   (length (cg-get game :stock)) (length (cg-get game :muck)))
           pad y-sm 11)
      (let* ((gtop (car (cg-spite--goal game 0)))
             (gspec (and gtop (cg-rummy--card-spec gtop))))
        (txt (format "Your goal (%d left)" (length (cg-spite--goal game 0)))
             pad y-ylabel 11)
        (txt "Discards" (+ pad colstep) y-ylabel 11)
        (pilecard gspec pad y-yours)
        (dotimes (d 4)
          (let* ((x (+ pad colstep (* d colstep)))
                 (dtop (car (aref (cg-spite--disc game 0) d)))
                 (dspec (and dtop (cg-rummy--card-spec dtop))))
            (pilecard dspec x y-yours))))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (cg-rummy--card-spec c)) (curp (= i cursor)))
            (cg-svg-card svg x y-hand :rank (car sp) :suit (cdr sp) :highlight curp)
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (cg-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (cg-svg-image svg (cg-scale)) 'cg-regions (nreverse regions))))

(cl-defmethod cg-render ((game cg-spite-game))
  "Return a depiction of the GAME: an SVG board if graphical, else text."
  (if (and cg-rummy-svg-cards (display-graphic-p))
      (cg-spite--board-svg game)
    (cg-spite--render-text game)))

(defun cg-spite--render-text (game)
  "Return a plain-text depiction of the Spite & Malice GAME."
  (let* ((out '()) (cursor (cg-get game :cursor)))
    (push "  Spite & Malice\n\n" out)
    (push (format "  Computer goal: %d left    hand: %d    discards: %s\n\n"
                  (length (cg-spite--goal game 1)) (length (cg-spite--hand game 1))
                  (cg-spite--disc-string game 1))
          out)
    (push (format "  Centre:  %s\n" (cg-spite--center-string game)) out)
    (push (format "  Stock: %d   Muck: %d\n\n"
                  (length (cg-get game :stock)) (length (cg-get game :muck))) out)
    (push (format "  Your goal: %s  (%d left)\n"
                  (let ((g (car (cg-spite--goal game 0))))
                    (if g (cg-rummy-card-string g) "--"))
                  (length (cg-spite--goal game 0)))
          out)
    (push (format "  Your discards: %s\n\n" (cg-spite--disc-string game 0)) out)
    (push "  Your hand:\n   " out)
    (push (cg-rummy--render-cards (cg-spite--hand game 0) cursor nil nil 'hand) out)
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-spite-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (cg-put g :cursor i)
      (cg-spite-play))
    (_ (cl-call-next-method))))

(defun cg-spite--redisplay ()
  (let ((game cg-spite--game) (inhibit-read-only t))
        (setq cg-current-game game cg-redisplay-function #'cg-spite--redisplay)
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-spite--my-turn-p (g)
  (and (eq (cg-get g :phase) 'play) (= (cg-get g :turn) 0)))

(defun cg-spite-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g cg-spite--game) (n (length (cg-spite--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1- (cg-get g :cursor)) n)))
    (cg-spite--redisplay)))

(defun cg-spite-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g cg-spite--game) (n (length (cg-spite--hand g 0))))
    (when (> n 0) (cg-put g :cursor (mod (1+ (cg-get g :cursor)) n)))
    (cg-spite--redisplay)))

(defun cg-spite--ensure-hand (g)
  "Draw your hand up to five at the start of your turn."
  (cg-spite--refill g 0))

(defun cg-spite-play ()
  "Play the cursor hand card onto the first centre pile it fits."
  (interactive)
  (let* ((g cg-spite--game) (card (nth (cg-get g :cursor) (cg-spite--hand g 0))))
    (cond
     ((not (cg-spite--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((null card) (cg-put g :message "No card selected."))
     (t (let ((i (cg-spite--legal-center g card)))
          (if (null i) (cg-put g :message "That card fits no centre pile.")
            (cg-spite--play-hand g 0 card i)
            (cg-put g :cursor 0)
            (cg-put g :message "Played.  Keep going, or d to discard and end turn.")))))
    (cg-spite--redisplay)))

(defun cg-spite-goal ()
  "Play your goal-pile top onto the first centre pile it fits."
  (interactive)
  (let* ((g cg-spite--game) (card (car (cg-spite--goal g 0))))
    (cond
     ((not (cg-spite--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((null card) (cg-put g :message "Your goal pile is empty."))
     (t (let ((i (cg-spite--legal-center g card)))
          (if (null i) (cg-put g :message "Your goal card fits no centre pile.")
            (cg-spite--play-goal g 0 i)
            (if (eq (cg-get g :phase) 'game-over)
                (cg-put g :message "You emptied your goal -- you win!  (n: new game)")
              (cg-put g :message "Goal card played!"))))))
    (cg-spite--redisplay)))

(defun cg-spite-play-disc ()
  "Play the top of the discard pile whose number you pressed."
  (interactive)
  (let* ((g cg-spite--game) (d (- last-command-event ?1))
         (top (and (>= d 0) (< d 4) (car (aref (cg-spite--disc g 0) d)))))
    (cond
     ((not (cg-spite--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((null top) (cg-put g :message "That discard pile is empty."))
     (t (let ((i (cg-spite--legal-center g top)))
          (if (null i) (cg-put g :message "That card fits no centre pile.")
            (cg-spite--play-disc g 0 d i)
            (cg-put g :message "Played from a discard pile.")))))
    (cg-spite--redisplay)))

(defun cg-spite-discard ()
  "Discard the cursor card to a discard pile and end your turn."
  (interactive)
  (let* ((g cg-spite--game) (card (nth (cg-get g :cursor) (cg-spite--hand g 0))))
    (cond
     ((not (cg-spite--my-turn-p g)) (cg-put g :message "Not your turn."))
     ((null card) (cg-put g :message "No card to discard."))
     (t (cg-spite--discard g 0 card (cg-spite--ai-disc-pile g 0 card))
        (cg-put g :cursor 0)
        (cg-spite--run g)
        (when (eq (cg-get g :phase) 'play)
          (cg-spite--ensure-hand g)
          (cg-put g :message "Your turn."))))
    (cg-spite--redisplay)))

(defun cg-spite-new ()
  "Deal a fresh game."
  (interactive)
  (cg-spite--deal cg-spite--game)
  (cg-spite--redisplay))

(defun cg-spite-redraw () "Redraw." (interactive) (cg-spite--redisplay))
(defun cg-spite-help () "Describe the controls." (interactive)
       (message "Arrows: choose  RET: play hand card  G: play goal  1-4: play discard top  d: discard/end  n: new"))

(defvar cg-spite-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-spite-left)
    (define-key map (kbd "<right>") #'cg-spite-right)
    (define-key map (kbd "RET") #'cg-spite-play)
    (define-key map "G" #'cg-spite-goal)
    (dolist (k '("1" "2" "3" "4")) (define-key map k #'cg-spite-play-disc))
    (define-key map "d" #'cg-spite-discard)
    (define-key map "n" #'cg-spite-new)
    (define-key map "g" #'cg-spite-redraw)
    (define-key map "?" #'cg-spite-help)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-spite-mode'.")

(define-derived-mode cg-spite-mode special-mode "Spite"
  "Major mode for Spite & Malice."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-spite ()
  "Play Spite & Malice against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Spite & Malice*")))
    (with-current-buffer buf
      (cg-spite-mode)
      (setq cg-spite--game (cg-spite-game))
      (cg-spite--deal cg-spite--game)
      (cg-spite--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'cg-cat-and-mouse #'cg-spite)

(provide 'cg-spite)
;;; cg-spite.el ends here
