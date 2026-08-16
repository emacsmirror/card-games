;;; card-games-eights.el --- Crazy Eights, a shedding card game  -*- lexical-binding: t; -*-

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

;; Crazy Eights: shed your whole hand by matching the suit or rank of the
;; card on top of the discard pile.  Eights are wild -- play one any time
;; and name the suit that must follow.  If you cannot play, draw a card.
;; You are the South player; the others are simple AI.  This is the direct
;; ancestor of UNO, and the shedding engine generalises to climbing games
;; such as President.
;;
;; Cards are the package-standard cons (SUIT . RANK) with SUIT 0 spades,
;; 1 clubs, 2 diamonds, 3 hearts and RANK 0 (the Two) .. 12 (the Ace).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-eights-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst card-games-eights--wild 6 "Rank index of the wild Eight.")

(defcustom card-games-eights-players 3
  "Number of players in Crazy Eights, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defun card-games-eights-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref card-games-eights-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defsubst card-games-eights-red-p (card) "Return non-nil when CARD is red." (and card (card-games-red-suit-p (car card))))

(defun card-games-eights--value (card)
  "Return the scoring value of CARD held at the end of a hand."
  (cond ((= (cdr card) card-games-eights--wild) 50)
        ((>= (cdr card) 9) 10)            ; J Q K
        ((= (cdr card) 12) 1)             ; (Ace handled above by >=9? no)
        (t (+ 2 (cdr card)))))

(defun card-games-eights--deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (card-games-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defclass card-games-eights-game (card-games-game)
  ((vname :initform "Crazy Eights"))
  "A game of Crazy Eights.")

(defsubst card-games-eights--hand (game s) "Return seat S's hand in GAME." (aref (card-games-get game :hands) s))
(defsubst card-games-eights--set-hand (game s v) "Set seat S's hand in GAME to V." (aset (card-games-get game :hands) s v))
(defsubst card-games-eights--top (game) "Return the top card of GAME's discard pile." (car (card-games-get game :discard)))

(cl-defmethod card-games-eights--deal ((game card-games-eights-game))
  "Deal a fresh Crazy Eights hand into GAME."
  (let* ((n (max 2 (min 4 card-games-eights-players)))
         (deck (card-games-eights--deck))
         (per (if (= n 2) 7 5))
         (hands (make-vector n nil)))
    (dotimes (s n)
      (aset hands s (cl-loop repeat per collect (pop deck))))
    ;; turn up a starter that is not an eight
    (let ((start (pop deck)))
      (while (= (cdr start) card-games-eights--wild)
        (setq deck (append deck (list start)) start (pop deck)))
      (card-games-put game :discard (list start))
      (card-games-put game :suit (car start)))
    (card-games-put game :stock deck)
    (card-games-put game :hands hands)
    (card-games-put game :nplayers n)
    (card-games-put game :turn 0)
    (card-games-put game :phase 'play)
    (card-games-put game :passes 0)
    (card-games-put game :cursor 0)
    (unless (card-games-get game :scores) (card-games-put game :scores (make-vector n 0)))
    (card-games-put game :message "Match the suit or rank; eights are wild.  d draws.")
    game))

(cl-defmethod card-games-eights--legal-p ((game card-games-eights-game) card)
  "Return non-nil when CARD may be played onto GAME's discard now."
  (or (= (cdr card) card-games-eights--wild)
      (= (car card) (card-games-get game :suit))
      (= (cdr card) (cdr (card-games-eights--top game)))))

(defun card-games-eights--legal-moves (game s)
  "Return the cards in GAME seat S's hand that may be played now."
  (cl-remove-if-not (lambda (c) (card-games-eights--legal-p game c))
                    (card-games-eights--hand game s)))

(defun card-games-eights--best-suit (game s)
  "Return the suit most common in GAME seat S's hand (ignoring eights)."
  (let ((counts (make-vector 4 0)))
    (dolist (c (card-games-eights--hand game s))
      (unless (= (cdr c) card-games-eights--wild)
        (aset counts (car c) (1+ (aref counts (car c))))))
    (let ((best 0))
      (dotimes (i 4) (when (> (aref counts i) (aref counts best)) (setq best i)))
      best)))

(cl-defmethod card-games-eights--play ((game card-games-eights-game) s card &optional suit)
  "Have GAME seat S play CARD; SUIT names the next suit for a wild eight."
  (card-games-eights--set-hand game s (remove card (card-games-eights--hand game s)))
  (card-games-put game :discard (cons card (card-games-get game :discard)))
  (card-games-put game :suit (if (= (cdr card) card-games-eights--wild)
                         (or suit (card-games-eights--best-suit game s))
                       (car card)))
  (card-games-put game :passes 0)
  (if (null (card-games-eights--hand game s))
      (card-games-eights--finish game s)
    (card-games-put game :turn (mod (1+ s) (card-games-get game :nplayers)))))

(defun card-games-eights--draw-card (game s)
  "Move one card from GAME's stock to seat S's hand, recycling if needed.
Return the drawn card, or nil when none is available."
  (when (and (null (card-games-get game :stock)) (cdr (card-games-get game :discard)))
    (let ((top (car (card-games-get game :discard))))
      (card-games-put game :stock (card-games-shuffle (cdr (card-games-get game :discard))))
      (card-games-put game :discard (list top))))
  (let ((stock (card-games-get game :stock)))
    (when stock
      (let ((card (car stock)))
        (card-games-put game :stock (cdr stock))
        (card-games-eights--set-hand game s (cons card (card-games-eights--hand game s)))
        card))))

(cl-defmethod card-games-eights--finish ((game card-games-eights-game) winner)
  "Record WINNER going out in GAME and score the other hands against them."
  (let ((sum 0))
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s winner)
        (dolist (c (card-games-eights--hand game s))
          (setq sum (+ sum (card-games-eights--value c))))))
    (aset (card-games-get game :scores) winner (+ (aref (card-games-get game :scores) winner) sum))
    (card-games-put game :phase 'game-over)
    (card-games-put game :winner winner)
    (card-games-put game :message
            (format "%s goes out and scores %d.  Press n for a new deal."
                    (if (= winner 0) "You" (format "Player %d" winner)) sum))))

(cl-defmethod card-games-eights--ai-turn ((game card-games-eights-game) s)
  "Take GAME seat S's whole turn: play if able, else draw then play or pass."
  (let ((moves (card-games-eights--legal-moves game s)))
    (unless moves
      ;; draw up to a small limit looking for a play
      (let ((tries 0))
        (while (and (not moves) (< tries 60) (card-games-eights--draw-card game s))
          (setq moves (card-games-eights--legal-moves game s) tries (1+ tries)))))
    (if moves
        ;; prefer a non-eight of lowest value; keep eights for later
        (let* ((non (cl-remove-if (lambda (c) (= (cdr c) card-games-eights--wild)) moves))
               (pick (car (sort (copy-sequence (or non moves))
                                (lambda (a b) (< (card-games-eights--value a)
                                                 (card-games-eights--value b)))))))
          (card-games-eights--play game s pick))
      ;; truly stuck: pass
      (card-games-put game :passes (1+ (card-games-get game :passes)))
      (card-games-put game :turn (mod (1+ s) (card-games-get game :nplayers))))))

(defun card-games-eights--run (game)
  "Advance GAME's AI seats until the human's turn or the hand ends."
  (while (and (eq (card-games-get game :phase) 'play)
              (/= (card-games-get game :turn) 0)
              (< (card-games-get game :passes) (card-games-get game :nplayers)))
    (card-games-eights--ai-turn game (card-games-get game :turn)))
  (when (>= (card-games-get game :passes) (card-games-get game :nplayers))
    (card-games-eights--deadlock game)))

(cl-defmethod card-games-eights--deadlock ((game card-games-eights-game))
  "End a GAME hand in which everyone passed; lowest hand value wins."
  (let ((best 0) (bestv most-positive-fixnum))
    (dotimes (s (card-games-get game :nplayers))
      (let ((v (apply #'+ (mapcar #'card-games-eights--value (card-games-eights--hand game s)))))
        (when (< v bestv) (setq bestv v best s))))
    (card-games-eights--finish game best)))

;;;; UI

(defvar-local card-games-eights--game nil "The Crazy Eights game in the current buffer.")

(defcustom card-games-eights-svg-cards t
  "When non-nil, draw the hand as SVG on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-eights--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref card-games-eights-ranks (cdr card)) (car card)))

(defun card-games-eights--board-svg (game)
  "Return an SVG board for the Crazy Eights GAME."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap) (pad 16)
         (hand (card-games-eights--hand game 0)) (n (length hand))
         (cursor (card-games-get game :cursor))
         (top (card-games-eights--top game)) (suit (card-games-get game :suit))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (np (card-games-get game :nplayers)) (nstock (length (card-games-get game :stock)))
         (y-title 6) (y-info 26)
         (y-mid (+ y-info (* (1- np) 16) 14))
         (y-hand (+ y-mid h 42))
         (height (+ y-hand h 30))
         (width (max (+ fanw (* 2 pad)) 560))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family card-games-svg-font-family (and bold '(:font-weight "bold")))))
      (txt "Crazy Eights" pad (+ y-title 12) 13 t)
      (let ((yy (+ y-info 4)))
        (dotimes (s np)
          (unless (= s 0)
            (txt (format "Player %d: %d cards   (score %d)" s
                         (length (card-games-eights--hand game s)) (aref (card-games-get game :scores) s))
                 pad yy 12)
            (setq yy (+ yy 16)))))
      (card-games-svg-card svg pad y-mid :down (> nstock 0) :gap (= nstock 0))
      (txt (format "Stock %d" nstock) pad (+ y-mid h 13) 11)
      (let ((dx (+ pad w gap 28)) (sp (card-games-eights--spec top)))
        (card-games-svg-card svg dx y-mid :rank (car sp) :suit (cdr sp))
        (txt "Discard" dx (+ y-mid h 13) 11)
        (let ((sx (+ dx w gap 34))
              (col (if (card-games-red-suit-p suit) "#c0392b" "#2c3e50")))
          (txt "Suit in play" sx (- y-mid 4) 11)
          (svg-text svg (card-games-suit-glyph suit) :x (+ sx 12) :y (+ y-mid 46)
                    :font-size 44 :fill col :font-family card-games-svg-font-family)))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (card-games-eights--spec c)) (curp (= i cursor))
                (hintp (card-games-eights--legal-p game c)))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (card-games-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale)) 'card-games-regions (nreverse regions))))

(cl-defmethod card-games-render ((game card-games-eights-game))
  "Return a depiction of GAME: an SVG board if graphical, else text."
  (if (and card-games-eights-svg-cards (display-graphic-p))
      (card-games-eights--board-svg game)
    (card-games-eights--render-text game)))

(defun card-games-eights--render-text (game)
  "Return a plain-text depiction of GAME."
  (let* ((out (list)) (top (card-games-eights--top game))
         (hand (card-games-eights--hand game 0)) (cursor (card-games-get game :cursor)))
    (push (format "  Crazy Eights\n\n") out)
    (dotimes (s (card-games-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   (score %d)\n"
                      s (length (card-games-eights--hand game s))
                      (aref (card-games-get game :scores) s)) out)))
    (push (format "\n  Discard top: %s    Suit in play: %s    Stock: %d\n\n"
                  (let ((cs (card-games-eights-card-string top)))
                    (if (card-games-eights-red-p top) (propertize cs 'face 'card-games-red-suit) cs))
                  (card-games-suit-glyph (card-games-get game :suit))
                  (length (card-games-get game :stock)))
          out)
    (push (format "  Your hand (score %d):\n   " (aref (card-games-get game :scores) 0)) out)
    (if (and card-games-eights-svg-cards (display-graphic-p))
        (let ((hi '()) (i 0))
          (dolist (c hand) (when (card-games-eights--legal-p game c) (push i hi)) (setq i (1+ i)))
          (push (card-games-svg-hand-image (mapcar #'card-games-eights--spec hand)
                                   :cursor cursor :hints hi
                                   :overlap (if (> (length hand) 11)
                                                (max 0 (- card-games-svg-card-width 24)) 0)
                                   :region-tag 'hand)
                out))
      (let ((i 0))
        (dolist (c hand)
          (let ((cs (card-games-eights-card-string c)) (faces nil))
            (when (card-games-eights-red-p c) (push 'card-games-red-suit faces))
            (when (card-games-eights--legal-p game c) (push 'card-games-hint faces))
            (when (= i cursor) (push 'card-games-cursor faces))
            (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
          (setq i (1+ i)))))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-eights-game) action)
  "Apply a click ACTION on the hand to GAME G (a click also plays)."
  (pcase action
    (`(hand . ,i)
     (card-games-put g :cursor i)
     (when (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0))
       (card-games-eights-act)))
    (_ (cl-call-next-method))))

(defun card-games-eights--redisplay ()
  "Redraw the Crazy Eights buffer."
  (let ((game card-games-eights--game) (inhibit-read-only t))
    (setq card-games-current-game game card-games-redisplay-function #'card-games-eights--redisplay)
    (setq-local mode-line-process (format " [%s]" (card-games-get game :phase)))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-eights--cursor-card (game)
  "Return the card under GAME's cursor in the human hand."
  (nth (card-games-get game :cursor) (card-games-eights--hand game 0)))

(defun card-games-eights-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((game card-games-eights--game) (n (length (card-games-eights--hand game 0))))
    (when (> n 0) (card-games-put game :cursor (mod (1- (card-games-get game :cursor)) n)))
    (card-games-eights--redisplay)))

(defun card-games-eights-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((game card-games-eights--game) (n (length (card-games-eights--hand game 0))))
    (when (> n 0) (card-games-put game :cursor (mod (1+ (card-games-get game :cursor)) n)))
    (card-games-eights--redisplay)))

(defun card-games-eights--choose-suit (game)
  "Return a suit the human names for a wild eight in GAME."
  (if noninteractive (card-games-eights--best-suit game 0)
    (let* ((names (mapcar (lambda (i) (cons (aref card-games-suit-names i) i)) '(0 1 2 3)))
           (pick (completing-read "Name the suit: " (mapcar #'car names) nil t)))
      (cdr (assoc pick names)))))

(defun card-games-eights-act ()
  "Play the selected card if it is legal."
  (interactive)
  (let* ((game card-games-eights--game) (card (card-games-eights--cursor-card game)))
    (cond
     ((not (eq (card-games-get game :phase) 'play)) (card-games-put game :message "Press n for a new deal."))
     ((/= (card-games-get game :turn) 0) (card-games-put game :message "Not your turn."))
     ((null card) (card-games-put game :message "No card selected."))
     ((not (card-games-eights--legal-p game card))
      (card-games-put game :message "That card does not match — draw with d if stuck."))
     (t (let ((suit (and (= (cdr card) card-games-eights--wild) (card-games-eights--choose-suit game))))
          (card-games-eights--play game 0 card suit)
          (card-games-put game :cursor 0)
          (card-games-eights--run game))))
    (card-games-eights--redisplay)))

(defun card-games-eights-draw ()
  "Draw a card; if the stock cannot help, pass your turn."
  (interactive)
  (let ((game card-games-eights--game))
    (when (eq (card-games-get game :phase) 'play)
      (if (= (card-games-get game :turn) 0)
          (let ((card (card-games-eights--draw-card game 0)))
            (if card
                (card-games-put game :message (format "You drew %s." (card-games-eights-card-string card)))
              (card-games-put game :passes (1+ (card-games-get game :passes)))
              (card-games-put game :turn (mod 1 (card-games-get game :nplayers)))
              (card-games-put game :message "Nothing to draw — you pass.")
              (card-games-eights--run game)))
        (card-games-put game :message "Not your turn.")))
    (card-games-eights--redisplay)))

(defun card-games-eights-pass ()
  "Pass your turn (only sensible after drawing with an empty stock)."
  (interactive)
  (let ((game card-games-eights--game))
    (when (and (eq (card-games-get game :phase) 'play) (= (card-games-get game :turn) 0))
      (card-games-put game :passes (1+ (card-games-get game :passes)))
      (card-games-put game :turn (mod 1 (card-games-get game :nplayers)))
      (card-games-eights--run game))
    (card-games-eights--redisplay)))

(defun card-games-eights-new ()
  "Deal a fresh hand."
  (interactive)
  (card-games-eights--deal card-games-eights--game)
  (card-games-eights--run card-games-eights--game)
  (card-games-eights--redisplay))

(defun card-games-eights-redraw () "Redraw." (interactive) (card-games-eights--redisplay))
(defun card-games-eights-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows or click: choose/play  RET: play  d: draw  x: pass  +/-: size  n: new  g: redraw"))

(defvar card-games-eights-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-eights-left)
    (define-key map (kbd "<right>") #'card-games-eights-right)
    (define-key map (kbd "RET") #'card-games-eights-act)
    (define-key map (kbd "SPC") #'card-games-eights-act)
    (define-key map "d" #'card-games-eights-draw)
    (define-key map "x" #'card-games-eights-pass)
    (define-key map "n" #'card-games-eights-new)
    (define-key map "g" #'card-games-eights-redraw)
    (define-key map "?" #'card-games-eights-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-eights-mode'.")

(define-derived-mode card-games-eights-mode special-mode "Crazy8"
  "Major mode for Crazy Eights."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-eights ()
  "Play Crazy Eights against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Crazy Eights*")))
    (with-current-buffer buf
      (card-games-eights-mode)
      (setq card-games-eights--game (card-games-eights-game))
      (card-games-eights--deal card-games-eights--game)
      (card-games-eights--run card-games-eights--game)
      (card-games-eights--redisplay))
    (switch-to-buffer buf)))

(provide 'card-games-eights)
;;; card-games-eights.el ends here
