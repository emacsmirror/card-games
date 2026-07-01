;;; cg-eights.el --- Crazy Eights, a shedding card game  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.91
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
(require 'cg-core)
(require 'cg-svg)

(defconst cg-eights-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
  "Rank labels indexed 0 (Two) .. 12 (Ace).")

(defconst cg-eights--wild 6 "Rank index of the wild Eight.")

(defcustom cg-eights-players 3
  "Number of players in Crazy Eights, including you (2-4)."
  :type '(choice (const 2) (const 3) (const 4)) :group 'card-games)

(defun cg-eights-card-string (card)
  "Return a short string for CARD."
  (if (null card) "·"
    (concat (aref cg-eights-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defsubst cg-eights-red-p (card) (and card (cg-red-suit-p (car card))))

(defun cg-eights--value (card)
  "Return the scoring value of CARD held at the end of a hand."
  (cond ((= (cdr card) cg-eights--wild) 50)
        ((>= (cdr card) 9) 10)            ; J Q K
        ((= (cdr card) 12) 1)             ; (Ace handled above by >=9? no) 
        (t (+ 2 (cdr card)))))

(defun cg-eights--deck ()
  "Return a fresh shuffled 52-card deck."
  (random t)
  (cg-shuffle (cl-loop for s below 4 append
                       (cl-loop for r below 13 collect (cons s r)))))

(defclass cg-eights-game (cg-game)
  ((vname :initform "Crazy Eights"))
  "A game of Crazy Eights.")

(defsubst cg-eights--hand (game s) (aref (cg-get game :hands) s))
(defsubst cg-eights--set-hand (game s v) (aset (cg-get game :hands) s v))
(defsubst cg-eights--top (game) (car (cg-get game :discard)))

(cl-defmethod cg-eights--deal ((game cg-eights-game))
  "Deal a fresh Crazy Eights hand into GAME."
  (let* ((n (max 2 (min 4 cg-eights-players)))
         (deck (cg-eights--deck))
         (per (if (= n 2) 7 5))
         (hands (make-vector n nil)))
    (dotimes (s n)
      (aset hands s (cl-loop repeat per collect (pop deck))))
    ;; turn up a starter that is not an eight
    (let ((start (pop deck)))
      (while (= (cdr start) cg-eights--wild)
        (setq deck (append deck (list start)) start (pop deck)))
      (cg-put game :discard (list start))
      (cg-put game :suit (car start)))
    (cg-put game :stock deck)
    (cg-put game :hands hands)
    (cg-put game :nplayers n)
    (cg-put game :turn 0)
    (cg-put game :phase 'play)
    (cg-put game :passes 0)
    (cg-put game :cursor 0)
    (unless (cg-get game :scores) (cg-put game :scores (make-vector n 0)))
    (cg-put game :message "Match the suit or rank; eights are wild.  d draws.")
    game))

(cl-defmethod cg-eights--legal-p ((game cg-eights-game) card)
  "Return non-nil when CARD may be played onto the discard now."
  (or (= (cdr card) cg-eights--wild)
      (= (car card) (cg-get game :suit))
      (= (cdr card) (cdr (cg-eights--top game)))))

(defun cg-eights--legal-moves (game s)
  "Return the cards in seat S's hand that may be played now."
  (cl-remove-if-not (lambda (c) (cg-eights--legal-p game c))
                    (cg-eights--hand game s)))

(defun cg-eights--best-suit (game s)
  "Return the suit seat S holds most of (ignoring eights)."
  (let ((counts (make-vector 4 0)))
    (dolist (c (cg-eights--hand game s))
      (unless (= (cdr c) cg-eights--wild)
        (aset counts (car c) (1+ (aref counts (car c))))))
    (let ((best 0))
      (dotimes (i 4) (when (> (aref counts i) (aref counts best)) (setq best i)))
      best)))

(cl-defmethod cg-eights--play ((game cg-eights-game) s card &optional suit)
  "Have seat S play CARD; SUIT names the next suit for a wild eight."
  (cg-eights--set-hand game s (remove card (cg-eights--hand game s)))
  (cg-put game :discard (cons card (cg-get game :discard)))
  (cg-put game :suit (if (= (cdr card) cg-eights--wild)
                         (or suit (cg-eights--best-suit game s))
                       (car card)))
  (cg-put game :passes 0)
  (if (null (cg-eights--hand game s))
      (cg-eights--finish game s)
    (cg-put game :turn (mod (1+ s) (cg-get game :nplayers)))))

(defun cg-eights--draw-card (game s)
  "Move one card from the stock to seat S's hand, recycling if needed.
Return the drawn card, or nil when none is available."
  (when (and (null (cg-get game :stock)) (cdr (cg-get game :discard)))
    (let ((top (car (cg-get game :discard))))
      (cg-put game :stock (cg-shuffle (cdr (cg-get game :discard))))
      (cg-put game :discard (list top))))
  (let ((stock (cg-get game :stock)))
    (when stock
      (let ((card (car stock)))
        (cg-put game :stock (cdr stock))
        (cg-eights--set-hand game s (cons card (cg-eights--hand game s)))
        card))))

(cl-defmethod cg-eights--finish ((game cg-eights-game) winner)
  "Record WINNER going out and score the other hands against them."
  (let ((sum 0))
    (dotimes (s (cg-get game :nplayers))
      (unless (= s winner)
        (dolist (c (cg-eights--hand game s))
          (setq sum (+ sum (cg-eights--value c))))))
    (aset (cg-get game :scores) winner (+ (aref (cg-get game :scores) winner) sum))
    (cg-put game :phase 'game-over)
    (cg-put game :winner winner)
    (cg-put game :message
            (format "%s goes out and scores %d.  Press n for a new deal."
                    (if (= winner 0) "You" (format "Player %d" winner)) sum))))

(cl-defmethod cg-eights--ai-turn ((game cg-eights-game) s)
  "Take seat S's whole turn: play if able, otherwise draw then play or pass."
  (let ((moves (cg-eights--legal-moves game s)))
    (unless moves
      ;; draw up to a small limit looking for a play
      (let ((tries 0))
        (while (and (not moves) (< tries 60) (cg-eights--draw-card game s))
          (setq moves (cg-eights--legal-moves game s) tries (1+ tries)))))
    (if moves
        ;; prefer a non-eight of lowest value; keep eights for later
        (let* ((non (cl-remove-if (lambda (c) (= (cdr c) cg-eights--wild)) moves))
               (pick (car (sort (copy-sequence (or non moves))
                                (lambda (a b) (< (cg-eights--value a)
                                                 (cg-eights--value b)))))))
          (cg-eights--play game s pick))
      ;; truly stuck: pass
      (cg-put game :passes (1+ (cg-get game :passes)))
      (cg-put game :turn (mod (1+ s) (cg-get game :nplayers))))))

(defun cg-eights--run (game)
  "Advance AI seats until it is the human's turn or the hand ends."
  (while (and (eq (cg-get game :phase) 'play)
              (/= (cg-get game :turn) 0)
              (< (cg-get game :passes) (cg-get game :nplayers)))
    (cg-eights--ai-turn game (cg-get game :turn)))
  (when (>= (cg-get game :passes) (cg-get game :nplayers))
    (cg-eights--deadlock game)))

(cl-defmethod cg-eights--deadlock ((game cg-eights-game))
  "End a hand in which everyone passed; lowest hand value wins."
  (let ((best 0) (bestv most-positive-fixnum))
    (dotimes (s (cg-get game :nplayers))
      (let ((v (apply #'+ (mapcar #'cg-eights--value (cg-eights--hand game s)))))
        (when (< v bestv) (setq bestv v best s))))
    (cg-eights--finish game best)))

;;;; UI

(defvar-local cg-eights--game nil "The Crazy Eights game in the current buffer.")

(defcustom cg-eights-svg-cards t
  "When non-nil, draw the hand as SVG on a graphical display."
  :type 'boolean :group 'card-games)

(defun cg-eights--spec (card)
  "Return the cg-svg display spec (RANK-STRING . SUIT) for CARD."
  (cons (aref cg-eights-ranks (cdr card)) (car card)))

(defun cg-eights--board-svg (game)
  "Return an SVG board for the Crazy Eights GAME."
  (let* ((w cg-svg-card-width) (h cg-svg-card-height) (gap cg-svg-card-gap) (pad 16)
         (hand (cg-eights--hand game 0)) (n (length hand))
         (cursor (cg-get game :cursor))
         (top (cg-eights--top game)) (suit (cg-get game :suit))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (np (cg-get game :nplayers)) (nstock (length (cg-get game :stock)))
         (y-title 6) (y-info 26)
         (y-mid (+ y-info (* (1- np) 16) 14))
         (y-hand (+ y-mid h 42))
         (height (+ y-hand h 30))
         (width (max (+ fanw (* 2 pad)) 560))
         (svg (svg-create width height))
         (lc (cg-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family cg-svg-font-family (and bold '(:font-weight "bold")))))
      (txt "Crazy Eights" pad (+ y-title 12) 13 t)
      (let ((yy (+ y-info 4)))
        (dotimes (s np)
          (unless (= s 0)
            (txt (format "Player %d: %d cards   (score %d)" s
                         (length (cg-eights--hand game s)) (aref (cg-get game :scores) s))
                 pad yy 12)
            (setq yy (+ yy 16)))))
      (cg-svg-card svg pad y-mid :down (> nstock 0) :gap (= nstock 0))
      (txt (format "Stock %d" nstock) pad (+ y-mid h 13) 11)
      (let ((dx (+ pad w gap 28)) (sp (cg-eights--spec top)))
        (cg-svg-card svg dx y-mid :rank (car sp) :suit (cdr sp))
        (txt "Discard" dx (+ y-mid h 13) 11)
        (let ((sx (+ dx w gap 34))
              (col (if (cg-red-suit-p suit) "#c0392b" "#2c3e50")))
          (txt "Suit in play" sx (- y-mid 4) 11)
          (svg-text svg (cg-suit-glyph suit) :x (+ sx 12) :y (+ y-mid 46)
                    :font-size 44 :fill col :font-family cg-svg-font-family)))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (cg-eights--spec c)) (curp (= i cursor))
                (hintp (cg-eights--legal-p game c)))
            (cg-svg-card svg x y-hand :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt (or (cg-get game :message) "") pad (- height 8) 12))
    (propertize "*" 'display (cg-svg-image svg (cg-scale)) 'cg-regions (nreverse regions))))

(cl-defmethod cg-render ((game cg-eights-game))
  "Return a depiction of GAME: an SVG board if graphical, else text."
  (if (and cg-eights-svg-cards (display-graphic-p))
      (cg-eights--board-svg game)
    (cg-eights--render-text game)))

(defun cg-eights--render-text (game)
  "Return a plain-text depiction of GAME."
  (let* ((out (list)) (top (cg-eights--top game))
         (hand (cg-eights--hand game 0)) (cursor (cg-get game :cursor)))
    (push (format "  Crazy Eights\n\n") out)
    (dotimes (s (cg-get game :nplayers))
      (unless (= s 0)
        (push (format "  Player %d: %d cards   (score %d)\n"
                      s (length (cg-eights--hand game s))
                      (aref (cg-get game :scores) s)) out)))
    (push (format "\n  Discard top: %s    Suit in play: %s    Stock: %d\n\n"
                  (let ((cs (cg-eights-card-string top)))
                    (if (cg-eights-red-p top) (propertize cs 'face 'cg-red-suit) cs))
                  (cg-suit-glyph (cg-get game :suit))
                  (length (cg-get game :stock)))
          out)
    (push (format "  Your hand (score %d):\n   " (aref (cg-get game :scores) 0)) out)
    (if (and cg-eights-svg-cards (display-graphic-p))
        (let ((hi '()) (i 0))
          (dolist (c hand) (when (cg-eights--legal-p game c) (push i hi)) (setq i (1+ i)))
          (push (cg-svg-hand-image (mapcar #'cg-eights--spec hand)
                                   :cursor cursor :hints hi
                                   :overlap (if (> (length hand) 11)
                                                (max 0 (- cg-svg-card-width 24)) 0)
                                   :region-tag 'hand)
                out))
      (let ((i 0))
        (dolist (c hand)
          (let ((cs (cg-eights-card-string c)) (faces nil))
            (when (cg-eights-red-p c) (push 'cg-red-suit faces))
            (when (cg-eights--legal-p game c) (push 'cg-hint faces))
            (when (= i cursor) (push 'cg-cursor faces))
            (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
          (setq i (1+ i)))))
    (push (format "\n\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod cg-render-apply ((g cg-eights-game) action)
  "Apply a click ACTION on the hand to GAME G (a click also plays)."
  (pcase action
    (`(hand . ,i)
     (cg-put g :cursor i)
     (when (and (eq (cg-get g :phase) 'play) (= (cg-get g :turn) 0))
       (cg-eights-act)))
    (_ (cl-call-next-method))))

(defun cg-eights--redisplay ()
  "Redraw the Crazy Eights buffer."
  (let ((game cg-eights--game) (inhibit-read-only t))
    (setq cg-current-game game cg-redisplay-function #'cg-eights--redisplay)
    (setq-local mode-line-process (format " [%s]" (cg-get game :phase)))
    (erase-buffer) (insert (cg-render game)) (goto-char (point-min))))

(defun cg-eights--cursor-card (game)
  (nth (cg-get game :cursor) (cg-eights--hand game 0)))

(defun cg-eights-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((game cg-eights--game) (n (length (cg-eights--hand game 0))))
    (when (> n 0) (cg-put game :cursor (mod (1- (cg-get game :cursor)) n)))
    (cg-eights--redisplay)))

(defun cg-eights-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((game cg-eights--game) (n (length (cg-eights--hand game 0))))
    (when (> n 0) (cg-put game :cursor (mod (1+ (cg-get game :cursor)) n)))
    (cg-eights--redisplay)))

(defun cg-eights--choose-suit (game)
  "Return a suit the human names for a wild eight."
  (if noninteractive (cg-eights--best-suit game 0)
    (let* ((names (mapcar (lambda (i) (cons (aref cg-suit-names i) i)) '(0 1 2 3)))
           (pick (completing-read "Name the suit: " (mapcar #'car names) nil t)))
      (cdr (assoc pick names)))))

(defun cg-eights-act ()
  "Play the selected card if it is legal."
  (interactive)
  (let* ((game cg-eights--game) (card (cg-eights--cursor-card game)))
    (cond
     ((not (eq (cg-get game :phase) 'play)) (cg-put game :message "Press n for a new deal."))
     ((/= (cg-get game :turn) 0) (cg-put game :message "Not your turn."))
     ((null card) (cg-put game :message "No card selected."))
     ((not (cg-eights--legal-p game card))
      (cg-put game :message "That card does not match — draw with d if stuck."))
     (t (let ((suit (and (= (cdr card) cg-eights--wild) (cg-eights--choose-suit game))))
          (cg-eights--play game 0 card suit)
          (cg-put game :cursor 0)
          (cg-eights--run game))))
    (cg-eights--redisplay)))

(defun cg-eights-draw ()
  "Draw a card; if the stock cannot help, pass your turn."
  (interactive)
  (let ((game cg-eights--game))
    (when (eq (cg-get game :phase) 'play)
      (if (= (cg-get game :turn) 0)
          (let ((card (cg-eights--draw-card game 0)))
            (if card
                (cg-put game :message (format "You drew %s." (cg-eights-card-string card)))
              (cg-put game :passes (1+ (cg-get game :passes)))
              (cg-put game :turn (mod 1 (cg-get game :nplayers)))
              (cg-put game :message "Nothing to draw — you pass.")
              (cg-eights--run game)))
        (cg-put game :message "Not your turn.")))
    (cg-eights--redisplay)))

(defun cg-eights-pass ()
  "Pass your turn (only sensible after drawing with an empty stock)."
  (interactive)
  (let ((game cg-eights--game))
    (when (and (eq (cg-get game :phase) 'play) (= (cg-get game :turn) 0))
      (cg-put game :passes (1+ (cg-get game :passes)))
      (cg-put game :turn (mod 1 (cg-get game :nplayers)))
      (cg-eights--run game))
    (cg-eights--redisplay)))

(defun cg-eights-new ()
  "Deal a fresh hand."
  (interactive)
  (cg-eights--deal cg-eights--game)
  (cg-eights--run cg-eights--game)
  (cg-eights--redisplay))

(defun cg-eights-redraw () "Redraw." (interactive) (cg-eights--redisplay))
(defun cg-eights-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows or click: choose/play  RET: play  d: draw  x: pass  +/-: size  n: new  g: redraw"))

(defvar cg-eights-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cg-card-click)
    (define-key map "+" #'cg-card-zoom-in)
    (define-key map "=" #'cg-card-zoom-in)
    (define-key map "-" #'cg-card-zoom-out)
    (define-key map "0" #'cg-card-zoom-reset)
    (define-key map (kbd "<left>")  #'cg-eights-left)
    (define-key map (kbd "<right>") #'cg-eights-right)
    (define-key map (kbd "RET") #'cg-eights-act)
    (define-key map (kbd "SPC") #'cg-eights-act)
    (define-key map "d" #'cg-eights-draw)
    (define-key map "x" #'cg-eights-pass)
    (define-key map "n" #'cg-eights-new)
    (define-key map "g" #'cg-eights-redraw)
    (define-key map "?" #'cg-eights-help)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-eights-mode'.")

(define-derived-mode cg-eights-mode special-mode "Crazy8"
  "Major mode for Crazy Eights."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-eights ()
  "Play Crazy Eights against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Crazy Eights*")))
    (with-current-buffer buf
      (cg-eights-mode)
      (setq cg-eights--game (cg-eights-game))
      (cg-eights--deal cg-eights--game)
      (cg-eights--run cg-eights--game)
      (cg-eights--redisplay))
    (switch-to-buffer buf)))

(provide 'cg-eights)
;;; cg-eights.el ends here
