;;; card-games-solitaire.el --- Tableau solitaires (Klondike, FreeCell, Spider, Yukon)  -*- lexical-binding: t; -*-

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

;; A shared engine for tableau solitaires, with four games built on it:
;;
;;   `card-games-klondike' -- the classic "Solitaire": seven columns, a stock and
;;                    waste, build the foundations up by suit from the Ace.
;;   `card-games-freecell' -- all cards dealt face up, four free cells, no stock;
;;                    a game of nearly pure skill.
;;   `card-games-spider'   -- two decks, ten columns; build down regardless of suit
;;                    but only same-suit runs move; clear eight K..A runs.
;;   `card-games-yukon'    -- Klondike's layout, all face up, move any buried group.
;;
;; Cards are the package-standard cons (SUIT . RANK) with SUIT 0 spades,
;; 1 clubs, 2 diamonds, 3 hearts and RANK 0 Ace .. 12 King.  Each tableau
;; column is a list ordered bottom (screen top) to top (the accessible
;; card); a per-column face-down count tracks the hidden prefix.
;;
;; Play is by keyboard: move the cursor between piles with the arrow keys
;; and press RET to pick up the movable run from a pile, then RET again on
;; a destination to drop it.  `f' sends a card to a foundation, `a' auto-
;; plays everything it can, and the stock pile deals or recycles on RET.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-svg)
(require 'card-games-render)

;;;; Cards

(defconst card-games-sol-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0..12 (Ace through King).")

(defun card-games-sol-card-string (card &optional down)
  "Return a short string for CARD.
With DOWN non-nil, draw a face-down back instead.  A nil CARD draws an
empty-slot dot."
  (cond (down "##")
        ((null card) "·")
        (t (concat (aref card-games-sol-ranks (cdr card)) (card-games-suit-glyph (car card))))))

(defsubst card-games-sol-red-p (card)
  "Return non-nil when CARD is a red suit."
  (and card (card-games-red-suit-p (car card))))

(defun card-games-sol--make-deck (ndecks)
  "Return a shuffled list of NDECKS standard 52-card decks."
  (random t)
  (let ((cards nil))
    (dotimes (_ ndecks)
      (dotimes (s 4)
        (dotimes (r 13)
          (push (cons s r) cards))))
    (card-games-shuffle cards)))

;;;; Game classes

(defclass card-games-solitaire-game (card-games-game)
  ((ncols     :initform 7   :documentation "Number of tableau columns.")
   (ndecks    :initform 1   :documentation "Number of 52-card decks used.")
   (nfound    :initform 4   :documentation "Number of foundation piles.")
   (nfree     :initform 0   :documentation "Number of free cells.")
   (has-stock :initform nil :documentation "Whether a stock pile exists.")
   (has-waste :initform nil :documentation "Whether a waste pile exists.")
   (build     :initform 'alt :documentation "Tableau placement rule: alt, suit, any.")
   (run-rule  :initform 'alt :documentation "Movable-run cohesion: alt, suit, any.")
   (empty-rule :initform 'king :documentation "Empty-column rule: king or any.")
   (redeal    :initform t   :documentation "Whether an empty stock recycles the waste.")
   (draw      :initform 1   :documentation "Cards turned from the stock to the waste.")
   (target-sets :initform 8 :documentation "Completed runs to win when NFOUND is 0.")
   (base      :initform 0   :documentation "Foundation base rank (0 = Ace).")
   (wrap      :initform nil :documentation "Whether foundations wrap King to Ace.")
   (has-reserve :initform nil :documentation "Whether a reserve pile exists (Canfield).")
   (vname     :initform "Solitaire" :documentation "Display name."))
  "Abstract base for tableau solitaires."
  :abstract t)

(defclass card-games-klondike-game (card-games-solitaire-game)
  ((has-stock :initform t) (has-waste :initform t)
   (vname :initform "Klondike"))
  "Klondike: seven columns, stock and waste, foundations up by suit.")

(defclass card-games-freecell-game (card-games-solitaire-game)
  ((ncols :initform 8) (nfree :initform 4) (empty-rule :initform 'any)
   (vname :initform "FreeCell"))
  "FreeCell: eight columns dealt face up, four free cells, no stock.")

(defclass card-games-yukon-game (card-games-solitaire-game)
  ((run-rule :initform 'any) (vname :initform "Yukon"))
  "Yukon: Klondike layout dealt mostly face up; move any buried group.")

(defclass card-games-spider-game (card-games-solitaire-game)
  ((ncols :initform 10) (ndecks :initform 2) (nfound :initform 0)
   (has-stock :initform t) (build :initform 'any) (run-rule :initform 'suit)
   (empty-rule :initform 'any) (vname :initform "Spider"))
  "Spider: two decks, ten columns; clear eight K..A same-suit runs.")

;;;; Rules (predicates)

(defun card-games-sol--diff-color-p (a b)
  "Return non-nil when cards A and B are of opposite colours."
  (not (eq (card-games-red-suit-p (car a)) (card-games-red-suit-p (car b)))))

(cl-defmethod card-games-sol--link-p ((game card-games-solitaire-game) upper lower)
  "Return non-nil when LOWER may rest on UPPER within a GAME run."
  (pcase (oref game run-rule)
    ('any t)
    ('suit (and (= (cdr lower) (1- (cdr upper))) (= (car lower) (car upper))))
    (_     (and (= (cdr lower) (1- (cdr upper))) (card-games-sol--diff-color-p upper lower)))))

(cl-defmethod card-games-sol--place-p ((game card-games-solitaire-game) top card)
  "Return non-nil when CARD may be placed on a GAME column topped by TOP."
  (pcase (oref game build)
    ('any  (= (cdr card) (1- (cdr top))))
    ('suit (and (= (cdr card) (1- (cdr top))) (= (car card) (car top))))
    (_     (and (= (cdr card) (1- (cdr top))) (card-games-sol--diff-color-p top card)))))

(cl-defmethod card-games-sol--empty-accepts ((game card-games-solitaire-game) card)
  "Return non-nil when CARD may be placed on an empty GAME column."
  (pcase (oref game empty-rule)
    ('king (= (cdr card) 12))
    (_ t)))

;;;; Layout and dealing

(cl-defgeneric card-games-sol--layout (game)
  "Return GAME's list of (DOWN . UP) card counts, one per tableau column.")

(cl-defmethod card-games-sol--layout ((_ card-games-klondike-game))
  "Return the Klondike tableau layout (DOWN . UP per column)."
  (cl-loop for i below 7 collect (cons i 1)))
(cl-defmethod card-games-sol--layout ((_ card-games-yukon-game))
  "Return the Yukon tableau layout (DOWN . UP per column)."
  (cons (cons 0 1) (cl-loop for i from 1 below 7 collect (cons i 5))))
(cl-defmethod card-games-sol--layout ((_ card-games-freecell-game))
  "Return the FreeCell tableau layout (DOWN . UP per column)."
  (append (make-list 4 (cons 0 7)) (make-list 4 (cons 0 6))))
(cl-defmethod card-games-sol--layout ((_ card-games-spider-game))
  "Return the Spider tableau layout (DOWN . UP per column)."
  (append (make-list 4 (cons 5 1)) (make-list 6 (cons 4 1))))

(cl-defmethod card-games-sol--deal ((game card-games-solitaire-game))
  "Deal a fresh layout into GAME and initialise its environment."
  (let* ((deck (card-games-sol--make-deck (oref game ndecks)))
         (nc (oref game ncols))
         (layout (card-games-sol--layout game))
         (tableau (make-vector nc nil))
         (down (make-vector nc 0)))
    (dotimes (c nc)
      (let* ((spec (nth c layout))
             (col nil))
        (dotimes (_ (+ (car spec) (cdr spec)))
          (push (pop deck) col))
        (aset tableau c (nreverse col))
        (aset down c (car spec))))
    (card-games-put game :tableau tableau)
    (card-games-put game :down down)
    (card-games-put game :found (make-vector (oref game nfound) nil))
    (card-games-put game :free (make-vector (oref game nfree) nil))
    (card-games-put game :stock deck)
    (card-games-put game :waste nil)
    (card-games-put game :sets 0)
    (card-games-put game :moves 0)
    (card-games-put game :cursor 0)
    (card-games-put game :sel nil)
    (card-games-put game :sel-n 0)
    (card-games-put game :history nil)
    (card-games-put game :message
            (format "%s.  Arrows move; RET picks up/drops; f=foundation; a=auto; ?=help."
                    (oref game vname)))
    game))

;;;; Spots (the cursor visits piles)

(cl-defmethod card-games-sol--spots ((game card-games-solitaire-game))
  "Return GAME's ordered (TYPE . INDEX) piles the cursor can visit."
  (append
   (when (oref game has-stock) '((stock . 0)))
   (when (oref game has-waste) '((waste . 0)))
   (when (oref game has-reserve) '((reserve . 0)))
   (cl-loop for i below (oref game nfree) collect (cons 'free i))
   (cl-loop for i below (oref game nfound) collect (cons 'found i))
   (cl-loop for i below (oref game ncols) collect (cons 'col i))))

(defun card-games-sol--cur-spot (game)
  "Return GAME's (TYPE . INDEX) spot currently under the cursor."
  (nth (card-games-get game :cursor) (card-games-sol--spots game)))

;;;; Pile access helpers

(defun card-games-sol--col (game c) "Column C of GAME (a list)." (aref (card-games-get game :tableau) c))
(defun card-games-sol--set-col (game c v) "Set column C in GAME to V." (aset (card-games-get game :tableau) c v))
(defun card-games-sol--down (game c) "Return the face-down count of column C in GAME." (aref (card-games-get game :down) c))
(defun card-games-sol--set-down (game c v) "Set the face-down count of column C in GAME to V." (aset (card-games-get game :down) c v))

(defun card-games-sol--col-top (game c)
  "Return the top (accessible) card of GAME column C, or nil."
  (car (last (card-games-sol--col game c))))

(defun card-games-sol--exposed (game c)
  "Return the face-up cards of GAME column C (bottom..top order)."
  (nthcdr (card-games-sol--down game c) (card-games-sol--col game c)))

(cl-defmethod card-games-sol--top-run ((game card-games-solitaire-game) c)
  "Return the longest movable run from GAME column C's top (bottom..top)."
  (let ((top->bottom (reverse (card-games-sol--exposed game c))))
    (if (null top->bottom)
        nil
      (let ((run (list (car top->bottom)))
            (prev (car top->bottom)))
        (catch 'done
          (dolist (card (cdr top->bottom))
            (if (card-games-sol--link-p game card prev)
                (progn (push card run) (setq prev card))
              (throw 'done nil))))
        run))))

(defun card-games-sol--spot-top (game spot)
  "Return the top card available at GAME SPOT, or nil."
  (pcase (car spot)
    ('col   (card-games-sol--col-top game (cdr spot)))
    ('waste (car (last (card-games-get game :waste))))
    ('free  (aref (card-games-get game :free) (cdr spot)))
    ('found (car (last (aref (card-games-get game :found) (cdr spot)))))
    ('reserve (car (last (card-games-get game :reserve))))
    (_ nil)))

;;;; Foundations

(defun card-games-sol--found-accepts (game i card)
  "Return non-nil when CARD may go onto foundation I of GAME."
  (and card
       (let ((f (aref (card-games-get game :found) i)))
         (if (null f)
             (= (cdr card) (oref game base))   ; empty foundation takes the base rank
           (let* ((top (car (last f)))
                  (need (if (oref game wrap) (mod (1+ (cdr top)) 13) (1+ (cdr top)))))
             (and (= (car card) (car top))
                  (= (cdr card) need)))))))

(defun card-games-sol--found-for (game card)
  "Return the index of a GAME foundation that would accept CARD, or nil."
  (cl-loop for i below (oref game nfound)
           when (card-games-sol--found-accepts game i card) return i))

;;;; Move primitives

(defun card-games-sol--snapshot (game)
  "Push a deep-ish copy of GAME's mutable state onto the undo history."
  (let ((tab (card-games-get game :tableau))
        (frv (card-games-get game :found))
        (fre (card-games-get game :free)))
    (card-games-put game :history
            (cons (list (vconcat (mapcar #'copy-sequence tab))
                        (copy-sequence (card-games-get game :down))
                        (vconcat (mapcar #'copy-sequence frv))
                        (copy-sequence fre)
                        (copy-sequence (card-games-get game :stock))
                        (copy-sequence (card-games-get game :waste))
                        (card-games-get game :sets)
                        (card-games-get game :moves)
                        (copy-sequence (card-games-get game :reserve)))
                  (card-games-get game :history)))))

(defun card-games-sol--restore (game)
  "Pop and restore the most recent undo snapshot of GAME, if any."
  (let ((h (card-games-get game :history)))
    (when h
      (cl-destructuring-bind (tab down frv fre stock waste sets moves reserve) (car h)
        (card-games-put game :tableau tab)
        (card-games-put game :down down)
        (card-games-put game :found frv)
        (card-games-put game :free fre)
        (card-games-put game :stock stock)
        (card-games-put game :waste waste)
        (card-games-put game :sets sets)
        (card-games-put game :moves moves)
        (card-games-put game :reserve reserve))
      (card-games-put game :history (cdr h))
      (card-games-put game :sel nil)
      t)))

(defun card-games-sol--flip (game c)
  "Flip the top of GAME column C face up if it is face down."
  (let ((len (length (card-games-sol--col game c)))
        (d (card-games-sol--down game c)))
    (when (and (> len 0) (>= d len))
      (card-games-sol--set-down game c (1- len)))))

(defun card-games-sol--take (game spot n)
  "Remove and return the top N cards (bottom..top) from GAME SPOT."
  (pcase (car spot)
    ('col (let* ((c (cdr spot)) (col (card-games-sol--col game c))
                 (run (last col n)))
            (card-games-sol--set-col game c (butlast col n))
            (let ((len (length (card-games-sol--col game c))))
              (when (> (card-games-sol--down game c) len)
                (card-games-sol--set-down game c len)))
            (card-games-sol--flip game c)
            run))
    ('waste (let ((w (card-games-get game :waste)))
              (card-games-put game :waste (butlast w 1)) (last w 1)))
    ('free (let ((card (aref (card-games-get game :free) (cdr spot))))
             (aset (card-games-get game :free) (cdr spot) nil) (list card)))
    ('found (let* ((i (cdr spot)) (f (aref (card-games-get game :found) i)))
              (aset (card-games-get game :found) i (butlast f 1)) (last f 1)))
    ('reserve (let ((r (card-games-get game :reserve)))
                (card-games-put game :reserve (butlast r 1)) (last r 1)))
    (_ nil)))

(defun card-games-sol--can-drop (game spot cards)
  "Return non-nil when run CARDS (bottom..top) may drop on GAME SPOT."
  (and cards
       (pcase (car spot)
         ('col (let* ((c (cdr spot)) (top (card-games-sol--col-top game c)))
                 (if top
                     (card-games-sol--place-p game top (car cards))
                   (card-games-sol--empty-accepts game (car cards)))))
         ('found (and (= 1 (length cards))
                      (card-games-sol--found-accepts game (cdr spot) (car cards))))
         ('free (and (= 1 (length cards))
                     (null (aref (card-games-get game :free) (cdr spot)))))
         (_ nil))))

(defun card-games-sol--drop (game spot cards)
  "Place run CARDS (bottom..top) onto GAME SPOT."
  (pcase (car spot)
    ('col (let ((c (cdr spot)))
            (card-games-sol--set-col game c (append (card-games-sol--col game c) cards))))
    ('found (let ((i (cdr spot)))
              (aset (card-games-get game :found) i
                    (append (aref (card-games-get game :found) i) cards))))
    ('free (aset (card-games-get game :free) (cdr spot) (car cards)))))

;;;; Spider: complete-run removal

(cl-defmethod card-games-sol--harvest ((game card-games-solitaire-game))
  "Remove a complete K..A same-suit run from a GAME column top; tally it.
Only games without foundations (NFOUND 0: Spider, Scorpion) harvest runs."
  (when (= 0 (oref game nfound))
   (dotimes (c (oref game ncols))
    (let* ((col (card-games-sol--col game c))
           (exp (card-games-sol--exposed game c)))
      (when (>= (length exp) 13)
        (let ((run (last exp 13)) (ok t) (suit (car (nth 0 (last exp 13)))))
          (cl-loop for k below 13
                   for card = (nth k run)
                   unless (and (= (car card) suit) (= (cdr card) (- 12 k)))
                   do (setq ok nil))
          (when ok
            (card-games-sol--set-col game c (butlast col 13))
            (let ((len (length (card-games-sol--col game c))))
              (when (> (card-games-sol--down game c) len) (card-games-sol--set-down game c len)))
            (card-games-sol--flip game c)
            (card-games-put game :sets (1+ (card-games-get game :sets))))))))))

;;;; Stock action

(defcustom card-games-sol-klondike-draw 1
  "Number of cards turned from the stock to the waste in Klondike."
  :type '(choice (const :tag "Draw one" 1) (const :tag "Draw three" 3))
  :group 'card-games)

(cl-defmethod card-games-sol--stock-action ((game card-games-solitaire-game))
  "Deal GAME's `draw' cards to the waste, recycling the waste when `redeal'."
  (if (not (oref game has-waste))
      (card-games-put game :message "No stock to deal.")
    (card-games-sol--snapshot game)
    (let ((stock (card-games-get game :stock)) (waste (card-games-get game :waste)))
      (if stock
          (let ((n (min (oref game draw) (length stock))))
            (dotimes (_ n)
              (setq waste (append waste (last stock 1)))
              (setq stock (butlast stock 1)))
            (card-games-put game :stock stock) (card-games-put game :waste waste)
            (card-games-put game :message "Dealt from stock."))
        (if (and (oref game redeal) waste)
            (progn (card-games-put game :stock (reverse waste)) (card-games-put game :waste nil)
                   (card-games-put game :message "Recycled the waste into the stock."))
          (card-games-put game :message "The stock is empty."))))))

(cl-defmethod card-games-sol--stock-action ((game card-games-klondike-game))
  "Deal GAME's Klondike stock to the waste, recycling when empty."
  (card-games-sol--snapshot game)
  (let ((stock (card-games-get game :stock)) (waste (card-games-get game :waste)))
    (if stock
        (let ((n (min card-games-sol-klondike-draw (length stock))))
          (dotimes (_ n)
            (setq waste (append waste (last stock 1)))
            (setq stock (butlast stock 1)))
          (card-games-put game :stock stock) (card-games-put game :waste waste)
          (card-games-put game :message "Dealt from stock."))
      (if waste
          (progn (card-games-put game :stock (reverse waste)) (card-games-put game :waste nil)
                 (card-games-put game :message "Recycled the waste into the stock."))
        (card-games-put game :message "Stock and waste are both empty.")))))

(cl-defmethod card-games-sol--stock-action ((game card-games-spider-game))
  "Deal a Spider row into GAME: one card onto every column."
  (let ((stock (card-games-get game :stock)))
    (cond
     ((null stock) (card-games-put game :message "The stock is empty."))
     ((cl-loop for c below (oref game ncols)
               thereis (null (card-games-sol--col game c)))
      (card-games-put game :message "Fill every column before dealing from the stock."))
     (t (card-games-sol--snapshot game)
        (dotimes (c (oref game ncols))
          (card-games-sol--set-col game c (append (card-games-sol--col game c) (last stock 1)))
          (setq stock (butlast stock 1)))
        (card-games-put game :stock stock)
        (card-games-sol--harvest game)
        (card-games-put game :message "Dealt a row from the stock.")))))

;;;; Win

(cl-defmethod card-games-won-p ((game card-games-solitaire-game))
  "Return non-nil when GAME is solved."
  (if (= 0 (oref game nfound))
      (>= (card-games-get game :sets) (oref game target-sets))
    (cl-every (lambda (f) (= 13 (length f)))
              (append (card-games-get game :found) nil))))

;;;; Interaction

(defvar-local card-games-sol--game nil "The solitaire game in the current buffer.")

(cl-defmethod card-games-sol--selectable ((game card-games-solitaire-game) spot)
  "Return the run (bottom..top) GAME would pick up from SPOT, or nil."
  (pcase (car spot)
    ('col (card-games-sol--top-run game (cdr spot)))
    ('waste (let ((c (card-games-sol--spot-top game spot))) (and c (list c))))
    ('free (let ((c (card-games-sol--spot-top game spot))) (and c (list c))))
    ('found (let ((c (card-games-sol--spot-top game spot))) (and c (list c))))
    ('reserve (let ((c (card-games-sol--spot-top game spot))) (and c (list c))))
    (_ nil)))

(defun card-games-sol-act (&optional count)
  "Pick up from, or drop onto, the pile under the cursor.
With prefix COUNT, pick up exactly COUNT cards from a column."
  (interactive "P")
  (let* ((game card-games-sol--game)
         (spot (card-games-sol--cur-spot game))
         (sel (card-games-get game :sel)))
    (cond
     ((eq (car spot) 'stock)
      (card-games-put game :sel nil)
      (card-games-sol--stock-action game))
     ((null sel)
      (let ((run (card-games-sol--selectable game spot)))
        (cond
         ((null run) (card-games-put game :message "Nothing to pick up there."))
         (t (when (and count (eq (car spot) 'col))
              (setq run (last run (min (prefix-numeric-value count) (length run)))))
            (card-games-put game :sel spot)
            (card-games-put game :sel-n (length run))
            (card-games-put game :message
                    (format "Picked up %d card%s.  RET on a destination."
                            (length run) (if (= 1 (length run)) "" "s")))))))
     ((equal sel spot)
      (card-games-put game :sel nil) (card-games-put game :message "Cancelled."))
     (t
      (let* ((n (card-games-get game :sel-n))
             (cards (last (pcase (car sel)
                            ('col (card-games-sol--col game (cdr sel)))
                            ('waste (card-games-get game :waste))
                            ('found (aref (card-games-get game :found) (cdr sel)))
                            ('free (list (aref (card-games-get game :free) (cdr sel)))))
                          n)))
        (if (card-games-sol--can-drop game spot cards)
            (progn (card-games-sol--snapshot game)
                   (card-games-sol--take game sel n)
                   (card-games-sol--drop game spot cards)
                   (card-games-put game :moves (1+ (card-games-get game :moves)))
                   (card-games-sol--harvest game)
                   (card-games-put game :sel nil)
                   (card-games-put game :message "Moved."))
          (card-games-put game :sel nil)
          (card-games-put game :message "That move is not allowed."))))))
  (card-games-sol--after card-games-sol--game))

(defun card-games-sol-to-foundation ()
  "Send the top card of the pile under the cursor to a foundation."
  (interactive)
  (let* ((game card-games-sol--game)
         (spot (card-games-sol--cur-spot game))
         (card (card-games-sol--spot-top game spot)))
    (if (and card (memq (car spot) '(col waste free reserve)))
        (let ((i (card-games-sol--found-for game card)))
          (if i
              (progn (card-games-sol--snapshot game)
                     (card-games-sol--take game spot 1)
                     (card-games-sol--drop game (cons 'found i) (list card))
                     (card-games-put game :moves (1+ (card-games-get game :moves)))
                     (card-games-put game :sel nil)
                     (card-games-put game :message "To the foundation."))
            (card-games-put game :message "No foundation will take that card.")))
      (card-games-put game :message "Nothing to send to a foundation.")))
  (card-games-sol--after card-games-sol--game))

(defun card-games-sol-auto ()
  "Repeatedly send any eligible card to the foundations."
  (interactive)
  (let ((game card-games-sol--game) (moved 0))
    (when (> (oref game nfound) 0)
      (card-games-sol--snapshot game)
      (let (again)
        (cl-loop
         do (setq again nil)
         (dolist (spot (card-games-sol--spots game))
           (when (memq (car spot) '(col waste free reserve))
             (let* ((card (card-games-sol--spot-top game spot))
                    (i (and card (card-games-sol--found-for game card))))
               (when i
                 (card-games-sol--take game spot 1)
                 (card-games-sol--drop game (cons 'found i) (list card))
                 (setq moved (1+ moved) again t)))))
         while again))
      (if (> moved 0)
          (progn (card-games-put game :moves (+ moved (card-games-get game :moves)))
                 (card-games-put game :sel nil)
                 (card-games-put game :message (format "Auto-played %d card%s."
                                               moved (if (= 1 moved) "" "s"))))
        (card-games-put game :history (cdr (card-games-get game :history)))
        (card-games-put game :message "Nothing to auto-play."))))
  (card-games-sol--after card-games-sol--game))

(defun card-games-sol-undo ()
  "Undo the last move."
  (interactive)
  (let ((game card-games-sol--game))
    (if (card-games-sol--restore game)
        (card-games-put game :message "Undid a move.")
      (card-games-put game :message "Nothing to undo."))
    (card-games-sol--redisplay)))

(defun card-games-sol--move (delta)
  "Move the cursor by DELTA spots."
  (let* ((game card-games-sol--game)
         (n (length (card-games-sol--spots game)))
         (cur (card-games-get game :cursor)))
    (card-games-put game :cursor (mod (+ cur delta) n))
    (card-games-sol--redisplay)))

(defun card-games-sol-left () "Move cursor left." (interactive) (card-games-sol--move -1))
(defun card-games-sol-right () "Move cursor right." (interactive) (card-games-sol--move 1))
(defun card-games-sol-up () "Move cursor left (previous pile)." (interactive) (card-games-sol--move -1))
(defun card-games-sol-down () "Move cursor right (next pile)." (interactive) (card-games-sol--move 1))

(defun card-games-sol--after (game)
  "Fill empty columns from the reserve, redisplay GAME, and announce a win."
  (card-games-sol--autofill game)
  (card-games-sol--redisplay)
  (when (card-games-won-p game)
    (card-games-put game :message "You won!  Press n for a new game.")
    (card-games-sol--redisplay)
    (message "Solved!  Well played.")))

(defun card-games-sol-new ()
  "Start a fresh deal of the same game."
  (interactive)
  (let ((game card-games-sol--game))
    (card-games-sol--deal game)
    (card-games-sol--redisplay)))

(defun card-games-sol-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows: move  RET: pick up/drop  f: to foundation  a: auto  u: undo  n: new  g: redraw"))

(defun card-games-sol-redraw () "Redraw the board." (interactive) (card-games-sol--redisplay))

;;;; Rendering (console)

(defun card-games-sol--render-card (card down sel cursor)
  "Return a propertized 3-column cell for CARD (DOWN, SEL, CURSOR flags)."
  (let* ((s (card-games-sol-card-string card down))
         (faces nil))
    (when (and card (not down) (card-games-sol-red-p card)) (push 'card-games-red-suit faces))
    (when down (push 'card-games-gap faces))
    (when sel (push 'card-games-hint faces))
    (when cursor (push 'card-games-cursor faces))
    (propertize (format "%3s " s) 'face (or faces 'default))))

(defcustom card-games-sol-svg-cards t
  "When non-nil, draw the solitaire board as SVG on a graphical display."
  :type 'boolean :group 'card-games)

(defun card-games-sol--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD, or nil."
  (and card (cons (aref card-games-sol-ranks (cdr card)) (car card))))

(defun card-games-sol--svg (game)
  "Return (DISPLAY . REGIONS) for an SVG board of solitaire GAME.
DISPLAY is a propertized one-image string; REGIONS is a click map of
\(RECT . SPOT) entries, RECT being (X Y W H) in unscaled image pixels."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height)
         (pad 12) (gap card-games-svg-card-gap) (colgap 8) (vdown 12) (vup 26)
         (ncols (oref game ncols))
         (cur-spot (card-games-sol--cur-spot game))
         (sel (card-games-get game :sel)) (sel-n (or (card-games-get game :sel-n) 0))
         (lc (card-games-color 'shadow :foreground "gray40"))
         (regions '()) (slots '()))
    (when (oref game has-stock)
      (push (list (format "Stock(%d)" (length (card-games-get game :stock)))
                  nil (and (card-games-get game :stock) t) (equal cur-spot '(stock . 0))
                  '(stock . 0)) slots))
    (when (oref game has-waste)
      (push (list "Waste" (card-games-sol--spec (car (last (card-games-get game :waste)))) nil
                  (equal cur-spot '(waste . 0)) '(waste . 0)) slots))
    (when (oref game has-reserve)
      (push (list (format "Resv(%d)" (length (card-games-get game :reserve)))
                  (card-games-sol--spec (car (last (card-games-get game :reserve)))) nil
                  (equal cur-spot '(reserve . 0)) '(reserve . 0)) slots))
    (dotimes (i (oref game nfree))
      (push (list (format "F%d" (1+ i)) (card-games-sol--spec (aref (card-games-get game :free) i)) nil
                  (equal cur-spot (cons 'free i)) (cons 'free i)) slots))
    (dotimes (i (oref game nfound))
      (push (list (format "%d" (1+ i))
                  (card-games-sol--spec (car (last (aref (card-games-get game :found) i)))) nil
                  (equal cur-spot (cons 'found i)) (cons 'found i)) slots))
    (setq slots (nreverse slots))
    (let* ((ntop (length slots))
           (topw (+ (* 2 pad) (* ntop (+ w gap))))
           (colsw (+ (* 2 pad) (* ncols (+ w colgap))))
           (width (max topw colsw))
           (top-y (+ pad 14)) (col-label-y (+ top-y h 18)) (col-y (+ col-label-y 6))
           (tab (card-games-get game :tableau))
           (maxext (let ((m h))
                     (dotimes (c ncols)
                       (let* ((col (aref tab c)) (d (card-games-sol--down game c))
                              (nu (- (length col) d))
                              (ext (+ (* d vdown) (* (max 0 (1- nu)) vup) h)))
                         (setq m (max m ext))))
                     m))
           (height (+ col-y maxext pad))
           (svg (svg-create width height)))
      (let ((x pad))
        (dolist (sl slots)
          (cl-destructuring-bind (label spec downp cursorp spot) sl
            (svg-text svg label :x (+ x 1) :y (- top-y 3) :font-size 11 :fill lc
                      :font-family card-games-svg-font-family)
            (cond (downp (card-games-svg-card svg x top-y :down t :highlight cursorp))
                  (spec (card-games-svg-card svg x top-y :rank (car spec) :suit (cdr spec)
                                     :highlight cursorp))
                  (t (card-games-svg-card svg x top-y :gap t :highlight cursorp)))
            (push (cons (list x top-y w h) spot) regions))
          (setq x (+ x w gap))))
      (dotimes (c ncols)
        (let* ((x (+ pad (* c (+ w colgap)))) (col (aref tab c)) (len (length col))
               (d (card-games-sol--down game c)) (y col-y) (r 0)
               (cursorp (equal cur-spot (cons 'col c))))
          (svg-text svg (format "%d" (1+ c)) :x (+ x 1) :y col-label-y
                    :font-size 11 :fill lc :font-family card-games-svg-font-family)
          (push (cons (list x col-y w maxext) (cons 'col c)) regions)
          (if (= len 0)
              (card-games-svg-card svg x y :gap t :highlight cursorp)
            (dolist (card col)
              (let* ((downp (< r d)) (top-card (= r (1- len)))
                     (selp (and (equal sel (cons 'col c)) (>= r (- len sel-n)))))
                (if downp (card-games-svg-card svg x y :down t)
                  (card-games-svg-card svg x y :rank (car (card-games-sol--spec card))
                               :suit (cdr (card-games-sol--spec card))
                               :highlight (and top-card cursorp) :hint selp))
                (setq y (+ y (if downp vdown vup)) r (1+ r)))))))
      (cons (propertize "*" 'display (card-games-svg-image svg (card-games-scale)))
            (nreverse regions)))))

(cl-defmethod card-games-render ((game card-games-solitaire-game))
  "Return a propertized depiction of GAME (SVG on a graphical display)."
  (if (and card-games-sol-svg-cards (display-graphic-p))
      (car (card-games-sol--svg game))
    (card-games-sol--render-text game)))

(cl-defmethod card-games-render-text ((game card-games-solitaire-game))
  "Return the plain-text rendering of GAME."
  (card-games-sol--render-text game))

(cl-defmethod card-games-render-svg ((game card-games-solitaire-game))
  "Return the SVG rendering of GAME."
  (if card-games-sol-svg-cards (card-games-sol--svg game)
    (cons (card-games-sol--render-text game) nil)))

(cl-defmethod card-games-render-apply ((game card-games-solitaire-game) action)
  "Apply GAME click ACTION (a cursor spot) by selecting it and acting."
  (let ((idx (cl-position action (card-games-sol--spots game) :test #'equal)))
    (when idx (card-games-put game :cursor idx) (card-games-sol-act))))

(defun card-games-sol--render-text (game)
  "Return a plain-text depiction of solitaire GAME."
  (let* ((spots (card-games-sol--spots game))
         (cur (card-games-get game :cursor))
         (cur-spot (nth cur spots))
         (sel (card-games-get game :sel))
         (sel-n (card-games-get game :sel-n))
         (out (list)))
    (push (format "  %s   Moves: %d%s\n\n"
                  (oref game vname) (card-games-get game :moves)
                  (if (> (oref game nfound) 0) ""
                    (format "   Sets: %d/%d" (card-games-get game :sets) (oref game target-sets))))
          out)
    ;; Top line: stock / waste / free cells / foundations.
    (let ((line "  "))
      (when (oref game has-stock)
        (let ((on (equal cur-spot '(stock . 0))))
          (setq line (concat line "Stock:"
                             (propertize (format "%-4s"
                                                 (if (card-games-get game :stock) "##" "·"))
                                         'face (if on 'card-games-cursor 'default))
                             (format "(%d) " (length (card-games-get game :stock))))) ))
      (when (oref game has-waste)
        (let ((on (equal cur-spot '(waste . 0))) (w (car (last (card-games-get game :waste)))))
          (setq line (concat line "Waste:"
                             (card-games-sol--render-card w nil nil on)))))
      (when (oref game has-reserve)
        (let ((on (equal cur-spot '(reserve . 0))) (r (car (last (card-games-get game :reserve)))))
          (setq line (concat line "Reserve:"
                             (card-games-sol--render-card r nil nil on)
                             (format "(%d) " (length (card-games-get game :reserve)))))))
      (dotimes (i (oref game nfree))
        (let ((on (equal cur-spot (cons 'free i))) (c (aref (card-games-get game :free) i)))
          (setq line (concat line (format "F%d:" (1+ i))
                             (card-games-sol--render-card c nil nil on)))))
      (dotimes (i (oref game nfound))
        (let ((on (equal cur-spot (cons 'found i)))
              (c (car (last (aref (card-games-get game :found) i)))))
          (setq line (concat line (format "%d:" (1+ i))
                             (card-games-sol--render-card c nil nil on)))))
      (push (concat line "\n\n") out))
    ;; Column headers.
    (let ((hdr "   "))
      (dotimes (c (oref game ncols))
        (let ((on (equal cur-spot (cons 'col c))))
          (setq hdr (concat hdr (propertize (format "%2d  " (1+ c))
                                            'face (if on 'card-games-cursor 'default))))))
      (push (concat hdr "\n") out))
    ;; Column bodies, row by row.
    (let* ((tab (card-games-get game :tableau))
           (maxlen (apply #'max 1 (mapcar #'length (append tab nil)))))
      (dotimes (r maxlen)
        (let ((row "   "))
          (dotimes (c (oref game ncols))
            (let* ((col (aref tab c))
                   (len (length col))
                   (card (and (< r len) (nth r col)))
                   (down (and card (< r (card-games-sol--down game c))))
                   (selp (and (equal sel (cons 'col c))
                              card (>= r (- len sel-n))))
                   (cursorp (and (equal cur-spot (cons 'col c)) (= r (1- len)))))
              (setq row (concat row
                                (if (< r len)
                                    (card-games-sol--render-card card down selp cursorp)
                                  "    ")))))
          (push (concat row "\n") out))))
    (push (format "\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun card-games-sol--redisplay ()
  "Redraw the current solitaire buffer."
  (let ((game card-games-sol--game)
        (inhibit-read-only t))
    (setq-local mode-line-process
                (format " [%s]" (if (card-games-won-p game) "solved"
                                  (let ((s (card-games-get game :sel)))
                                    (if s "carrying" "playing")))))
    (erase-buffer)
    (card-games-render-game game)
    (insert "\n")
    (card-games-insert-legend
     "arrows move · RET pick up/drop · f foundation · a auto · u undo · n new · q menu · ? help")
    (goto-char (point-min))))

;;;; Mode and commands

(defun card-games-sol-mouse (event)
  "Handle mouse EVENT on the solitaire board: select that pile and act."
  (interactive "e")
  (let* ((game card-games-sol--game)
         (r (and game (oref game renderer)))
         (action (and r (card-games-renderer-hit r game (event-start event)))))
    (when action (card-games-render-apply game action))))

(defvar card-games-sol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-sol-mouse)
    (define-key map (kbd "<left>")  #'card-games-sol-left)
    (define-key map (kbd "<right>") #'card-games-sol-right)
    (define-key map (kbd "<up>")    #'card-games-sol-up)
    (define-key map (kbd "<down>")  #'card-games-sol-down)
    (define-key map (kbd "RET") #'card-games-sol-act)
    (define-key map (kbd "SPC") #'card-games-sol-act)
    (define-key map "f" #'card-games-sol-to-foundation)
    (define-key map "a" #'card-games-sol-auto)
    (define-key map "u" #'card-games-sol-undo)
    (define-key map "n" #'card-games-sol-new)
    (define-key map "g" #'card-games-sol-redraw)
    (define-key map "?" #'card-games-sol-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-sol-mode'.")

(defun card-games-sol--classic-keymap ()
  "Return a copy of `card-games-sol-mode-map' with vi-style hjkl added."
  (let ((map (copy-keymap card-games-sol-mode-map)))
    (define-key map "h" #'card-games-sol-left)
    (define-key map "l" #'card-games-sol-right)
    (define-key map "j" #'card-games-sol-down)
    (define-key map "k" #'card-games-sol-up)
    map))

(define-derived-mode card-games-sol-mode special-mode "Solitaire"
  "Major mode for the tableau solitaires."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type)
  (when (eq card-games-keys 'classic)
    (use-local-map (card-games-sol--classic-keymap))))

(defun card-games-sol--play (class)
  "Start a solitaire game of CLASS in its own buffer."
  (let* ((game (card-games-sol--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (card-games-sol-mode)
      (setq card-games-sol--game game)
      (card-games-sol--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun card-games-klondike ()
  "Play Klondike, the classic solitaire."
  (interactive) (card-games-sol--play 'card-games-klondike-game))

;;;###autoload
(defun card-games-freecell ()
  "Play FreeCell solitaire."
  (interactive) (card-games-sol--play 'card-games-freecell-game))

;;;###autoload
(defun card-games-spider ()
  "Play Spider solitaire (two decks)."
  (interactive) (card-games-sol--play 'card-games-spider-game))

;;;###autoload
(defun card-games-yukon ()
  "Play Yukon solitaire."
  (interactive) (card-games-sol--play 'card-games-yukon-game))


;;;; More games: Forty Thieves, Scorpion, Canfield

(defclass card-games-forty-game (card-games-solitaire-game)
  ((ncols :initform 10) (ndecks :initform 2) (nfound :initform 8)
   (has-stock :initform t) (has-waste :initform t) (redeal :initform nil)
   (build :initform 'suit) (run-rule :initform 'suit) (empty-rule :initform 'any)
   (vname :initform "Forty Thieves"))
  "Forty Thieves: two decks, ten columns, eight foundations, no redeal.")

(cl-defmethod card-games-sol--layout ((_ card-games-forty-game))
  "Return the Forty Thieves tableau layout (DOWN . UP per column)."
  (make-list 10 (cons 0 4)))

(defclass card-games-scorpion-game (card-games-solitaire-game)
  ((ncols :initform 7) (nfound :initform 0) (has-stock :initform t)
   (build :initform 'suit) (run-rule :initform 'any) (empty-rule :initform 'king)
   (target-sets :initform 4) (vname :initform "Scorpion"))
  "Scorpion: build down by suit, move any buried group, clear four runs.")

(cl-defmethod card-games-sol--layout ((_ card-games-scorpion-game))
  "Return the Scorpion tableau layout (DOWN . UP per column)."
  (append (make-list 4 (cons 3 4)) (make-list 3 (cons 0 7))))

(cl-defmethod card-games-sol--stock-action ((game card-games-scorpion-game))
  "Deal GAME's three stock cards onto the first three columns."
  (let ((stock (card-games-get game :stock)))
    (if (null stock)
        (card-games-put game :message "The stock is empty.")
      (card-games-sol--snapshot game)
      (dotimes (c (min 3 (length stock)))
        (card-games-sol--set-col game c (append (card-games-sol--col game c) (last stock 1)))
        (setq stock (butlast stock 1)))
      (card-games-put game :stock stock)
      (card-games-sol--harvest game)
      (card-games-put game :message "Dealt the stock onto the first columns."))))

(defclass card-games-canfield-game (card-games-solitaire-game)
  ((ncols :initform 4) (nfound :initform 4) (has-stock :initform t)
   (has-waste :initform t) (has-reserve :initform t) (draw :initform 3)
   (redeal :initform t) (build :initform 'alt) (run-rule :initform 'alt)
   (empty-rule :initform 'any) (wrap :initform t) (vname :initform "Canfield"))
  "Canfield: a 13-card reserve and a variable foundation base rank.")

(cl-defmethod card-games-sol--deal ((game card-games-canfield-game))
  "Deal GAME's Canfield layout: reserve, base foundation, four columns, stock."
  (let* ((deck (card-games-sol--make-deck 1))
         (reserve (cl-loop repeat 13 collect (pop deck)))
         (first (pop deck))
         (found (make-vector 4 nil))
         (tableau (make-vector 4 nil))
         (down (make-vector 4 0)))
    (oset game base (cdr first))
    (aset found 0 (list first))
    (dotimes (c 4) (aset tableau c (list (pop deck))))
    (card-games-put game :reserve reserve)
    (card-games-put game :tableau tableau)
    (card-games-put game :down down)
    (card-games-put game :found found)
    (card-games-put game :free (make-vector 0 nil))
    (card-games-put game :stock deck)
    (card-games-put game :waste nil)
    (card-games-put game :sets 0)
    (card-games-put game :moves 0)
    (card-games-put game :cursor 0)
    (card-games-put game :sel nil)
    (card-games-put game :sel-n 0)
    (card-games-put game :history nil)
    (card-games-put game :message
            (format "Canfield.  Foundations build up from %s (wrapping).  RET deals three."
                    (aref card-games-sol-ranks (cdr first))))
    game))

(cl-defmethod card-games-sol--autofill ((_ card-games-solitaire-game)) "Most solitaires do not autofill empty columns." nil)
(cl-defmethod card-games-sol--autofill ((game card-games-canfield-game))
  "Fill GAME's empty columns from the reserve, as Canfield requires."
  (dotimes (c (oref game ncols))
    (when (and (null (card-games-sol--col game c)) (card-games-get game :reserve))
      (let ((card (car (last (card-games-get game :reserve)))))
        (card-games-put game :reserve (butlast (card-games-get game :reserve) 1))
        (card-games-sol--set-col game c (list card))))))


;;;; Russian Bank (single-player patience)

;; A one-deck patience in the spirit of Russian Bank (Crapette): eight
;; "houses" build down in alternating colours, the four foundations build
;; up by suit from the Ace, and a thirteen-card reserve feeds the houses.
;; The competitive two-player game (with its "stops" and loading rules)
;; is a separate, larger build; this is the solitaire adaptation.

(defclass card-games-russian-bank-game (card-games-solitaire-game)
  ((ncols :initform 8) (nfound :initform 4) (has-stock :initform t)
   (has-waste :initform t) (has-reserve :initform t) (draw :initform 1)
   (redeal :initform t) (build :initform 'alt) (run-rule :initform 'alt)
   (empty-rule :initform 'any) (base :initform 0) (wrap :initform nil)
   (vname :initform "Russian Bank"))
  "Russian Bank patience: eight houses down by alternating colour, four
foundations up by suit from the Ace, and a thirteen-card reserve.")

(cl-defmethod card-games-sol--deal ((game card-games-russian-bank-game))
  "Deal GAME's Russian Bank layout: reserve, eight houses, and a stock."
  (let* ((deck (card-games-sol--make-deck 1))
         (reserve (cl-loop repeat 13 collect (pop deck)))
         (tableau (make-vector 8 nil))
         (down (make-vector 8 0)))
    (dotimes (c 8) (aset tableau c (list (pop deck))))
    (card-games-put game :reserve reserve)
    (card-games-put game :tableau tableau)
    (card-games-put game :down down)
    (card-games-put game :found (make-vector 4 nil))
    (card-games-put game :free (make-vector 0 nil))
    (card-games-put game :stock deck)
    (card-games-put game :waste nil)
    (card-games-put game :sets 0)
    (card-games-put game :moves 0)
    (card-games-put game :cursor 0)
    (card-games-put game :sel nil)
    (card-games-put game :sel-n 0)
    (card-games-put game :history nil)
    (card-games-put game :message
            "Russian Bank.  Houses build down in alternating colours; foundations up by suit from the Ace.")
    game))

(cl-defmethod card-games-sol--autofill ((game card-games-russian-bank-game))
  "Fill a GAME empty house from the reserve, as Russian Bank does."
  (dotimes (c (oref game ncols))
    (when (and (null (card-games-sol--col game c)) (card-games-get game :reserve))
      (let ((card (car (last (card-games-get game :reserve)))))
        (card-games-put game :reserve (butlast (card-games-get game :reserve) 1))
        (card-games-sol--set-col game c (list card))))))

;;;###autoload
(defun card-games-forty-thieves ()
  "Play Forty Thieves solitaire (two decks)."
  (interactive) (card-games-sol--play 'card-games-forty-game))

;;;###autoload
(defun card-games-scorpion ()
  "Play Scorpion solitaire."
  (interactive) (card-games-sol--play 'card-games-scorpion-game))

;;;###autoload
(defun card-games-canfield ()
  "Play Canfield solitaire."
  (interactive) (card-games-sol--play 'card-games-canfield-game))

;;;###autoload
(defun card-games-russian-bank ()
  "Play Russian Bank, the single-player patience adaptation."
  (interactive) (card-games-sol--play 'card-games-russian-bank-game))

(provide 'card-games-solitaire)
;;; card-games-solitaire.el ends here
