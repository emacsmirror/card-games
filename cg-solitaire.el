;;; cg-solitaire.el --- Tableau solitaires (Klondike, FreeCell, Spider, Yukon)  -*- lexical-binding: t; -*-

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

;; A shared engine for tableau solitaires, with four games built on it:
;;
;;   `cg-klondike' -- the classic "Solitaire": seven columns, a stock and
;;                    waste, build the foundations up by suit from the Ace.
;;   `cg-freecell' -- all cards dealt face up, four free cells, no stock;
;;                    a game of nearly pure skill.
;;   `cg-spider'   -- two decks, ten columns; build down regardless of suit
;;                    but only same-suit runs move; clear eight K..A runs.
;;   `cg-yukon'    -- Klondike's layout, all face up, move any buried group.
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
(require 'cg-core)

;;;; Cards

(defconst cg-sol-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0..12 (Ace through King).")

(defun cg-sol-card-string (card &optional down)
  "Return a short string for CARD.
With DOWN non-nil, draw a face-down back instead.  A nil CARD draws an
empty-slot dot."
  (cond (down "##")
        ((null card) "·")
        (t (concat (aref cg-sol-ranks (cdr card)) (cg-suit-glyph (car card))))))

(defsubst cg-sol-red-p (card)
  "Return non-nil when CARD is a red suit."
  (and card (cg-red-suit-p (car card))))

(defun cg-sol--make-deck (ndecks)
  "Return a shuffled list of NDECKS standard 52-card decks."
  (random t)
  (let ((cards nil))
    (dotimes (_ ndecks)
      (dotimes (s 4)
        (dotimes (r 13)
          (push (cons s r) cards))))
    (cg-shuffle cards)))

;;;; Game classes

(defclass cg-solitaire-game (cg-game)
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

(defclass cg-klondike-game (cg-solitaire-game)
  ((has-stock :initform t) (has-waste :initform t)
   (vname :initform "Klondike"))
  "Klondike: seven columns, stock and waste, foundations up by suit.")

(defclass cg-freecell-game (cg-solitaire-game)
  ((ncols :initform 8) (nfree :initform 4) (empty-rule :initform 'any)
   (vname :initform "FreeCell"))
  "FreeCell: eight columns dealt face up, four free cells, no stock.")

(defclass cg-yukon-game (cg-solitaire-game)
  ((run-rule :initform 'any) (vname :initform "Yukon"))
  "Yukon: Klondike layout dealt mostly face up; move any buried group.")

(defclass cg-spider-game (cg-solitaire-game)
  ((ncols :initform 10) (ndecks :initform 2) (nfound :initform 0)
   (has-stock :initform t) (build :initform 'any) (run-rule :initform 'suit)
   (empty-rule :initform 'any) (vname :initform "Spider"))
  "Spider: two decks, ten columns; clear eight K..A same-suit runs.")

;;;; Rules (predicates)

(defun cg-sol--diff-color-p (a b)
  "Return non-nil when cards A and B are of opposite colours."
  (not (eq (cg-red-suit-p (car a)) (cg-red-suit-p (car b)))))

(cl-defmethod cg-sol--link-p ((game cg-solitaire-game) upper lower)
  "Return non-nil when LOWER may rest directly on UPPER within a run."
  (pcase (oref game run-rule)
    ('any t)
    ('suit (and (= (cdr lower) (1- (cdr upper))) (= (car lower) (car upper))))
    (_     (and (= (cdr lower) (1- (cdr upper))) (cg-sol--diff-color-p upper lower)))))

(cl-defmethod cg-sol--place-p ((game cg-solitaire-game) top card)
  "Return non-nil when CARD may be placed on a column whose top is TOP."
  (pcase (oref game build)
    ('any  (= (cdr card) (1- (cdr top))))
    ('suit (and (= (cdr card) (1- (cdr top))) (= (car card) (car top))))
    (_     (and (= (cdr card) (1- (cdr top))) (cg-sol--diff-color-p top card)))))

(cl-defmethod cg-sol--empty-accepts ((game cg-solitaire-game) card)
  "Return non-nil when CARD may be placed on an empty column."
  (pcase (oref game empty-rule)
    ('king (= (cdr card) 12))
    (_ t)))

;;;; Layout and dealing

(cl-defgeneric cg-sol--layout (game)
  "Return a list of (DOWN . UP) card counts, one per tableau column.")

(cl-defmethod cg-sol--layout ((_ cg-klondike-game))
  (cl-loop for i below 7 collect (cons i 1)))
(cl-defmethod cg-sol--layout ((_ cg-yukon-game))
  (cons (cons 0 1) (cl-loop for i from 1 below 7 collect (cons i 5))))
(cl-defmethod cg-sol--layout ((_ cg-freecell-game))
  (append (make-list 4 (cons 0 7)) (make-list 4 (cons 0 6))))
(cl-defmethod cg-sol--layout ((_ cg-spider-game))
  (append (make-list 4 (cons 5 1)) (make-list 6 (cons 4 1))))

(cl-defmethod cg-sol--deal ((game cg-solitaire-game))
  "Deal a fresh layout into GAME and initialise its environment."
  (let* ((deck (cg-sol--make-deck (oref game ndecks)))
         (nc (oref game ncols))
         (layout (cg-sol--layout game))
         (tableau (make-vector nc nil))
         (down (make-vector nc 0)))
    (dotimes (c nc)
      (let* ((spec (nth c layout))
             (col nil))
        (dotimes (_ (+ (car spec) (cdr spec)))
          (push (pop deck) col))
        (aset tableau c (nreverse col))
        (aset down c (car spec))))
    (cg-put game :tableau tableau)
    (cg-put game :down down)
    (cg-put game :found (make-vector (oref game nfound) nil))
    (cg-put game :free (make-vector (oref game nfree) nil))
    (cg-put game :stock deck)
    (cg-put game :waste nil)
    (cg-put game :sets 0)
    (cg-put game :moves 0)
    (cg-put game :cursor 0)
    (cg-put game :sel nil)
    (cg-put game :sel-n 0)
    (cg-put game :history nil)
    (cg-put game :message
            (format "%s.  Arrows move; RET picks up/drops; f=foundation; a=auto; ?=help."
                    (oref game vname)))
    game))

;;;; Spots (the cursor visits piles)

(cl-defmethod cg-sol--spots ((game cg-solitaire-game))
  "Return the ordered list of (TYPE . INDEX) piles the cursor can visit."
  (append
   (when (oref game has-stock) '((stock . 0)))
   (when (oref game has-waste) '((waste . 0)))
   (when (oref game has-reserve) '((reserve . 0)))
   (cl-loop for i below (oref game nfree) collect (cons 'free i))
   (cl-loop for i below (oref game nfound) collect (cons 'found i))
   (cl-loop for i below (oref game ncols) collect (cons 'col i))))

(defun cg-sol--cur-spot (game)
  "Return the (TYPE . INDEX) spot currently under the cursor."
  (nth (cg-get game :cursor) (cg-sol--spots game)))

;;;; Pile access helpers

(defun cg-sol--col (game c) "Column C of GAME (a list)." (aref (cg-get game :tableau) c))
(defun cg-sol--set-col (game c v) (aset (cg-get game :tableau) c v))
(defun cg-sol--down (game c) "Face-down count of column C." (aref (cg-get game :down) c))
(defun cg-sol--set-down (game c v) (aset (cg-get game :down) c v))

(defun cg-sol--col-top (game c)
  "Return the top (accessible) card of column C, or nil."
  (car (last (cg-sol--col game c))))

(defun cg-sol--exposed (game c)
  "Return the face-up cards of column C (bottom..top order)."
  (nthcdr (cg-sol--down game c) (cg-sol--col game c)))

(cl-defmethod cg-sol--top-run ((game cg-solitaire-game) c)
  "Return the longest movable run from the top of column C (bottom..top)."
  (let ((top->bottom (reverse (cg-sol--exposed game c))))
    (if (null top->bottom)
        nil
      (let ((run (list (car top->bottom)))
            (prev (car top->bottom)))
        (catch 'done
          (dolist (card (cdr top->bottom))
            (if (cg-sol--link-p game card prev)
                (progn (push card run) (setq prev card))
              (throw 'done nil))))
        run))))

(defun cg-sol--spot-top (game spot)
  "Return the top card available at SPOT, or nil."
  (pcase (car spot)
    ('col   (cg-sol--col-top game (cdr spot)))
    ('waste (car (last (cg-get game :waste))))
    ('free  (aref (cg-get game :free) (cdr spot)))
    ('found (car (last (aref (cg-get game :found) (cdr spot)))))
    ('reserve (car (last (cg-get game :reserve))))
    (_ nil)))

;;;; Foundations

(defun cg-sol--found-accepts (game i card)
  "Return non-nil when CARD may go onto foundation I of GAME."
  (and card
       (let ((f (aref (cg-get game :found) i)))
         (if (null f)
             (= (cdr card) (oref game base))   ; empty foundation takes the base rank
           (let* ((top (car (last f)))
                  (need (if (oref game wrap) (mod (1+ (cdr top)) 13) (1+ (cdr top)))))
             (and (= (car card) (car top))
                  (= (cdr card) need)))))))

(defun cg-sol--found-for (game card)
  "Return the index of a foundation that would accept CARD, or nil."
  (cl-loop for i below (oref game nfound)
           when (cg-sol--found-accepts game i card) return i))

;;;; Move primitives

(defun cg-sol--snapshot (game)
  "Push a deep-ish copy of GAME's mutable state onto the undo history."
  (let ((tab (cg-get game :tableau))
        (frv (cg-get game :found))
        (fre (cg-get game :free)))
    (cg-put game :history
            (cons (list (vconcat (mapcar #'copy-sequence tab))
                        (copy-sequence (cg-get game :down))
                        (vconcat (mapcar #'copy-sequence frv))
                        (copy-sequence fre)
                        (copy-sequence (cg-get game :stock))
                        (copy-sequence (cg-get game :waste))
                        (cg-get game :sets)
                        (cg-get game :moves)
                        (copy-sequence (cg-get game :reserve)))
                  (cg-get game :history)))))

(defun cg-sol--restore (game)
  "Pop and restore the most recent undo snapshot of GAME, if any."
  (let ((h (cg-get game :history)))
    (when h
      (cl-destructuring-bind (tab down frv fre stock waste sets moves reserve) (car h)
        (cg-put game :tableau tab)
        (cg-put game :down down)
        (cg-put game :found frv)
        (cg-put game :free fre)
        (cg-put game :stock stock)
        (cg-put game :waste waste)
        (cg-put game :sets sets)
        (cg-put game :moves moves)
        (cg-put game :reserve reserve))
      (cg-put game :history (cdr h))
      (cg-put game :sel nil)
      t)))

(defun cg-sol--flip (game c)
  "Flip the top of column C face up if it is face down."
  (let ((len (length (cg-sol--col game c)))
        (d (cg-sol--down game c)))
    (when (and (> len 0) (>= d len))
      (cg-sol--set-down game c (1- len)))))

(defun cg-sol--take (game spot n)
  "Remove and return the top N cards (bottom..top order) from SPOT."
  (pcase (car spot)
    ('col (let* ((c (cdr spot)) (col (cg-sol--col game c))
                 (run (last col n)))
            (cg-sol--set-col game c (butlast col n))
            (let ((len (length (cg-sol--col game c))))
              (when (> (cg-sol--down game c) len)
                (cg-sol--set-down game c len)))
            (cg-sol--flip game c)
            run))
    ('waste (let ((w (cg-get game :waste)))
              (cg-put game :waste (butlast w 1)) (last w 1)))
    ('free (let ((card (aref (cg-get game :free) (cdr spot))))
             (aset (cg-get game :free) (cdr spot) nil) (list card)))
    ('found (let* ((i (cdr spot)) (f (aref (cg-get game :found) i)))
              (aset (cg-get game :found) i (butlast f 1)) (last f 1)))
    ('reserve (let ((r (cg-get game :reserve)))
                (cg-put game :reserve (butlast r 1)) (last r 1)))
    (_ nil)))

(defun cg-sol--can-drop (game spot cards)
  "Return non-nil when the run CARDS (bottom..top) may drop on SPOT."
  (and cards
       (pcase (car spot)
         ('col (let* ((c (cdr spot)) (top (cg-sol--col-top game c)))
                 (if top
                     (cg-sol--place-p game top (car cards))
                   (cg-sol--empty-accepts game (car cards)))))
         ('found (and (= 1 (length cards))
                      (cg-sol--found-accepts game (cdr spot) (car cards))))
         ('free (and (= 1 (length cards))
                     (null (aref (cg-get game :free) (cdr spot)))))
         (_ nil))))

(defun cg-sol--drop (game spot cards)
  "Place the run CARDS (bottom..top) onto SPOT."
  (pcase (car spot)
    ('col (let ((c (cdr spot)))
            (cg-sol--set-col game c (append (cg-sol--col game c) cards))))
    ('found (let ((i (cdr spot)))
              (aset (cg-get game :found) i
                    (append (aref (cg-get game :found) i) cards))))
    ('free (aset (cg-get game :free) (cdr spot) (car cards)))))

;;;; Spider: complete-run removal

(cl-defmethod cg-sol--harvest ((game cg-solitaire-game))
  "Remove any complete K..A same-suit run from a column top; bump :sets.
Only games without foundations (NFOUND 0: Spider, Scorpion) harvest runs."
  (when (= 0 (oref game nfound))
   (dotimes (c (oref game ncols))
    (let* ((col (cg-sol--col game c))
           (exp (cg-sol--exposed game c)))
      (when (>= (length exp) 13)
        (let ((run (last exp 13)) (ok t) (suit (car (nth 0 (last exp 13)))))
          (cl-loop for k below 13
                   for card = (nth k run)
                   unless (and (= (car card) suit) (= (cdr card) (- 12 k)))
                   do (setq ok nil))
          (when ok
            (cg-sol--set-col game c (butlast col 13))
            (let ((len (length (cg-sol--col game c))))
              (when (> (cg-sol--down game c) len) (cg-sol--set-down game c len)))
            (cg-sol--flip game c)
            (cg-put game :sets (1+ (cg-get game :sets))))))))))

;;;; Stock action

(defcustom cg-sol-klondike-draw 1
  "Number of cards turned from the stock to the waste in Klondike."
  :type '(choice (const :tag "Draw one" 1) (const :tag "Draw three" 3))
  :group 'card-games)

(cl-defmethod cg-sol--stock-action ((game cg-solitaire-game))
  "Deal `draw' cards to the waste, recycling the waste when `redeal'."
  (if (not (oref game has-waste))
      (cg-put game :message "No stock to deal.")
    (cg-sol--snapshot game)
    (let ((stock (cg-get game :stock)) (waste (cg-get game :waste)))
      (if stock
          (let ((n (min (oref game draw) (length stock))))
            (dotimes (_ n)
              (setq waste (append waste (last stock 1)))
              (setq stock (butlast stock 1)))
            (cg-put game :stock stock) (cg-put game :waste waste)
            (cg-put game :message "Dealt from stock."))
        (if (and (oref game redeal) waste)
            (progn (cg-put game :stock (reverse waste)) (cg-put game :waste nil)
                   (cg-put game :message "Recycled the waste into the stock."))
          (cg-put game :message "The stock is empty."))))))

(cl-defmethod cg-sol--stock-action ((game cg-klondike-game))
  (cg-sol--snapshot game)
  (let ((stock (cg-get game :stock)) (waste (cg-get game :waste)))
    (if stock
        (let ((n (min cg-sol-klondike-draw (length stock))))
          (dotimes (_ n)
            (setq waste (append waste (last stock 1)))
            (setq stock (butlast stock 1)))
          (cg-put game :stock stock) (cg-put game :waste waste)
          (cg-put game :message "Dealt from stock."))
      (if waste
          (progn (cg-put game :stock (reverse waste)) (cg-put game :waste nil)
                 (cg-put game :message "Recycled the waste into the stock."))
        (cg-put game :message "Stock and waste are both empty.")))))

(cl-defmethod cg-sol--stock-action ((game cg-spider-game))
  (let ((stock (cg-get game :stock)))
    (cond
     ((null stock) (cg-put game :message "The stock is empty."))
     ((cl-loop for c below (oref game ncols)
               thereis (null (cg-sol--col game c)))
      (cg-put game :message "Fill every column before dealing from the stock."))
     (t (cg-sol--snapshot game)
        (dotimes (c (oref game ncols))
          (cg-sol--set-col game c (append (cg-sol--col game c) (last stock 1)))
          (setq stock (butlast stock 1)))
        (cg-put game :stock stock)
        (cg-sol--harvest game)
        (cg-put game :message "Dealt a row from the stock.")))))

;;;; Win

(cl-defmethod cg-won-p ((game cg-solitaire-game))
  "Return non-nil when GAME is solved."
  (if (= 0 (oref game nfound))
      (>= (cg-get game :sets) (oref game target-sets))
    (cl-every (lambda (f) (= 13 (length f)))
              (append (cg-get game :found) nil))))

;;;; Interaction

(defvar-local cg-sol--game nil "The solitaire game in the current buffer.")

(cl-defmethod cg-sol--selectable ((game cg-solitaire-game) spot)
  "Return the run (bottom..top) GAME would pick up from SPOT, or nil."
  (pcase (car spot)
    ('col (cg-sol--top-run game (cdr spot)))
    ('waste (let ((c (cg-sol--spot-top game spot))) (and c (list c))))
    ('free (let ((c (cg-sol--spot-top game spot))) (and c (list c))))
    ('found (let ((c (cg-sol--spot-top game spot))) (and c (list c))))
    ('reserve (let ((c (cg-sol--spot-top game spot))) (and c (list c))))
    (_ nil)))

(defun cg-sol-act (&optional count)
  "Pick up from, or drop onto, the pile under the cursor.
With prefix COUNT, pick up exactly COUNT cards from a column."
  (interactive "P")
  (let* ((game cg-sol--game)
         (spot (cg-sol--cur-spot game))
         (sel (cg-get game :sel)))
    (cond
     ((eq (car spot) 'stock)
      (cg-put game :sel nil)
      (cg-sol--stock-action game))
     ((null sel)
      (let ((run (cg-sol--selectable game spot)))
        (cond
         ((null run) (cg-put game :message "Nothing to pick up there."))
         (t (when (and count (eq (car spot) 'col))
              (setq run (last run (min (prefix-numeric-value count) (length run)))))
            (cg-put game :sel spot)
            (cg-put game :sel-n (length run))
            (cg-put game :message
                    (format "Picked up %d card%s.  RET on a destination."
                            (length run) (if (= 1 (length run)) "" "s")))))))
     ((equal sel spot)
      (cg-put game :sel nil) (cg-put game :message "Cancelled."))
     (t
      (let* ((n (cg-get game :sel-n))
             (cards (last (pcase (car sel)
                            ('col (cg-sol--col game (cdr sel)))
                            ('waste (cg-get game :waste))
                            ('found (aref (cg-get game :found) (cdr sel)))
                            ('free (list (aref (cg-get game :free) (cdr sel)))))
                          n)))
        (if (cg-sol--can-drop game spot cards)
            (progn (cg-sol--snapshot game)
                   (cg-sol--take game sel n)
                   (cg-sol--drop game spot cards)
                   (cg-put game :moves (1+ (cg-get game :moves)))
                   (cg-sol--harvest game)
                   (cg-put game :sel nil)
                   (cg-put game :message "Moved."))
          (cg-put game :sel nil)
          (cg-put game :message "That move is not allowed."))))))
  (cg-sol--after cg-sol--game))

(defun cg-sol-to-foundation ()
  "Send the top card of the pile under the cursor to a foundation."
  (interactive)
  (let* ((game cg-sol--game)
         (spot (cg-sol--cur-spot game))
         (card (cg-sol--spot-top game spot)))
    (if (and card (memq (car spot) '(col waste free reserve)))
        (let ((i (cg-sol--found-for game card)))
          (if i
              (progn (cg-sol--snapshot game)
                     (cg-sol--take game spot 1)
                     (cg-sol--drop game (cons 'found i) (list card))
                     (cg-put game :moves (1+ (cg-get game :moves)))
                     (cg-put game :sel nil)
                     (cg-put game :message "To the foundation."))
            (cg-put game :message "No foundation will take that card.")))
      (cg-put game :message "Nothing to send to a foundation.")))
  (cg-sol--after cg-sol--game))

(defun cg-sol-auto ()
  "Repeatedly send any eligible card to the foundations."
  (interactive)
  (let ((game cg-sol--game) (moved 0))
    (when (> (oref game nfound) 0)
      (cg-sol--snapshot game)
      (let (again)
        (cl-loop
         do (setq again nil)
         (dolist (spot (cg-sol--spots game))
           (when (memq (car spot) '(col waste free reserve))
             (let* ((card (cg-sol--spot-top game spot))
                    (i (and card (cg-sol--found-for game card))))
               (when i
                 (cg-sol--take game spot 1)
                 (cg-sol--drop game (cons 'found i) (list card))
                 (setq moved (1+ moved) again t)))))
         while again))
      (if (> moved 0)
          (progn (cg-put game :moves (+ moved (cg-get game :moves)))
                 (cg-put game :sel nil)
                 (cg-put game :message (format "Auto-played %d card%s."
                                               moved (if (= 1 moved) "" "s"))))
        (cg-put game :history (cdr (cg-get game :history)))
        (cg-put game :message "Nothing to auto-play."))))
  (cg-sol--after cg-sol--game))

(defun cg-sol-undo ()
  "Undo the last move."
  (interactive)
  (let ((game cg-sol--game))
    (if (cg-sol--restore game)
        (cg-put game :message "Undid a move.")
      (cg-put game :message "Nothing to undo."))
    (cg-sol--redisplay)))

(defun cg-sol--move (delta)
  "Move the cursor by DELTA spots."
  (let* ((game cg-sol--game)
         (n (length (cg-sol--spots game)))
         (cur (cg-get game :cursor)))
    (cg-put game :cursor (mod (+ cur delta) n))
    (cg-sol--redisplay)))

(defun cg-sol-left () "Move cursor left." (interactive) (cg-sol--move -1))
(defun cg-sol-right () "Move cursor right." (interactive) (cg-sol--move 1))
(defun cg-sol-up () "Move cursor left (previous pile)." (interactive) (cg-sol--move -1))
(defun cg-sol-down () "Move cursor right (next pile)." (interactive) (cg-sol--move 1))

(defun cg-sol--after (game)
  "Fill empty columns from the reserve, redisplay GAME, and announce a win."
  (cg-sol--autofill game)
  (cg-sol--redisplay)
  (when (cg-won-p game)
    (cg-put game :message "You won!  Press n for a new game.")
    (cg-sol--redisplay)
    (message "Solved!  Well played.")))

(defun cg-sol-new ()
  "Start a fresh deal of the same game."
  (interactive)
  (let ((game cg-sol--game))
    (cg-sol--deal game)
    (cg-sol--redisplay)))

(defun cg-sol-help ()
  "Describe the controls."
  (interactive)
  (message "Arrows: move  RET: pick up/drop  f: to foundation  a: auto  u: undo  n: new  g: redraw"))

(defun cg-sol-redraw () "Redraw the board." (interactive) (cg-sol--redisplay))

;;;; Rendering (console)

(defun cg-sol--render-card (card down sel cursor)
  "Return a propertized 3-column cell for CARD (DOWN, SEL, CURSOR flags)."
  (let* ((s (cg-sol-card-string card down))
         (faces nil))
    (when (and card (not down) (cg-sol-red-p card)) (push 'cg-red-suit faces))
    (when down (push 'cg-gap faces))
    (when sel (push 'cg-hint faces))
    (when cursor (push 'cg-cursor faces))
    (propertize (format "%3s " s) 'face (or faces 'default))))

(cl-defmethod cg-render ((game cg-solitaire-game))
  "Return a propertized string depicting GAME for a text display."
  (let* ((spots (cg-sol--spots game))
         (cur (cg-get game :cursor))
         (cur-spot (nth cur spots))
         (sel (cg-get game :sel))
         (sel-n (cg-get game :sel-n))
         (out (list)))
    (push (format "  %s   Moves: %d%s\n\n"
                  (oref game vname) (cg-get game :moves)
                  (if (> (oref game nfound) 0) "" 
                    (format "   Sets: %d/%d" (cg-get game :sets) (oref game target-sets))))
          out)
    ;; Top line: stock / waste / free cells / foundations.
    (let ((line "  "))
      (when (oref game has-stock)
        (let ((on (equal cur-spot '(stock . 0))))
          (setq line (concat line "Stock:"
                             (propertize (format "%-4s"
                                                 (if (cg-get game :stock) "##" "·"))
                                         'face (if on 'cg-cursor 'default))
                             (format "(%d) " (length (cg-get game :stock))))) ))
      (when (oref game has-waste)
        (let ((on (equal cur-spot '(waste . 0))) (w (car (last (cg-get game :waste)))))
          (setq line (concat line "Waste:"
                             (cg-sol--render-card w nil nil on)))))
      (when (oref game has-reserve)
        (let ((on (equal cur-spot '(reserve . 0))) (r (car (last (cg-get game :reserve)))))
          (setq line (concat line "Reserve:"
                             (cg-sol--render-card r nil nil on)
                             (format "(%d) " (length (cg-get game :reserve)))))))
      (dotimes (i (oref game nfree))
        (let ((on (equal cur-spot (cons 'free i))) (c (aref (cg-get game :free) i)))
          (setq line (concat line (format "F%d:" (1+ i))
                             (cg-sol--render-card c nil nil on)))))
      (dotimes (i (oref game nfound))
        (let ((on (equal cur-spot (cons 'found i)))
              (c (car (last (aref (cg-get game :found) i)))))
          (setq line (concat line (format "%d:" (1+ i))
                             (cg-sol--render-card c nil nil on)))))
      (push (concat line "\n\n") out))
    ;; Column headers.
    (let ((hdr "   "))
      (dotimes (c (oref game ncols))
        (let ((on (equal cur-spot (cons 'col c))))
          (setq hdr (concat hdr (propertize (format "%2d  " (1+ c))
                                            'face (if on 'cg-cursor 'default))))))
      (push (concat hdr "\n") out))
    ;; Column bodies, row by row.
    (let* ((tab (cg-get game :tableau))
           (maxlen (apply #'max 1 (mapcar #'length (append tab nil)))))
      (dotimes (r maxlen)
        (let ((row "   "))
          (dotimes (c (oref game ncols))
            (let* ((col (aref tab c))
                   (len (length col))
                   (card (and (< r len) (nth r col)))
                   (down (and card (< r (cg-sol--down game c))))
                   (selp (and (equal sel (cons 'col c))
                              card (>= r (- len sel-n))))
                   (cursorp (and (equal cur-spot (cons 'col c)) (= r (1- len)))))
              (setq row (concat row
                                (if (< r len)
                                    (cg-sol--render-card card down selp cursorp)
                                  "    ")))))
          (push (concat row "\n") out))))
    (push (format "\n  %s\n" (cg-get game :message)) out)
    (apply #'concat (nreverse out))))

(defun cg-sol--redisplay ()
  "Redraw the current solitaire buffer."
  (let ((game cg-sol--game)
        (inhibit-read-only t))
    (setq-local mode-line-process
                (format " [%s]" (if (cg-won-p game) "solved"
                                  (let ((s (cg-get game :sel)))
                                    (if s "carrying" "playing")))))
    (erase-buffer)
    (insert (cg-render game))
    (goto-char (point-min))))

;;;; Mode and commands

(defvar cg-sol-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-sol-left)
    (define-key map (kbd "<right>") #'cg-sol-right)
    (define-key map (kbd "<up>")    #'cg-sol-up)
    (define-key map (kbd "<down>")  #'cg-sol-down)
    (define-key map (kbd "RET") #'cg-sol-act)
    (define-key map (kbd "SPC") #'cg-sol-act)
    (define-key map "f" #'cg-sol-to-foundation)
    (define-key map "a" #'cg-sol-auto)
    (define-key map "u" #'cg-sol-undo)
    (define-key map "n" #'cg-sol-new)
    (define-key map "g" #'cg-sol-redraw)
    (define-key map "?" #'cg-sol-help)
    map)
  "Keymap for `cg-sol-mode'.")

(defun cg-sol--classic-keymap ()
  "Return a copy of `cg-sol-mode-map' with vi-style hjkl added."
  (let ((map (copy-keymap cg-sol-mode-map)))
    (define-key map "h" #'cg-sol-left)
    (define-key map "l" #'cg-sol-right)
    (define-key map "j" #'cg-sol-down)
    (define-key map "k" #'cg-sol-up)
    map))

(define-derived-mode cg-sol-mode special-mode "Solitaire"
  "Major mode for the tableau solitaires."
  (setq-local truncate-lines t)
  (when (eq cg-keys 'classic)
    (use-local-map (cg-sol--classic-keymap))))

(defun cg-sol--play (class)
  "Start a solitaire game of CLASS in its own buffer."
  (let* ((game (cg-sol--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (oref game vname)))))
    (with-current-buffer buf
      (cg-sol-mode)
      (setq cg-sol--game game)
      (cg-sol--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun cg-klondike ()
  "Play Klondike, the classic solitaire."
  (interactive) (cg-sol--play 'cg-klondike-game))

;;;###autoload
(defun cg-freecell ()
  "Play FreeCell solitaire."
  (interactive) (cg-sol--play 'cg-freecell-game))

;;;###autoload
(defun cg-spider ()
  "Play Spider solitaire (two decks)."
  (interactive) (cg-sol--play 'cg-spider-game))

;;;###autoload
(defun cg-yukon ()
  "Play Yukon solitaire."
  (interactive) (cg-sol--play 'cg-yukon-game))


;;;; More games: Forty Thieves, Scorpion, Canfield

(defclass cg-forty-game (cg-solitaire-game)
  ((ncols :initform 10) (ndecks :initform 2) (nfound :initform 8)
   (has-stock :initform t) (has-waste :initform t) (redeal :initform nil)
   (build :initform 'suit) (run-rule :initform 'suit) (empty-rule :initform 'any)
   (vname :initform "Forty Thieves"))
  "Forty Thieves: two decks, ten columns, eight foundations, no redeal.")

(cl-defmethod cg-sol--layout ((_ cg-forty-game))
  (make-list 10 (cons 0 4)))

(defclass cg-scorpion-game (cg-solitaire-game)
  ((ncols :initform 7) (nfound :initform 0) (has-stock :initform t)
   (build :initform 'suit) (run-rule :initform 'any) (empty-rule :initform 'king)
   (target-sets :initform 4) (vname :initform "Scorpion"))
  "Scorpion: build down by suit, move any buried group, clear four runs.")

(cl-defmethod cg-sol--layout ((_ cg-scorpion-game))
  (append (make-list 4 (cons 3 4)) (make-list 3 (cons 0 7))))

(cl-defmethod cg-sol--stock-action ((game cg-scorpion-game))
  "Deal the three stock cards onto the first three columns."
  (let ((stock (cg-get game :stock)))
    (if (null stock)
        (cg-put game :message "The stock is empty.")
      (cg-sol--snapshot game)
      (dotimes (c (min 3 (length stock)))
        (cg-sol--set-col game c (append (cg-sol--col game c) (last stock 1)))
        (setq stock (butlast stock 1)))
      (cg-put game :stock stock)
      (cg-sol--harvest game)
      (cg-put game :message "Dealt the stock onto the first columns."))))

(defclass cg-canfield-game (cg-solitaire-game)
  ((ncols :initform 4) (nfound :initform 4) (has-stock :initform t)
   (has-waste :initform t) (has-reserve :initform t) (draw :initform 3)
   (redeal :initform t) (build :initform 'alt) (run-rule :initform 'alt)
   (empty-rule :initform 'any) (wrap :initform t) (vname :initform "Canfield"))
  "Canfield: a 13-card reserve and a variable foundation base rank.")

(cl-defmethod cg-sol--deal ((game cg-canfield-game))
  "Deal a Canfield layout: reserve, base foundation, four columns, stock."
  (let* ((deck (cg-sol--make-deck 1))
         (reserve (cl-loop repeat 13 collect (pop deck)))
         (first (pop deck))
         (found (make-vector 4 nil))
         (tableau (make-vector 4 nil))
         (down (make-vector 4 0)))
    (oset game base (cdr first))
    (aset found 0 (list first))
    (dotimes (c 4) (aset tableau c (list (pop deck))))
    (cg-put game :reserve reserve)
    (cg-put game :tableau tableau)
    (cg-put game :down down)
    (cg-put game :found found)
    (cg-put game :free (make-vector 0 nil))
    (cg-put game :stock deck)
    (cg-put game :waste nil)
    (cg-put game :sets 0)
    (cg-put game :moves 0)
    (cg-put game :cursor 0)
    (cg-put game :sel nil)
    (cg-put game :sel-n 0)
    (cg-put game :history nil)
    (cg-put game :message
            (format "Canfield.  Foundations build up from %s (wrapping).  RET deals three."
                    (aref cg-sol-ranks (cdr first))))
    game))

(cl-defmethod cg-sol--autofill ((_ cg-solitaire-game)) nil)
(cl-defmethod cg-sol--autofill ((game cg-canfield-game))
  "Fill empty columns from the reserve, as Canfield requires."
  (dotimes (c (oref game ncols))
    (when (and (null (cg-sol--col game c)) (cg-get game :reserve))
      (let ((card (car (last (cg-get game :reserve)))))
        (cg-put game :reserve (butlast (cg-get game :reserve) 1))
        (cg-sol--set-col game c (list card))))))

;;;###autoload
(defun cg-forty-thieves ()
  "Play Forty Thieves solitaire (two decks)."
  (interactive) (cg-sol--play 'cg-forty-game))

;;;###autoload
(defun cg-scorpion ()
  "Play Scorpion solitaire."
  (interactive) (cg-sol--play 'cg-scorpion-game))

;;;###autoload
(defun cg-canfield ()
  "Play Canfield solitaire."
  (interactive) (cg-sol--play 'cg-canfield-game))

(provide 'cg-solitaire)
;;; cg-solitaire.el ends here
