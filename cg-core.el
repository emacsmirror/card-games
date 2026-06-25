;;; cg-core.el --- Shared engine core for card games  -*- lexical-binding: t; -*-

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

;; A small EIEIO scaffolding shared by the games in this package.  It
;; provides the abstract `cg-game' class with a plist "environment" for
;; mutable per-game state, the `cg-render' and `cg-won-p' generics, and
;; a handful of card and display utilities (suit glyphs, colour
;; helpers, a shuffle, and common faces).
;;
;; Cards are normally represented as a cons cell (SUIT . RANK) with suit
;; indices 0=Spades 1=Clubs 2=Diamonds 3=Hearts; games define their own
;; rank scales.  nil conventionally denotes an empty slot.

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defgroup card-games nil
  "Play card games in Emacs."
  :group 'games
  :prefix "cg-")


;;;; Engine base

(defcustom cg-keys 'emacs
  "Keybinding scheme for the card games.
`emacs' follows Emacs conventions (arrow keys to move, RET to act,
g to redraw).  `classic' additionally enables vi-style hjkl movement
and SPC as an action key.  Takes effect the next time a game starts."
  :type '(choice (const :tag "Emacs conventions" emacs)
                 (const :tag "Classic (adds hjkl, SPC)" classic))
  :group 'card-games)

(defclass cg-game ()
  ((name :initarg :name :initform "game" :type string
         :documentation "Human-readable game name.")
   (env  :initarg :env  :initform nil
         :documentation "Mutable per-game data, stored as a plist.")
   (renderer :initarg :renderer :initform nil
             :documentation "Current `cg-renderer', or nil for the default."))
  "Abstract base class for card games."
  :abstract t)

(cl-defgeneric cg-render (game)
  "Return a propertized string depicting GAME.")

(cl-defgeneric cg-won-p (game)
  "Return non-nil when GAME has been won.")

(cl-defmethod cg-get ((game cg-game) key)
  "Return value for KEY in GAME's environment."
  (plist-get (oref game env) key))

(cl-defmethod cg-put ((game cg-game) key value)
  "Set KEY to VALUE in GAME's environment and return VALUE."
  (oset game env (plist-put (oref game env) key value))
  value)


;;;; Renderer "skins"

;; A renderer (a "skin") is a display treatment: it knows how to draw a
;; game and how to map a click back to a game action.  Treatments are
;; EIEIO classes registered by name in `cg-renderers'; a game holds the
;; one it is currently drawn with.  This lets a single game be shown as
;; plain text, as SVG, or as a full-window SVG table without subclassing
;; the game itself once per treatment.  Concrete treatments and the
;; game-specific drawing methods live in cg-render.el and the games.

(defclass cg-renderer ()
  ((name :initarg :name :initform 'text :type symbol
         :documentation "Symbol naming this treatment."))
  "Abstract base class for a display treatment (a \"skin\")."
  :abstract t)

(cl-defgeneric cg-renderer-draw (renderer game)
  "Draw GAME under RENDERER by inserting into the current buffer.")

(cl-defgeneric cg-renderer-hit (renderer game position)
  "Map POSITION under RENDERER to an action on GAME.
Return non-nil when the click was handled.")

(cl-defmethod cg-renderer-draw ((renderer cg-renderer) (game cg-game))
  "Default method: report that no drawing method exists for this pair."
  (error "No `cg-renderer-draw' for %s under the `%s' renderer"
         (eieio-object-class-name game) (oref renderer name)))

(cl-defmethod cg-renderer-hit ((_renderer cg-renderer) (_game cg-game) _position)
  "Default method: treat the click as unhandled."
  nil)

(defvar cg-renderers nil
  "Alist mapping a treatment name (a symbol) to a `cg-renderer' subclass.
Populate it with `cg-register-renderer' and look entries up with
`cg-make-renderer'.")

(defun cg-register-renderer (name class)
  "Register renderer CLASS (an EIEIO class) under the treatment NAME."
  (setf (alist-get name cg-renderers) class))

(defun cg-make-renderer (name)
  "Return a fresh renderer instance for treatment NAME, or nil if unknown."
  (let ((class (alist-get name cg-renderers)))
    (and class (make-instance class :name name))))

(defun cg-renderer-names ()
  "Return the registered treatment names."
  (mapcar #'car cg-renderers))


;;;; Cards and colours

(defcustom cg-symbols
  '((0 . "♠") (1 . "♣") (2 . "♦") (3 . "♥") (joker . "★"))
  "Glyphs used to draw suits, both as text and inside the SVG cards.
The value is an alist mapping a suit index (0 spades, 1 clubs,
2 diamonds, 3 hearts) or the symbol `joker' to the string drawn for it.
Customize this to use alternative Unicode symbols, for example the
outlined suits \"♤\" \"♧\" \"♢\" \"♡\"."
  :type '(alist :key-type sexp :value-type string)
  :group 'card-games)

(defconst cg-suit-names ["Spades" "Clubs" "Diamonds" "Hearts"]
  "Suit names indexed 0..3 to match the suit indices used throughout.")

(defun cg-suit-glyph (suit)
  "Return the glyph drawn for SUIT, a suit index 0-3 or the symbol `joker'.
The glyphs are taken from `cg-symbols'."
  (or (cdr (assoc suit cg-symbols))
      (and (integerp suit) (aref cg-suit-names suit))
      "?"))

(defsubst cg-red-suit-p (suit)
  "Return non-nil when SUIT index denotes a red suit."
  (memq suit '(2 3)))

(defsubst cg-sister-suit (suit)
  "Return the other suit index of the same colour as SUIT."
  (pcase suit (0 1) (1 0) (2 3) (3 2)))

(defun cg-shuffle (seq)
  "Return a new list with the elements of SEQ in random order."
  (let* ((v (vconcat seq))
         (n (length v)))
    (dotimes (i n)
      (let ((j (+ i (random (- n i)))))
        (cl-rotatef (aref v i) (aref v j))))
    (append v nil)))


;;;; Shared faces

(defface cg-red-suit '((t :foreground "red3"))
  "Face for red-suited cards." :group 'card-games)

(defface cg-cursor '((t :inverse-video t))
  "Face for the cell or card under the cursor." :group 'card-games)

(defface cg-gap '((t :foreground "gray50"))
  "Face for an empty slot." :group 'card-games)

(defface cg-hint '((t :foreground "green3" :weight bold))
  "Face for a valid move target (a fillable gap)." :group 'card-games)

(defun cg-color (face attribute fallback)
  "Return FACE's ATTRIBUTE colour if usable on this display, else FALLBACK.
Degrades gracefully when there is no theme/frame (e.g. in a terminal
or batch), so callers always get a drawable colour string."
  (let ((c (ignore-errors (face-attribute face attribute nil t))))
    (if (and (stringp c)
             (not (string-prefix-p "unspecified" c))
             (ignore-errors (color-defined-p c)))
        c
      fallback)))

(defun cg-scale ()
  "Return the SVG card scale factor for the current buffer.
Tracks `text-scale-increase'/`text-scale-decrease' via the buffer-local
`text-scale-mode-amount', so enlarging the text enlarges the cards."
  (let ((amt (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0)))
    (max 0.4 (min 4.0 (expt 1.15 amt)))))

(provide 'cg-core)
;;; cg-core.el ends here
