;;; cg-core.el --- Shared engine core for card games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.50
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games
;; URL: https://github.com/corwin/card-games

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
         :documentation "Mutable per-game data, stored as a plist."))
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


;;;; Cards and colours

(defconst cg-suits ["♠" "♣" "♦" "♥"]
  "Suit glyphs indexed 0..3: spades, clubs, diamonds, hearts.")

(defconst cg-suit-names ["Spades" "Clubs" "Diamonds" "Hearts"]
  "Suit names indexed to match `cg-suits'.")

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
