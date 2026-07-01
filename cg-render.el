;;; cg-render.el --- Renderer "skins" for card games  -*- lexical-binding: t; -*-

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

;; Concrete display treatments ("skins") built on the `cg-renderer'
;; protocol from cg-core.  Each treatment is a small EIEIO class that
;; registers itself by name:
;;
;;   text      plain UNICODE text (works in a terminal and on Android)
;;   svg       SVG cards on a graphical display
;;   svg-fill  a full-window SVG table that grows with the window
;;
;; A game draws itself by calling `cg-render-game', which selects the
;; game's current renderer (falling back to a default treatment chosen
;; for the display) and dispatches `cg-renderer-draw'.  The actual,
;; game-specific drawing is supplied as methods specialised on a
;; (TREATMENT GAME) pair in the individual game files.

;;; Code:

(require 'cg-core)

(defgroup cg-render nil
  "Display treatments (\"skins\") for card games."
  :group 'card-games
  :prefix "cg-render-")

(defclass cg-text-renderer (cg-renderer)
  ((name :initform 'text))
  "Plain UNICODE text treatment.")

(defclass cg-svg-renderer (cg-renderer)
  ((name :initform 'svg))
  "SVG cards on a graphical display.")

(defclass cg-svg-fill-renderer (cg-svg-renderer)
  ((name :initform 'svg-fill))
  "Full-window SVG table that grows to fill the window.")

(cg-register-renderer 'text     'cg-text-renderer)
(cg-register-renderer 'svg      'cg-svg-renderer)
(cg-register-renderer 'svg-fill 'cg-svg-fill-renderer)

(defcustom cg-render-default-treatment 'auto
  "Default display treatment for games.
The value `auto' chooses `svg' on a graphical display and `text'
otherwise.  It may instead name a treatment registered in
`cg-renderers', such as `text', `svg', or `svg-fill'."
  :type '(choice (const :tag "Automatic (svg if graphical, else text)" auto)
                 (const text) (const svg) (const svg-fill)
                 (symbol :tag "Other registered treatment"))
  :group 'cg-render)

(defun cg-render-resolve-treatment (&optional name)
  "Return a concrete treatment name, resolving `auto' and NAME for this display.
NAME defaults to `cg-render-default-treatment'."
  (let ((n (or name cg-render-default-treatment)))
    (if (eq n 'auto)
        (if (display-graphic-p) 'svg 'text)
      n)))

(defun cg-render-game (game)
  "Draw GAME with its current renderer, creating a default one if needed.
The default treatment comes from `cg-render-resolve-treatment'."
  (let ((r (or (oref game renderer)
               (let ((new (cg-make-renderer (cg-render-resolve-treatment))))
                 (oset game renderer new)
                 new))))
    (cg-renderer-draw r game)))

(defun cg-render-set-treatment (game name)
  "Switch GAME to the treatment NAME and return its new renderer."
  (oset game renderer (cg-make-renderer name)))

(cl-defgeneric cg-render-text (game)
  "Return the plain-text display string for GAME.
The default falls back to the game's `cg-render' method."
  (cg-render game))

(cl-defgeneric cg-render-svg (game)
  "Return (DISPLAY-STRING . REGIONS) for GAME's SVG treatment.
The default falls back to the `cg-render' string with no click regions."
  (cons (cg-render game) nil))

(cl-defmethod cg-renderer-draw ((r cg-text-renderer) (game cg-game))
  "Draw GAME as plain text on renderer R, clearing any click regions."
  (oset r regions nil)
  (insert (cg-render-text game)))

(cl-defmethod cg-renderer-draw ((r cg-svg-renderer) (game cg-game))
  "Draw GAME as SVG and record its click regions on R."
  (let ((res (cg-render-svg game)))
    (oset r regions (cdr res))
    (insert (car res))))

(cl-defmethod cg-renderer-hit ((r cg-svg-renderer) (game cg-game) position)
  "Map POSITION to a GAME action via R regions from the last draw."
  (ignore game)
  (let ((xy (posn-object-x-y position)) (sc (cg-scale)))
    (and xy (cg-regions-hit (oref r regions)
                            (round (/ (car xy) sc)) (round (/ (cdr xy) sc))))))

(provide 'cg-render)
;;; cg-render.el ends here
