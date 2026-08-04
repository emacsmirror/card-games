;;; card-games-render.el --- Renderer "skins" for card games  -*- lexical-binding: t; -*-

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

;; Concrete display treatments ("skins") built on the `card-games-renderer'
;; protocol from card-games-core.  Each treatment is a small EIEIO class that
;; registers itself by name:
;;
;;   text      plain UNICODE text (works in a terminal and on Android)
;;   svg       SVG cards on a graphical display
;;   svg-fill  a full-window SVG table that grows with the window
;;
;; A game draws itself by calling `card-games-render-game', which selects the
;; game's current renderer (falling back to a default treatment chosen
;; for the display) and dispatches `card-games-renderer-draw'.  The actual,
;; game-specific drawing is supplied as methods specialised on a
;; (TREATMENT GAME) pair in the individual game files.

;;; Code:

(require 'card-games-core)

(defgroup card-games-render nil
  "Display treatments (\"skins\") for card games."
  :group 'card-games
  :prefix "card-games-render-")

(defclass card-games-text-renderer (card-games-renderer)
  ((name :initform 'text))
  "Plain UNICODE text treatment.")

(defclass card-games-svg-renderer (card-games-renderer)
  ((name :initform 'svg))
  "SVG cards on a graphical display.")

(defclass card-games-svg-fill-renderer (card-games-svg-renderer)
  ((name :initform 'svg-fill))
  "Full-window SVG table that grows to fill the window.")

(card-games-register-renderer 'text     'card-games-text-renderer)
(card-games-register-renderer 'svg      'card-games-svg-renderer)
(card-games-register-renderer 'svg-fill 'card-games-svg-fill-renderer)

(defcustom card-games-render-default-treatment 'auto
  "Default display treatment for games.
The value `auto' chooses `svg' on a graphical display and `text'
otherwise.  It may instead name a treatment registered in
`card-games-renderers', such as `text', `svg', or `svg-fill'."
  :type '(choice (const :tag "Automatic (svg if graphical, else text)" auto)
                 (const text) (const svg) (const svg-fill)
                 (symbol :tag "Other registered treatment"))
  :group 'card-games-render)

(defun card-games-render-resolve-treatment (&optional name)
  "Return a concrete treatment name, resolving `auto' and NAME for this display.
NAME defaults to `card-games-render-default-treatment'."
  (let ((n (or name card-games-render-default-treatment)))
    (if (eq n 'auto)
        (if (display-graphic-p) 'svg 'text)
      n)))

(defun card-games-render-game (game)
  "Draw GAME with its current renderer, creating a default one if needed.
The default treatment comes from `card-games-render-resolve-treatment'."
  (let ((r (or (oref game renderer)
               (let ((new (card-games-make-renderer (card-games-render-resolve-treatment))))
                 (oset game renderer new)
                 new))))
    (card-games-renderer-draw r game)))

(defun card-games-render-set-treatment (game name)
  "Switch GAME to the treatment NAME and return its new renderer."
  (oset game renderer (card-games-make-renderer name)))

(cl-defgeneric card-games-render-text (game)
  "Return the plain-text display string for GAME.
The default falls back to the game's `card-games-render' method."
  (card-games-render game))

(cl-defgeneric card-games-render-svg (game)
  "Return (DISPLAY-STRING . REGIONS) for GAME's SVG treatment.
The default falls back to the `card-games-render' string with no click regions."
  (cons (card-games-render game) nil))

(cl-defmethod card-games-renderer-draw ((r card-games-text-renderer) (game card-games-game))
  "Draw GAME as plain text on renderer R, clearing any click regions."
  (oset r regions nil)
  (insert (card-games-render-text game)))

(cl-defmethod card-games-renderer-draw ((r card-games-svg-renderer) (game card-games-game))
  "Draw GAME as SVG and record its click regions on R."
  (let ((res (card-games-render-svg game)))
    (oset r regions (cdr res))
    (insert (car res))))

(cl-defmethod card-games-renderer-hit ((r card-games-svg-renderer) (game card-games-game) position)
  "Map POSITION to a GAME action via R regions from the last draw."
  (ignore game)
  (let ((xy (posn-object-x-y position)) (sc (card-games-scale)))
    (and xy (card-games-regions-hit (oref r regions)
                            (round (/ (car xy) sc)) (round (/ (cdr xy) sc))))))

(provide 'card-games-render)
;;; card-games-render.el ends here
