;;; card-games.el --- Play card games in Emacs (console + SVG)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.50
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

;; Card games for Emacs, rendered as UNICODE text in a terminal and as
;; SVG cards on a graphical display.  This file is the umbrella: it
;; pulls in the individual games and offers a chooser.
;;
;; Run `M-x card-game' for a menu, or start a game directly:
;;
;;   `cg-bid'   -- 500, the four-handed partnership trick-taking game,
;;                 played against three computer opponents.
;;   `cg-gaps'  -- Gaps / Montana / "Hell's Half-Acre" solitaire.
;;
;; New games register themselves by adding to `card-games-list'.

;;; Code:

(require 'cg-core)
(require 'cg-render)
(require 'cg-net)
(require 'cg-gaps)
(require 'cg-bid-ui)

(defvar card-games-list
  '(("500 (Bid)" cg-bid
     "Four-handed partnership trick-taking versus three AI opponents.")
    ("Gaps  (Montana)" cg-montana
     "Solitaire: a Two anchors each row; build up 2 through King.")
    ("Hell's Half-Acre" cg-hells-half-acre
     "Solitaire: a King anchors each row; build down King through 2."))
  "Registry of playable games.
Each entry is (NAME COMMAND DESCRIPTION); `card-game' lists them.")

(defun card-game--launch (button)
  "Start the game whose command is stored on BUTTON."
  (let ((cmd (button-get button 'card-game-command)))
    (quit-window)
    (call-interactively cmd)))

(defvar card-game-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map)
  "Keymap for `card-game-mode'.")

(define-derived-mode card-game-mode special-mode "Card-Games"
  "Major mode for the `card-game' chooser."
  (setq-local cursor-type nil))

;;;###autoload
(defun card-game ()
  "Open a chooser listing the available card games.
Press RET (or click) on a game to start it."
  (interactive)
  (let ((buf (get-buffer-create "*Card Games*")))
    (with-current-buffer buf
      (card-game-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "  Card Games for Emacs\n" 'face 'bold))
        (insert (propertize
                 "  Choose a game with RET or the mouse.  q to quit.\n\n"
                 'face 'shadow))
        (dolist (g card-games-list)
          (insert "    ")
          (insert-text-button
           (format "%-26s" (nth 0 g))
           'face 'link
           'help-echo (nth 2 g)
           'card-game-command (nth 1 g)
           'action #'card-game--launch)
          (insert (propertize (concat "  " (nth 2 g) "\n") 'face 'shadow)))
        (insert "\n")))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (forward-button 1)))

;;;###autoload
(defalias 'card-games #'card-game
  "Alias for `card-game'.")

(defconst card-games-themes
  '((classic  :felt "#15692f" :theme t)
    (dark     :felt "#23272e" :back "#3b4252" :highlight "#88c0d0" :theme nil)
    (contrast :felt "#0a0a0a" :back "#000000" :highlight "#ffd400" :theme nil))
  "Named table/card colour presets for `card-games-set-theme'.")

;;;###autoload
(defun card-games-set-theme (name)
  "Apply the card-games colour preset NAME (see `card-games-themes')."
  (interactive
   (list (intern (completing-read
                  "Card-games theme: "
                  (mapcar (lambda (e) (symbol-name (car e))) card-games-themes)
                  nil t))))
  (let ((p (alist-get name card-games-themes)))
    (unless p (user-error "No such card-games theme: %s" name))
    (setq cg-bid-felt-color (plist-get p :felt))
    (when (plist-member p :theme)
      (setq cg-svg-theme-colors (plist-get p :theme)))
    (when (plist-get p :back) (setq cg-svg-back-color (plist-get p :back)))
    (when (plist-get p :highlight)
      (setq cg-svg-highlight-color (plist-get p :highlight)))
    (message "card-games theme: %s" name)))

(provide 'card-games)
;;; card-games.el ends here
