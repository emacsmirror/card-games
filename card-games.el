;;; card-games.el --- Play card games in Emacs (console + SVG)  -*- lexical-binding: t; -*-

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
(require 'cg-bid-net)
(require 'cg-solitaire)
(require 'cg-trick)
(require 'cg-eights)
(require 'cg-patience)
(require 'cg-president)
(require 'cg-rummy)
(require 'cg-rum500)
(require 'cg-handfoot)
(require 'cg-match)
(require 'cg-cribbage)
(require 'cg-scopa)
(require 'cg-trick-ext)
(require 'cg-spite)
(require 'cg-bridge)

(defvar card-games-list
  '(("500 (Bid)" cg-bid
     "Four-handed partnership trick-taking versus three AI opponents.")
    ("Gaps  (Montana)" cg-montana
     "Solitaire: a Two anchors each row; build up 2 through King.")
    ("Hell's Half-Acre" cg-hells-half-acre
     "Solitaire: a King anchors each row; build down King through 2.")
    ("Klondike" cg-klondike
     "Solitaire: the classic; build the foundations up by suit from the Ace.")
    ("FreeCell" cg-freecell
     "Solitaire: every card in view, four free cells, a game of skill.")
    ("Spider" cg-spider
     "Solitaire: two decks; build down and clear eight same-suit runs.")
    ("Yukon" cg-yukon
     "Solitaire: Klondike's layout, all face up; move any buried group.")
    ("Hearts" cg-hearts
     "Trick-taking: dodge every heart and the Queen of Spades.")
    ("Spades" cg-spades
     "Trick-taking: partnership bidding to 500; spades are always trump.")
    ("Crazy Eights" cg-eights
     "Shedding: match the suit or rank; eights are wild.")
    ("Canfield" cg-canfield
     "Solitaire: a 13-card reserve and a shifting foundation base rank.")
    ("Forty Thieves" cg-forty-thieves
     "Solitaire: two decks, ten columns, eight foundations, no redeal.")
    ("Scorpion" cg-scorpion
     "Solitaire: build down by suit and free four buried King-to-Ace runs.")
    ("Golf" cg-golf
     "Solitaire: clear the layout one rank at a time onto the waste.")
    ("TriPeaks" cg-tripeaks
     "Solitaire: clear three peaks with Ace-King wrapping chains.")
    ("Pyramid" cg-pyramid
     "Solitaire: remove pairs of cards that sum to thirteen.")
    ("Whist" cg-whist
     "Trick-taking: fixed trump, no bidding, race past the book of six.")
    ("Oh Hell" cg-ohhell
     "Trick-taking: shrinking hands; bid the exact tricks you will take.")
    ("President" cg-president
     "Climbing: shed your hand; first out rules, last out scrubs.")
    ("Gin Rummy" cg-gin
     "Rummy: form melds, knock with little deadwood; head to head to 100.")
    ("Rummy" cg-rummy-basic
     "Rummy: meld your whole hand to the table to go out.")
    ("Rummy 500" cg-rum500
     "Rummy: score the cards you lay down; race past 500.")
    ("Hand & Foot" cg-handfoot
     "Rummy: partnership Canasta cousin; build books from hand and foot.")
    ("Go Fish" cg-go-fish
     "Matching: ask for ranks and collect books of four.")
    ("Old Maid" cg-old-maid
     "Matching: shed pairs and avoid the leftover Queen.")
    ("Cribbage" cg-cribbage
     "Pegging and the show: fifteens, pairs, runs, and his nobs to 121.")
    ("Scopa" cg-scopa
     "Capturing: take table cards by value; sweep for a scopa.")
    ("Casino" cg-casino
     "Capturing: pairs and sums; big and little casino, aces, sweeps.")
    ("Euchre" cg-euchre
     "Trick-taking: 24 cards, bowers, order up; partnership to 10.")
    ("Pitch" cg-pitch
     "Trick-taking: bid, pitch to set trump, score High-Low-Jack-Game.")
    ("Briscola" cg-briscola
     "Trick-taking: fixed trump, no follow; capture the points to 61.")
    ("Spite & Malice" cg-spite
     "Climbing patience: race to empty your goal pile; Kings are wild.")
    ("Bridge" cg-bridge
     "Trick-taking: the auction, the dummy, and rubber scoring, to 121."))
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
  (setq-local cursor-type cg-cursor-type))

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
