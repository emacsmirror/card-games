;;; card-games.el --- Play card games (console + SVG)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.92
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
;; Run `M-x card-games' for a menu, or start a game directly:
;;
;;   `card-games-bid'   -- 500, the four-handed partnership trick-taking game,
;;                 played against three computer opponents.
;;   `card-games-gaps'  -- Gaps / Montana / "Hell's Half-Acre" solitaire.
;;
;; New games register themselves by adding to `card-games-list'.

;;; Code:

(require 'card-games-core)
(require 'card-games-render)
(require 'card-games-net)
(require 'card-games-gaps)
(require 'card-games-bid-ui)
(require 'card-games-bid-net)
(require 'card-games-solitaire)
(require 'card-games-trick)
(require 'card-games-eights)
(require 'card-games-patience)
(require 'card-games-president)
(require 'card-games-rummy)
(require 'card-games-rum500)
(require 'card-games-handfoot)
(require 'card-games-match)
(require 'card-games-cribbage)
(require 'card-games-scopa)
(require 'card-games-trick-ext)
(require 'card-games-spite)
(require 'card-games-bridge)
(require 'card-games-crapette)

(defvar card-games-list
  '(("500 (Bid)" card-games-bid
     "Four-handed partnership trick-taking versus three AI opponents.")
    ("Gaps  (Montana)" card-games-montana
     "Solitaire: a Two anchors each row; build up 2 through King.")
    ("Hell's Half-Acre" card-games-hells-half-acre
     "Solitaire: a King anchors each row; build down King through 2.")
    ("Klondike" card-games-klondike
     "Solitaire: the classic; build the foundations up by suit from the Ace.")
    ("FreeCell" card-games-freecell
     "Solitaire: every card in view, four free cells, a game of skill.")
    ("Spider" card-games-spider
     "Solitaire: two decks; build down and clear eight same-suit runs.")
    ("Yukon" card-games-yukon
     "Solitaire: Klondike's layout, all face up; move any buried group.")
    ("Hearts" card-games-hearts
     "Trick-taking: dodge every heart and the Queen of Spades.")
    ("Spades" card-games-spades
     "Trick-taking: partnership bidding to 500; spades are always trump.")
    ("Crazy Eights" card-games-eights
     "Shedding: match the suit or rank; eights are wild.")
    ("Canfield" card-games-canfield
     "Solitaire: a 13-card reserve and a shifting foundation base rank.")
    ("Russian Bank" card-games-russian-bank
     "Solitaire: eight houses down by alternating colour; a reserve feeds them.")
    ("Forty Thieves" card-games-forty-thieves
     "Solitaire: two decks, ten columns, eight foundations, no redeal.")
    ("Scorpion" card-games-scorpion
     "Solitaire: build down by suit and free four buried King-to-Ace runs.")
    ("Golf" card-games-golf
     "Solitaire: clear the layout one rank at a time onto the waste.")
    ("TriPeaks" card-games-tripeaks
     "Solitaire: clear three peaks with Ace-King wrapping chains.")
    ("Pyramid" card-games-pyramid
     "Solitaire: remove pairs of cards that sum to thirteen.")
    ("Whist" card-games-whist
     "Trick-taking: fixed trump, no bidding, race past the book of six.")
    ("Oh Hell" card-games-ohhell
     "Trick-taking: shrinking hands; bid the exact tricks you will take.")
    ("President" card-games-president
     "Climbing: shed your hand; first out rules, last out scrubs.")
    ("Gin Rummy" card-games-gin
     "Rummy: form melds, knock with little deadwood; head to head to 100.")
    ("Rummy" card-games-rummy-basic
     "Rummy: meld your whole hand to the table to go out.")
    ("Rummy 500" card-games-rum500
     "Rummy: score the cards you lay down; race past 500.")
    ("Hand & Foot" card-games-handfoot
     "Rummy: partnership Canasta cousin; build books from hand and foot.")
    ("Go Fish" card-games-go-fish
     "Matching: ask for ranks and collect books of four.")
    ("Old Maid" card-games-old-maid
     "Matching: shed pairs and avoid the leftover Queen.")
    ("Cribbage" card-games-cribbage
     "Pegging and the show: fifteens, pairs, runs, and his nobs to 121.")
    ("Scopa" card-games-scopa
     "Capturing: take table cards by value; sweep for a scopa.")
    ("Casino" card-games-casino
     "Capturing: pairs and sums; big and little casino, aces, sweeps.")
    ("Euchre" card-games-euchre
     "Trick-taking: 24 cards, bowers, order up; partnership to 10.")
    ("Pitch" card-games-pitch
     "Trick-taking: bid, pitch to set trump, score High-Low-Jack-Game.")
    ("Briscola" card-games-briscola
     "Trick-taking: fixed trump, no follow; capture the points to 61.")
    ("Spite & Malice" card-games-spite
     "Climbing patience: race to empty your goal pile; Kings are wild.")
    ("Bridge" card-games-bridge
     "Trick-taking: the auction, the dummy, and rubber scoring, to 121.")
    ("Crapette (Russian Bank)" card-games-crapette
     "Two-player Russian Bank versus the computer; empty your reserve, hand, and waste."))
  "Registry of playable games.
Each entry is (NAME COMMAND DESCRIPTION); `card-games' lists them.")

(defvar card-games--svg-card-vars
  '(card-games-sol-svg-cards card-games-trick-svg-cards card-games-rummy-svg-cards card-games-eights-svg-cards
    card-games-bridge-svg-cards card-games-crapette-svg-cards card-games-pat-svg-cards card-games-pres-svg-cards)
  "Per-game SVG-cards toggles that `card-games-set-treatment' flips together.")

(defvar card-games--full-svg-vars
  '(card-games-gaps-svg-ui card-games-bid-svg-ui)
  "Full-window SVG toggles (Gaps and 500) used by the `full' treatment.")

(defvar card-games-treatment 'svg
  "Display treatment chosen from the menu: `text', `svg', or `full'.")

;;;###autoload
(defun card-games-set-treatment (treatment)
  "Set how games are drawn to TREATMENT.
TREATMENT is `text' (UNICODE), `svg' (cards), or `full'.
`full' also uses the full-window SVG table where a game has one (Gaps and
500).  Takes effect the next time a game is drawn -- press g to redraw an
open game.  Gaps and 500 are always graphical on a window system."
  (interactive
   (list (intern (completing-read "Treatment: " '("text" "svg" "full") nil t))))
  (setq card-games-treatment treatment)
  (let ((cards (and (memq treatment '(svg full)) t))
        (full (and (eq treatment 'full) t)))
    (dolist (v card-games--svg-card-vars) (when (boundp v) (set v cards)))
    (dolist (v card-games--full-svg-vars) (when (boundp v) (set v full))))
  (when (called-interactively-p 'interactive)
    (message "Display treatment: %s" treatment)))

(defun card-games--cycle-treatment (_button)
  "Cycle the display treatment and refresh the chooser."
  (card-games-set-treatment
   (pcase card-games-treatment ('text 'svg) ('svg 'full) (_ 'text)))
  (card-games))

(defun card-games--cycle-ai (_button)
  "Cycle the AI difficulty (`card-games-ai-level') and refresh the chooser."
  (setq card-games-ai-level (pcase card-games-ai-level ('easy 'normal) ('normal 'hard) (_ 'easy)))
  (card-games))

;;;###autoload
(defun card-games-set-ai-level (level)
  "Set the computer-opponent difficulty to LEVEL (easy, normal, or hard)."
  (interactive
   (list (intern (completing-read "AI level: " '("easy" "normal" "hard") nil t))))
  (setq card-games-ai-level level)
  (message "AI level: %s" level))

(defun card-games--launch (button)
  "Start the game whose command is stored on BUTTON."
  (let ((cmd (button-get button 'card-games-command)))
    (quit-window)
    (call-interactively cmd)))

(defvar card-games-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" #'forward-button)
    (define-key map "p" #'backward-button)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    map)
  "Keymap for `card-games-mode'.")

(define-derived-mode card-games-mode special-mode "Card-Games"
  "Major mode for the `card-games' chooser."
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games ()
  "Open a chooser listing the available card games.
Press RET (or click) on a game to start it."
  (interactive)
  (when (and (boundp 'card-games-svg-card-back) (eq card-games-svg-card-back 'random)
             (fboundp 'card-games-svg--roll-back))
    (card-games-svg--roll-back))                 ; a fresh random back per menu visit
  (let ((buf (get-buffer-create "*Card Games*")))
    (with-current-buffer buf
      (card-games-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "  Card Games for Emacs\n" 'face 'bold))
        (insert (propertize
                 "  Choose a game with RET or the mouse.  q to quit.\n"
                 'face 'shadow))
        (insert "  AI opponents: ")
        (insert-text-button
         (symbol-name card-games-ai-level)
         'face 'link
         'help-echo "Click to change the AI difficulty (easy/normal/hard)"
         'action #'card-games--cycle-ai)
        (insert (propertize "   (click to cycle easy/normal/hard)\n" 'face 'shadow))
        (insert "  Cards: ")
        (insert-text-button
         (symbol-name card-games-treatment)
         'face 'link
         'help-echo "Click to cycle the display: text / svg / full"
         'action #'card-games--cycle-treatment)
        (insert (propertize "   (click to cycle text/svg/full)\n\n" 'face 'shadow))
        (dolist (g card-games-list)
          (insert "    ")
          (insert-text-button
           (format "%-26s" (nth 0 g))
           'face 'link
           'help-echo (nth 2 g)
           'card-games-command (nth 1 g)
           'action #'card-games--launch)
          (insert (propertize (concat "  " (nth 2 g) "\n") 'face 'shadow)))
        (insert "\n")))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (forward-button 1)))

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
    (setq card-games-bid-felt-color (plist-get p :felt))
    (when (plist-member p :theme)
      (setq card-games-svg-theme-colors (plist-get p :theme)))
    (when (plist-get p :back) (setq card-games-svg-back-color (plist-get p :back)))
    (when (plist-get p :highlight)
      (setq card-games-svg-highlight-color (plist-get p :highlight)))
    (message "card-games theme: %s" name)))

(provide 'card-games)
;;; card-games.el ends here
