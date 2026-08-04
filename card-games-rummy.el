;;; card-games-rummy.el --- Rummy meld engine and Gin Rummy  -*- lexical-binding: t; -*-

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

;; The shared meld engine for the rummy family, plus Gin Rummy.
;;
;; A "meld" is a list of cards that is either a *set* (three or four cards
;; of the same rank) or a *run* (three or more cards of the same suit in
;; consecutive rank).  The engine validates melds, enumerates the melds
;; latent in a hand, and -- the hard part -- finds the layout that leaves
;; the least deadwood, which drives both knock detection and the AI.
;;
;; Cards use the package-standard cons (SUIT . RANK) with SUIT 0 spades,
;; 1 clubs, 2 diamonds, 3 hearts, and RANK 0 (Ace) .. 12 (King).  A joker,
;; used only by Hand & Foot, is the cons (joker . 0).
;;
;; Gin Rummy: a two-handed game of ten-card hands.  Draw from the stock or
;; take the discard, then discard one card.  Knock when your deadwood is
;; ten or less, or go gin with none; your opponent then lays off onto your
;; melds.  First to 100 points wins.  This file also provides the abstract
;; `card-games-rummy-game' base and the rendering helpers reused by the other
;; rummy games (card-games-rum500.el, card-games-handfoot.el).

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-rummy-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0 (Ace) .. 12 (King).")

(defface card-games-rummy-mark '((t :background "steelblue" :foreground "white"))
  "Face for a card the player has marked toward a meld."
  :group 'card-games)

(defun card-games-rummy-card-string (card)
  "Return a short label for CARD, or a dot for nil."
  (cond ((null card) "·")
        ((eq (car card) 'joker) (card-games-suit-glyph 'joker))
        (t (concat (aref card-games-rummy-ranks (cdr card)) (card-games-suit-glyph (car card))))))

(defun card-games-rummy-joker-p (card)
  "Return non-nil when CARD is a joker."
  (and card (eq (car card) 'joker)))

(defun card-games-rummy-value (card &optional ace-15)
  "Return the point value of CARD.
Faces and tens are 10, an Ace is 1 (or 15 when ACE-15 is non-nil), and
other cards their pip value.  A joker is worth 0 here; games that use
jokers value them separately."
  (cond ((or (null card) (card-games-rummy-joker-p card)) 0)
        (t (let ((r (cdr card)))
             (cond ((= r 0) (if ace-15 15 1))
                   ((<= r 8) (1+ r))
                   (t 10))))))


;;;; Meld predicates

(defun card-games-rummy--consec-p (ranks)
  "Return non-nil when sorted RANKS rise by exactly one with no repeats."
  (let ((ok t) (prev nil))
    (dolist (r ranks ok)
      (when (and prev (/= r (1+ prev))) (setq ok nil))
      (setq prev r))))

(cl-defun card-games-rummy-set-p (cards &key (min 3) distinct-suits)
  "Return non-nil when CARDS form a set (>= MIN cards of one rank).
With DISTINCT-SUITS, every suit must differ (a single-deck rule)."
  (and (>= (length cards) min)
       (not (cl-some #'card-games-rummy-joker-p cards))
       (let ((r (cdr (car cards))))
         (cl-every (lambda (c) (= (cdr c) r)) cards))
       (or (not distinct-suits)
           (let ((suits (mapcar #'car cards)))
             (= (length suits)
                (length (delete-dups (copy-sequence suits))))))))

(cl-defun card-games-rummy-run-p (cards &key (min 3) ace-high)
  "Return non-nil when CARDS form a run (>= MIN cards, one suit, in sequence).
With ACE-HIGH, an Ace may sit above the King (Q-K-A)."
  (and (>= (length cards) min)
       (not (cl-some #'card-games-rummy-joker-p cards))
       (let ((suit (car (car cards)))
             (ranks (sort (mapcar #'cdr cards) #'<)))
         (and (cl-every (lambda (c) (= (car c) suit)) cards)
              (= (length ranks) (length (delete-dups (copy-sequence ranks))))
              (or (card-games-rummy--consec-p ranks)
                  (and ace-high (eql (car ranks) 0)
                       (card-games-rummy--consec-p (sort (cons 13 (cdr ranks)) #'<))))))))

(cl-defun card-games-rummy-meld-p (cards &key (min 3) ace-high distinct-suits)
  "Return non-nil when CARDS form a valid set or run.
MIN, ACE-HIGH and DISTINCT-SUITS are passed through to the predicates."
  (or (card-games-rummy-set-p cards :min min :distinct-suits distinct-suits)
      (card-games-rummy-run-p cards :min min :ace-high ace-high)))


;;;; Candidate melds and best partition

(defun card-games-rummy--combinations (lst k)
  "Return all K-element combinations of LST, each as a list."
  (cond ((= k 0) (list nil))
        ((null lst) nil)
        (t (append
            (mapcar (lambda (c) (cons (car lst) c))
                    (card-games-rummy--combinations (cdr lst) (1- k)))
            (card-games-rummy--combinations (cdr lst) k)))))

(defun card-games-rummy--runs-in (pairs)
  "Return index-lists of runs (length >= 3) within PAIRS.
PAIRS is a list of (RANK . INDEX) sorted ascending by RANK."
  (let ((res '()) (vec (vconcat pairs)))
    (let ((n (length vec)))
      (dotimes (i n)
        (let ((idxs (list (cdr (aref vec i))))
              (last (car (aref vec i)))
              (j (1+ i)))
          (while (and (< j n) (= (car (aref vec j)) (1+ last)))
            (setq last (car (aref vec j)))
            (setq idxs (cons (cdr (aref vec j)) idxs))
            (when (>= (length idxs) 3)
              (push (reverse idxs) res))
            (setq j (1+ j))))))
    res))

(cl-defun card-games-rummy--candidate-melds (cards &key ace-high)
  "Return candidate melds for CARDS as a list of index-lists.
Each index-list refers to positions in CARDS.  With ACE-HIGH, high runs
ending in an Ace are also generated."
  (let* ((vec (vconcat cards)) (n (length vec)) (melds '()))
    ;; sets, grouped by rank
    (let ((byrank (make-hash-table :test 'eql)))
      (dotimes (i n)
        (let ((c (aref vec i)))
          (unless (card-games-rummy-joker-p c)
            (push i (gethash (cdr c) byrank)))))
      (maphash
       (lambda (_r idxs)
         (when (>= (length idxs) 3)
           (dolist (k '(3 4))
             (dolist (combo (card-games-rummy--combinations idxs k))
               (when (card-games-rummy-set-p (mapcar (lambda (i) (aref vec i)) combo)
                                     :distinct-suits t)
                 (push (sort (copy-sequence combo) #'<) melds))))))
       byrank))
    ;; runs, grouped by suit
    (let ((bysuit (make-hash-table :test 'eql)))
      (dotimes (i n)
        (let ((c (aref vec i)))
          (unless (card-games-rummy-joker-p c)
            (push (cons (cdr c) i) (gethash (car c) bysuit)))))
      (maphash
       (lambda (_s pairs)
         (let ((sorted (sort (copy-sequence pairs)
                             (lambda (a b) (< (car a) (car b))))))
           (dolist (m (card-games-rummy--runs-in sorted)) (push (sort m #'<) melds)))
         (when (and ace-high (assq 0 pairs))
           (let* ((hi (mapcar (lambda (p) (if (= (car p) 0) (cons 13 (cdr p)) p))
                              pairs))
                  (sh (sort hi (lambda (a b) (< (car a) (car b))))))
             (dolist (m (card-games-rummy--runs-in sh)) (push (sort m #'<) melds)))))
       bysuit))
    (delete-dups melds)))

(cl-defun card-games-rummy-best-partition (cards &key ace-high ace-15)
  "Return the lowest-deadwood layout of CARDS.
The result is a plist (:melds MELDS :deadwood CARDS :count N) where MELDS
is a list of card-lists, DEADWOOD the unmelded cards and N their value
sum.  ACE-HIGH allows Q-K-A runs; ACE-15 scores Aces at 15."
  (let* ((vec (vconcat cards)) (n (length vec))
         (cand (card-games-rummy--candidate-melds cards :ace-high ace-high))
         (mmasks (mapcar (lambda (m)
                           (let ((b 0)) (dolist (i m) (setq b (logior b (ash 1 i))))
                                b))
                         cand))
         (memo (make-hash-table :test 'eql))
         (full (1- (ash 1 n))))
    (cl-labels
        ((cval (i) (card-games-rummy-value (aref vec i) ace-15))
         (lowbit (avail)
           (let ((i 0)) (while (zerop (logand avail (ash 1 i))) (setq i (1+ i))) i))
         (solve (avail)
           (if (zerop avail) (cons 0 '())
             (or (gethash avail memo)
                 (let* ((i (lowbit avail))
                        (ibit (ash 1 i))
                        (sub0 (solve (logxor avail ibit)))
                        (best (cons (+ (cval i) (car sub0)) (cdr sub0))))
                   (dolist (mask mmasks)
                     (when (and (/= 0 (logand mask ibit))
                                (= mask (logand mask avail)))
                       (let ((sub (solve (logand avail (lognot mask)))))
                         (when (< (car sub) (car best))
                           (setq best (cons (car sub) (cons mask (cdr sub))))))))
                   (puthash avail best memo)
                   best)))))
      (let* ((res (solve full)) (maskmelds (cdr res)) (used 0))
        (dolist (m maskmelds) (setq used (logior used m)))
        (list :melds
              (mapcar (lambda (mask)
                        (let ((cl '()))
                          (dotimes (i n)
                            (when (/= 0 (logand mask (ash 1 i)))
                              (push (aref vec i) cl)))
                          (nreverse cl)))
                      maskmelds)
              :deadwood
              (let ((dl '()))
                (dotimes (i n)
                  (when (= 0 (logand used (ash 1 i))) (push (aref vec i) dl)))
                (nreverse dl))
              :count (car res))))))

(defun card-games-rummy-deadwood (cards &optional ace-high ace-15)
  "Return the minimum deadwood value of CARDS.
ACE-HIGH and ACE-15 are passed to `card-games-rummy-best-partition'."
  (plist-get (card-games-rummy-best-partition cards :ace-high ace-high :ace-15 ace-15)
             :count))

(defun card-games-rummy-layoff-p (card melds &optional ace-high)
  "Return the first meld in MELDS that CARD extends, or nil.
ACE-HIGH allows extending a run with a high Ace."
  (cl-find-if (lambda (m)
                (card-games-rummy-meld-p (cons card m) :min 3 :ace-high ace-high))
              melds))


;;;; Shared deck and base game

(defun card-games-rummy-deck (&optional ndecks jokers)
  "Return a shuffled deck of NDECKS standard packs plus JOKERS jokers each.
NDECKS defaults to 1 and JOKERS to 0."
  (random t)
  (let ((cards '()))
    (dotimes (_ (or ndecks 1))
      (dotimes (s 4) (dotimes (r 13) (push (cons s r) cards)))
      (dotimes (_ (or jokers 0)) (push (cons 'joker 0) cards)))
    (card-games-shuffle cards)))

(defclass card-games-rummy-game (card-games-game) ()
  "Abstract base for rummy-style draw-and-discard games."
  :abstract t)

(defsubst card-games-rummy--hand (game s) (aref (card-games-get game :hands) s))
(defsubst card-games-rummy--set-hand (game s v) (aset (card-games-get game :hands) s v))
(defsubst card-games-rummy--top (game) (car (card-games-get game :discard)))

(defun card-games-rummy-sort-hand (cards)
  "Return CARDS sorted by suit then rank for display, jokers last."
  (sort (copy-sequence cards)
        (lambda (a b)
          (let ((sa (if (card-games-rummy-joker-p a) 99 (car a)))
                (sb (if (card-games-rummy-joker-p b) 99 (car b))))
            (if (= sa sb) (< (cdr a) (cdr b)) (< sa sb))))))

(defcustom card-games-rummy-svg-cards t
  "When non-nil, draw cards as SVG images on a graphical display.
Set to nil to force the plain-text card row everywhere."
  :type 'boolean :group 'card-games)

(defun card-games-rummy--card-spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD."
  (if (card-games-rummy-joker-p card) (cons "" 'joker)
    (cons (aref card-games-rummy-ranks (cdr card)) (car card))))

(defun card-games-rummy--svg-row (cards cursor marks hint-fn &optional region-tag)
  "Return a one-image SVG row for CARDS (clickable + sliderful when REGION-TAG).
HINT-FN is an optional predicate marking playable cards."
  (let ((hints (when hint-fn
                 (let ((hs '()) (i 0))
                   (dolist (c cards) (when (funcall hint-fn c) (push i hs))
                           (setq i (1+ i)))
                   hs))))
    (card-games-svg-hand-image (mapcar #'card-games-rummy--card-spec cards)
                       :cursor (and (integerp cursor) (>= cursor 0) cursor)
                       :marks marks :hints hints
                       :overlap (if (> (length cards) 11)
                                    (max 0 (- card-games-svg-card-width 24)) 0)
                       :region-tag region-tag)))

(defun card-games-rummy--render-cards (cards cursor marks &optional hint-fn region-tag)
  "Return a propertized row of CARDS.
CURSOR is the highlighted index, MARKS a list of marked indices, and
HINT-FN an optional predicate marking playable cards.  When REGION-TAG is
non-nil the SVG row is clickable (each card mapped to (REGION-TAG . INDEX))
and carries a card-size slider.  Draws SVG cards on a graphical display
\(see `card-games-rummy-svg-cards'), else a plain-text row."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-rummy--svg-row cards cursor marks hint-fn region-tag)
  (let ((i 0) (out '()))
    (dolist (c cards)
      (let ((cs (card-games-rummy-card-string c)) (faces nil))
        (when (and (not (card-games-rummy-joker-p c)) (card-games-red-suit-p (car c)))
          (push 'card-games-red-suit faces))
        (when (and hint-fn (funcall hint-fn c)) (push 'card-games-hint faces))
        (when (memq i marks) (push 'card-games-rummy-mark faces))
        (when (eql i cursor) (push 'card-games-cursor faces))
        (push (propertize (format "%4s" cs) 'face (or faces 'default)) out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out)))))

(defun card-games-rummy--board-svg (&rest args)
  "Draw a rummy-style board; return a propertized display string.
Keyword ARGS: :title :infos :melds :discard :stock :hand :cursor :marks
:hint-fn :message.  :melds is a list of (LABEL . CARDS), or the symbol
`none' to hide the table area.  The hand carries (hand . INDEX) regions."
  (let* ((title (or (plist-get args :title) ""))
         (infos (plist-get args :infos))
         (raw-melds (plist-get args :melds))
         (show-table (not (eq raw-melds 'none)))
         (melds (and show-table raw-melds))
         (raw-discard (plist-get args :discard))
         (show-discard (not (eq raw-discard 'none)))
         (discard (and show-discard raw-discard))
         (stock (or (plist-get args :stock) 0))
         (stock-label (or (plist-get args :stock-label) "Stock"))
         (hand (plist-get args :hand))
         (cursor (or (plist-get args :cursor) 0))
         (marks (plist-get args :marks))
         (hint-fn (plist-get args :hint-fn))
         (msg (or (plist-get args :message) ""))
         (w card-games-svg-card-width) (h card-games-svg-card-height) (gap card-games-svg-card-gap)
         (pad 16) (label-w 46)
         (n (length hand))
         (overlap (cond ((> n 11) (- w 24)) ((> n 8) 20) (t 0)))
         (step (max 14 (- (+ w gap) overlap)))
         (fanw (if (> n 0) (+ (* (1- n) step) w) w))
         (mstep (max 16 (round (* w 0.5))))
         (n-info (length infos))
         (y-title 6) (y-info 26)
         (y-sd (+ y-info (* n-info 16) 8))
         (meld-rowh (+ h 8))
         (y-melds (+ y-sd h 26))
         (melds-area (if show-table (+ (* (max 1 (length melds)) meld-rowh) 16) 6))
         (y-hand (+ y-melds melds-area))
         (height (+ y-hand h 30))
         (meld-maxw (if melds
                        (apply #'max 0
                               (mapcar (lambda (m)
                                         (+ label-w
                                            (let ((k (length (cdr m))))
                                              (if (> k 0) (+ (* (1- k) mstep) w) w))))
                                       melds))
                      0))
         (width (max (+ fanw (* 2 pad)) (+ meld-maxw (* 2 pad)) 640))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels ((txt (str x y &optional sz bold)
                     (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                            :font-family card-games-svg-font-family
                            (and bold '(:font-weight "bold"))))
                (drow (specs x y stp)
                      (let ((xx x))
                        (dolist (sp specs)
                          (if sp (card-games-svg-card svg xx y :rank (car sp) :suit (cdr sp))
                            (card-games-svg-card svg xx y :gap t))
                          (setq xx (+ xx stp))))))
      (txt title pad (+ y-title 12) 13 t)
      (let ((yy (+ y-info 4)))
        (dolist (line infos) (txt line pad yy 12) (setq yy (+ yy 16))))
      (card-games-svg-card svg pad y-sd :down (> stock 0) :gap (= stock 0))
      (txt (format "%s %d" stock-label stock) pad (+ y-sd h 13) 11)
      (when show-discard
        (let ((dx (+ pad w gap 24)))
          (if discard
              (let ((sp (card-games-rummy--card-spec discard)))
                (card-games-svg-card svg dx y-sd :rank (car sp) :suit (cdr sp)))
            (card-games-svg-card svg dx y-sd :gap t))
          (txt "Discard" dx (+ y-sd h 13) 11)))
      (when show-table
        (txt "Table" pad (- y-melds 4) 11)
        (if (null melds)
            (txt "(no melds yet)" (+ pad label-w) (+ y-melds (round (* h 0.5))) 11)
          (let ((yy y-melds))
            (dolist (m melds)
              (txt (car m) pad (+ yy (round (* h 0.55))) 11)
              (drow (mapcar #'card-games-rummy--card-spec (cdr m)) (+ pad label-w) yy mstep)
              (setq yy (+ yy meld-rowh))))))
      (txt "Your hand" pad (- y-hand 6) 11)
      (let ((x (max pad (- (/ width 2) (/ fanw 2)))) (i 0))
        (dolist (c hand)
          (let ((sp (card-games-rummy--card-spec c))
                (curp (= i cursor))
                (markp (and marks (memq i marks)))
                (hintp (and hint-fn (funcall hint-fn c))))
            (card-games-svg-card svg x y-hand :rank (car sp) :suit (cdr sp)
                         :highlight curp :hint hintp)
            (when markp
              (svg-rectangle svg (- x 3) (- y-hand 3) (+ w 6) (+ h 6)
                             :rx 8 :fill "none" :stroke "#4a90d9" :stroke-width 3))
            (push (cons (list x y-hand (if (= i (1- n)) w step) h) (cons 'hand i)) regions))
          (setq x (+ x step) i (1+ i))))
      (txt msg pad (- height 8) 12))
    (propertize "*" 'display (card-games-svg-image svg (card-games-scale))
                'card-games-regions (nreverse regions))))

(defun card-games-rummy--melds-string (melds)
  "Return a one-line depiction of MELDS (a list of card-lists)."
  (mapconcat (lambda (m) (mapconcat #'card-games-rummy-card-string m " "))
             melds "  |  "))


;;;; Gin Rummy

(defcustom card-games-gin-target 100
  "Points needed to win a game of Gin Rummy."
  :type 'integer :group 'card-games)

(defclass card-games-gin-game (card-games-rummy-game)
  ((vname :initform "Gin Rummy"))
  "A two-handed game of Gin Rummy.")

(cl-defmethod card-games-gin--deal ((game card-games-gin-game))
  "Deal a fresh Gin hand into GAME."
  (let ((deck (card-games-rummy-deck)) (hands (make-vector 2 nil)))
    (dotimes (s 2)
      (aset hands s (card-games-rummy-sort-hand (cl-loop repeat 10 collect (pop deck)))))
    (card-games-put game :hands hands)
    (card-games-put game :discard (list (pop deck)))
    (card-games-put game :stock deck)
    (card-games-put game :turn 0)
    (card-games-put game :step 'draw)
    (card-games-put game :phase 'play)
    (card-games-put game :cursor 0)
    (unless (card-games-get game :scores) (card-games-put game :scores (make-vector 2 0)))
    (card-games-put game :message
            "Your turn: s draws from stock, t takes the discard.")
    game))

(defun card-games-gin--deadwood (cards) (card-games-rummy-deadwood cards))

(defun card-games-gin--draw (game s)
  "Move one stock card to seat S's hand; return it or nil if stock empty."
  (let ((stock (card-games-get game :stock)))
    (when stock
      (let ((c (pop stock)))
        (card-games-put game :stock stock)
        (card-games-rummy--set-hand game s (cons c (card-games-rummy--hand game s)))
        c))))

(defun card-games-gin--take (game s)
  "Move the discard top to seat S's hand and return it."
  (let ((c (card-games-rummy--top game)))
    (card-games-put game :discard (cdr (card-games-get game :discard)))
    (card-games-rummy--set-hand game s (cons c (card-games-rummy--hand game s)))
    c))

(defun card-games-gin--discard (game s card)
  "Discard CARD from seat S's hand onto the pile."
  (card-games-rummy--set-hand game s (card-games-rummy-sort-hand
                              (remove card (card-games-rummy--hand game s))))
  (card-games-put game :discard (cons card (card-games-get game :discard))))

(defun card-games-gin--ai-best-discard (game s)
  "Return the card seat S should discard to minimise its deadwood."
  (let ((hand (card-games-rummy--hand game s)) (best nil) (bestv most-positive-fixnum))
    (dolist (c hand best)
      (let ((d (card-games-gin--deadwood (remove c hand))))
        (when (or (< d bestv)
                  (and (= d bestv) best
                       (> (card-games-rummy-value c) (card-games-rummy-value best))))
          (setq best c bestv d))))))

(cl-defmethod card-games-gin--ai-turn ((game card-games-gin-game) s)
  "Play seat S's whole turn, then hand control back."
  (let* ((hand (card-games-rummy--hand game s))
         (cur (card-games-gin--deadwood hand))
         (up (card-games-rummy--top game))
         (with (and up (card-games-gin--deadwood (cons up hand))))
         ;; take the discard only when it strictly helps
         (drew (if (and up with (< with cur))
                   (card-games-gin--take game s)
                 (card-games-gin--draw game s))))
    (if (not drew)
        (card-games-gin--exhaust game)               ; stock exhausted: wash the hand
      (let ((card (card-games-gin--ai-best-discard game s)))
        (card-games-gin--discard game s card)
        (let ((dw (card-games-gin--deadwood (card-games-rummy--hand game s))))
          (if (<= dw 10)
              (card-games-gin--knock game s)
            (card-games-put game :turn 0)
            (card-games-put game :step 'draw)
            (card-games-put game :message
                    (format "Opponent discarded %s.  s draws, t takes."
                            (card-games-rummy-card-string card)))))))))

(cl-defmethod card-games-gin--knock ((game card-games-gin-game) knocker)
  "Resolve the hand when KNOCKER knocks; score and end the hand."
  (let* ((kpart (card-games-rummy-best-partition (card-games-rummy--hand game knocker)))
         (kmelds (plist-get kpart :melds))
         (kdw (plist-get kpart :count))
         (opp (- 1 knocker))
         (opart (card-games-rummy-best-partition (card-games-rummy--hand game opp)))
         (odead (plist-get opart :deadwood))
         (gin (= kdw 0)))
    ;; opponent lays off onto the knocker's melds (not allowed on gin)
    (unless gin
      (let (remaining)
        (dolist (c odead)
          (let ((m (card-games-rummy-layoff-p c kmelds)))
            (if m (setcdr (last m) (list c))   ; extend that meld in place
              (push c remaining))))
        (setq odead (nreverse remaining))))
    (let* ((odw (apply #'+ (mapcar #'card-games-rummy-value odead)))
           (scores (card-games-get game :scores))
           (winner knocker) (pts 0) (note ""))
      (cond
       (gin (setq pts (+ 25  odw) note "Gin!"))
       ((< kdw odw) (setq pts (- odw kdw) note "Knock."))
       (t ;; undercut: defender wins
        (setq winner opp pts (+ 25 (- kdw odw)) note "Undercut!")))
      (aset scores winner (+ (aref scores winner) pts))
      (card-games-put game :phase 'hand-over)
      (card-games-put game :reveal t)
      (card-games-put game :winner winner)
      (card-games-put game :message
              (format "%s %s scores %d.  %s  (n: next hand)"
                      note
                      (if (= winner 0) "You" "Opponent") pts
                      (if (>= (aref scores winner) card-games-gin-target)
                          (progn (card-games-put game :phase 'game-over)
                                 (format "%s wins the game!"
                                         (if (= winner 0) "You" "Opponent")))
                        (format "Score %d-%d."
                                (aref scores 0) (aref scores 1))))))))

(cl-defmethod card-games-gin--exhaust ((game card-games-gin-game))
  "End a hand washed out because the stock ran dry."
  (card-games-put game :phase 'hand-over)
  (card-games-put game :reveal t)
  (card-games-put game :message "Stock exhausted -- the hand is a wash.  n: next hand."))

;;;; Gin UI

(defvar-local card-games-gin--game nil "The Gin Rummy game in the current buffer.")

(defun card-games-gin--svg (game)
  "Return an SVG board for the Gin GAME."
  (let* ((scores (card-games-get game :scores)) (reveal (card-games-get game :reveal))
         (hand (card-games-rummy--hand game 0)) (infos '()))
    (push (format "Opponent: %d cards    score %d"
                  (length (card-games-rummy--hand game 1)) (aref scores 1)) infos)
    (when reveal
      (let ((pp (card-games-rummy-best-partition (card-games-rummy--hand game 1))))
        (push (format "  melds %s   deadwood %s (%d)"
                      (card-games-rummy--melds-string (plist-get pp :melds))
                      (mapconcat #'card-games-rummy-card-string (plist-get pp :deadwood) " ")
                      (plist-get pp :count))
              infos)))
    (push (format "You: deadwood %d   score %d"
                  (card-games-gin--deadwood hand) (aref scores 0)) infos)
    (card-games-rummy--board-svg
     :title (format "Gin Rummy   (first to %d)" card-games-gin-target)
     :infos (nreverse infos) :melds 'none
     :discard (card-games-rummy--top game) :stock (length (card-games-get game :stock))
     :hand hand :cursor (card-games-get game :cursor)
     :message (card-games-get game :message))))

(cl-defmethod card-games-render ((game card-games-gin-game))
  "Return a depiction of the Gin GAME: SVG board if graphical, else text."
  (if (and card-games-rummy-svg-cards (display-graphic-p))
      (card-games-gin--svg game)
    (card-games-gin--render-text game)))

(defun card-games-gin--render-text (game)
  "Return a plain-text depiction of the Gin GAME."
  (let* ((out '()) (scores (card-games-get game :scores))
         (reveal (card-games-get game :reveal))
         (hand (card-games-rummy--hand game 0)) (cursor (card-games-get game :cursor)))
    (push (format "  Gin Rummy        first to %d\n\n" card-games-gin-target) out)
    (push (format "  Opponent: %d cards    score %d\n"
                  (length (card-games-rummy--hand game 1)) (aref scores 1)) out)
    (when reveal
      (let ((p (card-games-rummy-best-partition (card-games-rummy--hand game 1))))
        (push (format "    melds: %s\n    deadwood: %s (%d)\n"
                      (card-games-rummy--melds-string (plist-get p :melds))
                      (mapconcat #'card-games-rummy-card-string (plist-get p :deadwood) " ")
                      (plist-get p :count))
              out)))
    (push (format "\n  Discard: %s     Stock: %d\n\n"
                  (let ((cs (card-games-rummy-card-string (card-games-rummy--top game))))
                    (if (and (card-games-rummy--top game) (card-games-red-suit-p (car (card-games-rummy--top game))))
                        (propertize cs 'face 'card-games-red-suit) cs))
                  (length (card-games-get game :stock)))
          out)
    (push (format "  Your hand (deadwood %d, score %d):\n   "
                  (card-games-gin--deadwood hand) (aref scores 0)) out)
    (push (card-games-rummy--render-cards hand cursor nil nil 'hand) out)
    (when reveal
      (let ((p (card-games-rummy-best-partition hand)))
        (push (format "\n    melds: %s\n    deadwood: %s"
                      (card-games-rummy--melds-string (plist-get p :melds))
                      (mapconcat #'card-games-rummy-card-string (plist-get p :deadwood) " "))
              out)))
    (push (format "\n\n  %s\n" (card-games-get game :message)) out)
    (apply #'concat (nreverse out))))

(cl-defmethod card-games-render-apply ((g card-games-gin-game) action)
  "Apply a click ACTION on the hand to GAME G."
  (pcase action
    (`(hand . ,i) (card-games-put g :cursor i))
    (_ (cl-call-next-method))))

(defun card-games-gin--redisplay ()
  "Redraw the Gin Rummy buffer."
  (let ((game card-games-gin--game) (inhibit-read-only t))
        (setq card-games-current-game game card-games-redisplay-function #'card-games-gin--redisplay)
    (setq-local mode-line-process
                (format " [%s]" (or (card-games-get game :step) (card-games-get game :phase))))
    (erase-buffer) (insert (card-games-render game)) (goto-char (point-min))))

(defun card-games-gin--cursor-card ()
  "Return the card under the cursor in your hand."
  (nth (card-games-get card-games-gin--game :cursor) (card-games-rummy--hand card-games-gin--game 0)))

(defun card-games-gin-left ()
  "Move the hand cursor left."
  (interactive)
  (let* ((g card-games-gin--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1- (card-games-get g :cursor)) n)))
    (card-games-gin--redisplay)))

(defun card-games-gin-right ()
  "Move the hand cursor right."
  (interactive)
  (let* ((g card-games-gin--game) (n (length (card-games-rummy--hand g 0))))
    (when (> n 0) (card-games-put g :cursor (mod (1+ (card-games-get g :cursor)) n)))
    (card-games-gin--redisplay)))

(defun card-games-gin--my-turn-p (g)
  "Return non-nil when it is your turn to act in G."
  (and (eq (card-games-get g :phase) 'play) (= (card-games-get g :turn) 0)))

(defun card-games-gin-draw-stock ()
  "Draw the top stock card into your hand."
  (interactive)
  (let ((g card-games-gin--game))
    (cond
     ((not (card-games-gin--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "Discard first (RET)."))
     ((card-games-gin--draw g 0)
      (card-games-put g :step 'discard)
      (card-games-put g :cursor 0)
      (card-games-put g :message "Choose a card and discard with RET, or knock with k."))
     (t (card-games-gin--exhaust g)))
    (card-games-gin--redisplay)))

(defun card-games-gin-take-discard ()
  "Take the discard top into your hand."
  (interactive)
  (let ((g card-games-gin--game))
    (cond
     ((not (card-games-gin--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'draw)) (card-games-put g :message "Discard first (RET)."))
     ((null (card-games-rummy--top g)) (card-games-put g :message "The discard pile is empty."))
     (t (let ((c (card-games-gin--take g 0)))
          (card-games-put g :step 'discard)
          (card-games-put g :cursor 0)
          (card-games-put g :message
                  (format "Took %s.  Discard with RET, or knock with k."
                          (card-games-rummy-card-string c))))))
    (card-games-gin--redisplay)))

(defun card-games-gin--after-discard (g)
  "Hand control to the opponent after you discard in G."
  (card-games-put g :turn 1)
  (card-games-put g :step 'draw)
  (card-games-gin--ai-turn g 1)
  (card-games-gin--redisplay))

(defun card-games-gin-discard ()
  "Discard the selected card and end your turn."
  (interactive)
  (let* ((g card-games-gin--game) (card (card-games-gin--cursor-card)))
    (cond
     ((not (card-games-gin--my-turn-p g)) (card-games-put g :message "Not your turn.")
      (card-games-gin--redisplay))
     ((not (eq (card-games-get g :step) 'discard))
      (card-games-put g :message "Draw first (s or t).") (card-games-gin--redisplay))
     ((null card) (card-games-put g :message "No card selected.") (card-games-gin--redisplay))
     (t (card-games-gin--discard g 0 card)
        (card-games-put g :message (format "You discarded %s." (card-games-rummy-card-string card)))
        (card-games-gin--after-discard g)))))

(defun card-games-gin-knock ()
  "Knock, discarding the selected card, if your deadwood allows."
  (interactive)
  (let* ((g card-games-gin--game) (card (card-games-gin--cursor-card)))
    (cond
     ((not (card-games-gin--my-turn-p g)) (card-games-put g :message "Not your turn."))
     ((not (eq (card-games-get g :step) 'discard))
      (card-games-put g :message "Draw first (s or t)."))
     ((null card) (card-games-put g :message "No card selected."))
     (t (let ((dw (card-games-gin--deadwood (remove card (card-games-rummy--hand g 0)))))
          (if (> dw 10)
              (card-games-put g :message
                      (format "Can't knock: that leaves %d deadwood (need <= 10)." dw))
            (card-games-gin--discard g 0 card)
            (card-games-gin--knock g 0)))))
    (card-games-gin--redisplay)))

(defun card-games-gin-new ()
  "Deal a fresh hand (or a new game when one is over)."
  (interactive)
  (let ((g card-games-gin--game))
    (when (eq (card-games-get g :phase) 'game-over) (card-games-put g :scores (make-vector 2 0)))
    (card-games-put g :reveal nil)
    (card-games-gin--deal g)
    (card-games-gin--redisplay)))

(defun card-games-gin-redraw () "Redraw the board." (interactive) (card-games-gin--redisplay))

(defun card-games-gin-help ()
  "Describe the Gin Rummy controls."
  (interactive)
  (message "Arrows: choose  s: draw stock  t: take discard  RET: discard  k: knock  n: new  g: redraw"))

(defvar card-games-gin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'card-games-card-click)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "<left>")  #'card-games-gin-left)
    (define-key map (kbd "<right>") #'card-games-gin-right)
    (define-key map "s" #'card-games-gin-draw-stock)
    (define-key map "t" #'card-games-gin-take-discard)
    (define-key map (kbd "RET") #'card-games-gin-discard)
    (define-key map "k" #'card-games-gin-knock)
    (define-key map "n" #'card-games-gin-new)
    (define-key map "g" #'card-games-gin-redraw)
    (define-key map "?" #'card-games-gin-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-gin-mode'.")

(define-derived-mode card-games-gin-mode special-mode "Gin"
  "Major mode for Gin Rummy."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-gin ()
  "Play Gin Rummy against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Gin Rummy*")))
    (with-current-buffer buf
      (card-games-gin-mode)
      (setq card-games-gin--game (card-games-gin-game))
      (card-games-gin--deal card-games-gin--game)
      (card-games-gin--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'card-games-gin-rummy #'card-games-gin)

(provide 'card-games-rummy)
;;; card-games-rummy.el ends here
