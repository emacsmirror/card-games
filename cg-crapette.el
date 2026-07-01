;;; cg-crapette.el --- Russian Bank (Crapette), two-player vs AI  -*- lexical-binding: t; -*-

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

;; Russian Bank -- also called Crapette -- is the competitive, two-player
;; ancestor of the single-player patience in `cg-solitaire'.  You (South)
;; play against one computer opponent (North).
;;
;; Two 52-card packs are used, one per player.  In the centre sit eight
;; FOUNDATIONS, built up by suit from the Ace, and eight HOUSES, built
;; down in alternating colours; both are common ground either player may
;; build on.  Each player also has a 13-card RESERVE (its top card face
;; up), a WASTE, and a face-down HAND.  You win by getting rid of every
;; card in your reserve, hand, and waste.
;;
;; On your turn you make as many legal moves as you like:
;;   - move the top of your reserve, your waste, or any house onto a
;;     foundation or a house;
;;   - move a whole SEQUENCE (a run built down in alternating colours)
;;     from one house to another -- but only when there are enough empty
;;     houses to have shifted it a card at a time; and
;;   - LOAD a card from your reserve or waste onto the opponent's reserve
;;     or waste when it is the same suit and one rank up or down.
;;
;; FOUNDATION PRIORITY and "STOP": a card that can go to a foundation must
;; be played there before anything else.  If you build a house, load your
;; opponent, turn a card, or end your turn while a foundation play is
;; waiting, your opponent calls "STOP!" and your turn ends at once.  The
;; piles that owe a foundation play are ringed in the hint colour.  Set
;; `cg-crapette-stops' to nil for a gentler assist mode that blocks the
;; slip with a reminder instead of ending your turn.
;;
;; When you can do no more, turn the top of your hand: if it fits
;; somewhere you keep going, otherwise it goes to your waste and your turn
;; ends.
;;
;; SIMPLIFICATION (documented): the AI is a straightforward greedy player
;; and always observes foundation priority, so in practice only you can be
;; "stopped".

;;; Code:

(require 'cg-core)

(defconst cg-crap-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0 (Ace) .. 12 (King).")

(defcustom cg-crapette-stops t
  "How Russian Bank enforces foundation priority on your turn.
When non-nil (the competitive rule), the opponent calls \"stop\" and you
forfeit the rest of your turn if you make any play other than an
available foundation move.  When nil, such a slip is simply blocked with
a reminder and costs you nothing -- a gentler assist mode for learning."
  :type 'boolean :group 'card-games)

(defclass cg-crapette-game (cg-game)
  ((vname :initform "Russian Bank"))
  "Two-player Russian Bank (Crapette): you (South) versus one AI opponent.")

(defvar-local cg-crap--game nil
  "The `cg-crapette-game' played in the current buffer.")

(defvar cg-crap--recording t
  "When nil, `cg-crap--snapshot' does not record (used during the AI turn).")


;;;; Cards

(defun cg-crap--card-string (card)
  "Return a short label for CARD, or a dot for an empty pile."
  (if (null card) "·"
    (concat (aref cg-crap-ranks (cdr card)) (cg-suit-glyph (car card)))))

(defun cg-crap--red-p (card)
  "Return non-nil when CARD is a red suit."
  (and card (cg-red-suit-p (car card))))

(defun cg-crap--deck ()
  "Return one freshly shuffled 52-card deck."
  (let (cards)
    (dotimes (s 4) (dotimes (r 13) (push (cons s r) cards)))
    (cg-shuffle cards)))

(defun cg-crap--top (pile)
  "Return the top (last) card of PILE, or nil."
  (car (last pile)))


;;;; Pile access

(defun cg-crap--found (g i) "Foundation I of G." (aref (cg-get g :found) i))
(defun cg-crap--house (g i) "House I of G." (aref (cg-get g :houses) i))
(defun cg-crap--reserve (g p) "Player P's reserve list in G." (aref (cg-get g :reserve) p))
(defun cg-crap--waste (g p) "Player P's waste list in G." (aref (cg-get g :waste) p))
(defun cg-crap--hand (g p) "Player P's hand list in G." (aref (cg-get g :hand) p))


;;;; Dealing

(defun cg-crap--deal (game)
  "Deal a fresh Russian Bank layout into GAME and return it."
  (let ((d0 (cg-crap--deck)) (d1 (cg-crap--deck))
        (found (make-vector 8 nil)) (houses (make-vector 8 nil))
        (reserve (make-vector 2 nil)) (waste (make-vector 2 nil))
        (hand (make-vector 2 nil)))
    (dotimes (i 4) (aset houses i (list (pop d0))))
    (dotimes (i 4) (aset houses (+ 4 i) (list (pop d1))))
    (aset reserve 0 (cl-loop repeat 13 collect (pop d0)))
    (aset reserve 1 (cl-loop repeat 13 collect (pop d1)))
    (aset hand 0 d0)
    (aset hand 1 d1)
    (cg-put game :found found)
    (cg-put game :houses houses)
    (cg-put game :reserve reserve)
    (cg-put game :waste waste)
    (cg-put game :hand hand)
    (cg-put game :turn 0)
    (cg-put game :cursor 0)
    (cg-put game :sel nil)
    (cg-put game :moves 0)
    (cg-put game :history nil)
    (cg-put game :winner nil)
    (cg-put game :message
            "Your turn.  Foundations first -- skip one and North calls STOP!  Build the houses or load North.")
    game))


;;;; Legality

(defun cg-crap--found-accepts (game i card)
  "Return non-nil when CARD may go onto foundation I of GAME."
  (and card
       (let ((f (cg-crap--found game i)))
         (if (null f)
             (= (cdr card) 0)                       ; empty foundation takes an Ace
           (let ((top (cg-crap--top f)))
             (and (= (car card) (car top))
                  (= (cdr card) (1+ (cdr top)))))))))

(defun cg-crap--house-accepts (game i card)
  "Return non-nil when CARD may go onto house I of GAME."
  (and card
       (let ((h (cg-crap--house game i)))
         (if (null h)
             t                                       ; empty house takes anything
           (cg-crap--builds-down-p (cg-crap--top h) card)))))

(defun cg-crap--builds-down-p (upper lower)
  "Return non-nil when LOWER may sit on UPPER: one rank down, opposite colour."
  (and upper lower
       (= (cdr lower) (1- (cdr upper)))
       (not (eq (cg-crap--red-p upper) (cg-crap--red-p lower)))))

(defun cg-crap--load-accepts (pile-top card)
  "Return non-nil when CARD may be loaded onto a pile whose top is PILE-TOP.
Loading needs the same suit and a rank one step up or down."
  (and pile-top card
       (= (car pile-top) (car card))
       (= 1 (abs (- (cdr pile-top) (cdr card))))))

(defun cg-crap--found-for (game card)
  "Return the index of a foundation that would accept CARD, or nil."
  (cl-loop for i below 8 when (cg-crap--found-accepts game i card) return i))


;;;; Sequenced house moves

(defun cg-crap--house-run (game i)
  "Return the movable top run of house I as a list, bottom-to-top.
The run is the longest sequence of cards at the top of the house that is
built down in alternating colours."
  (let ((rev (reverse (cg-crap--house game i))))     ; top-first
    (if (null rev) nil
      (let ((run (list (car rev))) (prev (car rev)))
        (catch 'done
          (dolist (c (cdr rev))                       ; each C sits below PREV
            (if (cg-crap--builds-down-p c prev)
                (progn (push c run) (setq prev c))     ; PREV builds down on C
              (throw 'done nil))))
        run))))

(defun cg-crap--free-houses (game)
  "Return the number of empty houses in GAME."
  (cl-count-if #'null (append (cg-get game :houses) nil)))

(defun cg-crap--capacity (game dest-i)
  "Return how many cards may be moved as a group onto house DEST-I.
With F empty houses you may relay F+1 cards onto a non-empty house, or
F onto an empty house (the destination itself cannot serve as a relay)."
  (let ((free (cg-crap--free-houses game)))
    (if (cg-crap--house game dest-i) (1+ free) free)))

(defun cg-crap--house-move (game i j)
  "Move a legal sequence from house I onto house J in GAME.
Return non-nil on success, the symbol `space' when the sequence is legal
but there are too few empty houses, and nil when nothing fits."
  (if (= i j)
      nil
    (let* ((run (cg-crap--house-run game i))
           (rlen (length run))
           (dst (cg-crap--house game j))
           (dsttop (cg-crap--top dst))
           (cap (cg-crap--capacity game j))
           (n nil))
      (when (> rlen 0)
        (if (null dsttop)
            (setq n (min rlen (max cap 0)))           ; empty house: as many as fit
          (cl-loop for k from 0 below rlen            ; RUN is bottom-to-top
                   for card = (nth k run)
                   when (cg-crap--builds-down-p dsttop card)
                   do (setq n (- rlen k)) (cl-return)))
        (cond
         ((or (null n) (< n 1)) nil)
         ((> n cap) 'space)
         (t
          (cg-crap--snapshot game)
          (let* ((pile (cg-crap--house game i))
                 (keep (butlast pile n))
                 (moved (last pile n)))
            (aset (cg-get game :houses) i keep)
            (aset (cg-get game :houses) j (append dst moved)))
          (cg-put game :moves (1+ (cg-get game :moves)))
          t))))))


;;;; Spots (the cursor visits piles)

(defun cg-crap--spots (_game)
  "Return the ordered list of (TYPE . INDEX) spots the cursor visits."
  (append (list '(res . 0) '(was . 0))
          (cl-loop for i below 8 collect (cons 'house i))
          (cl-loop for i below 8 collect (cons 'found i))
          (list '(res . 1) '(was . 1))))

(defun cg-crap--sources (_game p)
  "Return the spots whose top card player P may pick up."
  (append (list (cons 'res p) (cons 'was p))
          (cl-loop for i below 8 collect (cons 'house i))))

(defun cg-crap--spot-top (game spot)
  "Return the top card available at SPOT of GAME, or nil."
  (pcase (car spot)
    ('res   (cg-crap--top (cg-crap--reserve game (cdr spot))))
    ('was   (cg-crap--top (cg-crap--waste game (cdr spot))))
    ('house (cg-crap--top (cg-crap--house game (cdr spot))))
    ('found (cg-crap--top (cg-crap--found game (cdr spot))))))

(defun cg-crap--source-p (spot p)
  "Return non-nil when SPOT is a pile player P may take a card from."
  (pcase (car spot)
    ('res (= (cdr spot) p))
    ('was (= (cdr spot) p))
    ('house t)
    (_ nil)))

(defun cg-crap--dest-ok (game from to card p)
  "Return non-nil when player P may move CARD from spot FROM to spot TO."
  (pcase (car to)
    ('found (cg-crap--found-accepts game (cdr to) card))
    ('house (cg-crap--house-accepts game (cdr to) card))
    ((or 'res 'was)
     (and (= (cdr to) (- 1 p))                       ; only onto the opponent
          (memq (car from) '(res was))               ; only your reserve/waste cards load
          (cg-crap--load-accepts (cg-crap--spot-top game to) card)))))


;;;; Moving cards

(defun cg-crap--snapshot (game)
  "Record GAME's mutable state for undo, unless recording is disabled."
  (when cg-crap--recording
    (cg-put game :history
            (cons (list (cg-crap--copy-vec (cg-get game :found))
                        (cg-crap--copy-vec (cg-get game :houses))
                        (cg-crap--copy-vec (cg-get game :reserve))
                        (cg-crap--copy-vec (cg-get game :waste))
                        (cg-crap--copy-vec (cg-get game :hand))
                        (cg-get game :turn))
                  (cg-get game :history)))))

(defun cg-crap--copy-vec (v)
  "Return a copy of vector V with each element list copied."
  (vconcat (mapcar #'copy-sequence v)))

(defun cg-crap--take (game spot)
  "Remove and return the top card of SPOT's pile in GAME."
  (pcase (car spot)
    ('res (let* ((p (cdr spot)) (pile (cg-crap--reserve game p)))
            (aset (cg-get game :reserve) p (butlast pile 1)) (cg-crap--top pile)))
    ('was (let* ((p (cdr spot)) (pile (cg-crap--waste game p)))
            (aset (cg-get game :waste) p (butlast pile 1)) (cg-crap--top pile)))
    ('house (let* ((i (cdr spot)) (pile (cg-crap--house game i)))
              (aset (cg-get game :houses) i (butlast pile 1)) (cg-crap--top pile)))))

(defun cg-crap--place (game spot card)
  "Add CARD to the top of SPOT's pile in GAME."
  (pcase (car spot)
    ('found (let ((i (cdr spot)))
              (aset (cg-get game :found) i (append (cg-crap--found game i) (list card)))))
    ('house (let ((i (cdr spot)))
              (aset (cg-get game :houses) i (append (cg-crap--house game i) (list card)))))
    ('res (let ((p (cdr spot)))
            (aset (cg-get game :reserve) p (append (cg-crap--reserve game p) (list card)))))
    ('was (let ((p (cdr spot)))
            (aset (cg-get game :waste) p (append (cg-crap--waste game p) (list card)))))))

(defun cg-crap--move (game from to p)
  "Move the top card of FROM onto TO for player P.
Return non-nil when the move was legal and performed."
  (let ((card (cg-crap--spot-top game from)))
    (when (and card (cg-crap--source-p from p) (cg-crap--dest-ok game from to card p))
      (cg-crap--snapshot game)
      (cg-crap--take game from)
      (cg-crap--place game to card)
      (cg-put game :moves (1+ (cg-get game :moves)))
      t)))

(defun cg-crap--do-move (game from to p)
  "Perform player P's move from FROM to TO in GAME.
A house-to-house move may carry a legal sequence of cards (see
`cg-crap--house-move'); every other move carries a single card.  Returns
the same values as those functions (non-nil on success, `space' when a
sequence is too long for the empty houses)."
  (if (and (eq (car from) 'house) (eq (car to) 'house))
      (cg-crap--house-move game (cdr from) (cdr to))
    (cg-crap--move game from to p)))


;;;; Turn logic

(defun cg-crap--forced (game p)
  "Return the source spots of GAME whose top card must go to a foundation."
  (let (out)
    (dolist (spot (cg-crap--sources game p))
      (let ((card (cg-crap--spot-top game spot)))
        (when (and card (cg-crap--found-for game card)) (push spot out))))
    (nreverse out)))

(defun cg-crap--card-playable (game p card)
  "Return non-nil when CARD has any legal destination for player P."
  (or (cg-crap--found-for game card)
      (cl-loop for i below 8 thereis (cg-crap--house-accepts game i card))
      (cg-crap--load-accepts (cg-crap--top (cg-crap--reserve game (- 1 p))) card)
      (cg-crap--load-accepts (cg-crap--top (cg-crap--waste game (- 1 p))) card)))

(defun cg-crap--won-p (game p)
  "Return non-nil when player P has emptied reserve, hand, and waste."
  (and (null (cg-crap--reserve game p))
       (null (cg-crap--hand game p))
       (null (cg-crap--waste game p))))

(cl-defmethod cg-won-p ((game cg-crapette-game))
  "Return non-nil when you (South) have won GAME."
  (eq (cg-get game :winner) 0))

(defun cg-crap--msg (game s)
  "Set GAME's status message to S."
  (cg-put game :message s))

(defun cg-crap--after-human-move (game)
  "Note a win after a human move in GAME."
  (when (cg-crap--won-p game 0)
    (cg-put game :winner 0)
    (cg-crap--msg game "You emptied your cards -- you win!")))

(defun cg-crap--end-turn (game)
  "Pass the turn from you to the AI, run it, and return control to you."
  (unless (cg-get game :winner)
    (cg-put game :sel nil)
    (cg-put game :turn 1)
    (let ((cg-crap--recording nil))
      (cg-crap--ai-play game))
    (unless (cg-get game :winner)
      (cg-put game :turn 0)
      (cg-put game :history nil))))

(defun cg-crap--stop (game)
  "Enforce foundation priority: call \"stop\" or block, per `cg-crapette-stops'.
Return non-nil when the offending action must be abandoned by its caller."
  (when (cg-crap--forced game 0)
    (if cg-crapette-stops
        (let ((note "North calls STOP -- you must play to a foundation first!"))
          (message "%s" note)
          (cg-put game :sel nil)
          (cg-crap--end-turn game)
          (cg-put game :message (concat note "  " (cg-get game :message))))
      (cg-crap--msg game "Play to a foundation first (press f on a highlighted pile)."))
    t))


;;;; The AI opponent (player 1)

(defun cg-crap--ai-found-move (game)
  "Return an AI (SOURCE . FOUNDATION) foundation move, or nil."
  (cl-loop for spot in (cg-crap--sources game 1)
           for card = (cg-crap--spot-top game spot)
           for fi = (and card (cg-crap--found-for game card))
           when fi return (cons spot (cons 'found fi))))

(defun cg-crap--ai-useful-move (game)
  "Return an AI (SOURCE . DEST) move that empties its reserve or waste, or nil.
The AI only moves its own reserve/waste tops -- onto your piles (loading)
or onto a house -- so every such move reduces the AI's cards and its turn
is guaranteed to end."
  (catch 'm
    (dolist (spot (list (cons 'res 1) (cons 'was 1)))
      (let ((card (cg-crap--spot-top game spot)))
        (when card
          (dolist (dst (list (cons 'res 0) (cons 'was 0)))
            (when (cg-crap--load-accepts (cg-crap--spot-top game dst) card)
              (throw 'm (cons spot dst))))
          (cl-loop for i below 8
                   when (cg-crap--house-accepts game i card)
                   do (throw 'm (cons spot (cons 'house i)))))))
    nil))

(defun cg-crap--ai-play (game)
  "Play the AI opponent's whole turn on GAME."
  (let ((guard 0))
    (catch 'done
      (while t
        (when (> (setq guard (1+ guard)) 400) (throw 'done nil))
        (when (cg-crap--won-p game 1) (throw 'done nil))
        (let ((mv (or (cg-crap--ai-found-move game)
                      (cg-crap--ai-useful-move game))))
          (cond
           (mv (cg-crap--move game (car mv) (cdr mv) 1))
           ((cg-crap--hand game 1)
            (let* ((hand (cg-crap--hand game 1)) (card (cg-crap--top hand)))
              (aset (cg-get game :hand) 1 (butlast hand 1))
              (aset (cg-get game :waste) 1 (append (cg-crap--waste game 1) (list card)))
              (unless (cg-crap--card-playable game 1 card) (throw 'done 'flip-end))))
           (t (throw 'done 'stuck))))))
    (if (cg-crap--won-p game 1)
        (progn (cg-put game :winner 1)
               (cg-crap--msg game "Your opponent emptied their cards -- you lose."))
      (cg-crap--msg game "Your opponent finished their turn.  Your move."))))


;;;; Rendering (text)

(defun cg-crap--render (g)
  "Return a propertized depiction of Russian Bank game G."
  (let* ((spots (cg-crap--spots g))
         (cur (nth (cg-get g :cursor) spots))
         (sel (cg-get g :sel))
         (forced (cg-crap--forced g 0))
         (turn (cg-get g :turn))
         (win (cg-get g :winner)))
    (cl-flet ((cell (spot label)
                (let* ((card (cg-crap--spot-top g spot))
                       (txt (concat label (cg-crap--card-string card)))
                       (face (cond ((equal spot cur) 'cg-cursor)
                                   ((equal spot sel) 'cg-hint)
                                   ((member spot forced) 'cg-hint)
                                   ((cg-crap--red-p card) 'cg-red-suit)
                                   (t 'default))))
                  (propertize (format "%-8s" txt) 'face face))))
      (concat
       (format "  Russian Bank -- %s\n\n"
               (cond ((eq win 0) "you win!")
                     ((eq win 1) "you lose")
                     ((= turn 0) "your turn")
                     (t "opponent's turn")))
       (format "  Opponent (North)   hand:%2d   reserve:%2d   waste:%2d\n    "
               (length (cg-crap--hand g 1)) (length (cg-crap--reserve g 1))
               (length (cg-crap--waste g 1)))
       (cell '(res . 1) "R:") (cell '(was . 1) "W:") "\n\n"
       "  Foundations (build up by suit)\n    "
       (cell '(found . 0) "") (cell '(found . 1) "")
       (cell '(found . 2) "") (cell '(found . 3) "") "\n    "
       (cell '(found . 4) "") (cell '(found . 5) "")
       (cell '(found . 6) "") (cell '(found . 7) "") "\n\n"
       "  Houses (build down, alternating colours)\n    "
       (cell '(house . 0) "") (cell '(house . 1) "")
       (cell '(house . 2) "") (cell '(house . 3) "") "\n    "
       (cell '(house . 4) "") (cell '(house . 5) "")
       (cell '(house . 6) "") (cell '(house . 7) "") "\n\n"
       (format "  You (South)        hand:%2d   reserve:%2d   waste:%2d\n    "
               (length (cg-crap--hand g 0)) (length (cg-crap--reserve g 0))
               (length (cg-crap--waste g 0)))
       (cell '(res . 0) "R:") (cell '(was . 0) "W:") "\n\n"
       (format "  %s\n" (cg-get g :message))))))

(cl-defmethod cg-render ((game cg-crapette-game))
  "Return a text depiction of GAME."
  (cg-crap--render game))

(defun cg-crap--redisplay ()
  "Redraw the current Russian Bank buffer."
  (let ((g cg-crap--game) (inhibit-read-only t))
    (setq-local mode-line-process
                (format " [%s]"
                        (cond ((eq (cg-get g :winner) 0) "you win")
                              ((eq (cg-get g :winner) 1) "you lose")
                              ((= (cg-get g :turn) 0) "your turn")
                              (t "opponent"))))
    (erase-buffer)
    (insert (cg-crap--render g))
    (cg-insert-legend
     "arrows move · RET pick up/drop (runs move house→house) · f foundation · SPC turn a card · e end · u undo · n new · q menu")
    (goto-char (point-min))))


;;;; Commands

(defun cg-crap--move-cursor (g d)
  "Move G's cursor by D spots and redisplay."
  (let ((n (length (cg-crap--spots g))))
    (cg-put g :cursor (mod (+ (cg-get g :cursor) d) n)))
  (cg-crap--redisplay))

(defun cg-crap-left ()  "Move the cursor to the previous pile."
  (interactive) (cg-crap--move-cursor cg-crap--game -1))
(defun cg-crap-right () "Move the cursor to the next pile."
  (interactive) (cg-crap--move-cursor cg-crap--game 1))

(defun cg-crap--your-turn-p (g)
  "Return non-nil when it is your move (and warn otherwise)."
  (cond ((cg-get g :winner)
         (cg-crap--msg g "The game is over -- press n for a new game.") nil)
        ((/= (cg-get g :turn) 0)
         (cg-crap--msg g "Wait for your turn.") nil)
        (t t)))

(defun cg-crap-act ()
  "Pick up the card under the cursor, or drop the picked-up card there."
  (interactive)
  (let* ((g cg-crap--game) (spots (cg-crap--spots g))
         (spot (nth (cg-get g :cursor) spots)) (sel (cg-get g :sel)))
    (when (cg-crap--your-turn-p g)
      (if sel
          (unless (and (not (eq (car spot) 'found)) (cg-crap--stop g))
            (let ((res (cg-crap--do-move g sel spot 0)))
              (cond
               ((eq res 'space)
                (cg-crap--msg g "Not enough empty houses to move that whole sequence."))
               (res (cg-put g :sel nil) (cg-crap--after-human-move g))
               (t (cg-crap--msg g "That card can't go there.")))))
        (if (and (cg-crap--source-p spot 0) (cg-crap--spot-top g spot))
            (let* ((top (cg-crap--spot-top g spot))
                   (run (and (eq (car spot) 'house) (cg-crap--house-run g (cdr spot))))
                   (n (length run)))
              (cg-put g :sel spot)
              (cg-crap--msg g
                            (if (> n 1)
                                (format "Picked up a run of %d (%s on top) -- drop it on a house."
                                        n (cg-crap--card-string top))
                              (format "Picked up %s -- choose where to drop it."
                                      (cg-crap--card-string top)))))
          (cg-crap--msg g "Nothing of yours to pick up there."))))
    (cg-crap--redisplay)))

(defun cg-crap-found ()
  "Send the card under the cursor (or the picked-up card) to a foundation."
  (interactive)
  (let* ((g cg-crap--game) (spots (cg-crap--spots g))
         (spot (nth (cg-get g :cursor) spots)))
    (when (cg-crap--your-turn-p g)
      (let* ((src (or (cg-get g :sel) spot))
             (card (and (cg-crap--source-p src 0) (cg-crap--spot-top g src)))
             (fi (and card (cg-crap--found-for g card))))
        (if (and fi (cg-crap--move g src (cons 'found fi) 0))
            (progn (cg-put g :sel nil) (cg-crap--after-human-move g))
          (cg-crap--msg g "No foundation accepts that card."))))
    (cg-crap--redisplay)))

(defun cg-crap-draw ()
  "Turn the top card of your hand onto your waste.
If it fits nowhere your turn ends."
  (interactive)
  (let ((g cg-crap--game))
    (when (cg-crap--your-turn-p g)
      (unless (cg-crap--stop g)
        (if (null (cg-crap--hand g 0))
            (cg-crap--msg g "Your hand is empty -- press e to end your turn.")
          (cg-crap--snapshot g)
          (let* ((hand (cg-crap--hand g 0)) (card (cg-crap--top hand)))
            (aset (cg-get g :hand) 0 (butlast hand 1))
            (aset (cg-get g :waste) 0 (append (cg-crap--waste g 0) (list card)))
            (cg-put g :sel nil)
            (if (cg-crap--card-playable g 0 card)
                (cg-crap--msg g (format "Turned %s onto your waste -- play on."
                                        (cg-crap--card-string card)))
              (progn
                (cg-crap--msg g (format "Turned %s -- nothing to do, your turn ends."
                                        (cg-crap--card-string card)))
                (cg-crap--end-turn g)))))))
    (cg-crap--redisplay)))

(defun cg-crap-end ()
  "End your turn and let the opponent play."
  (interactive)
  (let ((g cg-crap--game))
    (when (cg-crap--your-turn-p g)
      (unless (cg-crap--stop g)
        (cg-crap--msg g "You end your turn.")
        (cg-crap--end-turn g)))
    (cg-crap--redisplay)))

(defun cg-crap-undo ()
  "Undo your last move this turn."
  (interactive)
  (let* ((g cg-crap--game) (h (cg-get g :history)))
    (if (null h)
        (cg-crap--msg g "Nothing to undo.")
      (let ((s (car h)))
        (cg-put g :found (nth 0 s)) (cg-put g :houses (nth 1 s))
        (cg-put g :reserve (nth 2 s)) (cg-put g :waste (nth 3 s))
        (cg-put g :hand (nth 4 s)) (cg-put g :turn (nth 5 s))
        (cg-put g :history (cdr h)) (cg-put g :sel nil) (cg-put g :winner nil)
        (cg-crap--msg g "Undid a move.")))
    (cg-crap--redisplay)))

(defun cg-crap-new ()
  "Deal a fresh Russian Bank game."
  (interactive)
  (cg-crap--deal cg-crap--game)
  (cg-crap--redisplay))

(defun cg-crap-redraw () "Redraw the board." (interactive) (cg-crap--redisplay))

(defun cg-crap-help ()
  "Describe the controls."
  (interactive)
  (message
   "Arrows: move  RET: pick up/drop (runs move house->house)  f: to foundation  SPC: turn a card  e: end turn  u: undo  n: new  q: menu"))

(defvar cg-crapette-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-crap-left)
    (define-key map (kbd "<right>") #'cg-crap-right)
    (define-key map (kbd "<up>")    #'cg-crap-left)
    (define-key map (kbd "<down>")  #'cg-crap-right)
    (define-key map (kbd "RET") #'cg-crap-act)
    (define-key map (kbd "SPC") #'cg-crap-draw)
    (define-key map "f" #'cg-crap-found)
    (define-key map "d" #'cg-crap-draw)
    (define-key map "e" #'cg-crap-end)
    (define-key map "u" #'cg-crap-undo)
    (define-key map "n" #'cg-crap-new)
    (define-key map "g" #'cg-crap-redraw)
    (define-key map "?" #'cg-crap-help)
    (define-key map "q" #'cg-quit-to-menu)
    map)
  "Keymap for `cg-crapette-mode'.")

(define-derived-mode cg-crapette-mode special-mode "Crapette"
  "Major mode for two-player Russian Bank (Crapette)."
  (setq-local truncate-lines t)
  (setq-local cursor-type cg-cursor-type))

;;;###autoload
(defun cg-crapette ()
  "Play two-player Russian Bank (Crapette) against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Russian Bank (Crapette)*")))
    (with-current-buffer buf
      (cg-crapette-mode)
      (setq cg-crap--game (cg-crap--deal (cg-crapette-game)))
      (cg-crap--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'cg-russian-bank-duel #'cg-crapette
  "Alias for `cg-crapette'.")

(provide 'cg-crapette)
;;; cg-crapette.el ends here
