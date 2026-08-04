;;; card-games-crapette.el --- Russian Bank (Crapette), two-player vs AI  -*- lexical-binding: t; -*-

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

;; Russian Bank -- also called Crapette -- is the competitive, two-player
;; ancestor of the single-player patience in `card-games-solitaire'.  You (South)
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
;; `card-games-crapette-stops' to nil for a gentler assist mode that blocks the
;; slip with a reminder instead of ending your turn.
;;
;; When you can do no more, turn the top of your hand: if it fits
;; somewhere you keep going, otherwise it goes to your waste and your turn
;; ends.
;;
;; The AI observes foundation priority, empties its reserve first (the
;; bottleneck), prefers loading its cards onto you, and looks one move
;; ahead to rearrange the houses when that frees a stuck reserve or waste
;; card.  It never breaks foundation priority, so in practice only you can
;; be "stopped".

;;; Code:

(require 'card-games-core)
(require 'card-games-svg)

(defconst card-games-crap-ranks
  ["A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0 (Ace) .. 12 (King).")

(defcustom card-games-crapette-stops t
  "How Russian Bank enforces foundation priority on your turn.
When non-nil (the competitive rule), the opponent calls \"stop\" and you
forfeit the rest of your turn if you make any play other than an
available foundation move.  When nil, such a slip is simply blocked with
a reminder and costs you nothing -- a gentler assist mode for learning."
  :type 'boolean :group 'card-games)

(defcustom card-games-crapette-svg-cards t
  "When non-nil, draw the board with SVG cards on a graphical display.
Set to nil to force the plain-text board everywhere."
  :type 'boolean :group 'card-games)

(defclass card-games-crapette-game (card-games-game)
  ((vname :initform "Russian Bank"))
  "Two-player Russian Bank (Crapette): you (South) versus one AI opponent.")

(defvar-local card-games-crap--game nil
  "The `card-games-crapette-game' played in the current buffer.")

(defvar card-games-crap--recording t
  "When nil, `card-games-crap--snapshot' does not record (used during the AI turn).")


;;;; Cards

(defun card-games-crap--card-string (card)
  "Return a short label for CARD, or a dot for an empty pile."
  (if (null card) "·"
    (concat (aref card-games-crap-ranks (cdr card)) (card-games-suit-glyph (car card)))))

(defun card-games-crap--red-p (card)
  "Return non-nil when CARD is a red suit."
  (and card (card-games-red-suit-p (car card))))

(defun card-games-crap--spec (card)
  "Return the card-games-svg display spec (RANK-STRING . SUIT) for CARD, or nil."
  (and card (cons (aref card-games-crap-ranks (cdr card)) (car card))))

(defun card-games-crap--deck ()
  "Return one freshly shuffled 52-card deck."
  (let (cards)
    (dotimes (s 4) (dotimes (r 13) (push (cons s r) cards)))
    (card-games-shuffle cards)))

(defun card-games-crap--top (pile)
  "Return the top (last) card of PILE, or nil."
  (car (last pile)))


;;;; Pile access

(defun card-games-crap--found (g i) "Foundation I of G." (aref (card-games-get g :found) i))
(defun card-games-crap--house (g i) "House I of G." (aref (card-games-get g :houses) i))
(defun card-games-crap--reserve (g p) "Player P's reserve list in G." (aref (card-games-get g :reserve) p))
(defun card-games-crap--waste (g p) "Player P's waste list in G." (aref (card-games-get g :waste) p))
(defun card-games-crap--hand (g p) "Player P's hand list in G." (aref (card-games-get g :hand) p))


;;;; Dealing

(defun card-games-crap--deal (game)
  "Deal a fresh Russian Bank layout into GAME and return it."
  (let ((d0 (card-games-crap--deck)) (d1 (card-games-crap--deck))
        (found (make-vector 8 nil)) (houses (make-vector 8 nil))
        (reserve (make-vector 2 nil)) (waste (make-vector 2 nil))
        (hand (make-vector 2 nil)))
    (dotimes (i 4) (aset houses i (list (pop d0))))
    (dotimes (i 4) (aset houses (+ 4 i) (list (pop d1))))
    (aset reserve 0 (cl-loop repeat 13 collect (pop d0)))
    (aset reserve 1 (cl-loop repeat 13 collect (pop d1)))
    (aset hand 0 d0)
    (aset hand 1 d1)
    (card-games-put game :found found)
    (card-games-put game :houses houses)
    (card-games-put game :reserve reserve)
    (card-games-put game :waste waste)
    (card-games-put game :hand hand)
    (card-games-put game :turn 0)
    (card-games-put game :cursor 0)
    (card-games-put game :sel nil)
    (card-games-put game :moves 0)
    (card-games-put game :history nil)
    (card-games-put game :winner nil)
    (card-games-put game :message
            "Your turn.  Foundations first -- skip one and North calls STOP!  Build the houses or load North.")
    game))


;;;; Legality

(defun card-games-crap--found-accepts (game i card)
  "Return non-nil when CARD may go onto foundation I of GAME."
  (and card
       (let ((f (card-games-crap--found game i)))
         (if (null f)
             (= (cdr card) 0)                       ; empty foundation takes an Ace
           (let ((top (card-games-crap--top f)))
             (and (= (car card) (car top))
                  (= (cdr card) (1+ (cdr top)))))))))

(defun card-games-crap--house-accepts (game i card)
  "Return non-nil when CARD may go onto house I of GAME."
  (and card
       (let ((h (card-games-crap--house game i)))
         (if (null h)
             t                                       ; empty house takes anything
           (card-games-crap--builds-down-p (card-games-crap--top h) card)))))

(defun card-games-crap--builds-down-p (upper lower)
  "Return non-nil when LOWER may sit on UPPER: one rank down, opposite colour."
  (and upper lower
       (= (cdr lower) (1- (cdr upper)))
       (not (eq (card-games-crap--red-p upper) (card-games-crap--red-p lower)))))

(defun card-games-crap--load-accepts (pile-top card)
  "Return non-nil when CARD may be loaded onto a pile whose top is PILE-TOP.
Loading needs the same suit and a rank one step up or down."
  (and pile-top card
       (= (car pile-top) (car card))
       (= 1 (abs (- (cdr pile-top) (cdr card))))))

(defun card-games-crap--found-for (game card)
  "Return the index of a GAME foundation that would accept CARD, or nil."
  (cl-loop for i below 8 when (card-games-crap--found-accepts game i card) return i))


;;;; Sequenced house moves

(defun card-games-crap--house-run (game i)
  "Return the movable top run of GAME house I as a list, bottom-to-top.
The run is the longest sequence of cards at the top of the house that is
built down in alternating colours."
  (let ((rev (reverse (card-games-crap--house game i))))     ; top-first
    (if (null rev) nil
      (let ((run (list (car rev))) (prev (car rev)))
        (catch 'done
          (dolist (c (cdr rev))                       ; each C sits below PREV
            (if (card-games-crap--builds-down-p c prev)
                (progn (push c run) (setq prev c))     ; PREV builds down on C
              (throw 'done nil))))
        run))))

(defun card-games-crap--free-houses (game)
  "Return the number of empty houses in GAME."
  (cl-count-if #'null (append (card-games-get game :houses) nil)))

(defun card-games-crap--capacity (game dest-i)
  "Return how many cards may be moved as a group onto GAME house DEST-I.
With F empty houses you may relay F+1 cards onto a non-empty house, or
F onto an empty house (the destination itself cannot serve as a relay)."
  (let ((free (card-games-crap--free-houses game)))
    (if (card-games-crap--house game dest-i) (1+ free) free)))

(defun card-games-crap--house-move (game i j &optional want)
  "Move a legal sequence from house I onto house J in GAME.
WANT, when given, is how many of the top cards to move onto an EMPTY
house (default the whole run); it is ignored for a non-empty house, where
the landing rank fixes the count.  Return non-nil on success, the symbol
`space' when the sequence is legal but there are too few empty houses,
and nil when nothing fits."
  (if (= i j)
      nil
    (let* ((run (card-games-crap--house-run game i))
           (rlen (length run))
           (dst (card-games-crap--house game j))
           (dsttop (card-games-crap--top dst))
           (cap (card-games-crap--capacity game j))
           (n nil))
      (when (> rlen 0)
        (if (null dsttop)
            (setq n (min (or want rlen) rlen))        ; empty house: the chosen count
          (cl-loop for k from 0 below rlen            ; RUN is bottom-to-top
                   for card = (nth k run)
                   when (card-games-crap--builds-down-p dsttop card)
                   do (setq n (- rlen k)) (cl-return)))
        (cond
         ((or (null n) (< n 1)) nil)
         ((> n cap) 'space)
         (t
          (card-games-crap--snapshot game)
          (let* ((pile (card-games-crap--house game i))
                 (keep (butlast pile n))
                 (moved (last pile n)))
            (aset (card-games-get game :houses) i keep)
            (aset (card-games-get game :houses) j (append dst moved)))
          (card-games-put game :moves (1+ (card-games-get game :moves)))
          t))))))


;;;; Spots (the cursor visits piles)

(defun card-games-crap--spots (_game)
  "Return the ordered list of (TYPE . INDEX) spots the cursor visits."
  (append (list '(res . 0) '(was . 0))
          (cl-loop for i below 8 collect (cons 'house i))
          (cl-loop for i below 8 collect (cons 'found i))
          (list '(res . 1) '(was . 1))))

(defun card-games-crap--sources (_game p)
  "Return the spots whose top card player P may pick up."
  (append (list (cons 'res p) (cons 'was p))
          (cl-loop for i below 8 collect (cons 'house i))))

(defun card-games-crap--spot-top (game spot)
  "Return the top card available at SPOT of GAME, or nil."
  (pcase (car spot)
    ('res   (card-games-crap--top (card-games-crap--reserve game (cdr spot))))
    ('was   (card-games-crap--top (card-games-crap--waste game (cdr spot))))
    ('house (card-games-crap--top (card-games-crap--house game (cdr spot))))
    ('found (card-games-crap--top (card-games-crap--found game (cdr spot))))))

(defun card-games-crap--source-p (spot p)
  "Return non-nil when SPOT is a pile player P may take a card from."
  (pcase (car spot)
    ('res (= (cdr spot) p))
    ('was (= (cdr spot) p))
    ('house t)
    (_ nil)))

(defun card-games-crap--dest-ok (game from to card p)
  "Return non-nil when player P may move CARD from FROM to TO in GAME."
  (pcase (car to)
    ('found (card-games-crap--found-accepts game (cdr to) card))
    ('house (card-games-crap--house-accepts game (cdr to) card))
    ((or 'res 'was)
     (and (= (cdr to) (- 1 p))                       ; only onto the opponent
          (memq (car from) '(res was))               ; only your reserve/waste cards load
          (card-games-crap--load-accepts (card-games-crap--spot-top game to) card)))))


;;;; Moving cards

(defun card-games-crap--snapshot (game)
  "Record GAME's mutable state for undo, unless recording is disabled."
  (when card-games-crap--recording
    (card-games-put game :history
            (cons (list (card-games-crap--copy-vec (card-games-get game :found))
                        (card-games-crap--copy-vec (card-games-get game :houses))
                        (card-games-crap--copy-vec (card-games-get game :reserve))
                        (card-games-crap--copy-vec (card-games-get game :waste))
                        (card-games-crap--copy-vec (card-games-get game :hand))
                        (card-games-get game :turn))
                  (card-games-get game :history)))))

(defun card-games-crap--copy-vec (v)
  "Return a copy of vector V with each element list copied."
  (vconcat (mapcar #'copy-sequence v)))

(defun card-games-crap--take (game spot)
  "Remove and return the top card of SPOT's pile in GAME."
  (pcase (car spot)
    ('res (let* ((p (cdr spot)) (pile (card-games-crap--reserve game p)))
            (aset (card-games-get game :reserve) p (butlast pile 1)) (card-games-crap--top pile)))
    ('was (let* ((p (cdr spot)) (pile (card-games-crap--waste game p)))
            (aset (card-games-get game :waste) p (butlast pile 1)) (card-games-crap--top pile)))
    ('house (let* ((i (cdr spot)) (pile (card-games-crap--house game i)))
              (aset (card-games-get game :houses) i (butlast pile 1)) (card-games-crap--top pile)))))

(defun card-games-crap--place (game spot card)
  "Add CARD to the top of SPOT's pile in GAME."
  (pcase (car spot)
    ('found (let ((i (cdr spot)))
              (aset (card-games-get game :found) i (append (card-games-crap--found game i) (list card)))))
    ('house (let ((i (cdr spot)))
              (aset (card-games-get game :houses) i (append (card-games-crap--house game i) (list card)))))
    ('res (let ((p (cdr spot)))
            (aset (card-games-get game :reserve) p (append (card-games-crap--reserve game p) (list card)))))
    ('was (let ((p (cdr spot)))
            (aset (card-games-get game :waste) p (append (card-games-crap--waste game p) (list card)))))))

(defun card-games-crap--move (game from to p)
  "Move the top card of FROM onto TO in GAME for player P.
Return non-nil when the move was legal and performed."
  (let ((card (card-games-crap--spot-top game from)))
    (when (and card (card-games-crap--source-p from p) (card-games-crap--dest-ok game from to card p))
      (card-games-crap--snapshot game)
      (card-games-crap--take game from)
      (card-games-crap--place game to card)
      (card-games-put game :moves (1+ (card-games-get game :moves)))
      t)))

(defun card-games-crap--do-move (game from to p &optional want)
  "Perform player P's move from FROM to TO in GAME.
A house-to-house move may carry a legal sequence of cards (see
`card-games-crap--house-move', to which WANT is passed); every other
move carries a single card.  Returns the same values as those
functions (non-nil on success, `space' when a sequence is too long
for the empty houses)."
  (if (and (eq (car from) 'house) (eq (car to) 'house))
      (card-games-crap--house-move game (cdr from) (cdr to) want)
    (card-games-crap--move game from to p)))


;;;; Turn logic

(defun card-games-crap--forced (game p)
  "Return player P's GAME spots whose top card must go to a foundation."
  (let (out)
    (dolist (spot (card-games-crap--sources game p))
      (let ((card (card-games-crap--spot-top game spot)))
        (when (and card (card-games-crap--found-for game card)) (push spot out))))
    (nreverse out)))

(defun card-games-crap--card-playable (game p card)
  "Return non-nil when CARD has a legal destination in GAME for player P."
  (or (card-games-crap--found-for game card)
      (cl-loop for i below 8 thereis (card-games-crap--house-accepts game i card))
      (card-games-crap--load-accepts (card-games-crap--top (card-games-crap--reserve game (- 1 p))) card)
      (card-games-crap--load-accepts (card-games-crap--top (card-games-crap--waste game (- 1 p))) card)))

(defun card-games-crap--won-p (game p)
  "Return non-nil when GAME player P has emptied reserve, hand, and waste."
  (and (null (card-games-crap--reserve game p))
       (null (card-games-crap--hand game p))
       (null (card-games-crap--waste game p))))

(cl-defmethod card-games-won-p ((game card-games-crapette-game))
  "Return non-nil when you (South) have won GAME."
  (eq (card-games-get game :winner) 0))

(defun card-games-crap--msg (game s)
  "Set GAME's status message to S."
  (card-games-put game :message s))

(defun card-games-crap--after-human-move (game)
  "Note a win after a human move in GAME."
  (when (card-games-crap--won-p game 0)
    (card-games-put game :winner 0)
    (card-games-crap--msg game "You emptied your cards -- you win!")))

(defun card-games-crap--end-turn (game)
  "Pass GAME's turn from you to the AI, run it, and return control to you."
  (unless (card-games-get game :winner)
    (card-games-put game :sel nil)
    (card-games-put game :turn 1)
    (let ((card-games-crap--recording nil))
      (card-games-crap--ai-play game))
    (unless (card-games-get game :winner)
      (card-games-put game :turn 0)
      (card-games-put game :history nil))))

(defun card-games-crap--stop (game)
  "Enforce GAME's foundation priority, per `card-games-crapette-stops'.
Call \"stop\" or block; return non-nil when the offending action must be
abandoned by its caller."
  (when (card-games-crap--forced game 0)
    (if card-games-crapette-stops
        (let ((note "North calls STOP -- you must play to a foundation first!"))
          (message "%s" note)
          (card-games-put game :sel nil)
          (card-games-crap--end-turn game)
          (card-games-put game :message (concat note "  " (card-games-get game :message))))
      (card-games-crap--msg game "Play to a foundation first (press f on a highlighted pile)."))
    t))


;;;; The AI opponent (player 1)

(defun card-games-crap--ai-found-move (game)
  "Return an AI (SOURCE . FOUNDATION) foundation move for GAME, or nil."
  (cl-loop for spot in (card-games-crap--sources game 1)
           for card = (card-games-crap--spot-top game spot)
           for fi = (and card (card-games-crap--found-for game card))
           when fi return (cons spot (cons 'found fi))))

(defun card-games-crap--ai-unload-move (game)
  "Return GAME's best AI (SOURCE . DEST) move that empties reserve or waste.
Emptying the RESERVE is the goal of the game, so it outscores the waste;
LOADING a card onto you (which also burdens you) outscores building a
house.  Every such move reduces the AI's own cards, so its turn ends."
  (let ((best nil) (bestscore 0))
    (dolist (spot (list (cons 'res 1) (cons 'was 1)))
      (let ((card (card-games-crap--spot-top game spot))
            (base (if (eq (car spot) 'res) 40 0)))   ; the reserve is the bottleneck
        (when card
          (dolist (dst (list (cons 'res 0) (cons 'was 0)))
            (when (card-games-crap--load-accepts (card-games-crap--spot-top game dst) card)
              (let ((sc (+ base 60)))                ; loading: rid a card AND burden you
                (when (> sc bestscore)
                  (setq bestscore sc best (cons spot dst))))))
          (cl-loop for i below 8
                   when (card-games-crap--house-accepts game i card)
                   do (let ((sc (+ base 50)))        ; else build it onto a house
                        (when (> sc bestscore)
                          (setq bestscore sc best (cons spot (cons 'house i)))))
                   (cl-return)))))
    best))

(defun card-games-crap--ai-enabling-move (game)
  "Return a GAME single-card house->house move that unlocks an unload, or nil.
This is the crafty bit: when the AI cannot place its reserve or waste top
anywhere, it looks one move ahead for a house rearrangement that would
make such a placement legal.  It only fires when no direct unload exists,
and only when the shuffle genuinely opens one, so the turn still ends."
  (when (null (card-games-crap--ai-unload-move game))
    (catch 'found
      (dotimes (i 8)
        (dotimes (j 8)
          (let ((pilei (card-games-crap--house game i)) (pilej (card-games-crap--house game j)))
            (when (and (/= i j) pilei)
              (let ((card (card-games-crap--top pilei)))
                (when (card-games-crap--house-accepts game j card)
                  (aset (card-games-get game :houses) i (butlast pilei 1))
                  (aset (card-games-get game :houses) j (append pilej (list card)))
                  (let ((opens (card-games-crap--ai-unload-move game)))
                    (aset (card-games-get game :houses) i pilei)
                    (aset (card-games-get game :houses) j pilej)
                    (when opens
                      (throw 'found (cons (cons 'house i) (cons 'house j)))))))))))
      nil)))

(defun card-games-crap--ai-greedy-move (game)
  "A simple first-fit unload move for GAME -- the `easy' AI.
Empties the reserve or waste top onto the first legal spot, without the
scoring or the house-rearranging lookahead of the tougher levels."
  (catch 'm
    (dolist (spot (list (cons 'res 1) (cons 'was 1)))
      (let ((card (card-games-crap--spot-top game spot)))
        (when card
          (dolist (dst (list (cons 'res 0) (cons 'was 0)))
            (when (card-games-crap--load-accepts (card-games-crap--spot-top game dst) card)
              (throw 'm (cons spot dst))))
          (cl-loop for i below 8
                   when (card-games-crap--house-accepts game i card)
                   do (throw 'm (cons spot (cons 'house i)))))))
    nil))

(defun card-games-crap--ai-play (game)
  "Play the AI opponent's whole turn on GAME, per `card-games-ai-level'."
  (let ((guard 0) (level card-games-ai-level))
    (catch 'done
      (while t
        (when (> (setq guard (1+ guard)) 800) (throw 'done nil))
        (when (card-games-crap--won-p game 1) (throw 'done nil))
        (let ((mv (or (card-games-crap--ai-found-move game)
                      (if (eq level 'easy)
                          (card-games-crap--ai-greedy-move game)
                        (card-games-crap--ai-unload-move game))
                      (and (eq level 'hard) (card-games-crap--ai-enabling-move game)))))
          (cond
           (mv (card-games-crap--move game (car mv) (cdr mv) 1))
           ((card-games-crap--hand game 1)
            (let* ((hand (card-games-crap--hand game 1)) (card (card-games-crap--top hand)))
              (aset (card-games-get game :hand) 1 (butlast hand 1))
              (aset (card-games-get game :waste) 1 (append (card-games-crap--waste game 1) (list card)))
              (unless (card-games-crap--card-playable game 1 card) (throw 'done 'flip-end))))
           (t (throw 'done 'stuck))))))
    (if (card-games-crap--won-p game 1)
        (progn (card-games-put game :winner 1)
               (card-games-crap--msg game "Your opponent emptied their cards -- you lose."))
      (card-games-crap--msg game "Your opponent finished their turn.  Your move."))))


;;;; Rendering (text)

(defun card-games-crap--render (g)
  "Return a propertized depiction of Russian Bank game G."
  (let* ((spots (card-games-crap--spots g))
         (cur (nth (card-games-get g :cursor) spots))
         (sel (card-games-get g :sel))
         (forced (card-games-crap--forced g 0))
         (turn (card-games-get g :turn))
         (win (card-games-get g :winner)))
    (cl-flet ((cell (spot label)
                (let* ((card (card-games-crap--spot-top g spot))
                       (txt (concat label (card-games-crap--card-string card)))
                       (face (cond ((equal spot cur) 'card-games-cursor)
                                   ((equal spot sel) 'card-games-hint)
                                   ((member spot forced) 'card-games-hint)
                                   ((card-games-crap--red-p card) 'card-games-red-suit)
                                   (t 'default))))
                  (propertize (format "%-8s" txt) 'face face))))
      (concat
       (format "  Russian Bank -- %s\n\n"
               (cond ((eq win 0) "you win!")
                     ((eq win 1) "you lose")
                     ((= turn 0) "your turn")
                     (t "opponent's turn")))
       (format "  Opponent (North)   hand:%2d   reserve:%2d   waste:%2d\n    "
               (length (card-games-crap--hand g 1)) (length (card-games-crap--reserve g 1))
               (length (card-games-crap--waste g 1)))
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
               (length (card-games-crap--hand g 0)) (length (card-games-crap--reserve g 0))
               (length (card-games-crap--waste g 0)))
       (cell '(res . 0) "R:") (cell '(was . 0) "W:") "\n\n"
       (format "  %s\n" (card-games-get g :message))
       (card-games-crap--holding-line g)))))

(defun card-games-crap--holding-line (g)
  "Return a line showing G's picked-up house run, marking the held top cards.
Empty when nothing (or a single card) is held from a house."
  (let ((sel (card-games-get g :sel)))
    (if (not (and sel (eq (car sel) 'house)))
        ""
      (let* ((run (card-games-crap--house-run g (cdr sel)))
             (m (length run))
             (held (min (or (card-games-get g :sel-n) m) m))
             (i 0) (parts nil))
        (if (< m 2)
            ""
          (dolist (c run)
            (let ((on (>= i (- m held))))
              (push (propertize (concat (card-games-crap--card-string c) " ")
                                'face (cond (on 'card-games-hint)
                                            ((card-games-crap--red-p c) 'card-games-red-suit)
                                            (t 'shadow)))
                    parts))
            (setq i (1+ i)))
          (concat "  Holding: " (apply #'concat (nreverse parts))
                  (format "(moving top %d of %d)\n" held m)))))))

(defun card-games-crap--svg (g)
  "Return (DISPLAY-STRING . REGIONS) drawing Russian Bank game G as SVG.
REGIONS maps clicked rectangles to (TYPE . INDEX) spots."
  (let* ((w card-games-svg-card-width) (h card-games-svg-card-height)
         (gap card-games-svg-card-gap) (pad 12) (colgap 14)
         (vdown (max 16 (round (* h 0.26))))
         (spots (card-games-crap--spots g))
         (cur (nth (card-games-get g :cursor) spots))
         (sel (card-games-get g :sel))
         (forced (card-games-crap--forced g 0))
         (heldn (and sel (eq (car sel) 'house)
                     (min (or (card-games-get g :sel-n) 0)
                          (length (card-games-crap--house-run g (cdr sel))))))
         (houses (card-games-get g :houses))
         (maxlen (apply #'max 1 (mapcar #'length (append houses nil))))
         (fx (+ w gap)) (hx (+ w colgap))
         (width (+ (* 2 pad) (* 8 w) (* 7 colgap)))
         (y-title 6) (y-opp 34)
         (y-found (+ y-opp h 30))
         (y-house (+ y-found h 30))
         (house-h (+ h (* (1- maxlen) vdown)))
         (y-you (+ y-house house-h 28))
         (height (+ y-you h 34))
         (svg (svg-create width height))
         (lc (card-games-color 'shadow :foreground "gray50"))
         (regions '()))
    (cl-labels
        ((txt (str x y &optional sz bold)
              (apply #'svg-text svg str :x x :y y :font-size (or sz 12) :fill lc
                     :font-family card-games-svg-font-family
                     (if bold (list :font-weight "bold") nil)))
         (curp (spot) (equal spot cur))
         (forcedp (spot) (and (member spot forced) t))
         (pile-cell (spot x y)
                    (let ((spec (card-games-crap--spec (card-games-crap--spot-top g spot))))
                      (if spec
                          (card-games-svg-card svg x y :rank (car spec) :suit (cdr spec)
                                       :highlight (curp spot) :hint (forcedp spot))
                        (card-games-svg-card svg x y :gap t :highlight (curp spot)))
                      (push (cons (list x y w h) spot) regions))))
      (txt (format "Russian Bank -- %s"
                   (cond ((eq (card-games-get g :winner) 0) "you win!")
                         ((eq (card-games-get g :winner) 1) "you lose")
                         ((= (card-games-get g :turn) 0) "your turn")
                         (t "opponent's turn")))
           pad (+ y-title 12) 13 t)
      (txt (format "North  reserve %d  waste %d  hand %d"
                   (length (card-games-crap--reserve g 1)) (length (card-games-crap--waste g 1))
                   (length (card-games-crap--hand g 1)))
           pad (- y-opp 4))
      (pile-cell '(res . 1) pad y-opp)
      (pile-cell '(was . 1) (+ pad fx) y-opp)
      (txt "Foundations" pad (- y-found 4))
      (dotimes (i 8) (pile-cell (cons 'found i) (+ pad (* i fx)) y-found))
      (txt "Houses" pad (- y-house 4))
      (dotimes (i 8)
        (let* ((x (+ pad (* i hx))) (s (cons 'house i))
               (pile (aref houses i)) (len (length pile)))
          (push (cons (list x y-house w house-h) s) regions)
          (if (= len 0)
              (card-games-svg-card svg x y-house :gap t :highlight (curp s))
            (let ((y y-house) (k 0))
              (dolist (card pile)
                (let ((spec (card-games-crap--spec card)) (topp (= k (1- len))))
                  (card-games-svg-card svg x y :rank (car spec) :suit (cdr spec)
                               :highlight (and topp (curp s))
                               :hint (and topp (forcedp s)))
                  (when (and (equal sel s) heldn (>= k (- len heldn)))
                    (svg-rectangle svg (- x 3) (- y 3) (+ w 6) (+ h 6)
                                   :rx 8 :fill "none" :stroke "#4a90d9" :stroke-width 3)))
                (setq y (+ y vdown) k (1+ k)))))))
      (txt (format "You  reserve %d  waste %d  hand %d"
                   (length (card-games-crap--reserve g 0)) (length (card-games-crap--waste g 0))
                   (length (card-games-crap--hand g 0)))
           pad (- y-you 4))
      (pile-cell '(res . 0) pad y-you)
      (pile-cell '(was . 0) (+ pad fx) y-you)
      (txt (card-games-get g :message) pad (- height 10) 12))
    (let ((rev (nreverse regions)))
      (cons (propertize "*" 'display (card-games-svg-image svg (card-games-scale))
                        'card-games-regions rev)
            rev))))

(defun card-games-crap-mouse (event)
  "Handle click EVENT on the SVG board: select that pile and act on it."
  (interactive "e")
  (let* ((g card-games-crap--game) (spot (card-games-mouse-action event)))
    (when spot
      (let ((idx (cl-position spot (card-games-crap--spots g) :test #'equal)))
        (when idx (card-games-put g :cursor idx) (card-games-crap-act))))))

(cl-defmethod card-games-render ((game card-games-crapette-game))
  "Return a text depiction of GAME."
  (card-games-crap--render game))

(defun card-games-crap--redisplay ()
  "Redraw the current Russian Bank buffer (SVG on a graphical display)."
  (let ((g card-games-crap--game) (inhibit-read-only t))
    (setq card-games-current-game g)
    (setq-local card-games-redisplay-function #'card-games-crap--redisplay)
    (setq-local mode-line-process
                (format " [%s]"
                        (cond ((eq (card-games-get g :winner) 0) "you win")
                              ((eq (card-games-get g :winner) 1) "you lose")
                              ((= (card-games-get g :turn) 0) "your turn")
                              (t "opponent"))))
    (erase-buffer)
    (if (and card-games-crapette-svg-cards (display-graphic-p))
        (insert (car (card-games-crap--svg g)) "\n")
      (insert (card-games-crap--render g)))
    (card-games-insert-legend
     "arrows/click move · RET pick up/drop · [ ] group size · f foundation · SPC turn · e end · u undo · n new · q menu")
    (goto-char (point-min))))


;;;; Commands

(defun card-games-crap--move-cursor (g d)
  "Move G's cursor by D spots and redisplay."
  (let ((n (length (card-games-crap--spots g))))
    (card-games-put g :cursor (mod (+ (card-games-get g :cursor) d) n)))
  (card-games-crap--redisplay))

(defun card-games-crap-left ()  "Move the cursor to the previous pile."
  (interactive) (card-games-crap--move-cursor card-games-crap--game -1))
(defun card-games-crap-right () "Move the cursor to the next pile."
  (interactive) (card-games-crap--move-cursor card-games-crap--game 1))

(defun card-games-crap--your-turn-p (g)
  "Return non-nil when it is your move in G (and warn otherwise)."
  (cond ((card-games-get g :winner)
         (card-games-crap--msg g "The game is over -- press n for a new game.") nil)
        ((/= (card-games-get g :turn) 0)
         (card-games-crap--msg g "Wait for your turn.") nil)
        (t t)))

(defun card-games-crap-act ()
  "Pick up the card under the cursor, or drop the picked-up card there."
  (interactive)
  (let* ((g card-games-crap--game) (spots (card-games-crap--spots g))
         (spot (nth (card-games-get g :cursor) spots)) (sel (card-games-get g :sel)))
    (when (card-games-crap--your-turn-p g)
      (if sel
          (unless (and (not (eq (car spot) 'found)) (card-games-crap--stop g))
            (let ((res (card-games-crap--do-move g sel spot 0 (card-games-get g :sel-n))))
              (cond
               ((eq res 'space)
                (card-games-crap--msg g "Not enough empty houses to move that many cards."))
               (res (card-games-put g :sel nil) (card-games-put g :sel-n nil) (card-games-crap--after-human-move g))
               (t (card-games-crap--msg g "That card can't go there.")))))
        (if (and (card-games-crap--source-p spot 0) (card-games-crap--spot-top g spot))
            (let* ((top (card-games-crap--spot-top g spot))
                   (run (and (eq (car spot) 'house) (card-games-crap--house-run g (cdr spot))))
                   (n (length run)))
              (card-games-put g :sel spot)
              (card-games-put g :sel-n (max 1 n))
              (card-games-crap--msg g
                            (if (> n 1)
                                (format "Picked up a run of %d (%s on top) -- [ / ] to change how many, drop on a house."
                                        n (card-games-crap--card-string top))
                              (format "Picked up %s -- choose where to drop it."
                                      (card-games-crap--card-string top)))))
          (card-games-crap--msg g "Nothing of yours to pick up there."))))
    (card-games-crap--redisplay)))

(defun card-games-crap-found ()
  "Send the card under the cursor (or the picked-up card) to a foundation."
  (interactive)
  (let* ((g card-games-crap--game) (spots (card-games-crap--spots g))
         (spot (nth (card-games-get g :cursor) spots)))
    (when (card-games-crap--your-turn-p g)
      (let* ((src (or (card-games-get g :sel) spot))
             (card (and (card-games-crap--source-p src 0) (card-games-crap--spot-top g src)))
             (fi (and card (card-games-crap--found-for g card))))
        (if (and fi (card-games-crap--move g src (cons 'found fi) 0))
            (progn (card-games-put g :sel nil) (card-games-crap--after-human-move g))
          (card-games-crap--msg g "No foundation accepts that card."))))
    (card-games-crap--redisplay)))

(defun card-games-crap--hold-adjust (d)
  "Change how many cards of a picked-up house run you hold, by D."
  (let* ((g card-games-crap--game) (sel (card-games-get g :sel)))
    (when (card-games-crap--your-turn-p g)
      (if (and sel (eq (car sel) 'house))
          (let* ((m (length (card-games-crap--house-run g (cdr sel))))
                 (new (max 1 (min m (+ (or (card-games-get g :sel-n) m) d)))))
            (card-games-put g :sel-n new)
            (card-games-crap--msg g (format "Holding the top %d of %d -- drop on an empty house."
                                    new m)))
        (card-games-crap--msg g "Pick up a house run first, then [ and ] set how many to move.")))
    (card-games-crap--redisplay)))

(defun card-games-crap-hold-less ()
  "Hold one fewer card of the picked-up run."
  (interactive) (card-games-crap--hold-adjust -1))

(defun card-games-crap-hold-more ()
  "Hold one more card of the picked-up run."
  (interactive) (card-games-crap--hold-adjust 1))

(defun card-games-crap-draw ()
  "Turn the top card of your hand onto your waste.
If it fits nowhere your turn ends."
  (interactive)
  (let ((g card-games-crap--game))
    (when (card-games-crap--your-turn-p g)
      (unless (card-games-crap--stop g)
        (if (null (card-games-crap--hand g 0))
            (card-games-crap--msg g "Your hand is empty -- press e to end your turn.")
          (card-games-crap--snapshot g)
          (let* ((hand (card-games-crap--hand g 0)) (card (card-games-crap--top hand)))
            (aset (card-games-get g :hand) 0 (butlast hand 1))
            (aset (card-games-get g :waste) 0 (append (card-games-crap--waste g 0) (list card)))
            (card-games-put g :sel nil)
            (if (card-games-crap--card-playable g 0 card)
                (card-games-crap--msg g (format "Turned %s onto your waste -- play on."
                                        (card-games-crap--card-string card)))
              (progn
                (card-games-crap--msg g (format "Turned %s -- nothing to do, your turn ends."
                                        (card-games-crap--card-string card)))
                (card-games-crap--end-turn g)))))))
    (card-games-crap--redisplay)))

(defun card-games-crap-end ()
  "End your turn and let the opponent play."
  (interactive)
  (let ((g card-games-crap--game))
    (when (card-games-crap--your-turn-p g)
      (unless (card-games-crap--stop g)
        (card-games-crap--msg g "You end your turn.")
        (card-games-crap--end-turn g)))
    (card-games-crap--redisplay)))

(defun card-games-crap-undo ()
  "Undo your last move this turn."
  (interactive)
  (let* ((g card-games-crap--game) (h (card-games-get g :history)))
    (if (null h)
        (card-games-crap--msg g "Nothing to undo.")
      (let ((s (car h)))
        (card-games-put g :found (nth 0 s)) (card-games-put g :houses (nth 1 s))
        (card-games-put g :reserve (nth 2 s)) (card-games-put g :waste (nth 3 s))
        (card-games-put g :hand (nth 4 s)) (card-games-put g :turn (nth 5 s))
        (card-games-put g :history (cdr h)) (card-games-put g :sel nil) (card-games-put g :winner nil)
        (card-games-crap--msg g "Undid a move.")))
    (card-games-crap--redisplay)))

(defun card-games-crap-new ()
  "Deal a fresh Russian Bank game."
  (interactive)
  (card-games-crap--deal card-games-crap--game)
  (card-games-crap--redisplay))

(defun card-games-crap-redraw () "Redraw the board." (interactive) (card-games-crap--redisplay))

(defun card-games-crap-help ()
  "Describe the controls."
  (interactive)
  (message
   "Arrows: move  RET: pick up/drop  [ ]: how many cards of a run  f: to foundation  SPC: turn a card  e: end  u: undo  n: new  q: menu"))

(defvar card-games-crapette-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'card-games-crap-left)
    (define-key map (kbd "<right>") #'card-games-crap-right)
    (define-key map (kbd "<up>")    #'card-games-crap-left)
    (define-key map (kbd "<down>")  #'card-games-crap-right)
    (define-key map (kbd "RET") #'card-games-crap-act)
    (define-key map [mouse-1] #'card-games-crap-mouse)
    (define-key map "+" #'card-games-card-zoom-in)
    (define-key map "=" #'card-games-card-zoom-in)
    (define-key map "-" #'card-games-card-zoom-out)
    (define-key map "0" #'card-games-card-zoom-reset)
    (define-key map (kbd "SPC") #'card-games-crap-draw)
    (define-key map "f" #'card-games-crap-found)
    (define-key map "[" #'card-games-crap-hold-less)
    (define-key map "]" #'card-games-crap-hold-more)
    (define-key map "d" #'card-games-crap-draw)
    (define-key map "e" #'card-games-crap-end)
    (define-key map "u" #'card-games-crap-undo)
    (define-key map "n" #'card-games-crap-new)
    (define-key map "g" #'card-games-crap-redraw)
    (define-key map "?" #'card-games-crap-help)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-crapette-mode'.")

(define-derived-mode card-games-crapette-mode special-mode "Crapette"
  "Major mode for two-player Russian Bank (Crapette)."
  (setq-local truncate-lines t)
  (setq-local cursor-type card-games-cursor-type))

;;;###autoload
(defun card-games-crapette ()
  "Play two-player Russian Bank (Crapette) against the computer."
  (interactive)
  (let ((buf (get-buffer-create "*Russian Bank (Crapette)*")))
    (with-current-buffer buf
      (card-games-crapette-mode)
      (setq card-games-crap--game (card-games-crap--deal (card-games-crapette-game)))
      (card-games-crap--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defalias 'card-games-russian-bank-duel #'card-games-crapette
  "Alias for `card-games-crapette'.")

(provide 'card-games-crapette)
;;; card-games-crapette.el ends here
