;;; card-games-gaps.el --- Gaps-style row solitaires (Montana, Hell's Half-Acre)  -*- lexical-binding: t; -*-

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

;; The "gaps" family of solitaires: 48 cards dealt into four rows of
;; thirteen with four gaps.  A gap is filled by the card one rank along
;; from the card to its left, of the same suit; the head gap of a row
;; takes the anchor rank in any suit.
;;
;; Two variants ship, demonstrating how a new game is *derived* by
;; subclassing the abstract `card-games-gaps-game' and overriding two methods
;; (`card-games-gaps--head' and `card-games-gaps--step'):
;;
;;   `card-games-montana'          Gaps / Montana: Two at the head, rows build
;;                         UP  2 3 4 ... K; nothing follows a King.
;;   `card-games-hells-half-acre'  Hell's Half-Acre: King at the head, rows
;;                         build DOWN K Q J ... 2; nothing follows a Two.
;;
;; When stuck you may redeal (twice): each correct run from the head
;; stays, a gap opens just past it, and the rest are reshuffled.
;;
;; Renders as UNICODE text in a terminal and as SVG cards on a graphical
;; display; fillable gaps are highlighted.  Play via `M-x card-games' or
;; the commands above.

;;; Code:

(require 'card-games-core)
(require 'card-games-svg)
(require 'card-games-render)


;;;; Cards

(defconst card-games-gaps-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0..11 (Two through King).")

(defconst card-games-gaps-rank-names
  ["Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine"
   "Ten" "Jack" "Queen" "King"]
  "Full rank names indexed to match `card-games-gaps-ranks'.")

;; A card is a cons cell (SUIT . RANK); nil denotes a gap.

(defsubst card-games-gaps-red-p (card)
  "Return non-nil when CARD is a red suit (diamonds or hearts)."
  (and card (card-games-red-suit-p (car card))))

(defun card-games-gaps-card-string (card)
  "Return a short string for CARD, or a dot for a gap (nil)."
  (if (null card)
      "·"
    (concat (aref card-games-gaps-ranks (cdr card))
            (card-games-suit-glyph (car card)))))


;;;; Games — an abstract base and two subclasses

(defclass card-games-gaps-game (card-games-game)
  ((name :initform "Gaps"))
  "Abstract base for gaps-style row solitaires.
Subclasses set the head rank and build direction by overriding
`card-games-gaps--head' and `card-games-gaps--step'."
  :abstract t)

(cl-defgeneric card-games-gaps--head (game)
  "Return the rank index that anchors the head (left) of each row in GAME.")
(cl-defgeneric card-games-gaps--step (game)
  "Return GAME's per-column rank increment: +1 ascending, -1 descending.")
(cl-defgeneric card-games-gaps--vname (game)
  "Return the human-readable variant name for GAME.")

(defclass card-games-montana-game (card-games-gaps-game)
  ((name :initform "Montana"))
  "Gaps / Montana: a Two anchors the head; rows build up 2..K.")
(cl-defmethod card-games-gaps--head ((_ card-games-montana-game)) "Montana anchors its head on the Two (rank 0)." 0)
(cl-defmethod card-games-gaps--step ((_ card-games-montana-game)) "Montana builds up, +1 per column." 1)
(cl-defmethod card-games-gaps--vname ((_ card-games-montana-game)) "Return Montana's display name." "Gaps (Montana)")

(defclass card-games-acre-game (card-games-gaps-game)
  ((name :initform "Hell's Half-Acre"))
  "Hell's Half-Acre: a King anchors the head; rows build down K..2.")
(cl-defmethod card-games-gaps--head ((_ card-games-acre-game)) "Hell's Half-Acre anchors its head on the King (rank 11)." 11)
(cl-defmethod card-games-gaps--step ((_ card-games-acre-game)) "Hell's Half-Acre builds down, -1 per column." -1)
(cl-defmethod card-games-gaps--vname ((_ card-games-acre-game)) "Return Hell's Half-Acre's display name." "Hell's Half-Acre")

(defalias 'card-games-gaps--shuffle 'card-games-shuffle)

(defun card-games-gaps--full-deck ()
  "Return the 48 playable cards (Two..King in every suit)."
  (cl-loop for s below 4
           append (cl-loop for r below 12 collect (cons s r))))

(cl-defmethod card-games-gaps--deal ((game card-games-gaps-game))
  "Deal a fresh layout into GAME."
  (random t)
  (let ((cells (card-games-gaps--shuffle (append (card-games-gaps--full-deck)
                                         (make-list 4 nil))))
        (board (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 13)
          (aset row c (pop cells)))
        (aset board r row)))
    (card-games-put game :board board)
    (card-games-put game :moves 0)
    (card-games-put game :redeals 2)
    (card-games-put game :cursor (cons 0 0))
    (card-games-put game :history nil)
    (card-games-put game :message
            (format
             "Each row one suit, %s.  Move to a green + gap and press RET (or click it).  ? = help."
             (if (> (card-games-gaps--step game) 0) "2 up to K" "K down to 2")))
    game))

(defun card-games-gaps--cell (board r c)
  "Return the card at row R column C of BOARD (nil for a gap)."
  (aref (aref board r) c))

(cl-defmethod card-games-gaps--needed ((game card-games-gaps-game) board r c)
  "Return what may fill the gap at R, C of BOARD for GAME.
Returns the symbol `head' for a head gap, a (SUIT . RANK) card for any
other fillable gap, or nil if nothing fits."
  (if (= c 0)
      'head
    (let ((left (card-games-gaps--cell board r (1- c))))
      (if (null left)
          nil
        (let ((nr (+ (cdr left) (card-games-gaps--step game))))
          (and (>= nr 0) (<= nr 11) (cons (car left) nr)))))))

(defun card-games-gaps--find (board card)
  "Return (ROW . COL) of CARD in BOARD, or nil if absent."
  (catch 'hit
    (dotimes (r 4)
      (dotimes (c 13)
        (when (equal (card-games-gaps--cell board r c) card)
          (throw 'hit (cons r c)))))
    nil))

(defun card-games-gaps--copy-board (board)
  "Return a shallow copy of BOARD safe to mutate cell-by-cell."
  (apply #'vector (mapcar #'copy-sequence (append board nil))))

(cl-defmethod card-games-gaps--save-undo ((game card-games-gaps-game))
  "Push the current state of GAME onto its undo history."
  (card-games-put game :history
          (cons (list (card-games-gaps--copy-board (card-games-get game :board))
                      (card-games-get game :moves)
                      (card-games-get game :redeals))
                (card-games-get game :history))))

(cl-defmethod card-games-gaps--do-move ((game card-games-gaps-game) r c card)
  "Move CARD into the gap at R, C of GAME.  Return non-nil on success."
  (let* ((board (card-games-get game :board))
         (loc (card-games-gaps--find board card)))
    (if (not loc)
        (progn (card-games-put game :message
                       (format "The %s is not on the board?!"
                               (card-games-gaps-card-string card)))
               nil)
      (card-games-gaps--save-undo game)
      (setf (aref (aref board (car loc)) (cdr loc)) nil)
      (setf (aref (aref board r) c) card)
      (card-games-put game :moves (1+ (card-games-get game :moves)))
      (card-games-put game :message (format "Moved %s." (card-games-gaps-card-string card)))
      t)))

(cl-defmethod card-games-gaps--fill ((game card-games-gaps-game) r c)
  "Try to fill the gap at R, C of GAME.  Return non-nil on success."
  (let* ((board (card-games-get game :board))
         (cell (card-games-gaps--cell board r c)))
    (cond
     (cell
      (card-games-put game :message "That cell is not a gap.") nil)
     (t
      (let ((needed (card-games-gaps--needed game board r c)))
        (cond
         ((null needed)
          (card-games-put game :message "Nothing can fill that gap.") nil)
         ((eq needed 'head)
          (let ((suit (card-games-gaps--read-head game)))
            (and suit (card-games-gaps--do-move game r c (cons suit (card-games-gaps--head game))))))
         (t
          (card-games-gaps--do-move game r c needed))))))))

(cl-defmethod card-games-gaps--read-head ((game card-games-gaps-game))
  "Prompt for the suit of the head card of GAME.  Return suit 0..3 or nil."
  (let* ((name (aref card-games-gaps-rank-names (card-games-gaps--head game)))
         (ch (read-char-choice
              (format "Head gap — which %s? [s]pades [c]lubs [d]iamonds [h]earts (q=cancel): "
                      name)
              '(?s ?c ?d ?h ?q))))
    (cdr (assq ch '((?s . 0) (?c . 1) (?d . 2) (?h . 3))))))

(cl-defmethod card-games-won-p ((game card-games-gaps-game))
  "Return non-nil when every row of GAME is a full suited run with a trailing gap."
  (let ((board (card-games-get game :board))
        (head (card-games-gaps--head game))
        (step (card-games-gaps--step game)))
    (catch 'no
      (dotimes (r 4)
        (let* ((row (aref board r))
               (c0 (aref row 0)))
          (unless c0 (throw 'no nil))
          (let ((suit (car c0)))
            (dotimes (c 12)
              (let ((cell (aref row c)))
                (unless (and cell (= (car cell) suit)
                             (= (cdr cell) (+ head (* c step))))
                  (throw 'no nil))))
            (when (aref row 12) (throw 'no nil)))))
      t)))

(cl-defmethod card-games-gaps--stuck-p ((game card-games-gaps-game))
  "Return non-nil when no gap of GAME can currently be filled."
  (null (card-games-gaps--hints game)))

(cl-defmethod card-games-gaps--hints ((game card-games-gaps-game))
  "Return the list of (ROW . COL) gaps of GAME that can be filled now."
  (let ((board (card-games-get game :board))
        (hints nil))
    (dotimes (r 4)
      (dotimes (c 13)
        (when (and (null (card-games-gaps--cell board r c))
                   (card-games-gaps--needed game board r c))
          (push (cons r c) hints))))
    hints))

(cl-defmethod card-games-gaps--prefix-len ((game card-games-gaps-game) board r)
  "Return the length of GAME's correct run at the head of row R of BOARD."
  (let ((row (aref board r))
        (head (card-games-gaps--head game))
        (step (card-games-gaps--step game))
        (len 0))
    (let ((c0 (aref row 0)))
      (when (and c0 (= (cdr c0) head))
        (let ((suit (car c0)) (i 0) (cont t))
          (while (and cont (< i 12))
            (let ((cell (aref row i)))
              (if (and cell (= (car cell) suit)
                       (= (cdr cell) (+ head (* i step))))
                  (setq i (1+ i))
                (setq cont nil))))
          (setq len i))))
    len))

(cl-defmethod card-games-gaps--do-redeal ((game card-games-gaps-game))
  "Gather misplaced cards of GAME, reshuffle, and lay them back."
  (let* ((board (card-games-get game :board))
         (lens (make-vector 4 0))
         (kept nil))
    (dotimes (r 4)
      (let ((len (card-games-gaps--prefix-len game board r)))
        (aset lens r len)
        (dotimes (i len) (push (card-games-gaps--cell board r i) kept))))
    (let* ((remaining
            (card-games-gaps--shuffle
             (cl-remove-if (lambda (card) (cl-member card kept :test #'equal))
                           (card-games-gaps--full-deck))))
           (new (make-vector 4 nil)))
      (dotimes (r 4)
        (let ((row (make-vector 13 nil))
              (len (aref lens r)))
          (dotimes (i len)
            (aset row i (card-games-gaps--cell board r i)))
          ;; column LEN stays a gap.
          (cl-loop for c from (1+ len) below 13
                   do (aset row c (pop remaining)))
          (aset new r row)))
      (card-games-put game :board new))))


;;;; Rendering

(defun card-games-gaps--header (game)
  "Return the header text for GAME."
  (format "  ♠♣ %s ♦♥\n  Moves: %-4d  Redeals left: %d\n\n"
          (card-games-gaps--vname game) (card-games-get game :moves) (card-games-get game :redeals)))

(defun card-games-gaps--footer (game)
  "Return the footer text (just the current message) for GAME.
The control line is inserted separately by `card-games-gaps--insert-controls',
where each key hint is itself the clickable button."
  (format "\n  %s\n" (card-games-get game :message)))

(cl-defmethod card-games-render ((game card-games-gaps-game))
  "Return a propertized string depicting GAME (console rendering)."
  (let* ((board (card-games-get game :board))
         (cursor (card-games-get game :cursor))
         (cr (car cursor))
         (cc (cdr cursor))
         (hints (card-games-gaps--hints game))
         (out (list)))
    (push (card-games-gaps--header game) out)
    (dotimes (r 4)
      (dotimes (c 13)
        (let* ((cell (card-games-gaps--cell board r c))
               (gapp (null cell))
               (hintp (and gapp (member (cons r c) hints)))
               (str (cond ((not gapp) (card-games-gaps-card-string cell))
                          (hintp "+")
                          (t "·")))
               (faces nil))
          (when (card-games-gaps-red-p cell) (push 'card-games-red-suit faces))
          (when hintp (push 'card-games-hint faces))
          (when (and gapp (not hintp)) (push 'card-games-gap faces))
          (when (and (= r cr) (= c cc)) (push 'card-games-cursor faces))
          (let ((content (propertize (format "%3s" str)
                                     'face (or faces 'default))))
            (push (propertize (concat " " content)
                              'card-games-cell (cons r c)
                              'mouse-face 'highlight)
                  out))))
      (push "\n" out))
    (push (card-games-gaps--footer game) out)
    (apply #'concat (nreverse out))))

(defun card-games-gaps--board-specs (board)
  "Return BOARD as rows of SVG card specs for `card-games-svg-grid-svg'."
  (let ((rows nil))
    (dotimes (r 4)
      (let ((row nil))
        (dotimes (c 13)
          (let ((cell (card-games-gaps--cell board r c)))
            (push (and cell (cons (aref card-games-gaps-ranks (cdr cell)) (car cell)))
                  row)))
        (push (nreverse row) rows)))
    (nreverse rows)))

(defconst card-games-gaps--svg-card-w 46 "Base card width used by the SVG board.")
(defconst card-games-gaps--svg-card-h 64 "Base card height used by the SVG board.")
(defconst card-games-gaps--svg-gap 6 "Pixel gap between cards on the SVG board.")
(defconst card-games-gaps--svg-pad 10 "Margin around the SVG board.")

(defcustom card-games-gaps-svg-ui nil
  "Whether to render the gaps board as one full-buffer SVG.
When non-nil (and on a graphical display), the board fills the window with a
status/controls panel down the left side, mirroring the 500 full-SVG UI.
Toggle with `v'."
  :type 'boolean :group 'card-games-svg)

(defcustom card-games-gaps-svg-fill t
  "Whether the full-SVG gaps UI fills the window and re-fits on size changes.
Only used when `card-games-gaps-svg-ui' is enabled."
  :type 'boolean :group 'card-games-svg)

(defun card-games-gaps--insert-graphical (game)
  "Insert the GUI (SVG) depiction of GAME into the current buffer."
  (insert (card-games-gaps--header game))
  (let ((card-games-svg-card-width card-games-gaps--svg-card-w)
        (card-games-svg-card-height card-games-gaps--svg-card-h)
        (card-games-svg-card-gap card-games-gaps--svg-gap))
    (insert-image
     (card-games-svg-image
      (card-games-svg-grid-svg (card-games-gaps--board-specs (card-games-get game :board))
                       :cursor (card-games-get game :cursor)
                       :hints (card-games-gaps--hints game)
                       :pad card-games-gaps--svg-pad)
      (card-games-scale))))
  (insert "\n")
  (insert (card-games-gaps--footer game)))


;;;; Interaction

(defvar-local card-games-gaps--game nil
  "The `card-games-gaps-game' object played in the current buffer.")

(defun card-games-gaps--goto-cell (r c)
  "Move point onto the rendered cell at row R column C, if present."
  (let ((target (cons r c))
        (pos (point-min))
        (found nil))
    (while (and pos (not found))
      (when (equal (get-text-property pos 'card-games-cell) target)
        (setq found pos))
      (setq pos (next-single-property-change pos 'card-games-cell)))
    (when found (goto-char (1+ found)))))

(defun card-games-gaps--key-button (key word cmd help)
  "Insert a control where the KEY hint itself is the button running CMD.
Shown as \"KEY WORD\" (e.g. \"r redeal\"); HELP is the tooltip."
  (insert-text-button (format "%s %s" key word)
                      'action (lambda (_) (call-interactively cmd))
                      'help-echo help 'follow-link t 'face 'link)
  (insert "   "))

(defun card-games-gaps--insert-controls ()
  "Insert a single control line.
Movement keys are a plain hint; the action keys double as their own
buttons (the keyboard hint *is* the button)."
  (insert "  ")
  (insert (propertize "←→↑↓ move   " 'face 'shadow))
  (card-games-gaps--key-button "RET" "fill"   #'card-games-gaps-fill   "Fill the gap under the cursor")
  (card-games-gaps--key-button "r"   "redeal" #'card-games-gaps-redeal "Reshuffle the misplaced cards")
  (card-games-gaps--key-button "u"   "undo"   #'card-games-gaps-undo   "Undo the last move")
  (card-games-gaps--key-button "n"   "new"    #'card-games-gaps-new    "Deal a new game")
  (card-games-gaps--key-button "q"   "menu"   #'card-games-quit-to-menu "Back to the game list")
  (card-games-gaps--key-button "?"   "help"   #'card-games-gaps-help   "Show the rules and keys")
  (insert "\n"))

(cl-defmethod card-games-renderer-draw ((_renderer card-games-text-renderer) (game card-games-gaps-game))
  "Draw the Gaps GAME as UNICODE text with the control line."
  (insert (card-games-render game))
  (card-games-gaps--insert-controls))

(cl-defmethod card-games-renderer-draw ((_renderer card-games-svg-renderer) (game card-games-gaps-game))
  "Draw the Gaps GAME as an inline SVG board with the control line."
  (card-games-gaps--insert-graphical game)
  (card-games-gaps--insert-controls))

(cl-defmethod card-games-renderer-draw ((_renderer card-games-svg-fill-renderer) (game card-games-gaps-game))
  "Draw the Gaps GAME as a full-window SVG table."
  (card-games-gaps--insert-svg-ui game))

(defun card-games-gaps--treatment ()
  "Return the display treatment symbol for the current Gaps buffer.
Honours `card-games-gaps-svg-ui' and whether the display is graphical."
  (cond ((and card-games-gaps-svg-ui (display-graphic-p)) 'svg-fill)
        ((display-graphic-p) 'svg)
        (t 'text)))

(defun card-games-gaps--redisplay ()
  "Redraw the current Gaps buffer through its renderer.
The treatment is chosen by `card-games-gaps--treatment' and dispatched with
`card-games-renderer-draw'."
  (let* ((game card-games-gaps--game)
         (inhibit-read-only t)
         (renderer (card-games-render-set-treatment game (card-games-gaps--treatment))))
    (setq-local mode-line-process (card-games-gaps--mode-line game))
    (erase-buffer)
    (card-games-renderer-draw renderer game)
    (if (display-graphic-p)
        (goto-char (point-min))
      (let ((cur (card-games-get game :cursor)))
        (card-games-gaps--goto-cell (car cur) (cdr cur))))))

(defun card-games-gaps--move (dr dc)
  "Move the cursor by DR rows and DC columns, then redisplay."
  (let* ((game card-games-gaps--game)
         (cur (card-games-get game :cursor))
         (r (min 3 (max 0 (+ (car cur) dr))))
         (c (min 12 (max 0 (+ (cdr cur) dc)))))
    (card-games-put game :cursor (cons r c))
    (card-games-gaps--redisplay)))

(defun card-games-gaps-left ()  "Move cursor left."  (interactive) (card-games-gaps--move 0 -1))
(defun card-games-gaps-right () "Move cursor right." (interactive) (card-games-gaps--move 0  1))
(defun card-games-gaps-up ()    "Move cursor up."    (interactive) (card-games-gaps--move -1 0))
(defun card-games-gaps-down ()  "Move cursor down."  (interactive) (card-games-gaps--move  1 0))

(defun card-games-gaps--after-move ()
  "Check for a win or a stuck position and report it."
  (let ((game card-games-gaps--game))
    (cond
     ((card-games-won-p game)
      (card-games-put game :message
              (format "\U0001F389 Solved in %d moves!  Press n for a new game."
                      (card-games-get game :moves))))
     ((card-games-gaps--stuck-p game)
      (card-games-put game :message
              (if (> (card-games-get game :redeals) 0)
                  (format "Stuck!  Press r to redeal (%d left)."
                          (card-games-get game :redeals))
                "Stuck, and no redeals left.  Press n for a new game."))))
    (card-games-gaps--redisplay)
    (message "%s" (card-games-get game :message))))

(defun card-games-gaps-fill ()
  "Fill the gap under the cursor."
  (interactive)
  (let* ((game card-games-gaps--game)
         (cur (card-games-get game :cursor)))
    (if (card-games-gaps--fill game (car cur) (cdr cur))
        (card-games-gaps--after-move)
      (card-games-gaps--redisplay))))

(defun card-games-gaps-redeal ()
  "Reshuffle and redeal the misplaced cards, if redeals remain."
  (interactive)
  (let ((game card-games-gaps--game))
    (if (<= (card-games-get game :redeals) 0)
        (progn (card-games-put game :message "No redeals left.")
               (card-games-gaps--redisplay))
      (card-games-gaps--save-undo game)
      (card-games-gaps--do-redeal game)
      (card-games-put game :redeals (1- (card-games-get game :redeals)))
      (card-games-put game :message
              (format "Redealt.  %d redeals left." (card-games-get game :redeals)))
      (card-games-gaps--after-move))))

(defun card-games-gaps-undo ()
  "Undo the last move or redeal."
  (interactive)
  (let* ((game card-games-gaps--game)
         (hist (card-games-get game :history)))
    (if (null hist)
        (progn (card-games-put game :message "Nothing to undo.")
               (card-games-gaps--redisplay))
      (let ((snap (car hist)))
        (card-games-put game :board   (nth 0 snap))
        (card-games-put game :moves   (nth 1 snap))
        (card-games-put game :redeals (nth 2 snap))
        (card-games-put game :history (cdr hist))
        (card-games-put game :message "Undone.")
        (card-games-gaps--redisplay)))))

(defun card-games-gaps-new ()
  "Start a new game in the current buffer."
  (interactive)
  (card-games-gaps--deal card-games-gaps--game)
  (card-games-gaps--redisplay))

(defun card-games-gaps--xy->cell (px py)
  "Map pixel coordinates PX, PY on the SVG board to a (ROW . COL), or nil."
  (let* ((w card-games-gaps--svg-card-w) (h card-games-gaps--svg-card-h)
         (g card-games-gaps--svg-gap) (pad card-games-gaps--svg-pad))
    (when (and (>= px pad) (>= py pad))
      (let* ((col (/ (- px pad) (+ w g)))
             (row (/ (- py pad) (+ h g)))
             (xin (- px pad (* col (+ w g))))
             (yin (- py pad (* row (+ h g)))))
        (when (and (< col 13) (< row 4) (<= xin w) (<= yin h))
          (cons row col))))))

(defun card-games-gaps-mouse (event)
  "Fill the gap clicked by EVENT (or move the cursor there).
Dispatches to the full-SVG UI when active; otherwise hit-tests the inline
SVG board (pixel) or the text grid (text property)."
  (interactive "e")
  (let ((start (event-start event)))
    (if (and card-games-gaps-svg-ui (display-graphic-p) (posn-image start))
        (card-games-gaps--svg-ui-click start)
      (let ((cell (if (and (display-graphic-p) (posn-image start))
                      (let ((xy (posn-object-x-y start)) (s (card-games-scale)))
                        (and xy (card-games-gaps--xy->cell (round (/ (car xy) s))
                                                   (round (/ (cdr xy) s)))))
                    (let ((pos (posn-point start)))
                      (and pos (get-text-property pos 'card-games-cell))))))
        (when cell
          (card-games-put card-games-gaps--game :cursor cell)
          (card-games-gaps-fill))))))

(defun card-games-gaps-help ()
  "Show a one-line reminder of the controls."
  (interactive)
  (let ((game card-games-gaps--game))
    (message "%s"
             (format "%s: move to a highlighted gap and RET to fill it (a %s anchors the head). r=redeal u=undo n=new q=quit."
                     (if game (card-games-gaps--vname game) "Gaps")
                     (if game (aref card-games-gaps-rank-names (card-games-gaps--head game)) "card")))))

(defun card-games-gaps--mode-line (game)
  "Return the mode-line status string for GAME."
  (cond ((card-games-won-p game) " [Solved!]")
        ((card-games-gaps--stuck-p game)
         (if (> (card-games-get game :redeals) 0) " [Stuck — r to redeal]" " [Stuck]"))
        (t (format " [moves %d · redeals %d]"
                   (card-games-get game :moves) (card-games-get game :redeals)))))

(defun card-games-gaps-zoom-in ()
  "Enlarge the cards."  (interactive) (text-scale-increase 1) (card-games-gaps--redisplay))
(defun card-games-gaps-zoom-out ()
  "Shrink the cards."   (interactive) (text-scale-decrease 1) (card-games-gaps--redisplay))
(defun card-games-gaps-zoom-reset ()
  "Reset the card size." (interactive) (text-scale-set 0) (card-games-gaps--redisplay))

(defun card-games-gaps-redraw ()
  "Redraw the board (e.g. after a theme or frame change)."
  (interactive)
  (card-games-gaps--redisplay))


;;;; Frameless full-SVG UI (opt-in; see `card-games-gaps-svg-ui')

(defconst card-games-gaps--ui-w 820 "Default full-SVG gaps canvas width.")
(defconst card-games-gaps--ui-h 380 "Default full-SVG gaps canvas height.")

(defvar-local card-games-gaps--regions nil
  "Plist of clickable regions for the full-SVG gaps UI.")
(defvar-local card-games-gaps--ui-last-size nil
  "Last window pixel size used to render the full-SVG gaps UI.")

(defun card-games-gaps--in-rect (px py rect)
  "Return non-nil when PX,PY lie inside RECT (X Y W H)."
  (and rect (>= px (nth 0 rect)) (< px (+ (nth 0 rect) (nth 2 rect)))
       (>= py (nth 1 rect)) (< py (+ (nth 1 rect) (nth 3 rect)))))

(defun card-games-gaps--ui-text (svg str x y size color &optional bold anchor)
  "Draw text STR on SVG at X,Y (SIZE, COLOR); ANCHOR defaults to start."
  (let ((a (list :x (round x) :y (round y) :font-size (round size)
                 :fill color :text-anchor (or anchor "start")
                 :font-family card-games-svg-font-family)))
    (when bold (setq a (append a (list :font-weight "bold"))))
    (apply #'svg-text svg str a)))

(defun card-games-gaps--ui-label (svg str x y size)
  "Draw STR as an all-caps, letter-spaced section label on SVG at X, Y, SIZE."
  (svg-text svg (upcase str) :x (round x) :y (round y) :font-size (round size)
            :fill "#8fc79b" :text-anchor "start" :font-family card-games-svg-font-family
            :font-weight "bold" :letter-spacing "2"))

(defun card-games-gaps--ui-divider (svg x1 x2 y)
  "Draw a faint horizontal divider on SVG from X1 to X2 at height Y."
  (svg-line svg x1 y x2 y :stroke "#1b6b35" :stroke-width 1))

(defun card-games-gaps--draw-panel (svg game h lpw fs)
  "Draw GAME's left status/controls panel on SVG (height H, width LPW, scale FS).
Return a plist of clickable control regions."
  (let* ((regions nil)
         (F (lambda (n) (round (* n fs))))
         (px0 (funcall F 14)) (pxr (- lpw (funcall F 12)))
         (dl (funcall F 8)) (dr (- lpw (funcall F 8)))
         (cxp (/ lpw 2)) (y 0))
    (svg-rectangle svg 6 6 (- lpw 8) (- h 12) :rx 10 :fill "#0d4a22" :fill-opacity 0.9
                   :stroke "#0a3a1a" :stroke-width 1)
    (setq y (funcall F 30))
    (card-games-svg--text svg (card-games-gaps--vname game) cxp y (funcall F 15) "#f1c40f" t)
    (setq y (+ y (funcall F 14))) (card-games-gaps--ui-divider svg dl dr y)
    ;; stats
    (setq y (+ y (funcall F 22)))
    (card-games-gaps--ui-text svg "Moves" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (card-games-get game :moves)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family card-games-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 20)))
    (card-games-gaps--ui-text svg "Redeals left" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (card-games-get game :redeals)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family card-games-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 16))) (card-games-gaps--ui-divider svg dl dr y)
    ;; rules
    (setq y (+ y (funcall F 20)))
    (card-games-gaps--ui-label svg "Rules" px0 (- y (funcall F 6)) (funcall F 10))
    (setq y (+ y (funcall F 16)))
    (card-games-gaps--ui-text svg (format "Head: %s"
                                  (aref card-games-gaps-rank-names (card-games-gaps--head game)))
                      px0 y (funcall F 12) "#cfeccf")
    (setq y (+ y (funcall F 16)))
    (card-games-gaps--ui-text svg (if (> (card-games-gaps--step game) 0) "Build up  2..K"
                            "Build down  K..2")
                      px0 y (funcall F 12) "#cfeccf")
    (setq y (+ y (funcall F 16)) )
    (card-games-gaps--ui-text svg "One suit per row" px0 y (funcall F 12) "#9fd0a8")
    (setq y (+ y (funcall F 14))) (card-games-gaps--ui-divider svg dl dr y)
    ;; controls: the key shown on each button is the keyboard shortcut
    (setq y (+ y (funcall F 20)))
    (let* ((bw (- lpw px0 (funcall F 12))) (bh (funcall F 26)) (bg (funcall F 8))
           (canredeal (> (card-games-get game :redeals) 0))
           (canundo (and (card-games-get game :history) t))
           (defs (list (list :redeal "R" "Redeal" canredeal)
                       (list :undo   "U" "Undo"   canundo)
                       (list :new    "N" "New"    t)
                       (list :help   "?" "Help"   t))))
      (dolist (d defs)
        (let* ((key (nth 0 d)) (kc (nth 1 d)) (word (nth 2 d)) (on (nth 3 d))
               (rect (list px0 y bw bh)))
          (svg-rectangle svg px0 y bw bh :rx 6
                         :fill (if on "#14401f" "#0e2a15")
                         :fill-opacity (if on 0.9 0.5)
                         :stroke "#0a3a1a" :stroke-width 1)
          (card-games-gaps--ui-text svg kc (+ px0 (funcall F 10)) (+ y (round (* bh 0.68)))
                            (funcall F 13) (if on "#f1c40f" "#5f7f68") t)
          (card-games-gaps--ui-text svg word (+ px0 (funcall F 30)) (+ y (round (* bh 0.68)))
                            (funcall F 13) (if on "#eaffea" "#5f7f68"))
          (setq regions (plist-put regions key rect))
          (setq y (+ y bh bg)))))
    ;; status message, wrapped to the panel
    (let ((msg (card-games-get game :message)))
      (when (and msg (> (length msg) 0))
        (let ((m (if (> (length msg) (max 18 (round (/ (- lpw px0 (funcall F 12))
                                                       (* 0.55 (funcall F 11))))))
                     (substring msg 0 (max 18 (round (/ (- lpw px0 (funcall F 12))
                                                        (* 0.55 (funcall F 11))))))
                   msg)))
          (card-games-gaps--ui-text svg m px0 (- h (funcall F 14)) (funcall F 11) "#9fd0a8"))))
    regions))

(defun card-games-gaps--ui-svg (game &optional w h)
  "Return (SVG . REGIONS) for the full-buffer gaps UI of GAME (W by H).
The board scales to fill the area beside a proportional left panel."
  (let* ((W (or w card-games-gaps--ui-w)) (H (or h card-games-gaps--ui-h))
         (svg (svg-create W H)) (regions nil)
         (fs (max 1.0 (min 2.2 (/ (+ (/ (float W) card-games-gaps--ui-w)
                                     (/ (float H) card-games-gaps--ui-h)) 2.0))))
         (pscale (max 1.0 (min 1.7 (/ (float W) card-games-gaps--ui-w))))
         (lpw (round (* 190 pscale)))
         (bx (+ lpw 14)) (by 10)
         (aw (- W bx 14)) (ah (- H by 10))
         (board (card-games-get game :board))
         (cur (card-games-get game :cursor)) (cr (car cur)) (cc (cdr cur))
         (hints (card-games-gaps--hints game))
         (g (max 4 (round (* 6 fs))))
         (cww (/ (- aw (* 12 g)) 13))
         (chh (/ (- ah (* 3 g)) 4))
         (aspect (/ 64.0 46.0))
         (cw (max 18 (min cww (round (/ chh aspect)))))
         (ch (round (* cw aspect)))
         (bw (+ (* 13 cw) (* 12 g)))
         (bh (+ (* 4 ch) (* 3 g)))
         (x0 (+ bx (max 0 (/ (- aw bw) 2))))
         (y0 (+ by (max 0 (/ (- ah bh) 2)))))
    ;; felt background + play-area panel
    (svg-gradient svg "card-games-gfelt" 'radial '((0 . "#1a7a38") (100 . "#0c4720")))
    (svg-rectangle svg 0 0 W H :rx 14 :gradient "card-games-gfelt")
    (svg-rectangle svg (- bx 6) by (+ aw 12) ah :rx 12
                   :fill "#000000" :fill-opacity 0.10
                   :stroke "#0e5226" :stroke-width 2)
    ;; board
    (let ((card-games-svg-card-width cw) (card-games-svg-card-height ch))
      (dotimes (r 4)
        (dotimes (c 13)
          (let* ((cell (card-games-gaps--cell board r c))
                 (spec (and cell (cons (aref card-games-gaps-ranks (cdr cell)) (car cell))))
                 (x (+ x0 (* c (+ cw g)))) (y (+ y0 (* r (+ ch g))))
                 (hl (and (= r cr) (= c cc)))
                 (hint (and (null cell) (member (cons r c) hints) t)))
            (card-games-svg--draw-spec svg x y spec hl hint)))))
    (setq regions (plist-put regions :board (list x0 y0 cw ch g)))
    (setq regions (append regions (card-games-gaps--draw-panel svg game H lpw fs)))
    (cons svg regions)))

(defun card-games-gaps--ui-cell (px py geom)
  "Map pixel PX,PY to a (ROW . COL) given board GEOM (X0 Y0 CW CH G), or nil."
  (when geom
    (let ((x0 (nth 0 geom)) (y0 (nth 1 geom)) (cw (nth 2 geom))
          (ch (nth 3 geom)) (g (nth 4 geom)))
      (when (and (>= px x0) (>= py y0))
        (let* ((col (/ (- px x0) (+ cw g))) (row (/ (- py y0) (+ ch g)))
               (xin (- px x0 (* col (+ cw g)))) (yin (- py y0 (* row (+ ch g)))))
          (when (and (< col 13) (< row 4) (<= xin cw) (<= yin ch))
            (cons row col)))))))

(defun card-games-gaps--svg-ui-click (start)
  "Dispatch a click at posn START within the full-SVG gaps UI."
  (let* ((xy (posn-object-x-y start)) (s (card-games-scale))
         (px (round (/ (car xy) s))) (py (round (/ (cdr xy) s)))
         (game card-games-gaps--game) (rg card-games-gaps--regions))
    (cond
     ((card-games-gaps--in-rect px py (plist-get rg :redeal)) (card-games-gaps-redeal))
     ((card-games-gaps--in-rect px py (plist-get rg :undo)) (card-games-gaps-undo))
     ((card-games-gaps--in-rect px py (plist-get rg :new)) (card-games-gaps-new))
     ((card-games-gaps--in-rect px py (plist-get rg :help)) (card-games-gaps-help))
     (t (let ((cell (card-games-gaps--ui-cell px py (plist-get rg :board))))
          (when cell (card-games-put game :cursor cell) (card-games-gaps-fill)))))))

(defun card-games-gaps--insert-svg-ui (game)
  "Insert the full-buffer SVG gaps UI for GAME and record its regions."
  (let* ((win (get-buffer-window (current-buffer)))
         (fill (and card-games-gaps-svg-fill win))
         (w (if fill (max 640 (window-body-width win t)) card-games-gaps--ui-w))
         (h (if fill (max 320 (- (window-body-height win t) 4)) card-games-gaps--ui-h))
         (sr (card-games-gaps--ui-svg game w h)))
    (when fill (setq card-games-gaps--ui-last-size (cons (window-body-width win t)
                                                 (window-body-height win t))))
    (setq card-games-gaps--regions (cdr sr))
    (insert-image (card-games-svg-image (car sr) (if fill 1.0 (card-games-scale))))))

(defun card-games-gaps--fit (&rest _)
  "Re-render the full-SVG gaps UI to fit the window after a config change."
  (when (and card-games-gaps--game card-games-gaps-svg-ui card-games-gaps-svg-fill
             (eq major-mode 'card-games-gaps-mode))
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (let ((sz (cons (window-body-width win t) (window-body-height win t))))
          (unless (equal sz card-games-gaps--ui-last-size)
            (setq card-games-gaps--ui-last-size sz)
            (card-games-gaps--redisplay)))))))

(defun card-games-gaps-toggle-svg-ui ()
  "Toggle the full-buffer SVG board for the gaps games."
  (interactive)
  (setq card-games-gaps-svg-ui (not card-games-gaps-svg-ui))
  (setq card-games-gaps--ui-last-size nil)
  (card-games-gaps--redisplay)
  (message "Full-SVG board %s" (if card-games-gaps-svg-ui "enabled" "disabled")))

(defvar card-games-gaps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'card-games-gaps-left)
    (define-key map (kbd "<right>") #'card-games-gaps-right)
    (define-key map (kbd "<up>")    #'card-games-gaps-up)
    (define-key map (kbd "<down>")  #'card-games-gaps-down)
    (define-key map (kbd "RET") #'card-games-gaps-fill)
    (define-key map "g" #'card-games-gaps-redraw)
    (define-key map "r" #'card-games-gaps-redeal)
    (define-key map "u" #'card-games-gaps-undo)
    (define-key map "n" #'card-games-gaps-new)
    (define-key map "?" #'card-games-gaps-help)
    (define-key map "+" #'card-games-gaps-zoom-in)
    (define-key map "=" #'card-games-gaps-zoom-in)
    (define-key map "-" #'card-games-gaps-zoom-out)
    (define-key map "0" #'card-games-gaps-zoom-reset)
    (define-key map "v" #'card-games-gaps-toggle-svg-ui)
    (define-key map [mouse-1] #'card-games-gaps-mouse)
    (define-key map "q" #'card-games-quit-to-menu)
    map)
  "Keymap for `card-games-gaps-mode' (Emacs style; see `card-games-keys').")

(defun card-games-gaps--classic-keymap ()
  "Return a copy of `card-games-gaps-mode-map' with vi-style hjkl and SPC added."
  (let ((map (copy-keymap card-games-gaps-mode-map)))
    (define-key map "h" #'card-games-gaps-left)
    (define-key map "l" #'card-games-gaps-right)
    (define-key map "k" #'card-games-gaps-up)
    (define-key map "j" #'card-games-gaps-down)
    (define-key map (kbd "SPC") #'card-games-gaps-fill)
    map))

(define-derived-mode card-games-gaps-mode special-mode "Gaps"
  "Major mode for playing the gaps family of solitaires."
  (setq-local cursor-type card-games-cursor-type)
  (setq-local truncate-lines t)
  (add-hook 'window-configuration-change-hook #'card-games-gaps--fit nil t)
  (when (eq card-games-keys 'classic)
    (use-local-map (card-games-gaps--classic-keymap))))

(defun card-games-gaps--play (class)
  "Start a gaps-style game of CLASS in its own buffer."
  (let* ((game (card-games-gaps--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (card-games-gaps--vname game)))))
    (with-current-buffer buf
      (card-games-gaps-mode)
      (setq card-games-gaps--game game)
      (card-games-gaps--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun card-games-montana ()
  "Play Gaps / Montana solitaire (Two at the head, build up 2..K)."
  (interactive)
  (card-games-gaps--play 'card-games-montana-game))

;;;###autoload
(defun card-games-hells-half-acre ()
  "Play Hell's Half-Acre solitaire (King at the head, build down K..2)."
  (interactive)
  (card-games-gaps--play 'card-games-acre-game))

;;;###autoload
(defalias 'card-games-gaps #'card-games-montana
  "Alias for `card-games-montana'.")

(provide 'card-games-gaps)
;;; card-games-gaps.el ends here
