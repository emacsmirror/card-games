;;; cg-gaps.el --- Gaps-style row solitaires (Montana, Hell's Half-Acre)  -*- lexical-binding: t; -*-

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

;; The "gaps" family of solitaires: 48 cards dealt into four rows of
;; thirteen with four gaps.  A gap is filled by the card one rank along
;; from the card to its left, of the same suit; the head gap of a row
;; takes the anchor rank in any suit.
;;
;; Two variants ship, demonstrating how a new game is *derived* by
;; subclassing the abstract `cg-gaps-game' and overriding two methods
;; (`cg-gaps--head' and `cg-gaps--step'):
;;
;;   `cg-montana'          Gaps / Montana: Two at the head, rows build
;;                         UP  2 3 4 ... K; nothing follows a King.
;;   `cg-hells-half-acre'  Hell's Half-Acre: King at the head, rows
;;                         build DOWN K Q J ... 2; nothing follows a Two.
;;
;; When stuck you may redeal (twice): each correct run from the head
;; stays, a gap opens just past it, and the rest are reshuffled.
;;
;; Renders as UNICODE text in a terminal and as SVG cards on a graphical
;; display; fillable gaps are highlighted.  Play via `M-x card-game' or
;; the commands above.

;;; Code:

(require 'cg-core)
(require 'cg-svg)
(require 'cg-render)


;;;; Cards

(defconst cg-gaps-ranks
  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"]
  "Rank labels indexed 0..11 (Two through King).")

(defconst cg-gaps-rank-names
  ["Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine"
   "Ten" "Jack" "Queen" "King"]
  "Full rank names indexed to match `cg-gaps-ranks'.")

;; A card is a cons cell (SUIT . RANK); nil denotes a gap.

(defsubst cg-gaps-red-p (card)
  "Return non-nil when CARD is a red suit (diamonds or hearts)."
  (and card (cg-red-suit-p (car card))))

(defun cg-gaps-card-string (card)
  "Return a short string for CARD, or a dot for a gap (nil)."
  (if (null card)
      "·"
    (concat (aref cg-gaps-ranks (cdr card))
            (cg-suit-glyph (car card)))))


;;;; Games — an abstract base and two subclasses

(defclass cg-gaps-game (cg-game)
  ((name :initform "Gaps"))
  "Abstract base for gaps-style row solitaires.
Subclasses set the head rank and build direction by overriding
`cg-gaps--head' and `cg-gaps--step'."
  :abstract t)

(cl-defgeneric cg-gaps--head (game)
  "Return the rank index that anchors the head (left) of each row.")
(cl-defgeneric cg-gaps--step (game)
  "Return the per-column rank increment: +1 ascending, -1 descending.")
(cl-defgeneric cg-gaps--vname (game)
  "Return the human-readable variant name for GAME.")

(defclass cg-montana-game (cg-gaps-game)
  ((name :initform "Montana"))
  "Gaps / Montana: a Two anchors the head; rows build up 2..K.")
(cl-defmethod cg-gaps--head ((_ cg-montana-game)) 0)
(cl-defmethod cg-gaps--step ((_ cg-montana-game)) 1)
(cl-defmethod cg-gaps--vname ((_ cg-montana-game)) "Gaps (Montana)")

(defclass cg-acre-game (cg-gaps-game)
  ((name :initform "Hell's Half-Acre"))
  "Hell's Half-Acre: a King anchors the head; rows build down K..2.")
(cl-defmethod cg-gaps--head ((_ cg-acre-game)) 11)
(cl-defmethod cg-gaps--step ((_ cg-acre-game)) -1)
(cl-defmethod cg-gaps--vname ((_ cg-acre-game)) "Hell's Half-Acre")

(defalias 'cg-gaps--shuffle 'cg-shuffle)

(defun cg-gaps--full-deck ()
  "Return the 48 playable cards (Two..King in every suit)."
  (cl-loop for s below 4
           append (cl-loop for r below 12 collect (cons s r))))

(cl-defmethod cg-gaps--deal ((game cg-gaps-game))
  "Deal a fresh layout into GAME."
  (random t)
  (let ((cells (cg-gaps--shuffle (append (cg-gaps--full-deck)
                                         (make-list 4 nil))))
        (board (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 13)
          (aset row c (pop cells)))
        (aset board r row)))
    (cg-put game :board board)
    (cg-put game :moves 0)
    (cg-put game :redeals 2)
    (cg-put game :cursor (cons 0 0))
    (cg-put game :history nil)
    (cg-put game :message
            (format "Fill the gaps: each row one suit, %s.  RET on a gap.  ? = help."
                    (if (> (cg-gaps--step game) 0) "2 up to K" "K down to 2")))
    game))

(defun cg-gaps--cell (board r c)
  "Return the card at row R column C of BOARD (nil for a gap)."
  (aref (aref board r) c))

(cl-defmethod cg-gaps--needed ((game cg-gaps-game) board r c)
  "Return what may fill the gap at R, C of BOARD for GAME.
Returns the symbol `head' for a head gap, a (SUIT . RANK) card for any
other fillable gap, or nil if nothing fits."
  (if (= c 0)
      'head
    (let ((left (cg-gaps--cell board r (1- c))))
      (if (null left)
          nil
        (let ((nr (+ (cdr left) (cg-gaps--step game))))
          (and (>= nr 0) (<= nr 11) (cons (car left) nr)))))))

(defun cg-gaps--find (board card)
  "Return (ROW . COL) of CARD in BOARD, or nil if absent."
  (catch 'hit
    (dotimes (r 4)
      (dotimes (c 13)
        (when (equal (cg-gaps--cell board r c) card)
          (throw 'hit (cons r c)))))
    nil))

(defun cg-gaps--copy-board (board)
  "Return a shallow copy of BOARD safe to mutate cell-by-cell."
  (apply #'vector (mapcar #'copy-sequence (append board nil))))

(cl-defmethod cg-gaps--save-undo ((game cg-gaps-game))
  "Push the current state of GAME onto its undo history."
  (cg-put game :history
          (cons (list (cg-gaps--copy-board (cg-get game :board))
                      (cg-get game :moves)
                      (cg-get game :redeals))
                (cg-get game :history))))

(cl-defmethod cg-gaps--do-move ((game cg-gaps-game) r c card)
  "Move CARD into the gap at R, C of GAME.  Return non-nil on success."
  (let* ((board (cg-get game :board))
         (loc (cg-gaps--find board card)))
    (if (not loc)
        (progn (cg-put game :message
                       (format "The %s is not on the board?!"
                               (cg-gaps-card-string card)))
               nil)
      (cg-gaps--save-undo game)
      (setf (aref (aref board (car loc)) (cdr loc)) nil)
      (setf (aref (aref board r) c) card)
      (cg-put game :moves (1+ (cg-get game :moves)))
      (cg-put game :message (format "Moved %s." (cg-gaps-card-string card)))
      t)))

(cl-defmethod cg-gaps--fill ((game cg-gaps-game) r c)
  "Try to fill the gap at R, C of GAME.  Return non-nil on success."
  (let* ((board (cg-get game :board))
         (cell (cg-gaps--cell board r c)))
    (cond
     (cell
      (cg-put game :message "That cell is not a gap.") nil)
     (t
      (let ((needed (cg-gaps--needed game board r c)))
        (cond
         ((null needed)
          (cg-put game :message "Nothing can fill that gap.") nil)
         ((eq needed 'head)
          (let ((suit (cg-gaps--read-head game)))
            (and suit (cg-gaps--do-move game r c (cons suit (cg-gaps--head game))))))
         (t
          (cg-gaps--do-move game r c needed))))))))

(cl-defmethod cg-gaps--read-head ((game cg-gaps-game))
  "Prompt for the suit of the head card of GAME.  Return suit 0..3 or nil."
  (let* ((name (aref cg-gaps-rank-names (cg-gaps--head game)))
         (ch (read-char-choice
              (format "Head gap — which %s? [s]pades [c]lubs [d]iamonds [h]earts (q=cancel): "
                      name)
              '(?s ?c ?d ?h ?q))))
    (cdr (assq ch '((?s . 0) (?c . 1) (?d . 2) (?h . 3))))))

(cl-defmethod cg-won-p ((game cg-gaps-game))
  "Return non-nil when every row of GAME is a full suited run with a trailing gap."
  (let ((board (cg-get game :board))
        (head (cg-gaps--head game))
        (step (cg-gaps--step game)))
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

(cl-defmethod cg-gaps--stuck-p ((game cg-gaps-game))
  "Return non-nil when no gap of GAME can currently be filled."
  (null (cg-gaps--hints game)))

(cl-defmethod cg-gaps--hints ((game cg-gaps-game))
  "Return the list of (ROW . COL) gaps of GAME that can be filled now."
  (let ((board (cg-get game :board))
        (hints nil))
    (dotimes (r 4)
      (dotimes (c 13)
        (when (and (null (cg-gaps--cell board r c))
                   (cg-gaps--needed game board r c))
          (push (cons r c) hints))))
    hints))

(cl-defmethod cg-gaps--prefix-len ((game cg-gaps-game) board r)
  "Return the length of the correct run at the head of row R of BOARD."
  (let ((row (aref board r))
        (head (cg-gaps--head game))
        (step (cg-gaps--step game))
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

(cl-defmethod cg-gaps--do-redeal ((game cg-gaps-game))
  "Gather misplaced cards of GAME, reshuffle, and lay them back."
  (let* ((board (cg-get game :board))
         (lens (make-vector 4 0))
         (kept nil))
    (dotimes (r 4)
      (let ((len (cg-gaps--prefix-len game board r)))
        (aset lens r len)
        (dotimes (i len) (push (cg-gaps--cell board r i) kept))))
    (let* ((remaining
            (cg-gaps--shuffle
             (cl-remove-if (lambda (card) (cl-member card kept :test #'equal))
                           (cg-gaps--full-deck))))
           (new (make-vector 4 nil)))
      (dotimes (r 4)
        (let ((row (make-vector 13 nil))
              (len (aref lens r)))
          (dotimes (i len)
            (aset row i (cg-gaps--cell board r i)))
          ;; column LEN stays a gap.
          (cl-loop for c from (1+ len) below 13
                   do (aset row c (pop remaining)))
          (aset new r row)))
      (cg-put game :board new))))


;;;; Rendering

(defun cg-gaps--header (game)
  "Return the header text for GAME."
  (format "  ♠♣ %s ♦♥\n  Moves: %-4d  Redeals left: %d\n\n"
          (cg-gaps--vname game) (cg-get game :moves) (cg-get game :redeals)))

(defun cg-gaps--footer (game)
  "Return the footer text (just the current message) for GAME.
The control line is inserted separately by `cg-gaps--insert-controls',
where each key hint is itself the clickable button."
  (format "\n  %s\n" (cg-get game :message)))

(cl-defmethod cg-render ((game cg-gaps-game))
  "Return a propertized string depicting GAME (console rendering)."
  (let* ((board (cg-get game :board))
         (cursor (cg-get game :cursor))
         (cr (car cursor))
         (cc (cdr cursor))
         (hints (cg-gaps--hints game))
         (out (list)))
    (push (cg-gaps--header game) out)
    (dotimes (r 4)
      (dotimes (c 13)
        (let* ((cell (cg-gaps--cell board r c))
               (gapp (null cell))
               (hintp (and gapp (member (cons r c) hints)))
               (str (cond ((not gapp) (cg-gaps-card-string cell))
                          (hintp "+")
                          (t "·")))
               (faces nil))
          (when (cg-gaps-red-p cell) (push 'cg-red-suit faces))
          (when hintp (push 'cg-hint faces))
          (when (and gapp (not hintp)) (push 'cg-gap faces))
          (when (and (= r cr) (= c cc)) (push 'cg-cursor faces))
          (let ((content (propertize (format "%3s" str)
                                     'face (or faces 'default))))
            (push (propertize (concat " " content)
                              'cg-cell (cons r c)
                              'mouse-face 'highlight)
                  out))))
      (push "\n" out))
    (push (cg-gaps--footer game) out)
    (apply #'concat (nreverse out))))

(defun cg-gaps--board-specs (board)
  "Return BOARD as rows of SVG card specs for `cg-svg-grid-svg'."
  (let ((rows nil))
    (dotimes (r 4)
      (let ((row nil))
        (dotimes (c 13)
          (let ((cell (cg-gaps--cell board r c)))
            (push (and cell (cons (aref cg-gaps-ranks (cdr cell)) (car cell)))
                  row)))
        (push (nreverse row) rows)))
    (nreverse rows)))

(defconst cg-gaps--svg-card-w 46 "Base card width used by the SVG board.")
(defconst cg-gaps--svg-card-h 64 "Base card height used by the SVG board.")
(defconst cg-gaps--svg-gap 6 "Pixel gap between cards on the SVG board.")
(defconst cg-gaps--svg-pad 10 "Margin around the SVG board.")

(defcustom cg-gaps-svg-ui nil
  "When non-nil (and on a graphical display), render the gaps board as a
single full-buffer SVG: the board fills the window with a status/controls
panel down the left side, mirroring the 500 full-SVG UI.  Toggle with `v'."
  :type 'boolean :group 'cg-svg)

(defcustom cg-gaps-svg-fill t
  "When non-nil, size the full-SVG gaps UI to fill the window and re-fit on
window changes.  Only used when `cg-gaps-svg-ui' is enabled."
  :type 'boolean :group 'cg-svg)

(defun cg-gaps--insert-graphical (game)
  "Insert the GUI (SVG) depiction of GAME into the current buffer."
  (insert (cg-gaps--header game))
  (let ((cg-svg-card-width cg-gaps--svg-card-w)
        (cg-svg-card-height cg-gaps--svg-card-h)
        (cg-svg-card-gap cg-gaps--svg-gap))
    (insert-image
     (cg-svg-image
      (cg-svg-grid-svg (cg-gaps--board-specs (cg-get game :board))
                       :cursor (cg-get game :cursor)
                       :hints (cg-gaps--hints game)
                       :pad cg-gaps--svg-pad)
      (cg-scale))))
  (insert "\n")
  (insert (cg-gaps--footer game)))


;;;; Interaction

(defvar-local cg-gaps--game nil
  "The `cg-gaps-game' object played in the current buffer.")

(defun cg-gaps--goto-cell (r c)
  "Move point onto the rendered cell at row R column C, if present."
  (let ((target (cons r c))
        (pos (point-min))
        (found nil))
    (while (and pos (not found))
      (when (equal (get-text-property pos 'cg-cell) target)
        (setq found pos))
      (setq pos (next-single-property-change pos 'cg-cell)))
    (when found (goto-char (1+ found)))))

(defun cg-gaps--key-button (key word cmd help)
  "Insert a control where the KEY hint itself is the button running CMD.
Shown as \"key word\" (e.g. \"r redeal\"); HELP is the tooltip."
  (insert-text-button (format "%s %s" key word)
                      'action (lambda (_) (call-interactively cmd))
                      'help-echo help 'follow-link t 'face 'link)
  (insert "   "))

(defun cg-gaps--insert-controls ()
  "Insert a single control line.
Movement keys are a plain hint; the action keys double as their own
buttons (the keyboard hint *is* the button)."
  (insert "  ")
  (insert (propertize "←→↑↓ move   " 'face 'shadow))
  (cg-gaps--key-button "RET" "fill"   #'cg-gaps-fill   "Fill the gap under the cursor")
  (cg-gaps--key-button "r"   "redeal" #'cg-gaps-redeal "Reshuffle the misplaced cards")
  (cg-gaps--key-button "u"   "undo"   #'cg-gaps-undo   "Undo the last move")
  (cg-gaps--key-button "n"   "new"    #'cg-gaps-new    "Deal a new game")
  (cg-gaps--key-button "?"   "help"   #'cg-gaps-help   "Show the rules and keys")
  (insert "\n"))

(cl-defmethod cg-renderer-draw ((_renderer cg-text-renderer) (game cg-gaps-game))
  "Draw the Gaps GAME as UNICODE text with the control line."
  (insert (cg-render game))
  (cg-gaps--insert-controls))

(cl-defmethod cg-renderer-draw ((_renderer cg-svg-renderer) (game cg-gaps-game))
  "Draw the Gaps GAME as an inline SVG board with the control line."
  (cg-gaps--insert-graphical game)
  (cg-gaps--insert-controls))

(cl-defmethod cg-renderer-draw ((_renderer cg-svg-fill-renderer) (game cg-gaps-game))
  "Draw the Gaps GAME as a full-window SVG table."
  (cg-gaps--insert-svg-ui game))

(defun cg-gaps--treatment ()
  "Return the display treatment symbol for the current Gaps buffer.
Honours `cg-gaps-svg-ui' and whether the display is graphical."
  (cond ((and cg-gaps-svg-ui (display-graphic-p)) 'svg-fill)
        ((display-graphic-p) 'svg)
        (t 'text)))

(defun cg-gaps--redisplay ()
  "Redraw the current Gaps buffer through its renderer.
The treatment is chosen by `cg-gaps--treatment' and dispatched with
`cg-renderer-draw'."
  (let* ((game cg-gaps--game)
         (inhibit-read-only t)
         (renderer (cg-render-set-treatment game (cg-gaps--treatment))))
    (setq-local mode-line-process (cg-gaps--mode-line game))
    (erase-buffer)
    (cg-renderer-draw renderer game)
    (if (display-graphic-p)
        (goto-char (point-min))
      (let ((cur (cg-get game :cursor)))
        (cg-gaps--goto-cell (car cur) (cdr cur))))))

(defun cg-gaps--move (dr dc)
  "Move the cursor by DR rows and DC columns, then redisplay."
  (let* ((game cg-gaps--game)
         (cur (cg-get game :cursor))
         (r (min 3 (max 0 (+ (car cur) dr))))
         (c (min 12 (max 0 (+ (cdr cur) dc)))))
    (cg-put game :cursor (cons r c))
    (cg-gaps--redisplay)))

(defun cg-gaps-left ()  "Move cursor left."  (interactive) (cg-gaps--move 0 -1))
(defun cg-gaps-right () "Move cursor right." (interactive) (cg-gaps--move 0  1))
(defun cg-gaps-up ()    "Move cursor up."    (interactive) (cg-gaps--move -1 0))
(defun cg-gaps-down ()  "Move cursor down."  (interactive) (cg-gaps--move  1 0))

(defun cg-gaps--after-move ()
  "Check for a win or a stuck position and report it."
  (let ((game cg-gaps--game))
    (cond
     ((cg-won-p game)
      (cg-put game :message
              (format "\U0001F389 Solved in %d moves!  Press n for a new game."
                      (cg-get game :moves))))
     ((cg-gaps--stuck-p game)
      (cg-put game :message
              (if (> (cg-get game :redeals) 0)
                  (format "Stuck!  Press r to redeal (%d left)."
                          (cg-get game :redeals))
                "Stuck, and no redeals left.  Press n for a new game."))))
    (cg-gaps--redisplay)
    (message "%s" (cg-get game :message))))

(defun cg-gaps-fill ()
  "Fill the gap under the cursor."
  (interactive)
  (let* ((game cg-gaps--game)
         (cur (cg-get game :cursor)))
    (if (cg-gaps--fill game (car cur) (cdr cur))
        (cg-gaps--after-move)
      (cg-gaps--redisplay))))

(defun cg-gaps-redeal ()
  "Reshuffle and redeal the misplaced cards, if redeals remain."
  (interactive)
  (let ((game cg-gaps--game))
    (if (<= (cg-get game :redeals) 0)
        (progn (cg-put game :message "No redeals left.")
               (cg-gaps--redisplay))
      (cg-gaps--save-undo game)
      (cg-gaps--do-redeal game)
      (cg-put game :redeals (1- (cg-get game :redeals)))
      (cg-put game :message
              (format "Redealt.  %d redeals left." (cg-get game :redeals)))
      (cg-gaps--after-move))))

(defun cg-gaps-undo ()
  "Undo the last move or redeal."
  (interactive)
  (let* ((game cg-gaps--game)
         (hist (cg-get game :history)))
    (if (null hist)
        (progn (cg-put game :message "Nothing to undo.")
               (cg-gaps--redisplay))
      (let ((snap (car hist)))
        (cg-put game :board   (nth 0 snap))
        (cg-put game :moves   (nth 1 snap))
        (cg-put game :redeals (nth 2 snap))
        (cg-put game :history (cdr hist))
        (cg-put game :message "Undone.")
        (cg-gaps--redisplay)))))

(defun cg-gaps-new ()
  "Start a new game in the current buffer."
  (interactive)
  (cg-gaps--deal cg-gaps--game)
  (cg-gaps--redisplay))

(defun cg-gaps--xy->cell (px py)
  "Map pixel coordinates PX, PY on the SVG board to a (ROW . COL), or nil."
  (let* ((w cg-gaps--svg-card-w) (h cg-gaps--svg-card-h)
         (g cg-gaps--svg-gap) (pad cg-gaps--svg-pad))
    (when (and (>= px pad) (>= py pad))
      (let* ((col (/ (- px pad) (+ w g)))
             (row (/ (- py pad) (+ h g)))
             (xin (- px pad (* col (+ w g))))
             (yin (- py pad (* row (+ h g)))))
        (when (and (< col 13) (< row 4) (<= xin w) (<= yin h))
          (cons row col))))))

(defun cg-gaps-mouse (event)
  "Fill the gap clicked by EVENT (or move the cursor there).
Dispatches to the full-SVG UI when active; otherwise hit-tests the inline
SVG board (pixel) or the text grid (text property)."
  (interactive "e")
  (let ((start (event-start event)))
    (if (and cg-gaps-svg-ui (display-graphic-p) (posn-image start))
        (cg-gaps--svg-ui-click start)
      (let ((cell (if (and (display-graphic-p) (posn-image start))
                      (let ((xy (posn-object-x-y start)) (s (cg-scale)))
                        (and xy (cg-gaps--xy->cell (round (/ (car xy) s))
                                                   (round (/ (cdr xy) s)))))
                    (let ((pos (posn-point start)))
                      (and pos (get-text-property pos 'cg-cell))))))
        (when cell
          (cg-put cg-gaps--game :cursor cell)
          (cg-gaps-fill))))))

(defun cg-gaps-help ()
  "Show a one-line reminder of the controls."
  (interactive)
  (let ((game cg-gaps--game))
    (message "%s"
             (format "%s: move to a highlighted gap and RET to fill it (a %s anchors the head). r=redeal u=undo n=new q=quit."
                     (if game (cg-gaps--vname game) "Gaps")
                     (if game (aref cg-gaps-rank-names (cg-gaps--head game)) "card")))))

(defun cg-gaps--mode-line (game)
  "Return the mode-line status string for GAME."
  (cond ((cg-won-p game) " [Solved!]")
        ((cg-gaps--stuck-p game)
         (if (> (cg-get game :redeals) 0) " [Stuck — r to redeal]" " [Stuck]"))
        (t (format " [moves %d · redeals %d]"
                   (cg-get game :moves) (cg-get game :redeals)))))

(defun cg-gaps-zoom-in ()
  "Enlarge the cards."  (interactive) (text-scale-increase 1) (cg-gaps--redisplay))
(defun cg-gaps-zoom-out ()
  "Shrink the cards."   (interactive) (text-scale-decrease 1) (cg-gaps--redisplay))
(defun cg-gaps-zoom-reset ()
  "Reset the card size." (interactive) (text-scale-set 0) (cg-gaps--redisplay))

(defun cg-gaps-redraw ()
  "Redraw the board (e.g. after a theme or frame change)."
  (interactive)
  (cg-gaps--redisplay))


;;;; Frameless full-SVG UI (opt-in; see `cg-gaps-svg-ui')

(defconst cg-gaps--ui-w 820 "Default full-SVG gaps canvas width.")
(defconst cg-gaps--ui-h 380 "Default full-SVG gaps canvas height.")

(defvar-local cg-gaps--regions nil
  "Plist of clickable regions for the full-SVG gaps UI.")
(defvar-local cg-gaps--ui-last-size nil
  "Last window pixel size used to render the full-SVG gaps UI.")

(defun cg-gaps--in-rect (px py rect)
  "Return non-nil when PX,PY lie inside RECT (X Y W H)."
  (and rect (>= px (nth 0 rect)) (< px (+ (nth 0 rect) (nth 2 rect)))
       (>= py (nth 1 rect)) (< py (+ (nth 1 rect) (nth 3 rect)))))

(defun cg-gaps--ui-text (svg str x y size color &optional bold anchor)
  "Draw text STR on SVG at X,Y (SIZE, COLOR); ANCHOR defaults to start."
  (let ((a (list :x (round x) :y (round y) :font-size (round size)
                 :fill color :text-anchor (or anchor "start")
                 :font-family cg-svg-font-family)))
    (when bold (setq a (append a (list :font-weight "bold"))))
    (apply #'svg-text svg str a)))

(defun cg-gaps--ui-label (svg str x y size)
  "Draw an all-caps, letter-spaced section label on SVG."
  (svg-text svg (upcase str) :x (round x) :y (round y) :font-size (round size)
            :fill "#8fc79b" :text-anchor "start" :font-family cg-svg-font-family
            :font-weight "bold" :letter-spacing "2"))

(defun cg-gaps--ui-divider (svg x1 x2 y)
  "Draw a faint horizontal divider on SVG."
  (svg-line svg x1 y x2 y :stroke "#1b6b35" :stroke-width 1))

(defun cg-gaps--draw-panel (svg game h lpw fs)
  "Draw the left status/controls panel (width LPW, scale FS).
Return a plist of clickable control regions."
  (let* ((regions nil)
         (F (lambda (n) (round (* n fs))))
         (px0 (funcall F 14)) (pxr (- lpw (funcall F 12)))
         (dl (funcall F 8)) (dr (- lpw (funcall F 8)))
         (cxp (/ lpw 2)) (y 0))
    (svg-rectangle svg 6 6 (- lpw 8) (- h 12) :rx 10 :fill "#0d4a22" :fill-opacity 0.9
                   :stroke "#0a3a1a" :stroke-width 1)
    (setq y (funcall F 30))
    (cg-svg--text svg (cg-gaps--vname game) cxp y (funcall F 15) "#f1c40f" t)
    (setq y (+ y (funcall F 14))) (cg-gaps--ui-divider svg dl dr y)
    ;; stats
    (setq y (+ y (funcall F 22)))
    (cg-gaps--ui-text svg "Moves" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (cg-get game :moves)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family cg-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 20)))
    (cg-gaps--ui-text svg "Redeals left" px0 y (funcall F 13) "#eaffea")
    (svg-text svg (number-to-string (cg-get game :redeals)) :x pxr :y y
              :font-size (funcall F 14) :fill "#eaffea" :text-anchor "end"
              :font-family cg-svg-font-family :font-weight "bold")
    (setq y (+ y (funcall F 16))) (cg-gaps--ui-divider svg dl dr y)
    ;; rules
    (setq y (+ y (funcall F 20)))
    (cg-gaps--ui-label svg "Rules" px0 (- y (funcall F 6)) (funcall F 10))
    (setq y (+ y (funcall F 16)))
    (cg-gaps--ui-text svg (format "Head: %s"
                                  (aref cg-gaps-rank-names (cg-gaps--head game)))
                      px0 y (funcall F 12) "#cfeccf")
    (setq y (+ y (funcall F 16)))
    (cg-gaps--ui-text svg (if (> (cg-gaps--step game) 0) "Build up  2..K"
                            "Build down  K..2")
                      px0 y (funcall F 12) "#cfeccf")
    (setq y (+ y (funcall F 16)) )
    (cg-gaps--ui-text svg "One suit per row" px0 y (funcall F 12) "#9fd0a8")
    (setq y (+ y (funcall F 14))) (cg-gaps--ui-divider svg dl dr y)
    ;; controls: the key shown on each button is the keyboard shortcut
    (setq y (+ y (funcall F 20)))
    (let* ((bw (- lpw px0 (funcall F 12))) (bh (funcall F 26)) (bg (funcall F 8))
           (canredeal (> (cg-get game :redeals) 0))
           (canundo (and (cg-get game :history) t))
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
          (cg-gaps--ui-text svg kc (+ px0 (funcall F 10)) (+ y (round (* bh 0.68)))
                            (funcall F 13) (if on "#f1c40f" "#5f7f68") t)
          (cg-gaps--ui-text svg word (+ px0 (funcall F 30)) (+ y (round (* bh 0.68)))
                            (funcall F 13) (if on "#eaffea" "#5f7f68"))
          (setq regions (plist-put regions key rect))
          (setq y (+ y bh bg)))))
    ;; status message, wrapped to the panel
    (let ((msg (cg-get game :message)))
      (when (and msg (> (length msg) 0))
        (let ((m (if (> (length msg) (max 18 (round (/ (- lpw px0 (funcall F 12))
                                                       (* 0.55 (funcall F 11))))))
                     (substring msg 0 (max 18 (round (/ (- lpw px0 (funcall F 12))
                                                        (* 0.55 (funcall F 11))))))
                   msg)))
          (cg-gaps--ui-text svg m px0 (- h (funcall F 14)) (funcall F 11) "#9fd0a8"))))
    regions))

(defun cg-gaps--ui-svg (game &optional w h)
  "Return (SVG . REGIONS) for the full-buffer gaps UI of GAME (W by H).
The board scales to fill the area beside a proportional left panel."
  (let* ((W (or w cg-gaps--ui-w)) (H (or h cg-gaps--ui-h))
         (svg (svg-create W H)) (regions nil)
         (fs (max 1.0 (min 2.2 (/ (+ (/ (float W) cg-gaps--ui-w)
                                     (/ (float H) cg-gaps--ui-h)) 2.0))))
         (pscale (max 1.0 (min 1.7 (/ (float W) cg-gaps--ui-w))))
         (lpw (round (* 190 pscale)))
         (bx (+ lpw 14)) (by 10)
         (aw (- W bx 14)) (ah (- H by 10))
         (board (cg-get game :board))
         (cur (cg-get game :cursor)) (cr (car cur)) (cc (cdr cur))
         (hints (cg-gaps--hints game))
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
    (svg-gradient svg "cg-gfelt" 'radial '((0 . "#1a7a38") (100 . "#0c4720")))
    (svg-rectangle svg 0 0 W H :rx 14 :gradient "cg-gfelt")
    (svg-rectangle svg (- bx 6) by (+ aw 12) ah :rx 12
                   :fill "#000000" :fill-opacity 0.10
                   :stroke "#0e5226" :stroke-width 2)
    ;; board
    (let ((cg-svg-card-width cw) (cg-svg-card-height ch))
      (dotimes (r 4)
        (dotimes (c 13)
          (let* ((cell (cg-gaps--cell board r c))
                 (spec (and cell (cons (aref cg-gaps-ranks (cdr cell)) (car cell))))
                 (x (+ x0 (* c (+ cw g)))) (y (+ y0 (* r (+ ch g))))
                 (hl (and (= r cr) (= c cc)))
                 (hint (and (null cell) (member (cons r c) hints) t)))
            (cg-svg--draw-spec svg x y spec hl hint)))))
    (setq regions (plist-put regions :board (list x0 y0 cw ch g)))
    (setq regions (append regions (cg-gaps--draw-panel svg game H lpw fs)))
    (cons svg regions)))

(defun cg-gaps--ui-cell (px py geom)
  "Map pixel PX,PY to a (ROW . COL) given board GEOM (X0 Y0 CW CH G), or nil."
  (when geom
    (let ((x0 (nth 0 geom)) (y0 (nth 1 geom)) (cw (nth 2 geom))
          (ch (nth 3 geom)) (g (nth 4 geom)))
      (when (and (>= px x0) (>= py y0))
        (let* ((col (/ (- px x0) (+ cw g))) (row (/ (- py y0) (+ ch g)))
               (xin (- px x0 (* col (+ cw g)))) (yin (- py y0 (* row (+ ch g)))))
          (when (and (< col 13) (< row 4) (<= xin cw) (<= yin ch))
            (cons row col)))))))

(defun cg-gaps--svg-ui-click (start)
  "Dispatch a click at posn START within the full-SVG gaps UI."
  (let* ((xy (posn-object-x-y start)) (s (cg-scale))
         (px (round (/ (car xy) s))) (py (round (/ (cdr xy) s)))
         (game cg-gaps--game) (rg cg-gaps--regions))
    (cond
     ((cg-gaps--in-rect px py (plist-get rg :redeal)) (cg-gaps-redeal))
     ((cg-gaps--in-rect px py (plist-get rg :undo)) (cg-gaps-undo))
     ((cg-gaps--in-rect px py (plist-get rg :new)) (cg-gaps-new))
     ((cg-gaps--in-rect px py (plist-get rg :help)) (cg-gaps-help))
     (t (let ((cell (cg-gaps--ui-cell px py (plist-get rg :board))))
          (when cell (cg-put game :cursor cell) (cg-gaps-fill)))))))

(defun cg-gaps--insert-svg-ui (game)
  "Insert the full-buffer SVG gaps UI for GAME and record its regions."
  (let* ((win (get-buffer-window (current-buffer)))
         (fill (and cg-gaps-svg-fill win))
         (w (if fill (max 640 (window-body-width win t)) cg-gaps--ui-w))
         (h (if fill (max 320 (- (window-body-height win t) 4)) cg-gaps--ui-h))
         (sr (cg-gaps--ui-svg game w h)))
    (when fill (setq cg-gaps--ui-last-size (cons (window-body-width win t)
                                                 (window-body-height win t))))
    (setq cg-gaps--regions (cdr sr))
    (insert-image (cg-svg-image (car sr) (if fill 1.0 (cg-scale))))))

(defun cg-gaps--fit (&rest _)
  "Re-render the full-SVG gaps UI to fit the window after a config change."
  (when (and cg-gaps--game cg-gaps-svg-ui cg-gaps-svg-fill
             (eq major-mode 'cg-gaps-mode))
    (let ((win (get-buffer-window (current-buffer))))
      (when win
        (let ((sz (cons (window-body-width win t) (window-body-height win t))))
          (unless (equal sz cg-gaps--ui-last-size)
            (setq cg-gaps--ui-last-size sz)
            (cg-gaps--redisplay)))))))

(defun cg-gaps-toggle-svg-ui ()
  "Toggle the full-buffer SVG board for the gaps games."
  (interactive)
  (setq cg-gaps-svg-ui (not cg-gaps-svg-ui))
  (setq cg-gaps--ui-last-size nil)
  (cg-gaps--redisplay)
  (message "Full-SVG board %s" (if cg-gaps-svg-ui "enabled" "disabled")))

(defvar cg-gaps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>")  #'cg-gaps-left)
    (define-key map (kbd "<right>") #'cg-gaps-right)
    (define-key map (kbd "<up>")    #'cg-gaps-up)
    (define-key map (kbd "<down>")  #'cg-gaps-down)
    (define-key map (kbd "RET") #'cg-gaps-fill)
    (define-key map "g" #'cg-gaps-redraw)
    (define-key map "r" #'cg-gaps-redeal)
    (define-key map "u" #'cg-gaps-undo)
    (define-key map "n" #'cg-gaps-new)
    (define-key map "?" #'cg-gaps-help)
    (define-key map "+" #'cg-gaps-zoom-in)
    (define-key map "=" #'cg-gaps-zoom-in)
    (define-key map "-" #'cg-gaps-zoom-out)
    (define-key map "0" #'cg-gaps-zoom-reset)
    (define-key map "v" #'cg-gaps-toggle-svg-ui)
    (define-key map [mouse-1] #'cg-gaps-mouse)
    map)
  "Keymap for `cg-gaps-mode' (Emacs style; see `cg-keys').")

(defun cg-gaps--classic-keymap ()
  "Return a copy of `cg-gaps-mode-map' with vi-style hjkl and SPC added."
  (let ((map (copy-keymap cg-gaps-mode-map)))
    (define-key map "h" #'cg-gaps-left)
    (define-key map "l" #'cg-gaps-right)
    (define-key map "k" #'cg-gaps-up)
    (define-key map "j" #'cg-gaps-down)
    (define-key map (kbd "SPC") #'cg-gaps-fill)
    map))

(define-derived-mode cg-gaps-mode special-mode "Gaps"
  "Major mode for playing the gaps family of solitaires."
  (setq-local cursor-type cg-cursor-type)
  (setq-local truncate-lines t)
  (add-hook 'window-configuration-change-hook #'cg-gaps--fit nil t)
  (when (eq cg-keys 'classic)
    (use-local-map (cg-gaps--classic-keymap))))

(defun cg-gaps--play (class)
  "Start a gaps-style game of CLASS in its own buffer."
  (let* ((game (cg-gaps--deal (make-instance class)))
         (buf (get-buffer-create (format "*%s*" (cg-gaps--vname game)))))
    (with-current-buffer buf
      (cg-gaps-mode)
      (setq cg-gaps--game game)
      (cg-gaps--redisplay))
    (switch-to-buffer buf)))

;;;###autoload
(defun cg-montana ()
  "Play Gaps / Montana solitaire (Two at the head, build up 2..K)."
  (interactive)
  (cg-gaps--play 'cg-montana-game))

;;;###autoload
(defun cg-hells-half-acre ()
  "Play Hell's Half-Acre solitaire (King at the head, build down K..2)."
  (interactive)
  (cg-gaps--play 'cg-acre-game))

;;;###autoload
(defalias 'cg-gaps #'cg-montana
  "Alias for `cg-montana'.")

(provide 'cg-gaps)
;;; cg-gaps.el ends here
