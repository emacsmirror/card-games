;;; card-games-bid-net.el --- Networked live 500 (Bid)  -*- lexical-binding: t; -*-

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

;; Live multiplayer 500 over `card-games-net'.  One Emacs hosts with
;; `card-games-bid-host'; up to three others join with `card-games-bid-join'.  The host
;; owns the canonical game and sits South (seat 0); joining players take
;; seats West, North and East in turn.  Any seat left open when play
;; begins is driven by the existing AI, so a table of one human and
;; three robots, or four humans, or anything between, all work.
;;
;; The host is authoritative: a client sends a move "intent" (bid, pass,
;; discard or play); the host validates it, applies it to the canonical
;; game, lets the AI take any open seats, then broadcasts a fresh view
;; to every client.  Each client receives a per-seat view rotated so the
;; recipient sits South: it sees only its own cards, opponents collapse
;; to face-down counts, and the kitty stays hidden until won.  Because a
;; client's view places itself at seat 0, the ordinary single-player
;; commands and renderer work unchanged for everyone.

;;; Code:

(require 'cl-lib)
(require 'card-games-core)
(require 'card-games-net)
(require 'card-games-bid)
(require 'card-games-bid-ui)

(defcustom card-games-bid-shuffle-partners nil
  "When non-nil, randomize seating when a hosted game starts.
The host keeps South; joined players are shuffled among West, North and
East, so it is chance, not arrival order, that decides who partners whom."
  :type 'boolean :group 'card-games-net)

(defvar card-games-bid--net-role nil
  "Role of this Emacs in a live game: `host', `client', or nil (solo).")

(defvar card-games-bid--net-seat 0
  "This player's absolute seat in a live game (the host is always 0).")

(defvar card-games-bid--applying-remote nil
  "Bound non-nil while the host applies a remote player's move.
While set, prompts that would block the host (such as nominating a suit
for a Joker lead) fall back to an automatic choice.")

;;;; Per-seat state filter (host -> client)

(defun card-games-bid--rot (x seat)
  "Rotate absolute seat X into SEAT's frame so SEAT becomes 0.
Return nil when X is nil."
  (and x (mod (- x seat) 4)))

(defun card-games-bid--rot-team (team seat)
  "Rotate TEAM index (0 or 1) into SEAT's frame.
Return nil when TEAM is nil."
  (and team (if (cl-oddp seat) (- 1 team) team)))

(defun card-games-bid--rotate-vec4 (vec seat)
  "Return a fresh 4-vector whose element I is VEC element (I+SEAT) mod 4."
  (let ((v (make-vector 4 nil)))
    (dotimes (i 4) (aset v i (aref vec (mod (+ i seat) 4))))
    v))

(cl-defmethod card-games-net-game-state ((game card-games-bid-game) &optional seat)
  "Return GAME's shared state for SEAT, rotated so SEAT sits South.
Other players' hands collapse to face-down counts, the kitty is hidden,
and a hand exposed by an open misère is revealed to everyone."
  (let ((seat (or seat 0)))
    (if (null (card-games-get game :hands))
        ;; Lobby: nothing dealt yet.
        (list :phase (or (card-games-get game :phase) 'lobby)
              :message (or (card-games-get game :message) "Waiting for players…")
              :hand-no (or (card-games-get game :hand-no) 0))
      (let* ((hands (card-games-get game :hands))
             (exposed (card-games-get game :exposed))
             (rhands (make-vector 4 nil))
             (scores (card-games-get game :scores)))
        (dotimes (i 4)
          (let* ((abs (mod (+ i seat) 4))
                 (cards (aref hands abs)))
            (aset rhands i
                  (if (or (= abs seat) (eql abs exposed))
                      (copy-sequence cards)
                    (make-list (length cards) (cons 0 0))))))
        (list
         :hands rhands
         :kitty nil
         :phase (card-games-get game :phase)
         :contract (card-games-get game :contract)
         :contractor (card-games-bid--rot (card-games-get game :contractor) seat)
         :high-bid (card-games-get game :high-bid)
         :high-bidder (card-games-bid--rot (card-games-get game :high-bidder) seat)
         :bidder (card-games-bid--rot (card-games-get game :bidder) seat)
         :dealer (card-games-bid--rot (card-games-get game :dealer) seat)
         :passed (card-games-bid--rotate-vec4 (card-games-get game :passed) seat)
         :turn (card-games-bid--rot (card-games-get game :turn) seat)
         :leader (card-games-bid--rot (card-games-get game :leader) seat)
         :led (card-games-get game :led)
         :trick (mapcar (lambda (p) (cons (card-games-bid--rot (car p) seat) (cdr p)))
                        (card-games-get game :trick))
         :last-trick (mapcar (lambda (p) (cons (card-games-bid--rot (car p) seat) (cdr p)))
                             (card-games-get game :last-trick))
         :tricks (card-games-bid--rotate-vec4 (card-games-get game :tricks) seat)
         :ntricks (card-games-get game :ntricks)
         :exposed (card-games-bid--rot exposed seat)
         :scores (if (cl-oddp seat) (cons (cdr scores) (car scores)) scores)
         :game-over (card-games-bid--rot-team (card-games-get game :game-over) seat)
         :hand-no (card-games-get game :hand-no)
         :hand-result (card-games-get game :hand-result)
         :message (card-games-get game :message)
         :log (card-games-get game :log)
         :log-scroll (card-games-get game :log-scroll)
         :you seat)))))

(cl-defmethod card-games-net-set-game-state ((game card-games-bid-game) state)
  "Install host STATE into GAME on a client, keeping the local cursor and scroll."
  (let ((old (oref game env)))
    (oset game env state)
    (dolist (k '(:cursor :marks :log-scroll))
      (card-games-put game k (and (plist-member old k) (plist-get old k))))))

;;;; Apply a move on the host

(defun card-games-bid--net-holds-p (hand cards)
  "Return non-nil when every card in CARDS is present in HAND.
Multiplicity counts: naming one held card five times is not holding
five cards.  Cards are (SUIT . RANK) conses compared with `equal'."
  (let ((left (copy-sequence hand)))
    (catch 'missing
      (dolist (c cards t)
        (if (member c left)
            (setq left (cl-remove c left :test #'equal :count 1))
          (throw 'missing nil))))))

(cl-defmethod card-games-net-apply-move ((game card-games-bid-game) seat move)
  "Apply MOVE made by absolute SEAT to the host's 500 GAME.
MOVE is (bid BID), (pass), (discard CARD...) or (play CARD).  Return
non-nil when the move was legal and applied, so the host broadcasts."
  (let ((phase (card-games-get game :phase)) (ok nil))
    (pcase move
      (`(bid ,bid)
       (when (and (eq phase 'auction) (eql (card-games-get game :bidder) seat))
         (card-games-bid--auction-act game seat bid) (setq ok t)))
      (`(pass)
       (when (and (eq phase 'auction) (eql (card-games-get game :bidder) seat))
         (card-games-bid--auction-act game seat nil) (setq ok t)))
      (`(discard . ,cards)
       (when (and (eq phase 'kitty) (eql (card-games-get game :contractor) seat)
                  (= (length cards) 5)
                  (card-games-bid--net-holds-p (card-games-bid--hand game seat) cards))
         (card-games-bid--discard game seat cards) (setq ok t)))
      (`(play ,card)
       (when (and (eq phase 'play) (eql (card-games-get game :turn) seat)
                  (member card (card-games-bid-legal-cards
                                (card-games-bid--hand game seat)
                                (card-games-get game :led)
                                (card-games-bid-trump (card-games-get game :contract)))))
         (let ((card-games-bid--applying-remote t)) (card-games-bid--play game seat card))
         (setq ok t))))
    (when ok
      (let ((card-games-bid--applying-remote t)) (card-games-bid--run game))
      (card-games-bid--net-host-refresh))
    ok))

(defun card-games-bid--net-nominate-advice (orig game seat)
  "Around advice for `card-games-bid--nominate-suit'.
While the host applies a remote move (ORIG GAME SEAT), pick the longest
suit automatically instead of prompting."
  (if card-games-bid--applying-remote
      (let ((counts (make-vector 4 0)) (best 0))
        (dolist (c (card-games-bid--hand game seat))
          (unless (card-games-bid-joker-p c) (cl-incf (aref counts (car c)))))
        (dotimes (s 4) (when (> (aref counts s) (aref counts best)) (setq best s)))
        best)
    (funcall orig game seat)))
(advice-add 'card-games-bid--nominate-suit :around #'card-games-bid--net-nominate-advice)

;;;; Host bookkeeping and display

(defun card-games-bid--net-host-refresh ()
  "Redraw the host's own table buffer."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf (card-games-bid--redisplay)))))

(defun card-games-bid--net-broadcast-advice (&rest _)
  "After advice on `card-games-bid--refresh' that broadcasts when hosting."
  (when (and (eq card-games-bid--net-role 'host) (card-games-net-hosting-p))
    (card-games-net-host-broadcast)))
(advice-add 'card-games-bid--refresh :after #'card-games-bid--net-broadcast-advice)

(defun card-games-bid--net-lobby-display ()
  "Show the host's pre-game lobby of seats."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (seats (cl-remove-duplicates card-games-bid--human-seats)))
          (erase-buffer)
          (insert "\n  500 — LIVE TABLE (hosting)\n\n")
          (dotimes (s 4)
            (insert (format "    %-6s %s\n" (aref card-games-bid-seat-names s)
                            (cond ((= s 0) "you (host)")
                                  ((memq s seats) "joined")
                                  (t "open — AI will fill")))))
          (insert "\n  Press s to start now")
          (when card-games-bid-shuffle-partners (insert " (partners shuffled)"))
          (insert ".\n")
          (goto-char (point-min)))))))

(defun card-games-bid--net-shuffle-seats ()
  "Randomly reassign joined clients among seats 1, 2 and 3.
The host keeps South (seat 0)."
  (let ((clients (and card-games-net--host
                      (cl-remove-if-not #'process-live-p
                                        (card-games-net-host-clients card-games-net--host))))
        (seats (card-games-shuffle (list 1 2 3))))
    (dolist (p clients) (process-put p 'card-games-net-seat (pop seats)))
    (setq card-games-bid--human-seats
          (cons 0 (mapcar (lambda (p) (process-get p 'card-games-net-seat)) clients)))))

(defun card-games-bid--net-start ()
  "Deal and begin the hosted game, AI filling any open seat."
  (let ((game card-games-bid--game))
    (when card-games-bid-shuffle-partners (card-games-bid--net-shuffle-seats))
    (setq card-games-bid--human-seats (cl-remove-duplicates card-games-bid--human-seats))
    (card-games-bid--deal game 3)
    (let ((card-games-bid--applying-remote t)) (card-games-bid--run game))
    (card-games-bid--net-host-refresh)
    (card-games-net-host-broadcast)))

(defun card-games-bid--net-on-connect (host seat)
  "Host hook: a client has joined at SEAT (HOST is the server struct)."
  (when (eq card-games-bid--net-role 'host)
    (if (> seat 3)
        ;; Table full: turn the latecomer away.
        (let ((p (cl-find seat (card-games-net-host-clients host)
                          :key (lambda (q) (process-get q 'card-games-net-seat)))))
          (when p (card-games-net--send p '(:type full)) (delete-process p)))
      (cl-pushnew seat card-games-bid--human-seats)
      (card-games-bid--net-lobby-display)
      (message "Player joined at %s." (aref card-games-bid-seat-names seat))
      (when (= (length (cl-remove-duplicates card-games-bid--human-seats)) 4)
        (card-games-bid--net-start)))))

;;;; Client display

(defun card-games-bid--net-client-update (game)
  "Redraw the client's buffer after the host sends new state for GAME."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (memq (card-games-get game :phase) '(lobby nil))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n  500 — connected to host.\n\n  "
                      (or (card-games-get game :message)
                          "Waiting for the host to start…")
                      "\n")
              (goto-char (point-min)))
          (card-games-bid--redisplay))))))

;;;; Client move interception

(defun card-games-bid--net-client-bid-advice (orig)
  "Around advice on `card-games-bid-make-bid' (ORIG): send the bid, do not apply it."
  (if (eq card-games-bid--net-role 'client)
      (let ((game card-games-bid--game))
        (if (or (not (eq (card-games-get game :phase) 'auction))
                (/= (card-games-get game :bidder) 0))
            (progn (card-games-put game :message "Not your turn to bid.")
                   (card-games-bid--redisplay))
          (let* ((legal (card-games-bid--legal-bids game))
                 (completion-ignore-case t)
                 (choices (append
                           (mapcar (lambda (b)
                                     (cons (format "%-4s %s (%d)"
                                                   (card-games-bid--code b)
                                                   (card-games-bid-name b)
                                                   (card-games-bid-value b))
                                           b))
                                   legal)
                           '(("Pass" . pass))))
                 (pick (completing-read
                        "Your bid (e.g. 7H, 8NT, NL; or Pass): "
                        (mapcar #'car choices) nil t))
                 (sel (cdr (assoc pick choices))))
            (card-games-net-send-move (if (eq sel 'pass) '(pass) (list 'bid sel)))
            (card-games-put game :message "Bid sent — waiting…")
            (card-games-bid--redisplay))))
    (funcall orig)))
(advice-add 'card-games-bid-make-bid :around #'card-games-bid--net-client-bid-advice)

(defun card-games-bid--net-client-pass-advice (orig)
  "Around advice on `card-games-bid-pass' (ORIG): send a pass, do not apply it."
  (if (eq card-games-bid--net-role 'client)
      (let ((game card-games-bid--game))
        (if (or (not (eq (card-games-get game :phase) 'auction))
                (/= (card-games-get game :bidder) 0))
            (progn (card-games-put game :message "Not your turn to bid.")
                   (card-games-bid--redisplay))
          (card-games-net-send-move '(pass))
          (card-games-put game :message "Pass sent — waiting…")
          (card-games-bid--redisplay)))
    (funcall orig)))
(advice-add 'card-games-bid-pass :around #'card-games-bid--net-client-pass-advice)

(defun card-games-bid--net-client-select-advice (orig)
  "Around advice on `card-games-bid-select' (ORIG): send a play, or mark locally."
  (if (eq card-games-bid--net-role 'client)
      (let* ((game card-games-bid--game)
             (phase (card-games-get game :phase))
             (card (card-games-bid--current-card)))
        (pcase phase
          ('kitty
           (when (eql (card-games-get game :contractor) 0)
             (let ((marks (card-games-get game :marks)))
               (card-games-put game :marks (if (member card marks)
                                       (remove card marks)
                                     (cons card marks)))
               (card-games-put game :message
                       (format "%d of 5 marked for discard."
                               (length (card-games-get game :marks))))
               (card-games-bid--redisplay))))
          ('play
           (cond
            ((/= (card-games-get game :turn) 0)
             (card-games-put game :message "Not your turn.") (card-games-bid--redisplay))
            ((null card) (card-games-bid--redisplay))
            (t (card-games-net-send-move (list 'play card))
               (card-games-put game :message "Card sent — waiting…")
               (card-games-bid--redisplay))))
          (_ (card-games-bid--redisplay))))
    (funcall orig)))
(advice-add 'card-games-bid-select :around #'card-games-bid--net-client-select-advice)

(defun card-games-bid--net-client-discard-advice (orig)
  "Around advice on `card-games-bid-discard-marked' (ORIG): send the discard intent."
  (if (eq card-games-bid--net-role 'client)
      (let* ((game card-games-bid--game) (marks (card-games-get game :marks)))
        (cond
         ((not (eq (card-games-get game :phase) 'kitty))
          (card-games-put game :message "Nothing to discard now.") (card-games-bid--redisplay))
         ((/= (length marks) 5)
          (card-games-put game :message
                  (format "Mark exactly 5 (have %d)." (length marks)))
          (card-games-bid--redisplay))
         (t (card-games-net-send-move (cons 'discard marks))
            (card-games-put game :marks nil)
            (card-games-put game :message "Discard sent — waiting…")
            (card-games-bid--redisplay))))
    (funcall orig)))
(advice-add 'card-games-bid-discard-marked :around #'card-games-bid--net-client-discard-advice)

;;;; Commands

(defun card-games-bid-start-now ()
  "Start a hosted game immediately, AI filling any empty seats."
  (interactive)
  (if (and (eq card-games-bid--net-role 'host)
           (eq (card-games-get card-games-bid--game :phase) 'lobby))
      (card-games-bid--net-start)
    (message "Not hosting a lobby.")))
(define-key card-games-bid-mode-map "s" #'card-games-bid-start-now)

;;;###autoload
(defun card-games-bid-host (port)
  "Host a live game of 500 on PORT.  Others join with `card-games-bid-join'."
  (interactive (list (read-number "Host on port: " card-games-net-port)))
  (let ((buf (get-buffer-create "*500 Bid*")))
    (with-current-buffer buf
      (card-games-bid-mode)
      (setq card-games-bid--game (make-instance 'card-games-bid-game)
            card-games-bid--net-role 'host
            card-games-bid--net-seat 0
            card-games-bid--human-seats '(0))
      (card-games-put card-games-bid--game :phase 'lobby)
      (card-games-put card-games-bid--game :message "Lobby")
      (card-games-net-host-start card-games-bid--game port)
      (setf (card-games-net-host-next-seat card-games-net--host) 1)
      (add-hook 'card-games-net-connect-functions #'card-games-bid--net-on-connect)
      (card-games-bid--net-lobby-display))
    (switch-to-buffer buf)
    (message "Hosting 500 on port %d — waiting for players (press s to start)."
             port)))

;;;###autoload
(defun card-games-bid-join (host port name)
  "Join a hosted game of 500 at HOST and PORT as NAME."
  (interactive (list (read-string "Host: " "127.0.0.1")
                     (read-number "Port: " card-games-net-port)
                     (read-string "Your name: " (user-login-name))))
  (let ((buf (get-buffer-create "*500 Bid*")))
    (with-current-buffer buf
      (card-games-bid-mode)
      (setq card-games-bid--game (make-instance 'card-games-bid-game)
            card-games-bid--net-role 'client
            card-games-bid--human-seats '(0))
      (card-games-put card-games-bid--game :phase 'lobby)
      (card-games-put card-games-bid--game :message "Connecting…")
      (add-hook 'card-games-net-state-functions #'card-games-bid--net-client-update)
      (card-games-net-connect host port name card-games-bid--game)
      (card-games-bid--net-client-update card-games-bid--game))
    (switch-to-buffer buf)))

(provide 'card-games-bid-net)
;;; card-games-bid-net.el ends here
