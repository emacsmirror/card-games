;;; cg-bid-net.el --- Networked live 500 (Bid)  -*- lexical-binding: t; -*-

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

;; Live multiplayer 500 over `cg-net'.  One Emacs hosts with
;; `cg-bid-host'; up to three others join with `cg-bid-join'.  The host
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
(require 'cg-core)
(require 'cg-net)
(require 'cg-bid)
(require 'cg-bid-ui)

(defcustom cg-bid-shuffle-partners nil
  "When non-nil, randomize seating when a hosted game starts.
The host keeps South; joined players are shuffled among West, North and
East, so it is chance, not arrival order, that decides who partners whom."
  :type 'boolean :group 'cg-net)

(defvar cg-bid--net-role nil
  "Role of this Emacs in a live game: `host', `client', or nil (solo).")

(defvar cg-bid--net-seat 0
  "This player's absolute seat in a live game (the host is always 0).")

(defvar cg-bid--applying-remote nil
  "Bound non-nil while the host applies a remote player's move.
While set, prompts that would block the host (such as nominating a suit
for a Joker lead) fall back to an automatic choice.")

;;;; Per-seat state filter (host -> client)

(defun cg-bid--rot (x seat)
  "Rotate absolute seat X into SEAT's frame so SEAT becomes 0.
Return nil when X is nil."
  (and x (mod (- x seat) 4)))

(defun cg-bid--rot-team (team seat)
  "Rotate TEAM index (0 or 1) into SEAT's frame.
Return nil when TEAM is nil."
  (and team (if (cl-oddp seat) (- 1 team) team)))

(defun cg-bid--rotate-vec4 (vec seat)
  "Return a fresh 4-vector whose element I is VEC element (I+SEAT) mod 4."
  (let ((v (make-vector 4 nil)))
    (dotimes (i 4) (aset v i (aref vec (mod (+ i seat) 4))))
    v))

(cl-defmethod cg-net-game-state ((game cg-bid-game) &optional seat)
  "Return GAME's shared state for SEAT, rotated so SEAT sits South.
Other players' hands collapse to face-down counts, the kitty is hidden,
and a hand exposed by an open misère is revealed to everyone."
  (let ((seat (or seat 0)))
    (if (null (cg-get game :hands))
        ;; Lobby: nothing dealt yet.
        (list :phase (or (cg-get game :phase) 'lobby)
              :message (or (cg-get game :message) "Waiting for players…")
              :hand-no (or (cg-get game :hand-no) 0))
      (let* ((hands (cg-get game :hands))
             (exposed (cg-get game :exposed))
             (rhands (make-vector 4 nil))
             (scores (cg-get game :scores)))
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
         :phase (cg-get game :phase)
         :contract (cg-get game :contract)
         :contractor (cg-bid--rot (cg-get game :contractor) seat)
         :high-bid (cg-get game :high-bid)
         :high-bidder (cg-bid--rot (cg-get game :high-bidder) seat)
         :bidder (cg-bid--rot (cg-get game :bidder) seat)
         :dealer (cg-bid--rot (cg-get game :dealer) seat)
         :passed (cg-bid--rotate-vec4 (cg-get game :passed) seat)
         :turn (cg-bid--rot (cg-get game :turn) seat)
         :leader (cg-bid--rot (cg-get game :leader) seat)
         :led (cg-get game :led)
         :trick (mapcar (lambda (p) (cons (cg-bid--rot (car p) seat) (cdr p)))
                        (cg-get game :trick))
         :last-trick (mapcar (lambda (p) (cons (cg-bid--rot (car p) seat) (cdr p)))
                             (cg-get game :last-trick))
         :tricks (cg-bid--rotate-vec4 (cg-get game :tricks) seat)
         :ntricks (cg-get game :ntricks)
         :exposed (cg-bid--rot exposed seat)
         :scores (if (cl-oddp seat) (cons (cdr scores) (car scores)) scores)
         :game-over (cg-bid--rot-team (cg-get game :game-over) seat)
         :hand-no (cg-get game :hand-no)
         :hand-result (cg-get game :hand-result)
         :message (cg-get game :message)
         :log (cg-get game :log)
         :log-scroll (cg-get game :log-scroll)
         :you seat)))))

(cl-defmethod cg-net-set-game-state ((game cg-bid-game) state)
  "Install host STATE into GAME on a client, keeping the local cursor and scroll."
  (let ((old (oref game env)))
    (oset game env state)
    (dolist (k '(:cursor :marks :log-scroll))
      (cg-put game k (and (plist-member old k) (plist-get old k))))))

;;;; Apply a move on the host

(cl-defmethod cg-net-apply-move ((game cg-bid-game) seat move)
  "Apply MOVE made by absolute SEAT to the host's 500 GAME.
MOVE is (bid BID), (pass), (discard CARD...) or (play CARD).  Return
non-nil when the move was legal and applied, so the host broadcasts."
  (let ((phase (cg-get game :phase)) (ok nil))
    (pcase move
      (`(bid ,bid)
       (when (and (eq phase 'auction) (eql (cg-get game :bidder) seat))
         (cg-bid--auction-act game seat bid) (setq ok t)))
      (`(pass)
       (when (and (eq phase 'auction) (eql (cg-get game :bidder) seat))
         (cg-bid--auction-act game seat nil) (setq ok t)))
      (`(discard . ,cards)
       (when (and (eq phase 'kitty) (eql (cg-get game :contractor) seat)
                  (= (length cards) 5))
         (cg-bid--discard game seat cards) (setq ok t)))
      (`(play ,card)
       (when (and (eq phase 'play) (eql (cg-get game :turn) seat)
                  (member card (cg-bid-legal-cards
                                (cg-bid--hand game seat)
                                (cg-get game :led)
                                (cg-bid-trump (cg-get game :contract)))))
         (let ((cg-bid--applying-remote t)) (cg-bid--play game seat card))
         (setq ok t))))
    (when ok
      (let ((cg-bid--applying-remote t)) (cg-bid--run game))
      (cg-bid--net-host-refresh))
    ok))

(defun cg-bid--net-nominate-advice (orig game seat)
  "Around advice for `cg-bid--nominate-suit'.
While the host applies a remote move (ORIG GAME SEAT), pick the longest
suit automatically instead of prompting."
  (if cg-bid--applying-remote
      (let ((counts (make-vector 4 0)) (best 0))
        (dolist (c (cg-bid--hand game seat))
          (unless (cg-bid-joker-p c) (cl-incf (aref counts (car c)))))
        (dotimes (s 4) (when (> (aref counts s) (aref counts best)) (setq best s)))
        best)
    (funcall orig game seat)))
(advice-add 'cg-bid--nominate-suit :around #'cg-bid--net-nominate-advice)

;;;; Host bookkeeping and display

(defun cg-bid--net-host-refresh ()
  "Redraw the host's own table buffer."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf (cg-bid--redisplay)))))

(defun cg-bid--net-broadcast-advice (&rest _)
  "After advice on `cg-bid--refresh' that broadcasts when hosting."
  (when (and (eq cg-bid--net-role 'host) (cg-net-hosting-p))
    (cg-net-host-broadcast)))
(advice-add 'cg-bid--refresh :after #'cg-bid--net-broadcast-advice)

(defun cg-bid--net-lobby-display ()
  "Show the host's pre-game lobby of seats."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (seats (cl-remove-duplicates cg-bid--human-seats)))
          (erase-buffer)
          (insert "\n  500 — LIVE TABLE (hosting)\n\n")
          (dotimes (s 4)
            (insert (format "    %-6s %s\n" (aref cg-bid-seat-names s)
                            (cond ((= s 0) "you (host)")
                                  ((memq s seats) "joined")
                                  (t "open — AI will fill")))))
          (insert "\n  Press s to start now")
          (when cg-bid-shuffle-partners (insert " (partners shuffled)"))
          (insert ".\n")
          (goto-char (point-min)))))))

(defun cg-bid--net-shuffle-seats ()
  "Randomly reassign joined clients among seats 1, 2 and 3.
The host keeps South (seat 0)."
  (let ((clients (and cg-net--host
                      (cl-remove-if-not #'process-live-p
                                        (cg-net-host-clients cg-net--host))))
        (seats (cg-shuffle (list 1 2 3))))
    (dolist (p clients) (process-put p 'cg-net-seat (pop seats)))
    (setq cg-bid--human-seats
          (cons 0 (mapcar (lambda (p) (process-get p 'cg-net-seat)) clients)))))

(defun cg-bid--net-start ()
  "Deal and begin the hosted game, AI filling any open seat."
  (let ((game cg-bid--game))
    (when cg-bid-shuffle-partners (cg-bid--net-shuffle-seats))
    (setq cg-bid--human-seats (cl-remove-duplicates cg-bid--human-seats))
    (cg-bid--deal game 3)
    (let ((cg-bid--applying-remote t)) (cg-bid--run game))
    (cg-bid--net-host-refresh)
    (cg-net-host-broadcast)))

(defun cg-bid--net-on-connect (host seat)
  "Host hook: a client has joined at SEAT (HOST is the server struct)."
  (when (eq cg-bid--net-role 'host)
    (if (> seat 3)
        ;; Table full: turn the latecomer away.
        (let ((p (cl-find seat (cg-net-host-clients host)
                          :key (lambda (q) (process-get q 'cg-net-seat)))))
          (when p (cg-net--send p '(:type full)) (delete-process p)))
      (cl-pushnew seat cg-bid--human-seats)
      (cg-bid--net-lobby-display)
      (message "Player joined at %s." (aref cg-bid-seat-names seat))
      (when (= (length (cl-remove-duplicates cg-bid--human-seats)) 4)
        (cg-bid--net-start)))))

;;;; Client display

(defun cg-bid--net-client-update (game)
  "Redraw the client's buffer after the host sends new state for GAME."
  (let ((buf (get-buffer "*500 Bid*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (memq (cg-get game :phase) '(lobby nil))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n  500 — connected to host.\n\n  "
                      (or (cg-get game :message)
                          "Waiting for the host to start…")
                      "\n")
              (goto-char (point-min)))
          (cg-bid--redisplay))))))

;;;; Client move interception

(defun cg-bid--net-client-bid-advice (orig)
  "Around advice on `cg-bid-make-bid' (ORIG): send the bid, do not apply it."
  (if (eq cg-bid--net-role 'client)
      (let ((game cg-bid--game))
        (if (or (not (eq (cg-get game :phase) 'auction))
                (/= (cg-get game :bidder) 0))
            (progn (cg-put game :message "Not your turn to bid.")
                   (cg-bid--redisplay))
          (let* ((legal (cg-bid--legal-bids game))
                 (completion-ignore-case t)
                 (choices (append
                           (mapcar (lambda (b)
                                     (cons (format "%-4s %s (%d)"
                                                   (cg-bid--code b)
                                                   (cg-bid-name b)
                                                   (cg-bid-value b))
                                           b))
                                   legal)
                           '(("Pass" . pass))))
                 (pick (completing-read
                        "Your bid (e.g. 7H, 8NT, NL; or Pass): "
                        (mapcar #'car choices) nil t))
                 (sel (cdr (assoc pick choices))))
            (cg-net-send-move (if (eq sel 'pass) '(pass) (list 'bid sel)))
            (cg-put game :message "Bid sent — waiting…")
            (cg-bid--redisplay))))
    (funcall orig)))
(advice-add 'cg-bid-make-bid :around #'cg-bid--net-client-bid-advice)

(defun cg-bid--net-client-pass-advice (orig)
  "Around advice on `cg-bid-pass' (ORIG): send a pass, do not apply it."
  (if (eq cg-bid--net-role 'client)
      (let ((game cg-bid--game))
        (if (or (not (eq (cg-get game :phase) 'auction))
                (/= (cg-get game :bidder) 0))
            (progn (cg-put game :message "Not your turn to bid.")
                   (cg-bid--redisplay))
          (cg-net-send-move '(pass))
          (cg-put game :message "Pass sent — waiting…")
          (cg-bid--redisplay)))
    (funcall orig)))
(advice-add 'cg-bid-pass :around #'cg-bid--net-client-pass-advice)

(defun cg-bid--net-client-select-advice (orig)
  "Around advice on `cg-bid-select' (ORIG): send a play, or mark locally."
  (if (eq cg-bid--net-role 'client)
      (let* ((game cg-bid--game)
             (phase (cg-get game :phase))
             (card (cg-bid--current-card)))
        (pcase phase
          ('kitty
           (when (eql (cg-get game :contractor) 0)
             (let ((marks (cg-get game :marks)))
               (cg-put game :marks (if (member card marks)
                                       (remove card marks)
                                     (cons card marks)))
               (cg-put game :message
                       (format "%d of 5 marked for discard."
                               (length (cg-get game :marks))))
               (cg-bid--redisplay))))
          ('play
           (cond
            ((/= (cg-get game :turn) 0)
             (cg-put game :message "Not your turn.") (cg-bid--redisplay))
            ((null card) (cg-bid--redisplay))
            (t (cg-net-send-move (list 'play card))
               (cg-put game :message "Card sent — waiting…")
               (cg-bid--redisplay))))
          (_ (cg-bid--redisplay))))
    (funcall orig)))
(advice-add 'cg-bid-select :around #'cg-bid--net-client-select-advice)

(defun cg-bid--net-client-discard-advice (orig)
  "Around advice on `cg-bid-discard-marked' (ORIG): send the discard intent."
  (if (eq cg-bid--net-role 'client)
      (let* ((game cg-bid--game) (marks (cg-get game :marks)))
        (cond
         ((not (eq (cg-get game :phase) 'kitty))
          (cg-put game :message "Nothing to discard now.") (cg-bid--redisplay))
         ((/= (length marks) 5)
          (cg-put game :message
                  (format "Mark exactly 5 (have %d)." (length marks)))
          (cg-bid--redisplay))
         (t (cg-net-send-move (cons 'discard marks))
            (cg-put game :marks nil)
            (cg-put game :message "Discard sent — waiting…")
            (cg-bid--redisplay))))
    (funcall orig)))
(advice-add 'cg-bid-discard-marked :around #'cg-bid--net-client-discard-advice)

;;;; Commands

(defun cg-bid-start-now ()
  "Start a hosted game immediately, AI filling any empty seats."
  (interactive)
  (if (and (eq cg-bid--net-role 'host)
           (eq (cg-get cg-bid--game :phase) 'lobby))
      (cg-bid--net-start)
    (message "Not hosting a lobby.")))
(define-key cg-bid-mode-map "s" #'cg-bid-start-now)

;;;###autoload
(defun cg-bid-host (port)
  "Host a live game of 500 on PORT.  Others join with `cg-bid-join'."
  (interactive (list (read-number "Host on port: " cg-net-port)))
  (let ((buf (get-buffer-create "*500 Bid*")))
    (with-current-buffer buf
      (cg-bid-mode)
      (setq cg-bid--game (make-instance 'cg-bid-game)
            cg-bid--net-role 'host
            cg-bid--net-seat 0
            cg-bid--human-seats '(0))
      (cg-put cg-bid--game :phase 'lobby)
      (cg-put cg-bid--game :message "Lobby")
      (cg-net-host-start cg-bid--game port)
      (setf (cg-net-host-next-seat cg-net--host) 1)
      (add-hook 'cg-net-connect-functions #'cg-bid--net-on-connect)
      (cg-bid--net-lobby-display))
    (switch-to-buffer buf)
    (message "Hosting 500 on port %d — waiting for players (press s to start)."
             port)))

;;;###autoload
(defun cg-bid-join (host port name)
  "Join a hosted game of 500 at HOST and PORT as NAME."
  (interactive (list (read-string "Host: " "127.0.0.1")
                     (read-number "Port: " cg-net-port)
                     (read-string "Your name: " (user-login-name))))
  (let ((buf (get-buffer-create "*500 Bid*")))
    (with-current-buffer buf
      (cg-bid-mode)
      (setq cg-bid--game (make-instance 'cg-bid-game)
            cg-bid--net-role 'client
            cg-bid--human-seats '(0))
      (cg-put cg-bid--game :phase 'lobby)
      (cg-put cg-bid--game :message "Connecting…")
      (add-hook 'cg-net-state-functions #'cg-bid--net-client-update)
      (cg-net-connect host port name cg-bid--game)
      (cg-bid--net-client-update cg-bid--game))
    (switch-to-buffer buf)))

(provide 'cg-bid-net)
;;; cg-bid-net.el ends here
