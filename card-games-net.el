;;; card-games-net.el --- Networked multiplayer for card games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.92
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

;; Host-authoritative networking for the card games.  One Emacs is the
;; HOST: it owns the canonical game and listens for players.  Other
;; Emacsen CONNECT as clients, send move "intents", and receive the new
;; game state to redraw.  Because the games are turn-based there is
;; nothing to merge, so this is a simple authoritative server rather
;; than a CRDT; crdt.el is kept in reserve for any future free-form
;; shared state.
;;
;; Transport is line-delimited s-expressions over `make-network-process'
;; (plain TCP), so it works wherever Emacs has TCP -- including an
;; Android client joining a desktop host.  A message is a plist with a
;; `:type' key:
;;
;;   client -> host:  (:type hello :name NAME)
;;                    (:type move  :move MOVE)
;;   host -> client:  (:type welcome :seat N)
;;                    (:type state  :state SEXP)
;;
;; A game plugs in by defining methods on `card-games-net-apply-move' (host
;; side) and, if its state is not just the env plist, on
;; `card-games-net-game-state' / `card-games-net-set-game-state'.  Clients add a
;; redraw function to `card-games-net-state-functions'.

;;; Code:

(require 'card-games-core)

(defgroup card-games-net nil
  "Networked play for card games."
  :group 'card-games
  :prefix "card-games-net-")

(defcustom card-games-net-port 7500
  "Default TCP port used to host or join a game."
  :type 'integer :group 'card-games-net)

(defcustom card-games-net-host-address "127.0.0.1"
  "Address the host's listening socket binds when hosting a game.
The default, \"127.0.0.1\", accepts connections only from this
machine; remote players reach it through a tunnel they were
deliberately given (for example ssh port forwarding), which also
encrypts the traffic in transit.

Setting this to \"0.0.0.0\" listens on every network interface, which
means anyone able to reach this machine's port can take a seat: there
is no password and no encryption on the wire.  That can be a
reasonable choice on a trusted LAN, but it is a choice -- make it
deliberately."
  :type '(choice (const :tag "This machine only (recommended)" "127.0.0.1")
                 (const :tag "Every interface (anyone who can reach you)" "0.0.0.0")
                 (string :tag "A specific interface address"))
  :group 'card-games-net)

(defcustom card-games-net-max-line 65536
  "Longest unterminated line accepted from a connection, in bytes.
Messages in this protocol are short; 64 KiB is generous.  A connection
whose pending (newline-less) data exceeds this is closed, so one peer
cannot grow the line buffer until memory runs out."
  :type 'integer :group 'card-games-net)

(defcustom card-games-net-max-connections 8
  "Most simultaneous client connections a host will accept.
A table seats four, so the default leaves headroom without letting the
client list grow unboundedly.  Connections beyond the limit are closed
as they arrive."
  :type 'integer :group 'card-games-net)

(defvar card-games-net-state-functions nil
  "Abnormal hook run on a client after the game state is updated.
Each function is called with the client's game object.")

(defvar card-games-net-connect-functions nil
  "Abnormal hook run on the host when a client connects.
Each function is called with (HOST SEAT): the `card-games-net-host' struct and
the seat number just assigned to the new connection.")

;;;; Game integration points

(cl-defgeneric card-games-net-apply-move (game seat move)
  "Apply MOVE made by SEAT to GAME on the host.
Return non-nil when the move was accepted (and state should broadcast).")

(cl-defgeneric card-games-net-game-state (game &optional seat)
  "Return a `read'able representation of GAME's shared state for SEAT.
SEAT is the recipient's seat number, letting a game hide other players'
private information; nil requests the full host view.")

(cl-defmethod card-games-net-game-state ((game card-games-game) &optional _seat)
  "Default: return GAME's env plist (no per-seat filtering)."
  (oref game env))

(cl-defgeneric card-games-net-set-game-state (game state)
  "Replace GAME's shared state with STATE on a client.")

(cl-defmethod card-games-net-set-game-state ((game card-games-game) state)
  "Default: install STATE as GAME's env plist."
  (oset game env state))

;;;; Wire protocol

(defun card-games-net--send (proc msg)
  "Send MSG (a sexp) to PROC as one newline-terminated line."
  (when (process-live-p proc)
    (let ((print-length nil) (print-level nil))
      (process-send-string proc (concat (prin1-to-string msg) "\n")))))

(defun card-games-net--scrub (x)
  "Return X with text properties removed from every string inside it.
Walks conses and vectors, tolerating shared and circular structure.
Everything arriving over the network passes through this: text
properties can rebind keys or carry expressions evaluated during
redisplay, and there is never a reason to honour a remote peer's."
  (let ((seen (make-hash-table :test 'eq)))
    (cl-labels ((walk (v)
                  (cond
                   ((stringp v) (substring-no-properties v))
                   ((consp v)
                    (or (gethash v seen)
                        (let ((cell (cons nil nil)))
                          (puthash v cell seen)
                          (setcar cell (walk (car v)))
                          (setcdr cell (walk (cdr v)))
                          cell)))
                   ((vectorp v)
                    (or (gethash v seen)
                        (let ((copy (make-vector (length v) nil)))
                          (puthash v copy seen)
                          (dotimes (i (length v))
                            (aset copy i (walk (aref v i))))
                          copy)))
                   (t v))))
      (walk x))))

(defconst card-games-net-max-nodes 20000
  "Upper bound on the number of nodes accepted in one wire message.")

(defun card-games-net--valid-p (msg types)
  "Return non-nil when MSG is a well-shaped protocol message.
TYPES is the list of message-type symbols accepted from this peer.
MSG must be a proper plist whose `:type' is in TYPES, built only from
conses, vectors, strings, numbers and symbols, with no shared or
circular structure and at most `card-games-net-max-nodes' nodes.  Accept what
is recognised rather than trying to spot what is bad: parsed text can
carry self-references that hang code walking them, and objects that
impersonate internal record types."
  (let ((seen (make-hash-table :test 'eq))
        (nodes 0))
    (cl-labels ((clean-p (v)
                  (cond
                   ((> (cl-incf nodes) card-games-net-max-nodes) nil)
                   ((consp v)
                    (and (not (gethash v seen))
                         (progn (puthash v t seen)
                                (and (clean-p (car v)) (clean-p (cdr v))))))
                   ((vectorp v)
                    (and (not (gethash v seen))
                         (progn (puthash v t seen)
                                (cl-every #'clean-p v))))
                   ((or (stringp v) (numberp v) (symbolp v)) t)
                   (t nil))))
      (and (consp msg)
           (clean-p msg)                      ; safe before plist-get: no cycles past here
           (null (cdr (last msg)))            ; a proper list
           (cl-evenp (length msg))            ; of key/value pairs
           (memq (plist-get msg :type) types)))))

(defun card-games-net--filter (handler types)
  "Return a process filter dispatching each complete line to HANDLER.
HANDLER is called with (PROC MSG) for each line that parses into a
well-shaped message (`card-games-net--valid-p') whose type is in TYPES;
strings inside MSG have their text properties stripped first, by
`card-games-net--scrub'.  Anything else is dropped where it lands."
  (lambda (proc string)
    (let ((buf (concat (or (process-get proc 'card-games-net-buf) "") string))
          (start 0) nl)
      (while (setq nl (cl-search "\n" buf :start2 start))
        (let ((line (substring buf start nl)))
          (setq start (1+ nl))
          (unless (string-empty-p line)
            (condition-case err
                (let ((msg (car (read-from-string line))))
                  (if (card-games-net--valid-p msg types)
                      (funcall handler proc (card-games-net--scrub msg))
                    (message "card-games-net: dropped malformed message")))
              (error (message "card-games-net: bad message: %S" err))))))
      (let ((rest (substring buf start)))
        (if (> (length rest) card-games-net-max-line)
            (progn
              (process-put proc 'card-games-net-buf nil)
              (message "card-games-net: dropping %s (line over %d bytes)"
                       (process-name proc) card-games-net-max-line)
              (delete-process proc))
          (process-put proc 'card-games-net-buf rest))))))

;;;; Host

(cl-defstruct (card-games-net-host (:constructor card-games-net--host-make))
  server game (clients nil) (next-seat 0))

(defvar card-games-net--host nil
  "The running `card-games-net-host', or nil when not hosting.")

(defun card-games-net-hosting-p ()
  "Return non-nil when this Emacs is hosting a game."
  (and card-games-net--host (process-live-p (card-games-net-host-server card-games-net--host))))

(defun card-games-net-host-start (game &optional port)
  "Begin hosting GAME on PORT (default `card-games-net-port').
Return the server process.  The socket binds
`card-games-net-host-address' -- by default, this machine only."
  (let* ((port (or port card-games-net-port))
         (server (make-network-process
                  :name "card-games-host" :server t :service port
                  :host card-games-net-host-address :family 'ipv4 :coding 'utf-8
                  :log #'card-games-net--host-accept)))
    (setq card-games-net--host (card-games-net--host-make :server server :game game))
    server))

(defun card-games-net-host-stop ()
  "Stop hosting and close all client connections."
  (when card-games-net--host
    (dolist (c (card-games-net-host-clients card-games-net--host))
      (when (process-live-p c) (delete-process c)))
    (when (process-live-p (card-games-net-host-server card-games-net--host))
      (delete-process (card-games-net-host-server card-games-net--host)))
    (setq card-games-net--host nil)))

(defun card-games-net--host-sentinel (proc _event)
  "Reap PROC from the client list when its connection has ended.
Without this, departed players stay listed forever and the host keeps
sending to them.  Seat numbers are deliberately not reused: a stale
seat must not be inherited by a stranger mid-game."
  (unless (process-live-p proc)
    (when card-games-net--host
      (setf (card-games-net-host-clients card-games-net--host)
            (delq proc (card-games-net-host-clients card-games-net--host))))))

(defun card-games-net--host-accept (_server connection _message)
  "Set up an accepted CONNECTION: assign a seat and send the current state.
A connection arriving past `card-games-net-max-connections' is closed instead."
  (if (>= (length (cl-remove-if-not #'process-live-p
                                    (card-games-net-host-clients card-games-net--host)))
          card-games-net-max-connections)
      (progn
        (message "card-games-net: refusing connection (table is at %d)"
                 card-games-net-max-connections)
        (delete-process connection))
    (let ((seat (card-games-net-host-next-seat card-games-net--host)))
      (setf (card-games-net-host-next-seat card-games-net--host) (1+ seat))
      (push connection (card-games-net-host-clients card-games-net--host))
      (process-put connection 'card-games-net-seat seat)
      (set-process-coding-system connection 'utf-8 'utf-8)
      (set-process-sentinel connection #'card-games-net--host-sentinel)
      (set-process-filter connection
                          (card-games-net--filter #'card-games-net--host-handle '(hello move)))
      (card-games-net--send connection (list :type 'welcome :seat seat))
      (card-games-net--send connection
                    (list :type 'state
                          :state (card-games-net-game-state (card-games-net-host-game card-games-net--host) seat)))
      (run-hook-with-args 'card-games-net-connect-functions card-games-net--host seat))))

(defun card-games-net--host-handle (proc msg)
  "Handle one message MSG from a client PROC on the host."
  (pcase (plist-get msg :type)
    ('hello (process-put proc 'card-games-net-name (plist-get msg :name)))
    ('move
     (let ((seat (process-get proc 'card-games-net-seat))
           (game (card-games-net-host-game card-games-net--host)))
       (when (card-games-net-apply-move game seat (plist-get msg :move))
         (card-games-net-host-broadcast))))))

(defun card-games-net-host-broadcast ()
  "Send each connected client the game state filtered for its seat."
  (when card-games-net--host
    (let ((game (card-games-net-host-game card-games-net--host)))
      (dolist (c (card-games-net-host-clients card-games-net--host))
        (card-games-net--send c (list :type 'state
                              :state (card-games-net-game-state
                                      game (process-get c 'card-games-net-seat))))))))

;;;; Client

(cl-defstruct (card-games-net-client (:constructor card-games-net--client-make))
  proc game (seat nil))

(defvar card-games-net--client nil
  "The active `card-games-net-client', or nil when not connected.")

(defun card-games-net-connected-p ()
  "Return non-nil when connected to a host as a client."
  (and card-games-net--client (process-live-p (card-games-net-client-proc card-games-net--client))))

(defun card-games-net-connect (host port name game)
  "Connect to HOST on PORT as NAME, syncing into the local GAME.
Return the new `card-games-net-client'."
  (let ((proc (make-network-process
               :name "card-games-client" :host host :service port
               :family 'ipv4 :coding 'utf-8)))
    (setq card-games-net--client (card-games-net--client-make :proc proc :game game))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc
                        (card-games-net--filter #'card-games-net--client-handle
                                        '(welcome state full)))
    (card-games-net--send proc (list :type 'hello :name name))
    card-games-net--client))

(defun card-games-net-disconnect ()
  "Disconnect from the host."
  (when (and card-games-net--client (process-live-p (card-games-net-client-proc card-games-net--client)))
    (delete-process (card-games-net-client-proc card-games-net--client)))
  (setq card-games-net--client nil))

(defun card-games-net--client-handle (_proc msg)
  "Handle one message MSG from the host on a client."
  (pcase (plist-get msg :type)
    ('welcome (setf (card-games-net-client-seat card-games-net--client) (plist-get msg :seat)))
    ('state
     (let ((game (card-games-net-client-game card-games-net--client)))
       (card-games-net-set-game-state game (plist-get msg :state))
       (run-hook-with-args 'card-games-net-state-functions game)))))

(defun card-games-net-send-move (move)
  "Send MOVE to the host from this client."
  (card-games-net--send (card-games-net-client-proc card-games-net--client) (list :type 'move :move move)))

(provide 'card-games-net)
;;; card-games-net.el ends here
