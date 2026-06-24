;;; cg-net.el --- Networked multiplayer for card games  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Corwin Brust

;; Author: Corwin Brust <corwin@bru.st>
;; Maintainer: Corwin Brust <corwin@bru.st>
;; Version: 1.0.50
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
;; A game plugs in by defining methods on `cg-net-apply-move' (host
;; side) and, if its state is not just the env plist, on
;; `cg-net-game-state' / `cg-net-set-game-state'.  Clients add a
;; redraw function to `cg-net-state-functions'.

;;; Code:

(require 'cg-core)

(defgroup cg-net nil
  "Networked play for card games."
  :group 'card-games
  :prefix "cg-net-")

(defcustom cg-net-port 7500
  "Default TCP port used to host or join a game."
  :type 'integer :group 'cg-net)

(defvar cg-net-state-functions nil
  "Abnormal hook run on a client after the game state is updated.
Each function is called with the client's game object.")

;;;; Game integration points

(cl-defgeneric cg-net-apply-move (game seat move)
  "Apply MOVE made by SEAT to GAME on the host.
Return non-nil when the move was accepted (and state should broadcast).")

(cl-defgeneric cg-net-game-state (game)
  "Return a `read'able representation of GAME's shared state.")

(cl-defmethod cg-net-game-state ((game cg-game))
  "Default: return GAME's env plist as the shared state."
  (oref game env))

(cl-defgeneric cg-net-set-game-state (game state)
  "Replace GAME's shared state with STATE on a client.")

(cl-defmethod cg-net-set-game-state ((game cg-game) state)
  "Default: install STATE as GAME's env plist."
  (oset game env state))

;;;; Wire protocol

(defun cg-net--send (proc msg)
  "Send MSG (a sexp) to PROC as one newline-terminated line."
  (when (process-live-p proc)
    (let ((print-length nil) (print-level nil))
      (process-send-string proc (concat (prin1-to-string msg) "\n")))))

(defun cg-net--filter (handler)
  "Return a process filter dispatching each complete line to HANDLER.
HANDLER is called with (PROC MSG)."
  (lambda (proc string)
    (let ((buf (concat (or (process-get proc 'cg-net-buf) "") string))
          (start 0) nl)
      (while (setq nl (string-search "\n" buf start))
        (let ((line (substring buf start nl)))
          (setq start (1+ nl))
          (unless (string-empty-p line)
            (condition-case err
                (funcall handler proc (car (read-from-string line)))
              (error (message "cg-net: bad message: %S" err)))))
        )
      (process-put proc 'cg-net-buf (substring buf start)))))

;;;; Host

(cl-defstruct (cg-net-host (:constructor cg-net--host-make))
  server game (clients nil) (next-seat 0))

(defvar cg-net--host nil
  "The running `cg-net-host', or nil when not hosting.")

(defun cg-net-hosting-p ()
  "Return non-nil when this Emacs is hosting a game."
  (and cg-net--host (process-live-p (cg-net-host-server cg-net--host))))

(defun cg-net-host-start (game &optional port)
  "Begin hosting GAME on PORT (default `cg-net-port').  Return the server process."
  (let* ((port (or port cg-net-port))
         (server (make-network-process
                  :name "cg-host" :server t :service port
                  :host "0.0.0.0" :family 'ipv4 :coding 'utf-8
                  :log #'cg-net--host-accept)))
    (setq cg-net--host (cg-net--host-make :server server :game game))
    server))

(defun cg-net-host-stop ()
  "Stop hosting and close all client connections."
  (when cg-net--host
    (dolist (c (cg-net-host-clients cg-net--host))
      (when (process-live-p c) (delete-process c)))
    (when (process-live-p (cg-net-host-server cg-net--host))
      (delete-process (cg-net-host-server cg-net--host)))
    (setq cg-net--host nil)))

(defun cg-net--host-accept (_server connection _message)
  "Set up an accepted CONNECTION: assign a seat and send the current state."
  (let ((seat (cg-net-host-next-seat cg-net--host)))
    (setf (cg-net-host-next-seat cg-net--host) (1+ seat))
    (push connection (cg-net-host-clients cg-net--host))
    (process-put connection 'cg-net-seat seat)
    (set-process-coding-system connection 'utf-8 'utf-8)
    (set-process-filter connection (cg-net--filter #'cg-net--host-handle))
    (cg-net--send connection (list :type 'welcome :seat seat))
    (cg-net--send connection
                  (list :type 'state
                        :state (cg-net-game-state (cg-net-host-game cg-net--host))))))

(defun cg-net--host-handle (proc msg)
  "Handle one message MSG from a client PROC on the host."
  (pcase (plist-get msg :type)
    ('hello (process-put proc 'cg-net-name (plist-get msg :name)))
    ('move
     (let ((seat (process-get proc 'cg-net-seat))
           (game (cg-net-host-game cg-net--host)))
       (when (cg-net-apply-move game seat (plist-get msg :move))
         (cg-net-host-broadcast))))))

(defun cg-net-host-broadcast ()
  "Send the current game state to every connected client."
  (when cg-net--host
    (let ((state (cg-net-game-state (cg-net-host-game cg-net--host))))
      (dolist (c (cg-net-host-clients cg-net--host))
        (cg-net--send c (list :type 'state :state state))))))

;;;; Client

(cl-defstruct (cg-net-client (:constructor cg-net--client-make))
  proc game (seat nil))

(defvar cg-net--client nil
  "The active `cg-net-client', or nil when not connected.")

(defun cg-net-connected-p ()
  "Return non-nil when connected to a host as a client."
  (and cg-net--client (process-live-p (cg-net-client-proc cg-net--client))))

(defun cg-net-connect (host port name game)
  "Connect to HOST on PORT as NAME, syncing into the local GAME.
Return the new `cg-net-client'."
  (let ((proc (make-network-process
               :name "cg-client" :host host :service port
               :family 'ipv4 :coding 'utf-8)))
    (setq cg-net--client (cg-net--client-make :proc proc :game game))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc (cg-net--filter #'cg-net--client-handle))
    (cg-net--send proc (list :type 'hello :name name))
    cg-net--client))

(defun cg-net-disconnect ()
  "Disconnect from the host."
  (when (and cg-net--client (process-live-p (cg-net-client-proc cg-net--client)))
    (delete-process (cg-net-client-proc cg-net--client)))
  (setq cg-net--client nil))

(defun cg-net--client-handle (_proc msg)
  "Handle one message MSG from the host on a client."
  (pcase (plist-get msg :type)
    ('welcome (setf (cg-net-client-seat cg-net--client) (plist-get msg :seat)))
    ('state
     (let ((game (cg-net-client-game cg-net--client)))
       (cg-net-set-game-state game (plist-get msg :state))
       (run-hook-with-args 'cg-net-state-functions game)))))

(defun cg-net-send-move (move)
  "Send MOVE to the host from this client."
  (cg-net--send (cg-net-client-proc cg-net--client) (list :type 'move :move move)))

(provide 'cg-net)
;;; cg-net.el ends here
