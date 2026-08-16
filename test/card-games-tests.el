;;; card-games-tests.el --- ERT tests for card-games  -*- lexical-binding: t; -*-

;; Run with:  make test   (or)
;;   emacs -Q --batch -L . -L test -l test/card-games-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'card-games)

;;;; card-games-core

(ert-deftest cgt-core-color-fallback ()
  (should (equal "#123456" (card-games-color 'no-such-face-xyzzy :background "#123456"))))

(ert-deftest cgt-core-suit-glyph ()
  (let ((card-games-symbols '((0 . "S") (1 . "C") (2 . "D") (3 . "H") (joker . "JK"))))
    (should (equal "S" (card-games-suit-glyph 0)))
    (should (equal "JK" (card-games-suit-glyph 'joker))))
  (should (equal "♠" (card-games-suit-glyph 0))))

;;;; Renderer skins

(ert-deftest cgt-render-registry ()
  (should (memq 'text (card-games-renderer-names)))
  (should (memq 'svg (card-games-renderer-names)))
  (should (memq 'svg-fill (card-games-renderer-names)))
  (should (object-of-class-p (card-games-make-renderer 'svg) 'card-games-svg-renderer))
  (should (object-of-class-p (card-games-make-renderer 'svg-fill) 'card-games-svg-renderer))
  (should-not (card-games-make-renderer 'no-such-treatment)))

(ert-deftest cgt-render-resolve ()
  (should (eq 'text (card-games-render-resolve-treatment 'text)))
  (should (memq (card-games-render-resolve-treatment 'auto) '(text svg))))

;;;; Networking

(defclass cgt-net-game (card-games-game)
  ((env :initarg :env :initform '(:counter 0)))
  "Throwaway game whose only move adds to a counter.")

(cl-defmethod card-games-net-apply-move ((g cgt-net-game) _seat move)
  (card-games-put g :counter (+ (or (card-games-get g :counter) 0) move))
  t)

(ert-deftest cgt-net-loopback ()
  "Host-authoritative move sync over a loopback TCP socket."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
         (srv (card-games-net-host-start hgame 0))
         (port (process-contact srv :service))
         (cgame (make-instance 'cgt-net-game :env (list :counter 0))))
    (unwind-protect
        (progn
          (card-games-net-connect "127.0.0.1" port "Test" cgame)
          (dotimes (_ 12) (accept-process-output nil 0.05))
          (should (= 0 (card-games-get cgame :counter)))
          (card-games-net-send-move 7)
          (dotimes (_ 12) (accept-process-output nil 0.05))
          (should (= 7 (card-games-get hgame :counter)))
          (should (= 7 (card-games-get cgame :counter))))
      (card-games-net-disconnect)
      (card-games-net-host-stop))))

(ert-deftest cgt-net-strips-properties ()
  "Text properties on wire strings are stripped at the boundary, both ways.
Finding 2 of the 2026-07-29 review: a propertized hello :name from a
client must reach the host bare, and a propertized :message from the
host must reach the client bare."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe3" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 12) (accept-process-output nil 0.05))))
    (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (cgame (make-instance 'cgt-net-game :env (list :counter 0))))
      (unwind-protect
          (progn
            (card-games-net-connect "127.0.0.1" port
                            (propertize "Eve" 'keymap '(keymap)) cgame)
            (pump)
            ;; client -> host: the hello name arrives with no attachments
            (let* ((conn (car (card-games-net-host-clients card-games-net--host)))
                   (name (process-get conn 'card-games-net-name)))
              (should (equal name "Eve"))
              (should-not (text-properties-at 0 name)))
            ;; host -> client: a state string arrives with no attachments
            (card-games-put hgame :message
                    (propertize "hi" 'keymap '(keymap) 'help-echo "boo"))
            (card-games-net-host-broadcast)
            (pump)
            (let ((m (card-games-get cgame :message)))
              (should (equal m "hi"))
              (should-not (text-properties-at 0 m))))
        (card-games-net-disconnect)
        (card-games-net-host-stop)))))

(ert-deftest cgt-net-host-loopback-default ()
  "Hosting binds to this machine only unless deliberately widened.
Finding 1 of the 2026-07-29 review: the default of
`card-games-net-host-address' is loopback, and the listening socket really
binds it -- wider exposure is a setting the user turns on, not a
silent default."
  (should (equal "127.0.0.1" (default-value 'card-games-net-host-address)))
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe4" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
         (srv (card-games-net-host-start hgame 0)))
    (unwind-protect
        (should (equal [127 0 0 1]
                       (substring (process-contact srv :local) 0 4)))
      (card-games-net-host-stop))))

(ert-deftest cgt-net-line-cap ()
  "A connection sending endless bytes with no newline is dropped.
Finding 4 of the 2026-07-29 review: the partial-line buffer grew
without limit, so one connection could consume all available memory.
It is now bounded by `card-games-net-max-line'."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe5" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 8) (accept-process-output nil 0.05))))
    (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (raw (make-network-process :name "cgt-flood" :host "127.0.0.1"
                                      :service port :family 'ipv4)))
      (unwind-protect
          (progn
            (pump)
            (let ((chunk (make-string 8192 ?a)))
              (cl-loop repeat 10 while (process-live-p raw)
                       do (ignore-errors (process-send-string raw chunk))
                          (pump)))
            ;; 80 KiB with no newline: the host must have hung up on us
            (should-not (process-live-p raw)))
        (when (process-live-p raw) (delete-process raw))
        (card-games-net-host-stop)))))

(ert-deftest cgt-net-connection-cap ()
  "Connections beyond `card-games-net-max-connections' are refused.
Finding 4's second bound: the client list cannot grow without limit."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe6" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 8) (accept-process-output nil 0.05))))
    (let* ((card-games-net-max-connections 2)
           (hgame (make-instance 'cgt-net-game :env (list :counter 0)))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (procs nil))
      (unwind-protect
          (progn
            (dotimes (i 3)
              (push (make-network-process :name (format "cgt-c%d" i)
                                          :host "127.0.0.1" :service port
                                          :family 'ipv4)
                    procs)
              (pump))
            (should (= 2 (length (cl-remove-if-not
                                  #'process-live-p
                                  (card-games-net-host-clients card-games-net--host)))))
            ;; the newest connection is the one turned away
            (should-not (process-live-p (car procs))))
        (dolist (p procs) (when (process-live-p p) (delete-process p)))
        (card-games-net-host-stop)))))

(ert-deftest cgt-net-reaps-disconnected ()
  "A client that disconnects is removed from the host's client list.
Finding 5 of the 2026-07-29 review: nothing watched for a connection
ending, so departed players stayed listed forever and the host kept
sending to them.  A process sentinel now reaps them."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe7" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 12) (accept-process-output nil 0.05))))
    (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (cgame (make-instance 'cgt-net-game :env (list :counter 0))))
      (unwind-protect
          (progn
            (card-games-net-connect "127.0.0.1" port "Test" cgame)
            (pump)
            (should (= 1 (length (card-games-net-host-clients card-games-net--host))))
            (card-games-net-disconnect)
            (pump)
            (should (= 0 (length (card-games-net-host-clients card-games-net--host)))))
        (card-games-net-disconnect)
        (card-games-net-host-stop)))))

(ert-deftest cgt-net-shape-gate ()
  "Malformed messages are dropped before any game code sees them.
Finding 6 of the 2026-07-29 review: parsing can produce structures
that hang or confuse code walking them -- self-references, objects
impersonating internal types.  A message must be a proper plist of a
known :type built from plain data; anything else never reaches
`card-games-net-apply-move'."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe8" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 10) (accept-process-output nil 0.05))))
    (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (applied nil))
      (cl-letf (((symbol-function 'card-games-net-apply-move)
                 (lambda (_game _seat move) (push move applied) nil)))
        (let ((raw (make-network-process :name "cgt-shapes" :host "127.0.0.1"
                                         :service port :family 'ipv4)))
          (unwind-protect
              (progn
                (pump)
                (dolist (line '("(:type move :move #s(hash-table size 1))"
                                "(:type move :move #1=(1 2 . #1#))"
                                "42"
                                "(:type welcome :seat 3)"
                                "(:type move :move 5)"))
                  (process-send-string raw (concat line "\n")))
                (pump)
                ;; only the one well-shaped, host-legal move arrived
                (should (equal applied (list 5))))
            (when (process-live-p raw) (delete-process raw))
            (card-games-net-host-stop)))))))

(ert-deftest cgt-net-valid-p ()
  "Unit contract of the wire-shape check itself."
  (should (card-games-net--valid-p '(:type move :move (bid (7 . 3))) '(hello move)))
  (should (card-games-net--valid-p '(:type hello :name "n") '(hello move)))
  ;; wrong direction: a host does not accept host->client types
  (should-not (card-games-net--valid-p '(:type welcome :seat 1) '(hello move)))
  ;; not a plist / unknown / impersonating / shared structure
  (should-not (card-games-net--valid-p 42 '(hello move)))
  (should-not (card-games-net--valid-p '(:type reboot) '(hello move)))
  (should-not (card-games-net--valid-p (list :type 'move :move (make-hash-table))
                               '(hello move)))
  (should-not (card-games-net--valid-p (record 'card-games-net-host nil nil nil 0) '(hello move)))
  (let ((shared (list 1 2)))
    (should-not (card-games-net--valid-p (list :type 'move :move (list shared shared))
                                 '(hello move))))
  (let ((cyc (list 1 2)))
    (setcdr (cdr cyc) cyc)
    (should-not (card-games-net--valid-p (list :type 'move :move cyc) '(hello move)))))

;;;; Seeded shuffle

(ert-deftest cgt-shuffle-seeded ()
  "A seeded shuffle is deterministic, portable, and a true permutation;
the unseeded shuffle keeps its system-random behaviour.  Opening #4."
  (let ((deck (number-sequence 0 51)))
    ;; same seed -> identical order (FAILS on HEAD: card-games-shuffle takes 1 arg)
    (should (equal (card-games-shuffle deck 12345) (card-games-shuffle deck 12345)))
    ;; still a permutation of the input, nothing dropped or duplicated
    (should (equal (sort (copy-sequence (card-games-shuffle deck 12345)) #'<) deck))
    ;; different seeds -> different order (almost surely)
    (should-not (equal (card-games-shuffle deck 1) (card-games-shuffle deck 2)))
    ;; the dynamic variable seeds it too, and an explicit seed overrides it
    (should (equal (let ((card-games-shuffle-seed "deal-A")) (card-games-shuffle deck))
                   (let ((card-games-shuffle-seed "deal-A")) (card-games-shuffle deck))))
    (should (equal (card-games-shuffle deck 7)
                   (let ((card-games-shuffle-seed "ignored")) (card-games-shuffle deck 7))))
    ;; portability pin: this exact prefix must hold on every platform
    (should (equal (seq-take (card-games-shuffle deck "GOLDEN") 6) '(15 20 35 24 31 23)))))

;;;; Gaps

(ert-deftest cgt-gaps-deal ()
  (let* ((g (card-games-gaps--deal (make-instance 'card-games-montana-game)))
         (b (card-games-get g :board)) (cards 0) (gaps 0))
    (dotimes (r 4) (dotimes (c 13)
      (if (card-games-gaps--cell b r c) (cl-incf cards) (cl-incf gaps))))
    (should (= cards 48)) (should (= gaps 4))))

(ert-deftest cgt-gaps-win ()
  (let ((g (make-instance 'card-games-montana-game)) (b (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 12) (aset row c (cons r c)))
        (aset b r row)))
    (card-games-put g :board b)
    (should (card-games-won-p g))))

(ert-deftest cgt-acre-win ()
  ;; King-head descending: row r col c == (suit r . (- 11 c)), gap at col 12
  (let ((g (make-instance 'card-games-acre-game)) (b (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 12) (aset row c (cons r (- 11 c))))
        (aset b r row)))
    (card-games-put g :board b)
    (should (card-games-won-p g))
    ;; head gap wants a King; right of a Two is dead
    (should (eq 'head (card-games-gaps--needed g b 0 0)))))

(ert-deftest cgt-acre-needed ()
  (let ((g (make-instance 'card-games-acre-game)) (b (make-vector 4 nil)))
    (dotimes (r 4) (aset b r (make-vector 13 nil)))
    ;; left neighbour King(11) at (0,0): gap (0,1) wants Queen(10) same suit
    (aset (aref b 0) 0 (cons 0 11))
    (should (equal (cons 0 10) (card-games-gaps--needed g b 0 1)))
    ;; left neighbour Two(0): nothing follows -> nil (Two is the dead end)
    (aset (aref b 1) 0 (cons 0 0))
    (should (null (card-games-gaps--needed g b 1 1)))))

(ert-deftest cgt-gaps-hit ()
  (dotimes (r 4) (dotimes (c 13)
    (let ((px (+ card-games-gaps--svg-pad (* c (+ card-games-gaps--svg-card-w card-games-gaps--svg-gap))
               (/ card-games-gaps--svg-card-w 2)))
          (py (+ card-games-gaps--svg-pad (* r (+ card-games-gaps--svg-card-h card-games-gaps--svg-gap))
               (/ card-games-gaps--svg-card-h 2))))
      (should (equal (cons r c) (card-games-gaps--xy->cell px py)))))))

;;;; 500

(ert-deftest cgt-bid-deck () (should (= 45 (length (card-games-bid--full-deck)))))

(ert-deftest cgt-bid-power ()
  (should (> (card-games-bid-power card-games-bid-joker 0 0) (card-games-bid-power '(0 . 7) 0 0)))
  (should (> (card-games-bid-power '(0 . 7) 0 0) (card-games-bid-power '(1 . 7) 0 0)))
  (should (< (card-games-bid-power '(2 . 10) 0 3) 100)))

(ert-deftest cgt-bid-trick ()
  (should (= 2 (card-games-bid-trick-winner
                '((0 . (3 . 10)) (1 . (3 . 0)) (2 . (0 . 0)) (3 . (2 . 10))) 0 3))))

(ert-deftest cgt-bid-follow ()
  (should (equal '((1 . 7)) (card-games-bid-legal-cards '((1 . 7) (1 . 0) (3 . 0)) 0 0))))

(ert-deftest cgt-bid-sort-display ()
  (should (card-games-bid-joker-p
           (car (card-games-bid-sort-display (list '(3 . 10) card-games-bid-joker '(0 . 7)) 0)))))

(ert-deftest cgt-bid-south-hit ()
  (dolist (n '(1 5 10 15))
    (cl-destructuring-bind (x0 step y) (card-games-bid--south-layout n)
      (dotimes (i n)
        (should (equal i (card-games-bid--south-hit (+ x0 (* i step) 2) (+ y 5) n)))))))

(ert-deftest cgt-bid-full-game ()
  (let ((card-games-bid--human-seats nil))
    (dotimes (_ 12)
      (let ((g (card-games-bid--deal (make-instance 'card-games-bid-game))) (guard 0))
        (card-games-bid--run g)
        (while (and (not (eq (card-games-get g :phase) 'gameover)) (< (cl-incf guard) 4000))
          (card-games-bid--deal g (mod (1+ (card-games-get g :dealer)) 4)) (card-games-bid--run g))
        (should (eq 'gameover (card-games-get g :phase)))))))

(ert-deftest cgt-bid-smart-beats-basic ()
  (let ((w0 0) (w1 0))
    (random "ert-h2h")
    (dotimes (_ 50)
      (let ((card-games-bid--human-seats nil)
            (card-games-bid-ai-policies (vector 'smart 'basic 'smart 'basic))
            (g (card-games-bid--deal (make-instance 'card-games-bid-game))) (guard 0))
        (card-games-bid--run g)
        (while (and (not (eq (card-games-get g :phase) 'gameover)) (< (cl-incf guard) 4000))
          (card-games-bid--deal g (mod (1+ (card-games-get g :dealer)) 4)) (card-games-bid--run g))
        (pcase (card-games-get g :game-over) (0 (cl-incf w0)) (1 (cl-incf w1)))))
    (should (> w0 (* 1.8 w1)))))

;;;; SVG

(ert-deftest cgt-svg-builds ()
  (should (stringp (card-games-svg-to-string
                    (card-games-svg-grid-svg (list (list (cons "A" 0) (cons nil 'joker) 'down nil))))))
  (should (stringp (card-games-svg-to-string
                    (card-games-bid--table-svg (card-games-bid--deal (make-instance 'card-games-bid-game)))))))

;;;; Chooser

(ert-deftest cgt-chooser-registry ()
  (should (assoc "500 (Bid)" card-games-list))
  (should (commandp 'card-games)))

(provide 'card-games-tests)
;;; card-games-tests.el ends here

(ert-deftest cgt-ai-step ()
  ;; ai-step advances within a hand; the caller deals the next hand at 'done.
  (let ((card-games-bid--human-seats nil)
        (g (card-games-bid--deal (make-instance 'card-games-bid-game))) (guard 0))
    (while (and (not (eq (card-games-get g :phase) 'gameover)) (< (cl-incf guard) 6000))
      (unless (card-games-bid--ai-step g)
        (card-games-bid--deal g (mod (1+ (card-games-get g :dealer)) 4))))
    (should (eq 'gameover (card-games-get g :phase)))))

(ert-deftest cgt-card-back-presets ()
  (dolist (card-games-svg-card-back '(dots rings solid))
    (should (stringp (card-games-svg-to-string
                      (card-games-svg-cards-svg (list 'down)))))))

(ert-deftest cgt-set-theme ()
  (let ((inhibit-message t))            ; keep the theme banner out of test output
    (card-games-set-theme 'dark)
    (should (equal "#23272e" card-games-bid-felt-color))
    (card-games-set-theme 'classic)
    (should (equal "#15692f" card-games-bid-felt-color))))

(ert-deftest cgt-scale ()
  (require 'face-remap)
  (let ((text-scale-mode-amount 0)) (should (= 1.0 (card-games-scale))))
  (let ((text-scale-mode-amount 2)) (should (> (card-games-scale) 1.0)))
  (let ((text-scale-mode-amount -2)) (should (< (card-games-scale) 1.0))))

(ert-deftest cgt-mode-line-announce ()
  (let ((g (card-games-bid--deal (make-instance 'card-games-bid-game))))
    (should (stringp (card-games-bid--mode-line g)))
    (should (string-match-p "bid" (card-games-bid--mode-line g)))))

(ert-deftest cgt-gaps-mode-line ()
  (let ((g (card-games-gaps--deal (make-instance 'card-games-montana-game))))
    (should (string-match-p "moves" (card-games-gaps--mode-line g)))))

(ert-deftest cgt-keys-default ()
  (should (eq card-games-keys 'emacs))
  (should (commandp 'card-games-gaps-redraw))
  (should (commandp 'card-games-bid-redraw))
  ;; g redraws (not new)
  (should (eq 'card-games-gaps-redraw (lookup-key card-games-gaps-mode-map "g")))
  (should (eq 'card-games-bid-redraw (lookup-key card-games-bid-mode-map "g"))))

(ert-deftest cgt-keys-classic ()
  ;; emacs map has no h/SPC binding; classic adds them
  (should-not (lookup-key card-games-gaps-mode-map "h"))
  (should (eq 'card-games-gaps-left (lookup-key (card-games-gaps--classic-keymap) "h")))
  (should (eq 'card-games-bid-left (lookup-key (card-games-bid--classic-keymap) "h"))))

(ert-deftest cgt-svgui-builds ()
  (let ((g (card-games-bid--deal (make-instance 'card-games-bid-game))))
    (card-games-put g :phase 'auction) (card-games-put g :bidder 0)
    (let ((sr (card-games-bid--ui-svg g)))
      (should (stringp (card-games-svg-to-string (car sr))))
      ;; available bids have clickable rects; a cell centre maps back to its bid
      (let* ((bids (plist-get (cdr sr) :bids)) (e (car bids))
             (r (cdr e)) (cx (+ (nth 0 r) (/ (nth 2 r) 2))) (cy (+ (nth 1 r) (/ (nth 3 r) 2))))
        (should (eq (car e) (card-games-bid--region-bid cx cy (cdr sr))))))))

(ert-deftest cgt-gaps-svgui-builds ()
  ;; the full-SVG gaps UI builds, and a cell centre maps back to its (row . col)
  (let ((g (card-games-gaps--deal (make-instance 'card-games-montana-game))))
    (let* ((sr (card-games-gaps--ui-svg g 820 380))
           (geom (plist-get (cdr sr) :board))
           (x0 (nth 0 geom)) (y0 (nth 1 geom))
           (cw (nth 2 geom)) (ch (nth 3 geom)) (gp (nth 4 geom)))
      (should (stringp (card-games-svg-to-string (car sr))))
      (should (plist-get (cdr sr) :new))   ; control regions present
      (should (equal (cons 2 3)
                     (card-games-gaps--ui-cell (+ x0 (* 3 (+ cw gp)) (/ cw 2))
                                       (+ y0 (* 2 (+ ch gp)) (/ ch 2))
                                       geom))))))

(ert-deftest cgt-classic-folds-controls ()
  ;; graphical classic UI: one action-button row, no textual key-help
  (with-temp-buffer
    (card-games-bid-mode)
    (setq card-games-bid--game (card-games-bid--deal (make-instance 'card-games-bid-game)))
    (let ((card-games-bid-svg-ui nil) (inhibit-read-only t))
      (card-games-bid--insert-graphical card-games-bid--game)
      (card-games-bid--insert-buttons card-games-bid--game))
    (let ((s (buffer-string)))
      (should-not (string-match-p "RET\\] play card" s))   ; key-help removed
      (should (string-match-p "Help" s))                    ; button row present
      (should-not (string-match-p "New" s)))))              ; no mid-hand new deal

;;;; Live 500 (networking)

(ert-deftest cgt-bid-net-filter ()
  "Per-seat 500 state hides others' cards and rotates the viewer South."
  (cl-flet ((ck (h) (sort (mapcar #'prin1-to-string h) #'string<)))
    (let* ((card-games-bid--human-seats '(0 1 2 3))
           (g (card-games-bid--deal (make-instance 'card-games-bid-game) 3))
           (st (card-games-net-game-state g 1)))      ; West's viewpoint
      ;; West sees its own ten cards rotated to index 0
      (should (equal (ck (aref (plist-get st :hands) 0))
                     (ck (card-games-bid--hand g 1))))
      ;; opponents are face-down counts only, not the real cards
      (should (= 10 (length (aref (plist-get st :hands) 1))))
      (should-not (equal (ck (aref (plist-get st :hands) 1))
                         (ck (card-games-bid--hand g 2))))
      ;; the kitty stays hidden
      (should-not (plist-get st :kitty))
      ;; South (abs 0) bids first; in West's frame that is seat 3
      (should (= 3 (plist-get st :bidder)))
      (should (= 1 (plist-get st :you))))))   ; :you is the absolute seat

(ert-deftest cgt-bid-net-apply ()
  "The host applies only legal, in-turn moves."
  (let* ((card-games-bid--human-seats '(0 1 2 3))
         (g (card-games-bid--deal (make-instance 'card-games-bid-game) 3)))
    (should-not (card-games-net-apply-move g 1 '(pass)))   ; not West's turn yet
    (should (card-games-net-apply-move g 0 '(pass)))       ; South may pass
    (should (aref (card-games-get g :passed) 0))
    (should-not (card-games-net-apply-move g 0 '(play (0 . 0)))) ; wrong phase
    ))

(ert-deftest cgt-bid-net-discard-checked ()
  "The host rejects a kitty discard of cards the seat does not hold.
Finding 3 of the 2026-07-29 review: the other move checks were real,
but a discard only counted its cards, and cl-set-difference silently
ignores cards that are not in the hand -- so five phantom discards
left a 15-card hand in play."
  (let* ((card-games-bid--human-seats '(0 1 2 3))
         (g (card-games-bid--deal (make-instance 'card-games-bid-game) 3)))
    ;; put the game where a discard is legal: West won the auction
    (card-games-put g :phase 'kitty)
    (card-games-put g :contractor 1)
    (card-games-bid--set-hand g 1 (append (card-games-get g :kitty) (card-games-bid--hand g 1)))
    (let* ((hand (copy-sequence (card-games-bid--hand g 1)))
           (foreign (cl-subseq (card-games-bid--hand g 2) 0 5)))
      ;; five cards West does not hold: refused, nothing moves
      (should-not (card-games-net-apply-move g 1 (cons 'discard foreign)))
      (should (equal hand (card-games-bid--hand g 1)))
      (should (eq 'kitty (card-games-get g :phase)))
      ;; one held card named five times: also refused
      (should-not (card-games-net-apply-move g 1
                                     (cons 'discard (make-list 5 (car hand)))))
      (should (eq 'kitty (card-games-get g :phase)))
      ;; an honest five from the hand is accepted and play begins
      (should (card-games-net-apply-move g 1 (cons 'discard (cl-subseq hand 0 5))))
      (should (eq 'play (card-games-get g :phase)))
      (should (= 10 (length (card-games-bid--hand g 1)))))))

(ert-deftest cgt-bid-net-loopback ()
  "A 500 move travels client -> host -> filtered broadcast over TCP."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe2" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 16) (accept-process-output nil 0.05))))
    (let* ((card-games-bid--human-seats '(0 1))
           (hgame (card-games-bid--deal (make-instance 'card-games-bid-game) 3))
           (srv (card-games-net-host-start hgame 0))
           (port (process-contact srv :service))
           (cgame (make-instance 'card-games-bid-game)))
      (setf (card-games-net-host-next-seat card-games-net--host) 1) ; reserve South for host
      (unwind-protect
          (progn
            (card-games-net-connect "127.0.0.1" port "P1" cgame)
            (pump)
            ;; the client (seat 1) sees its own ten cards at index 0
            (should (= 10 (length (aref (card-games-get cgame :hands) 0))))
            (should (equal
                     (sort (mapcar #'prin1-to-string (aref (card-games-get cgame :hands) 0)) #'string<)
                     (sort (mapcar #'prin1-to-string (card-games-bid--hand hgame 1)) #'string<)))
            ;; make it the client's turn, then let it pass
            (card-games-put hgame :bidder 1)
            (card-games-net-host-broadcast)
            (pump)
            (should (= 0 (card-games-get cgame :bidder)))  ; rotated: West sees itself bidding
            (card-games-net-send-move '(pass))
            (pump)
            (should (aref (card-games-get hgame :passed) 1)))
        (card-games-net-disconnect)
        (card-games-net-host-stop)))))


;;;; --- New games added 2026-06-24: solitaires, trick games, Crazy Eights ---

(ert-deftest cgt-sol-deck ()
  (should (= 52 (length (card-games-sol--make-deck 1))))
  (should (= 104 (length (card-games-sol--make-deck 2)))))

(ert-deftest cgt-sol-klondike-deal ()
  (let* ((g (card-games-sol--deal (card-games-klondike-game))))
    (should (= 7 (oref g ncols)))
    (dotimes (c 7)
      (should (= (1+ c) (length (card-games-sol--col g c))))
      (should (= c (card-games-sol--down g c)))
      ;; top card is face up
      (should (card-games-sol--col-top g c)))
    (should (= 24 (length (card-games-get g :stock))))
    (should (cl-every #'null (append (card-games-get g :found) nil)))))

(ert-deftest cgt-sol-freecell-deal ()
  (let* ((g (card-games-sol--deal (card-games-freecell-game)))
         (total 0))
    (should (= 8 (oref g ncols)))
    (dotimes (c 8) (cl-incf total (length (card-games-sol--col g c)))
             (should (= 0 (card-games-sol--down g c))))
    (should (= 52 total))
    (should (null (card-games-get g :stock)))
    (should (= 4 (length (card-games-get g :free))))))

(ert-deftest cgt-sol-spider-deal ()
  (let* ((g (card-games-sol--deal (card-games-spider-game)))
         (total 0))
    (should (= 10 (oref g ncols)))
    (dotimes (c 10) (cl-incf total (length (card-games-sol--col g c))))
    (should (= 54 total))
    (should (= 50 (length (card-games-get g :stock))))
    (dotimes (c 4) (should (= 6 (length (card-games-sol--col g c)))))
    (dotimes (k 6) (should (= 5 (length (card-games-sol--col g (+ 4 k))))))))

(ert-deftest cgt-sol-rules-alt ()
  (let ((g (card-games-klondike-game)))
    ;; red 6 onto black 7 ok; black 6 onto black 7 no
    (should (card-games-sol--place-p g '(0 . 6) '(3 . 5)))      ; 7s under 6h
    (should-not (card-games-sol--place-p g '(0 . 6) '(1 . 5)))  ; 6c on 7s same color
    (should (card-games-sol--empty-accepts g '(0 . 12)))        ; king
    (should-not (card-games-sol--empty-accepts g '(0 . 11)))))  ; queen

(ert-deftest cgt-sol-rules-spider ()
  (let ((g (card-games-spider-game)))
    ;; build down any suit
    (should (card-games-sol--place-p g '(0 . 6) '(3 . 5)))
    (should (card-games-sol--place-p g '(0 . 6) '(1 . 5)))
    ;; run cohesion requires same suit
    (should (card-games-sol--link-p g '(0 . 6) '(0 . 5)))
    (should-not (card-games-sol--link-p g '(0 . 6) '(1 . 5)))
    (should (card-games-sol--empty-accepts g '(0 . 3)))))

(ert-deftest cgt-sol-top-run ()
  (let ((g (card-games-klondike-game)))
    (card-games-put g :tableau (vector (list '(0 . 9) '(2 . 8) '(1 . 7))))  ; 9s 8d 7c
    (card-games-put g :down (vector 0))
    ;; 9s(black) 8d(red) 7c(black) is a valid alt run of 3
    (should (= 3 (length (card-games-sol--top-run g 0))))
    ;; break color: 9s 8d 7d -> only 8d 7d? 7d red on 8d red invalid -> run is just 7d
    (card-games-put g :tableau (vector (list '(0 . 9) '(2 . 8) '(2 . 7))))
    (should (= 1 (length (card-games-sol--top-run g 0))))))

(ert-deftest cgt-sol-move-col ()
  (let ((g (card-games-sol--deal (card-games-klondike-game))))
    ;; craft: col0 top = 7c(black), col1 top = 6h(red); move 6h onto 7c
    (card-games-put g :tableau (vector (list '(1 . 7)) (list '(3 . 6)) nil nil nil nil nil))
    (card-games-put g :down (vector 0 0 0 0 0 0 0))
    (let ((cards (last (card-games-sol--col g 1) 1)))
      (should (card-games-sol--can-drop g '(col . 0) cards))
      (card-games-sol--take g '(col . 1) 1)
      (card-games-sol--drop g '(col . 0) cards))
    (should (equal '((1 . 7) (3 . 6)) (card-games-sol--col g 0)))
    (should (null (card-games-sol--col g 1)))))

(ert-deftest cgt-sol-foundation-and-win ()
  (let ((g (card-games-sol--deal (card-games-klondike-game))))
    ;; empty foundations: place an Ace then a 2 of same suit
    (should (card-games-sol--found-accepts g 0 '(0 . 0)))
    (card-games-sol--drop g '(found . 0) (list '(0 . 0)))
    (should (card-games-sol--found-accepts g 0 '(0 . 1)))
    (should-not (card-games-sol--found-accepts g 0 '(1 . 1)))
    ;; build a winning state: fill all four foundations A..K
    (let ((found (make-vector 4 nil)))
      (dotimes (s 4)
        (aset found s (cl-loop for r below 13 collect (cons s r))))
      (card-games-put g :found found))
    (should (card-games-won-p g))))

(ert-deftest cgt-sol-spider-harvest ()
  (let ((g (card-games-sol--deal (card-games-spider-game))))
    ;; put a complete K..A spade run as the whole of column 0
    (card-games-put g :tableau (let ((v (card-games-get g :tableau)))
                         (aset v 0 (cl-loop for r from 12 downto 0 collect (cons 0 r)))
                         v))
    (card-games-put g :down (let ((v (card-games-get g :down))) (aset v 0 0) v))
    (card-games-put g :sets 0)
    (card-games-sol--harvest g)
    (should (= 1 (card-games-get g :sets)))
    (should (null (card-games-sol--col g 0)))))

(ert-deftest cgt-sol-undo ()
  (let ((g (card-games-sol--deal (card-games-klondike-game))))
    (card-games-put g :tableau (vector (list '(1 . 7)) (list '(3 . 6)) nil nil nil nil nil))
    (card-games-put g :down (vector 0 0 0 0 0 0 0))
    (card-games-sol--snapshot g)
    (let ((cards (last (card-games-sol--col g 1) 1)))
      (card-games-sol--take g '(col . 1) 1)
      (card-games-sol--drop g '(col . 0) cards))
    (should (null (card-games-sol--col g 1)))
    (should (card-games-sol--restore g))
    (should (equal '((3 . 6)) (card-games-sol--col g 1)))))

(ert-deftest cgt-sol-render-builds ()
  (dolist (cls '(card-games-klondike-game card-games-freecell-game card-games-spider-game card-games-yukon-game))
    (let ((g (card-games-sol--deal (make-instance cls))))
      (should (stringp (card-games-render g))))))

(defun cgt--init-hearts ()
  (let ((g (card-games-hearts-game)))
    (card-games-put g :scores (make-vector 4 0))
    (card-games-trick--start-hand g) g))

(defun cgt--init-spades ()
  (let ((g (card-games-spades-game)))
    (card-games-put g :scores (make-vector 4 0))
    (card-games-put g :bags (make-vector 2 0))
    (card-games-put g :dealer 3)
    (card-games-trick--deal g)
    (card-games-put g :bids (let ((v (make-vector 4 0)))
                      (dotimes (s 4) (aset v s (card-games-trick--ai-bid g s))) v))
    (card-games-trick--leader-init g) g))

(ert-deftest cgt-trick-deal ()
  (let ((g (card-games-trick--deal (card-games-hearts-game))))
    (let ((tot 0))
      (dotimes (s 4) (cl-incf tot (length (card-games-trick--hand g s))))
      (should (= 52 tot)))
    (dotimes (s 4) (should (= 13 (length (card-games-trick--hand g s)))))))

(ert-deftest cgt-trick-winner-trump ()
  (let ((g (card-games-spades-game)))
    ;; play order: S leads 10h, W 13h(K), N 2s(trump), E 12h
    (card-games-put g :trick (list (cons 3 '(3 . 11)) (cons 2 '(0 . 0))
                           (cons 1 '(3 . 12)) (cons 0 '(3 . 8))))
    ;; :trick is stored reversed (newest first); winner = North (trump)
    (should (= 2 (card-games-trick--winner g)))))

(ert-deftest cgt-trick-winner-notrump ()
  (let ((g (card-games-hearts-game)))
    ;; led hearts; highest heart wins (no trump)
    (card-games-put g :trick (list (cons 3 '(1 . 12)) (cons 2 '(3 . 12))
                           (cons 1 '(3 . 5)) (cons 0 '(3 . 8))))
    ;; North played Ace of hearts (3 . 12) -> winner North
    (should (= 2 (card-games-trick--winner g)))))

(ert-deftest cgt-hearts-first-must-be-2c ()
  (let* ((g (cgt--init-hearts))
         (leader (card-games-get g :turn)))
    (let ((moves (card-games-trick--legal-moves g leader)))
      (should (equal moves '((1 . 0)))))))

(ert-deftest cgt-hearts-full-hand ()
  (let ((g (cgt--init-hearts)))
    (card-games-trick--simulate-hand g)
    ;; 13 tricks distributed
    (should (= 13 (apply #'+ (append (card-games-get g :tricks) nil))))
    ;; total points across players is 26 (no moon) or 78 (moon: 3*26)
    (let ((tot (apply #'+ (append (card-games-get g :scores) nil))))
      (should (memq tot '(26 78))))))

(ert-deftest cgt-hearts-many-hands ()
  (let ((g (cgt--init-hearts)) (n 0))
    (while (and (not (card-games-trick--game-over-p g)) (< n 60))
      (card-games-trick--simulate-hand g)
      (cl-incf n)
      (unless (card-games-trick--game-over-p g) (card-games-trick--start-hand g)))
    (should (card-games-trick--game-over-p g))
    (should (integerp (card-games-trick--winner-seat g)))))

(ert-deftest cgt-spades-bid-range ()
  (let ((g (card-games-trick--deal (card-games-spades-game))))
    (dotimes (s 4)
      (let ((b (card-games-trick--ai-bid g s)))
        (should (and (>= b 1) (<= b 13)))))))

(ert-deftest cgt-spades-full-hand ()
  (let ((g (cgt--init-spades)))
    (card-games-trick--simulate-hand g)
    (should (= 13 (apply #'+ (append (card-games-get g :tricks) nil))))
    ;; teammates share a score
    (should (= (aref (card-games-get g :scores) 0) (aref (card-games-get g :scores) 2)))
    (should (= (aref (card-games-get g :scores) 1) (aref (card-games-get g :scores) 3)))))

(ert-deftest cgt-spades-full-game ()
  (let ((g (cgt--init-spades)) (n 0))
    (while (and (not (card-games-trick--game-over-p g)) (< n 80))
      (card-games-trick--simulate-hand g)
      (cl-incf n)
      (unless (card-games-trick--game-over-p g)
        (card-games-trick--deal g)
        (card-games-put g :bids (let ((v (make-vector 4 0)))
                          (dotimes (s 4) (aset v s (card-games-trick--ai-bid g s))) v))
        (card-games-trick--leader-init g)))
    (should (card-games-trick--game-over-p g))))

(ert-deftest cgt-trick-ui-new-and-render ()
  (dolist (cls '(card-games-hearts-game card-games-spades-game))
    (let ((noninteractive t)
          (g (make-instance cls)))
      (card-games-trick--new g)            ; spades bids via ai (noninteractive), hearts -> pass phase
      (should (stringp (card-games-render g)))
      (should (memq (card-games-get g :phase) '(pass play))))))
(ert-deftest cgt-trick-ui-hearts-pass ()
  (let* ((noninteractive t) (g (make-instance 'card-games-hearts-game)))
    (card-games-trick--new g)
    ;; hand 1 passes left; mark 3 cards from South and pass
    (when (eq (card-games-get g :phase) 'pass)
      (card-games-put g :marks (cl-subseq (card-games-trick--sort (card-games-trick--hand g 0)) 0 3))
      (card-games-trick--do-pass g)
      (should (eq (card-games-get g :phase) 'play))
      ;; cards are conserved: hands plus the cards already played this trick
      (let ((tot (length (card-games-get g :trick))))
        (dotimes (s 4) (cl-incf tot (length (card-games-trick--hand g s))))
        (should (= 52 tot))))))
(ert-deftest cgt-trick-ui-spades-human-play ()
  (let* ((noninteractive t) (g (make-instance 'card-games-spades-game)))
    (card-games-trick--new g)              ; runs AI until South's turn
    (should (eq (card-games-get g :phase) 'play))
    (should (= 0 (card-games-get g :turn)))
    ;; play a legal card for South, then run; eventually hand completes/scores
    (let ((guard 0))
      (while (and (eq (card-games-get g :phase) 'play) (< guard 20)
                  (= 0 (card-games-get g :turn)))
        (let ((card (car (card-games-trick--legal-moves g 0))))
          (card-games-trick--play g 0 card)
          (card-games-trick--run g))
        (cl-incf guard)))
    (should (vectorp (card-games-get g :scores)))))

(ert-deftest cgt-eights-deal ()
  (let* ((card-games-eights-players 3) (g (card-games-eights--deal (card-games-eights-game))))
    (should (= 3 (card-games-get g :nplayers)))
    (dotimes (s 3) (should (= 5 (length (card-games-eights--hand g s)))))
    (should (card-games-eights--top g))
    (should-not (= card-games-eights--wild (cdr (card-games-eights--top g))))))  ; starter not an eight
(ert-deftest cgt-eights-legal ()
  (let ((g (card-games-eights-game)))
    (card-games-put g :discard (list '(0 . 3))) (card-games-put g :suit 0)
    (should (card-games-eights--legal-p g '(0 . 8)))      ; same suit (spades)
    (should (card-games-eights--legal-p g '(1 . 3)))      ; same rank
    (should (card-games-eights--legal-p g '(2 . 6)))      ; eight (wild)
    (should-not (card-games-eights--legal-p g '(1 . 4))))) ; neither
(ert-deftest cgt-eights-full-game ()
  (let* ((card-games-eights-players 4) (noninteractive t) (g (card-games-eights--deal (card-games-eights-game)))
         (guard 0))
    ;; drive entirely by AI from every seat
    (while (and (eq (card-games-get g :phase) 'play) (< guard 2000))
      (card-games-eights--ai-turn g (card-games-get g :turn))
      (when (>= (card-games-get g :passes) (card-games-get g :nplayers)) (card-games-eights--deadlock g))
      (cl-incf guard))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (integerp (card-games-get g :winner)))))
(ert-deftest cgt-eights-wild-sets-suit ()
  (let* ((card-games-eights-players 2) (g (card-games-eights--deal (card-games-eights-game))))
    (card-games-eights--set-hand g 0 (list '(3 . 6)))   ; the human holds only an eight
    (card-games-put g :discard (list '(0 . 3))) (card-games-put g :suit 0)
    (card-games-eights--play g 0 '(3 . 6) 2)            ; play it, name diamonds (2)
    (should (= 2 (card-games-get g :suit)))
    (should (eq (card-games-get g :phase) 'game-over))))  

;;;; --- Wave 2 (2026-06-24): Forty Thieves/Scorpion/Canfield, Golf/TriPeaks/Pyramid, Whist/Oh Hell, President ---

(ert-deftest cgt-sol-forty-deal ()
  (let ((g (card-games-sol--deal (card-games-forty-game))) (tot 0))
    (should (= 10 (oref g ncols)))
    (dotimes (c 10) (should (= 4 (length (card-games-sol--col g c)))) (cl-incf tot 4))
    (should (= 8 (oref g nfound)))
    (should (= 64 (length (card-games-get g :stock))))
    (should (= 104 (+ tot (length (card-games-get g :stock)))))))

(ert-deftest cgt-sol-forty-no-redeal ()
  (let ((g (card-games-sol--deal (card-games-forty-game))))
    (card-games-put g :stock nil) (card-games-put g :waste '((0 . 0) (1 . 1)))
    (card-games-sol--stock-action g)            ; redeal nil -> stays empty
    (should (null (card-games-get g :stock)))))

(ert-deftest cgt-sol-forty-win ()
  (let ((g (card-games-sol--deal (card-games-forty-game))) (found (make-vector 8 nil)))
    (dotimes (i 8) (aset found i (cl-loop for r below 13 collect (cons (mod i 4) r))))
    (card-games-put g :found found)
    (should (card-games-won-p g))))

(ert-deftest cgt-sol-scorpion-deal ()
  (let ((g (card-games-sol--deal (card-games-scorpion-game))) (tot 0))
    (should (= 7 (oref g ncols)))
    (dotimes (c 7) (should (= 7 (length (card-games-sol--col g c)))) (cl-incf tot 7))
    (dotimes (c 4) (should (= 3 (card-games-sol--down g c))))
    (dotimes (k 3) (should (= 0 (card-games-sol--down g (+ 4 k)))))
    (should (= 3 (length (card-games-get g :stock))))
    (should (= 0 (oref g nfound)))))

(ert-deftest cgt-sol-scorpion-harvest-win ()
  (let ((g (card-games-sol--deal (card-games-scorpion-game))))
    (card-games-put g :sets 3)
    ;; place a complete K..A clubs run as column 0
    (aset (card-games-get g :tableau) 0 (cl-loop for r from 12 downto 0 collect (cons 1 r)))
    (aset (card-games-get g :down) 0 0)
    (card-games-sol--harvest g)
    (should (= 4 (card-games-get g :sets)))
    (should (card-games-won-p g))))

(ert-deftest cgt-sol-canfield-deal ()
  (let ((g (card-games-sol--deal (card-games-canfield-game))))
    (should (= 13 (length (card-games-get g :reserve))))
    (should (= 1 (length (aref (card-games-get g :found) 0))))
    (dotimes (c 4) (should (= 1 (length (card-games-sol--col g c)))))
    (should (= 34 (length (card-games-get g :stock))))
    ;; base equals the rank of the first foundation card
    (should (= (oref g base) (cdr (car (aref (card-games-get g :found) 0)))))))

(ert-deftest cgt-sol-canfield-base-wrap ()
  (let ((g (card-games-canfield-game)))
    (oset g base 5) (oset g wrap t)
    (card-games-put g :found (make-vector 4 nil))
    (should (card-games-sol--found-accepts g 1 '(2 . 5)))       ; empty -> base rank 5
    (should-not (card-games-sol--found-accepts g 1 '(2 . 6)))
    ;; wrap: a King on top accepts the Ace next
    (aset (card-games-get g :found) 0 (list '(0 . 12)))
    (should (card-games-sol--found-accepts g 0 '(0 . 0)))))

(ert-deftest cgt-sol-canfield-autofill ()
  (let ((g (card-games-sol--deal (card-games-canfield-game))))
    (let ((rlen (length (card-games-get g :reserve))))
      (aset (card-games-get g :tableau) 0 nil)   ; empty a column
      (card-games-sol--autofill g)
      (should (= 1 (length (card-games-sol--col g 0))))
      (should (= (1- rlen) (length (card-games-get g :reserve)))))))

(ert-deftest cgt-sol-russian-bank-deal ()
  (let ((g (card-games-sol--deal (card-games-russian-bank-game))))
    (should (= 8 (oref g ncols)))
    (should (= 13 (length (card-games-get g :reserve))))
    (dotimes (c 8) (should (= 1 (length (card-games-sol--col g c)))))
    (dotimes (i 4) (should (null (aref (card-games-get g :found) i))))
    (should (= 31 (length (card-games-get g :stock))))   ; 52 - 13 reserve - 8 houses
    ;; Houses build down by alternating colour; foundations up by suit.
    (should (card-games-sol--place-p g '(0 . 6) '(2 . 5)))       ; 7S <- 6D (down, alt)
    (should-not (card-games-sol--place-p g '(0 . 6) '(1 . 5)))   ; 7S <- 6C (same colour)
    (should (card-games-sol--found-accepts g 0 '(3 . 0)))        ; empty foundation takes an Ace
    (should-not (card-games-sol--found-accepts g 0 '(3 . 1)))))  ; but not a Two

(ert-deftest cgt-sol-russian-bank-autofill ()
  (let ((g (card-games-sol--deal (card-games-russian-bank-game))))
    (let ((rlen (length (card-games-get g :reserve))))
      (aset (card-games-get g :tableau) 0 nil)   ; empty a house
      (card-games-sol--autofill g)
      (should (= 1 (length (card-games-sol--col g 0))))
      (should (= (1- rlen) (length (card-games-get g :reserve)))))))

;;;; Russian Bank duel (Crapette)

(ert-deftest cgt-crap-deal ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (dotimes (p 2)
      (should (= 13 (length (card-games-crap--reserve g p))))
      (should (= 35 (length (card-games-crap--hand g p))))
      (should (null (card-games-crap--waste g p))))
    (dotimes (i 8) (should (= 1 (length (card-games-crap--house g i)))))
    (dotimes (i 8) (should (null (card-games-crap--found g i))))
    (should (= 0 (card-games-get g :turn)))))

(ert-deftest cgt-crap-legality ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :found) 0 nil)
    (should (card-games-crap--found-accepts g 0 '(3 . 0)))       ; empty -> Ace
    (should-not (card-games-crap--found-accepts g 0 '(3 . 1)))   ; not a Two
    (aset (card-games-get g :found) 0 (list '(3 . 0)))
    (should (card-games-crap--found-accepts g 0 '(3 . 1)))       ; up in suit
    (should-not (card-games-crap--found-accepts g 0 '(2 . 1)))   ; wrong suit
    (aset (card-games-get g :houses) 0 (list '(0 . 6)))          ; 7 of spades (black)
    (should (card-games-crap--house-accepts g 0 '(2 . 5)))       ; 6 of diamonds (red)
    (should-not (card-games-crap--house-accepts g 0 '(1 . 5)))   ; 6 of clubs (black)
    (should (card-games-crap--load-accepts '(2 . 5) '(2 . 6)))   ; same suit, one up
    (should (card-games-crap--load-accepts '(2 . 5) '(2 . 4)))   ; same suit, one down
    (should-not (card-games-crap--load-accepts '(2 . 5) '(3 . 6))) ; wrong suit
    (should-not (card-games-crap--load-accepts '(2 . 5) '(2 . 7))))) ; not adjacent

(ert-deftest cgt-crap-win ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 0 nil)
    (aset (card-games-get g :hand) 0 nil)
    (aset (card-games-get g :waste) 0 nil)
    (should (card-games-crap--won-p g 0))
    (should-not (card-games-crap--won-p g 1))))

(ert-deftest cgt-crap-ai-turn ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (cl-flet ((total ()
                (let ((n 0))
                  (dotimes (i 8)
                    (setq n (+ n (length (card-games-crap--found g i))
                              (length (card-games-crap--house g i)))))
                  (dotimes (p 2)
                    (setq n (+ n (length (card-games-crap--reserve g p))
                              (length (card-games-crap--waste g p))
                              (length (card-games-crap--hand g p)))))
                  n)))
      (should (= 104 (total)))
      (card-games-put g :turn 1)
      (let ((card-games-crap--recording nil)) (card-games-crap--ai-play g))
      (should (= 104 (total)))              ; the AI turn conserves every card
      (should-not (eq (card-games-get g :winner) 0))))) ; the AI cannot make YOU win

(ert-deftest cgt-crap-ai-plays-foundation ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 1 (list '(0 . 0)))   ; AI reserve top = Ace of spades
    (aset (card-games-get g :waste) 1 nil)
    (aset (card-games-get g :hand) 1 nil)
    (aset (card-games-get g :found) 0 nil)
    (card-games-put g :turn 1)
    (let ((card-games-crap--recording nil)) (card-games-crap--ai-play g))
    (should (eq (card-games-get g :winner) 1))))     ; plays its last card, wins

(ert-deftest cgt-crap-ai-enabling ()
  ;; reserve 5D cannot be placed until the AI shifts 6H off 6S (crafty rearrange)
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 1 (list '(2 . 4)))            ; 5 of diamonds
    (aset (card-games-get g :waste) 1 nil) (aset (card-games-get g :hand) 1 nil)
    (aset (card-games-get g :reserve) 0 nil) (aset (card-games-get g :waste) 0 nil)
    (aset (card-games-get g :found) 0 nil)
    (aset (card-games-get g :houses) 0 (list '(0 . 5) '(3 . 5)))    ; 6S (bottom), 6H (top)
    (aset (card-games-get g :houses) 1 (list '(1 . 6)))             ; 7C
    (dotimes (k 6) (aset (card-games-get g :houses) (+ 2 k) (list '(2 . 6)))) ; 7D fillers, none empty
    (card-games-put g :turn 1)
    (let ((card-games-ai-level 'hard) (card-games-crap--recording nil)) (card-games-crap--ai-play g))
    (should (null (card-games-crap--reserve g 1)))))                ; hard unstuck and unloaded

(ert-deftest cgt-crap-ai-loads-you ()
  ;; reserve 6D loads onto your reserve top 7D; the AI should prefer that
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 1 (list '(2 . 5)))            ; 6D
    (aset (card-games-get g :waste) 1 nil) (aset (card-games-get g :hand) 1 nil)
    (aset (card-games-get g :reserve) 0 (list '(2 . 6)))            ; your reserve top 7D
    (aset (card-games-get g :waste) 0 nil) (aset (card-games-get g :found) 0 nil)
    (dotimes (k 8) (aset (card-games-get g :houses) k (list '(1 . 10)))) ; nowhere on a house, none empty
    (card-games-put g :turn 1)
    (let ((card-games-crap--recording nil)) (card-games-crap--ai-play g))
    (should (null (card-games-crap--reserve g 1)))
    (should (member '(2 . 5) (card-games-crap--reserve g 0)))))     ; loaded onto you

(ert-deftest cgt-svg-logo-smoke ()
  (dolist (choice '(modern classic gnu splash drawn none nonexistent))
    (let ((card-games-svg-emacs-logo choice) (svg (svg-create 200 120)))
      (card-games-svg-draw-logo svg 100 60 1.0)
      (should (imagep (svg-image svg))))))

(ert-deftest cgt-svg-card-back-smoke ()
  (dolist (b '(dots rings solid lattice waves diamond
               emacs emacs-classic gnu splash random))
    (let ((card-games-svg-card-back b) (svg (svg-create 80 100)))
      (card-games-svg-card svg 12 10 :down t)
      (should (imagep (svg-image svg)))))
  (card-games-svg--roll-back)
  (should (memq card-games-svg--random-back card-games-svg--card-backs)))

(ert-deftest cgt-crap-house-run ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; 9D 8S 7H
    (should (= 3 (length (card-games-crap--house-run g 0))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(0 . 6)))  ; 7S breaks colour
    (should (= 1 (length (card-games-crap--house-run g 0))))))

(ert-deftest cgt-crap-sequence-move ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; 9D 8S 7H
    (aset (card-games-get g :houses) 1 (list '(1 . 9)))                    ; 10C
    (dotimes (i 6) (aset (card-games-get g :houses) (+ 2 i) nil))          ; 6 empty houses
    (should (eq t (card-games-crap--house-move g 0 1)))
    (should (= 0 (length (card-games-crap--house g 0))))
    (should (= 4 (length (card-games-crap--house g 1))))
    (should (equal '(3 . 6) (card-games-crap--top (card-games-crap--house g 1))))))

(ert-deftest cgt-crap-sequence-space ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; 3-card run
    (aset (card-games-get g :houses) 1 (list '(1 . 9)))                    ; 10C
    (dotimes (i 6) (aset (card-games-get g :houses) (+ 2 i) (list '(0 . 0)))) ; no empty houses
    (should (eq 'space (card-games-crap--house-move g 0 1)))               ; too big to relay
    (should (= 3 (length (card-games-crap--house g 0))))))                 ; unchanged

(ert-deftest cgt-crap-stop ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 0 (list '(3 . 0)))   ; your reserve top is an Ace
    (aset (card-games-get g :found) 0 nil)
    (should (card-games-crap--forced g 0))
    (let ((card-games-crapette-stops nil))                 ; assist: block, no penalty
      (card-games-put g :turn 0)
      (should (card-games-crap--stop g))
      (should (= 0 (card-games-get g :turn)))
      (should (string-match-p "foundation first" (card-games-get g :message))))
    (let ((card-games-crapette-stops t))                   ; competitive: STOP ends your turn
      (card-games-put g :turn 0)
      (should (card-games-crap--stop g))
      (should (string-match-p "STOP" (card-games-get g :message))))))

(ert-deftest cgt-crap-sequence-partial ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; 9D 8S 7H
    (aset (card-games-get g :houses) 1 nil)                                ; empty destination
    (dotimes (i 6) (aset (card-games-get g :houses) (+ 2 i) nil))          ; plenty of space
    (should (eq t (card-games-crap--house-move g 0 1 2)))                  ; move only the top 2
    (should (equal '(2 . 8) (card-games-crap--top (card-games-crap--house g 0))))  ; 9D stays behind
    (should (= 2 (length (card-games-crap--house g 1))))
    (should (equal '(0 . 7) (car (card-games-crap--house g 1))))          ; 8S is the moved bottom
    (should (equal '(3 . 6) (card-games-crap--top (card-games-crap--house g 1)))))) ; 7H the top

(ert-deftest cgt-crap-partial-space ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; run of 3
    (aset (card-games-get g :houses) 1 nil)                                ; the only empty house
    (dotimes (i 6) (aset (card-games-get g :houses) (+ 2 i) (list '(0 . 0))))
    (should (eq 'space (card-games-crap--house-move g 0 1 3)))             ; 3 needs more space
    (should (eq t (card-games-crap--house-move g 0 1 1)))                  ; but 1 fits
    (should (= 2 (length (card-games-crap--house g 0))))
    (should (= 1 (length (card-games-crap--house g 1))))))

(ert-deftest cgt-crap-svg-smoke ()
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(2 . 8) '(0 . 7) '(3 . 6)))  ; a run to draw
    (aset (card-games-get g :reserve) 0 (list '(3 . 0)))                   ; a forced foundation
    (card-games-put g :sel '(house . 0)) (card-games-put g :sel-n 2) (card-games-put g :cursor 3)
    (let ((res (card-games-crap--svg g)))
      (should (stringp (car res)))
      (should (get-text-property 0 'display (car res)))
      (should (get-text-property 0 'card-games-regions (car res)))
      ;; every clickable region is a (RECT . SPOT) with a 4-number RECT
      (should (cl-every (lambda (r) (= 4 (length (car r)))) (cdr res)))
      (should (rassoc '(house . 0) (cdr res))))))

(ert-deftest cgt-trick-svg-smoke ()
  (let ((g (card-games-hearts-game)))
    (card-games-trick--deal g)
    (card-games-put g :cursor 0) (card-games-put g :phase 'play) (card-games-put g :turn 0) (card-games-put g :message "x")
    (card-games-put g :trick (list (cons 1 (car (card-games-trick--hand g 1)))))   ; West has led
    (should (stringp (card-games-trick--render-text g)))
    (let ((s (card-games-trick--svg g)))
      (should (stringp s))
      (should (get-text-property 0 'display s))
      (let ((regs (get-text-property 0 'card-games-regions s)))
        (should regs)
        (should (rassoc '(hand . 0) regs))
        (should (cl-every (lambda (r) (= 4 (length (car r)))) regs))))))

(ert-deftest cgt-trick-svg-bidding ()
  (let ((g (card-games-spades-game)))
    (card-games-trick--deal g)
    (card-games-put g :cursor 2) (card-games-put g :phase 'bid) (card-games-put g :turn 0) (card-games-put g :message "bid")
    (card-games-put g :bids (vector 3 nil 4 nil)) (card-games-put g :scores (vector 100 50 100 50))
    (should (stringp (card-games-trick--svg g)))))

(ert-deftest cgt-gin-svg-smoke ()
  (let ((g (card-games-gin-game)))
    (card-games-gin--deal g)
    (card-games-put g :cursor 0) (card-games-put g :message "x") (card-games-put g :scores (vector 0 0))
    (should (stringp (card-games-gin--render-text g)))
    (let ((sres (card-games-gin--svg g)))
      (should (get-text-property 0 'display sres))
      (should (rassoc '(hand . 0) (get-text-property 0 'card-games-regions sres))))))

(ert-deftest cgt-tm-svg-smoke ()
  (let ((g (card-games-rum500-game)))
    (card-games-tm--deal g)
    (card-games-put g :cursor 1) (card-games-put g :message "x")
    (card-games-put g :table (list (cons 0 (list '(0 . 0) '(0 . 1) '(0 . 2)))
                           (cons 2 (list '(1 . 5) '(2 . 5) '(3 . 5)))))
    (card-games-put g :marks (list 0 2))
    (should (stringp (card-games-tm--render-text g)))
    (let ((regs (get-text-property 0 'card-games-regions (card-games-tm--svg g))))
      (should (rassoc '(hand . 0) regs))
      (should (cl-every (lambda (r) (= 4 (length (car r)))) regs)))))

(ert-deftest cgt-hf-svg-smoke ()
  (let ((g (card-games-handfoot-game)))
    (card-games-put g :nplayers 4) (card-games-put g :nteams 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-hf--deal g)
    (card-games-put g :cursor 0) (card-games-put g :message "x")
    (should (stringp (card-games-hf--render-text g)))
    (let ((regs (get-text-property 0 'card-games-regions (card-games-hf--svg g))))
      (should (rassoc '(hand . 0) regs))
      (should (cl-every (lambda (r) (= 4 (length (car r)))) regs)))))

(ert-deftest cgt-fish-svg-smoke ()
  (let ((g (card-games-scopa-game)))
    (card-games-fish--deal-round g)
    (card-games-put g :cursor 0) (card-games-put g :message "x")
    (should (stringp (card-games-fish--render-text g)))
    (should (rassoc '(hand . 0)
                    (get-text-property 0 'card-games-regions (card-games-fish--svg g))))))

(ert-deftest cgt-eights-svg-smoke ()
  (let ((g (card-games-eights-game)))
    (card-games-put g :nplayers 3) (card-games-put g :scores (make-vector 3 0))
    (card-games-eights--deal g) (card-games-put g :cursor 0) (card-games-put g :message "x")
    (should (stringp (card-games-eights--render-text g)))
    (let ((regs (get-text-property 0 'card-games-regions (card-games-eights--board-svg g))))
      (should (rassoc '(hand . 0) regs))
      (should (cl-every (lambda (r) (= 4 (length (car r)))) regs)))))

(ert-deftest cgt-spite-svg-smoke ()
  (let ((g (card-games-spite-game)))
    (card-games-put g :nplayers 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-spite--deal g) (card-games-put g :cursor 0) (card-games-put g :message "x")
    (aset (card-games-get g :center) 0 (cons 3 (list '(1 . 3))))
    (should (stringp (card-games-spite--render-text g)))
    (should (rassoc '(hand . 0)
                    (get-text-property 0 'card-games-regions (card-games-spite--board-svg g))))))

(ert-deftest cgt-om-svg-smoke ()
  (let ((g (card-games-old-maid-game)))
    (card-games-om--deal g)
    (card-games-put g :phase 'play) (card-games-put g :turn 0) (card-games-put g :message "x")
    (should (stringp (card-games-om--render-text g)))
    (should (stringp (card-games-om--svg g)))))

(ert-deftest cgt-crib-svg-smoke ()
  (let ((g (card-games-cribbage-game)))
    (card-games-put g :scores (make-vector 2 0))
    (card-games-crib--deal g)
    (card-games-put g :cursor 0) (card-games-put g :message "x")
    (should (stringp (card-games-crib--render-text g)))
    (let ((regs (get-text-property 0 'card-games-regions (card-games-crib--svg g))))
      (should (rassoc '(hand . 0) regs))
      (should (cl-every (lambda (r) (= 4 (length (car r)))) regs))))
  (let ((g (card-games-cribbage-game)))
    (card-games-put g :scores (vector 60 90))
    (card-games-crib--deal g)
    (card-games-put g :phase 'play) (card-games-put g :starter '(2 . 4)) (card-games-put g :total 15)
    (card-games-put g :play (vector (card-games-crib--hand g 0) (card-games-crib--hand g 1)))
    (card-games-put g :seq (list '(0 . 6) '(3 . 8))) (card-games-put g :message "x")
    (should (stringp (card-games-crib--svg g)))))

(ert-deftest cgt-bridge-svg-smoke ()
  (let ((g (card-games-bridge-game)))
    (card-games-bridge--deal g)
    (card-games-put g :cursor 0) (card-games-put g :turn 0) (card-games-put g :bid-level 1) (card-games-put g :bid-strain 0)
    (card-games-put g :message "x")
    (should (stringp (card-games-bridge--render-text g)))
    (should (rassoc '(hand . 0)
                    (get-text-property 0 'card-games-regions (card-games-bridge--svg g)))))
  (let ((g (card-games-bridge-game)))
    (card-games-bridge--deal g)
    (card-games-put g :phase 'play) (card-games-put g :turn 0) (card-games-put g :cursor 0)
    (card-games-put g :declarer 0) (card-games-put g :dummy 2) (card-games-put g :exposed t) (card-games-put g :tricks 0)
    (card-games-put g :contract '(3 . 3)) (card-games-put g :doubled nil)
    (card-games-put g :trick (list (cons 1 (car (card-games-bridge--hand g 1)))))
    (card-games-put g :message "x")
    (should (stringp (card-games-bridge--svg g)))))

(ert-deftest cgt-pat-golf-deal ()
  (let ((g (card-games-pat--deal (card-games-golf-game))))
    (should (= 35 (length (card-games-get g :cards))))
    (should (= 16 (length (card-games-get g :stock))))
    (should (= 1 (length (card-games-get g :waste))))
    (should (= 7 (length (card-games-pat--exposed g))))))   ; one per column (r=4)

(ert-deftest cgt-pat-tripeaks-deal ()
  (let ((g (card-games-pat--deal (card-games-tripeaks-game))))
    (should (= 28 (length (card-games-get g :cards))))
    (should (= 23 (length (card-games-get g :stock))))
    (should (equal (number-sequence 18 27) (card-games-pat--exposed g)))   ; base row
    (should-not (card-games-pat--exposed-p g 0))))                          ; apex covered

(ert-deftest cgt-pat-pyramid-deal ()
  (let ((g (card-games-pat--deal (card-games-pyramid-game))))
    (should (= 28 (length (card-games-get g :cards))))
    (should (= 24 (length (card-games-get g :stock))))
    (should (null (card-games-get g :waste)))
    (should (equal (number-sequence 21 27) (card-games-pat--exposed g)))   ; base row r=6
    (should-not (card-games-pat--exposed-p g 0))))                          ; apex covered by 1,2

(ert-deftest cgt-pat-exposed-reveal ()
  (let ((g (card-games-pat--deal (card-games-golf-game))))
    ;; clear column 0's lower cards; slot 0 (top) becomes exposed
    (dolist (i '(4 3 2 1)) (card-games-pat--remove-slot g i))
    (should (card-games-pat--exposed-p g 0))))

(ert-deftest cgt-pat-build-and-win ()
  (let ((g (card-games-pat--deal (card-games-golf-game))))
    ;; reduce to one exposed card adjacent to the waste top
    (card-games-put g :cards (let ((v (make-vector 35 nil))) (aset v 34 '(0 . 5)) v))
    (card-games-put g :waste (list '(1 . 4)))           ; 5 of clubs, rank 4; 6s is adjacent
    (card-games-put g :stock nil) (card-games-put g :cursor 0)
    (with-temp-buffer
      (setq card-games-pat--game g)
      (card-games-pat-act))                              ; plays slot 34 onto the waste
    (should (null (aref (card-games-get g :cards) 34)))
    (should (card-games-won-p g))))

(ert-deftest cgt-pat-sum13 ()
  (let ((g (card-games-pat--deal (card-games-pyramid-game))))
    ;; King value is 13
    (should (= 13 (card-games-pat--value '(0 . 12))))
    ;; mark two base cards summing to 13 -> both removed
    (card-games-put g :cards (let ((v (make-vector 28 nil)))
                       (aset v 21 '(0 . 4)) (aset v 22 '(1 . 7)) v))  ; 5 (val5) + 8 (val8) = 13
    (card-games-put g :marks nil)
    (card-games-pat--toggle-mark g '(slot . 21))
    (card-games-pat--toggle-mark g '(slot . 22))
    (should (null (aref (card-games-get g :cards) 21)))
    (should (null (aref (card-games-get g :cards) 22)))))

(ert-deftest cgt-pat-render ()
  (dolist (cls '(card-games-golf-game card-games-tripeaks-game card-games-pyramid-game))
    (let ((g (card-games-pat--deal (make-instance cls))))
      (should (stringp (card-games-render g))))))

(defun cgt--drive (g limit)
  "Play a whole match with AI for every seat, including the human seat 0."
  (let ((n 0))
    (while (and (not (card-games-trick--game-over-p g)) (< n limit))
      (when (eq (card-games-get g :phase) 'play)
        (card-games-trick--play g (card-games-get g :turn) (card-games-trick--ai-play g (card-games-get g :turn)))
        (card-games-trick--run g))
      (cl-incf n))))

(ert-deftest cgt-whist-deal-trump ()
  (let ((noninteractive t) (g (make-instance 'card-games-whist-game)))
    (card-games-trick--new g)
    (should (memq (oref g trump) '(0 1 2 3)))
    (let ((tot (length (card-games-get g :trick))))
      (dotimes (s 4) (cl-incf tot (length (card-games-trick--hand g s))))
      (should (= 52 tot)))))

(ert-deftest cgt-whist-full-game ()
  (let ((noninteractive t) (g (make-instance 'card-games-whist-game)))
    (card-games-trick--new g)
    (cgt--drive g 400)
    (should (card-games-trick--game-over-p g))
    (should (integerp (card-games-trick--winner-seat g)))))

(ert-deftest cgt-ohhell-rounds ()
  (let ((noninteractive t) (g (make-instance 'card-games-ohhell-game)))
    (card-games-trick--new g)
    ;; first round deals 7 cards each
    (should (= 7 (length (card-games-trick--hand g 0))))
    (cgt--drive g 400)
    (should (card-games-trick--game-over-p g))
    (should (= 7 (card-games-get g :round)))            ; seven rounds played
    (should (integerp (card-games-trick--winner-seat g)))))

(ert-deftest cgt-ohhell-exact-scoring ()
  (let ((g (make-instance 'card-games-ohhell-game)))
    (card-games-put g :scores (make-vector 4 0))
    (card-games-put g :round 0)
    (card-games-put g :bids (vector 2 0 1 3))
    (card-games-put g :tricks (vector 2 1 1 0))         ; seats 0 and 2 made exact bids
    (card-games-trick--score-hand g)
    (should (= 12 (aref (card-games-get g :scores) 0)))  ; 10 + 2
    (should (= 0 (aref (card-games-get g :scores) 1)))   ; bid 0 took 1 -> miss
    (should (= 11 (aref (card-games-get g :scores) 2)))  ; 10 + 1
    (should (= 0 (aref (card-games-get g :scores) 3)))))

(ert-deftest cgt-whist-render ()
  (let ((noninteractive t) (g (make-instance 'card-games-whist-game)))
    (card-games-trick--new g) (should (stringp (card-games-render g)))))

(ert-deftest cgt-pres-power ()
  (should (> (card-games-pres--power 0) (card-games-pres--power 12)))   ; the Two beats the Ace
  (should (< (card-games-pres--power 1) (card-games-pres--power 11))))  ; 3 below King

(ert-deftest cgt-pres-deal ()
  (let* ((card-games-president-players 4) (g (card-games-pres--deal (card-games-president-game))) (tot 0))
    (should (= 4 (card-games-get g :nplayers)))
    (dotimes (s 4) (cl-incf tot (length (card-games-pres--hand g s))))
    (should (= 52 tot))
    (should (= 0 (card-games-get g :count)))))

(ert-deftest cgt-pres-legal ()
  (let ((g (card-games-president-game)))
    (card-games-put g :hands (vector (list '(0 . 5) '(1 . 5) '(0 . 8)) nil nil nil))
    (card-games-put g :nplayers 4)
    ;; leading: any rank ok
    (card-games-put g :count 0) (card-games-put g :top -1)
    (should (= 2 (length (card-games-pres--legal-ranks g 0))))   ; ranks 5 and 8
    ;; following a single of power 5: need power>5 -> only rank 8
    (card-games-put g :count 1) (card-games-put g :top 5)
    (should (equal '(8) (card-games-pres--legal-ranks g 0)))
    ;; following a PAIR: need 2 of a higher rank -> rank 5 has two but power 5 not >5; none
    (card-games-put g :count 2) (card-games-put g :top 5)
    (should (null (card-games-pres--legal-ranks g 0)))))

(ert-deftest cgt-pres-full-game ()
  (let* ((card-games-president-players 4) (g (card-games-pres--deal (card-games-president-game))) (n 0))
    (while (and (eq (card-games-get g :phase) 'play) (< n 5000))
      (card-games-pres--ai-move g (card-games-get g :turn))
      (cl-incf n))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (= 4 (length (card-games-get g :order))))            ; everyone placed
    (should (= 4 (length (delete-dups (copy-sequence (card-games-get g :order)))))))) ; all distinct

(ert-deftest cgt-pres-exchange ()
  (let* ((card-games-president-players 4) (g (card-games-president-game)))
    ;; simulate a prior finishing order, then deal and check the swap happened
    (card-games-put g :order '(2 3 1 0))   ; prez=2, scum=0
    (card-games-pres--deal g)
    ;; conservation: still 52 cards across 4 hands after the exchange
    (let ((tot 0)) (dotimes (s 4) (cl-incf tot (length (card-games-pres--hand g s))))
      (should (= 52 tot)))))

(ert-deftest cgt-pres-render ()
  (let ((g (card-games-pres--deal (card-games-president-game)))) (should (stringp (card-games-render g)))))
;;;; Rummy family

(ert-deftest cgt-rummy-set-run ()
  (should (card-games-rummy-set-p '((0 . 5) (1 . 5) (2 . 5)) :distinct-suits t))
  (should-not (card-games-rummy-set-p '((0 . 5) (0 . 5) (2 . 5)) :distinct-suits t))
  (should (card-games-rummy-run-p '((0 . 3) (0 . 4) (0 . 5))))
  (should-not (card-games-rummy-run-p '((0 . 3) (1 . 4) (0 . 5))))
  (should (card-games-rummy-run-p '((0 . 10) (0 . 11) (0 . 12) (0 . 0)) :ace-high t))
  (should-not (card-games-rummy-run-p '((0 . 11) (0 . 12) (0 . 0))))
  (should (card-games-rummy-run-p '((0 . 0) (0 . 1) (0 . 2)))))

(ert-deftest cgt-rummy-best-partition ()
  (let* ((hand '((0 . 2)(0 . 3)(0 . 4) (0 . 6)(1 . 6)(2 . 6)
                 (3 . 8)(3 . 9)(3 . 10) (3 . 12)))
         (p (card-games-rummy-best-partition hand)))
    (should (= 3 (length (plist-get p :melds))))
    (should (= 10 (plist-get p :count))))
  (should (= 0 (card-games-rummy-deadwood
                '((0 . 0)(0 . 1)(0 . 2) (1 . 4)(1 . 5)(1 . 6)
                  (2 . 8)(2 . 9)(2 . 10)(2 . 11))))))

(ert-deftest cgt-rummy-layoff ()
  (should (card-games-rummy-layoff-p '(3 . 11) '(((3 . 8)(3 . 9)(3 . 10)))))
  (should-not (card-games-rummy-layoff-p '(0 . 4) '(((3 . 8)(3 . 9)(3 . 10))))))

(ert-deftest cgt-gin-full-game ()
  (let ((g (card-games-gin-game)) (turns 0))
    (card-games-gin--deal g)
    (while (and (not (eq (card-games-get g :phase) 'game-over)) (< turns 100000))
      (if (eq (card-games-get g :phase) 'hand-over)
          (progn (card-games-put g :reveal nil) (card-games-gin--deal g))
        (card-games-gin--ai-turn g (card-games-get g :turn)) (cl-incf turns)))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (>= (apply #'max (append (card-games-get g :scores) nil)) card-games-gin-target))))

(ert-deftest cgt-gin-render () (let ((g (card-games-gin--deal (card-games-gin-game))))
                                 (should (stringp (card-games-render g)))))

(ert-deftest cgt-tablemeld-games ()
  (dolist (mk (list (lambda () (card-games-rummy-basic-game :nplayers 3 :hand-size 7))
                    (lambda () (card-games-rum500-game :nplayers 3 :hand-size 7))))
    (let ((g (funcall mk)) (turns 0))
      (card-games-tm--deal g)
      (while (and (not (eq (card-games-get g :phase) 'game-over)) (< turns 200000))
        (if (eq (card-games-get g :phase) 'hand-over)
            (progn (card-games-put g :reveal nil) (card-games-tm--deal g))
          (card-games-tm--ai-turn g (card-games-get g :turn)) (cl-incf turns)))
      (should (eq (card-games-get g :phase) 'game-over))
      (should (stringp (card-games-render g))))))

(ert-deftest cgt-tablemeld-conservation ()
  (let ((g (card-games-rum500-game :nplayers 3 :hand-size 7)))
    (card-games-tm--deal g)
    (dotimes (_ 30) (when (eq (card-games-get g :phase) 'play)
                      (card-games-tm--ai-turn g (card-games-get g :turn))))
    (let ((tot (+ (length (card-games-get g :stock)) (length (card-games-get g :discard))
                  (apply #'+ (mapcar (lambda (r) (length (cdr r))) (card-games-get g :table))))))
      (dotimes (s 3) (setq tot (+ tot (length (card-games-rummy--hand g s)))))
      (should (= tot 52)))))

(ert-deftest cgt-handfoot-book ()
  (should (card-games-hf--book-valid-p '((0 . 7)(1 . 7)(2 . 7))))
  (should (card-games-hf--book-valid-p '((0 . 7)(1 . 7)(joker . 0))))      ; dirty
  (should-not (card-games-hf--book-valid-p '((0 . 7)(joker . 0)(joker . 0)))) ; wilds > nats
  (should-not (card-games-hf--book-valid-p '((0 . 2)(1 . 2)(2 . 2))))      ; threes
  (should (card-games-hf--book-complete-p '(a b c d e f g)))
  (should (card-games-hf--book-clean-p '((0 . 7)(1 . 7)(2 . 7))))
  (should-not (card-games-hf--book-clean-p '((0 . 7)(1 . 7)(joker . 0)))))

(ert-deftest cgt-handfoot-full-game ()
  (let ((g (card-games-handfoot-game)) (rounds 0) (turns 0) (expect (* 5 54)))
    (card-games-put g :nplayers 4) (card-games-put g :nteams 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-hf--deal g)
    (while (and (not (eq (card-games-get g :phase) 'game-over)) (< rounds 60))
      (cond ((eq (card-games-get g :phase) 'round-over)
             (card-games-put g :reveal nil) (card-games-hf--deal g) (cl-incf rounds))
            (t (card-games-hf--ai-turn g (card-games-get g :turn)) (cl-incf turns)
               (when (> turns 500000) (error "runaway")))))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (stringp (card-games-render g)))))
;;;; Matching games (Go Fish, Old Maid)

(ert-deftest cgt-gofish-full ()
  (let ((card-games-go-fish-players 4) (g (card-games-go-fish-game)) (guard 0))
    (card-games-gf--deal g)
    (while (and (not (eq (card-games-get g :phase) 'game-over)) (< guard 20000))
      (card-games-gf--ai-turn g (card-games-get g :turn)) (cl-incf guard))
    (should (eq (card-games-get g :phase) 'game-over))
    (let ((tot 0)) (dotimes (s 4) (cl-incf tot (card-games-gf--books g s)))
      (should (= tot 13)))
    (should (stringp (card-games-render g)))))

(ert-deftest cgt-oldmaid-full ()
  (let ((card-games-old-maid-players 4) (g (card-games-old-maid-game)) (guard 0))
    (card-games-om--deal g)
    (while (and (not (eq (card-games-get g :phase) 'game-over)) (< guard 20000))
      (card-games-om--ai-turn g (card-games-get g :turn)) (cl-incf guard))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (= 1 (card-games-om--total g)))
    (should (stringp (card-games-render g)))))

;;;; Cribbage

(ert-deftest cgt-cribbage-scorer ()
  (should (= 29 (card-games-crib--score-show '((0 . 4)(1 . 4)(3 . 4)(2 . 10)) '(2 . 4))))
  (should (= 12 (card-games-crib--count-pairs '((0 . 4)(1 . 4)(2 . 4)(3 . 4)))))
  (should (= 5 (card-games-crib--count-runs '((0 . 1)(0 . 2)(0 . 3)(0 . 4)(0 . 5)))))
  (should (= 2 (card-games-crib--peg-score '((0 . 10)(0 . 4)) 15)))
  (should (= 3 (card-games-crib--peg-score '((0 . 2)(0 . 3)(0 . 4)) 12)))
  (should (= 6 (card-games-crib--peg-score '((0 . 6)(1 . 6)(2 . 6)) 21))))

(ert-deftest cgt-cribbage-full ()
  (let ((g (card-games-cribbage-game)) (deals 0))
    (card-games-put g :dealer 1)
    (cl-flet ((ai-deal (g)
                (card-games-crib--deal g)
                (let ((d0 (card-games-crib--ai-discard g 0)) (d1 (card-games-crib--ai-discard g 1)))
                  (card-games-crib--set-hand g 0 (cl-set-difference (card-games-crib--hand g 0) d0 :test #'equal))
                  (card-games-crib--set-hand g 1 (cl-set-difference (card-games-crib--hand g 1) d1 :test #'equal))
                  (card-games-put g :crib (append d0 d1)))
                (card-games-crib--start-play g)
                (let ((guard 0))
                  (while (and (eq (card-games-get g :phase) 'play) (not (card-games-crib--peg-over-p g))
                              (< guard 400))
                    (cl-incf guard)
                    (let ((s (card-games-get g :pturn)))
                      (if (card-games-crib--legal g s) (card-games-crib--ai-play g s) (card-games-crib--peg-go g s)))))
                (when (and (eq (card-games-get g :phase) 'play) (card-games-crib--peg-over-p g))
                  (card-games-crib--show g))))
      (while (and (not (eq (card-games-get g :phase) 'game-over)) (< deals 300))
        (card-games-put g :dealer (- 1 (card-games-get g :dealer)))
        (ai-deal g) (cl-incf deals)))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (stringp (card-games-render g)))))

;;;; Fishing games (Scopa, Casino)

(ert-deftest cgt-fish-capture ()
  (let ((g (card-games-scopa-game)))
    (card-games-put g :table '((0 . 0)(1 . 1)(2 . 3)))
    (should (equal (sort (mapcar #'cdr (card-games-fish--capture g '(3 . 4))) #'<) '(0 3))))
  (let ((g (card-games-casino-game)))
    (card-games-put g :table '((0 . 12)(1 . 12)(2 . 5)))
    (should (= 1 (length (card-games-fish--capture g '(3 . 12)))))))

(ert-deftest cgt-fish-full ()
  (dolist (mk (list #'card-games-scopa-game #'card-games-casino-game))
    (let ((g (funcall mk)) (rounds 0))
      (card-games-fish--deal-round g)
      (while (and (not (eq (card-games-get g :phase) 'game-over)) (< rounds 200))
        (if (eq (card-games-get g :phase) 'round-over)
            (card-games-fish--deal-round g)
          (card-games-fish--ai-play g (card-games-get g :turn)) (cl-incf rounds)))
      (should (eq (card-games-get g :phase) 'game-over))
      (should (stringp (card-games-render g))))))

;;;; Trick extensions (Euchre, Pitch, Briscola)

(ert-deftest cgt-euchre-bowers ()
  (should (> (card-games-euchre--power '(0 . 9) 0 0) (card-games-euchre--power '(1 . 9) 0 0)))
  (should (> (card-games-euchre--power '(1 . 9) 0 0) (card-games-euchre--power '(0 . 12) 0 0)))
  (should (= 120 (let ((s 0)) (dolist (su '(0 1 2 3))
                                (dolist (r card-games-briscola--ranks)
                                  (setq s (+ s (card-games-bris--points (cons su r)))))) s))))

(ert-deftest cgt-trick-ext-full ()
  (dolist (class '(card-games-briscola-game card-games-pitch-game card-games-euchre-game))
    (let ((g (make-instance class)) (guard 0))
      (card-games-trick--new g)
      (while (and (not (eq (card-games-get g :phase) 'game-over)) (< guard 50000))
        (cl-incf guard)
        (if (card-games-trick--hand-over-p g)
            (card-games-trick--finish-hand g)
          (let ((s (card-games-get g :turn)))
            (card-games-trick--play g s (card-games-trick--ai-play g s)))))
      (should (eq (card-games-get g :phase) 'game-over))
      (should (stringp (card-games-render g))))))

;;;; Spite & Malice

(ert-deftest cgt-spite-legal ()
  (let ((g (card-games-spite-game)))
    (card-games-spite--deal g) (card-games-put g :center (make-vector 4 nil))
    (should (eql 0 (card-games-spite--legal-center g '(0 . 0))))   ; Ace starts a pile
    (should (null (card-games-spite--legal-center g '(0 . 1))))    ; a Two cannot
    (should (eql 0 (card-games-spite--legal-center g '(0 . 12))))  ; King is wild
    (card-games-spite--put-center g '(0 . 0) 0)
    (should (= 1 (card-games-spite--needed g 0)))))

(ert-deftest cgt-spite-full ()
  (let ((card-games-spite-goal-size 10) (g (card-games-spite-game)) (turns 0))
    (card-games-spite--deal g)
    (while (and (eq (card-games-get g :phase) 'play) (< turns 6000))
      (cl-incf turns) (card-games-spite--ai-turn g (card-games-get g :turn)))
    (should (eq (card-games-get g :phase) 'game-over))
    (should (stringp (card-games-render g)))))
;;;; Bridge

(ert-deftest cgt-bridge-score ()
  (cl-flet ((b (l s d v tk) (plist-get (card-games-bridge--deal-score l s d v tk) :below))
            (a (l s d v tk) (plist-get (card-games-bridge--deal-score l s d v tk) :datk))
            (f (l s d v tk) (plist-get (card-games-bridge--deal-score l s d v tk) :defend)))
    (should (= 100 (b 3 4 0 nil 9)))     ; 3NT made
    (should (= 120 (b 4 3 0 nil 10)))    ; 4 spades made
    (should (= 180 (b 6 2 0 nil 12)))    ; 6 hearts made
    (should (= 500 (a 6 2 0 nil 12)))    ; small slam bonus
    (should (= 50  (a 1 4 1 nil 7)))     ; 1NT doubled, insult
    (should (= 100 (f 4 3 0 nil 8)))     ; down two undoubled
    (should (= 500 (f 4 3 1 t 8))))      ; down two doubled vulnerable
  (should (= 1 (card-games-bridge--trick-winner
                '((0 . (0 . 12)) (1 . (3 . 0)) (2 . (0 . 2)) (3 . (0 . 5))) 3))))

(ert-deftest cgt-bridge-full ()
  (let ((scored 0) (passed 0))
    (dotimes (i 12)
      (let ((g (card-games-bridge-game)) (guard 0))
        (card-games-put g :dealer (mod i 4))
        (card-games-bridge--deal g)
        (while (and (eq (card-games-get g :phase) 'auction) (< guard 60))
          (cl-incf guard)
          (let* ((s (card-games-get g :bidder)) (call (card-games-bridge--ai-call g s)))
            (unless (card-games-bridge--legal-call-p g call) (setq call 'pass))
            (card-games-bridge--apply-call g s call)
            (card-games-bridge--auction-done-p g)))
        (if (eq (card-games-get g :phase) 'passed-out) (cl-incf passed)
          (let ((p 0))
            (while (and (eq (card-games-get g :phase) 'play) (< p 60))
              (cl-incf p)
              (card-games-bridge--play-card g (card-games-get g :turn) (card-games-bridge--ai-play g (card-games-get g :turn)))))
          (when (eq (card-games-get g :phase) 'scored)
            (cl-incf scored)
            (should (cl-every #'null (append (card-games-get g :hands) nil)))))
        (should (memq (card-games-get g :phase) '(scored passed-out)))
        (should (stringp (card-games-render g)))))
    (should (> scored 0))))
;;;; Renderer registry / region keystone

(ert-deftest cgt-keystone-regions ()
  (let* ((g (card-games-sol--deal (make-instance 'card-games-klondike-game)))
         (res (card-games-render-svg g)))
    (should (stringp (car res)))
    (should (>= (length (cdr res)) 13))           ; 6 top slots + 7 columns
    (let* ((reg (cl-find '(col . 3) (cdr res) :key #'cdr :test #'equal))
           (rect (car reg)))
      (should reg)
      (should (equal '(col . 3)
                     (card-games-regions-hit (cdr res)
                                     (+ (nth 0 rect) (/ (nth 2 rect) 2))
                                     (+ (nth 1 rect) 10)))))
    (should (null (card-games-regions-hit (cdr res) -5 -5))))
  (with-temp-buffer
    (setq card-games-sol--game (card-games-sol--deal (make-instance 'card-games-klondike-game)))
    (card-games-render-game card-games-sol--game)
    (should (oref card-games-sol--game renderer))
    (should (> (buffer-size) 0))
    (card-games-render-apply card-games-sol--game '(col . 5))
    (should (= 11 (card-games-get card-games-sol--game :cursor)))))   ; spot index of (col . 5)
;;;; Hand-cluster click regions + card-size slider

(ert-deftest cgt-hand-regions ()
  (let* ((cards '((0 . 0) (0 . 1) (1 . 5)))
         (str (card-games-rummy--svg-row cards 0 nil nil 'hand))
         (regs (get-text-property 0 'card-games-regions str)))
    (should (= (+ 3 (length card-games-svg-slider-stops)) (length regs)))   ; 3 cards + stops
    (should (equal '(hand . 0) (cdr (car regs))))
    (should (cl-find-if (lambda (r) (eq (car-safe (cdr r)) 'scale)) regs)))
  (let ((g (card-games-gin--deal (card-games-gin-game))) (card-games-card-scale 1.0))
    (card-games-render-apply g '(hand . 2))
    (should (= 2 (card-games-get g :cursor)))
    (card-games-render-apply g '(scale . 1.5))         ; base method handles scale
    (should (= 1.5 card-games-card-scale))
    (card-games-render-apply g 'zoom-reset)
    (should (= 1.0 card-games-card-scale))))

;;;; Rummy 500 deep pickup and Hand & Foot rule completions

(ert-deftest cgt-rum500-deep-pickup ()
  "Taking a buried discard card melds it at once, keeping the cards above."
  (let ((g (card-games-rum500-game :nplayers 2 :hand-size 10)))
    (card-games-put g :nplayers 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-put g :hands (vector (list (cons 0 5) (cons 0 6)) nil)) ; 6S 7S
    (card-games-put g :discard (list (cons 1 9) (cons 0 4) (cons 2 3))) ; top 10C, buried 5S
    (card-games-put g :table nil) (card-games-put g :laid (make-vector 2 0))
    (card-games-put g :turn 0) (card-games-put g :step 'draw) (card-games-put g :phase 'play)
    (should (card-games-tm--take-deep g 0 1))
    (should (equal (card-games-get g :table) '((0 (0 . 4) (0 . 5) (0 . 6)))))
    (should (equal (card-games-rummy--hand g 0) '((1 . 9))))
    (should (= 1 (length (card-games-get g :discard))))
    ;; a card you cannot use immediately may not be taken
    (card-games-put g :hands (vector (list (cons 3 11)) nil))
    (card-games-put g :discard (list (cons 1 9) (cons 2 2)))
    (should-not (card-games-tm--take-deep g 0 1))))

(ert-deftest cgt-handfoot-redthree ()
  "Red threes leave the hand on deal and collect to the team pile."
  (let ((g (card-games-handfoot-game)))
    (card-games-put g :nplayers 4) (card-games-put g :nteams 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-hf--deal g)
    (dotimes (s 4)
      (should (= 0 (cl-count-if #'card-games-hf--red-three-p (card-games-rummy--hand g s)))))
    (card-games-put g :redthrees (make-vector 2 nil))
    (card-games-rummy--set-hand g 0 (list (cons 2 2) (cons 0 5))) ; 3 of diamonds + 6S
    (card-games-put g :stock (list (cons 0 9)))
    (should (= 1 (card-games-hf--collect-red-threes g 0)))
    (should (= 1 (length (aref (card-games-get g :redthrees) (card-games-hf--team g 0)))))
    (should-not (cl-find-if #'card-games-hf--red-three-p (card-games-rummy--hand g 0)))))

(ert-deftest cgt-handfoot-min-meld ()
  "Initial meld must reach the round minimum; below it is refused."
  (let ((g (card-games-handfoot-game)))
    (card-games-put g :nplayers 4) (card-games-put g :nteams 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-hf--deal g) (card-games-put g :round 0)
    (card-games-put g :down (make-vector 2 nil)) (card-games-put g :books (make-vector 2 nil))
    (card-games-put g :hands (vector (list (cons 0 3)(cons 1 3)(cons 2 3)) nil nil nil)) ; three 4s = 15
    (should-not (card-games-hf--initial-meld g 0 (card-games-rummy--hand g 0)))
    (should-not (card-games-hf--down-p g 0))
    (card-games-put g :hands (vector (list (cons 0 0)(cons 1 0)(cons 2 0)) nil nil nil)) ; three aces = 60
    (should (card-games-hf--initial-meld g 0 (card-games-rummy--hand g 0)))
    (should (card-games-hf--down-p g 0))
    (should (= 1 (length (card-games-hf--books g 0))))))

(ert-deftest cgt-handfoot-pickup ()
  "Picking up melds the top discard and rakes in the cards beneath it."
  (let ((g (card-games-handfoot-game)))
    (card-games-put g :nplayers 4) (card-games-put g :nteams 2) (card-games-put g :scores (make-vector 2 0))
    (card-games-hf--deal g)
    (card-games-put g :redthrees (make-vector 2 nil)) (card-games-put g :books (make-vector 2 nil))
    (card-games-put g :hands (vector (list (cons 0 8)(cons 1 8)(cons 0 4)) nil nil nil)) ; two 9s + 5S
    (card-games-put g :discard (list (cons 2 8)(cons 3 5)(cons 0 6)(cons 1 7))) ; top 9D + 3 beneath
    (card-games-put g :stock nil)
    (should (card-games-hf--pickup-eligible g 0))
    (let ((top (card-games-hf--pickup g 0)))
      (should (equal top (cons 2 8)))
      (should (= 1 (length (card-games-hf--books g 0))))
      (should (card-games-hf--book-valid-p (car (card-games-hf--books g 0))))
      (should (= 4 (length (card-games-rummy--hand g 0))))
      (should-not (card-games-get g :discard)))))

;;;; Mouse click-region wiring for eights, president, and the patience boards

(ert-deftest cgt-mouse-regions ()
  "The newly clickable games attach a `card-games-regions' map to their SVG."
  (let* ((g (card-games-pat--deal (card-games-golf-game)))
         (regs (get-text-property 0 'card-games-regions (card-games-pat--svg g))))
    (should regs)
    (should (cl-find '(waste . 0) (mapcar #'cdr regs) :test #'equal))
    (should (cl-find '(stock . 0) (mapcar #'cdr regs) :test #'equal))
    (should (cl-find-if (lambda (a) (eq (car a) 'slot)) (mapcar #'cdr regs))))
  (let ((g (card-games-president-game)))
    (card-games-put g :games 0) (card-games-pres--deal g)
    (let ((regs (get-text-property 0 'card-games-regions (card-games-pres--svg g))))
      (should regs)
      (should (cl-find-if (lambda (a) (and (consp a) (eq (car a) 'hand)))
                          (mapcar #'cdr regs)))))
  (let ((g (card-games-eights-game)))
    (card-games-put g :scores (make-vector 3 0)) (card-games-eights--deal g)
    (let* ((img (card-games-svg-hand-image (mapcar #'card-games-eights--spec (card-games-eights--hand g 0))
                                   :region-tag 'hand))
           (regs (get-text-property 0 'card-games-regions img)))
      (should regs)
      (should (cl-find-if (lambda (a) (and (consp a) (eq (car a) 'hand)))
                          (mapcar #'cdr regs))))))

;;;; 500 SVG-UI: kitty discard cap and on-screen controls

(ert-deftest cgt-bid-kitty-cap ()
  "Kitty marking never exceeds five cards, and the UI exposes the controls."
  (let ((g (card-games-bid-game)) (card-games-bid--human-seats '(0)))
    (card-games-bid--deal g 3)
    (card-games-put g :phase 'kitty) (card-games-put g :contractor 0)
    (card-games-put g :contract (nth 0 card-games-bid-schedule))
    (let ((hand (card-games-bid-sort-display (card-games-bid--hand g 0) nil)))
      (card-games-put g :sorted-hand hand)
      (card-games-put g :marks (cl-subseq hand 0 5))            ; already five
      (card-games-put g :cursor 5)                              ; a sixth, unmarked
      (with-temp-buffer
        (setq-local card-games-bid--game g)
        (card-games-bid-select)                                 ; must refuse the sixth
        (should (= 5 (length (card-games-get g :marks))))
        ;; unmark a card that is actually marked (redisplay may have re-sorted
        ;; the hand by trump, so cursor 0 is not necessarily a marked card)
        (let* ((sh (card-games-get g :sorted-hand)) (mk (card-games-get g :marks))
               (idx (cl-position-if (lambda (c) (member c mk)) sh)))
          (card-games-put g :cursor idx)
          (card-games-bid-select)
          (should (= 4 (length (card-games-get g :marks)))))))
    ;; the full-SVG kitty panel exposes a Discard region and a card-size slider
    (let* ((card-games-bid-svg-ui t) (card-games-bid-svg-fill t)
           (rg (cdr (card-games-bid--ui-svg g 1100 700))))
      (should (plist-get rg :discard))
      (should (plist-get rg :sizer)))))

(ert-deftest cgt-bid-svg-controls ()
  "The full-SVG UI exposes Help and, when open, the overlay's buttons."
  (let ((g (card-games-bid-game)) (card-games-bid-svg-ui t) (card-games-bid-svg-fill t))
    (card-games-bid--deal g 3)
    (card-games-put g :phase 'auction) (card-games-put g :bidder 0)
    (let ((rg (cdr (card-games-bid--ui-svg g 1100 700))))
      (should (plist-get rg :help))
      (should-not (plist-get rg :help-close)))
    (card-games-put g :help-open t)
    (let ((rg (cdr (card-games-bid--ui-svg g 1100 700))))
      (should (plist-get rg :help-close))
      (should (plist-get rg :help-classic))
      (should (plist-get rg :help-quit)))))


(ert-deftest cgt-red-suit-boolean ()
  "Two red suits compare equal by colour (the diamond/heart eq bug)."
  (should (eq (card-games-red-suit-p 2) (card-games-red-suit-p 3)))     ; both red -> eq
  (should (eq (card-games-red-suit-p 0) (card-games-red-suit-p 1)))     ; both black -> eq
  (should-not (eq (card-games-red-suit-p 2) (card-games-red-suit-p 0))) ; red vs black
  ;; a diamond may NOT sit on a heart in a house (both red)
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :houses) 0 (list '(3 . 5)))          ; 6 of hearts
    (should-not (card-games-crap--house-accepts g 0 '(2 . 4)))   ; 5 of diamonds (both red)
    (aset (card-games-get g :houses) 0 (list '(0 . 5)))          ; 6 of spades (black)
    (should (card-games-crap--house-accepts g 0 '(2 . 4)))))     ; 5 of diamonds ok

(ert-deftest cgt-ai-level-easy-crapette ()
  "The easy Crapette AI does not rearrange houses, so a stuck card stays."
  (let ((g (card-games-crap--deal (card-games-crapette-game))))
    (aset (card-games-get g :reserve) 1 (list '(2 . 4)))
    (aset (card-games-get g :waste) 1 nil) (aset (card-games-get g :hand) 1 nil)
    (aset (card-games-get g :reserve) 0 nil) (aset (card-games-get g :waste) 0 nil)
    (aset (card-games-get g :found) 0 nil)
    (aset (card-games-get g :houses) 0 (list '(0 . 5) '(3 . 5)))
    (aset (card-games-get g :houses) 1 (list '(1 . 6)))
    (dotimes (k 6) (aset (card-games-get g :houses) (+ 2 k) (list '(2 . 6))))
    (card-games-put g :turn 1)
    (let ((card-games-ai-level 'easy) (card-games-crap--recording nil)) (card-games-crap--ai-play g))
    (should (card-games-crap--reserve g 1))))


(ert-deftest cgt-treatment-set ()
  (let ((saved (mapcar (lambda (v) (cons v (symbol-value v)))
                       (append card-games--svg-card-vars card-games--full-svg-vars)))
        (savedt card-games-treatment))
    (unwind-protect
        (progn
          (card-games-set-treatment 'text)
          (should-not card-games-sol-svg-cards) (should-not card-games-crapette-svg-cards)
          (card-games-set-treatment 'full)
          (should card-games-sol-svg-cards) (should card-games-bid-svg-ui) (should card-games-gaps-svg-ui)
          (card-games-set-treatment 'svg)
          (should card-games-sol-svg-cards) (should-not card-games-bid-svg-ui))
      (dolist (pr saved) (set (car pr) (cdr pr)))
      (setq card-games-treatment savedt))))

;;;; Live 500: dispatch seam replaces advice (MELPA review, PR #10147)

(defun cgt--advised-p (symbol)
  "Return non-nil when SYMBOL carries any advice."
  (let (found)
    (advice-mapc (lambda (&rest _) (setq found t)) symbol)
    found))

(ert-deftest cgt-bid-net-no-advice ()
  "Loading the net layer must not install advice on bid functions."
  (require 'card-games-bid-net)
  (dolist (sym '(card-games-bid--nominate-suit card-games-bid--refresh
                 card-games-bid-make-bid card-games-bid-pass
                 card-games-bid-select card-games-bid-discard-marked))
    (should-not (cgt--advised-p sym))))

(ert-deftest cgt-bid-submit-local ()
  "`card-games-bid-submit' applies an action locally on a base game."
  (let ((game (make-instance 'card-games-bid-game)) calls)
    (cl-letf (((symbol-function 'card-games-bid--auction-act)
               (lambda (_g seat bid) (push (list 'act seat bid) calls)))
              ((symbol-function 'card-games-bid--refresh)
               (lambda () (push 'refresh calls))))
      (card-games-bid-submit game '(pass)))
    (should (equal (reverse calls) '((act 0 nil) refresh)))))

(ert-deftest cgt-bid-submit-client ()
  "`card-games-bid-submit' on a client game sends and never applies."
  (require 'card-games-bid-net)
  (let ((game (make-instance 'card-games-bid-client-game)) sent applied)
    (cl-letf (((symbol-function 'card-games-net-send-move)
               (lambda (move) (push move sent)))
              ((symbol-function 'card-games-bid--auction-act)
               (lambda (&rest _) (setq applied t)))
              ((symbol-function 'card-games-bid--redisplay) #'ignore))
      (card-games-bid-submit game '(pass)))
    (should (equal sent '((pass))))
    (should-not applied)
    (should (equal (card-games-get game :message) "Pass sent — waiting…"))))

(ert-deftest cgt-bid-nominate-inhibit ()
  "Inhibited prompts fall back to the AI pick for a human seat."
  (require 'card-games-bid-net)
  (let ((game (make-instance 'card-games-bid-game)))
    (card-games-put game :hands (vector '((2 . 5) (2 . 6) (0 . 14)) nil nil nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (&rest _) (error "Prompted despite inhibit"))))
      (let ((card-games-bid-inhibit-prompts t)
            (card-games-bid--human-seats '(0)))
        (should (= (card-games-bid--nominate-suit game 0) 2))))))

(ert-deftest cgt-bid-after-refresh-hook ()
  "`card-games-bid--refresh' runs `card-games-bid-after-refresh-hook'."
  (let ((game (make-instance 'card-games-bid-game)) ran)
    (cl-letf (((symbol-function 'card-games-bid--run) #'ignore)
              ((symbol-function 'card-games-bid--redisplay) #'ignore)
              ((symbol-function 'card-games-bid--announce) #'ignore))
      (let ((card-games-bid--game game)
            (card-games-bid-animate nil)
            (card-games-bid-after-refresh-hook (list (lambda () (setq ran t)))))
        (card-games-bid--refresh)))
    (should ran)))

(ert-deftest cgt-bid-start-now-key ()
  "The lobby start key lives in the owner keymap, not a load-time patch."
  (should (eq (lookup-key card-games-bid-mode-map "s") 'card-games-bid-start-now)))
