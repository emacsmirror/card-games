;;; card-games-tests.el --- ERT tests for card-games  -*- lexical-binding: t; -*-

;; Run with:  make test   (or)
;;   emacs -Q --batch -L . -L test -l test/card-games-tests.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'card-games)

;;;; cg-core

(ert-deftest cgt-core-color-fallback ()
  (should (equal "#123456" (cg-color 'no-such-face-xyzzy :background "#123456"))))

(ert-deftest cgt-core-suit-glyph ()
  (let ((cg-symbols '((0 . "S") (1 . "C") (2 . "D") (3 . "H") (joker . "JK"))))
    (should (equal "S" (cg-suit-glyph 0)))
    (should (equal "JK" (cg-suit-glyph 'joker))))
  (should (equal "♠" (cg-suit-glyph 0))))

;;;; Renderer skins

(ert-deftest cgt-render-registry ()
  (should (memq 'text (cg-renderer-names)))
  (should (memq 'svg (cg-renderer-names)))
  (should (memq 'svg-fill (cg-renderer-names)))
  (should (object-of-class-p (cg-make-renderer 'svg) 'cg-svg-renderer))
  (should (object-of-class-p (cg-make-renderer 'svg-fill) 'cg-svg-renderer))
  (should-not (cg-make-renderer 'no-such-treatment)))

(ert-deftest cgt-render-resolve ()
  (should (eq 'text (cg-render-resolve-treatment 'text)))
  (should (memq (cg-render-resolve-treatment 'auto) '(text svg))))

;;;; Networking

(defclass cgt-net-game (cg-game)
  ((env :initarg :env :initform '(:counter 0)))
  "Throwaway game whose only move adds to a counter.")

(cl-defmethod cg-net-apply-move ((g cgt-net-game) _seat move)
  (cg-put g :counter (+ (or (cg-get g :counter) 0) move))
  t)

(ert-deftest cgt-net-loopback ()
  "Host-authoritative move sync over a loopback TCP socket."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (let* ((hgame (make-instance 'cgt-net-game :env (list :counter 0)))
         (srv (cg-net-host-start hgame 0))
         (port (process-contact srv :service))
         (cgame (make-instance 'cgt-net-game :env (list :counter 0))))
    (unwind-protect
        (progn
          (cg-net-connect "127.0.0.1" port "Test" cgame)
          (dotimes (_ 12) (accept-process-output nil 0.05))
          (should (= 0 (cg-get cgame :counter)))
          (cg-net-send-move 7)
          (dotimes (_ 12) (accept-process-output nil 0.05))
          (should (= 7 (cg-get hgame :counter)))
          (should (= 7 (cg-get cgame :counter))))
      (cg-net-disconnect)
      (cg-net-host-stop))))

;;;; Gaps

(ert-deftest cgt-gaps-deal ()
  (let* ((g (cg-gaps--deal (make-instance 'cg-montana-game)))
         (b (cg-get g :board)) (cards 0) (gaps 0))
    (dotimes (r 4) (dotimes (c 13)
      (if (cg-gaps--cell b r c) (cl-incf cards) (cl-incf gaps))))
    (should (= cards 48)) (should (= gaps 4))))

(ert-deftest cgt-gaps-win ()
  (let ((g (make-instance 'cg-montana-game)) (b (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 12) (aset row c (cons r c)))
        (aset b r row)))
    (cg-put g :board b)
    (should (cg-won-p g))))

(ert-deftest cgt-acre-win ()
  ;; King-head descending: row r col c == (suit r . (- 11 c)), gap at col 12
  (let ((g (make-instance 'cg-acre-game)) (b (make-vector 4 nil)))
    (dotimes (r 4)
      (let ((row (make-vector 13 nil)))
        (dotimes (c 12) (aset row c (cons r (- 11 c))))
        (aset b r row)))
    (cg-put g :board b)
    (should (cg-won-p g))
    ;; head gap wants a King; right of a Two is dead
    (should (eq 'head (cg-gaps--needed g b 0 0)))))

(ert-deftest cgt-acre-needed ()
  (let ((g (make-instance 'cg-acre-game)) (b (make-vector 4 nil)))
    (dotimes (r 4) (aset b r (make-vector 13 nil)))
    ;; left neighbour King(11) at (0,0): gap (0,1) wants Queen(10) same suit
    (aset (aref b 0) 0 (cons 0 11))
    (should (equal (cons 0 10) (cg-gaps--needed g b 0 1)))
    ;; left neighbour Two(0): nothing follows -> nil (Two is the dead end)
    (aset (aref b 1) 0 (cons 0 0))
    (should (null (cg-gaps--needed g b 1 1)))))

(ert-deftest cgt-gaps-hit ()
  (dotimes (r 4) (dotimes (c 13)
    (let ((px (+ cg-gaps--svg-pad (* c (+ cg-gaps--svg-card-w cg-gaps--svg-gap))
               (/ cg-gaps--svg-card-w 2)))
          (py (+ cg-gaps--svg-pad (* r (+ cg-gaps--svg-card-h cg-gaps--svg-gap))
               (/ cg-gaps--svg-card-h 2))))
      (should (equal (cons r c) (cg-gaps--xy->cell px py)))))))

;;;; 500

(ert-deftest cgt-bid-deck () (should (= 45 (length (cg-bid--full-deck)))))

(ert-deftest cgt-bid-power ()
  (should (> (cg-bid-power cg-bid-joker 0 0) (cg-bid-power '(0 . 7) 0 0)))
  (should (> (cg-bid-power '(0 . 7) 0 0) (cg-bid-power '(1 . 7) 0 0)))
  (should (< (cg-bid-power '(2 . 10) 0 3) 100)))

(ert-deftest cgt-bid-trick ()
  (should (= 2 (cg-bid-trick-winner
                '((0 . (3 . 10)) (1 . (3 . 0)) (2 . (0 . 0)) (3 . (2 . 10))) 0 3))))

(ert-deftest cgt-bid-follow ()
  (should (equal '((1 . 7)) (cg-bid-legal-cards '((1 . 7) (1 . 0) (3 . 0)) 0 0))))

(ert-deftest cgt-bid-sort-display ()
  (should (cg-bid-joker-p
           (car (cg-bid-sort-display (list '(3 . 10) cg-bid-joker '(0 . 7)) 0)))))

(ert-deftest cgt-bid-south-hit ()
  (dolist (n '(1 5 10 15))
    (cl-destructuring-bind (x0 step y) (cg-bid--south-layout n)
      (dotimes (i n)
        (should (equal i (cg-bid--south-hit (+ x0 (* i step) 2) (+ y 5) n)))))))

(ert-deftest cgt-bid-full-game ()
  (let ((cg-bid--human-seats nil))
    (dotimes (_ 12)
      (let ((g (cg-bid--deal (make-instance 'cg-bid-game))) (guard 0))
        (cg-bid--run g)
        (while (and (not (eq (cg-get g :phase) 'gameover)) (< (cl-incf guard) 4000))
          (cg-bid--deal g (mod (1+ (cg-get g :dealer)) 4)) (cg-bid--run g))
        (should (eq 'gameover (cg-get g :phase)))))))

(ert-deftest cgt-bid-smart-beats-basic ()
  (let ((w0 0) (w1 0))
    (random "ert-h2h")
    (dotimes (_ 50)
      (let ((cg-bid--human-seats nil)
            (cg-bid-ai-policies (vector 'smart 'basic 'smart 'basic))
            (g (cg-bid--deal (make-instance 'cg-bid-game))) (guard 0))
        (cg-bid--run g)
        (while (and (not (eq (cg-get g :phase) 'gameover)) (< (cl-incf guard) 4000))
          (cg-bid--deal g (mod (1+ (cg-get g :dealer)) 4)) (cg-bid--run g))
        (pcase (cg-get g :game-over) (0 (cl-incf w0)) (1 (cl-incf w1)))))
    (should (> w0 (* 1.8 w1)))))

;;;; SVG

(ert-deftest cgt-svg-builds ()
  (should (stringp (cg-svg-to-string
                    (cg-svg-grid-svg (list (list (cons "A" 0) (cons nil 'joker) 'down nil))))))
  (should (stringp (cg-svg-to-string
                    (cg-bid--table-svg (cg-bid--deal (make-instance 'cg-bid-game)))))))

;;;; Chooser

(ert-deftest cgt-chooser-registry ()
  (should (assoc "500 (Bid)" card-games-list))
  (should (commandp 'card-game)))

(provide 'card-games-tests)
;;; card-games-tests.el ends here

(ert-deftest cgt-ai-step ()
  ;; ai-step advances within a hand; the caller deals the next hand at 'done.
  (let ((cg-bid--human-seats nil)
        (g (cg-bid--deal (make-instance 'cg-bid-game))) (guard 0))
    (while (and (not (eq (cg-get g :phase) 'gameover)) (< (cl-incf guard) 6000))
      (unless (cg-bid--ai-step g)
        (cg-bid--deal g (mod (1+ (cg-get g :dealer)) 4))))
    (should (eq 'gameover (cg-get g :phase)))))

(ert-deftest cgt-card-back-presets ()
  (dolist (cg-svg-card-back '(dots rings solid))
    (should (stringp (cg-svg-to-string
                      (cg-svg-cards-svg (list 'down)))))))

(ert-deftest cgt-set-theme ()
  (let ((inhibit-message t))            ; keep the theme banner out of test output
    (card-games-set-theme 'dark)
    (should (equal "#23272e" cg-bid-felt-color))
    (card-games-set-theme 'classic)
    (should (equal "#15692f" cg-bid-felt-color))))

(ert-deftest cgt-scale ()
  (require 'face-remap)
  (let ((text-scale-mode-amount 0)) (should (= 1.0 (cg-scale))))
  (let ((text-scale-mode-amount 2)) (should (> (cg-scale) 1.0)))
  (let ((text-scale-mode-amount -2)) (should (< (cg-scale) 1.0))))

(ert-deftest cgt-mode-line-announce ()
  (let ((g (cg-bid--deal (make-instance 'cg-bid-game))))
    (should (stringp (cg-bid--mode-line g)))
    (should (string-match-p "bid" (cg-bid--mode-line g)))))

(ert-deftest cgt-gaps-mode-line ()
  (let ((g (cg-gaps--deal (make-instance 'cg-montana-game))))
    (should (string-match-p "moves" (cg-gaps--mode-line g)))))

(ert-deftest cgt-keys-default ()
  (should (eq cg-keys 'emacs))
  (should (commandp 'cg-gaps-redraw))
  (should (commandp 'cg-bid-redraw))
  ;; g redraws (not new)
  (should (eq 'cg-gaps-redraw (lookup-key cg-gaps-mode-map "g")))
  (should (eq 'cg-bid-redraw (lookup-key cg-bid-mode-map "g"))))

(ert-deftest cgt-keys-classic ()
  ;; emacs map has no h/SPC binding; classic adds them
  (should-not (lookup-key cg-gaps-mode-map "h"))
  (should (eq 'cg-gaps-left (lookup-key (cg-gaps--classic-keymap) "h")))
  (should (eq 'cg-bid-left (lookup-key (cg-bid--classic-keymap) "h"))))

(ert-deftest cgt-svgui-builds ()
  (let ((g (cg-bid--deal (make-instance 'cg-bid-game))))
    (cg-put g :phase 'auction) (cg-put g :bidder 0)
    (let ((sr (cg-bid--ui-svg g)))
      (should (stringp (cg-svg-to-string (car sr))))
      ;; available bids have clickable rects; a cell centre maps back to its bid
      (let* ((bids (plist-get (cdr sr) :bids)) (e (car bids))
             (r (cdr e)) (cx (+ (nth 0 r) (/ (nth 2 r) 2))) (cy (+ (nth 1 r) (/ (nth 3 r) 2))))
        (should (eq (car e) (cg-bid--region-bid cx cy (cdr sr))))))))

(ert-deftest cgt-gaps-svgui-builds ()
  ;; the full-SVG gaps UI builds, and a cell centre maps back to its (row . col)
  (let ((g (cg-gaps--deal (make-instance 'cg-montana-game))))
    (let* ((sr (cg-gaps--ui-svg g 820 380))
           (geom (plist-get (cdr sr) :board))
           (x0 (nth 0 geom)) (y0 (nth 1 geom))
           (cw (nth 2 geom)) (ch (nth 3 geom)) (gp (nth 4 geom)))
      (should (stringp (cg-svg-to-string (car sr))))
      (should (plist-get (cdr sr) :new))   ; control regions present
      (should (equal (cons 2 3)
                     (cg-gaps--ui-cell (+ x0 (* 3 (+ cw gp)) (/ cw 2))
                                       (+ y0 (* 2 (+ ch gp)) (/ ch 2))
                                       geom))))))

(ert-deftest cgt-classic-folds-controls ()
  ;; graphical classic UI: one action-button row, no textual key-help
  (with-temp-buffer
    (cg-bid-mode)
    (setq cg-bid--game (cg-bid--deal (make-instance 'cg-bid-game)))
    (let ((cg-bid-svg-ui nil) (inhibit-read-only t))
      (cg-bid--insert-graphical cg-bid--game)
      (cg-bid--insert-buttons cg-bid--game))
    (let ((s (buffer-string)))
      (should-not (string-match-p "RET\\] play card" s))   ; key-help removed
      (should (string-match-p "Help" s))                    ; button row present
      (should-not (string-match-p "New" s)))))              ; no mid-hand new deal

;;;; Live 500 (networking)

(ert-deftest cgt-bid-net-filter ()
  "Per-seat 500 state hides others' cards and rotates the viewer South."
  (cl-flet ((ck (h) (sort (mapcar #'prin1-to-string h) #'string<)))
    (let* ((cg-bid--human-seats '(0 1 2 3))
           (g (cg-bid--deal (make-instance 'cg-bid-game) 3))
           (st (cg-net-game-state g 1)))      ; West's viewpoint
      ;; West sees its own ten cards rotated to index 0
      (should (equal (ck (aref (plist-get st :hands) 0))
                     (ck (cg-bid--hand g 1))))
      ;; opponents are face-down counts only, not the real cards
      (should (= 10 (length (aref (plist-get st :hands) 1))))
      (should-not (equal (ck (aref (plist-get st :hands) 1))
                         (ck (cg-bid--hand g 2))))
      ;; the kitty stays hidden
      (should-not (plist-get st :kitty))
      ;; South (abs 0) bids first; in West's frame that is seat 3
      (should (= 3 (plist-get st :bidder)))
      (should (= 1 (plist-get st :you))))))   ; :you is the absolute seat

(ert-deftest cgt-bid-net-apply ()
  "The host applies only legal, in-turn moves."
  (let* ((cg-bid--human-seats '(0 1 2 3))
         (g (cg-bid--deal (make-instance 'cg-bid-game) 3)))
    (should-not (cg-net-apply-move g 1 '(pass)))   ; not West's turn yet
    (should (cg-net-apply-move g 0 '(pass)))       ; South may pass
    (should (aref (cg-get g :passed) 0))
    (should-not (cg-net-apply-move g 0 '(play (0 . 0)))) ; wrong phase
    ))

(ert-deftest cgt-bid-net-loopback ()
  "A 500 move travels client -> host -> filtered broadcast over TCP."
  (condition-case _
      (delete-process
       (make-network-process :name "cgt-probe2" :server t :service 0
                             :host "127.0.0.1" :family 'ipv4))
    (error (ert-skip "TCP not available")))
  (cl-flet ((pump () (dotimes (_ 16) (accept-process-output nil 0.05))))
    (let* ((cg-bid--human-seats '(0 1))
           (hgame (cg-bid--deal (make-instance 'cg-bid-game) 3))
           (srv (cg-net-host-start hgame 0))
           (port (process-contact srv :service))
           (cgame (make-instance 'cg-bid-game)))
      (setf (cg-net-host-next-seat cg-net--host) 1) ; reserve South for host
      (unwind-protect
          (progn
            (cg-net-connect "127.0.0.1" port "P1" cgame)
            (pump)
            ;; the client (seat 1) sees its own ten cards at index 0
            (should (= 10 (length (aref (cg-get cgame :hands) 0))))
            (should (equal
                     (sort (mapcar #'prin1-to-string (aref (cg-get cgame :hands) 0)) #'string<)
                     (sort (mapcar #'prin1-to-string (cg-bid--hand hgame 1)) #'string<)))
            ;; make it the client's turn, then let it pass
            (cg-put hgame :bidder 1)
            (cg-net-host-broadcast)
            (pump)
            (should (= 0 (cg-get cgame :bidder)))  ; rotated: West sees itself bidding
            (cg-net-send-move '(pass))
            (pump)
            (should (aref (cg-get hgame :passed) 1)))
        (cg-net-disconnect)
        (cg-net-host-stop)))))


;;;; --- New games added 2026-06-24: solitaires, trick games, Crazy Eights ---

(ert-deftest cgt-sol-deck ()
  (should (= 52 (length (cg-sol--make-deck 1))))
  (should (= 104 (length (cg-sol--make-deck 2)))))

(ert-deftest cgt-sol-klondike-deal ()
  (let* ((g (cg-sol--deal (cg-klondike-game))))
    (should (= 7 (oref g ncols)))
    (dotimes (c 7)
      (should (= (1+ c) (length (cg-sol--col g c))))
      (should (= c (cg-sol--down g c)))
      ;; top card is face up
      (should (cg-sol--col-top g c)))
    (should (= 24 (length (cg-get g :stock))))
    (should (cl-every #'null (append (cg-get g :found) nil)))))

(ert-deftest cgt-sol-freecell-deal ()
  (let* ((g (cg-sol--deal (cg-freecell-game)))
         (total 0))
    (should (= 8 (oref g ncols)))
    (dotimes (c 8) (cl-incf total (length (cg-sol--col g c)))
             (should (= 0 (cg-sol--down g c))))
    (should (= 52 total))
    (should (null (cg-get g :stock)))
    (should (= 4 (length (cg-get g :free))))))

(ert-deftest cgt-sol-spider-deal ()
  (let* ((g (cg-sol--deal (cg-spider-game)))
         (total 0))
    (should (= 10 (oref g ncols)))
    (dotimes (c 10) (cl-incf total (length (cg-sol--col g c))))
    (should (= 54 total))
    (should (= 50 (length (cg-get g :stock))))
    (dotimes (c 4) (should (= 6 (length (cg-sol--col g c)))))
    (dotimes (k 6) (should (= 5 (length (cg-sol--col g (+ 4 k))))))))

(ert-deftest cgt-sol-rules-alt ()
  (let ((g (cg-klondike-game)))
    ;; red 6 onto black 7 ok; black 6 onto black 7 no
    (should (cg-sol--place-p g '(0 . 6) '(3 . 5)))      ; 7s under 6h
    (should-not (cg-sol--place-p g '(0 . 6) '(1 . 5)))  ; 6c on 7s same color
    (should (cg-sol--empty-accepts g '(0 . 12)))        ; king
    (should-not (cg-sol--empty-accepts g '(0 . 11)))))  ; queen

(ert-deftest cgt-sol-rules-spider ()
  (let ((g (cg-spider-game)))
    ;; build down any suit
    (should (cg-sol--place-p g '(0 . 6) '(3 . 5)))
    (should (cg-sol--place-p g '(0 . 6) '(1 . 5)))
    ;; run cohesion requires same suit
    (should (cg-sol--link-p g '(0 . 6) '(0 . 5)))
    (should-not (cg-sol--link-p g '(0 . 6) '(1 . 5)))
    (should (cg-sol--empty-accepts g '(0 . 3)))))

(ert-deftest cgt-sol-top-run ()
  (let ((g (cg-klondike-game)))
    (cg-put g :tableau (vector (list '(0 . 9) '(2 . 8) '(1 . 7))))  ; 9s 8d 7c
    (cg-put g :down (vector 0))
    ;; 9s(black) 8d(red) 7c(black) is a valid alt run of 3
    (should (= 3 (length (cg-sol--top-run g 0))))
    ;; break color: 9s 8d 7d -> only 8d 7d? 7d red on 8d red invalid -> run is just 7d
    (cg-put g :tableau (vector (list '(0 . 9) '(2 . 8) '(2 . 7))))
    (should (= 1 (length (cg-sol--top-run g 0))))))

(ert-deftest cgt-sol-move-col ()
  (let ((g (cg-sol--deal (cg-klondike-game))))
    ;; craft: col0 top = 7c(black), col1 top = 6h(red); move 6h onto 7c
    (cg-put g :tableau (vector (list '(1 . 7)) (list '(3 . 6)) nil nil nil nil nil))
    (cg-put g :down (vector 0 0 0 0 0 0 0))
    (let ((cards (last (cg-sol--col g 1) 1)))
      (should (cg-sol--can-drop g '(col . 0) cards))
      (cg-sol--take g '(col . 1) 1)
      (cg-sol--drop g '(col . 0) cards))
    (should (equal '((1 . 7) (3 . 6)) (cg-sol--col g 0)))
    (should (null (cg-sol--col g 1)))))

(ert-deftest cgt-sol-foundation-and-win ()
  (let ((g (cg-sol--deal (cg-klondike-game))))
    ;; empty foundations: place an Ace then a 2 of same suit
    (should (cg-sol--found-accepts g 0 '(0 . 0)))
    (cg-sol--drop g '(found . 0) (list '(0 . 0)))
    (should (cg-sol--found-accepts g 0 '(0 . 1)))
    (should-not (cg-sol--found-accepts g 0 '(1 . 1)))
    ;; build a winning state: fill all four foundations A..K
    (let ((found (make-vector 4 nil)))
      (dotimes (s 4)
        (aset found s (cl-loop for r below 13 collect (cons s r))))
      (cg-put g :found found))
    (should (cg-won-p g))))

(ert-deftest cgt-sol-spider-harvest ()
  (let ((g (cg-sol--deal (cg-spider-game))))
    ;; put a complete K..A spade run as the whole of column 0
    (cg-put g :tableau (let ((v (cg-get g :tableau)))
                         (aset v 0 (cl-loop for r from 12 downto 0 collect (cons 0 r)))
                         v))
    (cg-put g :down (let ((v (cg-get g :down))) (aset v 0 0) v))
    (cg-put g :sets 0)
    (cg-sol--harvest g)
    (should (= 1 (cg-get g :sets)))
    (should (null (cg-sol--col g 0)))))

(ert-deftest cgt-sol-undo ()
  (let ((g (cg-sol--deal (cg-klondike-game))))
    (cg-put g :tableau (vector (list '(1 . 7)) (list '(3 . 6)) nil nil nil nil nil))
    (cg-put g :down (vector 0 0 0 0 0 0 0))
    (cg-sol--snapshot g)
    (let ((cards (last (cg-sol--col g 1) 1)))
      (cg-sol--take g '(col . 1) 1)
      (cg-sol--drop g '(col . 0) cards))
    (should (null (cg-sol--col g 1)))
    (should (cg-sol--restore g))
    (should (equal '((3 . 6)) (cg-sol--col g 1)))))

(ert-deftest cgt-sol-render-builds ()
  (dolist (cls '(cg-klondike-game cg-freecell-game cg-spider-game cg-yukon-game))
    (let ((g (cg-sol--deal (make-instance cls))))
      (should (stringp (cg-render g))))))

(defun cgt--init-hearts ()
  (let ((g (cg-hearts-game)))
    (cg-put g :scores (make-vector 4 0))
    (cg-trick--start-hand g) g))

(defun cgt--init-spades ()
  (let ((g (cg-spades-game)))
    (cg-put g :scores (make-vector 4 0))
    (cg-put g :bags (make-vector 2 0))
    (cg-put g :dealer 3)
    (cg-trick--deal g)
    (cg-put g :bids (let ((v (make-vector 4 0)))
                      (dotimes (s 4) (aset v s (cg-trick--ai-bid g s))) v))
    (cg-trick--leader-init g) g))

(ert-deftest cgt-trick-deal ()
  (let ((g (cg-trick--deal (cg-hearts-game))))
    (let ((tot 0))
      (dotimes (s 4) (cl-incf tot (length (cg-trick--hand g s))))
      (should (= 52 tot)))
    (dotimes (s 4) (should (= 13 (length (cg-trick--hand g s)))))))

(ert-deftest cgt-trick-winner-trump ()
  (let ((g (cg-spades-game)))
    ;; play order: S leads 10h, W 13h(K), N 2s(trump), E 12h
    (cg-put g :trick (list (cons 3 '(3 . 11)) (cons 2 '(0 . 0))
                           (cons 1 '(3 . 12)) (cons 0 '(3 . 8))))
    ;; :trick is stored reversed (newest first); winner = North (trump)
    (should (= 2 (cg-trick--winner g)))))

(ert-deftest cgt-trick-winner-notrump ()
  (let ((g (cg-hearts-game)))
    ;; led hearts; highest heart wins (no trump)
    (cg-put g :trick (list (cons 3 '(1 . 12)) (cons 2 '(3 . 12))
                           (cons 1 '(3 . 5)) (cons 0 '(3 . 8))))
    ;; North played Ace of hearts (3 . 12) -> winner North
    (should (= 2 (cg-trick--winner g)))))

(ert-deftest cgt-hearts-first-must-be-2c ()
  (let* ((g (cgt--init-hearts))
         (leader (cg-get g :turn)))
    (let ((moves (cg-trick--legal-moves g leader)))
      (should (equal moves '((1 . 0)))))))

(ert-deftest cgt-hearts-full-hand ()
  (let ((g (cgt--init-hearts)))
    (cg-trick--simulate-hand g)
    ;; 13 tricks distributed
    (should (= 13 (apply #'+ (append (cg-get g :tricks) nil))))
    ;; total points across players is 26 (no moon) or 78 (moon: 3*26)
    (let ((tot (apply #'+ (append (cg-get g :scores) nil))))
      (should (memq tot '(26 78))))))

(ert-deftest cgt-hearts-many-hands ()
  (let ((g (cgt--init-hearts)) (n 0))
    (while (and (not (cg-trick--game-over-p g)) (< n 60))
      (cg-trick--simulate-hand g)
      (cl-incf n)
      (unless (cg-trick--game-over-p g) (cg-trick--start-hand g)))
    (should (cg-trick--game-over-p g))
    (should (integerp (cg-trick--winner-seat g)))))

(ert-deftest cgt-spades-bid-range ()
  (let ((g (cg-trick--deal (cg-spades-game))))
    (dotimes (s 4)
      (let ((b (cg-trick--ai-bid g s)))
        (should (and (>= b 1) (<= b 13)))))))

(ert-deftest cgt-spades-full-hand ()
  (let ((g (cgt--init-spades)))
    (cg-trick--simulate-hand g)
    (should (= 13 (apply #'+ (append (cg-get g :tricks) nil))))
    ;; teammates share a score
    (should (= (aref (cg-get g :scores) 0) (aref (cg-get g :scores) 2)))
    (should (= (aref (cg-get g :scores) 1) (aref (cg-get g :scores) 3)))))

(ert-deftest cgt-spades-full-game ()
  (let ((g (cgt--init-spades)) (n 0))
    (while (and (not (cg-trick--game-over-p g)) (< n 80))
      (cg-trick--simulate-hand g)
      (cl-incf n)
      (unless (cg-trick--game-over-p g)
        (cg-trick--deal g)
        (cg-put g :bids (let ((v (make-vector 4 0)))
                          (dotimes (s 4) (aset v s (cg-trick--ai-bid g s))) v))
        (cg-trick--leader-init g)))
    (should (cg-trick--game-over-p g))))

(ert-deftest cgt-trick-ui-new-and-render ()
  (dolist (cls '(cg-hearts-game cg-spades-game))
    (let ((noninteractive t)
          (g (make-instance cls)))
      (cg-trick--new g)            ; spades bids via ai (noninteractive), hearts -> pass phase
      (should (stringp (cg-render g)))
      (should (memq (cg-get g :phase) '(pass play))))))
(ert-deftest cgt-trick-ui-hearts-pass ()
  (let* ((noninteractive t) (g (make-instance 'cg-hearts-game)))
    (cg-trick--new g)
    ;; hand 1 passes left; mark 3 cards from South and pass
    (when (eq (cg-get g :phase) 'pass)
      (cg-put g :marks (cl-subseq (cg-trick--sort (cg-trick--hand g 0)) 0 3))
      (cg-trick--do-pass g)
      (should (eq (cg-get g :phase) 'play))
      ;; cards are conserved: hands plus the cards already played this trick
      (let ((tot (length (cg-get g :trick))))
        (dotimes (s 4) (cl-incf tot (length (cg-trick--hand g s))))
        (should (= 52 tot))))))
(ert-deftest cgt-trick-ui-spades-human-play ()
  (let* ((noninteractive t) (g (make-instance 'cg-spades-game)))
    (cg-trick--new g)              ; runs AI until South's turn
    (should (eq (cg-get g :phase) 'play))
    (should (= 0 (cg-get g :turn)))
    ;; play a legal card for South, then run; eventually hand completes/scores
    (let ((guard 0))
      (while (and (eq (cg-get g :phase) 'play) (< guard 20)
                  (= 0 (cg-get g :turn)))
        (let ((card (car (cg-trick--legal-moves g 0))))
          (cg-trick--play g 0 card)
          (cg-trick--run g))
        (cl-incf guard)))
    (should (vectorp (cg-get g :scores)))))

(ert-deftest cgt-eights-deal ()
  (let* ((cg-eights-players 3) (g (cg-eights--deal (cg-eights-game))))
    (should (= 3 (cg-get g :nplayers)))
    (dotimes (s 3) (should (= 5 (length (cg-eights--hand g s)))))
    (should (cg-eights--top g))
    (should-not (= cg-eights--wild (cdr (cg-eights--top g))))))  ; starter not an eight
(ert-deftest cgt-eights-legal ()
  (let ((g (cg-eights-game)))
    (cg-put g :discard (list '(0 . 3))) (cg-put g :suit 0)
    (should (cg-eights--legal-p g '(0 . 8)))      ; same suit (spades)
    (should (cg-eights--legal-p g '(1 . 3)))      ; same rank
    (should (cg-eights--legal-p g '(2 . 6)))      ; eight (wild)
    (should-not (cg-eights--legal-p g '(1 . 4))))) ; neither
(ert-deftest cgt-eights-full-game ()
  (let* ((cg-eights-players 4) (noninteractive t) (g (cg-eights--deal (cg-eights-game)))
         (guard 0))
    ;; drive entirely by AI from every seat
    (while (and (eq (cg-get g :phase) 'play) (< guard 2000))
      (cg-eights--ai-turn g (cg-get g :turn))
      (when (>= (cg-get g :passes) (cg-get g :nplayers)) (cg-eights--deadlock g))
      (cl-incf guard))
    (should (eq (cg-get g :phase) 'game-over))
    (should (integerp (cg-get g :winner)))))
(ert-deftest cgt-eights-wild-sets-suit ()
  (let* ((cg-eights-players 2) (g (cg-eights--deal (cg-eights-game))))
    (cg-eights--set-hand g 0 (list '(3 . 6)))   ; the human holds only an eight
    (cg-put g :discard (list '(0 . 3))) (cg-put g :suit 0)
    (cg-eights--play g 0 '(3 . 6) 2)            ; play it, name diamonds (2)
    (should (= 2 (cg-get g :suit)))
    (should (eq (cg-get g :phase) 'game-over))))  

;;;; --- Wave 2 (2026-06-24): Forty Thieves/Scorpion/Canfield, Golf/TriPeaks/Pyramid, Whist/Oh Hell, President ---

(ert-deftest cgt-sol-forty-deal ()
  (let ((g (cg-sol--deal (cg-forty-game))) (tot 0))
    (should (= 10 (oref g ncols)))
    (dotimes (c 10) (should (= 4 (length (cg-sol--col g c)))) (cl-incf tot 4))
    (should (= 8 (oref g nfound)))
    (should (= 64 (length (cg-get g :stock))))
    (should (= 104 (+ tot (length (cg-get g :stock)))))))

(ert-deftest cgt-sol-forty-no-redeal ()
  (let ((g (cg-sol--deal (cg-forty-game))))
    (cg-put g :stock nil) (cg-put g :waste '((0 . 0) (1 . 1)))
    (cg-sol--stock-action g)            ; redeal nil -> stays empty
    (should (null (cg-get g :stock)))))

(ert-deftest cgt-sol-forty-win ()
  (let ((g (cg-sol--deal (cg-forty-game))) (found (make-vector 8 nil)))
    (dotimes (i 8) (aset found i (cl-loop for r below 13 collect (cons (mod i 4) r))))
    (cg-put g :found found)
    (should (cg-won-p g))))

(ert-deftest cgt-sol-scorpion-deal ()
  (let ((g (cg-sol--deal (cg-scorpion-game))) (tot 0))
    (should (= 7 (oref g ncols)))
    (dotimes (c 7) (should (= 7 (length (cg-sol--col g c)))) (cl-incf tot 7))
    (dotimes (c 4) (should (= 3 (cg-sol--down g c))))
    (dotimes (k 3) (should (= 0 (cg-sol--down g (+ 4 k)))))
    (should (= 3 (length (cg-get g :stock))))
    (should (= 0 (oref g nfound)))))

(ert-deftest cgt-sol-scorpion-harvest-win ()
  (let ((g (cg-sol--deal (cg-scorpion-game))))
    (cg-put g :sets 3)
    ;; place a complete K..A clubs run as column 0
    (aset (cg-get g :tableau) 0 (cl-loop for r from 12 downto 0 collect (cons 1 r)))
    (aset (cg-get g :down) 0 0)
    (cg-sol--harvest g)
    (should (= 4 (cg-get g :sets)))
    (should (cg-won-p g))))

(ert-deftest cgt-sol-canfield-deal ()
  (let ((g (cg-sol--deal (cg-canfield-game))))
    (should (= 13 (length (cg-get g :reserve))))
    (should (= 1 (length (aref (cg-get g :found) 0))))
    (dotimes (c 4) (should (= 1 (length (cg-sol--col g c)))))
    (should (= 34 (length (cg-get g :stock))))
    ;; base equals the rank of the first foundation card
    (should (= (oref g base) (cdr (car (aref (cg-get g :found) 0)))))))

(ert-deftest cgt-sol-canfield-base-wrap ()
  (let ((g (cg-canfield-game)))
    (oset g base 5) (oset g wrap t)
    (cg-put g :found (make-vector 4 nil))
    (should (cg-sol--found-accepts g 1 '(2 . 5)))       ; empty -> base rank 5
    (should-not (cg-sol--found-accepts g 1 '(2 . 6)))
    ;; wrap: a King on top accepts the Ace next
    (aset (cg-get g :found) 0 (list '(0 . 12)))
    (should (cg-sol--found-accepts g 0 '(0 . 0)))))

(ert-deftest cgt-sol-canfield-autofill ()
  (let ((g (cg-sol--deal (cg-canfield-game))))
    (let ((rlen (length (cg-get g :reserve))))
      (aset (cg-get g :tableau) 0 nil)   ; empty a column
      (cg-sol--autofill g)
      (should (= 1 (length (cg-sol--col g 0))))
      (should (= (1- rlen) (length (cg-get g :reserve)))))))

(ert-deftest cgt-pat-golf-deal ()
  (let ((g (cg-pat--deal (cg-golf-game))))
    (should (= 35 (length (cg-get g :cards))))
    (should (= 16 (length (cg-get g :stock))))
    (should (= 1 (length (cg-get g :waste))))
    (should (= 7 (length (cg-pat--exposed g))))))   ; one per column (r=4)

(ert-deftest cgt-pat-tripeaks-deal ()
  (let ((g (cg-pat--deal (cg-tripeaks-game))))
    (should (= 28 (length (cg-get g :cards))))
    (should (= 23 (length (cg-get g :stock))))
    (should (equal (number-sequence 18 27) (cg-pat--exposed g)))   ; base row
    (should-not (cg-pat--exposed-p g 0))))                          ; apex covered

(ert-deftest cgt-pat-pyramid-deal ()
  (let ((g (cg-pat--deal (cg-pyramid-game))))
    (should (= 28 (length (cg-get g :cards))))
    (should (= 24 (length (cg-get g :stock))))
    (should (null (cg-get g :waste)))
    (should (equal (number-sequence 21 27) (cg-pat--exposed g)))   ; base row r=6
    (should-not (cg-pat--exposed-p g 0))))                          ; apex covered by 1,2

(ert-deftest cgt-pat-exposed-reveal ()
  (let ((g (cg-pat--deal (cg-golf-game))))
    ;; clear column 0's lower cards; slot 0 (top) becomes exposed
    (dolist (i '(4 3 2 1)) (cg-pat--remove-slot g i))
    (should (cg-pat--exposed-p g 0))))

(ert-deftest cgt-pat-build-and-win ()
  (let ((g (cg-pat--deal (cg-golf-game))))
    ;; reduce to one exposed card adjacent to the waste top
    (cg-put g :cards (let ((v (make-vector 35 nil))) (aset v 34 '(0 . 5)) v))
    (cg-put g :waste (list '(1 . 4)))           ; 5 of clubs, rank 4; 6s is adjacent
    (cg-put g :stock nil) (cg-put g :cursor 0)
    (with-temp-buffer
      (setq cg-pat--game g)
      (cg-pat-act))                              ; plays slot 34 onto the waste
    (should (null (aref (cg-get g :cards) 34)))
    (should (cg-won-p g))))

(ert-deftest cgt-pat-sum13 ()
  (let ((g (cg-pat--deal (cg-pyramid-game))))
    ;; King value is 13
    (should (= 13 (cg-pat--value '(0 . 12))))
    ;; mark two base cards summing to 13 -> both removed
    (cg-put g :cards (let ((v (make-vector 28 nil)))
                       (aset v 21 '(0 . 4)) (aset v 22 '(1 . 7)) v))  ; 5 (val5) + 8 (val8) = 13
    (cg-put g :marks nil)
    (cg-pat--toggle-mark g '(slot . 21))
    (cg-pat--toggle-mark g '(slot . 22))
    (should (null (aref (cg-get g :cards) 21)))
    (should (null (aref (cg-get g :cards) 22)))))

(ert-deftest cgt-pat-render ()
  (dolist (cls '(cg-golf-game cg-tripeaks-game cg-pyramid-game))
    (let ((g (cg-pat--deal (make-instance cls))))
      (should (stringp (cg-render g))))))

(defun cgt--drive (g limit)
  "Play a whole match with AI for every seat, including the human seat 0."
  (let ((n 0))
    (while (and (not (cg-trick--game-over-p g)) (< n limit))
      (when (eq (cg-get g :phase) 'play)
        (cg-trick--play g (cg-get g :turn) (cg-trick--ai-play g (cg-get g :turn)))
        (cg-trick--run g))
      (cl-incf n))))

(ert-deftest cgt-whist-deal-trump ()
  (let ((noninteractive t) (g (make-instance 'cg-whist-game)))
    (cg-trick--new g)
    (should (memq (oref g trump) '(0 1 2 3)))
    (let ((tot (length (cg-get g :trick))))
      (dotimes (s 4) (cl-incf tot (length (cg-trick--hand g s))))
      (should (= 52 tot)))))

(ert-deftest cgt-whist-full-game ()
  (let ((noninteractive t) (g (make-instance 'cg-whist-game)))
    (cg-trick--new g)
    (cgt--drive g 400)
    (should (cg-trick--game-over-p g))
    (should (integerp (cg-trick--winner-seat g)))))

(ert-deftest cgt-ohhell-rounds ()
  (let ((noninteractive t) (g (make-instance 'cg-ohhell-game)))
    (cg-trick--new g)
    ;; first round deals 7 cards each
    (should (= 7 (length (cg-trick--hand g 0))))
    (cgt--drive g 400)
    (should (cg-trick--game-over-p g))
    (should (= 7 (cg-get g :round)))            ; seven rounds played
    (should (integerp (cg-trick--winner-seat g)))))

(ert-deftest cgt-ohhell-exact-scoring ()
  (let ((g (make-instance 'cg-ohhell-game)))
    (cg-put g :scores (make-vector 4 0))
    (cg-put g :round 0)
    (cg-put g :bids (vector 2 0 1 3))
    (cg-put g :tricks (vector 2 1 1 0))         ; seats 0 and 2 made exact bids
    (cg-trick--score-hand g)
    (should (= 12 (aref (cg-get g :scores) 0)))  ; 10 + 2
    (should (= 0 (aref (cg-get g :scores) 1)))   ; bid 0 took 1 -> miss
    (should (= 11 (aref (cg-get g :scores) 2)))  ; 10 + 1
    (should (= 0 (aref (cg-get g :scores) 3)))))

(ert-deftest cgt-whist-render ()
  (let ((noninteractive t) (g (make-instance 'cg-whist-game)))
    (cg-trick--new g) (should (stringp (cg-render g)))))

(ert-deftest cgt-pres-power ()
  (should (> (cg-pres--power 0) (cg-pres--power 12)))   ; the Two beats the Ace
  (should (< (cg-pres--power 1) (cg-pres--power 11))))  ; 3 below King

(ert-deftest cgt-pres-deal ()
  (let* ((cg-president-players 4) (g (cg-pres--deal (cg-president-game))) (tot 0))
    (should (= 4 (cg-get g :nplayers)))
    (dotimes (s 4) (cl-incf tot (length (cg-pres--hand g s))))
    (should (= 52 tot))
    (should (= 0 (cg-get g :count)))))

(ert-deftest cgt-pres-legal ()
  (let ((g (cg-president-game)))
    (cg-put g :hands (vector (list '(0 . 5) '(1 . 5) '(0 . 8)) nil nil nil))
    (cg-put g :nplayers 4)
    ;; leading: any rank ok
    (cg-put g :count 0) (cg-put g :top -1)
    (should (= 2 (length (cg-pres--legal-ranks g 0))))   ; ranks 5 and 8
    ;; following a single of power 5: need power>5 -> only rank 8
    (cg-put g :count 1) (cg-put g :top 5)
    (should (equal '(8) (cg-pres--legal-ranks g 0)))
    ;; following a PAIR: need 2 of a higher rank -> rank 5 has two but power 5 not >5; none
    (cg-put g :count 2) (cg-put g :top 5)
    (should (null (cg-pres--legal-ranks g 0)))))

(ert-deftest cgt-pres-full-game ()
  (let* ((cg-president-players 4) (g (cg-pres--deal (cg-president-game))) (n 0))
    (while (and (eq (cg-get g :phase) 'play) (< n 5000))
      (cg-pres--ai-move g (cg-get g :turn))
      (cl-incf n))
    (should (eq (cg-get g :phase) 'game-over))
    (should (= 4 (length (cg-get g :order))))            ; everyone placed
    (should (= 4 (length (delete-dups (copy-sequence (cg-get g :order)))))))) ; all distinct

(ert-deftest cgt-pres-exchange ()
  (let* ((cg-president-players 4) (g (cg-president-game)))
    ;; simulate a prior finishing order, then deal and check the swap happened
    (cg-put g :order '(2 3 1 0))   ; prez=2, scum=0
    (cg-pres--deal g)
    ;; conservation: still 52 cards across 4 hands after the exchange
    (let ((tot 0)) (dotimes (s 4) (cl-incf tot (length (cg-pres--hand g s))))
      (should (= 52 tot)))))

(ert-deftest cgt-pres-render ()
  (let ((g (cg-pres--deal (cg-president-game)))) (should (stringp (cg-render g)))))
;;;; Rummy family

(ert-deftest cgt-rummy-set-run ()
  (should (cg-rummy-set-p '((0 . 5) (1 . 5) (2 . 5)) :distinct-suits t))
  (should-not (cg-rummy-set-p '((0 . 5) (0 . 5) (2 . 5)) :distinct-suits t))
  (should (cg-rummy-run-p '((0 . 3) (0 . 4) (0 . 5))))
  (should-not (cg-rummy-run-p '((0 . 3) (1 . 4) (0 . 5))))
  (should (cg-rummy-run-p '((0 . 10) (0 . 11) (0 . 12) (0 . 0)) :ace-high t))
  (should-not (cg-rummy-run-p '((0 . 11) (0 . 12) (0 . 0))))
  (should (cg-rummy-run-p '((0 . 0) (0 . 1) (0 . 2)))))

(ert-deftest cgt-rummy-best-partition ()
  (let* ((hand '((0 . 2)(0 . 3)(0 . 4) (0 . 6)(1 . 6)(2 . 6)
                 (3 . 8)(3 . 9)(3 . 10) (3 . 12)))
         (p (cg-rummy-best-partition hand)))
    (should (= 3 (length (plist-get p :melds))))
    (should (= 10 (plist-get p :count))))
  (should (= 0 (cg-rummy-deadwood
                '((0 . 0)(0 . 1)(0 . 2) (1 . 4)(1 . 5)(1 . 6)
                  (2 . 8)(2 . 9)(2 . 10)(2 . 11))))))

(ert-deftest cgt-rummy-layoff ()
  (should (cg-rummy-layoff-p '(3 . 11) '(((3 . 8)(3 . 9)(3 . 10)))))
  (should-not (cg-rummy-layoff-p '(0 . 4) '(((3 . 8)(3 . 9)(3 . 10))))))

(ert-deftest cgt-gin-full-game ()
  (let ((g (cg-gin-game)) (turns 0))
    (cg-gin--deal g)
    (while (and (not (eq (cg-get g :phase) 'game-over)) (< turns 100000))
      (if (eq (cg-get g :phase) 'hand-over)
          (progn (cg-put g :reveal nil) (cg-gin--deal g))
        (cg-gin--ai-turn g (cg-get g :turn)) (cl-incf turns)))
    (should (eq (cg-get g :phase) 'game-over))
    (should (>= (apply #'max (append (cg-get g :scores) nil)) cg-gin-target))))

(ert-deftest cgt-gin-render () (let ((g (cg-gin--deal (cg-gin-game))))
                                 (should (stringp (cg-render g)))))

(ert-deftest cgt-tablemeld-games ()
  (dolist (mk (list (lambda () (cg-rummy-basic-game :nplayers 3 :hand-size 7))
                    (lambda () (cg-rum500-game :nplayers 3 :hand-size 7))))
    (let ((g (funcall mk)) (turns 0))
      (cg-tm--deal g)
      (while (and (not (eq (cg-get g :phase) 'game-over)) (< turns 200000))
        (if (eq (cg-get g :phase) 'hand-over)
            (progn (cg-put g :reveal nil) (cg-tm--deal g))
          (cg-tm--ai-turn g (cg-get g :turn)) (cl-incf turns)))
      (should (eq (cg-get g :phase) 'game-over))
      (should (stringp (cg-render g))))))

(ert-deftest cgt-tablemeld-conservation ()
  (let ((g (cg-rum500-game :nplayers 3 :hand-size 7)))
    (cg-tm--deal g)
    (dotimes (_ 30) (when (eq (cg-get g :phase) 'play)
                      (cg-tm--ai-turn g (cg-get g :turn))))
    (let ((tot (+ (length (cg-get g :stock)) (length (cg-get g :discard))
                  (apply #'+ (mapcar (lambda (r) (length (cdr r))) (cg-get g :table))))))
      (dotimes (s 3) (setq tot (+ tot (length (cg-rummy--hand g s)))))
      (should (= tot 52)))))

(ert-deftest cgt-handfoot-book ()
  (should (cg-hf--book-valid-p '((0 . 7)(1 . 7)(2 . 7))))
  (should (cg-hf--book-valid-p '((0 . 7)(1 . 7)(joker . 0))))      ; dirty
  (should-not (cg-hf--book-valid-p '((0 . 7)(joker . 0)(joker . 0)))) ; wilds > nats
  (should-not (cg-hf--book-valid-p '((0 . 2)(1 . 2)(2 . 2))))      ; threes
  (should (cg-hf--book-complete-p '(a b c d e f g)))
  (should (cg-hf--book-clean-p '((0 . 7)(1 . 7)(2 . 7))))
  (should-not (cg-hf--book-clean-p '((0 . 7)(1 . 7)(joker . 0)))))

(ert-deftest cgt-handfoot-full-game ()
  (let ((g (cg-handfoot-game)) (rounds 0) (turns 0) (expect (* 5 54)))
    (cg-put g :nplayers 4) (cg-put g :nteams 2) (cg-put g :scores (make-vector 2 0))
    (cg-hf--deal g)
    (while (and (not (eq (cg-get g :phase) 'game-over)) (< rounds 60))
      (cond ((eq (cg-get g :phase) 'round-over)
             (cg-put g :reveal nil) (cg-hf--deal g) (cl-incf rounds))
            (t (cg-hf--ai-turn g (cg-get g :turn)) (cl-incf turns)
               (when (> turns 500000) (error "runaway")))))
    (should (eq (cg-get g :phase) 'game-over))
    (should (stringp (cg-render g)))))
;;;; Matching games (Go Fish, Old Maid)

(ert-deftest cgt-gofish-full ()
  (let ((cg-go-fish-players 4) (g (cg-go-fish-game)) (guard 0))
    (cg-gf--deal g)
    (while (and (not (eq (cg-get g :phase) 'game-over)) (< guard 20000))
      (cg-gf--ai-turn g (cg-get g :turn)) (cl-incf guard))
    (should (eq (cg-get g :phase) 'game-over))
    (let ((tot 0)) (dotimes (s 4) (cl-incf tot (cg-gf--books g s)))
      (should (= tot 13)))
    (should (stringp (cg-render g)))))

(ert-deftest cgt-oldmaid-full ()
  (let ((cg-old-maid-players 4) (g (cg-old-maid-game)) (guard 0))
    (cg-om--deal g)
    (while (and (not (eq (cg-get g :phase) 'game-over)) (< guard 20000))
      (cg-om--ai-turn g (cg-get g :turn)) (cl-incf guard))
    (should (eq (cg-get g :phase) 'game-over))
    (should (= 1 (cg-om--total g)))
    (should (stringp (cg-render g)))))

;;;; Cribbage

(ert-deftest cgt-cribbage-scorer ()
  (should (= 29 (cg-crib--score-show '((0 . 4)(1 . 4)(3 . 4)(2 . 10)) '(2 . 4))))
  (should (= 12 (cg-crib--count-pairs '((0 . 4)(1 . 4)(2 . 4)(3 . 4)))))
  (should (= 5 (cg-crib--count-runs '((0 . 1)(0 . 2)(0 . 3)(0 . 4)(0 . 5)))))
  (should (= 2 (cg-crib--peg-score '((0 . 10)(0 . 4)) 15)))
  (should (= 3 (cg-crib--peg-score '((0 . 2)(0 . 3)(0 . 4)) 12)))
  (should (= 6 (cg-crib--peg-score '((0 . 6)(1 . 6)(2 . 6)) 21))))

(ert-deftest cgt-cribbage-full ()
  (let ((g (cg-cribbage-game)) (deals 0))
    (cg-put g :dealer 1)
    (cl-flet ((ai-deal (g)
                (cg-crib--deal g)
                (let ((d0 (cg-crib--ai-discard g 0)) (d1 (cg-crib--ai-discard g 1)))
                  (cg-crib--set-hand g 0 (cl-set-difference (cg-crib--hand g 0) d0 :test #'equal))
                  (cg-crib--set-hand g 1 (cl-set-difference (cg-crib--hand g 1) d1 :test #'equal))
                  (cg-put g :crib (append d0 d1)))
                (cg-crib--start-play g)
                (let ((guard 0))
                  (while (and (eq (cg-get g :phase) 'play) (not (cg-crib--peg-over-p g))
                              (< guard 400))
                    (cl-incf guard)
                    (let ((s (cg-get g :pturn)))
                      (if (cg-crib--legal g s) (cg-crib--ai-play g s) (cg-crib--peg-go g s)))))
                (when (and (eq (cg-get g :phase) 'play) (cg-crib--peg-over-p g))
                  (cg-crib--show g))))
      (while (and (not (eq (cg-get g :phase) 'game-over)) (< deals 300))
        (cg-put g :dealer (- 1 (cg-get g :dealer)))
        (ai-deal g) (cl-incf deals)))
    (should (eq (cg-get g :phase) 'game-over))
    (should (stringp (cg-render g)))))

;;;; Fishing games (Scopa, Casino)

(ert-deftest cgt-fish-capture ()
  (let ((g (cg-scopa-game)))
    (cg-put g :table '((0 . 0)(1 . 1)(2 . 3)))
    (should (equal (sort (mapcar #'cdr (cg-fish--capture g '(3 . 4))) #'<) '(0 3))))
  (let ((g (cg-casino-game)))
    (cg-put g :table '((0 . 12)(1 . 12)(2 . 5)))
    (should (= 1 (length (cg-fish--capture g '(3 . 12)))))))

(ert-deftest cgt-fish-full ()
  (dolist (mk (list #'cg-scopa-game #'cg-casino-game))
    (let ((g (funcall mk)) (rounds 0))
      (cg-fish--deal-round g)
      (while (and (not (eq (cg-get g :phase) 'game-over)) (< rounds 200))
        (if (eq (cg-get g :phase) 'round-over)
            (cg-fish--deal-round g)
          (cg-fish--ai-play g (cg-get g :turn)) (cl-incf rounds)))
      (should (eq (cg-get g :phase) 'game-over))
      (should (stringp (cg-render g))))))

;;;; Trick extensions (Euchre, Pitch, Briscola)

(ert-deftest cgt-euchre-bowers ()
  (should (> (cg-euchre--power '(0 . 9) 0 0) (cg-euchre--power '(1 . 9) 0 0)))
  (should (> (cg-euchre--power '(1 . 9) 0 0) (cg-euchre--power '(0 . 12) 0 0)))
  (should (= 120 (let ((s 0)) (dolist (su '(0 1 2 3))
                                (dolist (r cg-briscola--ranks)
                                  (setq s (+ s (cg-bris--points (cons su r)))))) s))))

(ert-deftest cgt-trick-ext-full ()
  (dolist (class '(cg-briscola-game cg-pitch-game cg-euchre-game))
    (let ((g (make-instance class)) (guard 0))
      (cg-trick--new g)
      (while (and (not (eq (cg-get g :phase) 'game-over)) (< guard 50000))
        (cl-incf guard)
        (if (cg-trick--hand-over-p g)
            (cg-trick--finish-hand g)
          (let ((s (cg-get g :turn)))
            (cg-trick--play g s (cg-trick--ai-play g s)))))
      (should (eq (cg-get g :phase) 'game-over))
      (should (stringp (cg-render g))))))

;;;; Spite & Malice

(ert-deftest cgt-spite-legal ()
  (let ((g (cg-spite-game)))
    (cg-spite--deal g) (cg-put g :center (make-vector 4 nil))
    (should (eql 0 (cg-spite--legal-center g '(0 . 0))))   ; Ace starts a pile
    (should (null (cg-spite--legal-center g '(0 . 1))))    ; a Two cannot
    (should (eql 0 (cg-spite--legal-center g '(0 . 12))))  ; King is wild
    (cg-spite--put-center g '(0 . 0) 0)
    (should (= 1 (cg-spite--needed g 0)))))

(ert-deftest cgt-spite-full ()
  (let ((cg-spite-goal-size 10) (g (cg-spite-game)) (turns 0))
    (cg-spite--deal g)
    (while (and (eq (cg-get g :phase) 'play) (< turns 6000))
      (cl-incf turns) (cg-spite--ai-turn g (cg-get g :turn)))
    (should (eq (cg-get g :phase) 'game-over))
    (should (stringp (cg-render g)))))
;;;; Bridge

(ert-deftest cgt-bridge-score ()
  (cl-flet ((b (l s d v tk) (plist-get (cg-bridge--deal-score l s d v tk) :below))
            (a (l s d v tk) (plist-get (cg-bridge--deal-score l s d v tk) :datk))
            (f (l s d v tk) (plist-get (cg-bridge--deal-score l s d v tk) :defend)))
    (should (= 100 (b 3 4 0 nil 9)))     ; 3NT made
    (should (= 120 (b 4 3 0 nil 10)))    ; 4 spades made
    (should (= 180 (b 6 2 0 nil 12)))    ; 6 hearts made
    (should (= 500 (a 6 2 0 nil 12)))    ; small slam bonus
    (should (= 50  (a 1 4 1 nil 7)))     ; 1NT doubled, insult
    (should (= 100 (f 4 3 0 nil 8)))     ; down two undoubled
    (should (= 500 (f 4 3 1 t 8))))      ; down two doubled vulnerable
  (should (= 1 (cg-bridge--trick-winner
                '((0 . (0 . 12)) (1 . (3 . 0)) (2 . (0 . 2)) (3 . (0 . 5))) 3))))

(ert-deftest cgt-bridge-full ()
  (let ((scored 0) (passed 0))
    (dotimes (i 12)
      (let ((g (cg-bridge-game)) (guard 0))
        (cg-put g :dealer (mod i 4))
        (cg-bridge--deal g)
        (while (and (eq (cg-get g :phase) 'auction) (< guard 60))
          (cl-incf guard)
          (let* ((s (cg-get g :bidder)) (call (cg-bridge--ai-call g s)))
            (unless (cg-bridge--legal-call-p g call) (setq call 'pass))
            (cg-bridge--apply-call g s call)
            (cg-bridge--auction-done-p g)))
        (if (eq (cg-get g :phase) 'passed-out) (cl-incf passed)
          (let ((p 0))
            (while (and (eq (cg-get g :phase) 'play) (< p 60))
              (cl-incf p)
              (cg-bridge--play-card g (cg-get g :turn) (cg-bridge--ai-play g (cg-get g :turn)))))
          (when (eq (cg-get g :phase) 'scored)
            (cl-incf scored)
            (should (cl-every #'null (append (cg-get g :hands) nil)))))
        (should (memq (cg-get g :phase) '(scored passed-out)))
        (should (stringp (cg-render g)))))
    (should (> scored 0))))
;;;; Renderer registry / region keystone

(ert-deftest cgt-keystone-regions ()
  (let* ((g (cg-sol--deal (make-instance 'cg-klondike-game)))
         (res (cg-render-svg g)))
    (should (stringp (car res)))
    (should (>= (length (cdr res)) 13))           ; 6 top slots + 7 columns
    (let* ((reg (cl-find '(col . 3) (cdr res) :key #'cdr :test #'equal))
           (rect (car reg)))
      (should reg)
      (should (equal '(col . 3)
                     (cg-regions-hit (cdr res)
                                     (+ (nth 0 rect) (/ (nth 2 rect) 2))
                                     (+ (nth 1 rect) 10)))))
    (should (null (cg-regions-hit (cdr res) -5 -5))))
  (with-temp-buffer
    (setq cg-sol--game (cg-sol--deal (make-instance 'cg-klondike-game)))
    (cg-render-game cg-sol--game)
    (should (oref cg-sol--game renderer))
    (should (> (buffer-size) 0))
    (cg-render-apply cg-sol--game '(col . 5))
    (should (= 11 (cg-get cg-sol--game :cursor)))))   ; spot index of (col . 5)
;;;; Hand-cluster click regions + card-size slider

(ert-deftest cgt-hand-regions ()
  (let* ((cards '((0 . 0) (0 . 1) (1 . 5)))
         (str (cg-rummy--svg-row cards 0 nil nil 'hand))
         (regs (get-text-property 0 'cg-regions str)))
    (should (= (+ 3 (length cg-svg-slider-stops)) (length regs)))   ; 3 cards + stops
    (should (equal '(hand . 0) (cdr (car regs))))
    (should (cl-find-if (lambda (r) (eq (car-safe (cdr r)) 'scale)) regs)))
  (let ((g (cg-gin--deal (cg-gin-game))) (cg-card-scale 1.0))
    (cg-render-apply g '(hand . 2))
    (should (= 2 (cg-get g :cursor)))
    (cg-render-apply g '(scale . 1.5))         ; base method handles scale
    (should (= 1.5 cg-card-scale))
    (cg-render-apply g 'zoom-reset)
    (should (= 1.0 cg-card-scale))))

;;;; Rummy 500 deep pickup and Hand & Foot rule completions

(ert-deftest cgt-rum500-deep-pickup ()
  "Taking a buried discard card melds it at once, keeping the cards above."
  (let ((g (cg-rum500-game :nplayers 2 :hand-size 10)))
    (cg-put g :nplayers 2) (cg-put g :scores (make-vector 2 0))
    (cg-put g :hands (vector (list (cons 0 5) (cons 0 6)) nil)) ; 6S 7S
    (cg-put g :discard (list (cons 1 9) (cons 0 4) (cons 2 3))) ; top 10C, buried 5S
    (cg-put g :table nil) (cg-put g :laid (make-vector 2 0))
    (cg-put g :turn 0) (cg-put g :step 'draw) (cg-put g :phase 'play)
    (should (cg-tm--take-deep g 0 1))
    (should (equal (cg-get g :table) '((0 (0 . 4) (0 . 5) (0 . 6)))))
    (should (equal (cg-rummy--hand g 0) '((1 . 9))))
    (should (= 1 (length (cg-get g :discard))))
    ;; a card you cannot use immediately may not be taken
    (cg-put g :hands (vector (list (cons 3 11)) nil))
    (cg-put g :discard (list (cons 1 9) (cons 2 2)))
    (should-not (cg-tm--take-deep g 0 1))))

(ert-deftest cgt-handfoot-redthree ()
  "Red threes leave the hand on deal and collect to the team pile."
  (let ((g (cg-handfoot-game)))
    (cg-put g :nplayers 4) (cg-put g :nteams 2) (cg-put g :scores (make-vector 2 0))
    (cg-hf--deal g)
    (dotimes (s 4)
      (should (= 0 (cl-count-if #'cg-hf--red-three-p (cg-rummy--hand g s)))))
    (cg-put g :redthrees (make-vector 2 nil))
    (cg-rummy--set-hand g 0 (list (cons 2 2) (cons 0 5))) ; 3 of diamonds + 6S
    (cg-put g :stock (list (cons 0 9)))
    (should (= 1 (cg-hf--collect-red-threes g 0)))
    (should (= 1 (length (aref (cg-get g :redthrees) (cg-hf--team g 0)))))
    (should-not (cl-find-if #'cg-hf--red-three-p (cg-rummy--hand g 0)))))

(ert-deftest cgt-handfoot-min-meld ()
  "Initial meld must reach the round minimum; below it is refused."
  (let ((g (cg-handfoot-game)))
    (cg-put g :nplayers 4) (cg-put g :nteams 2) (cg-put g :scores (make-vector 2 0))
    (cg-hf--deal g) (cg-put g :round 0)
    (cg-put g :down (make-vector 2 nil)) (cg-put g :books (make-vector 2 nil))
    (cg-put g :hands (vector (list (cons 0 3)(cons 1 3)(cons 2 3)) nil nil nil)) ; three 4s = 15
    (should-not (cg-hf--initial-meld g 0 (cg-rummy--hand g 0)))
    (should-not (cg-hf--down-p g 0))
    (cg-put g :hands (vector (list (cons 0 0)(cons 1 0)(cons 2 0)) nil nil nil)) ; three aces = 60
    (should (cg-hf--initial-meld g 0 (cg-rummy--hand g 0)))
    (should (cg-hf--down-p g 0))
    (should (= 1 (length (cg-hf--books g 0))))))

(ert-deftest cgt-handfoot-pickup ()
  "Picking up melds the top discard and rakes in the cards beneath it."
  (let ((g (cg-handfoot-game)))
    (cg-put g :nplayers 4) (cg-put g :nteams 2) (cg-put g :scores (make-vector 2 0))
    (cg-hf--deal g)
    (cg-put g :redthrees (make-vector 2 nil)) (cg-put g :books (make-vector 2 nil))
    (cg-put g :hands (vector (list (cons 0 8)(cons 1 8)(cons 0 4)) nil nil nil)) ; two 9s + 5S
    (cg-put g :discard (list (cons 2 8)(cons 3 5)(cons 0 6)(cons 1 7))) ; top 9D + 3 beneath
    (cg-put g :stock nil)
    (should (cg-hf--pickup-eligible g 0))
    (let ((top (cg-hf--pickup g 0)))
      (should (equal top (cons 2 8)))
      (should (= 1 (length (cg-hf--books g 0))))
      (should (cg-hf--book-valid-p (car (cg-hf--books g 0))))
      (should (= 4 (length (cg-rummy--hand g 0))))
      (should-not (cg-get g :discard)))))

;;;; Mouse click-region wiring for eights, president, and the patience boards

(ert-deftest cgt-mouse-regions ()
  "The newly clickable games attach a `cg-regions' map to their SVG."
  (let* ((g (cg-pat--deal (cg-golf-game)))
         (regs (get-text-property 0 'cg-regions (cg-pat--svg g))))
    (should regs)
    (should (cl-find '(waste . 0) (mapcar #'cdr regs) :test #'equal))
    (should (cl-find '(stock . 0) (mapcar #'cdr regs) :test #'equal))
    (should (cl-find-if (lambda (a) (eq (car a) 'slot)) (mapcar #'cdr regs))))
  (let ((g (cg-president-game)))
    (cg-put g :games 0) (cg-pres--deal g)
    (let ((regs (get-text-property 0 'cg-regions (cg-pres--svg g))))
      (should regs)
      (should (cl-find-if (lambda (a) (and (consp a) (eq (car a) 'hand)))
                          (mapcar #'cdr regs)))))
  (let ((g (cg-eights-game)))
    (cg-put g :scores (make-vector 3 0)) (cg-eights--deal g)
    (let* ((img (cg-svg-hand-image (mapcar #'cg-eights--spec (cg-eights--hand g 0))
                                   :region-tag 'hand))
           (regs (get-text-property 0 'cg-regions img)))
      (should regs)
      (should (cl-find-if (lambda (a) (and (consp a) (eq (car a) 'hand)))
                          (mapcar #'cdr regs))))))
