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
