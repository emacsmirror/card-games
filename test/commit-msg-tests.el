;;; commit-msg-tests.el --- ERT tests for hooks/commit-msg  -*- lexical-binding: t; -*-

;; Run with:  make test-hooks   (or)
;;   emacs -Q --batch -l test/commit-msg-tests.el -f ert-run-tests-batch-and-exit
;;
;; The hook is POSIX sh; these tests drive it from Emacs Lisp, so the
;; check and its tests are written in different languages.  Each test
;; feeds one message file to the hook and reads only its exit status.
;; The table holds messages that must pass and messages that must be
;; refused, so a hook that accepts everything, or refuses everything,
;; fails here.

;;; Code:

(require 'ert)

(defconst cmt--hook
  (expand-file-name "../hooks/commit-msg"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The hook under test.")

(defun cmt--status (message)
  "Return the exit status of the hook run on MESSAGE."
  (let ((file (make-temp-file "commit-msg-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file file (insert message)))
          (call-process "sh" nil nil nil cmt--hook file))
      (delete-file file))))

(defconst cmt--subject "Fix a thing\n\nBody text.\n\n")

(defconst cmt--accept
  `(("no trailer at all"        . "Fix a thing\n\nBody text.\n")
    ("persona form, semicolon"  . ,(concat cmt--subject "Assisted-by: Mentat (for Corwin; claude-opus-5-5)\n"))
    ("persona form, comma"      . ,(concat cmt--subject "Assisted-by: Mentat (for Corwin, claude-opus-5-5)\n"))
    ("key case varies"          . ,(concat cmt--subject "Assisted-By: Mentat (for Corwin; claude-opus-5-5)\n"))
    ("CRLF line endings"        . "Fix a thing\r\n\r\nAssisted-by: Mentat (for Corwin; claude-opus-5-5)\r\n")
    ("no final newline"         . ,(concat cmt--subject "Assisted-by: Mentat (for Corwin; claude-opus-5-5)"))
    ("comment line ignored"     . ,(concat cmt--subject "# Claude-Session: https://example.invalid/x\n"))
    ("after the scissors line"  . ,(concat cmt--subject
                                           "# ------------------------ >8 ------------------------\n"
                                           "+Claude-Session: https://example.invalid/x\n"
                                           "+Co-Authored-By: Claude <noreply@anthropic.com>\n")))
  "Messages the hook must accept, by name.")

(defconst cmt--refuse
  `(("session link"             . ,(concat cmt--subject "Claude-Session: https://claude.ai/code/session_x\n"))
    ("session link, lower case" . ,(concat cmt--subject "claude-session: https://claude.ai/code/session_x\n"))
    ("co-author, Claude"        . ,(concat cmt--subject "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>\n"))
    ("co-author, lower case"    . ,(concat cmt--subject "co-authored-by: Someone <bot@anthropic.com>\n"))
    ("older Assisted-by form"   . ,(concat cmt--subject "Assisted-by: Claude:claude-opus-4-8\n"))
    ("persona form, no model"   . ,(concat cmt--subject "Assisted-by: Mentat (for Corwin)\n"))
    ("persona form, no person"  . ,(concat cmt--subject "Assisted-by: Mentat (claude-opus-5-5)\n"))
    ("good line then a bad one" . ,(concat cmt--subject
                                           "Assisted-by: Mentat (for Corwin; claude-opus-5-5)\n"
                                           "Claude-Session: https://claude.ai/code/session_x\n")))
  "Messages the hook must refuse, by name.")

(ert-deftest cmt-accepts ()
  "Every message in `cmt--accept' passes the hook."
  (skip-unless (executable-find "sh"))
  (dolist (case cmt--accept)
    (should (equal (cons (car case) 0)
                   (cons (car case) (cmt--status (cdr case)))))))

(ert-deftest cmt-refuses ()
  "Every message in `cmt--refuse' is refused by the hook."
  (skip-unless (executable-find "sh"))
  (dolist (case cmt--refuse)
    (should (equal (cons (car case) 1)
                   (cons (car case) (cmt--status (cdr case)))))))

(provide 'commit-msg-tests)
;;; commit-msg-tests.el ends here
