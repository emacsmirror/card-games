;;; build.el --- Batch Org -> Markdown export for card-games -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Corwin Brust
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A very small Emacs-batch exporter.  It renders the project's Org
;; sources to Markdown siblings (foo.org -> foo.md) using the built-in
;; `ox-md' backend, so GitHub and MELPA -- which render Markdown more
;; faithfully than Org -- can display them.
;;
;; Run it by hand, from the Makefile, or from the git pre-commit hook:
;;
;;   emacs -Q --batch -l build.el
;;
;; By default it exports the files named in `build-org-files' (README.org).
;; Set the CARD_GAMES_ORG environment variable to a space-separated list
;; to override that, e.g. to export more documents.  It never touches
;; known-games.org (an internal research list) unless you ask for it.

;;; Code:

(require 'org)
(require 'ox-md)

;; Keep batch mode from blocking on prompts.
(setq org-confirm-babel-evaluate nil
      org-export-show-temporary-export-buffer nil
      ;; A stray or not-yet-committed image link must never abort the run.
      org-export-with-broken-links 'mark
      make-backup-files nil)

(defvar build-org-files '("README.org")
  "Default list of Org files to export to Markdown.
Overridden by the CARD_GAMES_ORG environment variable when set.")

(defvar build-inhibit-run nil
  "When non-nil, loading build.el defines helpers but does not export.
ERT or an interactive session can bind this to exercise the helpers.")

(defun build--targets ()
  "Return the list of Org files to export.
Honours the CARD_GAMES_ORG environment variable; falls back to
`build-org-files'."
  (let ((env (getenv "CARD_GAMES_ORG")))
    (if (and env (not (string-empty-p (string-trim env))))
        (split-string (string-trim env) "[ \t\n]+" t)
      build-org-files)))

(defun build--export-one (orgfile)
  "Export ORGFILE to a Markdown sibling.
Log the outcome; never signal, so one bad file cannot abort the run."
  (cond
   ((not (file-readable-p orgfile))
    (message "build: SKIP %s (not readable)" orgfile))
   (t
    (message "build: exporting %s -> markdown" orgfile)
    (with-current-buffer (find-file-noselect orgfile)
      (condition-case err
          (let ((out (org-md-export-to-markdown)))
            (message "build:   wrote %s" out))
        (error
         (message "build:   ERROR exporting %s: %s"
                  orgfile (error-message-string err))))))))

(defun build--run ()
  "Export every file in `build--targets' to Markdown."
  (dolist (orgfile (build--targets))
    (build--export-one orgfile))
  (message "build: done"))

(unless (bound-and-true-p build-inhibit-run)
  (build--run))

(provide 'build)
;;; build.el ends here
