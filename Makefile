# Makefile for card-games -- byte-compile, test, and package.
EMACS   ?= emacs
PKG      = card-games
VERSION  = 1.0.91
# Source files in dependency order (cg-core first).
EL       = cg-core.el cg-svg.el cg-render.el cg-net.el cg-bid.el cg-gaps.el cg-bid-ui.el cg-bid-net.el cg-solitaire.el cg-trick.el cg-eights.el cg-patience.el cg-president.el cg-rummy.el cg-rum500.el cg-handfoot.el cg-match.el cg-cribbage.el cg-scopa.el cg-trick-ext.el cg-spite.el cg-bridge.el cg-crapette.el card-games.el
ELC      = $(EL:.el=.elc)
PKGDESC  = $(PKG)-pkg.el
TARDIR   = $(PKG)-$(VERSION)
TAR      = $(TARDIR).tar
SRCTAR   = $(PKG)-$(VERSION)-src.tar.gz
DIST     = dist
BATCH    = $(EMACS) -Q --batch -L .
MANUAL   = doc/card-games.texi
VERTEXI  = doc/version.texi
INFO     = doc/card-games.info
IMAGES   = doc/images/klondike.png doc/images/hearts.png
MAKEINFO ?= makeinfo
TEXI2PDF ?= texi2pdf
README   = README.md
EXTRA    = README.org $(README) $(PKGDESC) $(MANUAL) $(VERTEXI) $(IMAGES)

.PHONY: all compile test clean distclean checkdoc lint check package tarball elpa release help \
        info info-emacs html pdf docclean version readme hooks

help:
	@echo "card-games $(VERSION) -- make targets:"
	@echo "  compile    byte-compile all sources"
	@echo "  test       run the ERT test suite"
	@echo "  checkdoc   run checkdoc on all sources"
	@echo "  lint       run package-lint (if installed)"
	@echo "  check      compile + checkdoc + lint + test (the full gate)"
	@echo "  version    regenerate $(VERTEXI) from VERSION"
	@echo "  readme     export README.org -> README.md (Emacs batch)"
	@echo "  hooks      install the git pre-commit hook"
	@echo "  info       build the Info manual ($(INFO)) with makeinfo"
	@echo "  info-emacs build the Info manual with Emacs alone (fallback)"
	@echo "  html       build the one-file HTML manual"
	@echo "  pdf        build the PDF manual (needs a TeX installation)"
	@echo "  package    build the installable package tarball ($(TAR))"
	@echo "  elpa       build a one-package ELPA archive in $(DIST)/"
	@echo "  release    clean + test + package + source tarball"
	@echo "  clean      remove .elc and build artifacts"

all: compile

# Compile in order so each file sees its compiled dependencies.
compile:
	$(BATCH) -f batch-byte-compile $(EL)

test:
	$(BATCH) -L test -l test/$(PKG)-tests.el -f ert-run-tests-batch-and-exit

# One gate a contributor (or CI) can run: the same quality tools MELPA
# asks for -- byte-compile, checkdoc, package-lint -- plus the ERT suite.
check: compile checkdoc lint test
	@echo "card-games $(VERSION): compile + checkdoc + lint + tests all green"

checkdoc:
	$(BATCH) --eval "(progn (dolist (f '($(EL))) (checkdoc-file (symbol-name f))))"

lint:
	-$(BATCH) --eval "(progn (require 'package) (package-initialize) \
	  (unless (require 'package-lint nil t) (error \"package-lint not installed\")) \
	  (setq package-lint-main-file \"$(PKG).el\") \
	  (dolist (f '($(EL))) (with-temp-buffer (insert-file-contents f) \
	    (emacs-lisp-mode) (package-lint-buffer))))"

# Installable multi-file package: card-games-VERSION.tar with a top
# directory of the same name (package.el / package-install-file format).
package: tarball
# Uses GNU tar's --transform to add the top directory without a temp copy.
tarball: $(EL) $(EXTRA)
	tar --transform 's,^,$(TARDIR)/,' -cf $(TAR) $(EL) $(EXTRA)
	@echo "Built $(TAR)"

# A minimal ELPA archive (archive-contents + tar) under dist/.
# Testers can:  (add-to-list 'package-archives '("cg" . "/path/to/dist/"))
elpa: tarball
	rm -rf $(DIST)
	mkdir -p $(DIST)
	$(BATCH) --eval "(progn (require 'package-x) \
	  (let ((package-archive-upload-base (expand-file-name \"$(DIST)\"))) \
	    (package-upload-file \"$(TAR)\")))"
	@echo "ELPA archive in $(DIST)/"

# Source snapshot for a GitHub release.  Archive an explicit file list
# (not ".") so the growing output tarball and editor lock files are never
# read mid-write -- which is what caused "tar: .: file changed as we read it".
SRCFILES = $(EL) $(EXTRA) build.el hooks Makefile .gitignore test
release: distclean version readme check tarball
	rm -f $(SRCTAR)
	tar --transform 's,^,$(TARDIR)/,' \
	    --exclude='*.elc' --exclude='*.tar' --exclude='*.tar.gz' \
	    --exclude='.#*' --exclude='$(DIST)' --exclude='.git' \
	    -czf $(SRCTAR) $(SRCFILES)
	@echo "Built $(SRCTAR) and $(TAR) for release $(VERSION)"

# Documentation.  The Texinfo source lives in doc/; the Info manual is
# built from it with makeinfo.  doc/version.texi carries the version and
# is committed; `make version' regenerates it from VERSION (run at release).
version:
	@printf '@set VERSION %s\n@set UPDATED %s\n@set YEAR %s\n' \
	  "$(VERSION)" "$$(date '+%-d %B %Y')" "$$(date +%Y)" > $(VERTEXI)
	@echo "Wrote $(VERTEXI) (VERSION $(VERSION))"

# README: Org is the source; GitHub and MELPA render Markdown better than
# Org, so we export README.org -> README.md with Emacs batch (ox-md).  The
# git pre-commit hook (make hooks) keeps README.md in step automatically.
readme: $(README)
$(README): README.org build.el
	$(BATCH) -l build.el

# Install the pre-commit hook into this checkout's .git/hooks.
hooks:
	@if [ -d .git ]; then \
	  cp hooks/pre-commit .git/hooks/pre-commit && \
	  chmod +x .git/hooks/pre-commit && \
	  echo "Installed .git/hooks/pre-commit"; \
	else \
	  echo "No .git directory here; skipping hook install."; \
	fi

info: $(INFO)
$(INFO): $(MANUAL) $(VERTEXI)
	$(MAKEINFO) -o $@ $(MANUAL)

# Fallback: build the Info manual with Emacs alone (no makeinfo needed).
# Handy on Windows/MSYS2 where Emacs is present but texinfo may not be.
# The native formatter is lower fidelity than makeinfo, so `make info'
# stays the default; this is only for a makeinfo-less environment.
info-emacs: $(MANUAL) $(VERTEXI)
	cd doc && $(EMACS) -Q --batch --eval "(require 'texinfmt)" \
	  -f batch-texinfo-format card-games.texi

html: $(MANUAL) $(VERTEXI)
	$(MAKEINFO) --html --no-split -o doc/card-games.html $(MANUAL)

pdf: $(MANUAL) $(VERTEXI)
	cd doc && $(TEXI2PDF) -q card-games.texi

docclean:
	rm -f $(INFO) doc/card-games.html doc/card-games.pdf \
	      doc/card-games.aux doc/card-games.cp doc/card-games.cps \
	      doc/card-games.fn doc/card-games.ky doc/card-games.log \
	      doc/card-games.pg doc/card-games.toc doc/card-games.tp \
	      doc/card-games.vr doc/card-games.fns

clean:
	rm -f $(ELC)

distclean: clean docclean
	rm -rf $(DIST) $(TARDIR) $(TAR) $(SRCTAR)
