# Makefile for card-games -- byte-compile, test, and package.
EMACS   ?= emacs
PKG      = card-games
VERSION  = 1.0.50
# Source files in dependency order (cg-core first).
EL       = cg-core.el cg-svg.el cg-render.el cg-net.el cg-bid.el cg-gaps.el cg-bid-ui.el cg-bid-net.el card-games.el
ELC      = $(EL:.el=.elc)
PKGDESC  = $(PKG)-pkg.el
TARDIR   = $(PKG)-$(VERSION)
TAR      = $(TARDIR).tar
SRCTAR   = $(PKG)-$(VERSION)-src.tar.gz
DIST     = dist
BATCH    = $(EMACS) -Q --batch -L .
EXTRA    = README.org $(PKGDESC)

.PHONY: all compile test clean distclean checkdoc lint package tarball elpa release help

help:
	@echo "card-games $(VERSION) -- make targets:"
	@echo "  compile    byte-compile all sources"
	@echo "  test       run the ERT test suite"
	@echo "  checkdoc   run checkdoc on all sources"
	@echo "  lint       run package-lint (if installed)"
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

checkdoc:
	$(BATCH) --eval "(progn (dolist (f '($(EL))) (checkdoc-file f)))"

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
SRCFILES = $(EL) $(EXTRA) Makefile .gitignore test
release: distclean test tarball
	rm -f $(SRCTAR)
	tar --transform 's,^,$(TARDIR)/,' \
	    --exclude='*.elc' --exclude='*.tar' --exclude='*.tar.gz' \
	    --exclude='.#*' --exclude='$(DIST)' --exclude='.git' \
	    -czf $(SRCTAR) $(SRCFILES)
	@echo "Built $(SRCTAR) and $(TAR) for release $(VERSION)"

clean:
	rm -f $(ELC)

distclean: clean
	rm -rf $(DIST) $(TARDIR) $(TAR) $(SRCTAR)
