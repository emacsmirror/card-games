# LIFT · card-game — CANDIDATE

Session 2026-08-03. Everything here is CANDIDATE until Corwin says otherwise.
Scope: only the repo (`card-game.el`) was mounted — not the `card-game` project
wrapper the brief named, not `dungeon`, not `c4/`. The sandbox had no system
Emacs (apt blocked, non-root), but the recorded micromamba recipe delivered a
userspace Emacs 30 — so the oracle below was RUN this session, not quoted.

## 1. What it IS

**card-games** v1.0.91 ("pretest2: playtest"): a GNU ELPA-style Emacs package
of ~30 card games over one small EIEIO engine (`cg-core.el`, 324 lines).
Abstract `cg-game` (name + plist `env` + current renderer) with `cg-render` /
`cg-won-p` generics; family base classes (`cg-solitaire-game`, `cg-trick-game`,
`cg-rummy-game`→`cg-tablemeld-game`, `cg-fish-game`, `cg-patience-game`,
`cg-gaps-game`) carry shared rules; each game is a small subclass. Display is a
separate seam: renderer "skins" (`text`, `svg`, `svg-fill`) registered by name,
each mapping clicks back to game actions via rectangle→action region lists.
Cards are `(SUIT . RANK)` conses, suits indexed 0=♠ 1=♣ 2=♦ 3=♥, rank scales
per-game, `nil` = empty slot. Ships an Info manual, a Makefile exposing the
full lifecycle, network play for 500 (`cg-bid-host`/`cg-bid-join`), and a
150-test ERT suite. Maintenance-stage; published at code.bru.st/corwin/card-game.

## 2. The oracle (the ONE thing that already fails loudly)

**`make test` — test/card-games-tests.el, 150 `ert-deftest`s, batch exit
nonzero on any failure — and `release:` refuses to package without it**
(`release: distclean version readme test tarball`). The suite header documents
the exact no-make invocation:

    emacs -Q --batch -L . -L test -l test/card-games-tests.el \
          -f ert-run-tests-batch-and-exit

**RUN this session** (userspace Emacs 30, tree exported from git HEAD objects
into /tmp — no mount-desync exposure): **150/150 passed, 0 unexpected, 15.4s,
exit 0** (2026-08-03 20:07:52-0500). Secondary gate also run: batch
byte-compile of all 24 EL files — **clean, zero warnings**. Remaining gate
family not run here: `checkdoc`, `lint`.

## 3. What C4 takes FROM it

Each claim names the tool that fails it.

- **The renderer-seam pattern.** One game model, N display treatments,
  registered by name, never subclassing the game per treatment; clicks map
  back through declared `(RECT . ACTION)` regions; `cg-color` degrades to a
  drawable fallback in batch/terminal. This is the house answer to "one truth,
  two representations" for any C4 surface with console + graphical faces.
  *Fails-it:* `cgt-render-registry` and kin in the suite.
- **Taxonomy-by-family engine reuse.** Engine → family base → thin game
  subclass; 30 games prove the substrate. The C4RP shared-substrate story
  working in miniature. *Fails-it:* the per-family ERT groups.
- **The oracle discipline itself.** One test file, one batch line, wired into
  `make test` and gating `release`. rewire-order says go first with the
  project that has an oracle — this one HAS one. *Fails-it:* itself.
- **Org→MD canonicity, implemented.** README.org is source; `build.el` (ox-md,
  never-signal, `CARD_GAMES_ORG` override) exports; a pre-commit hook keeps
  README.md in step. ORG-NATIVE stance made mechanical. *Fails-it (host):*
  `make readme && git diff --exit-code README.md` — empty diff = in step.
  Not proven here (needs Emacs); the md-newer-than-org mtimes are consistent
  but mtime is never a tiebreaker.
- **Version fan-out.** Single `VERSION` in the Makefile; `make version`
  regenerates `doc/version.texi`; bump patch z for every releasable set.
  *Fails-it (ran, passed):* `grep '^;; Version:' *.el | sort -u` — one value,
  1.0.91, across 24 headers + Makefile + pkg.el + version.texi.
- **Scars kept as comments.** The release tar archives an explicit file list
  because ".": "the growing output tarball … is never read mid-write — which
  is what caused 'tar: .: file changed as we read it'." The failure mode rides
  with the fix. Liftable practice, not just this instance.

## 4. Vocabulary (for the `dungeon:cards` negotiation)

Dungeon is not mounted; I cannot see its side. This is card-game's offer,
one-sided, plus the C4 terms this repo does not use.

**card-game has, C4/dungeon must match or map:** card `(SUIT . RANK)`;
**suit index order 0♠ 1♣ 2♦ 3♥** (black-black-red-red — NOT bridge order
♣♦♥♠; first and likeliest silent-drift item, negotiate before any shared
register); per-game rank scales (no global ace-high/low); `joker`; `nil` =
empty slot; `sister-suit` (same-colour partner); renderer/"skin"/"treatment";
`env` (per-game plist); region/action click maps; pile nouns — foundation,
tableau, stock, waste, reserve, cell, gap, hand, trick, book, bags, meld,
house (Russian Bank), crib/peg, capture. Shuffle is Fisher-Yates over Emacs
`random` — seedability for replay/net determinism is a negotiation point,
not currently a promise.

**C4 has, this repo does not:** run-level, oracle (the word; it has the
thing), vocab/register, lift, candidate/canon, run-card, instrument. The repo
predates the register; nothing in it will collide, but nothing anchors it
either.

## 5. Run-level evidence (guess was ~4)

The Makefile exposes the lifecycle as named signals: `compile`, `test`,
`checkdoc`, `lint`, `package`, `elpa`, `release`, `info`, `hooks` — with
self-test gating release, install via standard package.el, and enablement via
`M-x card-game` plus a shipped Info manual. From this sandbox I verified only
static signals. The run-level scale itself lives in `c4/`, deliberately not
mounted — I hold no instrument for the pin; evidence reads at-or-above the
guess, and the pin stays with Corwin.

## 6. Session receipts

- No system Emacs (uid 1174, apt blocked); micromamba userspace Emacs 30
  worked (recipe from session memory). Oracle run 150/150; compile clean.
  Mount-vs-HEAD spot-check agreed on 4 files before the run (no desync seen).
- Memory drift found and corrected in session notes: tree is at 1.0.91 /
  30 games / 150 tests (notes said 1.0.60 / 5 games / 88).
- org-c4 brief arrived in the same paste; org-c4 is not mounted — not
  executable here, untouched.
- Git reads were object-reads only (`git log`); no index-touching commands
  on the mount. Tree tarballs and `*~` files are `.gitignore`d by design.
- This file is untracked on purpose: CANDIDATE, his pen to bless, never mine.
