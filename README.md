Card games for Emacs: about thirty of them, from Klondike and FreeCell
to Hearts, 500, Gin, Cribbage, and two-player Russian Bank against the
computer.  Every game plays with the keyboard everywhere and with the
mouse on a graphical display.

![img](doc/images/klondike.png)

On a graphical display the cards are drawn as SVG; in a terminal they
fall back to UNICODE glyphs (customize `card-game-symbols`).  You can
switch how every game is drawn from the menu &ndash; `text` (UNICODE),
`svg` (drawn cards), or `full` (a full-window SVG table) &ndash; and dial the
computer opponents between `easy`, `normal`, and `hard`.

A full Info manual ships with the package: after installing, `C-h i` and
choose **Card Games**, or `M-x info RET (card-games) RET`.


# Games

To open the game menu type `M-x card-game`, or start a game directly
with its command.  From the menu you can also switch the card treatment
(text / SVG / full-window) and the AI difficulty.


## Trick-taking

-   `cg-bid` &ndash; 500 (Bid).  Win the auction, name the trump suit, then take
    tricks with your partner to reach 500 points before the opposing pair.
    Also playable live over the network (`M-x cg-bid-host` / `cg-bid-join`).
-   `cg-hearts` &ndash; Hearts.  Avoid taking hearts and the Queen of Spades, or
    take them all to "shoot the moon"; lowest score loses.
-   `cg-spades` &ndash; Spades.  Partnership bidding to 500; spades are always
    trump.  Make your side's combined bid, mind the bags, dare a nil.
-   `cg-whist` &ndash; Whist.  Trump is the turned card, there is no bidding;
    score one point for each trick past the book of six.
-   `cg-ohhell` &ndash; Oh Hell.  The hand shrinks each round; bid the exact
    number of tricks you will take, no more and no fewer.


## Solitaire

-   `cg-montana` &ndash; Montana (also called Gaps).  Each row is anchored by a
    Two and built upward in one suit, 2 through King; slide cards into the
    gaps until all four rows are sorted.
-   `cg-gaps` &ndash; an alias for `cg-montana`.
-   `cg-hells-half-acre` &ndash; the build-down variant: each row is anchored by
    a King and built downward, King through 2.
-   `cg-klondike` &ndash; Klondike, the classic "Solitaire": build the four
    foundations up by suit from the Ace.
-   `cg-freecell` &ndash; FreeCell: every card in view, four free cells, a game
    of nearly pure skill.
-   `cg-spider` &ndash; Spider (two decks): build down regardless of suit, but
    only same-suit runs move; clear eight King-to-Ace runs.
-   `cg-yukon` &ndash; Yukon: Klondike's layout dealt mostly face up, with any
    buried group movable and no stock.
-   `cg-canfield` &ndash; Canfield: a 13-card reserve and a foundation base rank
    set by the deal; foundations wrap King to Ace.
-   `cg-forty-thieves` &ndash; Forty Thieves: two decks, ten columns, eight
    foundations, build down by suit, and no second pass through the stock.
-   `cg-scorpion` &ndash; Scorpion: build down by suit and free any buried group
    to assemble four King-to-Ace runs.
-   `cg-golf` &ndash; Golf: clear the layout by playing exposed cards one rank
    above or below the waste top.
-   `cg-tripeaks` &ndash; TriPeaks: the same, on three overlapping peaks, with
    Ace-King wrapping for long chains.
-   `cg-pyramid` &ndash; Pyramid: remove pairs of exposed cards whose ranks sum
    to thirteen; Kings go alone.


## Shedding and climbing

-   `cg-eights` &ndash; Crazy Eights.  Match the suit or rank of the discard;
    eights are wild and let you name the next suit.
-   `cg-president` &ndash; President (Scum).  Climb: play one to four of a rank,
    beat it or pass; first out rules, last out scrubs, and the roles trade
    cards on the next deal.


## Rummy

-   `cg-gin` &ndash; Gin Rummy.  A two-handed duel: draw or take the discard,
    build sets and runs, and knock once your deadwood is ten or less, or go
    gin with none; your opponent then lays off and may undercut you.  First
    to 100 wins.
-   `cg-rummy-basic` &ndash; Rummy.  Meld sets and runs onto the table and lay
    cards off onto them; empty your hand to go out and score the cards left
    in the other hands.
-   `cg-rum500` &ndash; Rummy 500.  As above, but you score the cards you lay
    down and lose the cards left in your hand; first past 500 wins.  Take a
    buried discard card with `T`: you take it and every card above it, and
    meld the chosen card at once.
-   `cg-handfoot` &ndash; Hand & Foot.  A partnership Canasta cousin: play a hand
    and then a foot, build books of a rank with Twos and Jokers wild, and go
    out once your side has completed two of them.  Each round opens with a
    rising go-down minimum (50, 90, 120, 150); red threes are bonus cards;
    and you can pick up the discard pile (`p`) by melding its top card with
    two matching naturals.


## Matching

-   `cg-go-fish` &ndash; Go Fish.  Ask another player for a rank you hold;
    collect all four to lay down a book, and make the most books.
-   `cg-old-maid` &ndash; Old Maid.  One Queen is set aside; discard pairs and
    draw blind from your neighbour, and do not be left with the odd Queen.


## Pegging

-   `cg-cribbage` &ndash; Cribbage.  Lay two cards to the crib, cut a starter,
    peg toward 31, then count fifteens, pairs, runs, flushes, and his nobs.
    Two-handed to 121.


## Capturing

-   `cg-scopa` &ndash; Scopa.  A 40-card deck; capture table cards by value and
    sweep the board for a scopa.  Score cards, coins, the sette bello, and
    primiera to 11.
-   `cg-casino` &ndash; Casino.  The full deck; capture by pairs and sums and
    score cards, spades, the casinos, and aces to 21.


## More trick-taking

-   `cg-euchre` &ndash; Euchre.  A 24-card deck with the two bowers; order up or
    call trump and take three of five tricks.  Partnership to 10.
-   `cg-pitch` &ndash; Auction Pitch.  Bid for the pitch; your first lead sets
    trump.  Score High, Low, Jack, and Game; first to 7.
-   `cg-briscola` &ndash; Briscola.  A fixed trump turned from the deal and no
    obligation to follow suit; capture the Aces and Threes.  Partnership to
    61 of the 120 points.


## Climbing patience

-   `cg-spite` &ndash; Spite & Malice.  Race the computer to empty your goal
    pile onto shared centre piles that build Ace to Queen; Kings are wild.


## Bridge

-   `cg-bridge` &ndash; Contract Bridge.  A full auction (bids, pass, double,
    redouble), play with the dummy exposed, and classic rubber scoring with
    vulnerability.  You are South; when you declare you play the dummy too.
    The bidding AI is a small natural system, sensible but no expert.


## Two-player patience

-   `cg-russian-bank` / `cg-crapette` &ndash; Russian Bank (Crapette).  A race
    against the computer: build the eight shared foundations up by suit,
    build the shared houses down in alternating colour, and empty your
    reserve to win.  Load your cards onto your opponent's piles when the
    rank and suit line up, mind the "stop" rule that ends your turn if you
    skip a foundation play, and sequence house-to-house moves within the
    free houses (`[` / `]` choose how many cards of a run to drop on an
    empty house).  The opponent plays all three difficulty levels.


# TODO 

-   [X] make the suit symbols customizable (`cg-symbols`) and obey them
-   [X] a Texinfo manual
-   [ ] finish `checkdoc` docstrings across the per-game files
    (the shared engine files are clean; `make compile` is warning-free)
-   [ ] renderer "skins": let games subclass the display components (text,
    SVG, full-window SVG)
-   [X] a manual card-size control for the full-window SVG UI
-   [ ] more games


# Install


## From the package tarball

    make package           # builds card-games-1.0.90.tar

Then in Emacs: `M-x package-install-file RET card-games-1.0.90.tar`.


## With `use-package`

Once the package is on your `load-path` (installed from the tarball or an
ELPA archive), the whole collection loads from the single `card-games`
feature:

    (use-package card-games
      :commands (card-game cg-klondike cg-bid cg-hearts cg-gin cg-crapette))

Then `M-x card-game` for the menu.


## From a local ELPA archive

    make elpa              # builds dist/ (archive-contents + tar)

    (add-to-list 'package-archives '("cg" . "/path/to/dist/"))
    (package-refresh-contents)
    (package-install 'card-games)


## Manually

Put the `cg-*.el` and `card-games.el` files on your `load-path` and
`(require 'card-games)`.


# Manual

A complete Info manual is included.  After installing, `C-h i` and pick
**Card Games**, or `M-x info RET (card-games) RET`.  To build it from a
source checkout:

    make info              # builds doc/card-games.info with makeinfo

The manual text is licensed CC-BY-4.0; the code is GPL-3.0-or-later.


# Playing

Every game works with the keyboard everywhere and with the mouse on a
graphical display.

-   500: `b` bid, `p` pass, arrows + `RET` to play (or click a card),
    `n` next hand / new game, `?` help.
-   Gaps: arrows to move (or `hjkl` when `cg-keys` is `classic`), `RET` to
    fill a gap (or click it), `r` redeal, `u` undo, `n` new, `?` help.
-   Klondike / FreeCell / Spider / Yukon: arrows move between piles, `RET`
    picks up a movable run and drops it, `f` sends a card to a foundation,
    `a` auto-plays everything it can, `u` undo, `n` new, `?` help.  On the
    stock pile, `RET` deals or recycles.
-   Hearts / Spades: arrows choose a card, `RET` plays it (in Hearts, `RET`
    marks a card to pass and `p` sends the three), `n` new match, `?` help.
-   Crazy Eights: arrows choose, `RET` plays, `d` draws, `x` passes, `n`
    new deal, `?` help.

On a graphical display, `v` toggles the full-window SVG table.  A
**Card size** slider (and the `+` / `-` / `0` keys) resizes the cards, and
in 500 the `? Help / Rules` button explains play.  Every game accepts the
mouse: click cards, board slots, buttons, and the slider.

![img](doc/images/hearts.png)


# Testing

This is a 1.0.90 pre-test snapshot.  To try it:

1.  `make compile && make test` &ndash; should be warning-free and all green.
2.  `M-x card-game` opens the menu, or jump straight in, e.g.
    `M-x cg-klondike`, `M-x cg-bid` (500), `M-x cg-gin`, `M-x cg-handfoot`.
3.  On a graphical display, press `v` in 500 for the full-window SVG
    table and play entirely with the mouse: click a bid, click five kitty
    cards and the **Discard** button, click cards to play, move the **Card
    size** slider, and open `? Help / Rules`.

Feedback most wanted: anything a mouse-only player who is new to Emacs
finds confusing or unreachable, rules bugs, and rendering glitches.


# Customization

`M-x customize-group RET cg-svg` and `RET card-games`:

-   `cg-ai-level` &ndash; how hard the computer plays: `easy`, `normal`, or
    `hard` (also on the `M-x card-game` menu, or `M-x
      card-games-set-ai-level`).
-   `card-games-treatment` &ndash; how the games are drawn: `text`, `svg`, or
    `full` (also on the menu, or `M-x card-games-set-treatment`).
-   `cg-svg-theme-colors` &ndash; derive the highlight ring and card backs
    from your theme (on by default).
-   `cg-svg-highlight-color` &ndash; the cursor/selection ring (gold by default).
-   `cg-bid-felt-color` &ndash; the 500 table felt.
-   `cg-svg-card-width`, `cg-svg-card-height`, `cg-svg-card-shadow`,
    `cg-svg-font-family` &ndash; card appearance.
-   `cg-svg-card-back` &ndash; the card-back design: `dots`, `rings`, `solid`,
    `lattice`, `waves`, `diamond`, an Emacs-logo back (`emacs`,
    `emacs-classic`, `gnu`, `splash`), or `random` (the default &ndash; picks
    one for the session; `M-x cg-svg-shuffle-card-back` rolls a new one).
-   `cg-svg-emacs-logo` &ndash; the emblem on the full-window table:
    `modern` (default), `classic`, `gnu`, `splash`, `drawn`, or `none`.
-   `cg-symbols` &ndash; the Unicode suit glyphs (and the joker) drawn on cards.
-   `cg-svg-four-color` &ndash; draw a four-colour deck (clubs green, diamonds
    blue-purple).
-   `cg-keys` &ndash; `emacs` (default) or `classic` (adds vi-style `hjkl` and
    `SPC`).
-   `cg-bid-animate`, `cg-bid-ai-delay`, `cg-bid-trick-pause` &ndash; pace the
    500 AI so play is watchable and completed tricks linger.
-   `M-x card-games-set-theme` &ndash; apply a preset (classic, dark, contrast).


# Development

    make compile     # byte-compile (should be warning-free)
    make test        # run the ERT suite
    make checkdoc    # documentation lint
    make release     # clean + test + package + source tarball


# License

GPL-3.0-or-later.  See the file headers; add a COPYING file with the
full GPLv3 text for distribution.

