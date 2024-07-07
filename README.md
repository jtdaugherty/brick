![](logo/brick-final-clearbg-with-text.svg)

`brick` is a Haskell terminal user interface (TUI) programming toolkit.
To use it, you write a pure function that describes how your user
interface should be drawn based on your current application state and
you provide a state transformation function to handle events.

`brick` exposes a declarative API. Unlike most GUI toolkits which
require you to write a long and tedious sequence of widget creations
and layout setup, `brick` just requires you to describe your interface
using a set of declarative layout combinators. Event-handling is done by
pattern-matching on incoming events and updating your application state.

Under the hood, this library builds upon
[vty](http://hackage.haskell.org/package/vty), so some knowledge of Vty
will be necessary to use this library. Brick depends on
`vty-crossplatform`, so Brick should work anywhere Vty works (Unix and
Windows). Brick releases prior to 2.0 only support Unix-based systems.

Example
-------

Here's an example interface (see `programs/ReadmeDemo.hs`):

```
joinBorders $
withBorderStyle unicode $
borderWithLabel (str "Hello!") $
(center (str "Left") <+> vBorder <+> center (str "Right"))
```

Result:

```
┌─────────Hello!─────────┐
│           │            │
│           │            │
│   Left    │   Right    │
│           │            │
│           │            │
└───────────┴────────────┘
```

Featured Projects
-----------------

To get an idea of what some people have done with `brick`, check out
these projects. If you have made something and would like me to include
it, get in touch!

| Project | Description |
| ------- | ----------- |
| [`2048Haskell`](https://github.com/8Gitbrix/2048Haskell) | An implementation of the 2048 game |
| [`babel-cards`](https://github.com/srhoulam/babel-cards) | A TUI spaced-repetition memorization tool. Similar to Anki. |
| [`bhoogle`](https://github.com/andrevdm/bhoogle) | A [Hoogle](https://www.haskell.org/hoogle/) client |
| [`brewsage`](https://github.com/gerdreiss/brewsage#readme) | A TUI for Homebrew |
| [`brick-trading-journal`](https://codeberg.org/amano.kenji/brick-trading-journal) | A TUI program that calculates basic statistics from trades |
| [`Brickudoku`](https://github.com/Thecentury/brickudoku) | A hybrid of Tetris and Sudoku |
| [`cbookview`](https://github.com/mlang/chessIO) | A TUI for exploring polyglot chess opening book files |
| [`clifm`](https://github.com/pasqu4le/clifm) | A file manager |
| [`codenames-haskell`](https://github.com/VigneshN1997/codenames-haskell) | An implementation of the Codenames game |
| [`fifteen`](https://github.com/benjaminselfridge/fifteen) | An implementation of the [15 puzzle](https://en.wikipedia.org/wiki/15_puzzle) |
| [`ghcup`](https://www.haskell.org/ghcup/) | A TUI for `ghcup`, the Haskell toolchain manager |
| [`git-brunch`](https://github.com/andys8/git-brunch) | A git branch checkout utility |
| [`Giter`](https://gitlab.com/refaelsh/giter) | A UI wrapper around Git CLI inspired by [Magit](https://magit.vc/). |
| [`gotta-go-fast`](https://github.com/callum-oakley/gotta-go-fast) | A typing tutor |
| [`haradict`](https://github.com/srhoulam/haradict) | A TUI Arabic dictionary powered by [ElixirFM](https://github.com/otakar-smrz/elixir-fm) |
| [`hascard`](https://github.com/Yvee1/hascard) | A program for reviewing "flash card" notes |
| [`haskell-player`](https://github.com/potomak/haskell-player) | An `afplay` frontend |
| [`herms`](https://github.com/jackkiefer/herms) | A command-line tool for managing kitchen recipes |
| [`hic-hac-hoe`](https://github.com/blastwind/hic-hac-hoe) | Play tic tac toe in terminal! |
| [`hledger-iadd`](http://github.com/rootzlevel/hledger-iadd) | An interactive terminal UI for adding hledger journal entries |
| [`hledger-ui`](https://github.com/simonmichael/hledger) | A terminal UI for the hledger accounting system. |
| [`homodoro`](https://github.com/c0nradLC/homodoro) | A terminal application to use the pomodoro technique and keep track of daily tasks |
| [`htyper`](https://github.com/Simon-Hostettler/htyper) | A typing speed test program |
| [`hyahtzee2`](https://github.com/DamienCassou/hyahtzee2#readme) | Famous Yahtzee dice game |
| [`kpxhs`](https://github.com/akazukin5151/kpxhs) | An interactive [Keepass](https://github.com/keepassxreboot/keepassxc/) database viewer |
| [`matterhorn`](https://github.com/matterhorn-chat/matterhorn) | A client for [Mattermost](https://about.mattermost.com/) |
| [`maze`](https://github.com/benjaminselfridge/maze) | A Brick-based maze game |
| [`monalog`](https://github.com/goosedb/Monalog) | Terminal logs observer |
| [`mushu`](https://github.com/elaye/mushu) | An `MPD` client |
| [`mywork`](https://github.com/kquick/mywork) [[Hackage]](https://hackage.haskell.org/package/mywork) | A tool to keep track of the projects you are working on |
| [`pboy`](https://github.com/2mol/pboy) | A tiny PDF organizer |
| [`purebred`](https://github.com/purebred-mua/purebred) | A mail user agent |
| [`sandwich`](https://codedownio.github.io/sandwich/) | A test framework with a TUI interface |
| [`silly-joy`](https://github.com/rootmos/silly-joy) | An interpreter for Joy |
| [`solitaire`](https://github.com/ambuc/solitaire) | The card game |
| [`sudoku-tui`](https://github.com/evanrelf/sudoku-tui) | A Sudoku implementation |
| [`summoner-tui`](https://github.com/kowainik/summoner/tree/master/summoner-tui) | An interactive frontend to the Summoner tool |
| [`swarm`](https://github.com/byorgey/swarm/) | A 2D programming and resource gathering game |
| [`tart`](https://github.com/jtdaugherty/tart) | A mouse-driven ASCII art drawing program |
| [`tetris`](https://github.com/SamTay/tetris) | An implementation of the Tetris game |
| [`thock`](https://github.com/rmehri01/thock) | A modern TUI typing game featuring online racing against friends |
| [`timeloop`](https://github.com/cdupont/timeloop) | A time-travelling demonstrator |
| [`towerHanoi`](https://github.com/shajenM/projects/tree/master/towerHanoi) | Animated solutions to The Tower of Hanoi |
| [`ttyme`](https://github.com/evuez/ttyme) | A TUI for [Harvest](https://www.getharvest.com/) |
| [`ullekha`](https://github.com/ajithnn/ullekha) | An interactive terminal notes/todo app with file/redis persistence |
| [`viewprof`](https://github.com/maoe/viewprof) | A GHC profile viewer |
| [`VOIDSPACE`](https://github.com/ChrisPenner/void-space) | A space-themed typing-tutor game |
| [`wordle`](https://github.com/ivanjermakov/wordle) | An implementation of the Wordle game |
| [`wrapping-editor`](https://github.com/ta0kira/wrapping-editor) | An embeddable editor with support for Brick |
| [`youbrick`](https://github.com/florentc/youbrick) | A feed aggregator and launcher for Youtube channels |

These third-party packages also extend `brick`:

| Project | Description |
| ------- | ----------- |
| [`brick-filetree`](https://github.com/ChrisPenner/brick-filetree) [[Hackage]](http://hackage.haskell.org/package/brick-filetree) | A widget for exploring a directory tree and selecting or flagging files and directories |
| [`brick-panes`](https://github.com/kquick/brick-panes) [[Hackage]](https://hackage.haskell.org/package/brick-panes) | A Brick overlay library providing composition and isolation of screen areas for TUI apps. |

Getting Started
---------------

Check out the many demo programs to get a feel for different aspects of
the library:

```
$ cabal new-build -f demos
$ find dist-newstyle -type f -name \*-demo
```

To get started, see the [user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst).

Documentation
-------------

Documentation for `brick` comes in a variety of forms:

* [The official brick user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst)
* [Haddock documentation](https://hackage.haskell.org/package/brick)
* [Demo programs](https://github.com/jtdaugherty/brick/blob/master/programs)
* [FAQ](https://github.com/jtdaugherty/brick/blob/master/FAQ.md)

Feature Overview
----------------

`brick` comes with a bunch of batteries included:

 * Vertical and horizontal box layout widgets
 * Basic single- and multi-line text editor widgets
 * List and table widgets
 * Progress bar widget
 * Simple dialog box widget
 * Border-drawing widgets (put borders around or in between things)
 * Generic scrollable viewports and viewport scroll bars
 * General-purpose layout control combinators
 * Extensible widget-building API
 * User-customizable attribute themes
 * Type-safe, validated input form API (see the `Brick.Forms` module)
 * A filesystem browser for file and directory selection
 * Borders can be configured to automatically connect!

Brick Discussion
----------------

There are two forums for discussing brick-related things:

1. The [Discussions page](https://github.com/jtdaugherty/brick/discussions) on the github repo, and
1. The `brick-users` Google Group / e-mail list. You can subscribe
   [here](https://groups.google.com/group/brick-users).

Status
------

There are some places were I have deliberately chosen to worry about
performance later for the sake of spending more time on the design
(and to wait on performance issues to arise first). `brick` is also
something of an experimental project of mine and some aspects of the
design involve trade-offs that might not be right for your application.
Brick is not intended to be all things to all people; rather, I want it
to provide a good foundation for building complex terminal interfaces
in a declarative style to take away specific headaches of building,
modifying, and working with such interfaces, all while seeing how far we
can get with a pure function to specify the interface.

`brick` exports an extension API that makes it possible to make your own
packages and widgets. If you use that, you'll also be helping to test
whether the exported interface is usable and complete!

A note on Windows support
-------------------------

Brick supports Windows implicitly by way of Vty's Windows support.
While I don't (and can't) personally test Brick on Windows hosts,
it should be possible to use Brick on Windows. If you have any
trouble, report any issues here. If needed, we'll migrate them to the
[vty-windows](https://github.com/chhackett/vty-windows) repository if
they need to be fixed there.

Reporting bugs
--------------

Please file bug reports as GitHub issues.  For best results:

 - Include the versions of relevant software packages: your terminal
   emulator, `brick`, `ghc`, `vty`, and Vty platform packages will be
   the most important ones.

 - Clearly describe the behavior you expected ...

 - ... and include a minimal demonstration program that exhibits the
   behavior you actually observed.

Contributing
------------

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, talk to me first; let's have a
   design/vision discussion before you start coding. Create a GitHub
   issue and we can use that as the place to hash things out.
 - Please make changes consistent with the conventions I've used in the
   codebase.
 - Please adjust or provide Haddock and/or user guide documentation
   relevant to any changes you make.
 - Please ensure that commits are `-Wall` clean.
 - Please ensure that each commit makes a single, logical, isolated
   change as much as possible.
 - Please do not submit changes that your linter told you to make. I
   will probably decline them. Relatedly: please do not submit changes
   that change only style without changing functionality.
 - Please do NOT include package version changes in your patches.
   Package version changes are only done at release time when the full
   scope of a release's changes can be evaluated to determine the
   appropriate version change.
