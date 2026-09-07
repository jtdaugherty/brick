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
| [`bollama`](https://github.com/andrevdm/bollama) | A simple [Ollama](https://ollama.com/) TUI |
| [`brewsage`](https://github.com/gerdreiss/brewsage#readme) | A TUI for Homebrew |
| [`brick-trading-journal`](https://codeberg.org/amano.kenji/brick-trading-journal) | A TUI program that calculates basic statistics from trades |
| [`Brickudoku`](https://github.com/Thecentury/brickudoku) | A hybrid of Tetris and Sudoku |
| [`cbookview`](https://github.com/mlang/cbookview) | A TUI for exploring polyglot chess opening book files |
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
| [`hskanban`](https://github.com/vincentaxhe/hskanban) | A Kanban organizer |
| [`htyper`](https://github.com/Simon-Hostettler/htyper) | A typing speed test program |
| [`hyahtzee2`](https://github.com/DamienCassou/hyahtzee2#readme) | Famous Yahtzee dice game |
| [`kpxhs`](https://github.com/akazukin5151/kpxhs) | An interactive [Keepass](https://github.com/keepassxreboot/keepassxc/) database viewer |
| [`matterhorn`](https://github.com/matterhorn-chat/matterhorn) | A client for [Mattermost](https://about.mattermost.com/) |
| [`maze`](https://github.com/benjaminselfridge/maze) | A Brick-based maze game |
| [`monad-torrent`](https://github.com/davorluc/monad-torrent) | A simple and minimal torrent client |
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
| [`tick-tock-tui`](https://github.com/sectore/tick-tock-tui) | A stylish TUI app to handle Bitcoin data provided by [Mempool REST API](https://mempool.space/docs/api/rest) incl. blocks, fees and price converter. |
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

These additional packages also extend `brick`:

| Project | Description | Hackage |
| ------- | ----------- | ------- |
| [`brick-filetree`](https://github.com/ChrisPenner/brick-filetree) | A widget for exploring a directory tree and selecting or flagging files and directories | [Hackage](https://hackage.haskell.org/package/brick-filetree) |
| [`brick-panes`](https://github.com/kquick/brick-panes) | A Brick overlay library providing composition and isolation of screen areas for TUI apps. | [Hackage](https://hackage.haskell.org/package/brick-panes) |
| [`brick-calendar`](https://github.com/ldgrp/brick-calendar) | A library providing a calendar widget for Brick-based applications. | [Hackage](https://hackage.haskell.org/package/brick-calendar) |
| [`brick-skylighting`](https://github.com/jtdaugherty/brick-skylighting) | A library providing integration support for [Skylighting](https://hackage.haskell.org/package/skylighting)-based syntax highlighting. | [Hackage](https://hackage.haskell.org/package/brick-skylighting) |

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
 * Animation support
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

 - Patches written completely or partially by AI are unlikely to be
   accepted. Please disclose any AI use.
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


## 🌐 Web Resources & Interactive Index
- [FARM VS ZOMBIES](https://eduquestspt.pages.dev/farm-vs-zombies.html)
- [BURGER HERE](https://quizzesarena.onrender.com/burger-here.html)
- [CITYQUEST](https://ieduquests.web.app/cityquest.html)
- [CAR DEALER IDLE](https://ieduquests.web.app/car-dealer-idle.html)
- [CATEGORY QUIZ40](https://eduquestspt.pages.dev/category-quiz40.html)
- [KNIT BEARS](https://ieduquests.web.app/knit-bears.html)
- [YOUTUBER MCRAFT 2PLAYER](https://eduquests.pages.dev/youtuber-mcraft-2player.html)
- [CATEGORY BATTLE524](https://eduquestkr.pages.dev/category-battle524.html)
- [SEAT JAM 3D](https://eduquests.pages.dev/seat-jam-3d.html)
- [REAL MOTORBIKE SIMULATOR RACE 3D](https://eduquestsfr.pages.dev/real-motorbike-simulator-race-3d.html)
- [CATEGORY RUNNING](https://eduquests.pages.dev/category-running.html)
- [CRUSH IT ALL](https://eduquestspt.pages.dev/crush-it-all.html)
- [MEGA ESCAPE CAR PARKING PUZZLE](https://eduquestspt.pages.dev/mega-escape-car-parking-puzzle.html)
- [PUZZLE BLOCKS CLASSIC](https://eduquests.pages.dev/puzzle-blocks-classic.html)
- [FRUIT CONNECT 3](https://ieduquests.web.app/fruit-connect-3.html)
- [CATEGORY SIMULATION](https://eduquestsfr.pages.dev/category-simulation.html)
- [GALAXY CONTROL 3D STRATEGY](https://eduquestsfr.pages.dev/galaxy-control-3d-strategy.html)
- [IDLE BATHROOM EMPIRE TYCOON](https://eduquests.pages.dev/idle-bathroom-empire-tycoon.html)
- [HERO MATCH](https://eduquestsfr.pages.dev/hero-match.html)
- [STUNT BIKE RIDER BROS](https://eduquestsfr.pages.dev/stunt-bike-rider-bros.html)
- [MOJICON WINTER CONNECT](https://eduquests.pages.dev/mojicon-winter-connect.html)
- [HOSTAGE FISHES](https://ieduquests.web.app/hostage-fishes.html)
- [CATEGORY MAKEUP51](https://eduquestsfr.pages.dev/category-makeup51.html)
- [ALIEN CHASE SWING AND FLY](https://eduquestsfr.pages.dev/alien-chase-swing-and-fly.html)
- [CATEGORY PUZZLE 7](https://eduquestsfr.pages.dev/category-puzzle-7.html)
- [PIZZA PUZZLE](https://eduquests.pages.dev/pizza-puzzle.html)
- [CATEGORY CRAFTING45](https://eduquestsfr.pages.dev/category-crafting45.html)
- [MINIGIANTS IO](https://eduquestspt.pages.dev/minigiants-io.html)
- [BFFS K POP FANGIRLS](https://eduquests.pages.dev/bffs-k-pop-fangirls.html)
- [ARCADE GP](https://eduquestspt.pages.dev/arcade-gp.html)
- [FRUITE SWIPE](https://ieduquests.web.app/fruite-swipe.html)
- [INDEX34](https://eduquestsfr.pages.dev/index34.html)
- [CATEGORY SOCCER](https://eduquestsfr.pages.dev/category-soccer.html)
- [CATEGORY DEFENSE174](https://eduquestsfr.pages.dev/category-defense174.html)
- [WINTER MAZE](https://eduquestspt.pages.dev/winter-maze.html)
- [CATEGORY QUIZ40](https://eduquestsfr.pages.dev/category-quiz40.html)
- [PRINCESS VS SHARK](https://eduquestspt.pages.dev/princess-vs-shark.html)
- [MONEY MAN 3D](https://eduquestsfr.pages.dev/money-man-3d.html)
- [MATCH TEN NUMBER PUZZLE](https://eduquests.pages.dev/match-ten-number-puzzle.html)
- [TURBO STARS](https://eduquestspt.pages.dev/turbo-stars.html)
- [WITCHY AND THE PUZZLE ADVENTURES](https://eduquestspt.pages.dev/witchy-and-the-puzzle-adventures.html)
- [WAR STATE IO CONQUER BATTLES](https://eduquestsfr.pages.dev/war-state-io-conquer-battles.html)
- [INDEX28](https://eduquestsfr.pages.dev/index28.html)
- [CATEGORY DRESS UP](https://eduquestsfr.pages.dev/category-dress-up.html)
- [FUNNY BALLS 2048](https://eduquests.pages.dev/funny-balls-2048.html)
- [GOLF MINI](https://ieduquests.web.app/golf-mini.html)
- [COOKING RESTAURANT KITCHEN](https://eduquestsfr.pages.dev/cooking-restaurant-kitchen.html)
- [IDLE MONEY FACTORY](https://eduquestsfr.pages.dev/idle-money-factory.html)
- [JAILBREAK ROBLOX JUMPER](https://eduquestspt.pages.dev/jailbreak-roblox-jumper.html)
- [ROYAL GARDEN MATCH](https://eduquests.pages.dev/royal-garden-match.html)
- [DOMINO ONLINE MULTIPLAYER](https://eduquestspt.pages.dev/domino-online-multiplayer.html)
- [BOLTS AND NUTS SORTING](https://ieduquests.web.app/bolts-and-nuts-sorting.html)
- [MOVE EMOJI](https://eduquests.pages.dev/move-emoji.html)
- [TRAFFIC RACING](https://eduquests.pages.dev/traffic-racing.html)
- [CATEGORY MOBILE2 112](https://eduquestspt.pages.dev/category-mobile2-112.html)
- [FIERCE BATTLE BREAKOUT](https://eduquestsfr.pages.dev/fierce-battle-breakout.html)
- [FNF UNBLOCKED ITALIAN BRAINROT](https://eduquests.pages.dev/fnf-unblocked-italian-brainrot.html)
- [ZUMBLE STORY](https://eduquestsfr.pages.dev/zumble-story.html)
- [ITALIAN BRAINROT CLICKER](https://eduquestspt.pages.dev/italian-brainrot-clicker.html)
- [DINO RANCH](https://eduquestses.pages.dev/dino-ranch.html)
- [HILL CLIMB TRUCK TRANSFORM ADVENTURE](https://eduquestsfr.pages.dev/hill-climb-truck-transform-adventure.html)
- [FLOWER COLLECTION](https://eduquestsfr.pages.dev/flower-collection.html)
- [CATEGORY TOWER DEFENSE](https://eduquestspt.pages.dev/category-tower-defense.html)
- [SUPER FOOTBALL FEVER](https://eduquestsfr.pages.dev/super-football-fever.html)
- [PIZZA PUZZLE](https://eduquestses.pages.dev/pizza-puzzle.html)
- [DRAWER SORT](https://eduquestses.pages.dev/drawer-sort.html)
- [GRANNY HALLOWEEN HOUSE](https://eduquestses.pages.dev/granny-halloween-house.html)
- [LAS VEGAS POKER](https://eduquestspt.pages.dev/las-vegas-poker.html)
- [BURGER CATCH](https://eduquestsfr.pages.dev/burger-catch.html)
- [CATEGORY DRAGON22](https://eduquestspt.pages.dev/category-dragon22.html)
- [CAPYBARA SKEWER MATCH](https://eduquestses.pages.dev/capybara-skewer-match.html)
- [STICK HERO FIGHT](https://eduquests.pages.dev/stick-hero-fight.html)
- [UPSIDE DOWN](https://ieduquests.web.app/upside-down.html)
- [DRAW TO SMASH](https://eduquestses.pages.dev/draw-to-smash.html)
- [SCHOOL TEACHER GAME SCHOOL DAY](https://eduquestses.pages.dev/school-teacher-game-school-day.html)
- [BRICK BLAZE](https://eduquestses.pages.dev/brick-blaze.html)
- [FUNNY WALK FAIL RUN](https://eduquestsfr.pages.dev/funny-walk-fail-run.html)
- [CATEGORY DRAWING](https://eduquestspt.pages.dev/category-drawing.html)
- [CATEGORY WAR GAME](https://eduquestsfr.pages.dev/category-war-game.html)
- [HIDDEN OBJECTS ISLAND](https://eduquestsfr.pages.dev/hidden-objects-island.html)
- [SOLITAIRE KLONDIKE](https://eduquestspt.pages.dev/solitaire-klondike.html)
- [CATEGORY FOOTBALL](https://eduquestses.pages.dev/category-football.html)
- [NUBIK IN THE MONSTER WORLD](https://eduquestsfr.pages.dev/nubik-in-the-monster-world.html)
- [IDLE PET](https://eduquestses.pages.dev/idle-pet.html)
- [CATEGORY NETSUPPORT](https://eduquests.pages.dev/category-netsupport.html)
- [REVERSI](https://eduquests.pages.dev/reversi.html)
- [SWAT CATS SHOOTER](https://eduquestspt.pages.dev/swat-cats-shooter.html)
- [ASMR MAKEOVER MAKEUP STUDIO](https://ieduquests.web.app/asmr-makeover-makeup-studio.html)
- [SAUSAGE MAN SHOOTING ADVENTURE](https://ieduquests.web.app/sausage-man-shooting-adventure.html)
- [TWO CARTS DOWNHILL](https://eduquestses.pages.dev/two-carts-downhill.html)
- [CATEGORY BUBBLE SHOOTER27](https://eduquestses.pages.dev/category-bubble-shooter27.html)
- [MAZE CUBE 2048](https://eduquestsfr.pages.dev/maze-cube-2048.html)
- [SUPER SWING](https://eduquestses.pages.dev/super-swing.html)
- [FOREST SURVIVOR ROUGELIKE](https://eduquestspt.pages.dev/forest-survivor-rougelike.html)
- [PORTALS](https://eduquestses.pages.dev/portals.html)
- [LOGIC BLAST EXPLORER](https://ieduquests.web.app/logic-blast-explorer.html)
- [BATTLE OF TANK STEEL](https://eduquestses.pages.dev/battle-of-tank-steel.html)
- [LITTLE CANDY BAKERY](https://eduquestspt.pages.dev/little-candy-bakery.html)
- [CHALLENGE YOUR FRIENDS](https://eduquestsfr.pages.dev/challenge-your-friends.html)
- [RACING CAR DESTROYING ZOMBIE](https://eduquestsfr.pages.dev/racing-car-destroying-zombie.html)
- [LOGIC SLIDE](https://eduquestses.pages.dev/logic-slide.html)
- [BOLTS UNSCREW IT](https://eduquestses.pages.dev/bolts-unscrew-it.html)
- [IDLE AIRPORT CEO](https://eduquestsfr.pages.dev/idle-airport-ceo.html)
- [BOAT GAME RACING SIMULATOR 3D](https://eduquestses.pages.dev/boat-game-racing-simulator-3d.html)
- [OBBY HIGHEST JUMP EVER](https://eduquestspt.pages.dev/obby-highest-jump-ever.html)
- [CATEGORY TRAFFIC34](https://welearnaction.onrender.com/category-traffic34.html)
- [CATEGORY SOCCER](https://welearnaction.onrender.com/category-soccer.html)
- [BUS DRIVER](https://learnaction.github.io/bus-driver.html)
- [3D BLOCK GLADIATOR SWORD DRAW](https://eduquests.github.io/3d-block-gladiator-sword-draw.html)
- [MAZE ESCAPE TOILET RUSH](https://eduquestses.pages.dev/maze-escape-toilet-rush.html)
- [CUBES CRUSHER](https://ieduquests.web.app/cubes-crusher.html)
- [UNTWIST ROAD](https://eduquestspt.pages.dev/untwist-road.html)
- [TENNIS MASTERS 2026](https://learnaction.github.io/tennis-masters-2026.html)
- [REAL DRIVE 3D](https://eduquestses.pages.dev/real-drive-3d.html)
- [CATEGORY HERO71](https://learnaction.github.io/category-hero71.html)
- [FARM TILES HARVEST](https://eduquestses.pages.dev/farm-tiles-harvest.html)
- [CATEGORY SIMULATION 5](https://eduquestsfr.pages.dev/category-simulation-5.html)
- [MOSCOW METRO DRIVER 3D](https://learnaction.github.io/moscow-metro-driver-3d.html)
- [CATEGORY BRAIN261](https://learnaction.github.io/category-brain261.html)
- [SUPER FOOTBALL FEVER](https://brainquests.pages.dev/super-football-fever.html)
- [SOFT GIRLS WINTER AESTHETICS](https://eduquestsfr.pages.dev/soft-girls-winter-aesthetics.html)
- [BRAINROT EVOLUTION](https://eduquestses.pages.dev/brainrot-evolution.html)
- [POP CULTURE HALLOWEEN MAKEUP](https://brainquests.pages.dev/pop-culture-halloween-makeup.html)
- [ISOMETRIC ESCAPE 2](https://welearnaction.onrender.com/isometric-escape-2.html)
- [ISOMETRIC ESCAPE 2](https://ieduquests.web.app/isometric-escape-2.html)
- [WHAT S THE DIFFERENCE ONLINE](https://eduquestsfr.pages.dev/what-s-the-difference-online.html)
- [REAL FLIGHT SIMULATOR](https://brainquests.pages.dev/real-flight-simulator.html)
- [HERO TRANSFORM RUN](https://learnaction.github.io/hero-transform-run.html)
- [ANGRY FLAPPY](https://brainquests.pages.dev/angry-flappy.html)
- [SUPER NINJA BALLOON](https://eduquestses.pages.dev/super-ninja-balloon.html)
