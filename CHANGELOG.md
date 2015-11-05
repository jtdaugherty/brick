
Brick changelog
---------------

0.2.2
-----

Demo changes:
* Improved the list demo by using characters instead of integers in the
  demo list and cleaned up item-adding code (thanks Jøhannes Lippmann
  <code@schauderbasis.de>)

0.2.1
-----

Bug fixes:
* List:
  * Fixed size policy of lists so that rather than being Fixed/Fixed,
    they are Greedy/Greedy. This resolves issues that arise when the box
    layout widget renders a list widget alongside a Fixed/Fixed one.
    (Closes issue #17, thanks Karl Voelker)
* Scrolling:
  * vScrollPage actually scrolls vertically now rather than horizontally
    (Thanks Hans-Peter Deifel <hpd@hpdeifel.de>)

0.2
---

API changes:
* Added top-level `Brick` module that re-exports the most important
  modules in the library.
* List:
  * Now instead of passing the item-drawing function to the `list` state
    constructor, it is passed to `renderList`
  * `renderList` now takes the row height of the list's item widgets.
    The list item-drawing function must respect this in order for
    scrolling to work properly. This change made it possible to optimize
    the list so that it only draws widgets visible in the viewport
    rather than rendering all of the list's items (even the ones
    off-screen). But to do this we must be able to tell in advance
    how high each one is, so we require this parameter. In addition
    this change means that lists no longer support items of different
    heights.
  * The list now uses Data.Vector instead of [a] to store items; this
    permits efficient slicing so we can do the optimized rendering
    described above.
* The `HandleEvent` type class `handleEvent` method now runs in
  `EventM`. This permits event-handling code implemented in terms of
  `HandleEvent` to do get access to viewport state and to run IO code,
  making it just as powerful as code in the top-level `EventM` handler.
* Many types were moved from `Brick.Widgets.Core` and `Brick.Main` to
  `Brick.Types`, making the former module merely a home for `Widget`
  constructors and combinators.
* The `IsString` instance for `Widget` was removed; this might be
  reinstated later, but this package provides enough `IsString`
  instances that things can get confusing.
* `EventM` is now reader monad over the most recent rendering pass's
  viewport state, in addition to being a state monad over viewport
  requests for the renderer. Added the `lookupViewport` function to
  provide access to the most recent viewport state. Exported the
  `Viewport` type and lenses.
* Now that `handleEvent` is now an `EventM` action, composition with
  `continue` et al got a little messier when using lenses to
  update the application state. To help with this, there is now
  `handleEventLensed`.

Bugfixes:
* Lists now perform well with 10 items or a million (see above; fixes
  #7, thanks Simon Michael)
* Added more haddock notes to `Brick.Widgets.Core` about growth
  policies.
* Forced evaluation of render states to address a space leak in the
  renderer (fixes #14, thanks Sebastian Reuße <seb@wirrsal.net>)
* str: only reference string content that can be shown (eliminates a
  space leak, fixes #14, thanks Sebastian Reuße <seb@wirrsal.net>)

Misc:
* Added a makefile for the user guide.
* List: added support for Home and End keys (thanks Simon Michael)
* Viewports: when rendering viewports, scroll requests from `EventM` are
  processed before visibility requests from the rendering process; this
  reverses this previous order of operations but permits user-supplied
  event handlers to reset viewports when desired.

Package changes:
* Added `deepseq` dependency

0.1
---
Initial release
