
Brick changelog
---------------

0.5
---

Functionality changes:
 * Markup: make markup support multi-line strings (fixes #41)
 * brick-edit-demo: support shift-tab to switch editors
 * Core: improve box layout algorithm (when rendering boxes, track
   remaining space while rendering high-priority children to use
   successively more constrained primary dimensions)
 * Core: make fixed padding take precedence over padded widgets (fixes #42)
   Prior to this commit, padding a widget meant that if there was room
   after rendering the widget, the specified amount of padding would be
   added. This meant that under tight layout constraints padding would
   disappear before a padded widget would. This is often a desirable
   outcome but it also led to unexpected behavior when adding padding
   to a widget that grows greedily: fixed padding would never show up
   because it was placed in a box adjacent to the widget in question,
   and boxes always render greedy children before fixed ones. As a
   result fixed padding would disappear under these conditions. Instead,
   in the case of fixed padding, since we often intend to *guarantee*
   that padding is present, all of the padding combinators have been
   modified so that when the padded widget is rendered with fixed
   padding in the amount V, the widget is given V fewer rows/columns
   when it is rendered so that the padding always has room.

0.4.1
-----

Bug fixes:
* Fixed a bug in the 'visible' combinator: If the size of the visibility
  request was larger than the available space, then the rendering of a
  viewport was toggling between two states, one with aligning on the
  end of the visibility request, and another one aligning on the start.
  This commit fixes it so that a visibility request is always aligned
  on the start if not enough space is available. (thanks Thomas Strobel
  <ts468@cam.ac.uk>)

Behavior changes:
* Honor multiple 'visible' markers in a single viewport with preference
  on the innermost request (thanks Thomas Strobel <ts468@cam.ac.uk>)

0.4
---

API changes:
* Added Brick.Widgets.Core.unsafeLookupViewport to make certain kinds
  of custom widget implementations easier when viewport states are needed
  (thanks Markus Hauck <markus1189@gmail.com>)
* List: added listClear and listReverse functions (thanks Markus Hauck)
* List: Derive instances for Functor, Foldable, Traversable (thanks
  Markus Hauck)

Documentation changes:
* Hyperlink "Data.Text.Markup" inside Brick.Markup haddock (thanks
  Markus Hauck)
* Fix typo in 'Attribute Management' section of user guide (thanks
  Markus Hauck)

0.3.1
-----

Bug fixes:
* EventM newtype again instances MonadIO (thanks Andrew Rademacher)

0.3
---

API changes:
* Made EventM a newtype instead of a type alias
* List: listReplace now takes the new selected index and no longer does
element diffing

Package changes:
* Removed the dependency on the Diff package

Misc:
* Applied some hlint hints (thanks Markus Hauck <markus1189@gmail.com>)
* Fixed a typo in the README (thanks Markus Hauck <markus1189@gmail.com>)
* Improved the renderList documentation (thanks Profpatsch <mail@profpatsch.de>)
* Types: added an explicit import of Applicative for older GHCs

0.2.3
-----

Bug fixes:
* Fixed viewport behavior when the image in a viewport reduces its size
  enough to render the viewport offsets invalid. Before, this behavior
  caused a crash during image croppin in vty; now the behavior is
  handled sanely (fixes #22; reported by Hans-Peter Deifel)

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
