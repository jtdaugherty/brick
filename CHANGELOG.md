
Brick changelog
---------------

0.29
----

API changes:
 * Added Ord instances for `Location` and `BrickEvent` (thanks Tom
   Sydney Kerckhove)
 * `Brick.AttrMap`: attribute name components are now exposed via the
   `attrNameComponents` function. Also added a Read instance for
   AttrName.

New features:
 * This release adds user-customizable theme support. Please see the
   "Attribute Themes" section of the User Guide for an introduction; see
   the Haddock documentation for `Brick.Themes` for full details. Also,
   see the new `programs/ThemeDemo.hs` for a working demonstration.

0.28
----

API changes:
 * Brick.AttrMap.setDefault was renamed to setDefaultAttr.
 * Added Brick.AttrMap.getDefaultAttr: get the default attribute from an
   attribute map.
 * Added Brick.Widgets.Core.modifyDefAttr to modify the default
   attribute of the rendering context.

Other changes:
 * Updated AttrDemo to show usage of modifyDefAttr.

0.27
----

API changes:
 * Brick.Widgets.Core: added `hyperlink` combinator (thanks Getty Ritter
   for hyperlinking support)

Other changes:
 * Updated AttrDemo to show how to use hyperlinking
 * README: Added `herms` to featured projects

0.26.1
------

 * Fixed haddock for listHandleEventVi.

0.26
----

API changes:
 * Added Brick.Widgets.List.handleListEventVi to add support for
   vi-style movements to lists (thanks Richard Alex Hofer)

Other changes:
 * Added ListViDemo.hs to demonstrate the Vi-style handler for lists
   (thanks Richard Alex Hofer)

0.25
----

API changes:
 * List: added page movement functions `listMoveByPages`,
   `listMovePageUp`, and `listMovePageDown` (thanks Richard Alex Hofer)

Miscellaneous:
 * Fixed a spelling mistake in the AttrMap haddock (thanks Edward Betts)

0.24.2
------

Miscellaneous:
 * Minor documentation updates including a clarification for #135

0.24.1
------

Bug fixes:
 * vBox/hBox: when there is leftover space and all elements are greedy,
   spread it amongst the elements as evenly as possible instead of
   assigning it all to the first element (fixes #133)

Package changes:
 * Include Sam Tay's brick tutorial files in extra-doc-files

0.24
----

API changes:
 * Added Brick.Widgets.Core.setAvailableSize to control rendering
   context size in cases where the screen size is too constraining (e.g.
   for a floating layer that might be bigger than the screen).

Documentation changes:
 * Samuel Tay has contributed his wonderful Brick tutorial to this
   package in docs/samtay-tutorial.md. Thank you!

0.23
----

API changes:
 * getVtyHandle: always return a Vty handle rather than Maybe
   (Previously, in appStartEvent you'd get Nothing because Vty had
   not been initialized yet. This made various use cases impossible
   to satisfy because appStartEvent is a natural place to get initial
   terminal state from Vty. This change makes it so that a Vty handle is
   always available, even in appStartEvent.)
 * txtWrapWith: added missing haddock

0.22
----

API changes:
 * Core: added txtWrapWith and strWrapWith functions to provide control
   over wrapping behavior by specifying custom wrapping settings.

Other changes:
 * Updated TextWrapDemo.hs to demonstrate customizing wrapping settings.

0.21
----

Package changes:
 * Upgrade to word-wrap 0.2

Other changes:
 * Brick.Types.Internal: improve mouse constructor haddock
 * Add a basic fill demonstration program (FillDemo.hs)

0.20.1
------

Bug fixes:
 * str: fixed an IsString constraint confusion on GHC 7.10.1

0.20
----

Package changes:
 * Added a dependency on "word-wrap" for text-wrapping.
 * Added a new TextWrapDemo demo program to illustrate text wrapping
   support

API changes:
 * Brick.Widgets.Core: added new functions txtWrap and strWrap to do
   wrapping of long lines of text.

Miscellaneous:
 * Guide: fixed event type (#126)

0.19
----

API changes:
 * The editor content drawing function is now passed to renderEditor,
   not the constructor, to improve separation of presentation and
   representation concerns. The corresponding Editor drawing function
   lens and accessor were removed.

0.18
----

Package changes:
 * Added a dependency on data-clist.

API changes:
 * Brick.Focus: removed the Functor instance for FocusRing.
 * Brick.Focus: re-implemented FocusRing in terms of the circular list
   data structure from data-clist. In addition, this change introduced
   "focusRingModify", which permits the user to use the data-clist API
   to directly manipulate the FocusRing's internals. This way brick
   doesn't have to re-invent the wheel on the focus ring behavior.

0.17.2
------

Package changes:
 * Added programs/ReadmeDemo.hs and featured its output and code in the
   README to provide an early demonstration

Library changes:
 * centerAbout now right- and bottom-pads its operand to behave
   consistently with h/vCenter

0.17.1
------

Package changes:
 * Use Extra-Doc-Files instead of Data-Files for documentation files

Bug fixes:
 * List: correctly update selected index in listInsert
 * Update example program in brick.cabal (thanks @timbod7)

0.17
----

Package changes:
* Updated to depend on Vty 5.15.
* Updated to remove dependency on data-default.
* Discontinued support for GHC versions prior to 7.10.1.

API changes:
* Removed Data.Default instances for AttrName, AttrMap, Result, and
  BorderStyle (use Monoid instances instead where possible).
* Added defaultBorderStyle :: BorderStyle.
* Added emptyResult :: Result n.

0.16
----

This release includes a breaking API change:
* Brick now uses bounded channels (Brick.BChan.BChan) for event
  communication rather than Control.Concurrent.Chan's unbounded channels
  to improve memory consumption for programs with runaway event
  production (thanks Joshua Chia)

Other API changes:
* Brick.List got a new function, listModify, for modifying the selected
  element (thanks @diegospd)

Performance improvements:
* hBox and vBox now use the more efficient DList data structure when
  rendering to improve performance for boxes with many elements (thanks
  Mitsutoshi Aoe)

0.15.2
------

Bug fixes:
* viewport: do not cull cursor locations on empty viewport contents
  (fixes #105)
* User guide CounterEvent type fix (thanks @diegospd)

0.15.1
------

Bug fixes:
* List: fixed empty list validation in listReplace (thanks Joshua Chia)

0.15
----

Demo changes:
* MouseDemo: add an editor and use mouse events to move the cursor
* MouseDemo: Enhance MouseDemo to show interaction between 'clickable'
  and viewports (thanks Kevin Quick)

New features:
* Editors now report mouse click events

API changes:
* Rename TerminalLocation row/column fields to avoid commonplace name
  clashes; rename row/column to locationRow/locationColumn (fixes #96)

Bug fixes:
* Core: make cropToContext also crop extents (fixes #101)
* viewport: if the sub-widget is not rendered, also cull all extents and
  cursor locations

Documentation changes:
* User Guide updates: minor fixes, updates to content on custom widgets,
  wide character support, and examples (thanks skapazzo@inventati.org,
  Kevin Quick)

0.14
----

This release added support for wide characters. In particular, wide
characters can now be entered into the text editor widget and used in
'str' and 'txt' widgets.

0.13
----

API changes:
 * Mouse mode is no longer enabled by default.
 * customMain's event channel parameter is now optional
 * FocusRing now provides a Functor instance (thanks Ian Jeffries)

0.12
----

This release primarily adds support for mouse interaction. For details,
see the Mouse Support section of the User Guide. This release also
includes breaking API changes for the App type. Here's a migration
guide:

 * Event handlers now take "BrickEvent n e" instead of "e", where "e"
   was the custom event type used before this change. To recover your
   own custom events, pattern-match on "AppEvent"; to recover Vty input
   events, pattern-match on "VtyEvent".
 * appLiftVtyEvent went away and can just be removed from your App
   record constructor.
 * If you aren't using the custom event type or were just using Vty's
   "Event" type as your App's event type, you can set your event type to
   just "e" because you'll now be able to get Vty events regardless of
   whether you use a custom event type.

API changes:
 * Added the Widget combinator "clickable" to indicate that a widget
   should generate mouse click events
 * Added the Extent data type and the "reportExtent" widget combinator
   to report the positions and sizes of widgets
 * Rendering "Result" values now include reported extents and update
   their offsets (adds "extents" field and "extentsL" lens)
 * Added "lookupExtent", "findClickedExtents", and "clickedExtent" in
   EventM to find extents and check them for mouse clicks
 * Removed appLiftVtyEvent. Instead of wrapping Vty's events in your own
   type, you now get a "BrickEvent" that always contains Vty events but
   has the ability to embed *your* custom events. See the User Guide for
   details.
 * Added demo program MouseDemo.hs
 * Added demo program ProgressBarDemo.hs (thanks Kevin Quick)
 * Added mapAttrname, mapAttrNames, and overrideAttr functions (thanks
   Kevin Quick)
 * Make handleEventLensed polymorphic over event type to allow use with
   custom events (thanks Kevin Quick)
 * Added Ord constraint to some library startup functions

Bug fixes:
 * Added Show instance for Editor, List (fixes #63)

Documentation changes:
 * Updated documentation to use new "resource name" terminology to
   reduce confusion and better explain the purpose of names.
 * Updated user guide with sections on mouse support, the rendering
   cache, resource names, paste mode, and extents

Package changes:
 * Depend on Vty 5.11.3 to get mouse mode support

0.11
----

API changes:
 * Added getVtyHandle in EventM for obtaining the current Vty context.
   It returns Nothing when calling the appStartEvent handler but after
   that a context is always available.

0.10
----

New features:
 * Added a rendering cache. To use the rendering cache, use the 'cached'
   widget combinator. This causes drawings of the specified widget to
   re-use a cached rendering until the rendering cache is invalidated
   with 'invalidateCacheEntry' or 'invalidateCache'. This change also
   includes programs/CacheDemo.hs. This change introduced an Ord
   constraint on the name type variable 'n'.
 * Added setTop and setLeft for setting viewport offsets directly in
   EventM.
 * Dialog event handlers now support left and right arrow keys (thanks
   Grégoire Charvet)

Library changes:
 * On resizes brick now draws the application twice before handling the
   resize event. This change makes it possible for event handlers to
   get the latest viewport states on a resize rather than getting the
   most recent (but stale) versions as before, at the cost of a second
   redraw.

Bug fixes:
 * We now use the most recent rendering state when setting up event handler
   viewport data. This mostly won't matter to anyone except in cases
   where a viewport name was expected to be in the viewport map but
   wasn't due to using stale rendering state to set up EventM.

0.9
---

Package changes:
 * Depend on text-zipper 0.7.1

API changes:
 * The editor widget state value is now polymorphic over the type of
   "string" value that can be edited, so you can now create editors over
   Text values as well as Strings. This is a breaking change but it only
   requires the addition of the string type variable to any uses of
   Editor. (thanks Jason Dagit and Getty Ritter)
 * Added some missing Eq and Show instances (thanks Grégoire Charvet)

New features:
 * The editor now binds Control-U to delete to beginning of line (thanks
   Hans-Peter Deifel)

Bug fixes:
 * List: avoid runtime exception by ensuring item height is always at
   least 1

0.8
---

API changes:
 * Center: added layer-friendly centering functions centerLayer,
   hCenterLayer, and vCenterLayer.

Functionality changes:
 * Dialog now uses new layer-friendly centering functions. This makes it
   possible to overlay a Dialog on top of your UI when you use a Dialog
   rendering as a separate layer.
 * Updated the LayerDemo to demonstrate a centered layer.
 * The renderer now uses a default Vty Picture background
   of spaces with the default attribute, rather than using
   ClearBackground (the Vty default). This is to compensate for an
   unexpected attribute behavior in Vty when ClearBackgrounds (see
   https://github.com/coreyoconnor/vty/issues/95)

0.7
---

NOTE: this release includes many API changes. Please see the "Widget
Names" section of the Brick User Guide for details on the fundamentals!

API changes:
 * The "Name" type was removed. In its place we now have a name type
   variable ("n") attached to many types (including EventM,
   CursorLocation, App, Editor, List, and FocusRing). This change makes
   it possible to:
   * Avoid runtime errors due to name typos
   * Achieve compile-time guarantees about name matching and usage
   * Force widget functions to be name-agnostic by being polymorphic
     in their name type
   * Clean up focus handling by making it possible to pattern-match
     on cursor location names
 * The EditDemo demonstration program was updated to use a FocusRing.
 * Added the "Named" type class to Brick.Widgets.Core for types that
   store names. This type class is used to streamline the Focus
   interface; see Brick.Focus.withFocusRing and EditDemo.hs.
 * The List and Editor types are now parameterized on names.
 * The List widget is now focus-aware; its rendering function now takes
   a boolean indicating whether it should be rendered with focus. The
   List uses the following attributes now:
   * When not focused, the cursor is rendered with listSelectedAttr.
   * When focused, the cursor is rendered with listSelectedFocusedAttr.
 * The Editor widget is now focus-aware; its rendering function now
   takes a boolean indicating whether it should be rendered with focus.
   The Editor uses the following attributes now:
   * When not focused, the widget is rendered with editAttr.
   * When focused, the widget is rendered with editFocusedAttr.
 * The Dialog's name constructor parameter and lens were removed.
 * The 'viewport' function was modified to raise a runtime exception if
   the widget name it receives is used more than once during the
   rendering of a single frame.

Miscellaneous:
 * Many modules now use conditional imports to silence redundancy
   warnings on GHCs with newer Preludes (e.g. including Monoid,
   Foldable, Traversable, Applicative, etc.)

0.6.4
-----

Bug fixes:
 * Add missing Functor instance for Next type (thanks Markus Hauck)

0.6.3
-----

Bug fixes:
 * List: the list now properly renders when the available height is not
   a multiple of the item height. Previously the list size would
   decrease relative to the available height. Now the list renders
   enough items to fill the space even if the top-most or bottom-most
   item is partially visible, which is the expected behavior.

0.6.2
-----

Bug fixes:
 * Editor: the 'editor' initial content parameter is now correctly split
   on newlines to ensure that the underlying editor zipper is
   initialized properly. (fixes #56; thanks @listx)

0.6.1
-----

Package changes:
 * Added lower bound for microlens >= 0.3.0.0 to fix build failure due
   to Field1 not being defined (thanks Markus Hauck)

Documentation changes:
 * Updated user guide and README to link to and mention microlens
   instead of lens

Misc:
 * Fixed a qualified import in the List demo to avoid ambiguity (thanks
   Alan Gilbert)

0.6
---

API changes:
 * Brick now uses the microlens family of packages instead of lens. This
   version of brick also depends on vty 5.5.0, which was modified to use
   microlens instead of lens. This change shouldn't impact functionality
   but will greatly reduce build times.

0.5.1
-----

Bug fixes:
 * Fix negative cropping in hCenter, vCenter, and cropResultToContext
   (fixes #52)
 * Remove unnecessary Eq constraint from listReplace (fixes #48; thanks
   sifmelcara)
 * Mention Google Group in README

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
