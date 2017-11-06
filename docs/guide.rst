Brick User Guide
~~~~~~~~~~~~~~~~

.. contents:: `Table of Contents`

Introduction
============

``brick`` is a Haskell library for programming terminal user interfaces.
Its main goal is to make terminal user interface development as painless
and as direct as possible. ``brick`` builds on `vty`_; `vty` provides
the terminal input and output interface and drawing primitives,
while ``brick`` builds on those to provide a high-level application
abstraction and combinators for expressing user interface layouts.

This documentation is intended to provide a high-level overview of
the library's design along with guidance for using it, but details on
specific functions can be found in the Haddock documentation.

The process of writing an application using ``brick`` entails writing
two important functions:

- A *drawing function* that turns your application state into a
  specification of how your interface should look, and
- An *event handler* that takes your application state and an input
  event and decides whether to change the state or quit the program.

We write drawing functions in ``brick`` using an extensive set of
primitives and combinators to place text on the screen, set its
attributes (e.g. foreground color), and express layout constraints (e.g.
padding, centering, box layouts, scrolling viewports, etc.).

These functions get packaged into a structure that we hand off to the
``brick`` library's main event loop. We'll cover that in detail in `The
App Type`_.

Installation
------------

``brick`` can be installed in the "usual way," either by installing
the latest `Hackage`_ release or by cloning the GitHub repository and
building locally.

To install from Hackage::

   $ cabal update
   $ cabal install brick

To clone and build locally::

   $ git clone https://github.com/jtdaugherty/brick.git
   $ cd brick
   $ cabal sandbox init
   $ cabal install -j

Building the Demonstration Programs
-----------------------------------

``brick`` includes a large collection of feature-specific demonstration
programs. These programs are not built by default but can be built by
passing the ``demos`` flag to ``cabal install``, e.g.::

   $ cabal install brick -f demos

Conventions
===========

``brick`` has some API conventions worth knowing about as you read this
documentation and as you explore the library source and write your own
programs.

- Use of `microlens`_ packages: ``brick`` uses ``microlens`` family of
  packages internally and also exposes lenses for many types in the
  library. However, if you prefer not to use the lens interface in your
  program, all lens interfaces have non-lens equivalents exported by
  the same module. In general, the "``L``" suffix on something tells
  you it is a lens; the name without the "``L``" suffix is the non-lens
  version. You can get by without using ``brick``'s lens interface but
  your life will probably be much more pleasant once your application
  state becomes sufficiently complex if you use lenses to modify it (see
  `appHandleEvent: Handling Events`_).
- Attribute names: some modules export attribute names (see `How
  Attributes Work`_) associated with user interface elements. These tend
  to end in an "``Attr``" suffix (e.g. ``borderAttr``). In addition,
  hierarchical relationships between attributes are documented in
  Haddock documentation.
- Use of qualified Haskell identifiers: in this document, where
  sensible, I will use fully-qualified identifiers whenever I mention
  something for the first time or whenever I use something that is
  not part of ``brick``. Use of qualified names is not intended to
  produce executable examples, but rather to guide you in writing your
  ``import`` statements.

The App Type
============

To use the library we must provide it with a value of type
``Brick.Main.App``. This type is a record type whose fields perform
various functions:

.. code:: haskell

   data App s e n =
       App { appDraw         :: s -> [Widget n]
           , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
           , appHandleEvent  :: s -> BrickEvent n e -> EventM n (Next s)
           , appStartEvent   :: s -> EventM n s
           , appAttrMap      :: s -> AttrMap
           }

The ``App`` type is parameterized over three types. These type variables
will appear in the signatures of many library functions and types. They
are:

- The **application state type** ``s``: the type of data that will
  evolve over the course of the application's execution. Your
  application will provide the library with its starting value and event
  handling will transform it as the program executes. When a ``brick``
  application exits, the final application state will be returned.
- The **event type** ``e``: the type of custom application events
  that your application will need to produce and handle in
  ``appHandleEvent``. All applications will be provided with events from
  the underlying ``vty`` library, such as keyboard events or resize
  events; this type variable indicates the type of *additional* events
  the application will need. For more details, see `Using Your Own Event
  Type`_.
- The **resource name type** ``n``: during application execution we
  sometimes need a way to refer to rendering state, such as the space
  taken up by a given widget, the state for a scrollable viewport, a
  mouse click, or a cursor position. For these situations we need a
  unique handle called a *resource name*. The type ``n`` specifies the
  name type the application will use to identify these bits of state
  produced and managed by the renderer. The resource name type must be
  provided by your application; for more details, see `Resource Names`_.

The various fields of ``App`` will be described in the sections below.

Running an Application
----------------------

To run an ``App``, we pass it to ``Brick.Main.defaultMain`` or
``Brick.Main.customMain`` along with an initial application state value:

.. code:: haskell

   main :: IO ()
   main = do
     let app = App { ... }
         initialState = ...
     finalState <- defaultMain app initialState
     -- Use finalState and exit

The ``customMain`` function is for more advanced uses; for details see
`Using Your Own Event Type`_.

appDraw: Drawing an Interface
-----------------------------

The value of ``appDraw`` is a function that turns the current
application state into a list of *layers* of type ``Widget``, listed
topmost first, that will make up the interface. Each ``Widget`` gets
turned into a ``vty`` layer and the resulting layers are drawn to the
terminal.

The ``Widget`` type is the type of *drawing instructions*.  The body of
your drawing function will use one or more drawing functions to build or
transform ``Widget`` values to describe your interface. These
instructions will then be executed with respect to three things:

- The size of the terminal: the size of the terminal determines how many
  ``Widget`` values behave. For example, fixed-size ``Widget`` values
  such as text strings behave the same under all conditions (and get
  cropped if the terminal is too small) but layout combinators such as
  ``Brick.Widgets.Core.vBox`` or ``Brick.Widgets.Center.center`` use the
  size of the terminal to determine how to lay other widgets out. See
  `How Widgets and Rendering Work`_.
- The application's attribute map (``appAttrMap``): drawing functions
  requesting the use of attributes cause the attribute map to be
  consulted. See `How Attributes Work`_.
- The state of scrollable viewports: the state of any scrollable
  viewports on the *previous* drawing will be considered. For more
  details, see `Viewports`_.

The ``appDraw`` function is called when the event loop begins to draw
the application as it initially appears. It is also called right after
an event is processed by ``appHandleEvent``. Even though the function
returns a specification of how to draw the entire screen, the underlying
``vty`` library goes to some trouble to efficiently update only the
parts of the screen that have changed so you don't need to worry about
this.

Where do I find drawing functions?
**********************************

The most important module providing drawing functions is
``Brick.Widgets.Core``. Beyond that, any module in the ``Brick.Widgets``
namespace provides specific kinds of functionality.

appHandleEvent: Handling Events
-------------------------------

The value of ``appHandleEvent`` is a function that decides how to modify
the application state as a result of an event:

.. code:: haskell

   appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)

The first parameter of type ``s`` is your application's state at the
time the event arrives. ``appHandleEvent`` is responsible for deciding
how to change the state based on the event and then return it.

The second parameter of type ``BrickEvent n e`` is the event itself.
The type variables ``n`` and ``e`` correspond to the *resource name
type* and *event type* of your application, respectively, and must match
the corresponding types in ``App`` and ``EventM``.

The return value type ``Next s`` value describes what should happen
after the event handler is finished. We have three choices:

* ``Brick.Main.continue s``: continue executing the event loop with the
  specified application state ``s`` as the next value. Commonly this is
  where you'd modify the state based on the event and return it.
* ``Brick.Main.halt s``: halt the event loop and return the final
  application state value ``s``. This state value is returned to the
  caller of ``defaultMain`` or ``customMain`` where it can be used prior
  to finally exiting ``main``.
* ``Brick.Main.suspendAndResume act``: suspend the ``brick`` event loop
  and execute the specified ``IO`` action ``act``. The action ``act``
  must be of type ``IO s``, so when it executes it must return the next
  application state. When ``suspendAndResume`` is used, the ``brick``
  event loop is shut down and the terminal state is restored to its
  state when the ``brick`` event loop began execution. When it finishes
  executing, the event loop will be resumed using the returned state
  value. This is useful for situations where your program needs to
  suspend your interface and execute some other program that needs to
  gain control of the terminal (such as an external editor).

The ``EventM`` monad is the event-handling monad. This monad is a
transformer around ``IO`` so you are free to do I/O in this monad by
using ``liftIO``. Beyond I/O, this monad is used to make scrolling
requests to the renderer (see `Viewports`_) and obtain named extents
(see `Extents`_). Keep in mind that time spent blocking in your event
handler is time during which your UI is unresponsive, so consider this
when deciding whether to have background threads do work instead of
inlining the work in the event handler.

Widget Event Handlers
*********************

Event handlers are responsible for transforming the application state.
While you can use ordinary methods to do this such as pattern matching
and pure function calls, some widget state types such as the ones
provided by the ``Brick.Widgets.List`` and ``Brick.Widgets.Edit``
modules provide their own widget-specific event-handling functions.
For example, ``Brick.Widgets.Edit`` provides ``handleEditorEvent`` and
``Brick.Widgets.List`` provides ``handleListEvent``.

Since these event handlers run in ``EventM``, they have access to
rendering viewport states via ``Brick.Main.lookupViewport`` and the
``IO`` monad via ``liftIO``.

To use these handlers in your program, invoke them on the relevant piece
of state in your application state. In the following example we use an
``Edit`` state from ``Brick.Widgets.Edit``:

.. code:: haskell

   data Name = Edit1
   type MyState = Editor String Name

   myEvent :: MyState -> BrickEvent n e -> EventM Name (Next MyState)
   myEvent s (VtyEvent e) = continue =<< handleEditorEvent e s

This pattern works well enough when your application state has an
event handler as shown in the ``Edit`` example above, but it can
become unpleasant if the value on which you want to invoke a handler
is embedded deeply within your application state. If you have chosen
to generate lenses for your application state fields, you can use the
convenience function ``handleEventLensed`` by specifying your state, a
lens, and the event:

.. code:: haskell

   data Name = Edit1
   data MyState = MyState { _theEdit :: Editor String Name
                          }
   makeLenses ''MyState

   myEvent :: MyState -> BrickEvent n e -> EventM Name (Next MyState)
   myEvent s (VtyEvent e) = continue =<< handleEventLensed s theEdit handleEditorEvent e

You might consider that preferable to the desugared version:

.. code:: haskell

   myEvent :: MyState -> BrickEvent n e -> EventM Name (Next MyState)
   myEvent s (VtyEvent e) = do
     newVal <- handleEditorEvent e (s^.theEdit)
     continue $ s & theEdit .~ newVal

Using Your Own Event Type
*************************

Since we often need to communicate application-specific events beyond
Vty input events to the event handler, brick supports embedding your
application's custom events in the stream of ``BrickEvent``-s that
your handler will receive. The type of these events is the type ``e``
mentioned in ``BrickEvent n e`` and ``App s e n``.

Note: ordinarily your application will not have its own custom event
type, so you can leave this type unused (e.g. ``App MyState e MyName``)
or just set it to unit (``App MyState () MyName``).

Here's an example of using a custom event type. Suppose that you'd like
to be able to handle counter events in your event handler. First we
define the counter event type:

.. code:: haskell

   data CounterEvent = Counter Int

With this type declaration we can now use counter events in our app by
using the application type ``App s CounterEvent n``. To handle these
events we'll just need to look for ``AppEvent`` values in the event
handler:

.. code:: haskell

   myEvent :: s -> BrickEvent n CounterEvent -> EventM n (Next s)
   myEvent s (AppEvent (CounterEvent i)) = ...

The next step is to actually *generate* our custom events and
inject them into the ``brick`` event stream so they make it to the
event handler. To do that we need to create a ``BChan`` for our
custom events, provide that ``BChan`` to ``brick``, and then send
our events over that channel. Once we've created the channel with
``Brick.BChan.newBChan``, we provide it to ``brick`` with
``customMain`` instead of ``defaultMain``:

.. code:: haskell

   main :: IO ()
   main = do
       eventChan <- Brick.BChan.newBChan 10
       finalState <- customMain
                       (Graphics.Vty.mkVty Data.Default.defaultConfig)
                       (Just eventChan) app initialState
       -- Use finalState and exit

The ``customMain`` function lets us have control over how the ``vty``
library is initialized *and* how ``brick`` gets custom events to give to
our event handler. ``customMain`` is the entry point into ``brick`` when
you need to use your own event type as shown here.

With all of this in place, sending our custom events to the event
handler is straightforward:

.. code:: haskell

   counterThread :: Brick.BChan.BChan CounterEvent -> IO ()
   counterThread chan = do
       Brick.BChan.writeBChan chan $ Counter 1

Bounded Channels
****************

A ``BChan``, or *bounded channel*, can hold a limited number of
items before attempts to write new items will block. In the call to
``newBChan`` above, the created channel has a capacity of 10 items.
Use of a bounded channel ensures that if the program cannot process
events quickly enough then there is a limit to how much memory will
be used to store unprocessed events. Thus the chosen capacity should
be large enough to buffer occasional spikes in event handling latency
without inadvertently blocking custom event producers. Each application
will have its own performance characteristics that determine the best
bound for the event channel. In general, consider the performance of
your event handler when choosing the channel capacity and design event
producers so that they can block if the channel is full.

Starting up: appStartEvent
**************************

When an application starts, it may be desirable to perform some of
the duties typically only possible when an event has arrived, such as
setting up initial scrolling viewport state. Since such actions can only
be performed in ``EventM`` and since we do not want to wait until the
first event arrives to do this work in ``appHandleEvent``, the ``App``
type provides ``appStartEvent`` function for this purpose:

.. code:: haskell

   appStartEvent :: s -> EventM n s

This function takes the initial application state and returns it in
``EventM``, possibly changing it and possibly making viewport requests.
This function is invoked once and only once, at application startup.
For more details, see `Viewports`_. You will probably just want to use
``return`` as the implementation of this function for most applications.

appChooseCursor: Placing the Cursor
-----------------------------------

The rendering process for a ``Widget`` may return information about
where that widget would like to place the cursor. For example, a text
editor will need to report a cursor position. However, since a
``Widget`` may be a composite of many such cursor-placing widgets, we
have to have a way of choosing which of the reported cursor positions,
if any, is the one we actually want to honor.

To decide which cursor placement to use, or to decide not to show one at
all, we set the ``App`` type's ``appChooseCursor`` function:

.. code:: haskell

   appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)

The event loop renders the interface and collects the
``Brick.Types.CursorLocation`` values produced by the rendering process
and passes those, along with the current application state, to this
function. Using your application state (to track which text input box
is "focused," say) you can decide which of the locations to return or
return ``Nothing`` if you do not want to show a cursor.

Many widgets in the rendering process can request cursor placements, but
it is up to our application to determine which one (if any) should be
used. Since we can only show at most a single cursor in the terminal,
we need to decide which location to show. One way is by looking at the
resource name contained in the ``cursorLocationName`` field. The name
value associated with a cursor location will be the name used to request
the cursor position with ``Brick.Widgets.Core.showCursor``.

``Brick.Main`` provides various convenience functions to make cursor
selection easy in common cases:

* ``neverShowCursor``: never show any cursor.
* ``showFirstCursor``: always show the first cursor request given; good
  for applications with only one cursor-placing widget.
* ``showCursorNamed``: show the cursor with the specified resource name
  or show no cursor if the name was not associated with any requested
  cursor position.

For example, this widget requests a cursor placement on the first
"``o``" in "``foo``" associated with the cursor name "``myCursor``":

.. code:: haskell

   data MyName = CustomName

   let w = showCursor CustomName (Brick.Types.Location (1, 0))
             (Brick.Widgets.Core.str "foobar")

The event handler for this application would use ``MyName`` as its
resource name type ``n`` and would be able to pattern-match on
``CustomName`` to match cursor requests when this widget is rendered:

.. code:: haskell

   myApp = App { ...
               , appChooseCursor = showCursorNamed CustomName
               }

See the next section for more information on using names.

Resource Names
--------------

We saw above in `appChooseCursor: Placing the Cursor`_ that resource
names are used to describe cursor locations. Resource names are also
used to name other kinds of resources:

* viewports (see `Viewports`_)
* rendering extents (see `Extents`_)
* mouse events (see `Mouse Support`_)

Assigning names to these resource types allows us to distinguish between
events based on the part of the interface to which an event is related.

Your application must provide some type of name. For simple applications
that don't make use of resource names, you may use ``()``. But if your
application has more than one named resource, you *must* provide a type
capable of assigning a unique name to every resource that needs one.

A Note of Caution
*****************

Resource names can be assigned to any of the resource types mentioned
above, but some resource types--viewports, extents, the render cache,
and cursor locations--form separate resource namespaces. So, for
example, the same name can be assigned to both a viewport and an extent,
since the ``brick`` API provides access to viewports and extents using
separate APIs and data structures. However, if the same name is used for
two resources of the same kind, it is undefined *which* of those you'll
be getting access to when you go to use one of those resources in your
event handler.

For example, if the same name is assigned to two viewports:

.. code:: haskell

   data Name = Viewport1

   ui :: Widget Name
   ui = (viewport Viewport1 Vertical $ str "Foo") <+>
        (viewport Viewport1 Vertical $ str "Bar") <+>

then in ``EventM`` when we attempt to scroll the viewport ``Viewport1``
we don't know which of the two uses of ``Viewport1`` will be affected:

.. code:: haskell

   do
     let vp = viewportScroll Viewport1
     vScrollBy vp 1

The solution is to ensure that for a given resource type (in this case
viewport), a unique name is assigned in each use.

.. code:: haskell

   data Name = Viewport1 | Viewport2

   ui :: Widget Name
   ui = (viewport Viewport1 Vertical $ str "Foo") <+>
        (viewport Viewport2 Vertical $ str "Bar") <+>

appAttrMap: Managing Attributes
-------------------------------

In ``brick`` we use an *attribute map* to assign attibutes to elements
of the interface. Rather than specifying specific attributes when
drawing a widget (e.g. red-on-black text) we specify an *attribute name*
that is an abstract name for the kind of thing we are drawing, e.g.
"keyword" or "e-mail address." We then provide an attribute map which
maps those attribute names to actual attributes.  This approach lets us:

* Change the attributes at runtime, letting the user change the
  attributes of any element of the application arbitrarily without
  forcing anyone to build special machinery to make this configurable;
* Write routines to load saved attribute maps from disk;
* Provide modular attribute behavior for third-party components, where
  we would not want to have to recompile third-party code just to change
  attributes, and where we would not want to have to pass in attribute
  arguments to third-party drawing functions.

This lets us put the attribute mapping for an entire app, regardless of
use of third-party widgets, in one place.

To create a map we use ``Brick.AttrMap.attrMap``, e.g.,

.. code:: haskell

   App { ...
       , appAttrMap = const $ attrMap Graphics.Vty.defAttr [(someAttrName, fg blue)]
       }

To use an attribute map, we specify the ``App`` field ``appAttrMap`` as
the function to return the current attribute map each time rendering
occurs. This function takes the current application state, so you may
choose to store the attribute map in your application state. You may
also choose not to bother with that and to just set ``appAttrMap = const
someMap``.

To draw a widget using an attribute name in the map, use
``Brick.Widgets.Core.withAttr``. For example, this draws a string with a
``blue`` background:

.. code:: haskell

   let w = withAttr blueBg $ str "foobar"
       blueBg = attrName "blueBg"
       myMap = attrMap defAttr [ (blueBg, Brick.Util.bg Graphics.Vty.blue)
                               ]

For complete details on how attribute maps and attribute names work, see
the Haddock documentation for the ``Brick.AttrMap`` module. See also
`How Attributes Work`_.

How Widgets and Rendering Work
==============================

When ``brick`` renders a ``Widget``, the widget's rendering routine is
evaluated to produce a ``vty`` ``Image`` of the widget. The widget's
rendering routine runs with some information called the *rendering
context* that contains:

* The size of the area in which to draw things
* The name of the current attribute to use to draw things
* The map of attributes to use to look up attribute names
* The active border style to use when drawing borders

Available Rendering Area
------------------------

The most important element in the rendering context is the rendering
area: This part of the context tells the widget being drawn how many
rows and columns are available for it to consume. When rendering begins,
the widget being rendered (i.e. a layer returned by an ``appDraw``
function) gets a rendering context whose rendering area is the size of
the terminal. This size information is used to let widgets take up that
space if they so choose. For example, a string "Hello, world!" will
always take up one row and 13 columns, but the string "Hello, world!"
*centered* will always take up one row and *all available columns*.

How widgets use space when rendered is described in two pieces of
information in each ``Widget``: the widget's horizontal and vertical
growth policies. These fields have type ``Brick.Types.Size`` and can
have the values ``Fixed`` and ``Greedy``. Note that these values are
merely *descriptive hints* about the behavior of the rendering function,
so it's important that they accurately describe the widget's use of
space.

A widget advertising a ``Fixed`` size in a given dimension is a widget
that will always consume the same number of rows or columns no
matter how many it is given. Widgets can advertise different
vertical and horizontal growth policies for example, the
``Brick.Widgets.Border.hCenter`` function centers a widget and is
``Greedy`` horizontally and defers to the widget it centers for vertical
growth behavior.

These size policies govern the box layout algorithm that is at
the heart of every non-trivial drawing specification. When we use
``Brick.Widgets.Core.vBox`` and ``Brick.Widgets.Core.hBox`` to
lay things out (or use their binary synonyms ``<=>`` and ``<+>``,
respectively), the box layout algorithm looks at the growth policies of
the widgets it receives to determine how to allocate the available space
to them.

For example, imagine that the terminal window is currently 10 rows high
and 50 columns wide.  We wish to render the following widget:

.. code:: haskell

   let w = (str "Hello," <=> str "World!")

Rendering this to the terminal will result in "Hello," and "World!"
underneath it, with 8 rows unoccupied by anything. But if we wished to
render a vertical border underneath those strings, we would write:

.. code:: haskell

   let w = (str "Hello," <=> str "World!" <=> vBorder)

Rendering this to the terminal will result in "Hello," and "World!"
underneath it, with 8 rows remaining occupied by vertical border
characters ("``|``") one column wide. The vertical border widget is
designed to take up however many rows it was given, but rendering the
box layout algorithm has to be careful about rendering such ``Greedy``
widgets because they won't leave room for anything else. Since the box
widget cannot know the sizes of its sub-widgets until they are rendered,
the ``Fixed`` widgets get rendered and their sizes are used to determine
how much space is left for ``Greedy`` widgets.

When using widgets it is important to understand their horizontal and
vertical space behavior by knowing their ``Size`` values. Those should
be made clear in the Haddock documentation.

The rendering context's specification of available space will also
govern how widgets get cropped, since all widgets are required to render
to an image no larger than the rendering context specifies. If they do,
they will be forcibly cropped.

Limiting Rendering Area
-----------------------

If you'd like to use a ``Greedy`` widget but want to limit how much
space it consumes, you can turn it into a ``Fixed`` widget by using
one of the *limiting combinators*, ``Brick.Widgets.Core.hLimit`` and
``Brick.Widgets.Core.vLimit``. These combinators take widgets and turn
them into widgets with a ``Fixed`` size (in the relevant dimension) and
run their rendering functions in a modified rendering context with a
restricted rendering area.

For example, the following will center a string in 30 columns, leaving
room for something to be placed next to it as the terminal width
changes:

.. code:: haskell

   let w = hLimit 30 $ hCenter $ str "Hello, world!"

The Attribute Map
-----------------

The rendering context contains an attribute map (see `How Attributes
Work`_ and `appAttrMap: Managing Attributes`_) which is used to look up
attribute names from the drawing specification. The map originates from
``Brick.Main.appAttrMap`` and can be manipulated on a per-widget basis
using ``Brick.Widgets.Core.updateAttrMap``.

The Active Border Style
-----------------------

Widgets in the ``Brick.Widgets.Border`` module draw border characters
(horizontal, vertical, and boxes) between and around other widgets. To
ensure that widgets across your application share a consistent visual
style, border widgets consult the rendering context's *active border
style*, a value of type ``Brick.Widgets.Border.Style``, to get the
characters used to draw borders.

The default border style is ``Brick.Widgets.Border.Style.unicode``. To
change border styles, use the ``Brick.Widgets.Core.withBorderStyle``
combinator to wrap a widget and change the border style it uses when
rendering. For example, this will use the ``ascii`` border style instead
of ``unicode``:

.. code:: haskell

   let w = withBorderStyle Brick.Widgets.Border.Style.ascii $
             Brick.Widgets.Border.border $ str "Hello, world!"

How Attributes Work
===================

In addition to letting us map names to attributes, attribute maps
provide hierarchical attribute inheritance: a more specific attribute
derives any properties (e.g. background color) that it does not specify
from more general attributes in hierarchical relationship to it, letting
us customize only the parts of attributes that we want to change without
having to repeat ourselves.

For example, this draws a string with a foreground color of ``white`` on
a background color of ``blue``:

.. code:: haskell

   let w = withAttr specificAttr $ str "foobar"
       generalAttr = attrName "general"
       specificAttr = attrName "general" <> attrName "specific"
       myMap = attrMap defAttr [ (generalAttr, bg blue)
                               , (specificAttr, fg white)
                               ]

Functions ``Brick.Util.fg`` and ``Brick.Util.bg`` specify
partial attributes, and map lookups start with the desired name
(``general/specific`` in this case) and walk up the name hierarchy (to
``general``), merging partial attribute settings as they go, letting
already-specified attribute settings take precedence. Finally, any
attribute settings not specified by map lookups fall back to the map's
*default attribute*, specified above as ``Graphics.Vty.defAttr``. In
this way, if you want everything in your application to have a ``blue``
background color, you only need to specify it *once*: in the attribute
map's default attribute. Any other attribute names can merely customize
the foreground color.

In addition to using the attribute map provided by ``appAttrMap``,
the map can be customized on a per-widget basis by using the attribute
map combinators:

* ``Brick.Widgets.Core.updateAttrMap``
* ``Brick.Widgets.Core.forceAttr``
* ``Brick.Widgets.Core.withDefAttr``
* ``Brick.Widgets.Core.overrideAttr``

Attribute Themes
================

Brick provides support for customizable attribute themes. This works as
follows:

* The application provides a default theme built in to the program.
* The application customizes the them by loading theme customizations
  from a user-specified customization file.
* The application can save new customizations to files for later
  re-loading.

Customizations are written in an INI-style file. Here's an example:

.. code::

   [default]
   default.fg = blue
   default.bg = black

   [other]
   someAttribute.fg = red
   someAttribute.style = underline
   otherAttribute.style = [underline, bold]
   otherAttribute.inner.fg = white

In the above example, the theme's *default attribute* -- the one that is
used when no other attributes are used -- is customized. Its foreground
and background colors are set. Then, other attributes specified by
the theme -- ``someAttribute`` and ``otherAttribute`` -- are also
customized. This example shows that styles can be customized, too, and
that a custom style can either be a single style (in this example,
``underline``) or a collection of styles to be applied simultaneously
(in this example, ``underline`` and ``bold``). Lastly, the hierarchical
attribute name ``otherAttribute.inner`` refers to an attribute name
with two components, ``otherAttribute <> inner``, similar to the
``specificAttr`` attribute described in `How Attributes Work`_. Full
documentation for the format of theme customization files can be found
in the module documentation for ``Brick.Themes``.

The above example can be used in a ``brick`` application as follows.
First, the application provides a default theme:

.. code:: haskell

   import Brick.Themes (Theme, newTheme)

   defaultTheme :: Theme
   defaultTheme =
       newTheme (white `on` blue)
                [ ("someAttribute",  fg yellow)
                , ("otherAttribute", fg magenta)
                ]

Notice that the attributes in the theme have defaults: ``someAttribute``
will default to a yellow foreground color if it is not customized. (And
its background will default to the theme's default background color,
blue, if it not customized either.) Then, the application can customize
the theme with the user's customization file:

.. code:: haskell

   import Brick.Themes (loadCustomizations)

   main :: IO ()
   main = do
       customizedTheme <- loadCustomizations "custom.ini" defaultTheme

Now we have a customized theme based on ``defaultTheme``. The next step
is to build an ``AttrMap`` from the theme:

.. code:: haskell

   import Brick.Themes (themeToAttrMap)

   main :: IO ()
   main = do
       customizedTheme <- loadCustomizations "custom.ini" defaultTheme
       let mapping = themeToAttrMap customizedTheme

The resulting ``AttrMap`` can then be returned by ``appAttrMap``
as described in `How Attributes Work`_ and `appAttrMap: Managing
Attributes`_.

If the theme is further customized at runtime, any changes can be saved
with ``Brick.Themes.saveCustomizations``.

Wide Character Support and the TextWidth class
==============================================

Brick supports rendering wide characters in all widgets, and the brick
editor supports entering and editing wide characters. Wide characters
are those such as many Asian characters and emoji that need more than
a single terminal column to be displayed. Brick relies on Vty's use of
the `utf8proc`_ library to determine the column width of each character
rendered.

As a result of supporting wide characters, it is important to know that
computing the length of a string to determine its screen width will
*only* work for single-column characters. So, for example, if you want
to support wide characters in your application, this will not work:

.. code:: haskell

   let width = Data.Text.length t

because if the string contains any wide characters, their widths
will not be counted properly. In order to get this right, use the
``TextWidth`` type class to compute the width:

.. code:: haskell

   let width = Brick.Widgets.Core.textWidth t

The ``TextWidth`` type class uses Vty's character width routine (and
thus ``utf8proc``) to compute the correct width. If you need to compute
the width of a single character, use ``Graphics.Text.wcwidth``.

Extents
=======

When an application needs to know where a particular widget was drawn by
the renderer, the application can request that the renderer record the
*extent* of the widget--its upper-left corner and size--and provide it
in an event handler. In the following example, the application needs to
know where the bordered box containing "Foo" is rendered:

.. code:: haskell

   ui = center $ border $ str "Foo"

We don't want to have to care about the particulars of the layout to
find out where the bordered box got placed during rendering. To get this
information we request that the extent of the box be reported to us by
the renderer using a resource name:

.. code:: haskell

   data Name = FooBox

   ui = center $
        reportExtent FooBox $
        border $ str "Foo"

Now, whenever the ``ui`` is rendered, the location and size of the
bordered box containing "Foo" will be recorded. We can then look it up
in event handlers in ``EventM``:

.. code:: haskell

   do
     mExtent <- Brick.Main.lookupExtent FooBox
     case mExtent of
       Nothing -> ...
       Just (Extent _ upperLeft (width, height)) -> ...

Paste Support
=============

Some terminal emulators support "bracketed paste" support. This feature
enables OS-level paste operations to send the pasted content as a
single chunk of data and bypass the usual input processing that the
application does. This enales more secure handling of pasted data since
the application can detect that a pasted occurred and avoid processing
the pasted data as ordinary keyboard input. For more information, see
`bracketed paste mode`_.

The Vty library used by brick provides support for bracketed pastes, but
this mode must be enabled. To enable paste mode, we need to get access
to the Vty library handle in ``EventM``:

.. code:: haskell

   do
     vty <- Brick.Main.getVtyHandle
     let output = outputIface vty
     when (supportsMode output BracketedPaste) $
         liftIO $ setMode output BracketedPaste True

Once enabled, paste mode will generate Vty ``EvPaste`` events. These
events will give you the entire pasted content as a ``ByteString`` which
you must decode yourself if, for example, you expect it to contain UTF-8
text data.

Mouse Support
=============

Some terminal emulators support mouse interaction. The Vty library used
by brick provides these low-level events if mouse mode has been enabled.
To enable mouse mode, we need to get access to the Vty library handle in
``EventM``:

.. code:: haskell

   do
     vty <- Brick.Main.getVtyHandle
     let output = outputIface vty
     when (supportsMode output Mouse) $
       liftIO $ setMode output Mouse True

Bear in mind that some terminals do not support mouse interaction, so
use Vty's ``getModeStatus`` to find out whether your terminal will
provide mouse events.

Also bear in mind that terminal users will usually expect to be able
to interact with your application entirely without a mouse, so if you
do choose to enable mouse interaction, consider using it to improve
existing interactions rather than provide new functionality that cannot
already be managed with a keyboard.

Low-level Mouse Events
----------------------

Once mouse events have been enabled, Vty will generate ``EvMouseDown``
and ``EvMouseUp`` events containing the mouse button clicked, the
location in the terminal, and any modifier keys pressed.

.. code:: haskell

   handleEvent s (VtyEvent (EvMouseDown col row button mods) = ...

Brick Mouse Events
------------------

Although these events may be adequate for your needs, ``brick`` provides
a higher-level mouse event interface that ties into the drawing
language. The disadvantage to the low-level interface described above is
that you still need to determine *what* was clicked, i.e., the part of
the interface that was under the mouse cursor. There are two ways to do
this with ``brick``: with *extent checking* and *click reporting*.

Extent checking
***************

The *extent checking* approach entails requesting extents (see
`Extents`_) for parts of your interface, then checking the Vty mouse
click event's coordinates against one or more extents.

The most direct way to do this is to check a specific extent:

.. code:: haskell

   handleEvent s (VtyEvent (EvMouseDown col row _ _)) = do
     mExtent <- lookupExtent SomeExtent
     case mExtent of
       Nothing -> continue s
       Just e -> do
         if Brick.Main.clickedExtent (col, row) e
           then ...
           else ...

This approach works well enough if you know which extent you're
interested in checking, but what if there are many extents and you
want to know which one was clicked? And what if those extents are in
different layers? The next approach is to find all clicked extents:

.. code:: haskell

   handleEvent s (VtyEvent (EvMouseDown col row _ _)) = do
     extents <- Brick.Main.findClickedExtents (col, row)
     -- Then check to see if a specific extent is in the list, or just
     -- take the first one in the list.

This approach finds all clicked extents and returns them in a list with
the following properties:

* For extents ``A`` and ``B``, if ``A``'s layer is higher than ``B``'s
  layer, ``A`` comes before ``B`` in the list.
* For extents ``A`` and ``B``, if ``A`` and ``B`` are in the same layer
  and ``A`` is contained within ``B``, ``A`` comes before ``B`` in the
  list.

As a result, the extents are ordered in a natural way, starting with the
most specific extents and proceeding to the most general.

Click reporting
***************

The *click reporting* approach is the most high-level approach
offered by ``brick``. When rendering the interface we use
``Brick.Widgets.Core.clickable`` to request that a given widget generate
``MouseDown`` and ``MouseUp`` events when it is clicked.

.. code:: haskell

   data Name = MyButton

   ui :: Widget Name
   ui = center $
        clickable MyButton $
        border $
        str "Click me"

   handleEvent s (MouseDown MyButton button modifiers coords) = ...
   handleEvent s (MouseUp MyButton button coords) = ...

This approach enables event handlers to use pattern matching to check
for mouse clicks on specific regions; this uses extent reporting
under the hood but makes it possible to denote which widgets are
clickable in the interface description. The event's click coordinates
are local to the widget being clicked. In the above example, a click
on the upper-left corner of the border would result in coordinates of
``(0,0)``.

Viewports
=========

A *viewport* is a scrollable window onto a widget. Viewports have a
*scrolling direction* of type ``Brick.Types.ViewportType`` which can be
one of:

* ``Horizontal``: the viewport can only scroll horizontally.
* ``Vertical``: the viewport can only scroll vertically.
* ``Both``: the viewport can scroll both horizontally and vertically.

The ``Brick.Widgets.Core.viewport`` combinator takes another widget
and embeds it in a named viewport. We name the viewport so that we can
keep track of its scrolling state in the renderer, and so that you can
make scrolling requests. The viewport's name is its handle for these
operations (see `Scrolling Viewports in Event Handlers`_ and `Resource
Names`_). **The viewport name must be unique across your application.**

For example, the following puts a string in a horizontally-scrollable
viewport:

.. code:: haskell

   -- Assuming that App uses 'Name' for its resource names:
   data Name = Viewport1
   let w = viewport Viewport1 Horizontal $ str "Hello, world!"

A ``viewport`` specification means that the widget in the viewport will
be placed in a viewport window that is ``Greedy`` in both directions
(see `Available Rendering Area`_). This is suitable if we want the
viewport size to be the size of the entire terminal window, but if
we want to limit the size of the viewport, we might use limiting
combinators (see `Limiting Rendering Area`_):

.. code:: haskell

   let w = hLimit 5 $
           vLimit 1 $
           viewport Viewport1 Horizontal $ str "Hello, world!"

Now the example produces a scrollable window one row high and five
columns wide initially showing "Hello". The next two sections discuss
the two ways in which this viewport can be scrolled.

Scrolling Viewports in Event Handlers
-------------------------------------

The most direct way to scroll a viewport is to make *scrolling requests*
in the ``EventM`` event-handling monad. Scrolling requests ask the
renderer to update the state of a viewport the next time the user
interface is rendered. Those state updates will be made with respect
to the *previous* viewport state, i.e., the state of the viewports as
of the end of the most recent rendering. This approach is the best
approach to use to scroll widgets that have no notion of a cursor.
For cursor-based scrolling, see `Scrolling Viewports With Visibility
Requests`_.

To make scrolling requests, we first create a
``Brick.Main.ViewportScroll`` from a viewport name with
``Brick.Main.viewportScroll``:

.. code:: haskell

   -- Assuming that App uses 'Name' for its resource names:
   data Name = Viewport1
   let vp = viewportScroll Viewport1

The ``ViewportScroll`` record type contains a number of scrolling
functions for making scrolling requests:

.. code:: haskell

   hScrollPage        :: Direction -> EventM n ()
   hScrollBy          :: Int       -> EventM n ()
   hScrollToBeginning ::              EventM n ()
   hScrollToEnd       ::              EventM n ()
   vScrollPage        :: Direction -> EventM n ()
   vScrollBy          :: Int       -> EventM n ()
   vScrollToBeginning ::              EventM n ()
   vScrollToEnd       ::              EventM n ()

In each case the scrolling function scrolls the viewport by the
specified amount in the specified direction; functions prefixed with
``h`` scroll horizontally and functions prefixed with ``v`` scroll
vertically.

Scrolling operations do nothing when they don't make sense for the
specified viewport; scrolling a ``Vertical`` viewport horizontally is a
no-op, for example.

Using ``viewportScroll`` we can write an event handler that scrolls the
``Viewport1`` viewport one column to the right:

.. code:: haskell

   myHandler :: s -> e -> EventM n (Next s)
   myHandler s e = do
       let vp = viewportScroll Viewport1
       hScrollBy vp 1
       continue s

Scrolling Viewports With Visibility Requests
--------------------------------------------

When we need to scroll widgets only when a cursor in the viewport
leaves the viewport's bounds, we need to use *visibility requests*. A
visibility request is a hint to the renderer that some element of a
widget inside a viewport should be made visible, i.e., that the viewport
should be scrolled to bring the requested element into view.

To use a visibility request to make a widget in a viewport visible, we
simply wrap it with ``visible``:

.. code:: haskell

   -- Assuming that App uses 'Name' for its resource names:
   data Name = Viewport1
   let w = viewport Viewport1 Horizontal $
           (visible $ str "Hello," <+> (str " world!")

This example requests that the ``Viewport1`` viewport be scrolled so
that "Hello," is visible. We could extend this example with a value
in the application state indicating which word in our string should
be visible and then use that to change which string gets wrapped with
``visible``; this is the basis of cursor-based scrolling.

Note that a visibility request does not change the state of a viewport
*if the requested widget is already visible*! This important detail is
what makes visibility requests so powerful, because they can be used to
capture various cursor-based scenarios:

* The ``Brick.Widgets.Edit`` widget uses a visibility request to make its
  1x1 cursor position visible, thus making the text editing widget fully
  scrollable *while being entirely scrolling-unaware*.
* The ``Brick.Widgets.List`` widget uses a visibility request to make
  its selected item visible regardless of its size, which makes
  the list widget scrolling-unaware.

Viewport Restrictions
---------------------

Viewports impose one restriction: a viewport that is scrollable in
some direction can only embed a widget that has a ``Fixed`` size in
that direction. This extends to ``Both`` type viewports: they can only
embed widgets that are ``Fixed`` in both directions. This restriction
is because when viewports embed a widget, they relax the rendering area
constraint in the rendering context, but doing so to a large enough
number for ``Greedy`` widgets would result in a widget that is too big
and not scrollable in a useful way.

Violating this restriction will result in a runtime exception.

The Rendering Cache
===================

When widgets become expensive to render, ``brick`` provides a *rendering
cache* that automatically caches and re-uses stored Vty images from
previous renderings to avoid expensive renderings. To cache the
rendering of a widget, just wrap it in the ``Brick.Widgets.Core.cached``
function:

.. code:: haskell

   data Name = ExpensiveThing

   ui :: Widget Name
   ui = center $
        cached ExpensiveThing $
        border $
        str "This will be cached"

In the example above, the first time the ``border $ str "This will be
cached"`` widget is rendered, the resulting Vty image will be stored
in the rendering cache under the key ``ExpensiveThing``. On subsequent
renderings the cached Vty image will be used instead of re-rendering the
widget. This example doesn't need caching to improve performance, but
more sophisticated widgets might.

Once ``cached`` has been used to store something in the rendering cache,
periodic cache invalidation may be required. For example, if the cached
widget is built from application state, the cache will need to be
invalidated when the relevant state changes. The cache may also need to
be invalidated when the terminal is resized. To invalidate the cache, we
use the cache invalidation functions in ``EventM``:

.. code:: haskell

   handleEvent s ... = do
     -- Invalidate just a single cache entry:
     Brick.Main.invalidateCacheEntry ExpensiveThing

     -- Invalidate the entire cache (useful on a resize):
     Brick.Main.invalidateCache

Implementing Custom Widgets
===========================

``brick`` exposes all of the internals you need to implement your
own widgets. Those internals, together with ``Graphics.Vty``, can be
used to create widgets from the ground up. You'll need to implement
your own widget if you can't write what you need in terms of existing
combinators. For example, an ordinary widget like

.. code:: haskell

   myWidget :: Widget n
   myWidget = str "Above" <=> str "Below"

can be expressed with ``<=>`` and ``str`` and needs no custom behavior.
But suppose we want to write a widget that renders some string followed
by the number of columns in the space available to the widget. We can't
do this without writing a custom widget because we need access to the
rendering context. We can write such a widget as follows:

.. code:: haskell

   customWidget :: String -> Widget n
   customWidget s =
       Widget Fixed Fixed $ do
           ctx <- getContext
           render $ str (s <> " " <> show (ctx^.availWidthL))

The ``Widget`` constructor takes the horizontal and vertical growth
policies as described in `How Widgets and Rendering Work`_. Here we just
provide ``Fixed`` for both because the widget will not change behavior
if we give it more space. We then get the rendering context and append
the context's available columns to the provided string. Lastly we call
``render`` to render the widget we made with ``str``. The ``render``
function returns a ``Brick.Types.Result`` value:

.. code:: haskell

    data Result n =
        Result { image              :: Graphics.Vty.Image
               , cursors            :: [Brick.Types.CursorLocation n]
               , visibilityRequests :: [Brick.Types.VisibilityRequest]
               , extents            :: [Extent n]
               }

The rendering function runs in the ``RenderM`` monad, which gives us
access to the rendering context (see `How Widgets and Rendering Work`_)
via the ``Brick.Types.getContext`` function as shown above. The context
tells us about the dimensions of the rendering area and the current
attribute state of the renderer, among other things:

.. code:: haskell

    data Context =
        Context { ctxAttrName    :: AttrName
                , availWidth     :: Int
                , availHeight    :: Int
                , ctxBorderStyle :: BorderStyle
                , ctxAttrMap     :: AttrMap
                }

and has lens fields exported as described in `Conventions`_.

As shown here, the job of the rendering function is to return a
rendering result which means producing a ``vty`` ``Image``. In addition,
if you so choose, you can also return one or more cursor positions in
the ``cursors`` field of the ``Result`` as well as visibility requests
(see `Viewports`_) in the ``visibilityRequests`` field. Returned
visibility requests and cursor positions should be relative to the
upper-left corner of your widget, ``Location (0, 0)``. When your widget
is placed in others, such as boxes, the ``Result`` data you returned
will be offset (as described in `Rendering Sub-Widgets`_) to result in
correct coordinates once the entire interface has been rendered.

Using the Rendering Context
---------------------------

The most important fields of the context are the rendering area fields
``availWidth`` and ``availHeight``. These fields must be used to
determine how much space your widget has to render.

To perform an attribute lookup in the attribute map for the context's
current attribute, use ``Brick.Types.attrL``.

For example, to build a widget that always fills the available width and
height with a fill character using the current attribute, we could
write:

.. code:: haskell

   myFill :: Char -> Widget n
   myFill ch =
       Widget Greedy Greedy $ do
           ctx <- getContext
           let a = ctx^.attrL
           return $ Result (Graphics.Vty.charFill a ch (ctx^.availWidthL) (ctx^.availHeightL))
                           [] []

Rendering Sub-Widgets
---------------------

If your custom widget wraps another, then in addition to rendering
the wrapped widget and augmenting its returned ``Result`` *it must
also translate the resulting cursor locations, visibility requests,
and extents*. This is vital to maintaining the correctness of
rendering metadata as widget layout proceeds. To do so, use the
``Brick.Widgets.Core.addResultOffset`` function to offset the elements
of a ``Result`` by a specified amount. The amount depends on the nature
of the offset introduced by your wrapper widget's logic.

Widgets are not required to respect the rendering context's width and
height restrictions. Widgets may be embedded in viewports or translated
so they must render without cropping to work in those scenarios.
However, widgets rendering other widgets *should* enforce the rendering
context's constraints to avoid using more space than is available. The
``Brick.Widgets.Core.cropToContext`` function is provided to make this
easy:

.. code:: haskell

   let w = cropToContext someWidget

Widgets wrapped with ``cropToContext`` can be safely embedded in other
widgets. If you don't want to crop in this way, you can use any of
``vty``'s cropping functions to operate on the ``Result`` image as
desired.

Sub-widgets may specify specific attribute name values influencing
that sub-widget.  If the custom widget utilizes its own attribute
names but needs to render the sub-widget, it can use ``overrideAttr``
or ``mapAttrNames`` to convert its custom names to the names that the
sub-widget uses for rendering its output.

.. _vty: https://github.com/coreyoconnor/vty
.. _Hackage: http://hackage.haskell.org/
.. _microlens: http://hackage.haskell.org/package/microlens
.. _bracketed paste mode: https://cirw.in/blog/bracketed-paste
.. _utf8proc: http://julialang.org/utf8proc/
