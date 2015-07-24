brick documentation
~~~~~~~~~~~~~~~~~~~

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
passing the ``demos`` flag to `cabal install`, e.g.::

   $ cabal install brick -f demos

Conventions
===========

``brick`` has some API conventions worth knowing about as you read this
documentation and as you explore the library source and write your own
programs.

- Use of `lens`_: ``brick`` uses ``lens`` functions internally and also
  exposes lenses for many types in the library. However, if you prefer
  not to use the ``lens`` interface in your program, all ``lens``
  interfaces have non-`lens` equivalents exported by the same module. In
  general, the "``L``" suffix on something tells you it is a ``lens``;
  the name without the "``L``" suffix is the non-`lens` version. You can
  get by without using ``brick``'s ``lens`` interface but your life will
  probably be much more pleasant once your application state becomes
  sufficiently complex if you use lenses to modify it (see
  `appHandleEvent: Handling Events`_).
- Attribute names: some modules export attribute names (see `How
  Attributes Work`_) associated with user interface elements. These tend
  to end in an "``Attr``" suffix (e.g. ``borderAttr``). In addition,
  hierarchical relationships between attributes are documented in
  Haddock documentation.
- Use of qualified names: in this document, where sensible, I will use
  fully-qualified names whenever I mention something for the first time
  or whenever I use something that is not part of ``brick``. Use of
  names in this way is not intended to produce executable examples, but
  rather to guide you in writing your ``import`` statements.

The App Type
============

To use the library we must provide it with a value of type
``Brick.Main.App``. This type is a record type whose fields perform
various functions:

.. code:: haskell

   data App s e =
       App { appDraw :: s -> [Widget]
           , appChooseCursor :: s -> [CursorLocation] -> Maybe CursorLocation
           , appHandleEvent :: s -> e -> EventM (Next s)
           , appStartEvent :: s -> EventM s
           , appAttrMap :: s -> AttrMap
           , appLiftVtyEvent :: Event -> e
           }

The ``App`` type is polymorphic over two types: your application state
type ``s`` and event type ``e``.

The application state type is the type of data that will evolve over the
course of the application's execution; we will provide the library with
its starting value and event handling will transform it as the program
executes.

The event type is the type of events that your event handler
(``appHandleEvent``) will handle. The underlying ``vty`` library
provides ``Graphics.Vty.Event`` and this forms the basis of all events
we will handle with ``brick`` applications. The
``Brick.Main.defaultMain`` function expects an ``App s Event`` since
this is a common case.

However, we often need to extend our notion of events beyond those
originating from the keyboard. Imagine an application with multiple
threads and network or disk I/O. Such an application will need to have
its own internal events to pass to the event handler as (for example)
network data arrives. To accommodate this we allow an ``App`` to use an
event type of your own design, so long as it provides a constructor for
``vty``'s ``Event`` type (``appLiftVtyEvent``). For more details, see
`Using Your Own Event Type`_.

The various fields of ``App`` will be described in the sections below.

To run an ``App``, we pass it to ``Brick.Main.defaultMain`` or
``Brick.Main.customMain`` along with an initial application state value.

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
the application state as a result of an event. It also decides whether
to continue program execution. The function takes the current
application state and the event and returns the *next application
state*:

.. code:: haskell

   appHandleEvent :: s -> e -> EventM (Next s)

The ``EventM`` monad is the event-handling monad. This monad is a
transformer around ``IO``, so you are free to do I/O in this monad by
using ``liftIO``. Beyond I/O, this monad is just used to make scrolling
requests to the renderer (see `Viewports`_). Keep in mind that time
spent blocking in your event handler is time during which your UI is
unresponsive, so consider this when deciding whether to have background
threads do work instead of inlining the work in the event handler.

The ``Next s`` value describes what should happen after the event
handler is finished. We have three choices:

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

Using Your Own Event Type
*************************

Since we often need to communicate application-specific events
beyond input events to the event handler, the ``App`` type is
polymorphic over the event type we want to handle. If we use
``Brick.Main.defaultMain`` to run our ``App``, we have to use
``Graphics.Vty.Event`` as our event type. But if our application has
other event-handling needs, we need to use our own event type.

To do this, we first define an event type:

.. code:: haskell

   data CustomEvent =
       VtyEvent Graphics.Vty.Event
       | CustomEvent1
       | CustomEvent2

Our custom event type *must* provide a constructor capable of taking
a ``Graphics.Vty.Event`` value. This allows the ``brick`` event loop
to send us ``vty`` events in the midst of our custom ones. To allow
``brick`` to do this, we provide this constructor as the value of
``appLiftVtyEvent``. This way, ``brick`` can wrap a ``vty`` event using
our custom event type and then pass it to our event handler (which takes
``CustomEvent`` values). In this case we'd set ``appLiftVtyEvent =
VtyEvent``.

Once we have set ``appLiftVtyEvent`` in this way, we also need to set up
a mechanism for getting our custom events into the ``brick`` event loop
from other threads. To do this we use a ``Control.Concurrent.Chan`` and
call ``Brick.Main.customMain`` instead of ``Brick.Main.defaultMain``:

.. code:: haskell

   main :: IO ()
   main = do
       eventChan <- Control.Concurrent.newChan
       finalState <- customMain (Graphics.Vty.mkVty Data.Default.def) eventChan app initialState
       -- Use finalState and exit

Beyond just the application and its initial state, the ``customMain``
function lets us have control over how the ``vty`` library is
initialized and how ``brick`` gets custom events to give to our event
handler. ``customMain`` is the entry point into ``brick`` when you need
to use your own event type.

Starting up: appStartEvent
**************************

When an application starts, it may be desirable to perform some of
the duties typically only possible when an event has arrived, such as
setting up initial scrolling viewport state. Since such actions can only
be performed in ``EventM`` and since we do not want to wait until the
first event arrives to do this work in ``appHandleEvent``, the ``App``
type provides ``appStartEvent`` function for this purpose:

.. code:: haskell

   appStartEvent :: s -> EventM s

This function takes the initial application state and returns it in
``EventM``, possibly changing it and possibly making viewport requests.
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

   appChooseCursor :: s -> [CursorLocation] -> Maybe CursorLocation

The event loop renders the interface and collects the
``Brick.Types.CursorLocation`` values produced by the rendering process
and passes those, along with the current application state, to this
function. Using your application state (to track which text input box
is "focused," say) you can decide which of the locations to return or
return ``Nothing`` if you do not want to show a cursor.

We decide which location to show by looking at the ``Brick.Types.Name``
value contained in the ``cursorLocationName`` field. The ``Name``
value associated with a cursor location will be the ``Name`` of the
``Widget`` that requested it; this is why constructors for widgets like
``Brick.Widgets.Edit.editor`` require a ``Name`` parameter. The ``Name``
lets us distinguish between many cursor-placing widgets of the same
type.

``Brick.Main`` provides various convenience functions to make cursor
selection easy in common cases:

* ``neverShowCursor``: never show any cursor.
* ``showFirstCursor``: always show the first cursor request given; good
  for applications with only one cursor-placing widget.
* ``showCursorNamed``: show the cursor with the specified name or
  ``Nothing`` if it is not requested.

Widgets request cursor placement by using the
``Brick.Widgets.Core.showCursor`` combinator. For example, this widget
places a cursor on the first "``o``" in "``foo``" assocated with the
cursor name "``myCursor``":

.. code:: haskell

   let w = showCursor (Name "myCursor") (Brick.Types.Location (1, 0))
             (Brick.Widgets.Core.str "foobar")

appAttrMap: Managing Attributes
-------------------------------

In ``brick`` we use an *attribute map* to assign attibutes to elements
of the interface. Rather than specifying specific attributes when
drawing a widget (e.g. red-on-black text) we specify an *attribute name*
that is an abstract name for the kind of thing we are drawing, e.g.
"keyword" or "e-mail address." We then provide an attribute map which
maps those attribute names to actual attributes.  This approach lets us:

* Change the attributes at runtime, letting us or the user change the
  attributes of any element of the application arbitrarily without
  aforcing nyone to build special machinery to make this configurable;
* Load saved attribute maps from disk, giving us serializable attribute
  configurations more or less for free;
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
growth policies. These fields have type ``Brick.Widgets.Core.Size`` and
can have the values ``Fixed`` and ``Greedy``.

A widget advertising a ``Fixed`` size in a given dimension is a widget
that will always consume the same number of rows or columns no matter
how many it is given. Widgets can advertise different growth policies;
for example, the ``Brick.Widgets.Border.hCenter`` function centers
a widget and is ``Greedy`` horizontally and defers to the widget it
centers for vertical growth behavior.

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
widgets because the won't leave room for anything else. Instead, the
box widget saves the ``Greedy`` widgets for last after rendering
the ``Fixed`` ones to prevent this from happening. In this way the
``Greedy`` ones are elastic, taking up the space left after rendering
the widgets that have a fixed size.

(Note that when we say "before" and "after" here, we don't mean visual
order; the ``vBorder`` above comes after the strings visually, but the
widget rendering order depends on size policies even though the final
widgets get reordered visually to match their original layout. Rendering
order here refers to the order in which we steadily consume available
space in the rendering context by rendering sub-widgets.)

When using widgets it is sometimes important to understand their
horizontal and vertical space behavior by knowing their ``Size`` values,
and those should be made clear in the Haddock documentation.

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
Work`_ and `Attributes`_) which is used to look up attribute names from
the drawing specification. The map originates from
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

Viewports
=========

A *viewport* is a scrollable window onto another widget. Viewports have
a *scrolling direction* which can be one of:

* ``Horizontal``: the viewport can only scroll horizontally.
* ``Vertical``: the viewport can only scroll vertically.
* ``Both``: the viewport can scroll both horizontally and vertically.

The ``Brick.Widgets.Core.viewport`` combinator takes another widget and
embeds it in a named viewport. We name the viewport so that we can
keep track of its scrolling state in the renderer, and so that you can
make scrolling requests. The viewport's name is its handle for these
operations (see `Scrolling Viewports in Event Handlers`_).

For example, the following puts a string in a horizontally-scrollable
viewport:

.. code:: haskell

   let w = viewport (Name "myViewport") Horizontal $ str "Hello, world!"

The above example is incomplete. A ``viewport`` specification means that
the widget in the viewport will be placed in a viewport window that is
``Greedy`` in both directions (see `Available Rendering Area`_). This
is suitable if we want the viewport size to be the size of the entire
terminal window, but if we want to embed this scrollable viewport
somewhere in our interface, we want to control its dimensions. To do so,
we use the limiting combinators (see `Limiting Rendering Area`_):

.. code:: haskell

   let w = hLimit 5 $
           vLimit 1 $
           viewport (Name "myViewport") Horizontal $ str "Hello, world!"

Now the example produces a scrollable window one row high and five
columns wide initially showing "Hello". The next two sections discuss
the two ways in which this viewport can be scrolled.

Scrolling Viewports in Event Handlers
-------------------------------------

The most direct way to scroll a viewport is to make *scrolling requests*
in the ``EventM`` event-handling monad. Scrolling requests ask the
render to update the state of a viewport the next time the user
interface is rendered. Those state updates will be made with respect to
the *previous* viewport state. This approach is the best approach to use
to scroll widgets that have no notion of a cursor. For cursor-based
scrolling, see `Scrolling Viewports With Visibility Requests`_.

To make scrolling requests, we first create a
``Brick.Main.ViewportScroll`` from a viewport name with
``Brick.Main.viewportScroll``:

.. code:: haskell

   let vp = viewportScroll (Name "myViewport")

The ``ViewportScroll`` record type contains a number of scrolling
functions for making scrolling requests:

.. code:: haskell

   hScrollPage :: Direction -> EventM ()
   hScrollBy :: Int -> EventM ()
   hScrollToBeginning :: EventM ()
   hScrollToEnd :: EventM ()
   vScrollPage :: Direction -> EventM ()
   vScrollBy :: Int -> EventM ()
   vScrollToBeginning :: EventM ()
   vScrollToEnd :: EventM ()

In each case the scrolling function scrolls the viewport by the
specified amount in the specified direction; functions prefixed with
``h`` scroll horizontally and functions prefixed with ``v`` scroll
vertically.

Scrolling operations do nothing when they don't make sense for the
specified viewport; scrolling a ``Vertical`` viewport horizontally is a
no-op, for example.

Using ``viewportScroll`` and the ``myViewport`` example given above, we
can write an event handler that scrolls the "Hello, world!" viewport one
column to the right:

.. code:: haskell

   myHandler :: s -> e -> EventM (Next s)
   myHandler s e = do
       let vp = viewportScroll (Name "myViewport")
       hScrollBy vp 1
       continue s

Scrolling Viewports With Visibility Requests
--------------------------------------------

When we need to scroll widgets only when a cursor in the viewport leaves
the viewport's bounds, we need to use *visibility requests*. A
visibility request is a hint to the renderer that some element of a
widget inside a viewport should be made visible, i.e., that the viewport
should be scrolled to bring the requested element into view.

To use a visibility request to make a widget in a viewport visible, we
simply wrap it with ``visible``:

.. code:: haskell

   let w = viewport (Name "myViewport") Horizontal $
           (visible $ str "Hello," <+> (str " world!")

This example requests that the "``myViewport``" viewport be scrolled so
that "Hello," is visible. We could extend this example with a value
in the application state indicating which word in our string should
be visible and then use that to change which string gets wrapped with
``visible``; this is the basis of cursor-based scrolling.

Note that a visibility request does not change the state of a viewport if
the requested widget is already visible! This important detail is what
makes visibility requests so powerful, because they can be used to
capture various cursor-based scenarios:

* The ``Brick.Widgets.Edit`` widget uses a visibility request to make its
  1x1 cursor position visible, thus making the text editing widget fully
  scrollable *while being entirely scrolling-unware*.
* The ``Brick.Widgets.List`` widget uses a visibility request to make
  its selected item visible regardless of its size, which makes the list
  widget both scrolling-unware and also makes it support variable-height
  items for free.

Implementing Your Own Widgets
=============================

Using the Rendering Context
---------------------------

Rendering Sub-Widgets
---------------------

.. _vty: https://github.com/coreyoconnor/vty
.. _Hackage: http://hackage.haskell.org/
.. _lens: http://hackage.haskell.org/package/lens
