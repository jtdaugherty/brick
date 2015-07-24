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

API Conventions
===============

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
we will handle with ``brick`` applications. The ``defaultMain`` function
expects an ``App s Event`` since this is a common case.

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
  ``vBox`` or ``center`` use the size of the terminal to determine how
  to lay other widgets out. See `How Widgets and Rendering Work`_.
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

appLiftVtyEvent

customMain

Starting up: appStartEvent
**************************

appChooseCursor: Placing the Cursor
-----------------------------------

appAttrMap: Providing Attributes
--------------------------------

How Widgets and Rendering Work
==============================

How Attributes Work
===================

Implementing Your Own Widgets
=============================

Using the Rendering Context
---------------------------

Rendering Sub-Widgets
---------------------

Viewports
=========

Drawing Viewports
-----------------

Scrolling Viewports With Visibility Requests
--------------------------------------------

Scrolling Viewports in Event Handlers
-------------------------------------

.. _vty: https://github.com/coreyoconnor/vty
.. _Hackage: http://hackage.haskell.org/
.. _lens: http://hackage.haskell.org/package/lens
