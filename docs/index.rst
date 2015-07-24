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

The process of writing an application using ``brick`` entails writing
two important functions:

 * A *drawing function* that turns your application state into a
   specification of how your interface should look, and
 * An *event handler* that takes your application state and an input
   event and decides whether to change the state or quit the program.

We write drawing functions in ``brick`` using an extensive set of
primitives and combinators to place text on the screen, set its
attributes (e.g. foreground color), and express layout constraints (e.g.
padding, centering, box layouts, scrolling viewports, etc.).

These functions get packaged into a structure that we hand off to the
``brick`` library's main event loop.

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

 * Use of `lens`_: ``brick`` uses ``lens`` functions internally and also
   exposes lenses for many types in the library. However, if you
   prefer not to use the ``lens`` interface in your program, all
   ``lens`` interfaces have non-`lens` equivalents exported by the
   same module. In general, the "``L``" suffix on something tells
   you it is a ``lens``; the name without the "``L``" suffix is the
   non-`lens` version. You can get by without using ``brick``'s ``lens``
   interface but your life will probably be much more pleasant once your
   application state becomes sufficiently complex (see `appHandleEvent:
   Handling Events`_).
 * Attribute names: some modules export attribute names (see `How
   Attributes Work`_) associated with user interface elements.
   These tend to end in an "``Attr``" suffix (e.g. ``borderAttr``).
   In addition, hierarchical relationships between attributes are
   documented in Haddock documentation.

The App Type
============

To use the library we must provide it with a value of type
``Brick.Main.App``. This type is a record type whose fields perform
various functions:

.. code:: haskell
   :linenos:

   data App s e =
       App { appDraw :: s -> [Widget]
           , appChooseCursor :: s -> [CursorLocation] -> Maybe CursorLocation
           , appHandleEvent :: s -> e -> EventM (Next s)
           , appStartEvent :: s -> EventM s
           , appAttrMap :: s -> AttrMap
           , appLiftVtyEvent :: Event -> e
           }

The various fields of ``App`` will be described in the sections below.

Once we have declared an ``App``, we can pass it to
``Brick.Main.defaultMain`` or ``Brick.Main.customMain`` to begin running
the application.

appDraw: Drawing an Interface
-----------------------------

appHandleEvent: Handling Events
-------------------------------

Suspend & Resume, Halt, Continue
********************************

Using Your Own Event Type
*************************

appLiftVtyEvent

Starting up: appStartEvent
**************************

appChooseCursor: Placing the Cursor
-----------------------------------

appAttrMap: Providing Attributes
--------------------------------

How Widgets and Rendering Work
==============================

How Attributs Work
==================

Implementing Your Own Widgets
=============================

Using the Rendering Context
---------------------------

Rendering Sub-Widgets
---------------------

.. _vty: https://github.com/coreyoconnor/vty
.. _Hackage: http://hackage.haskell.org/
.. _lens: http://hackage.haskell.org/package/lens
