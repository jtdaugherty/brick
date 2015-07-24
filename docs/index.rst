brick documentation
~~~~~~~~~~~~~~~~~~~

.. contents:: `Table of Contents`

Introduction
============

`brick` is a Haskell library for programming terminal user interfaces.
Its main goal is to make terminal user interface development as painless
and as direct as possible. `brick` builds on `vty`_; `vty` provides
the terminal input and output interface and drawing primitives, while
`brick` builds on those to provide a high-level application abstraction
and combinators for expressing user interface layouts.

Installation
------------

`brick` can be installed in the "usual way," either by installing the
latest `Hackage`_ release or by cloning the GitHub repository and
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

`brick` includes a large collection of feature-specific demonstration
programs. These programs are not built by default but can be built by
passing the `demos` flag to `cabal install`, e.g.::

   $ cabal install brick -f demos

API Conventions
===============

customMain and the App Type
===========================

Event handlers and HandleEvent
------------------------------

Suspend & Resume, Halt, Continue
--------------------------------

Cursor Selection
----------------

Using Your Own Event Type
-------------------------

How Widgets and Rendering Work
==============================

Implementing Your Own Widgets
=============================

Rendering Sub-Widgets
---------------------

Using the Rendering Context
---------------------------

.. _vty: https://github.com/coreyoconnor/vty
.. _Hackage: http://hackage.haskell.org/
