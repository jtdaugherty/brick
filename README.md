brick
-----

[![Build Status](https://travis-ci.org/jtdaugherty/brick.svg?branch=master)](https://travis-ci.org/jtdaugherty/brick)

`brick` is a Haskell terminal user interface programming library in the
style of [gloss](http://hackage.haskell.org/package/gloss). This means
you write a function that describes how your user interface should look,
but the library takes care of a lot of the book-keeping that so commonly
goes into writing such programs.

`brick` exposes a declarative API. Unlike most GUI toolkits which
require you to write a long and tedious sequence of "create a widget,
now bind an event handler", `brick` just requires you to describe your
interface using a set of declarative combinators. Then you provide a
function to transform your application state when input or other kinds
of events arrive.

Under the hood, this library builds upon
[vty](http://hackage.haskell.org/package/vty), so some knowledge of Vty
will be helpful in using this library.

This library deprecates [vty-ui](https://github.com/jtdaugherty/vty-ui).

Example
-------

Here's an example interface (see `programs/ReadmeDemo.hs`):

```
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
└────────────────────────┘
```

Getting Started
---------------

TLDR:

```
$ cabal sandbox init
$ cabal install -j -f demos
$ .cabal-sandbox/bin/brick-???-demo
```

To get started, see the [first few sections of the brick
user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst).

Feature Overview
----------------

`brick` comes with a bunch of widget types to get you started:

 * Vertical and horizontal box layout widgets
 * Basic single- and multi-line text editor widgets
 * List widget
 * Progress bar widget
 * Simple dialog box widget
 * Border-drawing widgets (put borders around or in between things)
 * Generic scrollable viewports
 * Extensible widget-building API
 * (And many more general-purpose layout control combinators)

In addition, some of `brick`'s more powerful features may not be obvious
right away:

 * All widgets can be arranged in predictable layouts so you don't have
   to worry about terminal resizes.
 * Attribute management is flexible and can be customized at runtime on
   a per-widget basis.

Brick-Users Discussion
----------------------

The `brick-users` Google Group / e-mail list is a place to discuss
library changes, give feedback, and ask questions. You can subscribe at:

[https://groups.google.com/group/brick-users](https://groups.google.com/group/brick-users)

Documentation
-------------

Your documentation options, in recommended order, are:

* [FAQ](https://github.com/jtdaugherty/brick/blob/master/FAQ.md)
* [The brick user guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst)
* Haddock (all modules)
* [Demo programs](https://github.com/jtdaugherty/brick/blob/master/programs)

Status
------

`brick` is young and may be missing some essential features. There are
some places were I have deliberately chosen to worry about performance
later for the sake of spending more time on the design (and to wait on
performance issues to arise first). `brick` is also something of an
experimental project of mine and some aspects of the design involve
trade-offs that are not entirely settled. In addition you can expect
this library to follow a principle of fearless improvement: new versions
will make (sometimes substantial) API changes if those changes really do
make the library better. I will place more importance on getting the API
right than on maintaining backwards compatibility.

`brick` exports an extension API that makes it possible to make your own
packages and widgets. If you use that, you'll also be helping to test
whether the exported interface is usable and complete!

Reporting bugs
--------------

Please file bug reports as GitHub issues.  For best results:

 - Include the versions of relevant software packages: your terminal
   emulator, `brick`, `ghc`, and `vty` will be the most important
   ones.

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
