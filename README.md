brick
-----

`brick` is a terminal user interface programming
library written in Haskell, in the style of
[gloss](http://hackage.haskell.org/package/gloss). This means you write
a function that describes how your user interface should look, but the
library takes care of a lot of the book-keeping that so commonly goes
into writing such programs.

The API exposed by `brick` is declarative. Unlike most GUI toolkits
which require you to write a long and tedious sequence of "create
a widget, now bind an event handler", `brick` just requires you to
describe your interface -- even the bits that are stateful -- using a
set of declarative combinators and it does the rest. All you have to do
is provide functions to transform your own application state when input
(or other kinds of) events arrive.

Under the hood, this library uses [vty](http://hackage.haskell.org/package/vty).

This library deprecates [vty-ui](https://github.com/jtdaugherty/vty-ui).
Some day `brick`, too, will have a [70-page
manual](http://jtdaugherty.github.io/vty-ui/manuals/vty-ui-users-manual-1.9.pdf).

Getting Started
---------------

The best way to get started is to build, run, and read the source for
the various demonstration programs in the `programs/` directory. This
will help you get to know the library and what it can do. There is also
extensive Haddock documentation.

```
$ cabal sandbox init
$ cabal install -j
$ .cabal-sandbox/bin/brick-???-demo
```

Status
------

`brick` is experimental. It does not yet support many of the features
of, say, `vty-ui`. And there are some places were I have deliberately
chosen to worry about performance later, for the sake of spending more
time on the design. For a while my goal with `brick` will be to develop
a very solid core library with minimal features. It *should* be possible
to extend this library by making your own packages that depend on
`brick`. If you do that, you'll also be helping me by testing whether
the exported interface is usable!

There is a lot that I haven't documented in terms of design and intended
API usage, but some of that can be gleaned from the demo program source
and by looking at the implementation of the widgets that are already
provided.

The development of this library has also revealed some bugs in `vty`,
and I've tried to report those as I go. If they haven't been resolved,
you'll see them arise when using `brick`.

Contributing
------------

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, talk to me first; let's have a
   design/vision discussion before you start coding. Create a GitHub
   issue and we can use that as the place to hash things out.
 - If you make changes, try to make them consistent with the syntactic
   conventions I've used in the codebase.
 - Please provide Haddock documentation for any new functions you add.
