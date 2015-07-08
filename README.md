brick
-----

`brick` is a terminal user interface programming
library written in Haskell, in the style of
[gloss](http://hackage.haskell.org/package/gloss). This means you write
a function that describes how your user interface should look, but the
library takes care of a lot of the book-keeping that so commonly goes
into writing such programs.

The API exposed by `brick` is purely functional. Unlike most GUI
toolkits which require you to write a long and tedious sequence of
"create a widget, now bind an event handler", `brick` just requires you
to describe your interface -- even the bits that are stateful -- using
a set of declarative combinators and it does the rest. All you have to
do is provide functions to do transform your own application state when
input (or other kinds of) events arrive.

The best way to get started is to build, run, and read the source for
the various demonstration programs in the `programs/` directory. This
will help you get to know the library and what it can do. There is also
extensive Haddock documentation.
