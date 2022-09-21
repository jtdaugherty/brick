brick FAQ
=========

* Q: Why doesn't brick get my Ctrl-arrow key events?
* A: Your terminal emulator probably isn't sending the expected
  sequences. For example, OS X's Terminal.app does not do this by
  default and requires configuration to make it work. See also:

  http://unix.stackexchange.com/questions/110022/how-do-i-find-out-the-keycodes-for-ctrlup-and-down-arrow-for-term-screen

* Q: Why do some emojis mess up the layout?
* A: For wide characters to be displayed correctly, [vty]'s
  determination of the character width and the user's
  terminal emulator's determination of the character width
  must match. Unfortunately, every terminal emulator
  calculates this differently, and none correctly follow
  the Unicode standard.
  The issue is further complicated by Unicode combining
  characters and releases of new versions of the Unicode
  standard. 

  As a result, the current recommendation is to avoid
  use of wide characters due to these issues.
  If you still must use them, you can read [vty]'s
  documentation for options that will affect character
  width calculations.


[vty]: https://hackage.haskell.org/package/vty
