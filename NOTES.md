Useful links
============

* https://askubuntu.com/questions/528928/how-to-do-underline-bold-italic-strikethrough-color-background-and-size-i
* https://gist.github.com/XVilka/8346728

Debugging
---------

* https://gitlab.haskell.org/ghc/ghc/wikis/debugging/compiled-code

Notes
=====

To compile with debugging enabled:

`stack build --ghc-options "-g -debug"`

To debug the GHC runtime (scheduler, gc, capabilities):

`+RTS -N -Dg -DG -Ds -Di -RTS`

To limit the size of the gc allocation area (make gc run more often):

`+RTS -A51k -RTS`

Alternatives
============

https://mystor.github.io/git-revise.html
https://git-cola.github.io/


Reduce executable size
======================

https://stackoverflow.com/questions/6687630/how-to-remove-unused-c-c-symbols-with-gcc-and-ld
https://stackoverflow.com/questions/6771905/how-to-decrease-the-size-of-generated-binaries

Haskell libraries
=================

http://hackage.haskell.org/package/gdiff
http://hackage.haskell.org/package/gdiff-th
http://hackage.haskell.org/package/aeson-diff
http://hackage.haskell.org/package/hspec-expectations-pretty-diff

Wcwidth problems
================

https://gitlab.freedesktop.org/terminal-wg/specifications/issues/9
https://stackoverflow.com/questions/52931146/unicode-formatting
https://sourceware.org/git/?p=glibc.git;a=blob;f=wctype/wchar-lookup.h;h=a16e6ca4e179cbf610f9dae2065e615460345b00;hb=HEAD
http://git.musl-libc.org/cgit/musl/tree/src/ctype/wcwidth.c?id=9b2921bea1d5017832e1b45d1fd64220047a9802
https://github.com/richfelker/musl-chartable-tools/tree/master/ctype
https://github.com/jquast/wcwidth/blob/master/wcwidth/wcwidth.py
https://github.com/kovidgoyal/kitty/blob/master/kitty/wcwidth-std.h

