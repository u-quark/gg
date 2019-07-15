Useful links
============

* https://askubuntu.com/questions/528928/how-to-do-underline-bold-italic-strikethrough-color-background-and-size-i
* https://gist.github.com/XVilka/8346728

Notes
=====

To compile with debugging enabled:

`stack build --ghc-options "-g -debug"`

To debug the GHC runtime (scheduler, gc, capabilities):

`+RTS -N -Dg -DG -Ds -Di -RTS`

To limit the size of the gc allocation area (make gc run more often):

`+RTS -A51k -RTS`
