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

and with nix (package.yaml):

```
library:
  source-dirs: src
  ghc-options:
  - -Wall
  - -g
  - -debug
  - -dcore-lint
```

To build with nix (using workers):

`nix-build --show-trace --max-jobs 0`

To debug the GHC runtime (scheduler, gc, capabilities):

`+RTS -N -Dg -DG -Ds -Di -RTS`

To limit the size of the gc allocation area (make gc run more often):

`+RTS -A51k -RTS`

To get the binary before it is striped from it's debugging information in nix:

 * Make the build fail (gg.nix):
    postBuild = "false";
 * Call nix with:
    > NIX_PATH='' nix-build -K --show-trace
   and it will keep and print the build directory.

To inspect a derivation:

```
> nix repl
nix-repl> d = import ./nix/default.nix { inputs = (import ./default.nix).inputs; }
nix-repl> d.unmodified_static-haskell.haskellPackages.callPackage
```

alternatively:

`nix repl --extra-experimental-features repl-flake .#`

To debug a derivation:

`nix show-derivation /nix/store/hngwpk25qnj35r6pkdb3zy5mnhlrfzrf-test-0.1.0.0.drv`

To update a flake input:

`nix flake lock --update-input vty`

To push to cachix cache:

`nix-build --show-trace --max-jobs 0 | cachix push gg`

alternatively:

`nix-store -qR --include-outputs $(nix-store -qd $(nix-build --show-trace --max-jobs 0)) | grep -v '\.drv$' | sort | uniq | cachix push gg`

To run a development shell:

`nix develop`

To run the e2e tests locally outside nix and create the screenshots:

Need to have `gg` built first! - disable the tests temporally.

`GG_PATH="$(realpath ./result/bin/gg)" nix develop --command bash -c "cd e2e-tests; pytest -vvv"`

While developing a tests, to add all screenshots:

`rm e2e-tests/screenshots/test_XXX_*; GG_PATH="$(realpath ./result/bin/gg)" nix develop --command bash -c "cd e2e-tests; pytest -vvv -k test_XXX --add-screenshots"; cat e2e-tests/screenshots/test_XXX_*`

Alternatives
============

https://mystor.github.io/git-revise.html
https://git-cola.github.io/
https://github.com/bigH/git-fuzzy A CLI interface to git that relies heavily on fzf.
https://github.com/jesseduffield/lazygit/ A simple terminal UI for git commands, written in Go.
https://github.com/wfxr/forgit Utility tool for using git interactively. Powered by junegunn/fzf.
https://en.wikipedia.org/wiki/Comparison_of_Git_GUIs
https://github.com/andys8/git-brunch A git branch checkout command-line tool
https://github.com/extrawurst/gitui Blazing fast terminal client for git written in Rust
https://github.com/lunaryorn/git-gone prune "gone" Git branches
https://github.com/git-up/GitUp Work quickly, safely, and without headaches. The Git interface you've been missing all your life has finally arrived.
https://github.com/jonas/tig Tig text-mode interface for Git
https://github.com/extrawurst/gitui Blazing fast terminal-ui for git written in rust
https://github.com/vamolessa/verco A simple Git/Mercurial/PlasticSCM tui client based on keyboard shortcuts
https://github.com/jwlodek/pyautogit A TUI for working with git repositories written in python using py_cui.
https://github.com/hugit-project/hugit The humane Terminal UI for git!
https://github.com/vaheqelyan/tygit A basic terminal interface for git, written on Node.js [Project not maintained]
https://github.com/SKalt/git-cc A git extension to help write conventional commits
https://github.com/ArthurSonzogni/git-tui Collection of human friendly terminal interface for git.
https://github.com/tommyclark/asciigit A command line ASCII Git GUI
https://github.com/kalkin/git-log-viewer An alternative to tig which supports folding the merges.
https://github.com/yasukotelin/gitone gitone is simple git tree viewer TUI application!
https://github.com/QaQAdrian/git-tui git tui tool
Awesome Open Source
https://awesomeopensource.com/projects/git/tui
https://awesomeopensource.com/projects/git/haskell

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

Diff libs/tools
===============

https://blog.trailofbits.com/2020/08/28/graphtage/
https://blog.trailofbits.com/2019/11/01/two-new-tools-that-tame-the-treachery-of-files/
https://dl.acm.org/doi/pdf/10.1145/3406088.3409026
https://github.com/paulfitz/daff
https://github.com/homeport/dyff
https://github.com/dandavison/delta GO and pretty

Wcwidth problems
================

https://gitlab.freedesktop.org/terminal-wg/specifications/issues/9
https://stackoverflow.com/questions/52931146/unicode-formatting
https://sourceware.org/git/?p=glibc.git;a=blob;f=wctype/wchar-lookup.h;h=a16e6ca4e179cbf610f9dae2065e615460345b00;hb=HEAD
http://git.musl-libc.org/cgit/musl/tree/src/ctype/wcwidth.c?id=9b2921bea1d5017832e1b45d1fd64220047a9802
https://github.com/richfelker/musl-chartable-tools/tree/master/ctype
https://github.com/jquast/wcwidth/blob/master/wcwidth/wcwidth.py
https://github.com/kovidgoyal/kitty/blob/master/kitty/wcwidth-std.h

Build
=====

https://input-output-hk.github.io/haskell.nix/user-guide/cross-compilation/#static-executables-with-musl-libc
https://gcc.gnu.org/onlinedocs/gcc/Link-Options.html
https://cristianadam.eu/20190501/bundling-together-static-libraries-with-cmake/
https://stackoverflow.com/questions/50022318/using-cmake-to-build-a-static-library-of-static-libraries
https://discourse.nixos.org/t/static-libraries-discarded/991
https://sourceware.org/binutils/docs/binutils/nm.html
https://renenyffenegger.ch/notes/development/languages/C-C-plus-plus/GCC/create-libraries/index
https://github.com/NixOS/nixpkgs/issues/36883
