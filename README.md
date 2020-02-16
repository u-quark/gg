![GG Mascot](assets/mascot/mascot.png)

# gg - git (G)UI

A tool for interactively manipulating the history of git repos and for making
easy (enforcing) the usage of good (opinionated) practices.

[![CircleCI](https://img.shields.io/circleci/build/github/u-quark/gg?logo=circleci&style=plastic)](https://circleci.com/gh/u-quark/gg)
[![License: GPLv3+](https://img.shields.io/static/v1?label=license&message=GPLv3+&color=bd0000&style=plastic)](https://www.gnu.org/licenses/gpl.html)

## Install

**Caution: this is very much work in progress.**

gg is built as a single statically linked binary. So to install it you can
just download the binary from the latest
[nightly build](https://github.com/u-quark/gg/releases/download/nightly/gg)
and `chmod +x` it.

Alternatively, you can build it with nix. You can speed up the build by using
nix binary caches, thanks to [Cachix](https://cachix.org/), by adding these
to your `~/.config/nix/nix.conf`:

```
substituters = [...] https://static-haskell-nix.cachix.org https://gg.cachix.org
trusted-public-keys = [...] static-haskell-nix.cachix.org0:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU= gg.cachix.org-1:pCGtBoTzcTr3Nd5ou0061JBJpZPUWBBfChdAp7H8xh8=
```

## Features (my TODO list)

 * Apply changes immediately - no stage/index.
 * Easy undo/redo.
 * Re-order commits.
 * Move commits to different branches.
 * Cut/Copy paste commits.
 * Protect master commits from changes. Prevent (force?) pushing to master.
 * Update policy (fetch/rebase/push).
 * Update all branches (only fail for the current one).
 * Cut/Copy paste changes around.
 * Merge strategy (create merge commit and push after rebase).
 * Read configuration from `~/.gitconfig`.
 * Auto-refresh repo view powered by inotify.
 * Collapse commits corresponding to merge into one, visually (implies
   rebase-merge strategy). Collapsed commit can be expanded. No complicated
   history graph: collapse feature branch commits into a single list.
 * Upon startup gg saves all uncommitted changes and untracked files (but not
   ignored) to a commit on HEAD (named `WIP - gg`) and upon exit resets all
   those commits that are on top. This is done that local changes are not lost
   while re-writing the history.
 * Protect from merging WIP commits to master. Same for
   TODO/XXX/FIXME/FIXUP/SQUASHME.
 * Always push with lease when updating remote.
 * GTK GUI
 * Enforce commit messages guidelines. Auto format to re-wrap text of commit
   messages. Limit title line length. Disallow non-imperative phrasing.
 * When rebasing/updating a branch, rebase also other branches that are on top
   of that: rebase on top of the commit that diverged from the other branch.
 * git pull/fetch always with --prune
 * git push non-master branches always with --force-with-lease
 * Knows about merge requests on bitbucket/github/gitlab etc. Knows to fetch
   info from them and also close merge requests there. Also annotates (merge)
   commits with merge request info from there.
 * Recursive git blame: ability to follow a line as it changed throughout the
   git history.
 * Support for different [git workflows][1] ([Git flow command][2]).


[1]: https://nvie.com/posts/a-successful-git-branching-model/
[2]: https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow

## License

Copyright (C) 2019-2020  Lefteris Kritikos (eleftherios.kritikos@gmail.com)

[![GPLv3 Logo](https://www.gnu.org/graphics/gplv3-with-text-136x68.png)](https://www.gnu.org/licenses/gpl.html)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Thanks to:
 * [Oleh Stolyar](https://github.com/stolyaroleh)

