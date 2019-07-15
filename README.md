# gg - git (G)UI

A tool for interactively manipulating the history of git repos and for making
easy (enforcing) the usage of good (opinionated) practices.

## Features

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
