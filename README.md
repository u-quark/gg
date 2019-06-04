# gg

## Features

 * Apply changes immediately - no stage/index.
 * Easy undo/redo.
 * Re-order commits.
 * Move commits to different branches.
 * Cut/Copy paste commits.
 * Protect master commits from changes. Prevent (force?) pushing to master.
 * Update policy (fetch/rebase/push).
 * Update all branches (only fail for the current one).
 * Cut/Copy/Paste changes around.
 * Merge strategy (create merge commit and push after rebase).
 * Read configuration from `~/.gitconfig`.
 * Auto-refresh repo view powered by inotify.
 * Collapse commits corresponding to merge into one, visually (implies rebase-merge strategy).
 * Ignore commits that don't confront to a rebase-merge strategy.
