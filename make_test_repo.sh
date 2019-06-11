#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"/test_repo
rm -rf "$DIR"
mkdir "$DIR"

(
cd "$DIR"
git init
echo e >e
git add e
git commit -m "E This is the first commit" --author="Someone <mystery@mail.com>"
echo d >d
git add d
git commit -m "D This is commit is less impressive" --author="Me again <again@mail.com>"
echo c >c
git add c
git commit -m "C This is an even more awesome commit" --author="I <i@mail.com>"
echo b >b
git add b
git commit -m "B This is an awesome commit" --author="Myself <myself@mail.com>"
echo a >a
git add a
git commit -m "A This is the last commit" --author="Me <me@mail.com>"
)
