module Libgit2.Global (
    libgit2Init
)

where

#include "git2/global.h"

{#context lib="git2" prefix="git_"#}

libgit2Init :: IO C2HSImp.CInt
libgit2Init = {#call unsafe libgit2_init#}
