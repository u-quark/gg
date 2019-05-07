module Libgit2.Global (
    libgit2Init
)

where

#define __CLANG_INTTYPES_H 

#include "global.h"

{#context prefix="git_"#}

libgit2Init :: IO C2HSImp.CInt
libgit2Init = {#call unsafe libgit2_init#}
