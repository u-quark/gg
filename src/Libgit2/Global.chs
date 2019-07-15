module Libgit2.Global (
    libgit2Init
)

where

#include "git2/global.h"

import Libgit2.Errors (intAndCheckReturnCode)

{#context lib="git2" prefix="git_"#}

{#fun libgit2_init as libgit2Init {} -> `Int' intAndCheckReturnCode*#}
