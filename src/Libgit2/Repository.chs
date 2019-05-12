module Libgit2.Repository (
    repositoryOpen
)

where

{#import Libgit2.Types#}

import Foreign (alloca)
import Libgit2.Errors (checkReturnCode)

#include "git2/repository.h"

{#context lib="git2" prefix="git_"#}

{#fun unsafe repository_open as repositoryOpen { alloca- `Repository' peekNewRepository*, `String' } -> `Int' checkReturnCode*-#}

