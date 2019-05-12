module Libgit2.Types (
    Repository(..)
  , peekNewRepository
  , repositoryFree
  , withRepository
)

where

import Libgit2.Utils (peekNew)

#include <git2/types.h>
#include <git2/repository.h>

{#context lib="git2" prefix="git_"#}

{#pointer *repository as Repository foreign finalizer repository_free as repositoryFree newtype#}
peekNewRepository = peekNew Repository repositoryFree
