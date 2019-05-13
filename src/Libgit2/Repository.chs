{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Libgit2.Repository (
    repositoryOpen
  , repositoryOpenExt
  , repositoryOpenNoFlags
  , repositoryOpenNoSearch
  , repositoryOpenCrossFs
  , repositoryOpenBare
  , repositoryOpenNoDotgit
  , repositoryOpenFromEnv
)

where

{#import Libgit2.Types#}

import Foreign (alloca)
import Foreign.C (CUInt)
import Data.Bits (Bits)
import Libgit2.Errors (checkReturnCode)

#include "git2/repository.h"

{#context lib="git2" prefix="git_"#}

{#fun unsafe repository_open as repositoryOpen { alloca- `Repository' peekNewRepository*, `String' } -> `Int' checkReturnCode*-#}

{#enum repository_open_flag_t as InternalOpenFlag {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_REPOSITORY_" add prefix = "internal" deriving (Eq, Show)#}

newtype RepositoryOpenFlags = RepositoryOpenFlags CUInt deriving (Eq, Show, Bits)

fromRepositoryOpenFlags (RepositoryOpenFlags flags) = flags

fromOpenEnum = RepositoryOpenFlags . fromIntegral . fromEnum

repositoryOpenNoFlags = RepositoryOpenFlags 0
repositoryOpenNoSearch = fromOpenEnum InternalOpenNoSearch
repositoryOpenCrossFs = fromOpenEnum InternalOpenCrossFs
repositoryOpenBare = fromOpenEnum InternalOpenBare
repositoryOpenNoDotgit = fromOpenEnum InternalOpenNoDotgit
repositoryOpenFromEnv = fromOpenEnum InternalOpenFromEnv

{#fun unsafe repository_open_ext as repositoryOpenExt { alloca- `Repository' peekNewRepository*, `String', fromRepositoryOpenFlags `RepositoryOpenFlags', `String' } -> `Int' checkReturnCode*-#}
