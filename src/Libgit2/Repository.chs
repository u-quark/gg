{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

  gg is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  gg is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with gg.  If not, see <https://www.gnu.org/licenses/>.
-}
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
  , repositoryHead
)

where

{#import Libgit2.Types#}

import Foreign (alloca)
import Foreign.C (CUInt)
import Data.Bits (Bits)
import Libgit2.Errors (checkReturnCode)

#include "git2/repository.h"

{#context lib="git2" prefix="git_"#}

{#fun repository_open as repositoryOpen { alloca- `Repository' peekNewRepository*, `String' } -> `Int' checkReturnCode*-#}

{#enum repository_open_flag_t as InternalOpenFlag {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_REPOSITORY_" add prefix = "internal" deriving (Eq, Show)#}

newtype RepositoryOpenFlags = RepositoryOpenFlags CUInt deriving (Eq, Show, Bits)

fromRepositoryOpenFlags :: RepositoryOpenFlags -> CUInt
fromRepositoryOpenFlags (RepositoryOpenFlags flags) = flags

fromOpenEnum :: InternalOpenFlag -> RepositoryOpenFlags
fromOpenEnum = RepositoryOpenFlags . fromIntegral . fromEnum

repositoryOpenNoFlags :: RepositoryOpenFlags
repositoryOpenNoFlags = RepositoryOpenFlags 0
repositoryOpenNoSearch :: RepositoryOpenFlags
repositoryOpenNoSearch = fromOpenEnum InternalOpenNoSearch
repositoryOpenCrossFs :: RepositoryOpenFlags
repositoryOpenCrossFs = fromOpenEnum InternalOpenCrossFs
repositoryOpenBare :: RepositoryOpenFlags
repositoryOpenBare = fromOpenEnum InternalOpenBare
repositoryOpenNoDotgit :: RepositoryOpenFlags
repositoryOpenNoDotgit = fromOpenEnum InternalOpenNoDotgit
repositoryOpenFromEnv :: RepositoryOpenFlags
repositoryOpenFromEnv = fromOpenEnum InternalOpenFromEnv

{#fun repository_open_ext as repositoryOpenExt { alloca- `Repository' peekNewRepository*, `String', fromRepositoryOpenFlags `RepositoryOpenFlags', `String' } -> `Int' checkReturnCode*-#}

{#fun repository_head as repositoryHead { alloca- `Reference' peekNewReference*, `Repository' } -> `Int' checkReturnCode*-#}
