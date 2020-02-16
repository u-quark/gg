{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2020  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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

module Libgit2.Revwalk
  ( SortFlags
  , sortNone
  , sortTopological
  , sortTime
  , sortReverse
  , revwalkNew
  , revwalkSorting
  , revwalkNext
  , revwalkPushHead
  ) where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import Foreign (alloca)
import Foreign.C (CUInt)
import Data.Bits (Bits)
import Libgit2.Errors (checkReturnCode, checkReturnCodeIter)
import Libgit2.Utils (IterResult)

#include "git2/revwalk.h"

{#context lib="git2" prefix="git_"#}

{#enum sort_t as InternalSortFlag {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_SORT_" add prefix = "internal" deriving (Eq, Show)#}

newtype SortFlags = SortFlags CUInt deriving (Eq, Show, Bits)

fromSortFlags :: SortFlags -> CUInt
fromSortFlags (SortFlags flags) = flags

fromSortEnum :: InternalSortFlag -> SortFlags
fromSortEnum = SortFlags . fromIntegral . fromEnum

sortNone :: SortFlags
sortNone = fromSortEnum InternalNone
sortTopological :: SortFlags
sortTopological = fromSortEnum InternalTopological
sortTime :: SortFlags
sortTime = fromSortEnum InternalTime
sortReverse :: SortFlags
sortReverse = fromSortEnum InternalReverse

{#fun revwalk_new as revwalkNew { alloca- `Revwalk' peekNewRevwalk*, `Repository' } -> `Int' checkReturnCode*-#}

{#fun revwalk_sorting as revwalkSorting { `Revwalk', fromSortFlags `SortFlags' } -> `()' #}

{#fun revwalk_next as revwalkNext { `OID', `Revwalk' } -> `IterResult' checkReturnCodeIter*#}

{#fun revwalk_push_head as revwalkPushHead { `Revwalk' } -> `Int' checkReturnCode*-#}
