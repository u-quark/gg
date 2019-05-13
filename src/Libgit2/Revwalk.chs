{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Libgit2.Revwalk (
    SortFlags
  , sortNone
  , sortTopological
  , sortTime
  , sortReverse
  , revwalkNew
  , revwalkSorting
)

where

{#import Libgit2.Types#}

import Foreign (alloca)
import Foreign.C (CUInt)
import Data.Bits (Bits)
import Libgit2.Errors (checkReturnCode)

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

{#fun unsafe revwalk_new as revwalkNew { alloca- `Revwalk' peekNewRevwalk*, `Repository' } -> `Int' checkReturnCode*-#}

{#fun unsafe revwalk_sorting as revwalkSorting { `Revwalk', fromSortFlags `SortFlags' } -> `()' #}
