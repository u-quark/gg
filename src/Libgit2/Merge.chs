{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2023  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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

module Libgit2.Merge
  ( mergeOptionsVersion
  , mergeOptionsInit
  , mergeDefaultOptions
  , pokeMergeOptionsFlags
  , pokeMergeOptionsRenameThreshold
  , pokeMergeOptionsTargetLimit
  , pokeMergeOptionsRecursionLimit
  , pokeMergeOptionsDefaultDriver
  , pokeMergeOptionsFileFavor
  , pokeMergeOptionsFileFlags
  , mergeTrees
  ) where

{#import Libgit2.Types#}

import Foreign (Ptr, alloca)
import Foreign.C (CInt(..))
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (malloca, pokeStruct, pokeStructWith)


#include "git2/merge.h"
#include "git2/sys/merge.h"

{#context lib="git2" prefix="git_"#}


mergeOptionsVersion :: Int
mergeOptionsVersion = {#const GIT_MERGE_OPTIONS_VERSION#}

{#fun merge_options_init as mergeOptionsInit { malloca- `MergeOptions' peekMergeOptions*, `Int' } -> `Int' checkReturnCode*-#}

mergeDefaultOptions :: IO (MergeOptions)
mergeDefaultOptions = mergeOptionsInit mergeOptionsVersion

_pokeMergeOptions :: (Ptr MergeOptions -> b -> IO ()) -> (a -> IO b) -> MergeOptions -> a -> IO ()
_pokeMergeOptions setter inMarshaller (MergeOptions fp) val = pokeStruct setter inMarshaller fp val

pokeMergeOptionsFlags :: MergeOptions -> MergeFlags -> IO ()
pokeMergeOptionsFlags = _pokeMergeOptions ({#set merge_options->flags#}) (pure . fromMergeFlags)

pokeMergeOptionsRenameThreshold :: MergeOptions -> Int -> IO ()
pokeMergeOptionsRenameThreshold = _pokeMergeOptions ({#set merge_options->rename_threshold#}) (pure . fromIntegral)

pokeMergeOptionsTargetLimit :: MergeOptions -> Int -> IO ()
pokeMergeOptionsTargetLimit = _pokeMergeOptions ({#set merge_options->target_limit#}) (pure . fromIntegral)

pokeMergeOptionsRecursionLimit :: MergeOptions -> Int -> IO ()
pokeMergeOptionsRecursionLimit = _pokeMergeOptions ({#set merge_options->recursion_limit#}) (pure . fromIntegral)

pokeMergeOptionsDefaultDriver :: MergeOptions -> MergeDriver -> IO ()
pokeMergeOptionsDefaultDriver (MergeOptions fp) = pokeStructWith ({#set merge_options->default_driver#}) withMergeDriver fp

pokeMergeOptionsFileFavor :: MergeOptions -> MergeFileFavour -> IO ()
pokeMergeOptionsFileFavor = _pokeMergeOptions ({#set merge_options->file_favor#}) (pure . fromIntegral . fromEnum)

pokeMergeOptionsFileFlags :: MergeOptions -> MergeFileFlags -> IO ()
pokeMergeOptionsFileFlags = _pokeMergeOptions ({#set merge_options->file_flags#}) (pure . fromMergeFileFlags)

{#fun merge_trees as mergeTrees { alloca- `Index' peekNewIndex*, `Repository', `Tree', `Tree', `Tree', `MergeOptions' } -> `Int' checkReturnCode*-#}
