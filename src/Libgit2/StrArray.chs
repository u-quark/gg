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

module Libgit2.StrArray
  ( StrArray(..)
  , StrArrayPtr
  , strArrayFree
  ) where

import Foreign (Storable, peekArray, newArray)
import Foreign.C (peekCString, newCString)

#include "git2/strarray.h"

{#context lib="git2" prefix="git_"#}


newtype StrArray = StrArray [String] deriving (Show)
{#pointer *strarray as StrArrayPtr -> StrArray#}
instance Storable StrArray where
  sizeOf _ = {#sizeof strarray#}
  alignment _ = {#alignof strarray#}
  peek p = do
    count <- fromIntegral <$> {#get strarray->count #} p
    ptrs <- {#get strarray->strings #} p
    strings <- mapM peekCString =<< peekArray count ptrs
    pure $ StrArray strings
  poke p (StrArray strings) = do
    {#set strarray->count #} p (fromIntegral $ length strings)
    cstrings <- mapM newCString strings
    ptrs <- newArray cstrings
    {#set strarray->strings #} p ptrs

{#fun strarray_free as strArrayFree { `StrArrayPtr' } -> `()'#}
