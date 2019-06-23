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

{#fun unsafe strarray_free as strArrayFree { `StrArrayPtr' } -> `()'#}
