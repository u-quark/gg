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
module Libgit2.Apply
  ( ApplyDeltaCb
  , wrapApplyDeltaCb
  , ApplyHunkCb
  , wrapApplyHunkCb
  , applyOptionsVersion
  , applyDefaultOptions
  , pokeApplyOptionsVersion
  , pokeApplyOptionsDeltaCb
  , pokeApplyOptionsHunkCb
  , apply
  , applyToTree
  ) where

{#import Libgit2.Types#}

import Foreign (alloca, Ptr, FunPtr, malloc, peek, nullPtr, nullFunPtr)
import Foreign.C (CInt)
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (pokeStruct)

#include "git2/apply.h"

{#context lib="git2" prefix="git_"#}

type ApplyDeltaCb = DiffDelta -> Payload -> IO Int

type ApplyDeltaCb_ = DiffDeltaPtr -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeApplyDeltaCbWrapper :: ApplyDeltaCb_ -> IO (FunPtr ApplyDeltaCb_)

wrapApplyDeltaCb :: ApplyDeltaCb -> IO (FunPtr ApplyDeltaCb_)
wrapApplyDeltaCb cb = _makeApplyDeltaCbWrapper wrapper
  where
    wrapper delta payload = do
      delta' <- peek delta
      let payload' = Payload payload
      res <- cb delta' payload'
      pure $ fromIntegral res

type ApplyHunkCb = DiffHunk -> Payload -> IO Int

type ApplyHunkCb_ = DiffHunkPtr -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeApplyHunkCbWrapper :: ApplyHunkCb_ -> IO (FunPtr ApplyHunkCb_)

wrapApplyHunkCb :: ApplyHunkCb -> IO (FunPtr ApplyHunkCb_)
wrapApplyHunkCb cb = _makeApplyHunkCbWrapper wrapper
  where
    wrapper hunk payload = do
      hunk' <- peek hunk
      let payload' = Payload payload
      res <- cb hunk' payload'
      pure $ fromIntegral res

applyOptionsVersion :: Int
applyOptionsVersion = {#const GIT_APPLY_OPTIONS_VERSION#}

applyDefaultOptions :: IO (ApplyOptions)
applyDefaultOptions = do
  p <- malloc
  fp <- peekApplyOptions p
  pokeApplyOptionsVersion fp applyOptionsVersion
  _pokeApplyOptions ({#set apply_options->delta_cb#}) pure fp nullFunPtr
  _pokeApplyOptions ({#set apply_options->hunk_cb#}) pure fp nullFunPtr
  _pokeApplyOptions ({#set apply_options->payload#}) pure fp nullPtr
  _pokeApplyOptions ({#set apply_options->flags#}) pure fp 0
  pure fp

_pokeApplyOptions :: (Ptr ApplyOptions -> b -> IO ()) -> (a -> IO b) -> ApplyOptions -> a -> IO ()
_pokeApplyOptions setter inMarshaller (ApplyOptions fp) val = pokeStruct setter inMarshaller fp val

pokeApplyOptionsVersion :: ApplyOptions -> Int -> IO ()
pokeApplyOptionsVersion = _pokeApplyOptions ({#set apply_options->version#}) (pure . fromIntegral)

pokeApplyOptionsDeltaCb :: ApplyOptions -> ApplyDeltaCb -> IO ()
pokeApplyOptionsDeltaCb = _pokeApplyOptions ({#set apply_options->delta_cb#}) wrapApplyDeltaCb

pokeApplyOptionsHunkCb :: ApplyOptions -> ApplyHunkCb -> IO ()
pokeApplyOptionsHunkCb = _pokeApplyOptions ({#set apply_options->hunk_cb#}) wrapApplyHunkCb

{#fun apply as apply { `Repository', `Diff', `ApplyLocation', `ApplyOptions' } -> `Int' checkReturnCode*-#}

{#fun apply_to_tree as applyToTree { alloca- `Index' peekNewIndex*, `Repository', `Tree', `Diff', `ApplyOptions' } -> `Int' checkReturnCode*-#}
