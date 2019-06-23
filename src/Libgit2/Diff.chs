{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Libgit2.Diff
  ( DiffOption(..)
  , DiffFlags(..)
  , Filemode(..)
  , DeltaType(..)
  , wrapDiffNotifyCb
  , wrapDiffProgressCb
  , diffOptionsVersion
  , diffInitOptions
  , diffDefaultOptions
  , pokeDiffOptionsFlags
  , pokeDiffOptionsIgnoreSubmodules
  , pokeDiffOptionsPathspec
  , pokeDiffOptionsNotifyCb
  , pokeDiffOptionsProgressCb
  , pokeDiffOptionsContextLines
  , pokeDiffOptionsInterhunkLines
  , pokeDiffOptionsIdAbbrev
  , pokeDiffOptionsMaxSize
  , pokeDiffOptionsOldPrefix
  , pokeDiffOptionsNewPrefix
  , diffTreeToTree
  , diffNumDeltas
  ) where

{#import Libgit2.Types#}

import Foreign (Ptr, FunPtr, newForeignPtr_, peek, withForeignPtr, poke, plusPtr, alloca)
import Foreign.C (CString, CInt(..), peekCString, newCString)
import Libgit2.StrArray (StrArray(..))
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (malloca)


#include "git2/diff.h"

{#context lib="git2" prefix="git_"#}


newtype Payload = Payload (Ptr ()) deriving Eq

type DiffNotifyCb = Diff -> DiffDelta -> String -> Payload -> IO Int

type DiffNotifyCb_ = Ptr Diff -> Ptr DiffDelta -> CString -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffNotifyCbWrapper :: DiffNotifyCb_ -> IO (FunPtr DiffNotifyCb_)

wrapDiffNotifyCb :: DiffNotifyCb -> IO (FunPtr DiffNotifyCb_)
wrapDiffNotifyCb cb = _makeDiffNotifyCbWrapper wrapper
  where
    wrapper diffSoFar deltaToAdd matchedPathspec payload = do
      diffSoFar' <- Diff <$> newForeignPtr_ diffSoFar
      deltaToAdd' <- peek deltaToAdd
      matchedPathspec' <- peekCString matchedPathspec
      let payload' = Payload payload
      res <- cb diffSoFar' deltaToAdd' matchedPathspec' payload'
      pure $ fromIntegral res

type DiffProgressCb = Diff -> String -> String -> Payload -> IO Int

type DiffProgressCb_ = Ptr Diff -> CString -> CString -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffProgressCbWrapper :: DiffProgressCb_ -> IO (FunPtr DiffProgressCb_)

wrapDiffProgressCb :: DiffProgressCb -> IO (FunPtr DiffProgressCb_)
wrapDiffProgressCb cb = _makeDiffProgressCbWrapper wrapper
  where
    wrapper diffSoFar oldPath newPath payload = do
      diffSoFar' <- Diff <$> newForeignPtr_ diffSoFar
      oldPath' <- peekCString oldPath
      newPath' <- peekCString newPath
      let payload' = Payload payload
      res <- cb diffSoFar' oldPath' newPath' payload'
      pure $ fromIntegral res

diffOptionsVersion :: Int
diffOptionsVersion = {#const GIT_DIFF_OPTIONS_VERSION#}

{#fun unsafe diff_init_options as diffInitOptions { malloca- `DiffOptions' peekDiffOptions*, `Int' } -> `Int' checkReturnCode*-#}

diffDefaultOptions :: IO (DiffOptions)
diffDefaultOptions = diffInitOptions diffOptionsVersion

_pokeDiffOptions :: (Ptr DiffOptions -> b -> IO ()) -> (a -> IO b) -> DiffOptions -> a -> IO ()
_pokeDiffOptions setter inMarshaller (DiffOptions fp) val =
  withForeignPtr fp (\p -> do
    cVal <- inMarshaller val
    setter p cVal)

pokeDiffOptionsFlags :: DiffOptions -> DiffOption -> IO ()
pokeDiffOptionsFlags = _pokeDiffOptions ({#set diff_options->flags#}) (pure . fromDiffOptions)

pokeDiffOptionsIgnoreSubmodules :: DiffOptions -> DiffSubmoduleIgnore -> IO ()
pokeDiffOptionsIgnoreSubmodules = _pokeDiffOptions ({#set diff_options->ignore_submodules#}) (pure . fromIntegral . fromEnum)

pokeDiffOptionsPathspec :: DiffOptions -> [String] -> IO ()
pokeDiffOptionsPathspec (DiffOptions fp) strs = withForeignPtr fp $ \p -> do
  let strArray = StrArray strs
  let pathspecPtr = p `plusPtr` {#offsetof diff_options->pathspec#}
  poke pathspecPtr strArray

pokeDiffOptionsNotifyCb :: DiffOptions -> DiffNotifyCb -> IO ()
pokeDiffOptionsNotifyCb = _pokeDiffOptions ({#set diff_options->notify_cb#}) wrapDiffNotifyCb

pokeDiffOptionsProgressCb :: DiffOptions -> DiffProgressCb -> IO ()
pokeDiffOptionsProgressCb = _pokeDiffOptions ({#set diff_options->progress_cb#}) wrapDiffProgressCb

pokeDiffOptionsContextLines :: DiffOptions -> Int -> IO ()
pokeDiffOptionsContextLines = _pokeDiffOptions ({#set diff_options->context_lines#}) (pure . fromIntegral)

pokeDiffOptionsInterhunkLines :: DiffOptions -> Int -> IO ()
pokeDiffOptionsInterhunkLines = _pokeDiffOptions ({#set diff_options->interhunk_lines#}) (pure . fromIntegral)

pokeDiffOptionsIdAbbrev :: DiffOptions -> Int -> IO ()
pokeDiffOptionsIdAbbrev = _pokeDiffOptions ({#set diff_options->id_abbrev#}) (pure . fromIntegral)

pokeDiffOptionsMaxSize :: DiffOptions -> GitOff -> IO ()
pokeDiffOptionsMaxSize = _pokeDiffOptions ({#set diff_options->max_size#}) (pure . fromIntegral)

pokeDiffOptionsOldPrefix :: DiffOptions -> String -> IO ()
pokeDiffOptionsOldPrefix = _pokeDiffOptions ({#set diff_options->old_prefix#}) newCString

pokeDiffOptionsNewPrefix :: DiffOptions -> String -> IO ()
pokeDiffOptionsNewPrefix = _pokeDiffOptions ({#set diff_options->new_prefix#}) newCString

{#fun unsafe diff_tree_to_tree as diffTreeToTree { alloca- `Diff' peekNewDiff*, `Repository', `Tree', `Tree', `DiffOptions' } -> `Int' checkReturnCode*-#}

{#fun unsafe diff_num_deltas as diffNumDeltas { `Diff' } -> `Int'#}
