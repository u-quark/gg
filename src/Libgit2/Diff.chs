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

module Libgit2.Diff
  ( DiffOption(..)
  , DiffFlags(..)
  , Filemode(..)
  , DeltaType(..)
  , DiffNotifyCb
  , DiffProgressCb
  , DiffFileCb
  , DiffBinaryCb
  , DiffHunkCb
  , DiffLineCb
  , DiffInfo
  , DeltaInfo
  , HunkInfo
  , wrapDiffNotifyCb
  , wrapDiffProgressCb
  , wrapDiffFileCb
  , wrapDiffBinaryCb
  , wrapDiffHunkCb
  , wrapDiffLineCb
  , diffOptionsVersion
  , diffOptionsInit
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
  , diffFindOptionsVersion
  , diffFindOptionsInit
  , diffFindDefaultOptions
  , pokeDiffFindFlags
  , pokeDiffFindRenameThreshold
  , pokeDiffFindRenameFromRewriteThreshold
  , pokeDiffFindCopyThreshold
  , pokeDiffFindBreakRewriteThreshold
  , pokeDiffFindRenameLimit
  , diffTreeToTree
  , diffNumDeltas
  , diffGetStats
  , diffStatsFilesChanged
  , diffStatsInsertions
  , diffStatsDeletions
  , diffForEach
  , diffInfo
  , diffFindSimilar
  ) where

{#import Libgit2.Types#}

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Foreign (Ptr, FunPtr, newForeignPtr_, peek, withForeignPtr, poke, plusPtr, alloca)
import Foreign.C (CString, CInt(..), CFloat(..), peekCString, newCString)
import Libgit2.StrArray (StrArray(..))
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (malloca, withFunPtr, withFunPtrM, alterAList, pokeStruct)


#include "git2/diff.h"

{#context lib="git2" prefix="git_"#}


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

type DiffFileCb = DiffDelta -> Float -> Payload -> IO Int

type DiffFileCb_ = Ptr DiffDelta -> CFloat -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffFileCbWrapper :: DiffFileCb_ -> IO (FunPtr DiffFileCb_)

wrapDiffFileCb :: DiffFileCb -> IO (FunPtr DiffFileCb_)
wrapDiffFileCb cb = _makeDiffFileCbWrapper wrapper
  where
    wrapper delta progress payload = do
      delta' <- peek delta
      let CFloat progress' = progress
      let payload' = Payload payload
      res <- cb delta' progress' payload'
      pure $ fromIntegral res

withDiffFileCb :: DiffFileCb -> (FunPtr DiffFileCb_ -> IO b) -> IO b
withDiffFileCb = withFunPtr wrapDiffFileCb

type DiffBinaryCb = DiffDelta -> DiffBinary -> Payload -> IO Int

type DiffBinaryCb_ = DiffDeltaPtr -> DiffBinaryPtr -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffBinaryCbWrapper :: DiffBinaryCb_ -> IO (FunPtr DiffBinaryCb_)

wrapDiffBinaryCb :: DiffBinaryCb -> IO (FunPtr DiffBinaryCb_)
wrapDiffBinaryCb cb = _makeDiffBinaryCbWrapper wrapper
  where
    wrapper delta binary payload = do
      delta' <- peek delta
      binary' <- peek binary
      let payload' = Payload payload
      res <- cb delta' binary' payload'
      pure $ fromIntegral res

withDiffBinaryCbM :: Maybe DiffBinaryCb -> (FunPtr DiffBinaryCb_ -> IO b) -> IO b
withDiffBinaryCbM = withFunPtrM wrapDiffBinaryCb

type DiffHunkCb = DiffDelta -> DiffHunk -> Payload -> IO Int

type DiffHunkCb_ = DiffDeltaPtr -> DiffHunkPtr -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffHunkCbWrapper :: DiffHunkCb_ -> IO (FunPtr DiffHunkCb_)

wrapDiffHunkCb :: DiffHunkCb -> IO (FunPtr DiffHunkCb_)
wrapDiffHunkCb cb = _makeDiffHunkCbWrapper wrapper
  where
    wrapper delta hunk payload = do
      delta' <- peek delta
      hunk' <- peek hunk
      let payload' = Payload payload
      res <- cb delta' hunk' payload'
      pure $ fromIntegral res

withDiffHunkCbM :: Maybe DiffHunkCb -> (FunPtr DiffHunkCb_ -> IO b) -> IO b
withDiffHunkCbM = withFunPtrM wrapDiffHunkCb

type DiffLineCb = DiffDelta -> DiffHunk -> DiffLine -> Payload -> IO Int

type DiffLineCb_ = DiffDeltaPtr -> DiffHunkPtr -> DiffLinePtr -> Ptr () -> IO CInt

foreign import ccall "wrapper" _makeDiffLineCbWrapper :: DiffLineCb_ -> IO (FunPtr DiffLineCb_)

wrapDiffLineCb :: DiffLineCb -> IO (FunPtr DiffLineCb_)
wrapDiffLineCb cb = _makeDiffLineCbWrapper wrapper
  where
    wrapper delta hunk line payload = do
      delta' <- peek delta
      hunk' <- peek hunk
      line' <- peek line
      let payload' = Payload payload
      res <- cb delta' hunk' line' payload'
      pure $ fromIntegral res

withDiffLineCbM :: Maybe DiffLineCb -> (FunPtr DiffLineCb_ -> IO b) -> IO b
withDiffLineCbM = withFunPtrM wrapDiffLineCb

diffOptionsVersion :: Int
diffOptionsVersion = {#const GIT_DIFF_OPTIONS_VERSION#}

{#fun diff_options_init as diffOptionsInit { malloca- `DiffOptions' peekDiffOptions*, `Int' } -> `Int' checkReturnCode*-#}

diffDefaultOptions :: IO (DiffOptions)
diffDefaultOptions = diffOptionsInit diffOptionsVersion

_pokeDiffOptions :: (Ptr DiffOptions -> b -> IO ()) -> (a -> IO b) -> DiffOptions -> a -> IO ()
_pokeDiffOptions setter inMarshaller (DiffOptions fp) val = pokeStruct setter inMarshaller fp val

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

{#fun diff_tree_to_tree as diffTreeToTree { alloca- `Diff' peekNewDiff*, `Repository', `Tree', `Tree', `DiffOptions' } -> `Int' checkReturnCode*-#}

{#fun diff_num_deltas as diffNumDeltas { `Diff' } -> `Int'#}

{#fun diff_get_stats as diffGetStats { alloca- `DiffStats' peekNewDiffStats*, `Diff' } -> `Int' checkReturnCode*-#}

{#fun pure diff_stats_files_changed as diffStatsFilesChanged { `DiffStats' } -> `Int'#}

{#fun pure diff_stats_insertions as diffStatsInsertions { `DiffStats' } -> `Int'#}

{#fun pure diff_stats_deletions as diffStatsDeletions { `DiffStats' } -> `Int'#}

{#fun diff_foreach as diffForEach { `Diff', withDiffFileCb* `DiffFileCb', withDiffBinaryCbM* `Maybe DiffBinaryCb', withDiffHunkCbM* `Maybe DiffHunkCb', withDiffLineCbM* `Maybe DiffLineCb', withPayload* `Payload' } -> `Int'#}

type DiffInfo = [(DiffDelta, DeltaInfo)]

type DeltaInfo = ([HunkInfo], [DiffBinary])

type HunkInfo = (DiffHunk, [DiffLine])

diffInfo :: Diff -> IO DiffInfo
diffInfo diff = do
  ref <- newIORef []
  let collectDeltaDiff delta _progress _payload = do
        modifyIORef ref (appendDelta delta)
        pure 0
  let collectLineDiff delta hunk line _payload = do
        modifyIORef ref (modifyDeltaHunks delta hunk line)
        pure 0
  let collectBinaryDiff delta binary _payload = do
        modifyIORef ref (modifyDeltaBinary delta binary)
        pure 0
  _ <- diffForEach diff collectDeltaDiff (Just collectBinaryDiff) Nothing (Just collectLineDiff) nullPayload
  readIORef ref
  where
    appendAlterF :: a -> Maybe [a] -> Maybe [a]
    appendAlterF v vsM = Just $ fromMaybe [] vsM ++ [v]
    appendHunkLine :: DiffHunk -> DiffLine -> [HunkInfo] -> [HunkInfo]
    appendHunkLine hunk line = alterAList (appendAlterF line) hunk
    modifyWithDefaultAlterF :: (a -> a) -> a -> Maybe a -> Maybe a
    modifyWithDefaultAlterF f d xM = Just $ f $ fromMaybe d xM
    modifyDelta :: (DeltaInfo -> DeltaInfo) -> DiffDelta -> DiffInfo -> DiffInfo
    modifyDelta f = alterAList (modifyWithDefaultAlterF f ([], []))
    modifyDeltaInfoWithHunk :: DiffHunk -> DiffLine -> DeltaInfo -> DeltaInfo
    modifyDeltaInfoWithHunk hunk line (hunks, binaries) = (appendHunkLine hunk line hunks, binaries)
    modifyDeltaHunks :: DiffDelta -> DiffHunk -> DiffLine -> DiffInfo -> DiffInfo
    modifyDeltaHunks delta hunk line = modifyDelta (modifyDeltaInfoWithHunk hunk line) delta
    modifyDeltaInfoWithBinary :: DiffBinary -> DeltaInfo -> DeltaInfo
    modifyDeltaInfoWithBinary binary (hunks, binaries) = (hunks, binaries ++ [binary])
    modifyDeltaBinary :: DiffDelta -> DiffBinary -> DiffInfo -> DiffInfo
    modifyDeltaBinary delta binary = modifyDelta (modifyDeltaInfoWithBinary binary) delta
    appendDelta :: DiffDelta -> DiffInfo -> DiffInfo
    appendDelta = modifyDelta id

diffFindOptionsVersion :: Int
diffFindOptionsVersion = {#const GIT_DIFF_FIND_OPTIONS_VERSION#}

{#fun diff_find_options_init as diffFindOptionsInit { malloca- `DiffFindOptions' peekDiffFindOptions*, `Int' } -> `Int' checkReturnCode*-#}

diffFindDefaultOptions :: IO (DiffFindOptions)
diffFindDefaultOptions = diffFindOptionsInit diffFindOptionsVersion

_pokeDiffFindOptions :: (Ptr DiffFindOptions -> b -> IO ()) -> (a -> IO b) -> DiffFindOptions -> a -> IO ()
_pokeDiffFindOptions setter inMarshaller (DiffFindOptions fp) val = pokeStruct setter inMarshaller fp val

pokeDiffFindFlags :: DiffFindOptions -> DiffFindFlag -> IO ()
pokeDiffFindFlags = _pokeDiffFindOptions ({#set diff_find_options->flags#}) (pure . fromDiffFindFlags)

pokeDiffFindRenameThreshold :: DiffFindOptions -> Int -> IO ()
pokeDiffFindRenameThreshold = _pokeDiffFindOptions ({#set diff_find_options->rename_threshold#}) (pure . fromIntegral)

pokeDiffFindRenameFromRewriteThreshold :: DiffFindOptions -> Int -> IO ()
pokeDiffFindRenameFromRewriteThreshold = _pokeDiffFindOptions ({#set diff_find_options->rename_from_rewrite_threshold#}) (pure . fromIntegral)

pokeDiffFindCopyThreshold :: DiffFindOptions -> Int -> IO ()
pokeDiffFindCopyThreshold = _pokeDiffFindOptions ({#set diff_find_options->copy_threshold#}) (pure . fromIntegral)

pokeDiffFindBreakRewriteThreshold :: DiffFindOptions -> Int -> IO ()
pokeDiffFindBreakRewriteThreshold = _pokeDiffFindOptions ({#set diff_find_options->break_rewrite_threshold#}) (pure . fromIntegral)

pokeDiffFindRenameLimit :: DiffFindOptions -> Int -> IO ()
pokeDiffFindRenameLimit = _pokeDiffFindOptions ({#set diff_find_options->rename_limit#}) (pure . fromIntegral)

{#fun diff_find_similar as diffFindSimilar { `Diff', `DiffFindOptions' } -> `Int' checkReturnCode*-#}
