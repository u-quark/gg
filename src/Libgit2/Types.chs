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

module Libgit2.Types
  ( GitOff
  , Repository(..)
  , peekNewRepository
  , repositoryFree
  , withRepository
  , Revwalk(..)
  , peekNewRevwalk
  , revwalkFree
  , withRevwalk
  , Commit(..)
  , peekNewCommit
  , commitFree
  , withCommit
  , Reference(..)
  , peekNewReference
  , referenceFree
  , withReference
  , gitToLocalTime
  , Signature(..)
  , SignaturePtr
  , Tree(..)
  , peekNewTree
  , treeFree
  , withTree
  , Diff(..)
  , peekNewDiff
  , diffFree
  , withDiff
  , DiffStats(..)
  , peekNewDiffStats
  , diffStatsFree
  , withDiffStats
  , DiffOption(..)
  , fromDiffOptions
  , diffNormal
  , diffReverse
  , diffIncludeIgnored
  , diffRecurseIgnoredDirs
  , diffIncludeUntracked
  , diffRecurseUntrackedDirs
  , diffIncludeUnmodified
  , diffIncludeTypechange
  , diffIncludeTypechangeTrees
  , diffIgnoreFilemode
  , diffIgnoreSubmodules
  , diffIgnoreCase
  , diffIncludeCasechange
  , diffDisablePathspecMatch
  , diffSkipBinaryCheck
  , diffEnableFastUntrackedDirs
  , diffUpdateIndex
  , diffIncludeUnreadable
  , diffIncludeUnreadableAsUntracked
  , diffForceText
  , diffForceBinary
  , diffIgnoreWhitespace
  , diffIgnoreWhitespaceChange
  , diffIgnoreWhitespaceEol
  , diffShowUntrackedContent
  , diffShowUnmodified
  , diffPatience
  , diffMinimal
  , diffShowBinary
  , diffIndentHeuristic
  , DiffFlags(..)
  , fromDiffFlags
  , diffBinary
  , diffNotBinary
  , diffValidId
  , diffExists
  , Filemode(..)
  , DeltaType(..)
  , DiffFile(..)
  , Similarity(..)
  , DiffFilePtr
  , DiffDelta(..)
  , DiffDeltaPtr
  , DiffHunk(..)
  , DiffHunkPtr
  , DiffLine(..)
  , DiffLinePtr
  , DiffBinaryType(..)
  , DiffBinaryFile(..)
  , DiffBinaryFilePtr
  , DiffBinary(..)
  , DiffBinaryPtr
  , DiffSubmoduleIgnore(..)
  , DiffOptions(..)
  , withDiffOptions
  , peekDiffOptions
  , DiffFindFlag(..)
  , fromDiffFindFlags
  , diffFindByConfig
  , diffFindRenames
  , diffFindRenamesFromRewrites
  , diffFindCopies
  , diffFindCopiesFromUnmodified
  , diffFindRewrites
  , diffBreakRewrites
  , diffFindAndBreakRewrites
  , diffFindForUntracked
  , diffFindAll
  , diffFindIgnoreLeadingWhitespace
  , diffFindIgnoreWhitespace
  , diffFindDontIgnoreWhitespace
  , diffFindExactMatchOnly
  , diffBreakRewritesForRenamesOnly
  , diffFindRemoveUnmodified
  , DiffFindOptions(..)
  , withDiffFindOptions
  , peekDiffFindOptions
)

where

import Foreign (Ptr, Storable, peek, newForeignPtr_, peekByteOff)
import Foreign.C (peekCString, peekCStringLen, CUInt, CLong, castCCharToChar)
import Data.Bits (Bits)
import Data.Time.LocalTime (ZonedTime, minutesToTimeZone, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.ByteString (ByteString, packCStringLen)
import Libgit2.OID (OID, _oidFromCString)
import Libgit2.Utils (peekNew)

#include <git2/types.h>
#include <git2/repository.h>
#include <git2/commit.h>
#include <git2/revwalk.h>
#include <git2/refs.h>
#include <git2/tree.h>
#include <git2/diff.h>

{#context lib="git2" prefix="git_"#}

{#pointer *repository as Repository foreign finalizer repository_free as repositoryFree newtype#}

peekNewRepository :: Ptr (Ptr Repository) -> IO Repository
peekNewRepository = peekNew Repository repositoryFree

{#pointer *revwalk as Revwalk foreign finalizer revwalk_free as revwalkFree newtype#}

peekNewRevwalk :: Ptr (Ptr Revwalk) -> IO Revwalk
peekNewRevwalk = peekNew Revwalk revwalkFree

{#pointer *commit as Commit foreign finalizer commit_free as commitFree newtype#}

peekNewCommit :: Ptr (Ptr Commit) -> IO Commit
peekNewCommit = peekNew Commit commitFree

{#pointer *reference as Reference foreign finalizer reference_free as referenceFree newtype#}

peekNewReference :: Ptr (Ptr Reference) -> IO Reference
peekNewReference = peekNew Reference referenceFree

gitToLocalTime :: POSIXTime -> Int -> ZonedTime
gitToLocalTime posixTime offset =
  utcToZonedTime timezone utcTime
  where
    utcTime = posixSecondsToUTCTime posixTime
    timezone = minutesToTimeZone offset

data Signature = Signature {
    signatureName :: String,
    signatureEmail :: String,
    signatureWhen :: ZonedTime
} deriving (Show)
{#pointer *signature as SignaturePtr -> Signature#}
instance Storable Signature where
  sizeOf _ = {#sizeof signature#}
  alignment _ = {#alignof signature#}
  peek p = do
    name <- peekCString =<< ({#get signature->name #} p)
    email <- peekCString =<< ({#get signature->email #} p)
    time <- fromIntegral <$> ({#get signature->when.time #} p)
    offset <- fromIntegral <$> ({#get signature->when.offset #} p)
    pure $ Signature name email (gitToLocalTime time offset)
  poke _ = error "Can't poke Signature"

{#pointer *tree as Tree foreign finalizer tree_free as treeFree newtype#}

peekNewTree :: Ptr (Ptr Tree) -> IO Tree
peekNewTree = peekNew Tree treeFree

{#pointer *diff as Diff foreign finalizer diff_free as diffFree newtype#}

peekNewDiff :: Ptr (Ptr Diff) -> IO Diff
peekNewDiff = peekNew Diff diffFree

{#pointer *diff_stats as DiffStats foreign finalizer diff_stats_free as diffStatsFree newtype#}

peekNewDiffStats :: Ptr (Ptr DiffStats) -> IO DiffStats
peekNewDiffStats = peekNew DiffStats diffStatsFree

{#typedef git_off_t GitOff#}
type GitOff = CLong

{#enum diff_option_t as InternalDiffOption {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_DIFF_" add prefix = "internal_diff_option" deriving (Eq, Show)#}

newtype DiffOption = DiffOption CUInt deriving (Eq, Show, Bits)

fromDiffOptions :: DiffOption -> CUInt
fromDiffOptions (DiffOption options) = options

fromDiffOptionsEnum :: InternalDiffOption -> DiffOption
fromDiffOptionsEnum = DiffOption . fromIntegral . fromEnum

diffNormal :: DiffOption
diffNormal = fromDiffOptionsEnum InternalDiffOptionNormal
diffReverse :: DiffOption
diffReverse = fromDiffOptionsEnum InternalDiffOptionReverse
diffIncludeIgnored :: DiffOption
diffIncludeIgnored = fromDiffOptionsEnum InternalDiffOptionIncludeIgnored
diffRecurseIgnoredDirs :: DiffOption
diffRecurseIgnoredDirs = fromDiffOptionsEnum InternalDiffOptionRecurseIgnoredDirs
diffIncludeUntracked :: DiffOption
diffIncludeUntracked = fromDiffOptionsEnum InternalDiffOptionIncludeUntracked
diffRecurseUntrackedDirs :: DiffOption
diffRecurseUntrackedDirs = fromDiffOptionsEnum InternalDiffOptionRecurseUntrackedDirs
diffIncludeUnmodified :: DiffOption
diffIncludeUnmodified = fromDiffOptionsEnum InternalDiffOptionIncludeUnmodified
diffIncludeTypechange :: DiffOption
diffIncludeTypechange = fromDiffOptionsEnum InternalDiffOptionIncludeTypechange
diffIncludeTypechangeTrees :: DiffOption
diffIncludeTypechangeTrees = fromDiffOptionsEnum InternalDiffOptionIncludeTypechangeTrees
diffIgnoreFilemode :: DiffOption
diffIgnoreFilemode = fromDiffOptionsEnum InternalDiffOptionIgnoreFilemode
diffIgnoreSubmodules :: DiffOption
diffIgnoreSubmodules = fromDiffOptionsEnum InternalDiffOptionIgnoreSubmodules
diffIgnoreCase :: DiffOption
diffIgnoreCase = fromDiffOptionsEnum InternalDiffOptionIgnoreCase
diffIncludeCasechange :: DiffOption
diffIncludeCasechange = fromDiffOptionsEnum InternalDiffOptionIncludeCasechange
diffDisablePathspecMatch :: DiffOption
diffDisablePathspecMatch = fromDiffOptionsEnum InternalDiffOptionDisablePathspecMatch
diffSkipBinaryCheck :: DiffOption
diffSkipBinaryCheck = fromDiffOptionsEnum InternalDiffOptionSkipBinaryCheck
diffEnableFastUntrackedDirs :: DiffOption
diffEnableFastUntrackedDirs = fromDiffOptionsEnum InternalDiffOptionEnableFastUntrackedDirs
diffUpdateIndex :: DiffOption
diffUpdateIndex = fromDiffOptionsEnum InternalDiffOptionUpdateIndex
diffIncludeUnreadable :: DiffOption
diffIncludeUnreadable = fromDiffOptionsEnum InternalDiffOptionIncludeUnreadable
diffIncludeUnreadableAsUntracked :: DiffOption
diffIncludeUnreadableAsUntracked = fromDiffOptionsEnum InternalDiffOptionIncludeUnreadableAsUntracked
diffForceText :: DiffOption
diffForceText = fromDiffOptionsEnum InternalDiffOptionForceText
diffForceBinary :: DiffOption
diffForceBinary = fromDiffOptionsEnum InternalDiffOptionForceBinary
diffIgnoreWhitespace :: DiffOption
diffIgnoreWhitespace = fromDiffOptionsEnum InternalDiffOptionIgnoreWhitespace
diffIgnoreWhitespaceChange :: DiffOption
diffIgnoreWhitespaceChange = fromDiffOptionsEnum InternalDiffOptionIgnoreWhitespaceChange
diffIgnoreWhitespaceEol :: DiffOption
diffIgnoreWhitespaceEol = fromDiffOptionsEnum InternalDiffOptionIgnoreWhitespaceEol
diffShowUntrackedContent :: DiffOption
diffShowUntrackedContent = fromDiffOptionsEnum InternalDiffOptionShowUntrackedContent
diffShowUnmodified :: DiffOption
diffShowUnmodified = fromDiffOptionsEnum InternalDiffOptionShowUnmodified
diffPatience :: DiffOption
diffPatience = fromDiffOptionsEnum InternalDiffOptionPatience
diffMinimal :: DiffOption
diffMinimal = fromDiffOptionsEnum InternalDiffOptionMinimal
diffShowBinary :: DiffOption
diffShowBinary = fromDiffOptionsEnum InternalDiffOptionShowBinary
diffIndentHeuristic :: DiffOption
diffIndentHeuristic = fromDiffOptionsEnum InternalDiffOptionIndentHeuristic

{#enum diff_flag_t as InternalDiffFlags {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_DIFF_FLAG_" add prefix = "internal_diff_flag" deriving (Eq, Show)#}

newtype DiffFlags = DiffFlags CUInt deriving (Eq, Show, Bits)

fromDiffFlags :: DiffFlags -> CUInt
fromDiffFlags (DiffFlags flags) = flags

fromDiffFlagsEnum :: InternalDiffFlags -> DiffFlags
fromDiffFlagsEnum = DiffFlags . fromIntegral . fromEnum

diffBinary :: DiffFlags
diffBinary = fromDiffFlagsEnum InternalDiffFlagBinary
diffNotBinary :: DiffFlags
diffNotBinary = fromDiffFlagsEnum InternalDiffFlagNotBinary
diffValidId :: DiffFlags
diffValidId = fromDiffFlagsEnum InternalDiffFlagValidId
diffExists :: DiffFlags
diffExists = fromDiffFlagsEnum InternalDiffFlagExists

{#enum delta_t as DeltaType {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

{#enum filemode_t as Filemode {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

data DiffFile = DiffFile
  { diffFileId :: OID
  , diffFilePath :: String
  , diffFileSize :: GitOff
  , diffFileFlags :: DiffFlags
  , diffFileMode :: Filemode
  , diffFileIdAbbrev :: Int
} deriving (Show, Eq)
{#pointer *diff_file as DiffFilePtr -> DiffFile#}
instance Storable DiffFile where
  sizeOf _ = {#sizeof diff_file#}
  alignment _ = {#alignof diff_file#}
  peek p = do
    id' <- _oidFromCString =<< ({#get diff_file->id.id #} p)
    path <- peekCString =<< ({#get diff_file->path #} p)
    size <- fromIntegral <$> ({#get diff_file->size #} p)
    flags <- DiffFlags <$> ({#get diff_file->flags #} p)
    mode <- (toEnum . fromIntegral) <$> ({#get diff_file->mode #} p)
    idAbbrev <- fromIntegral <$> ({#get diff_file->id_abbrev #} p)
    pure $ DiffFile id' path size flags mode idAbbrev
  poke _ = error "Can't poke DiffFile"

newtype Similarity = Similarity Int deriving (Num, Show, Eq)
instance Bounded Similarity
  where
    minBound = 0
    maxBound = 100

data DiffDelta = DiffDelta
  { diffDeltaStatus :: DeltaType
  , diffDeltaFlags :: DiffFlags
  , diffDeltaSimilarity :: Similarity
  , diffDeltaNFiles :: Int
  , diffDeltaOldFile :: DiffFile
  , diffDeltaNewFile :: DiffFile
} deriving (Show, Eq)
{#pointer *diff_delta as DiffDeltaPtr -> DiffDelta#}
instance Storable DiffDelta where
  sizeOf _ = {#sizeof diff_delta#}
  alignment _ = {#alignof diff_delta#}
  peek p = do
    status <- (toEnum . fromIntegral) <$> ({#get diff_delta->status #} p)
    flags <- DiffFlags <$> ({#get diff_delta->flags #} p)
    similarity <- (Similarity . fromIntegral) <$> ({#get diff_delta->similarity #} p)
    nFiles <- fromIntegral <$> ({#get diff_delta->nfiles #} p)
    oldFile <- peekByteOff p {#offsetof diff_delta->old_file #}
    newFile <- peekByteOff p {#offsetof diff_delta->new_file #}
    pure $ DiffDelta status flags similarity nFiles oldFile newFile
  poke _ = error "Can't poke DiffDelta"

data DiffHunk = DiffHunk
  { diffHunkOldStart :: Int
  , diffHunkOldLines :: Int
  , diffHunkNewStart :: Int
  , diffHunkNewLines :: Int
  , diffHunkHeader :: String
} deriving (Show, Eq)
{#pointer *diff_hunk as DiffHunkPtr -> DiffHunk#}
instance Storable DiffHunk where
  sizeOf _ = {#sizeof diff_hunk#}
  alignment _ = {#alignof diff_hunk#}
  peek p = do
    oldStart <- fromIntegral <$> ({#get diff_hunk->old_start #} p)
    oldLines <- fromIntegral <$> ({#get diff_hunk->old_lines #} p)
    newStart <- fromIntegral <$> ({#get diff_hunk->new_start #} p)
    newLines <- fromIntegral <$> ({#get diff_hunk->new_lines #} p)
    header <- peekCString =<< ({#get diff_hunk->header #} p)
    pure $ DiffHunk oldStart oldLines newStart newLines header
  poke _ = error "Can't poke DiffHunk"

data DiffLine = DiffLine
  { diffLineOrigin :: Char
  , diffLineOldLineno :: Int
  , diffLineNewLineno :: Int
  , diffLineNumLines :: Int
  , diffLineContent :: String
  , diffLineContentOffset :: Int
} deriving (Show)
{#pointer *diff_line as DiffLinePtr -> DiffLine#}
instance Storable DiffLine where
  sizeOf _ = {#sizeof diff_line#}
  alignment _ = {#alignof diff_line#}
  peek p = do
    origin <- castCCharToChar <$> ({#get diff_line->origin #} p)
    oldLineno <- fromIntegral <$> ({#get diff_line->old_lineno #} p)
    newLineno <- fromIntegral <$> ({#get diff_line->new_lineno #} p)
    numLines <- fromIntegral <$> ({#get diff_line->num_lines #} p)
    contentLen <- fromIntegral <$> ({#get diff_line->content_len #} p)
    content <- do
       str <- ({#get diff_line->content #} p)
       peekCStringLen (str, contentLen)
    contentOffset <- fromIntegral <$> ({#get diff_line->content_offset #} p)
    pure $ DiffLine origin oldLineno newLineno numLines content contentOffset
  poke _ = error "Can't poke DiffLine"

{#enum diff_binary_t as DiffBinaryType {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

data DiffBinaryFile = DiffBinaryFile
  { diffBinaryFileType :: DiffBinaryType
  , diffBinaryFileData :: ByteString
  , diffBinaryFileInflatedLen :: Int
} deriving (Show)
{#pointer *diff_binary_file as DiffBinaryFilePtr -> DiffBinaryFile#}
instance Storable DiffBinaryFile where
  sizeOf _ = {#sizeof diff_binary_file#}
  alignment _ = {#alignof diff_binary_file#}
  peek p = do
    type_ <- (toEnum . fromIntegral) <$> ({#get diff_binary_file->type #} p)
    dataLen <- fromIntegral <$> ({#get diff_binary_file->datalen #} p)
    data_ <- do
       str <- ({#get diff_binary_file->data #} p)
       packCStringLen (str, dataLen)
    inflatedLen <- fromIntegral <$> ({#get diff_binary_file->inflatedlen #} p)
    pure $ DiffBinaryFile type_ data_ inflatedLen
  poke _ = error "Can't poke DiffBinaryFile"

data DiffBinary = DiffBinary
  { diffBinaryContainsData :: Bool
  , diffBinaryOldFile :: DiffBinaryFile
  , diffBinaryNewFile :: DiffBinaryFile
} deriving (Show)
{#pointer *diff_binary as DiffBinaryPtr -> DiffBinary#}
instance Storable DiffBinary where
  sizeOf _ = {#sizeof diff_binary#}
  alignment _ = {#alignof diff_binary#}
  peek p = do
    containsData <- (== 1) <$> ({#get diff_binary->contains_data #} p)
    oldFile <- peekByteOff p {#offsetof diff_binary->old_file #}
    newFile <- peekByteOff p {#offsetof diff_binary->new_file #}
    pure $ DiffBinary containsData oldFile newFile
  poke _ = error "Can't poke DiffBinary"

{#enum submodule_ignore_t as DiffSubmoduleIgnore {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

{#pointer *diff_options as DiffOptions foreign newtype#}  -- TODO: Add finalizer

instance Storable DiffOptions where
  sizeOf _ = {#sizeof diff_options#}
  alignment _ = {#alignof diff_options#}
  peek = error "Can't peek DiffOptions"
  poke = error "Can't poke DiffOptions"

peekDiffOptions :: Ptr DiffOptions -> IO DiffOptions
peekDiffOptions p = DiffOptions <$> (newForeignPtr_ p)

{#enum diff_find_t as InternalDiffFindFlags {underscoreToCase, upcaseFirstLetter} with prefix = "GIT_DIFF_" add prefix = "internal_diff_" deriving (Eq, Show)#}

newtype DiffFindFlag = DiffFindFlag CUInt deriving (Eq, Show, Bits)

fromDiffFindFlags :: DiffFindFlag -> CUInt
fromDiffFindFlags (DiffFindFlag options) = options

fromDiffFindFlagsEnum :: InternalDiffFindFlags -> DiffFindFlag
fromDiffFindFlagsEnum = DiffFindFlag . fromIntegral . fromEnum

diffFindByConfig :: DiffFindFlag
diffFindByConfig = fromDiffFindFlagsEnum InternalDiffFindByConfig
diffFindRenames :: DiffFindFlag
diffFindRenames = fromDiffFindFlagsEnum InternalDiffFindRenames
diffFindRenamesFromRewrites :: DiffFindFlag
diffFindRenamesFromRewrites = fromDiffFindFlagsEnum InternalDiffFindRenamesFromRewrites
diffFindCopies :: DiffFindFlag
diffFindCopies = fromDiffFindFlagsEnum InternalDiffFindCopies
diffFindCopiesFromUnmodified :: DiffFindFlag
diffFindCopiesFromUnmodified = fromDiffFindFlagsEnum InternalDiffFindCopiesFromUnmodified
diffFindRewrites :: DiffFindFlag
diffFindRewrites = fromDiffFindFlagsEnum InternalDiffFindRewrites
diffBreakRewrites :: DiffFindFlag
diffBreakRewrites = fromDiffFindFlagsEnum InternalDiffBreakRewrites
diffFindAndBreakRewrites :: DiffFindFlag
diffFindAndBreakRewrites = fromDiffFindFlagsEnum InternalDiffFindAndBreakRewrites
diffFindForUntracked :: DiffFindFlag
diffFindForUntracked = fromDiffFindFlagsEnum InternalDiffFindForUntracked
diffFindAll :: DiffFindFlag
diffFindAll = fromDiffFindFlagsEnum InternalDiffFindAll
diffFindIgnoreLeadingWhitespace :: DiffFindFlag
diffFindIgnoreLeadingWhitespace = fromDiffFindFlagsEnum InternalDiffFindIgnoreLeadingWhitespace
diffFindIgnoreWhitespace :: DiffFindFlag
diffFindIgnoreWhitespace = fromDiffFindFlagsEnum InternalDiffFindIgnoreWhitespace
diffFindDontIgnoreWhitespace :: DiffFindFlag
diffFindDontIgnoreWhitespace = fromDiffFindFlagsEnum InternalDiffFindDontIgnoreWhitespace
diffFindExactMatchOnly :: DiffFindFlag
diffFindExactMatchOnly = fromDiffFindFlagsEnum InternalDiffFindExactMatchOnly
diffBreakRewritesForRenamesOnly :: DiffFindFlag
diffBreakRewritesForRenamesOnly = fromDiffFindFlagsEnum InternalDiffBreakRewritesForRenamesOnly
diffFindRemoveUnmodified :: DiffFindFlag
diffFindRemoveUnmodified = fromDiffFindFlagsEnum InternalDiffFindRemoveUnmodified

{#pointer *diff_find_options as DiffFindOptions foreign newtype#}  -- TODO: Add finalizer

instance Storable DiffFindOptions where
  sizeOf _ = {#sizeof diff_find_options#}
  alignment _ = {#alignof diff_find_options#}
  peek = error "Can't peek DiffFindOptions"
  poke = error "Can't poke DiffFindOptions"

peekDiffFindOptions :: Ptr DiffFindOptions -> IO DiffFindOptions
peekDiffFindOptions p = DiffFindOptions <$> (newForeignPtr_ p)
