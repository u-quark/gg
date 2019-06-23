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
  , DiffFilePtr
  , DiffDelta
  , DiffDeltaPtr
  , DiffSubmoduleIgnore(..)
  , DiffOptions(..)
  , withDiffOptions
  , peekDiffOptions
)

where

import Foreign (Ptr, Storable, peek, castPtr, newForeignPtr_)
import Foreign.C (peekCString, CUInt, CLong)
import Data.Bits (Bits)
import Data.Time.LocalTime (ZonedTime, minutesToTimeZone, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
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
} deriving (Show)
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

newtype Similarity = Similarity Int deriving (Num, Show)
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
  , diffDeltaNewFIle :: DiffFile
} deriving (Show)
{#pointer *diff_delta as DiffDeltaPtr -> DiffDelta#}
instance Storable DiffDelta where
  sizeOf _ = {#sizeof diff_delta#}
  alignment _ = {#alignof diff_delta#}
  peek p = do
    status <- (toEnum . fromIntegral) <$> ({#get diff_delta->status #} p)
    flags <- DiffFlags <$> ({#get diff_delta->flags #} p)
    similarity <- (Similarity . fromIntegral) <$> ({#get diff_delta->similarity #} p)
    nFiles <- fromIntegral <$> ({#get diff_delta->nfiles #} p)
    oldFile <- peek =<< castPtr <$> ({#get diff_delta->old_file #} p)
    newFile <- peek =<< castPtr <$> ({#get diff_delta->new_file #} p)
    pure $ DiffDelta status flags similarity nFiles oldFile newFile
  poke _ = error "Can't poke DiffDelta"

{#enum submodule_ignore_t as DiffSubmoduleIgnore {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

{#pointer *diff_options as DiffOptions foreign newtype#}  -- TODO: Add finalizer

instance Storable DiffOptions where
  sizeOf _ = {#sizeof diff_options#}
  alignment _ = {#alignof diff_options#}
  peek = error "Can't peek DiffOptions"
  poke = error "Can't poke DiffOptions"

peekDiffOptions :: Ptr DiffOptions -> IO DiffOptions
peekDiffOptions p = DiffOptions <$> (newForeignPtr_ p)
