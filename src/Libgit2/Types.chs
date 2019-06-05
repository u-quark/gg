module Libgit2.Types (
    Repository(..)
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
)

where

import Foreign (Ptr, Storable)
import Foreign.C (peekCString)
import Data.Time.LocalTime (ZonedTime, minutesToTimeZone, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Libgit2.Utils (peekNew)

#include <git2/types.h>
#include <git2/repository.h>
#include <git2/commit.h>
#include <git2/revwalk.h>
#include <git2/refs.h>

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
