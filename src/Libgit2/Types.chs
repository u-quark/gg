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
  , gitToLocalTime
)

where

import Foreign (Ptr)
import Data.Time.LocalTime (ZonedTime, minutesToTimeZone, utcToZonedTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Libgit2.Utils (peekNew)

#include <git2/types.h>
#include <git2/repository.h>
#include <git2/commit.h>
#include <git2/revwalk.h>

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

gitToLocalTime :: POSIXTime -> Int -> ZonedTime
gitToLocalTime posixTime offset =
  utcToZonedTime timezone utcTime
  where
    utcTime = posixSecondsToUTCTime posixTime
    timezone = minutesToTimeZone offset