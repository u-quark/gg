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
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE RecordWildCards       #-}

module GG.State where

import           Brick                        (EventM)
import qualified Brick.Widgets.List           as L
import           Control.Lens                 (Lens', (.=), (%=), (?=),
                                               ix, singular, _Just, use, preuse)
import           Control.Monad                (forM_)
import           Data.Generics.Product.Fields (field)
import           Data.Time                    (ZonedTime)
import qualified Data.Vector                  as Vec
import           GG.Actions.Common            (ActionFailure, ActionWarning)
import           GG.Config                    (Config (..))
import           GG.Timers                    (Timers)
import           GHC.Generics                 (Generic)
import qualified Libgit2                      as G
import           Prelude                      hiding (head)

data Event =
  Tick
  deriving (Eq, Ord, Show)

data Name
  = CommitListUI
  | CommitDiffVP
  | CommitDiffUI
  deriving (Eq, Ord, Show)

data TimerName =
  NotificationT
  deriving (Eq, Ord, Show)

data Commit =
  Commit
    { oid            :: G.OID
    , summary        :: String
    , body           :: String
    , authorName     :: String
    , authorEmail    :: String
    , authorWhen     :: ZonedTime
    , committerName  :: String
    , committerEmail :: String
    , committerWhen  :: ZonedTime
    , open           :: Bool
    }
  deriving (Generic)

data Reference =
  Reference
    { ref       :: G.Reference
    , shorthand :: String
    }
  deriving (Generic)

data OpenCommit =
  OpenCommit
    { openCommitIndex :: Int
    , openCommit      :: Commit
    , diffStats       :: G.DiffStats
    , diffInfo        :: G.DiffInfo
    }
  deriving (Generic)

data Notification
  = ActionFailure ActionFailure
  | ActionWarning ActionWarning

data State =
  State
    { config       :: Config
    , commitList   :: L.List Name Commit
    , head         :: Reference
    , repository   :: G.Repository
    , contCommit   :: G.Commit
    , openCommit   :: Maybe OpenCommit
    , notification :: Maybe (Notification, Double)
    , timers       :: Timers State Name TimerName
    }
  deriving (Generic)

initState :: Config -> G.Repository -> G.Commit -> Reference -> [Commit] -> Timers State Name TimerName -> State
initState config repository contCommit head l timers =
  State {commitList=(L.list CommitListUI (Vec.fromList l) 1), openCommit=Nothing, notification=Nothing, ..}

type ModifyState a = EventM Name State a

updateRepoState :: G.Commit -> Reference -> [Commit] -> ModifyState ()
updateRepoState commit head_ l = do
  field @"commitList" . L.listElementsL .= Vec.fromList l
  field @"contCommit" .= commit
  field @"head" .= head_

addMoreCommits :: [Commit] -> G.Commit -> ModifyState ()
addMoreCommits moreCommits newContCommit = do
  field @"commitList" . L.listElementsL %= flip (Vec.++) (Vec.fromList moreCommits)
  field @"contCommit" .= newContCommit

updateCommitsPos :: Int -> ModifyState ()
updateCommitsPos pos = field @"commitList" . L.listSelectedL .= Just pos

commitL :: Int -> Lens' State Commit
commitL i = field @"commitList" . L.listElementsL . singular (ix i)

openCommitDetails :: Int -> (G.DiffStats, G.DiffInfo) -> ModifyState ()
openCommitDetails i (diffStats, diffInfo) = do
  oldOpenCommitIxM <- preuse (field @"openCommit" . _Just . field @"openCommitIndex")
  forM_ oldOpenCommitIxM (\i' -> commitL i' . field @"open" .= False)
  commitL i . field @"open" .= True
  openCommit <- use (commitL i)
  field @"openCommit" ?= OpenCommit i openCommit diffStats diffInfo

closeCommitDetails :: ModifyState ()
closeCommitDetails = do
  oldOpenCommitIxM <- preuse (field @"openCommit" . _Just . field @"openCommitIndex")
  forM_ oldOpenCommitIxM (\i' -> commitL i' . field @"open" .= False)
  field @"openCommit" .= Nothing
