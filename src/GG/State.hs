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
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeApplications      #-}

module GG.State where

import qualified Brick.Widgets.List           as L
import           Control.Lens                 (Traversal', ix, over, set, (&),
                                               (.~), (?~), (^?), (^?!), _Just)
import           Data.Generics.Product.Fields (field)
import           Data.Time                    (ZonedTime)
import qualified Data.Vector                  as Vec
import           GHC.Generics                 (Generic)
import qualified Libgit2                      as G

data Name
  = CommitListUI
  | CommitDiffVP
  | CommitDiffUI
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

data OpenCommit =
  OpenCommit
    { openCommitIndex :: Int
    , openCommit      :: Commit
    , diffStats       :: G.DiffStats
    , diffInfo        :: G.DiffInfo
    }
  deriving (Generic)

data State =
  State
    { commitList :: L.List Name Commit
    , branchName :: String
    , repository :: G.Repository
    , contCommit :: G.Commit
    , openCommit :: Maybe OpenCommit
    }
  deriving (Generic)

initState :: G.Repository -> G.Commit -> String -> [Commit] -> State
initState repo commit branch l = State (L.list CommitListUI (Vec.fromList l) 1) branch repo commit Nothing

updateRepoState :: G.Commit -> String -> [Commit] -> State -> State
updateRepoState commit branch l =
  set (field @"commitList" . L.listElementsL) (Vec.fromList l) .
  set (field @"contCommit") commit . set (field @"branchName") branch

addMoreCommits :: [Commit] -> G.Commit -> State -> State
addMoreCommits moreCommits newContCommit =
  over (field @"commitList" . L.listElementsL) (Vec.++ Vec.fromList moreCommits) .
  set (field @"contCommit") newContCommit

updateCommitsPos :: Int -> State -> State
updateCommitsPos pos = set (field @"commitList" . L.listSelectedL) (Just pos)

commitL :: Int -> Traversal' State Commit
commitL i = field @"commitList" . L.listElementsL . ix i

openCommitDetails :: Int -> (G.DiffStats, G.DiffInfo) -> State -> State
openCommitDetails i (diffStats_, diffInfo_) state =
  state' & commitL i .~ openCommit_ & field @"openCommit" ?~ OpenCommit i openCommit_ diffStats_ diffInfo_
  where
    openCommit_ = (state ^?! commitL i) & field @"open" .~ True
    oldOpenCommitIxM = state ^? (field @"openCommit" . _Just . field @"openCommitIndex")
    state' = maybe state (\i' -> state & (commitL i' . field @"open") .~ False) oldOpenCommitIxM

closeCommitDetails :: State -> State
closeCommitDetails state =
  maybe state (\i -> state & (commitL i . field @"open") .~ False) oldOpenCommitIxM & field @"openCommit" .~ Nothing
  where
    oldOpenCommitIxM = state ^? (field @"openCommit" . _Just . field @"openCommitIndex")
