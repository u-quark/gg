{-# LANGUAGE TemplateHaskell #-}

module GG.State where

import qualified Brick.Widgets.List as L
import           Control.Lens       (over, set)
import           Control.Lens.TH    (makeLenses)
import           Data.Time          (ZonedTime)
import qualified Data.Vector        as Vec
import qualified Libgit2            as G

data Name =
  CommitList
  deriving (Eq, Ord, Show)

data Commit =
  Commit
    { _oid         :: String
    , _summary     :: String
    , _authorName  :: String
    , _authorEmail :: String
    , _authorWhen  :: ZonedTime
    }

data State =
  State
    { _commitList :: L.List Name Commit
    , _branchName :: String
    , _repository :: G.Repository
    , _contCommit :: G.Commit
    }

makeLenses ''Commit

makeLenses ''State

initState :: G.Repository -> G.Commit -> String -> [Commit] -> State
initState repo commit branch l = State (L.list CommitList (Vec.fromList l) 1) branch repo commit

updateRepoState :: G.Commit -> String -> [Commit] -> State -> State
updateRepoState commit branch l =
  set (commitList . L.listElementsL) (Vec.fromList l) . set contCommit commit . set branchName branch

addMoreCommits :: [Commit] -> G.Commit -> State -> State
addMoreCommits moreCommits newContCommit =
  over (commitList . L.listElementsL) (Vec.++ Vec.fromList moreCommits) . set contCommit newContCommit

updateCommitsPos :: Int -> State -> State
updateCommitsPos pos = set (commitList . L.listSelectedL) (Just pos)
