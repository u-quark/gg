{-# LANGUAGE TemplateHaskell #-}

module GG.State where

import qualified Brick.Widgets.List as L
import           Control.Lens       (set)
import           Control.Lens.TH    (makeLenses)
import           Data.Time          (ZonedTime)
import qualified Data.Vector        as Vec
import           Libgit2            (Repository, Revwalk)

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
    , _repository :: Repository
    , _revwalk    :: Revwalk
    }

makeLenses ''Commit

makeLenses ''State

initState :: Repository -> Revwalk -> String -> [Commit] -> State
initState repo revw branch l = State (L.list CommitList (Vec.fromList l) 1) branch repo revw

updateRepoState :: Revwalk -> String -> [Commit] -> State -> State
updateRepoState revw branch l =
  set (commitList . L.listElementsL) (Vec.fromList l) . set revwalk revw . set branchName branch

updateCommitsPos :: Int -> State -> State
updateCommitsPos pos = set (commitList . L.listSelectedL) (Just pos)
