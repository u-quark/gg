{-# LANGUAGE TemplateHaskell #-}

module GG.State where

import qualified Brick.Widgets.List as L
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

initState :: Repository -> Revwalk -> [Commit] -> State
initState repo revw l = State (L.list CommitList (Vec.fromList l) 1) "master" repo revw
