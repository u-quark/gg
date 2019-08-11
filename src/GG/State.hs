{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module GG.State where

import qualified Brick.Widgets.List           as L
import           Control.Lens                 (over, set)
import           Data.Generics.Product.Fields (field)
import           Data.Time                    (ZonedTime)
import qualified Data.Vector                  as Vec
import           GHC.Generics                 (Generic)
import qualified Libgit2                      as G

data Name =
  CommitList
  deriving (Eq, Ord, Show)

data Commit =
  Commit
    { oid         :: String
    , summary     :: String
    , authorName  :: String
    , authorEmail :: String
    , authorWhen  :: ZonedTime
    }
  deriving (Generic)

data State =
  State
    { commitList :: L.List Name Commit
    , branchName :: String
    , repository :: G.Repository
    , contCommit :: G.Commit
    }
  deriving (Generic)

initState :: G.Repository -> G.Commit -> String -> [Commit] -> State
initState repo commit branch l = State (L.list CommitList (Vec.fromList l) 1) branch repo commit

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
