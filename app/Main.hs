module Main where

import           GG.Repo  (readNCommits, readRepository)
import qualified GG.State as S
import qualified GG.UI    as UI

main :: IO ()
main = do
  (repo, revwalk) <- readRepository
  commits <- readNCommits 1000 repo revwalk
  UI.main $ S.initState repo revwalk commits
