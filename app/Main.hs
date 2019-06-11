module Main where

import           GG.Repo  (readCommits, readNCommits, readRepository)
import qualified GG.State as S
import qualified GG.UI    as UI

main :: IO ()
main = do
  repo <- readRepository
  (revwalk, branch) <- readCommits repo
  commits <- readNCommits 1000 repo revwalk
  UI.main $ S.initState repo revwalk branch commits
