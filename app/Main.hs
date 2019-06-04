module Main where

import           GG.Repo (readNCommits, readRepository)
import qualified GG.UI   as UI

main :: IO ()
main = do
  (repo, revwalk) <- readRepository
  commits <- readNCommits 1000 repo revwalk
  UI.main $ UI.initState commits
