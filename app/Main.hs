module Main where

import           GG.Repo  (readCommit, readNCommits, readRepoState,
                           readRepository)
import qualified GG.State as S
import qualified GG.UI    as UI

main :: IO ()
main = do
  repo <- readRepository
  (branch, headCommit) <- readRepoState repo
  (tailCommits, contCommit) <- readNCommits 999 headCommit
  commitsState <- mapM readCommit (headCommit : tailCommits)
  UI.main $ S.initState repo contCommit branch commitsState
