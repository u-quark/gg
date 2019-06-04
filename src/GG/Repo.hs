module GG.Repo
  ( readNCommits
  , readRepository
  ) where

import           Control.Monad.Loops (whileM)
import qualified GG.UI               as S
import           Libgit2             (IterResult (..), OID, Repository, Revwalk,
                                      Signature (..), commitAuthor,
                                      commitLookup, commitSummary, libgit2Init,
                                      newOID, oidToStrS, repositoryOpenExt,
                                      repositoryOpenNoFlags, revwalkNew,
                                      revwalkNext, revwalkPushHead)

readNCommits :: Int -> Repository -> Revwalk -> IO [S.Commit]
readNCommits n repo revwalk = do
  oid <- newOID
  commits <- whileM (hasCommit oid) (readCommit oid)
  pure $ take n commits
  where
    hasCommit :: OID -> IO Bool
    hasCommit oid = do
      ir <- revwalkNext oid revwalk
      case ir of
        IterHasMore -> pure True
        IterOver    -> pure False
    readCommit :: OID -> IO S.Commit
    readCommit oid = do
      oidStr <- oidToStrS oid
      commit <- commitLookup repo oid
      summary <- commitSummary commit
      author <- commitAuthor commit
      pure $ S.Commit oidStr summary (signatureName author) (signatureEmail author) (signatureWhen author)

readRepository :: IO (Repository, Revwalk)
readRepository = do
  _ <- libgit2Init
  repo <- repositoryOpenExt "." repositoryOpenNoFlags ""
  revwalk <- revwalkNew repo
  revwalkPushHead revwalk
  pure (repo, revwalk)
