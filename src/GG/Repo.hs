module GG.Repo
  ( readNCommits
  , readRepository
  ) where

import qualified GG.State as S
import           Libgit2  (IterResult (..), OID, Repository, Revwalk,
                           Signature (..), commitAuthor, commitLookup,
                           commitSummary, libgit2Init, newOID, oidToStrS,
                           repositoryOpenExt, repositoryOpenNoFlags, revwalkNew,
                           revwalkNext, revwalkPushHead)

readNCommits :: Int -> Repository -> Revwalk -> IO [S.Commit]
readNCommits n repo revwalk = do
  oid <- newOID
  loop 0 oid []
  where
    loop :: Int -> OID -> [S.Commit] -> IO [S.Commit]
    loop i oid acc =
      if i == n
        then pure $ reverse acc
        else do
          ir <- revwalkNext oid revwalk
          case ir of
            IterHasMore -> do
              oidStr <- oidToStrS oid
              commit <- commitLookup repo oid
              summary <- commitSummary commit
              author <- commitAuthor commit
              let c = S.Commit oidStr summary (signatureName author) (signatureEmail author) (signatureWhen author)
              loop (i + 1) oid (c : acc)
            IterOver -> pure $ reverse acc

readRepository :: IO (Repository, Revwalk)
readRepository = do
  _ <- libgit2Init
  repo <- repositoryOpenExt "." repositoryOpenNoFlags ""
  revwalk <- revwalkNew repo
  revwalkPushHead revwalk
  pure (repo, revwalk)
