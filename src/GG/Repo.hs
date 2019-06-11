module GG.Repo
  ( readNCommits
  , readRepository
  , readCommits
  , Action
  , doRebase
  , moveCommitUp
  , moveCommitDown
  , fixupCommit
  ) where

import           Data.Char      (toLower)
import           Data.List      (intercalate)
import qualified GG.State       as S
import           Libgit2        (IterResult (..), OID, Repository, Revwalk,
                                 Signature (..), commitAuthor, commitLookup,
                                 commitSummary, libgit2Init, newOID, oidToStrS,
                                 referenceShorthand, repositoryHead,
                                 repositoryOpenExt, repositoryOpenNoFlags,
                                 revwalkNew, revwalkNext, revwalkPushHead)
import           System.Exit    (ExitCode (..))
import           System.Process (readCreateProcessWithExitCode, shell)

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

readRepository :: IO Repository
readRepository = do
  _ <- libgit2Init
  repositoryOpenExt "." repositoryOpenNoFlags ""

readCommits :: Repository -> IO (Revwalk, String)
readCommits repo = do
  revwalk <- revwalkNew repo
  revwalkPushHead revwalk
  headRef <- repositoryHead repo
  branch <- referenceShorthand headRef
  pure (revwalk, branch)

system :: String -> IO ExitCode
system cmd = do
  (exitCode, _, _) <- readCreateProcessWithExitCode (shell cmd) ""
  pure exitCode

doRebase :: [String] -> Int -> Action -> IO (Maybe Int)
doRebase commitHashes pos action = do
  let aM = action commitHashes pos
  case aM of
    Just (newPos, upto, commitCommands) -> do
      let cmd = formatRebaseCommand upto commitCommands
      rc <- system cmd
      case rc of
        ExitSuccess -> pure $ Just newPos
        ExitFailure _ -> do
          _ <- system "git rebase --abort"
          pure Nothing
    Nothing -> pure Nothing

data RebaseCommand
  = Pick
  | Fixup
  deriving (Show)

type Action = [String] -> Int -> Maybe (Int, Int, [(RebaseCommand, String)])

moveCommitUp :: Action
moveCommitUp commitHashes pos =
  case pos of
    x
      | x >= 1 -> Just (pos - 1, noCommitsToRebase, reverse $ theRest <> lastTwo)
    _ -> Nothing
  where
    noCommitsToRebase = pos + 1
    lastTwo = [(Pick, commitHashes !! pos), (Pick, commitHashes !! (pos - 1))]
    theRest = [(Pick, c) | c <- take (pos - 1) commitHashes]

moveCommitDown :: Action
moveCommitDown commitHashes pos =
  case pos of
    x
      | x < length commitHashes -> Just (pos + 1, noCommitsToRebase, reverse $ theRest <> lastTwo)
    _ -> Nothing
  where
    noCommitsToRebase = pos + 2
    lastTwo = [(Pick, commitHashes !! (pos + 1)), (Pick, commitHashes !! pos)]
    theRest = [(Pick, c) | c <- take pos commitHashes]

fixupCommit :: Action
fixupCommit commitHashes pos =
  case pos of
    x
      | x < length commitHashes -> Just (pos, noCommitsToRebase, reverse $ theRest <> lastTwo)
    _ -> Nothing
  where
    noCommitsToRebase = pos + 2
    lastTwo = [(Fixup, commitHashes !! pos), (Pick, commitHashes !! (pos + 1))]
    theRest = [(Pick, c) | c <- take pos commitHashes]

formatRebaseCommand :: Int -> [(RebaseCommand, String)] -> String
formatRebaseCommand upto commitCommands = "GIT_EDITOR='echo " <> commitsStr <> " >$1' git rebase -i HEAD~" <> show upto
  where
    commitsStr = intercalate "\\\\n" [map toLower (show c) <> " " <> h | (c, h) <- commitCommands]
