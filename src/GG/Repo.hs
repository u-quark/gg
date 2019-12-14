{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

  gg is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  gg is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with gg.  If not, see <https://www.gnu.org/licenses/>.
-}
module GG.Repo
  ( readNCommits
  , readRepository
  , readRepoState
  , readCommit
  , readCommitDiff
  , Action
  , doRebase
  , moveCommitUp
  , moveCommitDown
  , fixupCommit
  ) where

import           Data.Char      (toLower)
import           Data.List      (intercalate)
import           Data.Maybe     (fromJust)
import qualified GG.State       as S
import           Libgit2        (Commit, DiffInfo, DiffStats, OID, Reference,
                                 Repository, commitAuthor, commitBody,
                                 commitCommitter, commitId, commitLookup,
                                 commitParent, commitParentCount, commitSummary,
                                 commitTree, diffDefaultOptions, diffFindAll,
                                 diffFindDefaultOptions, diffFindSimilar,
                                 diffGetStats, diffInfo, diffTreeToTree,
                                 libgit2Init, pokeDiffFindFlags,
                                 referenceResolve, referenceShorthand,
                                 referenceTarget, repositoryHead,
                                 repositoryOpenExt, repositoryOpenNoFlags,
                                 signatureEmail, signatureName, signatureWhen)
import           System.Exit    (ExitCode (..))
import           System.Process (readCreateProcessWithExitCode, shell)

readCommit :: Commit -> IO S.Commit
readCommit commit = do
  oid <- commitId commit
  summary <- commitSummary commit
  body <- commitBody commit
  author <- commitAuthor commit
  authorName <- signatureName author
  authorEmail <- signatureEmail author
  authorWhen <- signatureWhen author
  committer <- commitCommitter commit
  committerName <- signatureName committer
  committerEmail <- signatureEmail committer
  committerWhen <- signatureWhen committer
  pure $ S.Commit oid summary body authorName authorEmail authorWhen committerName committerEmail committerWhen False

readCommitDiff :: Repository -> OID -> IO (DiffStats, DiffInfo)
readCommitDiff repo oid = do
  commit <- commitLookup repo oid
  tree <- commitTree commit
  parentCommit <- commitParent commit 0
  parentTree <- commitTree parentCommit
  diffOptions <- diffDefaultOptions
  diff <- diffTreeToTree repo parentTree tree diffOptions
  diffFindOptions <- diffFindDefaultOptions
  pokeDiffFindFlags diffFindOptions diffFindAll
  diffFindSimilar diff diffFindOptions
  diffStats <- diffGetStats diff
  diffInfo_ <- diffInfo diff
  pure (diffStats, diffInfo_)

readNCommits :: Int -> Commit -> IO ([Commit], Commit)
readNCommits n leaf = loop 0 leaf []
  where
    loop :: Int -> Commit -> [Commit] -> IO ([Commit], Commit)
    loop i c acc =
      if i == n
        then pure (reverse acc, c)
        else do
          parentCount <- commitParentCount c
          if parentCount == 0
            then pure (reverse acc, c)
            else do
              commit <- commitParent c 0
              loop (i + 1) commit (commit : acc)

readRepository :: IO Repository
readRepository = do
  _ <- libgit2Init
  repositoryOpenExt "." repositoryOpenNoFlags ""

refToCommit :: Repository -> Reference -> IO Commit
refToCommit repo ref = do
  ref' <- referenceResolve ref
  oid <- fromJust <$> referenceTarget ref'
  commitLookup repo oid

readRepoState :: Repository -> IO (String, Commit)
readRepoState repo = do
  headRef <- repositoryHead repo
  branch <- referenceShorthand headRef
  commit <- refToCommit repo headRef
  pure (branch, commit)

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
