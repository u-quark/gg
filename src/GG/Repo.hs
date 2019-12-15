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
  , ActionOutcome(..)
  , doAction
  , moveCommitUp
  , moveCommitDown
  , squashCommit
  , fixupCommit
  ) where

import           Control.Exception (try)
import           Data.Maybe        (fromJust)
import qualified GG.State          as S
import           Libgit2           (Commit, DiffInfo, DiffStats,
                                    Libgit2Exception (..), OID, Reference,
                                    Repository, commitAuthor, commitBody,
                                    commitCommitter, commitId, commitLookup,
                                    commitParent, commitParentCount,
                                    commitSummary, commitTree,
                                    diffDefaultOptions, diffFindAll,
                                    diffFindDefaultOptions, diffFindSimilar,
                                    diffGetStats, diffInfo, diffTreeToTree,
                                    libgit2Init, pokeDiffFindFlags,
                                    referenceResolve, referenceShorthand,
                                    referenceTarget, repositoryHead,
                                    repositoryOpenExt, repositoryOpenNoFlags,
                                    signatureEmail, signatureName,
                                    signatureWhen)
import qualified Libgit2           as G

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

data ActionOutcome
  = Success
      { newCursorPosition :: Int
      }
  | InvalidAction
  | ApplyFailed
      { conflictMessage :: String
      }

doAction :: G.Repository -> String -> [G.OID] -> Int -> Action -> IO ActionOutcome
doAction repo branch commitOIDs pos action = do
  let aM = action commitOIDs pos
  case aM of
    Just (Plan base commands newPos) -> do
      res <- loop base commands
      case res of
        Right oid -> do
          ref <- G.referenceLookup repo "refs/heads/master"
          _ <- G.referenceSetTarget ref oid "gg - apply"
          pure $ Success newPos
        Left message -> pure $ ApplyFailed message
    Nothing -> pure InvalidAction
  where
    loop baseOid [] = pure $ Right baseOid
    loop baseOid (c:cs) = do
      res <- doCommand c repo baseOid
      case res of
        Right newBaseOid -> loop newBaseOid cs
        Left _           -> pure res

doCommand :: Command -> G.Repository -> G.OID -> IO (Either String G.OID)
doCommand (Apply oid) repo baseOid = do
  commit <- G.commitLookup repo oid
  parentCount <- G.commitParentCount commit
  summary <- G.commitSummary commit
  if parentCount > 1
    then pure $ Left $ "Can not apply merge commit \"" <> summary <> "\""
    else do
      tree <- G.commitTree commit
      parentCommit <- G.commitParent commit 0
      parentTree <- G.commitTree parentCommit
      diffOptions <- G.diffDefaultOptions
      diff <- G.diffTreeToTree repo parentTree tree diffOptions
      baseCommit <- G.commitLookup repo baseOid
      baseTree <- G.commitTree baseCommit
      applyOptions <- G.applyDefaultOptions
      indexE <- try $ G.applyToTree repo baseTree diff applyOptions
      case indexE of
        Left (Libgit2Exception _ _) -> pure $ Left $ "Conflicts applying commit \"" <> summary <> "\""
        Right index -> do
          message <- G.commitMessage commit
          author <- G.commitAuthor commit
          committer <- G.signatureDefault repo
          newTreeOid <- G.indexWriteTreeTo index repo
          newTree <- G.treeLookup repo newTreeOid
          newCommitOid <- G.commitCreate repo Nothing author committer "UTF-8" message newTree 1 [baseCommit]
          pure $ Right newCommitOid

data MessageSquashStrategy
  = KeepBase
  | MergeAtBottom

data Command
  -- Apply commit
  = Apply G.OID
  -- Squash commit messageStrategy
  | Squash G.OID MessageSquashStrategy

data Plan =
  Plan G.OID [Command] Int
  -- Plan baseCommit commands newCursorPosition

type Action = [G.OID] -> Int -> Maybe Plan

moveCommitUp :: Action
moveCommitUp commitOIDs pos =
  case pos of
    x
      | x >= 1 -> Just $ Plan base (reverse $ theRest <> lastTwo) (pos - 1)
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 1)
    lastTwo = [Apply $ commitOIDs !! pos, Apply $ commitOIDs !! (pos - 1)]
    theRest = [Apply c | c <- take (pos - 1) commitOIDs]

moveCommitDown :: Action
moveCommitDown commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs -> Just $ Plan base (reverse $ theRest <> lastTwo) (pos + 1)
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 2)
    lastTwo = [Apply $ commitOIDs !! (pos + 1), Apply $ commitOIDs !! pos]
    theRest = [Apply c | c <- take pos commitOIDs]

_squashCommit :: MessageSquashStrategy -> Action
_squashCommit mss commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs -> Just $ Plan base (reverse $ theRest <> lastTwo) pos
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 2)
    lastTwo = [Squash (commitOIDs !! pos) mss, Apply $ commitOIDs !! (pos + 1)]
    theRest = [Apply c | c <- take pos commitOIDs]

squashCommit :: Action
squashCommit = _squashCommit MergeAtBottom

fixupCommit :: Action
fixupCommit = _squashCommit KeepBase
