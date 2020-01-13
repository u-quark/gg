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
  , Action(..)
  , RebaseAction(..)
  , ActionOutcome(..)
  , doAction
  , reflogSuffix
  ) where

import           Data.Maybe        (fromJust)
import           GG.Actions.Common
import           GG.Actions.Undo   (doRedo, doUndo)
import qualified GG.State          as S
import qualified Libgit2           as G

readCommit :: G.Commit -> IO S.Commit
readCommit commit = do
  oid <- G.commitId commit
  summary <- G.commitSummary commit
  body <- G.commitBody commit
  author <- G.commitAuthor commit
  authorName <- G.signatureName author
  authorEmail <- G.signatureEmail author
  authorWhen <- G.signatureWhen author
  committer <- G.commitCommitter commit
  committerName <- G.signatureName committer
  committerEmail <- G.signatureEmail committer
  committerWhen <- G.signatureWhen committer
  pure $ S.Commit oid summary body authorName authorEmail authorWhen committerName committerEmail committerWhen False

readCommitDiff :: G.Repository -> G.OID -> IO (G.DiffStats, G.DiffInfo)
readCommitDiff repo oid = do
  commit <- G.commitLookup repo oid
  tree <- G.commitTree commit
  parentCommit <- G.commitParent commit 0
  parentTree <- G.commitTree parentCommit
  diffOptions <- G.diffDefaultOptions
  diff <- G.diffTreeToTree repo parentTree tree diffOptions
  diffFindOptions <- G.diffFindDefaultOptions
  G.pokeDiffFindOptionsFlags diffFindOptions G.diffFindAll
  G.diffFindSimilar diff diffFindOptions
  diffStats <- G.diffGetStats diff
  diffInfo_ <- G.diffInfo diff
  pure (diffStats, diffInfo_)

readNCommits :: Int -> G.Commit -> IO ([G.Commit], G.Commit)
readNCommits n leaf = loop 0 leaf []
  where
    loop :: Int -> G.Commit -> [G.Commit] -> IO ([G.Commit], G.Commit)
    loop i c acc =
      if i == n
        then pure (reverse acc, c)
        else do
          parentCount <- G.commitParentCount c
          if parentCount == 0
            then pure (reverse acc, c)
            else do
              commit <- G.commitParent c 0
              loop (i + 1) commit (commit : acc)

readRepository :: IO G.Repository
readRepository = do
  _ <- G.libgit2Init
  G.repositoryOpenExt "." G.repositoryOpenNoFlags ""

refToCommit :: G.Repository -> G.Reference -> IO G.Commit
refToCommit repo ref = do
  ref' <- G.referenceResolve ref
  oid <- fromJust <$> G.referenceTarget ref'
  G.commitLookup repo oid

readRepoState :: G.Repository -> IO (S.Reference, G.Commit)
readRepoState repo = do
  headRef <- G.repositoryHead repo
  headName <- G.referenceShorthand headRef
  commit <- refToCommit repo headRef
  pure (S.Reference headRef headName, commit)

doAction :: G.Repository -> Action -> IO ActionOutcome
doAction repo (RebaseAction pos action) = do
  ref <- G.repositoryHead repo
  headCommit <- refToCommit repo ref
  (tailCommits, _) <- readNCommits (pos + 3) headCommit
  oids <- traverse G.commitId (headCommit : tailCommits)
  let aM = toRebaseActionPlanner action oids pos
  case aM of
    Just (Plan base commands newPos summary) -> do
      res <- loop base commands
      case res of
        Right oid -> do
          summaryStr <- traverse (oidToCommitMessage repo) summary
          _ <- G.referenceSetTarget ref oid (describeActionSummary summaryStr <> reflogSuffix)
          pure $ Success newPos summaryStr
        Left failure -> pure $ Failure failure
    Nothing -> pure $ Failure InvalidAction
  where
    loop baseOid [] = pure $ Right baseOid
    loop baseOid (c:cs) = do
      res <- doCommand c repo baseOid
      case res of
        Right newBaseOid -> loop newBaseOid cs
        Left _           -> pure res
    oidToCommitMessage repository oid = do
      commit <- G.commitLookup repository oid
      G.commitSummary commit
doAction repo UndoA = doUndo repo
doAction repo RedoA = doRedo repo

data Plan
  -- Plan baseCommit commands newCursorPosition actionSummary
      =
  Plan G.OID [Command] Int (ActionSummary G.OID)

data Command
  -- ApplyC commit
  = ApplyC G.OID
  -- SquashC commit messageStrategy
  | SquashC G.OID MessageSquashStrategy

data MessageSquashStrategy
  = KeepBase
  | MergeAtBottom

doCommand :: Command -> G.Repository -> G.OID -> IO (Either ActionFailure G.OID)
doCommand (ApplyC oid) repo baseOid = doCommand_ repo oid baseOid getMessageAndAuthor False
  where
    getMessageAndAuthor _baseMessage cherryMessage _baseAuthor cherryAuthor = pure (cherryMessage, cherryAuthor)
doCommand (SquashC oid messageSquashStrategy) repo baseOid =
  doCommand_ repo oid baseOid (squashCommitInfo messageSquashStrategy) True

doCommand_ ::
     G.Repository
  -> G.OID
  -> G.OID
  -> (String -> String -> G.Signature -> G.Signature -> IO (String, G.Signature))
  -> Bool
  -> IO (Either ActionFailure G.OID)
doCommand_ repo oid baseOid getMessageAndAuthor isSquash = do
  commit <- G.commitLookup repo oid
  parentCount <- G.commitParentCount commit
  summary <- G.commitSummary commit
  if parentCount > 1
    then pure $ Left $ RebaseMergeCommit summary
    else do
      tree <- G.commitTree commit
      parentCommit <- G.commitParent commit 0
      parentTree <- G.commitTree parentCommit
      baseCommit <- G.commitLookup repo baseOid
      baseSummary <- G.commitSummary baseCommit
      baseTree <- G.commitTree baseCommit
      mergeOptions <- G.mergeDefaultOptions
      index <- G.mergeTrees repo parentTree baseTree tree mergeOptions
      hasConflicts <- G.indexHasConflicts index
      if hasConflicts
        then pure $ Left $ RebaseConflict summary baseSummary
        else do
          cherryMessage <- G.commitMessage commit
          cherryAuthor <- G.commitAuthor commit
          baseMessage <- G.commitMessage baseCommit
          baseAuthor <- G.commitAuthor baseCommit
          (message, author) <- getMessageAndAuthor baseMessage cherryMessage baseAuthor cherryAuthor
          committer <- G.signatureDefault repo
          newTreeOid <- G.indexWriteTreeTo index repo
          newTree <- G.treeLookup repo newTreeOid
          baseCommitParents <- G.commitParents baseCommit
          let newParents =
                if isSquash
                  then baseCommitParents
                  else [baseCommit]
          newCommitOid <- G.commitCreate repo Nothing author committer "UTF-8" message newTree newParents
          pure $ Right newCommitOid

squashCommitInfo :: MessageSquashStrategy -> String -> String -> G.Signature -> G.Signature -> IO (String, G.Signature)
squashCommitInfo KeepBase baseMessage _cherryMessage baseAuthor _cherryAuthor = pure (baseMessage, baseAuthor)
squashCommitInfo MergeAtBottom baseMessage cherryMessage baseAuthor cherryAuthor = do
  baseAuthorName <- G.signatureName baseAuthor
  baseAuthorEmail <- G.signatureEmail baseAuthor
  cherryAuthorName <- G.signatureName cherryAuthor
  cherryAuthorEmail <- G.signatureEmail cherryAuthor
  let mergedMessage =
        baseMessage <> "\n" <> cherryMessage <>
        (if not (baseAuthorName == cherryAuthorName && baseAuthorEmail == cherryAuthorEmail)
           then "\nCo-Authored-By: " <> cherryAuthorName <> " <" <> cherryAuthorEmail <> ">"
           else "")
  pure (mergedMessage, baseAuthor)

describeActionSummary :: ActionSummary String -> String
describeActionSummary (MoveUpS commit aboveCommit) =
  "Move up commit \"" <> commit <> "\" above commit \"" <> aboveCommit <> "\""
describeActionSummary (MoveDownS commit belowCommit) =
  "Move down commit \"" <> commit <> "\" below commit \"" <> belowCommit <> "\""
describeActionSummary (SquashS commit intoCommit) =
  "Squash commit \"" <> commit <> "\" into commit \"" <> intoCommit <> "\""
describeActionSummary (FixupS commit intoCommit) =
  "Fixup commit \"" <> commit <> "\" into commit \"" <> intoCommit <> "\""
describeActionSummary (DeleteS commit) = "Delete commit \"" <> commit <> "\""
describeActionSummary (UndoS summary) = "Undo: " <> summary
describeActionSummary (RedoS summary) = "Redo: " <> summary

type RebaseActionPlanner = [G.OID] -> Int -> Maybe Plan

toRebaseActionPlanner :: RebaseAction -> RebaseActionPlanner
toRebaseActionPlanner MoveUpA   = moveUpP
toRebaseActionPlanner MoveDownA = moveDownP
toRebaseActionPlanner SquashA   = squashP
toRebaseActionPlanner FixupA    = fixupP
toRebaseActionPlanner DeleteA   = deleteP

moveUpP :: RebaseActionPlanner
moveUpP commitOIDs pos =
  case pos of
    x
      | x >= 1 -> Just $ Plan base (reverse $ theRest <> lastTwo) (pos - 1) (MoveUpS (commitOIDs !! pos) base)
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 1)
    lastTwo = [ApplyC $ commitOIDs !! pos, ApplyC $ commitOIDs !! (pos - 1)]
    theRest = [ApplyC c | c <- take (pos - 1) commitOIDs]

moveDownP :: RebaseActionPlanner
moveDownP commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs ->
        Just $ Plan base (reverse $ theRest <> lastTwo) (pos + 1) (MoveDownS (commitOIDs !! (pos + 1)) base)
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 2)
    lastTwo = [ApplyC $ commitOIDs !! (pos + 1), ApplyC $ commitOIDs !! pos]
    theRest = [ApplyC c | c <- take pos commitOIDs]

_squashP :: MessageSquashStrategy -> (G.OID -> G.OID -> ActionSummary G.OID) -> RebaseActionPlanner
_squashP mss summary commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs -> Just $ Plan base (reverse $ theRest <> lastTwo) pos (summary (commitOIDs !! pos) base)
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 2)
    lastTwo = [SquashC (commitOIDs !! pos) mss, ApplyC $ commitOIDs !! (pos + 1)]
    theRest = [ApplyC c | c <- take pos commitOIDs]

squashP :: RebaseActionPlanner
squashP = _squashP MergeAtBottom SquashS

fixupP :: RebaseActionPlanner
fixupP = _squashP KeepBase FixupS

deleteP :: RebaseActionPlanner
deleteP commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs -> Just $ Plan base (reverse theRest) pos (DeleteS (commitOIDs !! pos))
    _ -> Nothing
  where
    base = commitOIDs !! (pos + 1)
    theRest = [ApplyC c | c <- take pos commitOIDs]
