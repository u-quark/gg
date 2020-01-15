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
module GG.Actions.Rebase
  ( doRebaseAction
  ) where

import           GG.Actions.Common
import           GG.Repo           (readNCommits, refToCommit)
import qualified Libgit2           as G

doRebaseAction :: G.Repository -> Int -> RebaseAction -> IO ActionOutcome
doRebaseAction repo pos rebaseAction = do
  ref <- G.repositoryHead repo
  headCommit <- refToCommit repo ref
  (tailCommits, _) <- readNCommits (pos + 3) headCommit
  oids <- traverse G.commitId (headCommit : tailCommits)
  let aM = toRebaseActionPlanner rebaseAction oids pos
  case aM of
    Right (Plan base commands newPos summary) -> do
      res <- loop base commands
      case res of
        Right oid -> do
          summaryStr <- traverse (oidToCommitMessage repo) summary
          _ <- G.referenceSetTarget ref oid (describeActionSummary summaryStr <> reflogSuffix)
          pure $ Success newPos summaryStr
        Left failure -> pure $ Failure failure
    Left failure -> pure $ Failure failure
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

type RebaseActionPlanner = [G.OID] -> Int -> Either ActionFailure Plan

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
      | x >= 1 -> Right $ Plan base (reverse $ theRest <> lastTwo) (pos - 1) (MoveUpS (commitOIDs !! pos) base)
    _ -> Left ReachedTop
  where
    base = commitOIDs !! (pos + 1)
    lastTwo = [ApplyC $ commitOIDs !! pos, ApplyC $ commitOIDs !! (pos - 1)]
    theRest = [ApplyC c | c <- take (pos - 1) commitOIDs]

moveDownP :: RebaseActionPlanner
moveDownP commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs ->
        Right $ Plan base (reverse $ theRest <> lastTwo) (pos + 1) (MoveDownS (commitOIDs !! (pos + 1)) base)
    _ -> Left ReachedBottom
  where
    base = commitOIDs !! (pos + 2)
    lastTwo = [ApplyC $ commitOIDs !! (pos + 1), ApplyC $ commitOIDs !! pos]
    theRest = [ApplyC c | c <- take pos commitOIDs]

_squashP :: MessageSquashStrategy -> (G.OID -> G.OID -> ActionSummary G.OID) -> RebaseActionPlanner
_squashP mss summary commitOIDs pos =
  case pos of
    x
      | x < length commitOIDs -> Right $ Plan base (reverse $ theRest <> lastTwo) pos (summary (commitOIDs !! pos) base)
    _ -> Left ReachedBottom
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
      | x < length commitOIDs -> Right $ Plan base (reverse theRest) pos (DeleteS (commitOIDs !! pos))
    _ -> Left InvalidAction
  where
    base = commitOIDs !! (pos + 1)
    theRest = [ApplyC c | c <- take pos commitOIDs]
