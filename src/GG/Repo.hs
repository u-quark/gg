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
  G.pokeDiffFindFlags diffFindOptions G.diffFindAll
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

data ActionOutcome
  = Success
      { newCursorPosition :: Int
      }
  | InvalidAction
  | ApplyFailed
      { conflictMessage :: String
      }

doAction :: G.Repository -> G.Reference -> [G.OID] -> Int -> Action -> IO ActionOutcome
doAction repo ref commitOIDs pos action = do
  let aM = action commitOIDs pos
  case aM of
    Just (Plan base commands newPos) -> do
      res <- loop base commands
      case res of
        Right oid -> do
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
doCommand (Apply oid) repo baseOid = doCommand_ repo oid baseOid getMessageAndAuthor False
  where
    getMessageAndAuthor _baseMessage cherryMessage _baseAuthor cherryAuthor = pure (cherryMessage, cherryAuthor)
doCommand (Squash oid messageSquashStrategy) repo baseOid =
  doCommand_ repo oid baseOid (squashCommitInfo messageSquashStrategy) True

doCommand_ ::
     G.Repository
  -> G.OID
  -> G.OID
  -> (String -> String -> G.Signature -> G.Signature -> IO (String, G.Signature))
  -> Bool
  -> IO (Either String G.OID)
doCommand_ repo oid baseOid getMessageAndAuthor isSquash = do
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
        Left (G.Libgit2Exception _ _) -> pure $ Left $ "Conflicts applying commit \"" <> summary <> "\""
        Right index -> do
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
          newCommitOid <-
            G.commitCreate repo Nothing author committer "UTF-8" message newTree (length newParents) newParents
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
