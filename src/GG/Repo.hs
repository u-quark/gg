{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2023  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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
  , refToCommit
  , module GG.Repo.Commit
  ) where

import           Data.Maybe     (fromJust)
import           GG.Repo.Commit
import qualified GG.State       as S
import qualified Libgit2        as G

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
