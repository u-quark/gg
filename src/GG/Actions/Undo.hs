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
module GG.Actions.Undo
  ( doUndo
  , doRedo
  ) where

import           Control.Exception (try)
import           Data.List         (isPrefixOf, isSuffixOf)
import           GG.Actions.Common
import qualified Libgit2           as G

isOurEntry :: String -> Bool
isOurEntry = isSuffixOf reflogSuffix

undoPrefix :: String
undoPrefix = "Undo: "

redoPrefix :: String
redoPrefix = "Redo: "

isUndoEntry :: String -> Bool
isUndoEntry = isPrefixOf undoPrefix

isRedoEntry :: String -> Bool
isRedoEntry = isPrefixOf redoPrefix

extractAction :: String -> String
extractAction = reverse . drop (length reflogSuffix) . reverse

extractUndoAction :: String -> String
extractUndoAction = extractAction . drop (length undoPrefix)

type Result = (Either UndoFailure (G.OID, String))

type Loop = G.Reflog -> Int -> Int -> Int -> IO Result

type LoopLogic = G.Repository -> G.ReflogEntry -> String -> Loop -> G.Reflog -> Int -> Int -> Int -> IO Result

undoLoopLogic :: LoopLogic
undoLoopLogic repo entry message loop reflog maxIndex index ignoreStackCounter
  | isUndoEntry message = loop reflog maxIndex (index + 1) (ignoreStackCounter + 1)
  | isRedoEntry message = loop reflog maxIndex (index + 1) (ignoreStackCounter - 1)
  | ignoreStackCounter == 0 = returnCommitIfExists repo entry (extractAction message)
  | otherwise = loop reflog maxIndex (index + 1) (ignoreStackCounter - 1)

redoLoopLogic :: LoopLogic
redoLoopLogic repo entry message loop reflog maxIndex index ignoreStackCounter
  | isUndoEntry message && ignoreStackCounter == 0 = returnCommitIfExists repo entry (extractUndoAction message)
  | isUndoEntry message = loop reflog maxIndex (index + 1) (ignoreStackCounter + 1)
  | isRedoEntry message = loop reflog maxIndex (index + 1) (ignoreStackCounter - 1)
  | ignoreStackCounter == 0 = pure $ Left UndoNotFound
  | otherwise = loop reflog maxIndex (index + 1) (ignoreStackCounter - 1)

returnCommitIfExists :: G.Repository -> G.ReflogEntry -> String -> IO Result
returnCommitIfExists repo entry message = do
  oid <- G.reflogEntryIdOld entry
  commitE <- try $ G.commitLookup repo oid
  case commitE of
    Left (G.Libgit2Exception _ _) -> pure $ Left CommitNotFound
    Right _commit                 -> pure $ Right (oid, message)

findCommit :: LoopLogic -> G.Repository -> IO (Either UndoFailure (G.OID, String))
findCommit loopLogic repo = do
  ref <- G.repositoryHead repo
  refName <- G.referenceName ref
  reflog <- G.reflogRead repo refName
  maxIndex <- G.reflogEntrycount reflog
  loop reflog maxIndex 0 0
  where
    loop :: G.Reflog -> Int -> Int -> Int -> IO (Either UndoFailure (G.OID, String))
    loop reflog maxIndex index ignoreStackCounter =
      if index >= maxIndex
        then pure $ Left NoActionsFound
        else do
          entry <- G.reflogEntryByIndex reflog index
          message <- G.reflogEntryMessage entry
          if isOurEntry message
            then loopLogic repo entry message loop reflog maxIndex index ignoreStackCounter
            else pure $ Left NotOurAction

doUndoOrRedo ::
     (G.Repository -> IO Result)
  -> String
  -> (String -> ActionSummary String)
  -> (UndoFailure -> ActionFailure)
  -> G.Repository
  -> IO ActionOutcome
doUndoOrRedo findCommitFn prefix summaryCtr failureCtr repo = do
  ref <- G.repositoryHead repo
  oidE <- findCommitFn repo
  case oidE of
    Right (oid, summary) -> do
      _ <- G.referenceSetTarget ref oid (prefix <> summary <> reflogSuffix)
      pure $ Success 0 (summaryCtr summary) Nothing
    Left failure -> pure $ Failure $ failureCtr failure

doUndo :: G.Repository -> IO ActionOutcome
doUndo = doUndoOrRedo (findCommit undoLoopLogic) undoPrefix UndoS UndoFailure

doRedo :: G.Repository -> IO ActionOutcome
doRedo = doUndoOrRedo (findCommit redoLoopLogic) redoPrefix RedoS RedoFailure
