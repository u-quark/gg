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
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module GG.Actions.Common
  ( Action(..)
  , RebaseAction(..)
  , ActionSummary(..)
  , ActionOutcome(..)
  , ActionFailure(..)
  , UndoFailure(..)
  , reflogSuffix
  ) where

data Action
  = RebaseAction Int RebaseAction
  | UndoA
  | RedoA

data RebaseAction
  = MoveUpA
  | MoveDownA
  | SquashA
  | FixupA
  | DeleteA

data ActionSummary commit
  -- MoveUpS commit aboveCommit
  = MoveUpS commit commit
  -- MoveDownS commit belowCommit
  | MoveDownS commit commit
  -- SquashS commit intoCommit
  | SquashS commit commit
  -- FixupS commit intoCommit
  | FixupS commit commit
  -- DeleteS commit
  | DeleteS commit
  -- Undo summary
  | UndoS String
  -- Redo summary
  | RedoS String
  deriving (Functor, Foldable, Traversable)

data ActionOutcome
  = Success
      { newCursorPosition :: Int
      , actionSummary     :: ActionSummary String
      }
  | Failure ActionFailure

data ActionFailure
  = InvalidAction
  | RebaseConflict String String
  | RebaseMergeCommit String
  | UndoFailure UndoFailure
  | RedoFailure UndoFailure

data UndoFailure
  = NoActionsFound
  | CommitNotFound
  | UndoNotFound
  | NotOurAction

reflogSuffix :: String
reflogSuffix = " [gg]"
