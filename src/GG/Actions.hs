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
module GG.Actions
  ( Action(..)
  , RebaseAction(..)
  , ActionSummary(..)
  , ActionOutcome(..)
  , ActionFailure(..)
  , ActionWarning(..)
  , UndoFailure(..)
  , doAction
  ) where

import           GG.Actions.Common
import           GG.Actions.Rebase (doRebaseAction)
import           GG.Actions.Undo   (doRedo, doUndo)
import qualified Libgit2           as G

doAction :: G.Repository -> Action -> IO ActionOutcome
doAction repo (RebaseAction pos rebaseAction) = doRebaseAction repo pos rebaseAction
doAction repo UndoA = doUndo repo
doAction repo RedoA = doRedo repo
