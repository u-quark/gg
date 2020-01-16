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
module GG.Main
  ( main
  ) where

import           Brick.BChan (newBChan)
import qualified GG.Repo     as R
import qualified GG.State    as S
import           GG.Timers   (initTimers)
import qualified GG.UI       as UI
import           Prelude     hiding (head)

main :: IO ()
main = do
  repo <- R.readRepository
  (head, headCommit) <- R.readRepoState repo
  (tailCommits, contCommit) <- R.readNCommits 999 headCommit
  bChan <- newBChan 10
  timers <- initTimers S.Tick 100000 bChan
  commitsState <- mapM R.readCommit (headCommit : tailCommits)
  UI.main bChan $ S.initState repo contCommit head commitsState timers
