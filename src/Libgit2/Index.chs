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
module Libgit2.Index
  ( indexHasConflicts
  , indexWriteTree
  ) where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import qualified Foreign.Ptr as C2HSImp
import Libgit2.Errors (checkReturnCode)

#include "git2/index.h"

{#context lib="git2" prefix="git_"#}

{#fun index_has_conflicts as indexHasConflicts { `Index' } -> `Bool'#}

{#fun index_write_tree as _indexWriteTree { `OID', `Index' } -> `Int' checkReturnCode*-#}

indexWriteTree :: Index -> IO OID
indexWriteTree index = do
  oid <- newOID
  _indexWriteTree oid index
  pure oid
