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
module Libgit2.Refs
  ( referenceShorthand
  , referenceTarget
  , referenceResolve
  ) where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import Foreign (alloca, Ptr, newForeignPtr_, maybePeek)
import Libgit2.Errors (checkReturnCode)

#include "git2/refs.h"

{#context lib="git2" prefix="git_"#}

{#fun reference_shorthand as referenceShorthand { `Reference' } -> `String'#}

maybeNullOID :: Ptr OID -> IO (Maybe OID)
maybeNullOID = maybePeek (fmap OID . newForeignPtr_)

{#fun reference_target as referenceTarget { `Reference' } -> `Maybe OID' maybeNullOID*#}

{#fun reference_resolve as referenceResolve {  alloca- `Reference' peekNewReference*, `Reference' } -> `Int' checkReturnCode*-#}
