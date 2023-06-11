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
module Libgit2.OID
  ( OID(..)
  , newOID
  , withOID
  , oidToStrS
  , _oidFromCString
  ) where

import Foreign (Ptr, Storable(..), mallocForeignPtr)
import Foreign.C (CUChar)

#include "git2/oid.h"

{#context lib="git2" prefix="git_"#}

instance Storable OID where
    sizeOf _ = {#sizeof oid#}
    alignment _ = {#alignof oid#}
    peek _ = error "Can't peek OID"
    poke _ = error "Can't poke OID"

instance Eq OID where
    (==) = _equalOID

{#pointer *oid as OID foreign newtype#}

newOID :: IO OID
newOID = do
    p <- mallocForeignPtr
    pure $ OID p

{#fun oid_tostr_s as oidToStrS { `OID' } -> `String' #}

{#fun pure oid_tostr_s as _showOID { `OID' } -> `String' #}

{#fun pure oid_equal as _equalOID { `OID', `OID' } -> `Bool' #}

instance Show OID where
  show = _showOID

_oidFromCString :: Ptr CUChar -> IO OID
_oidFromCString str = do
  oid <- newOID
  withOID oid $ flip {#call oid_fromraw #} str
  pure oid
