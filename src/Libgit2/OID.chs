module Libgit2.OID (
    OID(..)
  , newOID
  , withOID
  , oidToStrS
)

where

import Foreign (Storable(..), mallocForeignPtr)

#include "git2/oid.h"

{#context lib="git2" prefix="git_"#}

instance Storable OID where
    sizeOf _ = {#sizeof oid#}
    alignment _ = {#alignof oid#}
    peek _ = error "Can't peek OID"
    poke _ = error "Can't poke OID"

{#pointer *oid as OID foreign newtype#}

newOID :: IO OID
newOID = do
    p <- mallocForeignPtr
    pure $ OID p

{#fun unsafe oid_tostr_s as oidToStrS { `OID' } -> `String' #}
