module Libgit2.Refs
  (
    referenceShorthand
  , referenceTarget
  , referenceResolve
  ) where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import Foreign (alloca, Ptr)
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (maybeNullPeek, fprCtor_)

#include "git2/refs.h"

{#context lib="git2" prefix="git_"#}

{#fun unsafe reference_shorthand as referenceShorthand { `Reference' } -> `String'#}

maybeNullOID :: Ptr OID -> IO (Maybe OID)
maybeNullOID = maybeNullPeek (fprCtor_ OID)

{#fun unsafe reference_target as referenceTarget { `Reference' } -> `Maybe OID' maybeNullOID*#}

{#fun unsafe reference_resolve as referenceResolve {  alloca- `Reference' peekNewReference*, `Reference' } -> `Int' checkReturnCode*-#}
