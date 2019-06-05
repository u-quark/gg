module Libgit2.Refs
  (
    referenceShorthand
  ) where

{#import Libgit2.Types#}

#include "git2/refs.h"

{#context lib="git2" prefix="git_"#}

{#fun unsafe reference_shorthand as referenceShorthand { `Reference' } -> `String'#}
