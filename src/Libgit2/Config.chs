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
module Libgit2.Config (
    configOpenDefault
  , configOpenLevel
)

where

{#import Libgit2.Types#}

import Foreign (alloca)
import Libgit2.Errors (checkReturnCode)

#include "git2/config.h"

{#context lib="git2" prefix="git_"#}

{#fun config_open_default as configOpenDefault { alloca- `Config' peekNewConfig* } -> `Int' checkReturnCode*-#}

{#fun config_open_level as configOpenLevel { alloca- `Config' peekNewConfig*, `Config', `ConfigLevel' } -> `Int' checkReturnCode*-#}
