{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2020  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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
module Libgit2.Reflog
  ( reflogAppend
  , reflogDelete
  , reflogDrop
  , reflogEntryByIndex
  , reflogEntryCommitter
  , reflogEntryIdNew
  , reflogEntryIdOld
  , reflogEntryMessage
  , reflogEntrycount
  , reflogRead
  , reflogRename
  , reflogWrite
  ) where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import Foreign (alloca)
import Libgit2.Errors (checkReturnCode)

#include "git2/reflog.h"

{#context lib="git2" prefix="git_"#}


{#fun reflog_append as reflogAppend { `Reflog', `OID', `Signature', `String' } -> `Int' checkReturnCode*-#}

{#fun reflog_delete as reflogDelete { `Repository', `String' } -> `Int' checkReturnCode*-#}

{#fun reflog_drop as reflogDrop { `Reflog', `Int', `Bool' } -> `Int' checkReturnCode*-#}

{#fun reflog_entry_byindex as reflogEntryByIndex { `Reflog', `Int' } -> `ReflogEntry'#}

{#fun reflog_entry_committer as reflogEntryCommitter { `ReflogEntry' } -> `Signature'#}

{#fun reflog_entry_id_new as reflogEntryIdNew { `ReflogEntry' } -> `OID'#}

{#fun reflog_entry_id_old as reflogEntryIdOld { `ReflogEntry' } -> `OID'#}

{#fun reflog_entry_message as reflogEntryMessage { `ReflogEntry' } -> `String'#}

{#fun reflog_entrycount as reflogEntrycount { `Reflog' } -> `Int'#}

{#fun reflog_read as reflogRead { alloca- `Reflog' peekNewReflog*, `Repository', `String' } -> `Int' checkReturnCode*-#}

{#fun reflog_rename as reflogRename { `Repository', `String', `String' } -> `Int' checkReturnCode*-#}

{#fun reflog_write as reflogWrite { `Reflog' } -> `Int' checkReturnCode*-#}
