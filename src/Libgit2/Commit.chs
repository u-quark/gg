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
module Libgit2.Commit (
    commitLookup
  , commitId
  , commitMessageEncoding
  , commitMessage
  , commitSummary
  , commitBody
  , commitTime
  , commitAuthor
  , commitCommitter
  , commitParentcount
  , commitParent
  , commitTree
)

where

{#import Libgit2.Types#}
{#import Libgit2.OID#}

import Foreign (alloca, peek)
import Foreign.C (CString)
import Data.Time.LocalTime (ZonedTime)
import Libgit2.Errors (checkReturnCode)
import Libgit2.Utils (defaultPeekCString, errorPeekCString)

#include "git2/commit.h"

{#context lib="git2" prefix="git_"#}

{#fun commit_lookup as commitLookup { alloca- `Commit' peekNewCommit*, `Repository', `OID' } -> `Int' checkReturnCode*-#}

{#fun commit_id as commitId { `Commit' } -> `OID'#}

nullIsUTF8PeekCString :: CString -> IO String
nullIsUTF8PeekCString = defaultPeekCString "UTF-8"

{#fun commit_message_encoding as commitMessageEncoding { `Commit' } -> `String' nullIsUTF8PeekCString*#}

{#fun commit_message as commitMessage { `Commit' } -> `String'#}

errorSummaryPeekCString :: CString -> IO String
errorSummaryPeekCString = errorPeekCString "Error fetching commit summary"

{#fun commit_summary as commitSummary { `Commit' } -> `String' errorSummaryPeekCString*#}

nullIsEmptyPeekCString :: CString -> IO String
nullIsEmptyPeekCString = defaultPeekCString ""

{#fun commit_body as commitBody { `Commit' } -> `String' nullIsEmptyPeekCString*#}

{#fun commit_time as commitTime' { `Commit' } -> `Int'#}

{#fun commit_time_offset as commitTimeOffset { `Commit' } -> `Int'#}

commitTime :: Commit -> IO ZonedTime
commitTime commit = do
  systemTime <- commitTime' commit
  offset <- commitTimeOffset commit
  pure $ gitToLocalTime (fromIntegral systemTime) offset

{#fun commit_committer as commitCommitter { `Commit' } -> `Signature' peek*#}

{#fun commit_author as commitAuthor { `Commit' } -> `Signature' peek*#}

{#fun commit_parentcount as commitParentcount { `Commit' } -> `Int'#}

{#fun commit_parent as commitParent { alloca- `Commit' peekNewCommit*, `Commit', `Int' } -> `Int' checkReturnCode*-#}

{#fun commit_tree as commitTree { alloca- `Tree' peekNewTree*, `Commit' } -> `Int' checkReturnCode*-#}
