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

{#fun unsafe commit_lookup as commitLookup { alloca- `Commit' peekNewCommit*, `Repository', `OID' } -> `Int' checkReturnCode*-#}

{#fun unsafe commit_id as commitId { `Commit' } -> `OID'#}

nullIsUTF8PeekCString :: CString -> IO String
nullIsUTF8PeekCString = defaultPeekCString "UTF-8"

{#fun unsafe commit_message_encoding as commitMessageEncoding { `Commit' } -> `String' nullIsUTF8PeekCString*#}

{#fun unsafe commit_message as commitMessage { `Commit' } -> `String'#}

errorSummaryPeekCString :: CString -> IO String
errorSummaryPeekCString = errorPeekCString "Error fetching commit summary"

{#fun unsafe commit_summary as commitSummary { `Commit' } -> `String' errorSummaryPeekCString*#}

nullIsEmptyPeekCString :: CString -> IO String
nullIsEmptyPeekCString = defaultPeekCString ""

{#fun unsafe commit_body as commitBody { `Commit' } -> `String' nullIsEmptyPeekCString*#}

{#fun unsafe commit_time as commitTime' { `Commit' } -> `Int'#}

{#fun unsafe commit_time_offset as commitTimeOffset { `Commit' } -> `Int'#}

commitTime :: Commit -> IO ZonedTime
commitTime commit = do
  systemTime <- commitTime' commit
  offset <- commitTimeOffset commit
  pure $ gitToLocalTime (fromIntegral systemTime) offset

{#fun unsafe commit_committer as commitCommitter { `Commit' } -> `Signature' peek*#}

{#fun unsafe commit_author as commitAuthor { `Commit' } -> `Signature' peek*#}

{#fun unsafe commit_parentcount as commitParentcount { `Commit' } -> `Int'#}

{#fun unsafe commit_parent as commitParent { alloca- `Commit' peekNewCommit*, `Commit', `Int' } -> `Int' checkReturnCode*-#}

{#fun unsafe commit_tree as commitTree { alloca- `Tree' peekNewTree*, `Commit' } -> `Int' checkReturnCode*-#}
