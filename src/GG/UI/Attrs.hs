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
{-# LANGUAGE OverloadedStrings #-}

module GG.UI.Attrs where

import           Brick              (AttrName)
import           Brick.Widgets.List (listAttr, listSelectedAttr)

defaultAttr :: AttrName
defaultAttr = "default"

list :: AttrName
list = listAttr

listSelected :: AttrName
listSelected = listSelectedAttr

oid :: AttrName
oid = listAttr <> "oid"

author :: AttrName
author = listAttr <> "author"

date :: AttrName
date = listAttr <> "date"

statusBar :: AttrName
statusBar = "status_bar"

statusBranch :: AttrName
statusBranch = statusBar <> "status_branch"

notification :: AttrName
notification = statusBar <> "notification"

notificationEmphasis :: AttrName
notificationEmphasis = notification <> "emphasis"

notificationFailure :: AttrName
notificationFailure = notification <> "failure"

commitSummary :: AttrName
commitSummary = "commit_summary"

fullOid :: AttrName
fullOid = statusBranch <> "oid"

statsFilesModified :: AttrName
statsFilesModified = statusBranch <> "modifications"

statsInsertions :: AttrName
statsInsertions = statusBranch <> "additions"

statsDeletions :: AttrName
statsDeletions = statusBranch <> "deletions"

fileDelta :: AttrName
fileDelta = "file_delta"

fileAdded :: AttrName
fileAdded = fileDelta <> "added"

fileDeleted :: AttrName
fileDeleted = fileDelta <> "deleted"

fileModified :: AttrName
fileModified = fileDelta <> "modified"

fileRenamed :: AttrName
fileRenamed = fileDelta <> "renamed"

fileCopied :: AttrName
fileCopied = fileDelta <> "copied"

diff :: AttrName
diff = "diff"

diffHeader :: AttrName
diffHeader = diff <> "header"

diffAddedLine :: AttrName
diffAddedLine = diff <> "added_line"

diffDeletedLine :: AttrName
diffDeletedLine = diff <> "deleted_line"

diffAddedText :: AttrName
diffAddedText = diff <> "added_text"

diffDeletedText :: AttrName
diffDeletedText = diff <> "deleted_text"

diffSpecialText :: AttrName
diffSpecialText = diff <> "special_text"

diffLineNumber :: AttrName
diffLineNumber = diff <> "line_number"

diffLineNumberSep :: AttrName
diffLineNumberSep = diff <> "line_number_separator"
