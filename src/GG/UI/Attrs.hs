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

module GG.UI.Attrs where

import           Brick              (AttrName, attrName)
import           Brick.Widgets.List (listAttr, listSelectedAttr)

defaultAttr :: AttrName
defaultAttr = attrName "default"

list :: AttrName
list = listAttr

listSelected :: AttrName
listSelected = listSelectedAttr

oid :: AttrName
oid = listAttr <> attrName "oid"

author :: AttrName
author = listAttr <> attrName "author"

date :: AttrName
date = listAttr <> attrName "date"

statusBar :: AttrName
statusBar = attrName "status_bar"

statusBranch :: AttrName
statusBranch = statusBar <> attrName "status_branch"

notification :: AttrName
notification = statusBar <> attrName "notification"

notificationEmphasis :: AttrName
notificationEmphasis = notification <> attrName "emphasis"

notificationFailure :: AttrName
notificationFailure = notification <> attrName "failure"

commitSummary :: AttrName
commitSummary = attrName "commit_summary"

fullOid :: AttrName
fullOid = statusBranch <> attrName "oid"

statsFilesModified :: AttrName
statsFilesModified = statusBranch <> attrName "modifications"

statsInsertions :: AttrName
statsInsertions = statusBranch <> attrName "additions"

statsDeletions :: AttrName
statsDeletions = statusBranch <> attrName "deletions"

fileDelta :: AttrName
fileDelta = attrName "file_delta"

fileAdded :: AttrName
fileAdded = fileDelta <> attrName "added"

fileDeleted :: AttrName
fileDeleted = fileDelta <> attrName "deleted"

fileModified :: AttrName
fileModified = fileDelta <> attrName "modified"

fileRenamed :: AttrName
fileRenamed = fileDelta <> attrName "renamed"

fileCopied :: AttrName
fileCopied = fileDelta <> attrName "copied"

diff :: AttrName
diff = attrName "diff"

diffHeader :: AttrName
diffHeader = diff <> attrName "header"

diffAddedLine :: AttrName
diffAddedLine = diff <> attrName "added_line"

diffDeletedLine :: AttrName
diffDeletedLine = diff <> attrName "deleted_line"

diffAddedText :: AttrName
diffAddedText = diff <> attrName "added_text"

diffDeletedText :: AttrName
diffDeletedText = diff <> attrName "deleted_text"

diffSpecialText :: AttrName
diffSpecialText = diff <> attrName "special_text"

diffLineNumber :: AttrName
diffLineNumber = diff <> attrName "line_number"

diffLineNumberSep :: AttrName
diffLineNumberSep = diff <> attrName "line_number_separator"
