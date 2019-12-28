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
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module GG.UI
  ( main
  , Commit
  ) where

import           Brick                         (App (..), AttrMap, AttrName,
                                                BrickEvent (..),
                                                Direction (Down, Up), EventM,
                                                Next, Padding (..),
                                                ViewportType (Vertical), Widget,
                                                attrMap, bg, cached, continue,
                                                customMain, emptyWidget, fg,
                                                getVtyHandle, halt,
                                                invalidateCacheEntry,
                                                neverShowCursor, on, padBottom,
                                                padRight, str, textWidth,
                                                vScrollBy, vScrollPage,
                                                vScrollToBeginning,
                                                vScrollToEnd, viewport,
                                                viewportScroll, withAttr, (<+>),
                                                (<=>))
import qualified Brick.Widgets.List            as L
import           Control.Lens                  (element, mapMOf, to, (^.), (^?),
                                                (^?!))
import           Control.Monad                 (void, when)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bits                     (Bits, zeroBits, (.&.))
import           Data.Generics.Product.Fields  (field)
import           Data.List                     (intercalate)
import           Data.Maybe                    (fromMaybe, isJust, isNothing)
import           Data.String.Utils             (replace)
import           Data.Time                     (ZonedTime, defaultTimeLocale,
                                                formatTime,
                                                zonedTimeToLocalTime,
                                                zonedTimeZone)
import           Data.Vector                   (toList)
import           GG.Repo                       (Action, readCommit,
                                                readCommitDiff, readNCommits,
                                                readRepoState)
import qualified GG.Repo                       as R
import           GG.State                      (Commit, Name (..), OpenCommit,
                                                State (..), addMoreCommits,
                                                closeCommitDetails, commitL,
                                                openCommitDetails,
                                                updateCommitsPos,
                                                updateRepoState)
import           Graphics.Vty                  (defAttr)
import qualified Graphics.Vty                  as V
import           Graphics.Vty.Output.Interface (Output (resetBackgroundColor, setBackgroundColor))
import           Libgit2                       (DeltaInfo, DeltaType (..),
                                                DiffDelta (DiffDelta),
                                                DiffFile (DiffFile), DiffHunk,
                                                DiffInfo, DiffLine,
                                                Filemode (FilemodeBlob, FilemodeBlobExecutable, FilemodeLink),
                                                HunkInfo, OID,
                                                Similarity (Similarity),
                                                diffDeltaNewFile,
                                                diffDeltaOldFile,
                                                diffDeltaSimilarity,
                                                diffDeltaStatus, diffExists,
                                                diffFileFlags, diffFileMode,
                                                diffFilePath, diffHunkHeader,
                                                diffLineContent,
                                                diffLineNewLineno,
                                                diffLineOldLineno,
                                                diffLineOrigin, diffNotBinary,
                                                diffStatsDeletions,
                                                diffStatsFilesChanged,
                                                diffStatsInsertions)
import           Prelude                       hiding (head)
import           System.Environment            (lookupEnv, setEnv)

data Event

app :: App State Event Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = const theMap
    }

startEvent :: State -> EventM Name State
startEvent s = do
  vty <- getVtyHandle
  liftIO $ setBackgroundColor (V.outputIface vty) base3
  return s

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = closeAction s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = closeAction s
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = openCommitAction s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'K') [])) = doAction R.moveCommitUp s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'J') [])) = doAction R.moveCommitDown s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'S') [])) = doAction R.squashCommit s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'F') [])) = doAction R.fixupCommit s
handleEvent s (VtyEvent ev) = handleScrolling s ev
handleEvent s _ = continue s

handleScrolling :: State -> V.Event -> EventM Name (Next State)
handleScrolling s@State {openCommit = Just _, ..} ev = do
  handleOpenCommitScrolling ev
  continue s
handleScrolling s@State {openCommit = Nothing} ev = handleCommitsListScrolling s ev

handleCommitsListScrolling :: State -> V.Event -> EventM Name (Next State)
handleCommitsListScrolling s (V.EvKey (V.KChar 'G') []) = continue s -- disable going to the end of the list
handleCommitsListScrolling s (V.EvKey V.KEnd []) = continue s
handleCommitsListScrolling s ev = do
  s' <- liftIO $ checkNeedsMoreCommits s
  s'' <- mapMOf (field @"commitList") (L.handleListEventVi L.handleListEvent ev) s'
  continue s''

handleOpenCommitScrolling :: V.Event -> EventM Name ()
handleOpenCommitScrolling e = action vps
  where
    vps = viewportScroll CommitDiffVP
    action =
      case e of
        V.EvKey V.KUp []                -> flip vScrollBy (-1)
        V.EvKey V.KDown []              -> flip vScrollBy 1
        V.EvKey V.KHome []              -> vScrollToBeginning
        V.EvKey V.KEnd []               -> vScrollToEnd
        V.EvKey V.KPageDown []          -> flip vScrollPage Down
        V.EvKey V.KPageUp []            -> flip vScrollPage Up
        V.EvKey (V.KChar 'k') []        -> flip vScrollBy (-1)
        V.EvKey (V.KChar 'j') []        -> flip vScrollBy 1
        V.EvKey (V.KChar 'g') []        -> vScrollToBeginning
        V.EvKey (V.KChar 'G') []        -> vScrollToEnd
        V.EvKey (V.KChar 'f') [V.MCtrl] -> flip vScrollPage Down
        V.EvKey (V.KChar 'b') [V.MCtrl] -> flip vScrollPage Up
        _                               -> const $ pure ()

checkNeedsMoreCommits :: State -> IO State
checkNeedsMoreCommits s =
  if ((s ^. field @"commitList" . L.listElementsL . to length) -
      (s ^. field @"commitList" . L.listSelectedL . to (fromMaybe 0))) <
     500
    then do
      (moreCommits, contCommit') <- readNCommits 500 (s ^. field @"contCommit")
      moreCommitsState <- mapM readCommit moreCommits
      pure $ addMoreCommits moreCommitsState contCommit' s
    else pure s

doAction :: Action -> State -> EventM Name (Next State)
doAction a s = do
  s' <-
    liftIO $ do
      let commitOIDs = map (^. field @"oid") (s ^. field @"commitList" . L.listElementsL . to toList)
      let pos = s ^. field @"commitList" . L.listSelectedL . to (fromMaybe 0)
      result <- R.doAction (s ^. field @"repository") (s ^. field @"head" . field @"ref") commitOIDs pos a
      case result of
        R.Success newPos -> do
          (head, headCommit) <- readRepoState $ s ^. field @"repository"
          (tailCommits, contCommit') <- readNCommits (pos + 500) headCommit
          moreCommitsState <- mapM readCommit (headCommit : tailCommits)
          pure $ (updateRepoState contCommit' head moreCommitsState . updateCommitsPos newPos) s
        _ -> pure s
  continue s'

defaultAttr :: AttrName
defaultAttr = "default"

oidAttr :: AttrName
oidAttr = L.listAttr <> "oid"

authorAttr :: AttrName
authorAttr = L.listAttr <> "author"

dateAttr :: AttrName
dateAttr = L.listAttr <> "date"

statusBarAttr :: AttrName
statusBarAttr = "status_bar"

statusBranchAttr :: AttrName
statusBranchAttr = statusBarAttr <> "status_branch"

commitSummary :: AttrName
commitSummary = "commit_summary"

fullOid :: AttrName
fullOid = statusBranchAttr <> "oid"

statsFilesModified :: AttrName
statsFilesModified = statusBranchAttr <> "modifications"

statsInsertions :: AttrName
statsInsertions = statusBranchAttr <> "additions"

statsDeletions :: AttrName
statsDeletions = statusBranchAttr <> "deletions"

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

diffAttr :: AttrName
diffAttr = "diff"

diffHeader :: AttrName
diffHeader = diffAttr <> "header"

diffAddedLine :: AttrName
diffAddedLine = diffAttr <> "added_line"

diffDeletedLine :: AttrName
diffDeletedLine = diffAttr <> "deleted_line"

diffAddedText :: AttrName
diffAddedText = diffAttr <> "added_text"

diffDeletedText :: AttrName
diffDeletedText = diffAttr <> "deleted_text"

diffSpecialText :: AttrName
diffSpecialText = diffAttr <> "special_text"

diffLineNumber :: AttrName
diffLineNumber = diffAttr <> "line_number"

diffLineNumberSep :: AttrName
diffLineNumberSep = diffAttr <> "line_number_separator"

myFormatTime :: ZonedTime -> String
myFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

formatOid :: OID -> String
formatOid = take 8 . show

fillLine :: AttrName -> Widget Name -> Widget Name
fillLine attr line = withAttr attr $ padRight Max line

drawCommit :: Bool -> Commit -> Widget Name
drawCommit _selected c =
  foldr1
    (<+>)
    [ withAttr oidAttr (str $ formatOid (c ^. field @"oid"))
    , str " "
    , if c ^. field @"open"
        then str $ eyesIcon ++ " "
        else emptyWidget
    , str (c ^. field @"summary")
    , padRight Max (str " ")
    , withAttr authorAttr (str ((c ^. field @"authorName") <> " <" <> (c ^. field @"authorEmail") <> ">"))
    , str " "
    , withAttr dateAttr (str $ myFormatTime (c ^. field @"authorWhen"))
    ]

drawUI :: State -> [Widget Name]
drawUI s = [ui]
  where
    ui =
      foldr1
        (<=>)
        [ padBottom Max (L.renderList drawCommit True (s ^. field @"commitList"))
        , maybe emptyWidget (withAttr defaultAttr . padBottom Max . drawOpenCommit) (s ^. field @"openCommit")
        , withAttr
            statusBarAttr
            (withAttr statusBranchAttr (str ("On " <> s ^. field @"head" . field @"shorthand")) <+>
             padRight Max (str " "))
        ]

openCommitAction :: State -> EventM Name (Next State)
openCommitAction s = do
  s' <- liftIO $ maybe (pure s) openCommitAction' (s ^. (field @"commitList" . L.listSelectedL))
  let vps = viewportScroll CommitDiffVP
  vScrollToBeginning vps
  invalidateCacheEntry CommitDiffUI
  continue s'
  where
    openCommitAction' ix = do
      (diffStats, diffInfo_) <- readCommitDiff (s ^. field @"repository") (s ^?! (commitL ix . field @"oid"))
      pure $ openCommitDetails ix (diffStats, diffInfo_) s

drawSignatures :: Commit -> Widget Name
drawSignatures c = foldr1 (<=>) cases
  where
    authorName = c ^. field @"authorName"
    authorEmail = c ^. field @"authorEmail"
    authorWhen = c ^. field @"authorWhen"
    committerName = c ^. field @"committerName"
    committerEmail = c ^. field @"committerEmail"
    committerWhen = c ^. field @"committerWhen"
    nameEmailSame = authorName == committerName && authorEmail == committerEmail
    datesSame =
      (zonedTimeToLocalTime authorWhen == zonedTimeToLocalTime committerWhen) &&
      (zonedTimeZone authorWhen == zonedTimeZone committerWhen)
    sigName name email = withAttr authorAttr (str (name <> " <" <> email <> ">"))
    sigDate date = withAttr dateAttr (str $ myFormatTime date)
    sigAuthorName = sigName authorName authorEmail
    sigAuthorDate = sigDate authorWhen
    sigCommitterName = sigName committerName committerEmail
    sigCommitterDate = sigDate committerWhen
    cases
      | nameEmailSame && datesSame =
        map (fillLine defaultAttr) [str "Author:    " <+> sigAuthorName, str "Date:      " <+> sigAuthorDate]
      | not nameEmailSame && datesSame =
        map
          (fillLine defaultAttr)
          [ str "Author:    " <+> sigAuthorName
          , str "Committer: " <+> sigCommitterName
          , str "Date:      " <+> sigCommitterDate
          ]
      | nameEmailSame && not datesSame =
        map
          (fillLine defaultAttr)
          [ str "Author:      " <+> sigAuthorName
          , str "Date:        " <+> sigAuthorDate
          , str "Commit Date: " <+> sigCommitterDate
          ]
      | not nameEmailSame && not datesSame =
        map
          (fillLine defaultAttr)
          [ str "Author:    " <+> sigAuthorName
          , str "Date:      " <+> sigAuthorDate
          , str "Committer: " <+> sigCommitterName
          , str "Date:      " <+> sigCommitterDate
          ]
      | otherwise = error "Unreachable code!"

drawDiff :: DiffInfo -> Widget Name
drawDiff diffInfo =
  foldr1 (<=>) $
  map
    (\(diffDelta, deltaInfo) -> fillLine defaultAttr (str " ") <=> drawDelta diffDelta <=> drawDeltaInfo deltaInfo)
    diffInfo

data FileType
  = Normal
  | Binary
  deriving (Eq)

data FileMode
  = File
  | Executable
  | Link
  deriving (Eq)

isFlagSet :: (Bits a) => a -> a -> Bool
isFlagSet value flag = value .&. flag /= zeroBits

drawDelta :: DiffDelta -> Widget Name
drawDelta DiffDelta { diffDeltaSimilarity = Similarity similarity
                    , diffDeltaOldFile = old
                    , diffDeltaNewFile = new
                    , diffDeltaStatus = deltaType
                    } =
  fillLine fileDelta $ withAttr attr (str text <+> drawTypeTransition <+> drawModeTransition <+> drawSimilarity)
  where
    fileExists file = diffFileFlags file `isFlagSet` diffExists
    oldExists = fileExists old
    newExists = fileExists new
    oldName = diffFilePath old
    newName = diffFilePath new
    sameName = oldName == newName
    fileMode DiffFile {diffFileMode = FilemodeBlob} = File
    fileMode DiffFile {diffFileMode = FilemodeBlobExecutable} = Executable
    fileMode DiffFile {diffFileMode = FilemodeLink} = Link
    fileMode DiffFile {diffFileMode = mode} = error $ "Unexpected filemode " ++ show mode
    oldMode = fileMode old
    newMode = fileMode new
    fileType file
      | diffFileFlags file `isFlagSet` diffNotBinary = Normal
    fileType _file = Binary
    oldType = fileType old
    newType = fileType new
    typeIcon Normal = fileIcon
    typeIcon Binary = floppyIcon
    drawTypeTransition
      | not (oldExists && newExists) = emptyWidget
      | oldType /= newType =
        str ", " <+> withAttr fileModified (str $ typeIcon oldType <> rightArrowIcon <> typeIcon newType)
      | otherwise = emptyWidget
    modeIcon File       = ""
    modeIcon Executable = gearIcon
    modeIcon Link       = linkIcon
    drawModeTransitionStr =
      case (oldMode, newMode) of
        _
          | not (oldExists && newExists) -> ""
        (a, File)
          | a /= File -> modeIcon a <> rightArrowIcon <> typeIcon newType
        (File, b)
          | b /= File -> typeIcon oldType <> rightArrowIcon <> modeIcon b
        (a, b)
          | a /= b -> modeIcon a <> rightArrowIcon <> modeIcon b
        (_, _) -> ""
    drawModeTransition
      | drawModeTransitionStr == "" = emptyWidget
      | otherwise = str ", " <+> withAttr fileModified (str drawModeTransitionStr)
    sameNameModeIcon
      | drawModeTransitionStr == "" = ""
      | otherwise = modeIcon newMode
    drawSimilarity
      | similarity == 0 = emptyWidget
      | similarity == 100 = emptyWidget
      | otherwise = str ", " <+> withAttr fileModified (str (pencilIcon <> " " <> show similarity <> "% similar"))
    (attr, text) =
      case (oldExists, newExists) of
        (True, True)
          | sameName
          , deltaType `elem` [DeltaModified, DeltaTypechange]
          , newType == Normal -> (fileModified, intercalate "" [editIcon, newName, sameNameModeIcon])
        (True, True)
          | sameName
          , deltaType `elem` [DeltaModified, DeltaTypechange] ->
            (fileModified, intercalate "" [pencilIcon, typeIcon newType, newName, sameNameModeIcon])
        (True, True)
          | not sameName
          , deltaType == DeltaCopied ->
            ( fileCopied
            , intercalate
                ""
                [ sparklesIcon
                , typeIcon newType
                , newName
                , modeIcon newMode
                , " "
                , leftArrowIcon
                , " "
                , typeIcon oldType
                , oldName
                , modeIcon oldMode
                ])
        (True, True)
          | not sameName
          , deltaType == DeltaRenamed ->
            ( fileRenamed
            , intercalate
                ""
                [ typeIcon oldType
                , oldName
                , modeIcon oldMode
                , " "
                , rightArrowIcon
                , " "
                , typeIcon newType
                , newName
                , modeIcon newMode
                ])
        (False, True)
          | deltaType == DeltaAdded ->
            (fileAdded, intercalate "" [sparklesIcon, typeIcon newType, newName, modeIcon newMode])
        (True, False)
          | deltaType == DeltaDeleted ->
            (fileDeleted, intercalate "" [crossIcon, typeIcon oldType, oldName, modeIcon oldMode])
        _ -> (fileModified, "Unknown file change?!")

drawDeltaInfo :: DeltaInfo -> Widget Name
drawDeltaInfo (hunkInfos, _diffBinaries) =
  foldr ((<=>) . drawHunkInfo (oldLineWidth, newLineWidth)) emptyWidget hunkInfos
  where
    (oldLineWidth, newLineWidth) =
      foldr
        ((\(oldWidth, newWidth) (maxOldWidth, maxNewWidth) -> (max maxOldWidth oldWidth, max maxNewWidth newWidth)) .
         (\diffLine -> (length $ show $ diffLineOldLineno diffLine, length $ show $ diffLineNewLineno diffLine)))
        (0, 0) $
      concatMap snd hunkInfos

drawHunk :: DiffHunk -> Widget Name
drawHunk hunk = cases
  where
    header = diffHunkHeader hunk
    strippedHeader = drop k header
      where
        idx = [i | (c, i) <- zip header [0 ..], c == '@']
        k = 1 + fromMaybe (-1) (idx ^? element 3)
    cases
      | strippedHeader /= "" = withAttr diffHeader (str $ "  " <> strippedHeader)
      | otherwise = emptyWidget

drawHunkInfo :: (Int, Int) -> HunkInfo -> Widget Name
drawHunkInfo (oldLineWidth, newLineWidth) (hunk, diffLines) =
  hunkHeaderUI <=> foldr1 (<=>) (map (drawLine (oldLineWidth, newLineWidth)) diffLines)
  where
    hunkHeaderUI =
      fillLine defaultAttr $ withAttr diffLineNumber (str $ center (oldLineWidth + newLineWidth + 1) "...") <+>
      withAttr diffLineNumberSep (str doubleDividingLine) <+>
      drawHunk hunk

pad :: Int -> String -> String
pad width string = replicate (width - textWidth string) ' ' <> string

center :: Int -> String -> String
center width string = replicate left ' ' <> string <> replicate right ' '
  where
    excess = width - textWidth string
    left =
      excess `div` 2 +
      if excess `mod` 2 == 1
        then 1
        else 0
    right = excess `div` 2

drawLine :: (Int, Int) -> DiffLine -> Widget Name
drawLine (oldLineWidth, newLineWidth) diffLine = cases
  where
    origin = diffLineOrigin diffLine
    oldLineNo = diffLineOldLineno diffLine
    newLineNo = diffLineNewLineno diffLine
    content = replace "\t" "    " $ diffLineContent diffLine
    drawLineNos oldLineNoStr newLineNoStr =
      foldr1
        (<+>)
        [ withAttr diffLineNumber $ str $ pad oldLineWidth oldLineNoStr
        , withAttr diffLineNumberSep $ str singleDividingLine
        , withAttr diffLineNumber $ str $ pad newLineWidth newLineNoStr
        , withAttr diffLineNumberSep $ str doubleDividingLine
        ]
    drawLine' attr lineUI = withAttr attr $ padRight Max lineUI
    cases
      | origin == ' ' = drawLineNos (show oldLineNo) (show newLineNo) <+> drawLine' diffAttr (str content)
      | origin == '+' = drawLineNos "" (show newLineNo) <+> drawLine' diffAddedLine (str content)
      | origin == '-' = drawLineNos (show oldLineNo) "" <+> drawLine' diffDeletedLine (str content)
      | origin == '=' =
        drawLineNos "" "" <+> drawLine' diffAttr (withAttr diffSpecialText (str $ "no " <> carriageReturnIcon))
      | origin == '>' = drawLineNos "" "" <+> drawLine' diffAddedLine (withAttr diffAddedText (str carriageReturnIcon))
      | origin == '<' =
        drawLineNos "" "" <+> drawLine' diffDeletedLine (withAttr diffDeletedText (str carriageReturnIcon))
      | otherwise = error "Unknown line origin"

drawOpenCommit :: OpenCommit -> Widget Name
drawOpenCommit openCommit =
  title <=>
  viewport
    CommitDiffVP
    Vertical
    (cached CommitDiffUI $
     foldr1
       (<=>)
       [ fillLine defaultAttr $ str " "
       , fillLine defaultAttr $ str (openCommit ^. (field @"openCommit" . field @"body"))
       , fillLine defaultAttr $ str " "
       , drawSignatures (openCommit ^. field @"openCommit")
       , drawDiff (openCommit ^. field @"diffInfo")
       ])
  where
    title =
      withAttr commitSummary $
      foldr1
        (<+>)
        [ str (openCommit ^. (field @"openCommit" . field @"summary"))
        , drawDiffStats openCommit
        , padRight Max (str " ")
        , withAttr fullOid $ str $ show (openCommit ^. (field @"openCommit" . field @"oid"))
        ]

editIcon :: String
editIcon = "\x1F4DD"

plusIcon :: String
plusIcon = "\x2795\xFE0E"

minusIcon :: String
minusIcon = "\x2796\xFE0E"

eyesIcon :: String
eyesIcon = "\x1F440"

pencilIcon :: String
pencilIcon = "\x270F\xFE0F "

fileIcon :: String
fileIcon = "\x1F4C4"

floppyIcon :: String
floppyIcon = "\x1F4BE"

gearIcon :: String
gearIcon = "\x2699\xFE0F "

linkIcon :: String
linkIcon = "\x1F517"

leftArrowIcon :: String
leftArrowIcon = "\x2B05\xFE0F "

rightArrowIcon :: String
rightArrowIcon = "\x27A1\xFE0F "

crossIcon :: String
crossIcon = "\x274C"

sparklesIcon :: String
sparklesIcon = "\x2728"

singleDividingLine :: String
singleDividingLine = "\x2502"

doubleDividingLine :: String
doubleDividingLine = "\x2551"

carriageReturnIcon :: String
carriageReturnIcon = "\x21B5"

drawDiffStats :: OpenCommit -> Widget Name
drawDiffStats c =
  foldr1
    (<+>)
    [ str " "
    , withAttr statsFilesModified $ str $ editIcon ++ show (diffStatsFilesChanged diffStats)
    , str " "
    , withAttr statsInsertions $ str $ plusIcon ++ show (diffStatsInsertions diffStats)
    , str " "
    , withAttr statsDeletions $ str $ minusIcon ++ show (diffStatsDeletions diffStats)
    ]
  where
    diffStats = c ^. field @"diffStats"

closeAction :: State -> EventM Name (Next State)
closeAction s =
  if isJust (s ^. field @"openCommit")
    then continue $ closeCommitDetails s
    else halt s

rgbColor :: Integer -> Integer -> Integer -> V.Color
rgbColor = V.linearColor

-- Solarized (sort of because of color approximation) so my eyes don't bleed
base03 :: V.Color
base03 = rgbColor 0x00 0x2b 0x36

--
--base02 :: V.Color
--base02 = rgbColor 0x07 0x36 0x42
--
base01 :: V.Color
base01 = rgbColor 0x58 0x6e 0x75

--
--base00 :: V.Color
--base00 = rgbColor 0x65 0x7b 0x83
--
--base0 :: V.Color
--base0 = rgbColor 0x83 0x94 0x96
--
base1 :: V.Color
base1 = rgbColor 0x93 0xa1 0xa1

base2 :: V.Color
base2 = rgbColor 0xee 0xe8 0xd5

base3 :: V.Color
base3 = rgbColor 0xfd 0xf6 0xe3

--
--yellow :: V.Color
--yellow = rgbColor 0xb5 0x89 0x00
--
--orange :: V.Color
--orange = rgbColor 0xcb 0x4b 0x16
--
red :: V.Color
red = rgbColor 0xdc 0x32 0x2f

redBg :: V.Color
redBg = rgbColor 0xfd 0xe3 0xe3

redBgBold :: V.Color
redBgBold = rgbColor 0xff 0xbf 0xbf

magenta :: V.Color
magenta = rgbColor 0xd3 0x36 0x82

violet :: V.Color
violet = rgbColor 0x6c 0x71 0xc4

--
--blue :: V.Color
--blue = rgbColor 0x26 0x8b 0xd2
--
cyan :: V.Color
cyan = rgbColor 0x2a 0xa1 0x98

green :: V.Color
green = rgbColor 0x85 0x99 0x00

greenBg :: V.Color
greenBg = rgbColor 0xe3 0xfd 0xe3

greenBgBold :: V.Color
greenBgBold = rgbColor 0xb8 0xe6 0xb8

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (defaultAttr, base03 `on` base3)
    , (L.listAttr, base03 `on` base3)
    , (L.listSelectedAttr, base03 `on` base3 `V.withStyle` V.bold)
    , (oidAttr, fg base01)
    , (authorAttr, fg violet)
    , (dateAttr, fg green)
    , (statusBarAttr, base03 `on` base2)
    , (statusBranchAttr, fg base03)
    , (commitSummary, base03 `on` base2 `V.withStyle` V.bold)
    , (fullOid, fg base01 `V.withStyle` V.bold)
    , (statsFilesModified, fg cyan)
    , (statsInsertions, fg green)
    , (statsDeletions, fg red)
    , (fileDelta, base03 `on` base3)
    , (fileAdded, fg green)
    , (fileDeleted, fg red)
    , (fileModified, fg cyan)
    , (fileRenamed, fg violet)
    , (fileCopied, fg magenta)
    , (diffAttr, base03 `on` base3)
    , (diffHeader, defAttr `V.withForeColor` base1 `V.withStyle` V.italic)
    , (diffAddedLine, bg greenBg)
    , (diffAddedText, bg greenBgBold)
    , (diffDeletedLine, bg redBg)
    , (diffDeletedText, bg redBgBold)
    , (diffSpecialText, defAttr `V.withStyle` V.italic)
    , (diffLineNumber, base01 `on` base2)
    , (diffLineNumberSep, base1 `on` base2)
    ]

main :: State -> IO ()
main state = do
  terminfoDirs <- lookupEnv "TERMINFO_DIRS"
  when (isNothing terminfoDirs) (setEnv "TERMINFO_DIRS" "/etc/terminfo:/lib/terminfo:/usr/share/terminfo")
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app state
  resetBackgroundColor (V.outputIface initialVty)
