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
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module GG.UI
  ( main
  ) where

import           Brick                  hiding (attrMap)
import           Brick.BChan            (BChan)
import           Brick.Widgets.List
import           Control.Lens           (element, mapMOf, mapped, set, to, (^.),
                                         (^?), (^?!), _2)
import           Control.Monad          (foldM, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, runReader)
import           Data.Bits              (Bits, zeroBits, (.&.))
import           Data.Generics.Product  (field)
import           Data.List              (intercalate)
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Data.String.Utils      (replace)
import           Data.Time              (ZonedTime, defaultTimeLocale,
                                         formatTime, zonedTimeToLocalTime,
                                         zonedTimeZone)
import qualified GG.Actions             as A
import           GG.Config              (getCfg, getCfgM, withCfg)
import qualified GG.Config              as CFG
import           GG.Env                 (Env (..))
import qualified GG.Repo                as R
import qualified GG.State               as S
import           GG.Timers              (AnimationCb, AnimationEndCb, Duration,
                                         addAnimation, tickEventHandler)
import qualified GG.UI.Attrs            as Attr
import           GG.UI.Theme            (attrMap)
import           GG.Utils               (uncurry3)
import qualified Graphics.Vty           as V
import qualified Libgit2                as G
import           Prelude                hiding (head)
import           System.Environment     (lookupEnv, setEnv)

type UI = Reader Env (Widget S.Name)

app :: App S.State S.Event S.Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = getAttrMap
    }

getAttrMap :: S.State -> AttrMap
getAttrMap s = attrMap $ s ^. field @"config" . field @"ui" . field @"theme"

startEvent :: S.State -> EventM S.Name S.State
startEvent s = do
  vty <- getVtyHandle
  liftIO $ V.setBackgroundColor (V.outputIface vty) base3
  return s

handleEvent :: S.State -> BrickEvent S.Name S.Event -> EventM S.Name (Next S.State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = closeAction s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = closeAction s
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = openCommitAction s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'K') [])) = doRebaseAction A.MoveUpA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'J') [])) = doRebaseAction A.MoveDownA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'S') [])) = doRebaseAction A.SquashA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'F') [])) = doRebaseAction A.FixupA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'D') [])) = doRebaseAction A.DeleteA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'Z') [])) = doAction A.UndoA s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'R') [])) = doAction A.RedoA s
handleEvent s (VtyEvent ev) = handleScrolling s ev
handleEvent s (AppEvent S.Tick) = tickEventHandler (s ^. field @"timers") s
handleEvent s _ = continue s

handleScrolling :: S.State -> V.Event -> EventM S.Name (Next S.State)
handleScrolling s@S.State {openCommit = Just _, ..} ev = do
  handleOpenCommitScrolling ev
  continue s
handleScrolling s@S.State {openCommit = Nothing} ev = handleCommitsListScrolling s ev

handleCommitsListScrolling :: S.State -> V.Event -> EventM S.Name (Next S.State)
handleCommitsListScrolling s (V.EvKey (V.KChar 'G') []) = continue s -- disable going to the end of the list
handleCommitsListScrolling s (V.EvKey V.KEnd []) = continue s
handleCommitsListScrolling s ev = do
  s' <- liftIO $ checkNeedsMoreCommits s
  s'' <- mapMOf (field @"commitList") (handleListEventVi handleListEvent ev) s'
  continue s''

handleOpenCommitScrolling :: V.Event -> EventM S.Name ()
handleOpenCommitScrolling e = action vps
  where
    vps = viewportScroll S.CommitDiffVP
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

checkNeedsMoreCommits :: S.State -> IO S.State
checkNeedsMoreCommits s =
  if ((s ^. field @"commitList" . listElementsL . to length) -
      (s ^. field @"commitList" . listSelectedL . to (fromMaybe 0))) <
     500
    then do
      (moreCommits, contCommit') <- R.readNCommits 500 (s ^. field @"contCommit")
      moreCommitsState <- mapM R.readCommit moreCommits
      pure $ S.addMoreCommits moreCommitsState contCommit' s
    else pure s

handleActionSuccessReload :: S.State -> Int -> A.ActionSummary String -> IO S.State
handleActionSuccessReload s newPos _summary = do
  (head, headCommit) <- R.readRepoState $ s ^. field @"repository"
  (tailCommits, contCommit') <- R.readNCommits (newPos + 500) headCommit
  moreCommitsState <- mapM R.readCommit (headCommit : tailCommits)
  pure $
    (S.updateRepoState contCommit' head moreCommitsState . S.updateCommitsPos newPos .
     set (field @"notification") Nothing)
      s

handleActionFailure :: S.State -> A.ActionFailure -> IO S.State
handleActionFailure s failure = do
  let s' = set (field @"notification") (Just (S.ActionFailure failure, 1)) s
  addAnimation
    (s ^. field @"timers")
    S.NotificationT
    (failureNotificationDuration failure)
    notificationAnimation
    notificationAnimationEnd
  pure s'

handleActionWarning :: S.State -> A.ActionWarning -> IO S.State
handleActionWarning s warning = do
  let s' = set (field @"notification") (Just (S.ActionWarning warning, 1)) s
  addAnimation
    (s ^. field @"timers")
    S.NotificationT
    (warningNotificationDuration warning)
    notificationAnimation
    notificationAnimationEnd
  pure s'

handleAction :: S.State -> A.Action -> IO S.State
handleAction s action = do
  result <- A.doAction (s ^. field @"repository") action
  case result of
    A.Success newPos summary maybeWarning -> do
      s' <- handleActionSuccessReload s newPos summary
      case maybeWarning of
        Nothing      -> pure s'
        Just warning -> handleActionWarning s' warning
    A.Failure failure -> handleActionFailure s failure

doAction :: A.Action -> S.State -> EventM S.Name (Next S.State)
doAction action s = do
  s' <- liftIO $ handleAction s action
  continue s'

doRebaseAction :: A.RebaseAction -> S.State -> EventM S.Name (Next S.State)
doRebaseAction rebaseAction s = do
  let pos = s ^. field @"commitList" . listSelectedL . to (fromMaybe 0)
  s' <- liftIO $ handleAction s (A.RebaseAction pos rebaseAction)
  continue s'

failureNotificationDuration :: A.ActionFailure -> Duration
failureNotificationDuration (A.RebaseConflict _ _)  = 5
failureNotificationDuration (A.RebaseMergeCommit _) = 5
failureNotificationDuration (A.GPGError _ _)        = 5
failureNotificationDuration _                       = 2

warningNotificationDuration :: A.ActionWarning -> Duration
warningNotificationDuration A.X509SigningNotSupported = 5

notificationAnimation :: AnimationCb S.State S.Name
notificationAnimation t s = do
  let s' = set (field @"notification" . mapped . _2) (easeOut t) s
  pure (s', [])

notificationAnimationEnd :: AnimationEndCb S.State S.Name
notificationAnimationEnd s = do
  let s' = set (field @"notification") Nothing s
  pure (s', [])

myFormatTime :: ZonedTime -> String
myFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

formatOid :: G.OID -> String
formatOid = take 8 . show

fillLine :: AttrName -> Widget S.Name -> Widget S.Name
fillLine attr line = withAttr attr $ padRight Max line

drawCommit :: Bool -> S.Commit -> UI
drawCommit _selected c =
  pure $
  foldr1
    (<+>)
    [ withAttr Attr.oid (str $ formatOid (c ^. field @"oid"))
    , str " "
    , if c ^. field @"open"
        then str $ eyesIcon ++ " "
        else emptyWidget
    , str (c ^. field @"summary")
    , padRight Max (str " ")
    , withAttr Attr.author (str ((c ^. field @"authorName") <> " <" <> (c ^. field @"authorEmail") <> ">"))
    , str " "
    , withAttr Attr.date (str $ myFormatTime (c ^. field @"authorWhen"))
    ]

drawUI :: S.State -> [Widget S.Name]
drawUI s = [runReader ui env]
  where
    env = s ^. field @"env"
    ui = do
      statusBarUI <- drawStatusBar s
      openCommitUI <-
        maybe
          (pure emptyWidget)
          (fmap (withAttr Attr.defaultAttr . padBottom Max) . drawOpenCommit)
          (s ^. field @"openCommit")
      let commitRenderer isSelected commit = flip runReader env $ drawCommit isSelected commit
      let commitListUI = padBottom Max (renderList commitRenderer True (s ^. field @"commitList"))
      pure $ foldr1 (<=>) [commitListUI, openCommitUI, statusBarUI]

drawStatusBar :: S.State -> UI
drawStatusBar s = do
  notificationUI <- drawNotification (s ^. field @"notification")
  pure $
    withAttr
      Attr.statusBar
      (withAttr Attr.statusBranch (str ("On " <> s ^. field @"head" . field @"shorthand")) <+> padRight Max (str " ")) <+>
    notificationUI

drawNotification :: Maybe (S.Notification, Double) -> UI
drawNotification =
  \case
    Nothing -> pure emptyWidget
    Just (notification, opacity) ->
      pure $
      case notification of
        S.ActionFailure actionFailure ->
          case actionFailure of
            A.RebaseConflict commit baseCommit ->
              withAnimAttr Attr.notification opacity $ str (iconMaybe boomIcon) <+> str "Conflicts applying commit " <+>
              withAnimAttr Attr.notificationEmphasis opacity (str commit) <+>
              str " on top of commit " <+>
              withAnimAttr Attr.notificationEmphasis opacity (str baseCommit)
            A.RebaseMergeCommit commit ->
              withAnimAttr Attr.notification opacity $ str (iconMaybe boomIcon) <+> str "Can not apply merge commit " <+>
              withAnimAttr Attr.notificationEmphasis opacity (str commit)
            A.GPGError code errorMsg ->
              withAnimAttr Attr.notification opacity $ str (iconMaybe errorIcon) <+> str " GPG error: [" <+>
              withAnimAttr Attr.notificationEmphasis opacity (str (show code)) <+>
              str "] " <+>
              withAnimAttr Attr.notificationEmphasis opacity (str $ replace "\n" " " errorMsg)
            A.UndoFailure _ -> withAnimAttr Attr.notificationFailure opacity (str $ noIcon <> undoIcon)
            A.RedoFailure _ -> withAnimAttr Attr.notificationFailure opacity (str $ noIcon <> redoIcon)
            A.ReachedTop -> withAnimAttr Attr.notificationFailure opacity (str topIcon)
            A.ReachedBottom -> withAnimAttr Attr.notificationFailure opacity (str bottomIcon)
            A.InvalidAction -> withAnimAttr Attr.notificationFailure opacity (str noIcon)
        S.ActionWarning actionWarning ->
          case actionWarning of
            A.X509SigningNotSupported ->
              withAnimAttr Attr.notification opacity $ str (iconMaybe warningIcon) <+>
              str " Commit Singing with X509 is not supported. Disable with " <+>
              withAnimAttr Attr.notificationEmphasis opacity (str "gg-gpg.format = false")
      where iconMaybe icon =
              if opacity > 0.6
                then icon
                else ""

openCommitAction :: S.State -> EventM S.Name (Next S.State)
openCommitAction s = do
  s' <- liftIO $ maybe (pure s) openCommitAction' (s ^. (field @"commitList" . listSelectedL))
  let vps = viewportScroll S.CommitDiffVP
  vScrollToBeginning vps
  invalidateCacheEntry S.CommitDiffUI
  continue s'
  where
    openCommitAction' ix = do
      (diffStats, diffInfo_) <- R.readCommitDiff (s ^. field @"repository") (s ^?! (S.commitL ix . field @"oid"))
      pure $ S.openCommitDetails ix (diffStats, diffInfo_) s

drawSignatures :: S.Commit -> UI
drawSignatures c = pure $ foldr1 (<=>) cases
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
    sigName name email = withAttr Attr.author (str (name <> " <" <> email <> ">"))
    sigDate date = withAttr Attr.date (str $ myFormatTime date)
    sigAuthorName = sigName authorName authorEmail
    sigAuthorDate = sigDate authorWhen
    sigCommitterName = sigName committerName committerEmail
    sigCommitterDate = sigDate committerWhen
    cases
      | nameEmailSame && datesSame =
        map (fillLine Attr.defaultAttr) [str "Author:    " <+> sigAuthorName, str "Date:      " <+> sigAuthorDate]
      | not nameEmailSame && datesSame =
        map
          (fillLine Attr.defaultAttr)
          [ str "Author:    " <+> sigAuthorName
          , str "Committer: " <+> sigCommitterName
          , str "Date:      " <+> sigCommitterDate
          ]
      | nameEmailSame && not datesSame =
        map
          (fillLine Attr.defaultAttr)
          [ str "Author:      " <+> sigAuthorName
          , str "Date:        " <+> sigAuthorDate
          , str "Commit Date: " <+> sigCommitterDate
          ]
      | not nameEmailSame && not datesSame =
        map
          (fillLine Attr.defaultAttr)
          [ str "Author:    " <+> sigAuthorName
          , str "Date:      " <+> sigAuthorDate
          , str "Committer: " <+> sigCommitterName
          , str "Date:      " <+> sigCommitterDate
          ]
      | otherwise = error "Unreachable code!"

drawDiff :: G.DiffInfo -> UI
drawDiff diffInfo =
  foldr1 (<=>) <$>
  mapM
    (\(diffDelta, deltaInfo) -> do
       deltaUI <- drawDelta diffDelta
       deltaInfoUI <- drawDeltaInfo deltaInfo
       pure $ fillLine Attr.defaultAttr (str " ") <=> deltaUI <=> deltaInfoUI)
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

drawDelta :: G.DiffDelta -> UI
drawDelta G.DiffDelta { diffDeltaSimilarity = G.Similarity similarity
                      , diffDeltaOldFile = old
                      , diffDeltaNewFile = new
                      , diffDeltaStatus = deltaType
                      } =
  pure $ fillLine Attr.fileDelta $
  withAttr attr (str text <+> drawTypeTransition <+> drawModeTransition <+> drawSimilarity)
  where
    fileExists file = G.diffFileFlags file `isFlagSet` G.diffExists
    oldExists = fileExists old
    newExists = fileExists new
    oldName = G.diffFilePath old
    newName = G.diffFilePath new
    sameName = oldName == newName
    fileMode G.DiffFile {diffFileMode = G.FilemodeBlob} = File
    fileMode G.DiffFile {diffFileMode = G.FilemodeBlobExecutable} = Executable
    fileMode G.DiffFile {diffFileMode = G.FilemodeLink} = Link
    fileMode G.DiffFile {diffFileMode = mode} = error $ "Unexpected filemode " ++ show mode
    oldMode = fileMode old
    newMode = fileMode new
    fileType file
      | G.diffFileFlags file `isFlagSet` G.diffNotBinary = Normal
    fileType _file = Binary
    oldType = fileType old
    newType = fileType new
    typeIcon Normal = fileIcon
    typeIcon Binary = floppyIcon
    drawTypeTransition
      | not (oldExists && newExists) = emptyWidget
      | oldType /= newType =
        str ", " <+> withAttr Attr.fileModified (str $ typeIcon oldType <> rightArrowIcon <> typeIcon newType)
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
      | otherwise = str ", " <+> withAttr Attr.fileModified (str drawModeTransitionStr)
    sameNameModeIcon
      | drawModeTransitionStr == "" = ""
      | otherwise = modeIcon newMode
    drawSimilarity
      | similarity == 0 = emptyWidget
      | similarity == 100 = emptyWidget
      | otherwise = str ", " <+> withAttr Attr.fileModified (str (pencilIcon <> " " <> show similarity <> "% similar"))
    (attr, text) =
      case (oldExists, newExists) of
        (True, True)
          | sameName
          , deltaType `elem` [G.DeltaModified, G.DeltaTypechange]
          , newType == Normal -> (Attr.fileModified, intercalate "" [editIcon, newName, sameNameModeIcon])
        (True, True)
          | sameName
          , deltaType `elem` [G.DeltaModified, G.DeltaTypechange] ->
            (Attr.fileModified, intercalate "" [pencilIcon, typeIcon newType, newName, sameNameModeIcon])
        (True, True)
          | not sameName
          , deltaType == G.DeltaCopied ->
            ( Attr.fileCopied
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
          , deltaType == G.DeltaRenamed ->
            ( Attr.fileRenamed
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
          | deltaType == G.DeltaAdded ->
            (Attr.fileAdded, intercalate "" [sparklesIcon, typeIcon newType, newName, modeIcon newMode])
        (True, False)
          | deltaType == G.DeltaDeleted ->
            (Attr.fileDeleted, intercalate "" [crossIcon, typeIcon oldType, oldName, modeIcon oldMode])
        _ -> (Attr.fileModified, "Unknown file change?!")

drawDeltaInfo :: G.DeltaInfo -> UI
drawDeltaInfo (hunkInfos, _diffBinaries) =
  foldM (\ui hunk -> (ui <=>) <$> drawHunkInfo (oldLineWidth, newLineWidth) hunk) emptyWidget hunkInfos
  where
    (oldLineWidth, newLineWidth) =
      foldr
        ((\(oldWidth, newWidth) (maxOldWidth, maxNewWidth) -> (max maxOldWidth oldWidth, max maxNewWidth newWidth)) .
         (\diffLine -> (length $ show $ G.diffLineOldLineno diffLine, length $ show $ G.diffLineNewLineno diffLine)))
        (0, 0) $
      concatMap snd hunkInfos

drawHunk :: G.DiffHunk -> UI
drawHunk hunk = pure cases
  where
    header = G.diffHunkHeader hunk
    strippedHeader = drop k header
      where
        idx = [i | (c, i) <- zip header [0 ..], c == '@']
        k = 1 + fromMaybe (-1) (idx ^? element 3)
    cases
      | strippedHeader /= "" = withAttr Attr.diffHeader (str $ "  " <> strippedHeader)
      | otherwise = emptyWidget

drawHunkInfo :: (Int, Int) -> G.HunkInfo -> UI
drawHunkInfo (oldLineWidth, newLineWidth) (hunk, diffLines) = do
  hunkUI <- drawHunk hunk
  let hunkHeaderUI =
        fillLine Attr.defaultAttr $ withAttr Attr.diffLineNumber (str $ center (oldLineWidth + newLineWidth + 1) "...") <+>
        withAttr Attr.diffLineNumberSep (str doubleDividingLine) <+>
        hunkUI
  linesUI <- mapM (drawLine (oldLineWidth, newLineWidth)) diffLines
  pure $ hunkHeaderUI <=> foldr1 (<=>) linesUI

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

drawLine :: (Int, Int) -> G.DiffLine -> UI
drawLine (oldLineWidth, newLineWidth) diffLine = pure cases
  where
    origin = G.diffLineOrigin diffLine
    oldLineNo = G.diffLineOldLineno diffLine
    newLineNo = G.diffLineNewLineno diffLine
    content = replace "\t" "    " $ G.diffLineContent diffLine
    drawLineNos oldLineNoStr newLineNoStr =
      foldr1
        (<+>)
        [ withAttr Attr.diffLineNumber $ str $ pad oldLineWidth oldLineNoStr
        , withAttr Attr.diffLineNumberSep $ str singleDividingLine
        , withAttr Attr.diffLineNumber $ str $ pad newLineWidth newLineNoStr
        , withAttr Attr.diffLineNumberSep $ str doubleDividingLine
        ]
    drawLine' attr lineUI = withAttr attr $ padRight Max lineUI
    cases
      | origin == ' ' = drawLineNos (show oldLineNo) (show newLineNo) <+> drawLine' Attr.diff (str content)
      | origin == '+' = drawLineNos "" (show newLineNo) <+> drawLine' Attr.diffAddedLine (str content)
      | origin == '-' = drawLineNos (show oldLineNo) "" <+> drawLine' Attr.diffDeletedLine (str content)
      | origin == '=' =
        drawLineNos "" "" <+> drawLine' Attr.diff (withAttr Attr.diffSpecialText (str $ "no " <> carriageReturnIcon))
      | origin == '>' =
        drawLineNos "" "" <+> drawLine' Attr.diffAddedLine (withAttr Attr.diffAddedText (str carriageReturnIcon))
      | origin == '<' =
        drawLineNos "" "" <+> drawLine' Attr.diffDeletedLine (withAttr Attr.diffDeletedText (str carriageReturnIcon))
      | otherwise = error "Unknown line origin"

drawOpenCommit :: S.OpenCommit -> UI
drawOpenCommit openCommit = do
  diffUI <- drawDiff (openCommit ^. field @"diffInfo")
  signaturesUI <- drawSignatures (openCommit ^. field @"openCommit")
  diffStatsUI <- drawDiffStats openCommit
  let titleUI =
        withAttr Attr.commitSummary $
        foldr1
          (<+>)
          [ str (openCommit ^. (field @"openCommit" . field @"summary"))
          , diffStatsUI
          , padRight Max (str " ")
          , withAttr Attr.fullOid $ str $ show (openCommit ^. (field @"openCommit" . field @"oid"))
          ]
  pure $ titleUI <=>
    viewport
      S.CommitDiffVP
      Vertical
      (cached S.CommitDiffUI $
       foldr1
         (<=>)
         [ fillLine Attr.defaultAttr $ str " "
         , fillLine Attr.defaultAttr $ str (openCommit ^. (field @"openCommit" . field @"body"))
         , fillLine Attr.defaultAttr $ str " "
         , signaturesUI
         , diffUI
         ])

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

boomIcon :: String
boomIcon = "\x1F4A5"

noIcon :: String
noIcon = "\x1f6ab\xFE0E"

undoIcon :: String
undoIcon = "\x21b6"

redoIcon :: String
redoIcon = "\x21b6"

topIcon :: String
topIcon = "\x2912"

bottomIcon :: String
bottomIcon = "\x2913"

warningIcon :: String
warningIcon = "\x26A0\xFE0F "

errorIcon :: String
errorIcon = "\x26D4"

drawDiffStats :: S.OpenCommit -> UI
drawDiffStats c =
  pure $
  foldr1
    (<+>)
    [ str " "
    , withAttr Attr.statsFilesModified $ str $ editIcon ++ show (G.diffStatsFilesChanged diffStats)
    , str " "
    , withAttr Attr.statsInsertions $ str $ plusIcon ++ show (G.diffStatsInsertions diffStats)
    , str " "
    , withAttr Attr.statsDeletions $ str $ minusIcon ++ show (G.diffStatsDeletions diffStats)
    ]
  where
    diffStats = c ^. field @"diffStats"

closeAction :: S.State -> EventM S.Name (Next S.State)
closeAction s =
  if isJust (s ^. field @"openCommit")
    then continue $ S.closeCommitDetails s
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

withAnimAttr :: AttrName -> Double -> Widget S.Name -> Widget S.Name
withAnimAttr attr
  | attr == Attr.notification = withBlendFgColor (base03 `on` base2) base03 base2
withAnimAttr attr
  | attr == Attr.notificationEmphasis = withBlendFgColor (base03 `on` base2 `V.withStyle` V.bold) base03 base2
withAnimAttr attr
  | attr == Attr.notificationFailure = withBlendFgColor (red `on` base2) red base2
withAnimAttr attr = \_a -> withAttr attr

withBlendFgColor :: V.Attr -> V.Color -> V.Color -> Double -> Widget S.Name -> Widget S.Name
withBlendFgColor attr c1 c2 a = modifyDefAttr $ const attr {V.attrForeColor = V.SetTo $ blendColor c1 c2 a}

blendColor :: V.Color -> V.Color -> Double -> V.Color
blendColor (V.Color240 c1) (V.Color240 c2) a =
  V.Color240 $ uncurry3 V.rgbColorToColor240 $
  blendRGB (fromJust $ V.color240CodeToRGB c1) (fromJust $ V.color240CodeToRGB c2) a
blendColor (V.RGBColor r1 g1 b1) (V.RGBColor r2 g2 b2) a = uncurry3 V.RGBColor $ blendRGB (r1, g1, b1) (r2, g2, b2) a
blendColor c1 _c2 _a = c1

gamma :: Double
gamma = 1

blendRGB :: Integral i => (i, i, i) -> (i, i, i) -> Double -> (i, i, i)
blendRGB (r1, g1, b1) (r2, g2, b2) a = (blendChannel r1 r2, blendChannel g1 g2, blendChannel b1 b2)
  where
    blendGammaCorrected c1 c2 = c1 * a + c2 * (1 - a)
    blendNormalised c1 c2 = blendGammaCorrected (c1 ** gamma) (c2 ** gamma) ** (1 / gamma)
    blendChannel c1 c2 = round $ 255 * blendNormalised (fromIntegral c1 / 255) (fromIntegral c2 / 255)

easeOut :: Double -> Double
easeOut t =
  if t < sustain
    then 1
    else 1 - ((t - sustain) / (1 - sustain)) ** easeExp
  where
    sustain = 0.8
    easeExp = 0.7

main :: BChan S.Event -> S.State -> IO ()
main bChan state = do
  terminfoDirs <- lookupEnv "TERMINFO_DIRS"
  when (isNothing terminfoDirs) (setEnv "TERMINFO_DIRS" "/etc/terminfo:/lib/terminfo:/usr/share/terminfo")
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just bChan) app state
  V.resetBackgroundColor (V.outputIface initialVty)
