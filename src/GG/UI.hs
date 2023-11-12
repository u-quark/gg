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

{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments   #-}

module GG.UI
  ( main
  ) where

import           Brick                  hiding (attrMap, attrName)
import           Brick.BChan            (BChan)
import           Brick.Widgets.List
import           Control.Lens           ((^.), (^?), (.=), (?=),
                                         _2, element, to, set, use, mapped)
import           Control.Monad          (foldM, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, runReader, ask)
import           Data.Bits              (Bits, zeroBits, (.&.))
import           Data.Generics.Product  (field)
import           Data.List              (intercalate)
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing)
import           Data.String.Utils      (replace)
import           Data.Time              (ZonedTime, defaultTimeLocale,
                                         formatTime, zonedTimeToLocalTime,
                                         zonedTimeZone)
import           GHC.Generics           (Generic)
import qualified GG.Actions             as A
import qualified GG.Repo                as R
import qualified GG.State               as S
import           GG.Timers              (AnimationCb, AnimationEndCb, Duration,
                                         addAnimation, tickEventHandler)
import qualified GG.UI.Attrs            as Attr
import           GG.UI.Theme            (getAttrMap)
import           GG.Utils               (uncurry3)
import qualified Graphics.Vty           as V
import           Graphics.Vty.Platform.Unix
                                        (mkVty)
import qualified Libgit2                as G
import           Prelude                hiding (head)
import           System.Environment     (lookupEnv, setEnv)

data Env =
  Env
    { attrMap :: AttrMap
    }
  deriving (Generic)

getEnv :: S.State -> Env
getEnv s = Env {..}
  where
    attrMap = getAttrMap_ s

type UI = Reader Env (Widget S.Name)

app :: App S.State S.Event S.Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = getAttrMap_
    }

getAttrMap_ :: S.State -> AttrMap
getAttrMap_ s = getAttrMap $ s ^. field @"config" . field @"ui" . field @"theme"

startEvent :: S.ModifyState ()
startEvent = do
  state <- get
  let attrMap = getAttrMap_ state
  let attr = attrMapLookup Attr.defaultAttr attrMap
  let bgC_ = case V.attrBackColor attr of
                V.SetTo bgC -> bgC
                _ -> error "We should always have a defaultAttr color"
  vty <- getVtyHandle
  liftIO $ V.setBackgroundColor (V.outputIface vty) bgC_

type EventHandler event = event -> S.ModifyState ()

type Event = BrickEvent S.Name S.Event

handleEvent :: EventHandler Event
handleEvent (AppEvent S.Tick) = do
  timers <- use (field @"timers")
  tickEventHandler timers
handleEvent e = do
  handleKeyboardActions e
  handleScrolling e

handleKeyboardActions :: EventHandler Event
handleKeyboardActions (VtyEvent (V.EvKey key mods)) = case (key, mods) of
    ((V.KChar 'q'), []) -> closeAction
    (V.KEsc,        []) -> closeAction
    (V.KEnter,      []) -> openCommitAction
    ((V.KChar 'K'), []) -> handleRebaseAction A.MoveUpA
    ((V.KChar 'J'), []) -> handleRebaseAction A.MoveDownA
    ((V.KChar 'S'), []) -> handleRebaseAction A.SquashA
    ((V.KChar 'F'), []) -> handleRebaseAction A.FixupA
    ((V.KChar 'D'), []) -> handleRebaseAction A.DeleteA
    ((V.KChar 'Z'), []) -> handleAction A.UndoA
    ((V.KChar 'R'), []) -> handleAction A.RedoA
    _ -> pure ()
handleKeyboardActions _ = pure ()

handleScrolling :: EventHandler Event
handleScrolling e = do
  openCommitM <- use (field @"openCommit")
  if isJust openCommitM
    then handleViewportScrolling S.CommitDiffVP commit_diff_vps e
    else handleCommitsListScrolling e
  where
    commit_diff_vps = viewportScroll S.CommitDiffVP

handleCommitsListScrolling :: EventHandler Event
handleCommitsListScrolling (VtyEvent (V.EvKey (V.KChar 'G') [])) = pure () -- disable going to the end of the list
handleCommitsListScrolling (VtyEvent (V.EvKey V.KEnd [])) = pure ()
handleCommitsListScrolling e = do
  checkNeedsMoreCommits
  zoom (field @"commitList") do
       handleListEventMouse S.CommitListUI (handleListEventVi handleListEvent) e

handleListEventMouse :: (Foldable t, Splittable t, Ord n)
                  => S.Name
                  -> (V.Event -> EventM n (GenericList n t e) ())
                  -> Event
                  -> EventM n (GenericList n t e) ()
handleListEventMouse name fallback e = do
  case e of
    MouseDown name' V.BScrollDown _ _ | name == name' -> modify listMoveDown
    MouseDown name' V.BScrollUp   _ _ | name == name' -> modify listMoveUp
    VtyEvent vty_event -> fallback vty_event
    _ -> pure ()

handleViewportScrolling :: S.Name -> ViewportScroll S.Name -> EventHandler Event
handleViewportScrolling name vps e = case e of
  VtyEvent (V.EvKey key mods) -> handleKeyboard key mods
  MouseDown name' button _ _ | name == name' -> handleMouse button
  _ -> pure ()
  where
    handleKeyboard key mods = case (key, mods) of
      (V.KUp, [])                -> flip vScrollBy (-1) vps
      (V.KDown, [])              -> flip vScrollBy 1 vps
      (V.KHome, [])              -> vScrollToBeginning vps
      (V.KEnd, [])               -> vScrollToEnd vps
      (V.KPageDown, [])          -> flip vScrollPage Down vps
      (V.KPageUp, [])            -> flip vScrollPage Up vps
      ((V.KChar 'k'), [])        -> flip vScrollBy (-1) vps
      ((V.KChar 'j'), [])        -> flip vScrollBy 1 vps
      ((V.KChar 'g'), [])        -> vScrollToBeginning vps
      ((V.KChar 'G'), [])        -> vScrollToEnd vps
      ((V.KChar 'f'), [V.MCtrl]) -> flip vScrollPage Down vps
      ((V.KChar 'b'), [V.MCtrl]) -> flip vScrollPage Up vps
      _ -> pure ()
    handleMouse button = case button of
      V.BScrollDown -> flip vScrollBy 5 vps
      V.BScrollUp   -> flip vScrollBy (-5) vps
      _ -> pure ()

checkNeedsMoreCommits :: S.ModifyState ()
checkNeedsMoreCommits = do
  currentSize <- use (field @"commitList" . listElementsL . to length)
  selectedIx <- use (field @"commitList" . listSelectedL . to (fromMaybe 0))
  when (currentSize - selectedIx < 500) do
    contCommit <- use (field @"contCommit")
    (moreCommits, contCommit') <- liftIO $ fetchMoreCommits 500 contCommit
    S.addMoreCommits moreCommits contCommit'

fetchMoreCommits :: Int -> G.Commit -> IO ([S.Commit], G.Commit)
fetchMoreCommits numCommits contCommit = do
  (moreCommits, contCommit') <- R.readNCommits numCommits contCommit
  moreCommitsState <- mapM R.readCommit moreCommits
  pure (moreCommitsState, contCommit')

handleActionSuccessReload :: Int -> A.ActionSummary String -> S.ModifyState ()
handleActionSuccessReload newPos _summary = do
  repo <- use (field @"repository")
  (head, headCommit) <- liftIO $ R.readRepoState repo
  (moreCommitsState, contCommit) <- liftIO $ fetchMoreCommits (newPos + 500) headCommit
  headCommit' <- liftIO $ R.readCommit headCommit
  S.updateRepoState contCommit head (headCommit' : moreCommitsState)
  S.updateCommitsPos newPos
  field @"notification" .= Nothing

handleActionFailure :: A.ActionFailure -> S.ModifyState ()
handleActionFailure failure = do
  field @"notification" ?= (S.ActionFailure failure, 1)
  timers <- use (field @"timers")
  liftIO $ addAnimation
    timers
    S.NotificationT
    (failureNotificationDuration failure)
    notificationAnimation
    notificationAnimationEnd

handleActionWarning :: A.ActionWarning -> S.ModifyState ()
handleActionWarning warning = do
  field @"notification" ?= (S.ActionWarning warning, 1)
  timers <- use (field @"timers")
  liftIO $ addAnimation
    timers
    S.NotificationT
    (warningNotificationDuration warning)
    notificationAnimation
    notificationAnimationEnd

handleAction :: A.Action -> S.ModifyState ()
handleAction action = do
  repo <- use (field @"repository")
  result <- liftIO $ A.doAction repo action
  case result of
    A.Success newPos summary maybeWarning -> do
      handleActionSuccessReload newPos summary
      case maybeWarning of
        Nothing      -> pure ()
        Just warning -> handleActionWarning warning
    A.Failure failure -> handleActionFailure failure

handleRebaseAction :: A.RebaseAction -> S.ModifyState ()
handleRebaseAction rebaseAction = do
  pos <- use $ field @"commitList" . listSelectedL . to (fromMaybe 0)
  handleAction $ A.RebaseAction pos rebaseAction

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
    env = getEnv s
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
drawNotification notificationM = do
  env <- ask
  let attrMap = env ^. field @"attrMap"
  pure $ case notificationM of
    Nothing -> emptyWidget
    Just (notification, opacity) ->
      case notification of
        S.ActionFailure actionFailure ->
          case actionFailure of
            A.RebaseConflict commit baseCommit ->
              withAnimAttr Attr.notification attrMap opacity $ str (iconMaybe boomIcon) <+> str "Conflicts applying commit " <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str commit) <+>
              str " on top of commit " <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str baseCommit)
            A.RebaseMergeCommit commit ->
              withAnimAttr Attr.notification attrMap opacity $ str (iconMaybe boomIcon) <+> str "Can not apply merge commit " <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str commit)
            A.GPGError code errorMsg ->
              withAnimAttr Attr.notification attrMap opacity $ str (iconMaybe errorIcon) <+> str " GPG error: [" <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str (show code)) <+>
              str "] " <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str $ replace "\n" " " errorMsg)
            A.UndoFailure _ -> withAnimAttr Attr.notificationFailure attrMap opacity (str $ noIcon <> undoIcon)
            A.RedoFailure _ -> withAnimAttr Attr.notificationFailure attrMap opacity (str $ noIcon <> redoIcon)
            A.ReachedTop -> withAnimAttr Attr.notificationFailure attrMap opacity (str topIcon)
            A.ReachedBottom -> withAnimAttr Attr.notificationFailure attrMap opacity (str bottomIcon)
            A.InvalidAction -> withAnimAttr Attr.notificationFailure attrMap opacity (str noIcon)
        S.ActionWarning actionWarning ->
          case actionWarning of
            A.X509SigningNotSupported ->
              withAnimAttr Attr.notification attrMap opacity $ str (iconMaybe warningIcon) <+>
              str " Commit Singing with X509 is not supported. Disable with " <+>
              withAnimAttr Attr.notificationEmphasis attrMap opacity (str "gg-gpg.format = false")
      where iconMaybe icon =
              if opacity > 0.6
                then icon
                else ""

openCommitAction :: S.ModifyState ()
openCommitAction = do
  repo <- use (field @"repository")
  selectedIx <- use (field @"commitList" . listSelectedL . to fromJust)
  oid <- use (S.commitL selectedIx . field @"oid")
  (diffStats, diffInfo_) <- liftIO $ R.readCommitDiff repo oid
  S.openCommitDetails selectedIx (diffStats, diffInfo_)
  let vps = viewportScroll S.CommitDiffVP
  vScrollToBeginning vps
  invalidateCacheEntry S.CommitDiffUI

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

closeAction :: S.ModifyState ()
closeAction = do
  openCommitM <- use (field @"openCommit")
  if isJust openCommitM
    then S.closeCommitDetails
    else halt

withAnimAttr :: AttrName -> AttrMap -> Double -> Widget S.Name -> Widget S.Name
withAnimAttr attrName attrMap = withFadeout $ attrMapLookup attrName attrMap

withFadeout :: V.Attr -> Double -> Widget S.Name -> Widget S.Name
withFadeout attr a = case ((V.attrForeColor attr), (V.attrBackColor attr)) of
  (V.SetTo fgC, V.SetTo bgC) -> withBlendFg attr fgC bgC a
  _ -> id

withBlendFg :: V.Attr -> V.Color -> V.Color -> Double -> Widget S.Name -> Widget S.Name
withBlendFg attr c1 c2 a = modifyDefAttr $ const attr {V.attrForeColor = V.SetTo $ blendColor c1 c2 a}

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

buildVty :: IO V.Vty
buildVty = do
  vty <- mkVty V.defaultConfig
  let output_interface = V.outputIface vty
  when (V.supportsMode output_interface V.Mouse) $
        liftIO $ V.setMode output_interface V.Mouse True
  return vty

main :: BChan S.Event -> S.State -> IO ()
main bChan state = do
  terminfoDirs <- lookupEnv "TERMINFO_DIRS"
  when (isNothing terminfoDirs) (setEnv "TERMINFO_DIRS" "/etc/terminfo:/lib/terminfo:/usr/share/terminfo")
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just bChan) app state
  V.resetBackgroundColor (V.outputIface initialVty)
