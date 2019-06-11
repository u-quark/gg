{-# LANGUAGE OverloadedStrings #-}

module GG.UI
  ( main
  , Commit(..)
  ) where

import           Brick                  (App (..), AttrMap, AttrName,
                                         BrickEvent (..), EventM, Next,
                                         Padding (..), Widget, attrMap,
                                         continue, customMain, fg, halt,
                                         neverShowCursor, on, padBottom,
                                         padRight, str, withAttr, (<+>), (<=>))
import qualified Brick.Widgets.List     as L
import           Control.Lens           (over, set, to, (^.))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (ZonedTime, defaultTimeLocale,
                                         formatTime)
import           Data.Vector            (toList)
import qualified Data.Vector            as Vec
import           GG.Repo                (Action, doRebase, moveCommitUp,
                                         readCommits, readNCommits)
import           GG.State               (Commit (..), Name (..), State (..),
                                         authorEmail, authorName, authorWhen,
                                         branchName, commitList, oid,
                                         repository, revwalk, summary,
                                         updateCommitsPos, updateRepoState)
import qualified Graphics.Vty           as V

data Event

app :: App State Event Name
app =
  App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'G') [])) = continue s -- disable going to the end of the list
handleEvent s (VtyEvent (V.EvKey V.KEnd [])) = continue s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'K') [])) = doAction moveCommitUp s
handleEvent s (VtyEvent ev) = do
  l <- liftIO $ checkNeedsMoreCommits (s ^. commitList) s
  l' <- L.handleListEventVi L.handleListEvent ev l
  continue $ set commitList l' s
handleEvent s _ = continue s

checkNeedsMoreCommits :: L.List Name Commit -> State -> IO (L.List Name Commit)
checkNeedsMoreCommits l s =
  if ((l ^. L.listElementsL . to length) - (l ^. L.listSelectedL . to (fromMaybe 0))) < 500
    then do
      moreCommits <- readNCommits 500 (s ^. repository) (s ^. revwalk)
      pure $ over L.listElementsL (Vec.++ Vec.fromList moreCommits) l
    else pure l

doAction :: Action -> State -> EventM Name (Next State)
doAction a s = do
  s' <-
    liftIO $ do
      let commitHashes = map _oid (s ^. commitList . L.listElementsL . to toList)
      let pos = s ^. commitList . L.listSelectedL . to (fromMaybe 0)
      newPosM <- doRebase commitHashes pos a
      case newPosM of
        Just newPos -> do
          (revw, branch) <- readCommits $ s ^. repository
          commits <- readNCommits (pos + 500) (s ^. repository) revw
          pure $ (updateRepoState revw branch commits . updateCommitsPos newPos) s
        Nothing -> pure s
  continue s'

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

myFormatTime :: ZonedTime -> String
myFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

formatOid :: String -> String
formatOid = take 8

drawCommit :: Bool -> Commit -> Widget Name
drawCommit _selected c =
  withAttr oidAttr (str $ formatOid (c ^. oid)) <+> str " " <+> str (c ^. summary) <+> padRight Max (str " ") <+>
  withAttr authorAttr (str ((c ^. authorName) <> " <" <> (c ^. authorEmail) <> ">")) <+>
  str " " <+>
  withAttr dateAttr (str $ myFormatTime (c ^. authorWhen))

drawUI :: State -> [Widget Name]
drawUI s =
  [ padBottom Max (L.renderList drawCommit True (s ^. commitList)) <=>
    withAttr statusBarAttr (withAttr statusBranchAttr (str ("On " <> s ^. branchName)) <+> padRight Max (str " "))
  ]

rgbColor :: Integer -> Integer -> Integer -> V.Color
rgbColor = V.rgbColor

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
--base1 :: V.Color
--base1 = rgbColor 0x93 0xa1 0xa1
--
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
--red :: V.Color
--red = rgbColor 0xdc 0x32 0x2f
--
--magenta :: V.Color
--magenta = rgbColor 0xd3 0x36 0x82
--
violet :: V.Color
violet = rgbColor 0x6c 0x71 0xc4

--
--blue :: V.Color
--blue = rgbColor 0x26 0x8b 0xd2
--
--cyan :: V.Color
--cyan = rgbColor 0x2a 0xa1 0x98
--
green :: V.Color
green = rgbColor 0x85 0x99 0x00

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (L.listAttr, base03 `on` base3)
    , (L.listSelectedAttr, base03 `on` base3 `V.withStyle` V.bold)
    , (oidAttr, fg base01)
    , (authorAttr, fg violet)
    , (dateAttr, fg green)
    , (statusBarAttr, base03 `on` base2)
    , (statusBranchAttr, fg base03)
    ]

main :: State -> IO ()
main state = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing app state
