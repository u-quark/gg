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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GG.Main
  ( main
  ) where

import           Brick.BChan           (newBChan)
import           Control.Exception     (Handler (..), bracket, catches)
import           Control.Lens          ((^.))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import           GG.Config             (readConfig)
import qualified GG.Repo               as R
import qualified GG.State              as S
import           GG.Timers             (initTimers)
import qualified GG.UI                 as UI
import           Network.HTTP.Client   (HttpException (..),
                                        HttpExceptionContent (..))
import qualified Network.Wreq          as W
import           Prelude               hiding (head)
import           System.Directory      (canonicalizePath, copyFile,
                                        copyPermissions, getTemporaryDirectory,
                                        removeFile)
import           System.Environment    (getArgs)
import           System.IO             (hClose, hPrint, hPutStrLn, openTempFile,
                                        stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--upgrade"] -> downloadUpgrade
    []            -> openRepoUI
    _             -> hPutStrLn stderr "Unknown command line arguments"

openRepoUI :: IO ()
openRepoUI = do
  repo <- R.readRepository
  config <- readConfig repo
  (head, headCommit) <- R.readRepoState repo
  (tailCommits, contCommit) <- R.readNCommits 999 headCommit
  bChan <- newBChan 10
  timers <- initTimers S.Tick 100000 bChan
  commitsState <- mapM R.readCommit (headCommit : tailCommits)
  UI.main bChan $ S.initState config repo contCommit head commitsState timers

releaseURL :: String
releaseURL = "https://github.com/u-quark/gg/releases/download/nightly/gg"

handleHttException :: Handler ()
handleHttException = Handler handler
  where
    handler :: HttpException -> IO ()
    handler (HttpExceptionRequest _ (StatusCodeException response _)) = do
      let status = response ^. W.responseStatus
      let code = status ^. W.statusCode
      let message = BS.unpack $ status ^. W.statusMessage
      hPutStrLn stderr $ "Error downloading: " <> show code <> " - " <> message
    handler (HttpExceptionRequest _ e) = hPrint stderr e
    handler e = hPrint stderr e

downloadUpgrade :: IO ()
downloadUpgrade = do
  tmpDir <- getTemporaryDirectory
  bracket
    (openTempFile tmpDir "gg-upgrade-")
    (removeFile . fst)
    (\(tempFileName, tempFileHandle) -> do
       putStrLn $ "Downloading upgrade from " <> releaseURL
       response <- W.get releaseURL
       let responseBody = response ^. W.responseBody
       LBS.hPut tempFileHandle responseBody
       hClose tempFileHandle
       realExecPath <- canonicalizePath "/proc/self/exe"
       putStrLn $ "Upgrading gg at " <> realExecPath
       copyPermissions realExecPath tempFileName
       removeFile realExecPath
       copyFile tempFileName realExecPath) `catches`
    [handleHttException]
