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
{-# LANGUAGE ScopedTypeVariables #-}

module GG.Repo.Commit
  ( CreateCommitResult(..)
  , createCommit
  ) where

import           Control.Exception (IOException, catch)
import qualified Data.ByteString   as BS
import           GG.Config         (getCfg, getCfgM, withCfg)
import qualified GG.Config         as CFG
import           GG.Utils          (interactProcessBSS)
import qualified Libgit2           as G
import           System.Exit       (ExitCode (..))
import           System.Process    (proc)

data CreateCommitResult
  = Success G.OID
  | WarningX509SigningNotSupported G.OID
  | GPGError Int String

data SignConfig
  = DoNotSignCommits
  | UseX509
  | UseOpenGP
      { _program :: String
      , _keyM    :: Maybe String
      }

getSignConfig :: G.Repository -> IO SignConfig
getSignConfig repo =
  withCfg repo $ \cfg -> do
    signCommits <- getCfg CFG.commitGpgSign cfg
    if not signCommits
      then pure DoNotSignCommits
      else do
        format <- getCfg CFG.gpgFormat cfg
        if format == "openpgp"
          then do
            programM <- getCfgM CFG.gpgOpenpgpProgram cfg
            program <-
              case programM of
                Just program -> pure program
                Nothing      -> getCfg CFG.gpgProgram cfg
            keyM <- getCfgM CFG.userSigningKey cfg
            pure $ UseOpenGP program keyM
          else pure UseX509

getSignature :: BS.ByteString -> String -> Maybe String -> IO (Either (Int, String) String)
getSignature commitContent program keyM = do
  let pArgs = ["-bsa"] <> maybe [] (\key -> ["-u", key]) keyM
  (exitCode, signature, errorMsg) <-
    interactProcessBSS (proc program pArgs) commitContent `catch`
    (\(e :: IOException) -> pure (ExitFailure 1, "", show e))
  case exitCode of
    ExitSuccess      -> pure $ Right signature
    ExitFailure code -> pure $ Left (code, errorMsg)

createCommit :: G.Repository -> G.Signature -> String -> G.Tree -> [G.Commit] -> IO CreateCommitResult
createCommit repo author message tree parents = do
  signConfig <- getSignConfig repo
  committer <- G.signatureDefault repo
  commitContent <- G.commitCreateBuffer repo author committer "UTF-8" message tree parents
  signatureME <-
    case signConfig of
      DoNotSignCommits -> pure $ Right Nothing
      UseX509 -> pure $ Right Nothing
      UseOpenGP program keyM -> do
        signatureE <- getSignature commitContent program keyM
        case signatureE of
          Left errorMsg   -> pure $ Left errorMsg
          Right signature -> pure $ Right $ Just signature
  case signatureME of
    Left (code, errorMsg) -> pure $ GPGError code errorMsg
    Right signatureM -> do
      newCommitOid <- G.commitCreateWithSignature repo commitContent signatureM Nothing
      pure $
        (case signConfig of
           UseX509 -> WarningX509SigningNotSupported
           _       -> Success)
          newCommitOid
