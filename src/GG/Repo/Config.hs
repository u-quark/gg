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
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GG.Repo.Config
  ( Cfg
  , CfgEntry
  , CfgEntryM
  , withCfg
  , getCfg
  , getCfgM
  , commitGpgSign
  , gpgProgram
  , gpgFormat
  , gpgOpenpgpProgram
  , userSigningKey
  ) where

import           Control.Exception (catch)
import qualified Libgit2           as G

newtype Cfg =
  Cfg G.Config

data CfgEntry t
  = CfgGit
      { _section :: String
      , _name    :: String
      , _def     :: t
      }
  | CfgGG
      { _name :: String
      , _def  :: t
      }

data CfgEntryM t
  = CfgGitM
      { __section :: String
      , __name    :: String
      }
  | CfgGGM
      { __name :: String
      }

class CfgValue t where
  getCfg :: CfgEntry t -> Cfg -> IO t
  getCfgM :: CfgEntryM t -> Cfg -> IO (Maybe t)

withCfg :: G.Repository -> (Cfg -> IO a) -> IO a
withCfg repo f = f =<< (Cfg <$> G.repositoryConfigSnapshot repo)

getCfg_ :: (G.Config -> String -> IO ct) -> (ct -> t) -> CfgEntry t -> Cfg -> IO t
getCfg_ getFn transformFn (CfgGit section name def) (Cfg config) =
  (transformFn <$> getFn config ("gg-" <> section <> "." <> name)) `catch`
  (\(_ :: G.Libgit2Exception) ->
     (transformFn <$> getFn config (section <> "." <> name)) `catch` (\(_ :: G.Libgit2Exception) -> pure def))
getCfg_ getFn transformFn (CfgGG name def) (Cfg config) =
  (transformFn <$> getFn config ("gg." <> name)) `catch` (\(_ :: G.Libgit2Exception) -> pure def)

getCfgM_ :: (G.Config -> String -> IO ct) -> (ct -> t) -> CfgEntryM t -> Cfg -> IO (Maybe t)
getCfgM_ getFn transformFn (CfgGitM section name) (Cfg config) =
  (Just . transformFn <$> getFn config ("gg-" <> section <> "." <> name)) `catch`
  (\(_ :: G.Libgit2Exception) ->
     (Just . transformFn <$> getFn config (section <> "." <> name)) `catch` (\(_ :: G.Libgit2Exception) -> pure Nothing))
getCfgM_ getFn transformFn (CfgGGM name) (Cfg config) =
  (Just . transformFn <$> getFn config ("gg." <> name)) `catch` (\(_ :: G.Libgit2Exception) -> pure Nothing)

instance CfgValue Bool where
  getCfg = getCfg_ G.configGetBool id
  getCfgM = getCfgM_ G.configGetBool id

instance CfgValue String where
  getCfg = getCfg_ G.configGetString id
  getCfgM = getCfgM_ G.configGetString id

instance CfgValue Int where
  getCfg = getCfg_ G.configGetInt64 fromIntegral
  getCfgM = getCfgM_ G.configGetInt64 fromIntegral

commitGpgSign :: CfgEntry Bool
commitGpgSign = CfgGit "commit" "gpgsign" False

gpgProgram :: CfgEntry String
gpgProgram = CfgGit "gpg" "program" "gpg"

gpgFormat :: CfgEntry String
gpgFormat = CfgGit "gpg" "format" "openpgp"

gpgOpenpgpProgram :: CfgEntryM String
gpgOpenpgpProgram = CfgGitM "gpg" "openpgp.program"

userSigningKey :: CfgEntryM String
userSigningKey = CfgGitM "user" "signingKey"
