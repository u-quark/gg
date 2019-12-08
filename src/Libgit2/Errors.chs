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
{-# LANGUAGE DeriveAnyClass #-}

module Libgit2.Errors (
    Libgit2Exception(..)
  , checkReturnCode
  , checkReturnCodeIter
  , intAndCheckReturnCode
  , Error(..)
  , ErrorCode(..)
  , ErrorClass(..)
  , errorLast
)

where

import Control.Monad (liftM)
import Control.Exception (Exception, throwIO)
import Text.Printf (printf)
import Foreign (peek, Storable)
import Foreign.C (peekCString, CInt)
import Libgit2.Utils (IterResult(..))

#include "git2/errors.h"

{#context lib="git2" prefix="git_"#}

{#enum error_code as ErrorCode {underscoreToCase, upcaseFirstLetter, GIT_ERROR as GenericError} deriving (Eq, Show)#}
{#enum error_t as ErrorClass {underscoreToCase, upcaseFirstLetter} deriving (Eq, Show)#}

data Error = Error {message :: String, klass :: ErrorClass} deriving (Eq, Show)
{#pointer *error as ErrorPtr -> Error#}
instance Storable Error where
  sizeOf _ = {#sizeof error#}
  alignment _ = {#alignof error#}
  peek p = Error
    <$> (peekCString =<< ({#get error->message #} p))
    <*> liftM (toEnum . fromIntegral) ({#get error->klass #} p)
  poke _ = error "Can't poke Error"

{#fun git_error_last as errorLast {} -> `Error' peek*#}

data Libgit2Exception = Libgit2Exception Error ErrorCode deriving (Eq)
instance Exception Libgit2Exception

instance Show Libgit2Exception where
    show (Libgit2Exception (Error {message = m, klass = k}) errorCode) =
      printf "Libgit2 error %s[%d]/%s[%d]: %s"
        (show errorCode)
        (fromEnum errorCode)
        (show k)
        (fromEnum k)
        m

throwLastError :: Int -> IO a
throwLastError returnCode = do
    err <- errorLast
    let exception = Libgit2Exception err (toEnum returnCode)
    throwIO exception

checkReturnCode :: CInt -> IO ()
checkReturnCode cReturnCode = do
    let returnCode = fromIntegral cReturnCode
    if toEnum returnCode == Ok then
        pure ()
    else
        throwLastError returnCode

checkReturnCodeIter :: CInt -> IO IterResult
checkReturnCodeIter cReturnCode = do
    let returnCode = fromIntegral cReturnCode
    if toEnum returnCode == Ok then
        pure IterHasMore
    else if toEnum returnCode == Iterover then
        pure IterOver
    else
        throwLastError returnCode

intAndCheckReturnCode :: CInt -> IO Int
intAndCheckReturnCode cReturnCode = do
    let intOrReturnCode = fromIntegral cReturnCode
    if intOrReturnCode < fromEnum Ok then
        throwLastError $ toEnum intOrReturnCode
    else
        pure intOrReturnCode
