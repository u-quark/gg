{-# LANGUAGE DeriveAnyClass #-}

module Libgit2.Errors (
    Libgit2Exception(..)
  , checkReturnCode
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
import Foreign.C (peekCString)
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
  poke p = undefined

-- |
-- Corresponds to giterr_last() in the C library. The name matches
-- future compatibility as this call is deprecated and replaced with
-- git_error_last() in later release of libgit2.
--
{#fun unsafe giterr_last as errorLast {} -> `Error' peek*#}

data Libgit2Exception = Libgit2Exception Error ErrorCode deriving (Eq)
instance Exception Libgit2Exception

instance Show Libgit2Exception where
    show (Libgit2Exception (Error {message = message, klass = klass}) errorCode) =
      printf "Libgit2 error %s[%d]/%s[%d]: %s"
        (show errorCode)
        (fromEnum errorCode)
        (show klass)
        (fromEnum klass)
        message

throwLastError returnCode = do
    error <- errorLast
    let exception = Libgit2Exception error (toEnum returnCode)
    throwIO exception

checkReturnCode cReturnCode = do
    let returnCode = fromIntegral cReturnCode
    if toEnum returnCode == Ok then
        pure ()
    else
        throwLastError returnCode

checkReturnCodeIter cReturnCode = do
    let returnCode = fromIntegral cReturnCode
    if toEnum returnCode == Ok then
        pure IterHasMore
    else if toEnum returnCode == Iterover then
        pure IterOver
    else
        throwLastError returnCode
