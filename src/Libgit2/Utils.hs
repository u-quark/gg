module Libgit2.Utils
  ( peekNew
  , peekF
  , peekFCString
  , errorNullPeek
  , maybeNullPeek
  , defaultNullPeek
  , errorPeekCString
  , defaultPeekCString
  , errorPeekFCString
  , defaultPeekFCString
  , fprCtor_
  , IterResult(..)
  , malloca
  ) where

import           Foreign   (FinalizerPtr, ForeignPtr, Ptr, Storable, free,
                            malloc, newForeignPtr, newForeignPtr_, nullPtr,
                            peek)
import           Foreign.C (CString, peekCString)

peekNew :: (ForeignPtr a -> b) -> FinalizerPtr a -> Ptr (Ptr a) -> IO b
peekNew haskellConstructor cDestructor ptr = do
  obj <- peek ptr
  fptr <- newForeignPtr cDestructor obj
  pure $ haskellConstructor fptr

errorNullPeek :: Monad m => (Ptr a -> m b) -> String -> Ptr a -> m b
errorNullPeek peekFn errorMsg pointer =
  if pointer /= nullPtr
    then peekFn pointer
    else error errorMsg

maybeNullPeek :: Monad m => (Ptr a -> m b) -> Ptr a -> m (Maybe b)
maybeNullPeek peekFn pointer =
  if pointer /= nullPtr
    then do
      result <- peekFn pointer
      pure $ Just result
    else pure Nothing

defaultNullPeek :: Monad m => (Ptr a -> m b) -> b -> Ptr a -> m b
defaultNullPeek peekFn defaultValue pointer =
  if pointer /= nullPtr
    then peekFn pointer
    else pure defaultValue

errorPeekCString :: String -> CString -> IO String
errorPeekCString = errorNullPeek peekCString

defaultPeekCString :: String -> CString -> IO String
defaultPeekCString = defaultNullPeek peekCString

peekF :: (Ptr a -> IO b) -> Ptr a -> IO b
peekF peekFn p = do
  res <- peekFn p
  free p
  pure res

peekFCString :: CString -> IO String
peekFCString = peekF peekCString

errorPeekFCString :: String -> CString -> IO String
errorPeekFCString = errorNullPeek peekFCString

defaultPeekFCString :: String -> CString -> IO String
defaultPeekFCString = defaultNullPeek peekFCString

fprCtor_ :: (ForeignPtr a -> b) -> Ptr a -> IO b
fprCtor_ ctor p = do
  fpr <- newForeignPtr_ p
  pure $ ctor fpr

data IterResult
  = IterHasMore
  | IterOver

malloca :: Storable a => (Ptr a -> IO b) -> IO b
malloca = (>>=) malloc
