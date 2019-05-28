module Libgit2.Utils (
    peekNew
  , maybeNullPeek
  , defaultNullPeek
  , defaultPeekCString
  , IterResult(..)
)

where

import           Foreign   (FinalizerPtr, ForeignPtr, Ptr, newForeignPtr,
                            nullPtr, peek)
import           Foreign.C (CString, peekCString)

peekNew :: (ForeignPtr a -> b) -> FinalizerPtr a -> Ptr (Ptr a) -> IO b
peekNew haskellConstructor cDestructor ptr = do
    obj <- peek ptr
    fptr <- newForeignPtr cDestructor obj
    pure $ haskellConstructor fptr

maybeNullPeek :: Monad m => (Ptr a -> m b) -> Ptr a -> m (Maybe b)
maybeNullPeek peekFn pointer =
  if pointer /= nullPtr then do
    result <- peekFn pointer
    pure $ Just result
  else
    pure Nothing

defaultNullPeek :: Monad m => (Ptr a -> m b) -> b -> Ptr a -> m b
defaultNullPeek peekFn defaultValue pointer =
  if pointer /= nullPtr then
    peekFn pointer
  else
    pure defaultValue

defaultPeekCString :: String -> CString -> IO String
defaultPeekCString = defaultNullPeek peekCString

data IterResult = IterHasMore | IterOver
