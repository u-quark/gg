module Libgit2.Utils (
    peekNew
  , IterResult(..)
)

where

import           Foreign (FinalizerPtr, ForeignPtr, Ptr, newForeignPtr, peek)

peekNew :: (ForeignPtr a -> b) -> FinalizerPtr a -> Ptr (Ptr a) -> IO b
peekNew haskellConstructor cDestructor ptr = do
    obj <- peek ptr
    fptr <- newForeignPtr cDestructor obj
    pure $ haskellConstructor fptr

data IterResult = IterHasMore | IterOver
