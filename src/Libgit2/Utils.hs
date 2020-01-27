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
{-# LANGUAGE LambdaCase #-}

module Libgit2.Utils
  ( peekNew
  , peekF
  , peekFCString
  , peekCBool
  , peekCInt
  , peekCLong
  , peekPeekCString
  , errorNullPeek
  , defaultNullPeek
  , errorPeekCString
  , defaultPeekCString
  , errorPeekFCString
  , defaultPeekFCString
  , maybeWithCString
  , withManyArray
  , IterResult(..)
  , peekStruct
  , pokeStruct
  , pokeStructWith
  , malloca
  , withFunPtr
  , withFunPtrM
  , alterBy
  , alterAList
  ) where

import           Control.Monad (when, (<=<))
import           Data.Int      (Int32, Int64)
import           Data.Maybe    (maybeToList)
import           Foreign       (FinalizerPtr, ForeignPtr, FunPtr, Ptr, Storable,
                                free, freeHaskellFunPtr, malloc, maybeWith,
                                newForeignPtr, nullFunPtr, nullPtr, peek,
                                withArray, withForeignPtr, withMany)
import           Foreign.C     (CInt, CLong, CString, peekCString, withCString)

peekNew :: (ForeignPtr a -> b) -> FinalizerPtr a -> Ptr (Ptr a) -> IO b
peekNew haskellConstructor cFinalizer ptr = do
  obj <- peek ptr
  fptr <- newForeignPtr cFinalizer obj
  pure $ haskellConstructor fptr

errorNullPeek :: Monad m => (Ptr a -> m b) -> String -> Ptr a -> m b
errorNullPeek peekFn errorMsg pointer =
  if pointer /= nullPtr
    then peekFn pointer
    else error errorMsg

defaultNullPeek :: Monad m => (Ptr a -> m b) -> b -> Ptr a -> m b
defaultNullPeek peekFn defaultValue pointer =
  if pointer /= nullPtr
    then peekFn pointer
    else pure defaultValue

errorPeekCString :: String -> CString -> IO String
errorPeekCString = errorNullPeek peekCString

defaultPeekCString :: String -> CString -> IO String
defaultPeekCString = defaultNullPeek peekCString

maybeWithCString :: Maybe String -> (CString -> IO ()) -> IO ()
maybeWithCString = maybeWith withCString

peekCBool :: Ptr CInt -> IO Bool
peekCBool bp = do
  b <- peek bp
  pure $ b /= 0

peekCInt :: Ptr CInt -> IO Int32
peekCInt = fmap fromIntegral <$> peek

peekCLong :: Ptr CLong -> IO Int64
peekCLong = fmap fromIntegral <$> peek

peekPeekCString :: Ptr CString -> IO String
peekPeekCString = peekCString <=< peek

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

withManyArray :: Storable s => (a -> (s -> IO b) -> IO b) -> [a] -> (Ptr s -> IO b) -> IO b
withManyArray withFoo xs f = withMany withFoo xs (`withArray` f)

peekStruct :: (Ptr s -> IO a) -> (a -> IO b) -> ForeignPtr s -> IO b
peekStruct getter outMarshaller fp =
  withForeignPtr
    fp
    (\p -> do
       cVal <- getter p
       outMarshaller cVal)

pokeStruct :: (Ptr s -> b -> IO ()) -> (a -> IO b) -> ForeignPtr s -> a -> IO ()
pokeStruct setter inMarshaller fp val =
  withForeignPtr
    fp
    (\p -> do
       cVal <- inMarshaller val
       setter p cVal)

pokeStructWith :: (Ptr s -> b -> IO ()) -> (a -> (b -> IO ()) -> IO ()) -> ForeignPtr s -> a -> IO ()
pokeStructWith setter withMarshaller fp val = withForeignPtr fp (withMarshaller val . setter)

data IterResult
  = IterHasMore
  | IterOver

malloca :: Storable a => (Ptr a -> IO b) -> IO b
malloca = (>>=) malloc

withFunPtr :: (f -> IO (FunPtr f')) -> f -> (FunPtr f' -> IO a) -> IO a
withFunPtr wrapper function action = do
  fp <- wrapper function
  res <- action fp
  freeHaskellFunPtr fp
  return res

withFunPtrM :: (f -> IO (FunPtr f')) -> Maybe f -> (FunPtr f' -> IO a) -> IO a
withFunPtrM wrapper functionM action = do
  fp <-
    case functionM of
      Just function -> wrapper function
      Nothing       -> pure nullFunPtr
  res <- action fp
  when (fp /= nullFunPtr) (freeHaskellFunPtr fp)
  return res

alterBy :: (a -> Bool) -> (Maybe a -> Maybe a) -> [a] -> [a]
alterBy p f =
  \case
    (x:xs)
      | p x -> maybeToList (f (Just x)) ++ xs
    (x:xs) -> x : alterBy p f xs
    [] -> maybeToList (f Nothing)

alterAList :: Eq k => (Maybe a -> Maybe a) -> k -> [(k, a)] -> [(k, a)]
alterAList f k = alterBy ((==) k . fst) (fmap ((,) k) . f . fmap snd)
