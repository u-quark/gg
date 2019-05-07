module Main where

import Lib     (someFunc)
import Libgit2 (libgit2Init)

main :: IO ()
main = do
    someFunc
    r <- libgit2Init
    putStrLn $ show r
