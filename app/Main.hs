module Main where

import Lib     (someFunc)
import Libgit2 (libgit2Init, repositoryOpen)

main :: IO ()
main = do
    someFunc
    r <- libgit2Init
    putStrLn $ show r
    repo <- repositoryOpen "."
    pure ()
