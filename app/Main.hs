module Main where

import Lib     (someFunc)
import Libgit2 (libgit2Init, repositoryOpenExt, repositoryOpenNoFlags)

main :: IO ()
main = do
    someFunc
    r <- libgit2Init
    putStrLn $ show r
    repo <- repositoryOpenExt "./src/Libgit2" repositoryOpenNoFlags ""
    pure ()
