module Main where

import           Lib     (someFunc)
import           Libgit2 (IterResult (..), Revwalk, libgit2Init, newOID,
                          oidToStrS, repositoryOpenExt, repositoryOpenNoFlags,
                          revwalkNew, revwalkNext, revwalkPushHead)

printOIDs :: Revwalk -> IO ()
printOIDs revwalk = do
  oid <- newOID
  loop oid
  where
    loop oid = do
      ir <- revwalkNext oid revwalk
      case ir of
        IterHasMore -> do
          oidStr <- oidToStrS oid
          putStrLn oidStr
          loop oid
        IterOver -> pure ()

main :: IO ()
main = do
  someFunc
  _ <- libgit2Init
  repo <- repositoryOpenExt "." repositoryOpenNoFlags ""
  revwalk <- revwalkNew repo
  revwalkPushHead revwalk
  printOIDs revwalk
