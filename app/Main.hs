module Main where

import           Lib     (someFunc)
import           Libgit2 (IterResult (..), OID, Repository, Revwalk, commitBody,
                          commitLookup, commitMessageEncoding, commitSummary,
                          libgit2Init, newOID, oidToStrS, repositoryOpenExt,
                          repositoryOpenNoFlags, revwalkNew, revwalkNext,
                          revwalkPushHead)

printCommit :: Repository -> OID -> IO ()
printCommit repo oid = do
  oidStr <- oidToStrS oid
  let shortOID = take 8 oidStr
  commit <- commitLookup repo oid
  messageEncoding <- commitMessageEncoding commit
  summary <- commitSummary commit
  body <- commitBody commit
  putStrLn $ shortOID <> " " <> messageEncoding <> " " <> summary
  putStrLn ""
  if body /= "" then do
    putStrLn body
    putStrLn ""
  else
    pure ()

printOIDs :: Repository -> Revwalk -> IO ()
printOIDs repo revwalk = do
  oid <- newOID
  loop oid
  where
    loop oid = do
      ir <- revwalkNext oid revwalk
      case ir of
        IterHasMore -> do
          printCommit repo oid
          loop oid
        IterOver -> pure ()

main :: IO ()
main = do
  someFunc
  _ <- libgit2Init
  repo <- repositoryOpenExt "." repositoryOpenNoFlags ""
  revwalk <- revwalkNew repo
  revwalkPushHead revwalk
  printOIDs repo revwalk
