module Main where

import           Data.Time        (ZonedTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Lib              (someFunc)
import           Libgit2          (IterResult (..), OID, Repository, Revwalk,
                                   Signature (..), commitAuthor, commitBody,
                                   commitCommitter, commitLookup,
                                   commitMessageEncoding, commitSummary,
                                   commitTime, libgit2Init, newOID, oidToStrS,
                                   repositoryOpenExt, repositoryOpenNoFlags,
                                   revwalkNew, revwalkNext, revwalkPushHead)

myFormatTime :: ZonedTime -> String
myFormatTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"

formatSignature :: Signature -> String
formatSignature sig = signatureName sig <> " <" <> signatureEmail sig <> "> " <> myFormatTime (signatureWhen sig)

printCommit :: Repository -> OID -> IO ()
printCommit repo oid = do
  oidStr <- oidToStrS oid
  let shortOID = take 8 oidStr
  commit <- commitLookup repo oid
  messageEncoding <- commitMessageEncoding commit
  summary <- commitSummary commit
  time <- commitTime commit
  let timeStr = myFormatTime time
  putStrLn $ shortOID <> " " <> messageEncoding <> " " <> timeStr <> " " <> summary
  author <- commitAuthor commit
  putStrLn $ "Author: " <> formatSignature author
  committer <- commitCommitter commit
  putStrLn $ "Committer: " <> formatSignature committer
  putStrLn ""
  body <- commitBody commit
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
