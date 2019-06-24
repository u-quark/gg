module Main where

import           GG.Repo     (readCommit, readNCommits, readRepoState,
                              readRepository)
import qualified GG.State    as S
import qualified GG.UI       as UI
import           Libgit2     (Commit, DiffFile, DiffFileCb, DiffHunkCb,
                              DiffLineCb, Repository, commitParent, commitTree,
                              diffDefaultOptions, diffDeltaNewFile,
                              diffDeltaOldFile, diffFileFlags, diffFileId,
                              diffFileIdAbbrev, diffFileMode, diffFilePath,
                              diffFileSize, diffForEach, diffGetStats,
                              diffHunkHeader, diffHunkNewLines,
                              diffHunkNewStart, diffHunkOldLines,
                              diffHunkOldStart, diffLineContent,
                              diffLineNewLineno, diffLineOldLineno,
                              diffLineOrigin, diffNumDeltas, diffStatsDeletions,
                              diffStatsFilesChanged, diffStatsInsertions,
                              diffTreeToTree, nullPayload)
import           Text.Printf (printf)

showCommitDiff :: Repository -> Commit -> IO ()
showCommitDiff repo commit = do
  tree <- commitTree commit
  parentCommit <- commitParent commit 0
  parentTree <- commitTree parentCommit
  diffOptions <- diffDefaultOptions
  diff <- diffTreeToTree repo parentTree tree diffOptions
  numDeltas <- diffNumDeltas diff
  putStrLn $ "Deltas: " <> show numDeltas
  diffStats <- diffGetStats diff
  changed <- diffStatsFilesChanged diffStats
  insertions <- diffStatsInsertions diffStats
  deletions <- diffStatsDeletions diffStats
  putStrLn $ "Files changed: " <> show changed <> " insertions: " <> show insertions <> " deletions: " <> show deletions
  _ <- diffForEach diff printFileDiff Nothing (Just printHunkDiff) (Just printLineDiff) nullPayload
  pure ()

printDiffFile :: DiffFile -> IO ()
printDiffFile df = do
  print $ diffFileId df
  putStrLn $
    unwords
      [ diffFilePath df
      , "Size:"
      , show $ diffFileSize df
      , "bytes, Mode:"
      , show $ diffFileMode df
      , "Flags:"
      , show $ diffFileFlags df
      , "Abrev:"
      , show $ diffFileIdAbbrev df
      ]

printFileDiff :: DiffFileCb
printFileDiff delta progress _payload = do
  putStrLn $ "Progress: " <> show (100 * progress) <> "%"
  putStr "--- a/"
  printDiffFile $ diffDeltaOldFile delta
  putStr "+++ b/"
  printDiffFile $ diffDeltaNewFile delta
  pure 0

printHunkDiff :: DiffHunkCb
printHunkDiff _delta hunk _payload = do
  putStrLn $
    concat
      [ "@@ -"
      , show $ diffHunkOldStart hunk
      , ","
      , show $ diffHunkOldLines hunk
      , " +"
      , show $ diffHunkNewStart hunk
      , ","
      , show $ diffHunkNewLines hunk
      , " @@ "
      , diffHunkHeader hunk
      ]
  pure 0

printLineDiff :: DiffLineCb
printLineDiff _delta _hunk line _payload = do
  case diffLineOrigin line of
    c
      | c == ' ' || c == '=' ->
        printf
          "%c%4d %4d: %s"
          (diffLineOrigin line)
          (diffLineOldLineno line)
          (diffLineNewLineno line)
          (diffLineContent line)
    c
      | c == '+' || c == '>' ->
        printf "%c     %4d: %s" (diffLineOrigin line) (diffLineNewLineno line) (diffLineContent line)
    c
      | c == '-' || c == '<' ->
        printf "%c%4d     : %s" (diffLineOrigin line) (diffLineOldLineno line) (diffLineContent line)
    _ -> error "Unknown line origin"
  pure 0

main :: IO ()
main = do
  repo <- readRepository
  (branch, headCommit) <- readRepoState repo
  (tailCommits, contCommit) <- readNCommits 999 headCommit
  commitsState <- mapM readCommit (headCommit : tailCommits)
  showCommitDiff repo headCommit
  UI.main $ S.initState repo contCommit branch commitsState
