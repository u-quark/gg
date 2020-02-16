{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2020  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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
module GG.Utils
  ( interactProcessBSS
  ) where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.DeepSeq         (NFData, rnf)
import           Control.Exception       (SomeException, bracketOnError,
                                          evaluate, handle, mask, onException,
                                          throwIO, try)
import           Control.Monad           (unless)
import qualified Data.ByteString         as BS
import           Foreign.C.Error         (Errno (..), ePIPE)
import           GHC.IO.Exception        (IOErrorType (..), IOException (..))
import           System.Exit             (ExitCode)
import           System.IO               (Handle, hClose, hGetContents,
                                          hSetBinaryMode)
import           System.Process          (CreateProcess (..), ProcessHandle,
                                          StdStream (CreatePipe),
                                          cleanupProcess, createProcess_,
                                          waitForProcess)

withCreateProcess_ ::
     String -> CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcess_ fun c action =
  bracketOnError (createProcess_ fun c) cleanupProcess (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

interactProcess ::
     (NFData stdout, NFData stderr)
  => (stdin -> Bool)
  -> (Handle -> stdin -> IO ())
  -> (Handle -> IO stdout)
  -> (Handle -> IO stderr)
  -> CreateProcess
  -> stdin
  -> IO (ExitCode, stdout, stderr)
interactProcess isNullStdin writeStdin readStdout readStderr cp input = do
  let cp_opts = cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  withCreateProcess_ "interactProcess" cp_opts $ \mb_inh mb_outh mb_errh ph ->
    case (mb_inh, mb_outh, mb_errh) of
      (Just inh, Just outh, Just errh) -> do
        out <- readStdout outh
        err <- readStderr errh
        -- fork off threads to start consuming stdout & stderr
        withForkWait (evaluate $ rnf out) $ \waitOut ->
          withForkWait (evaluate $ rnf err) $ \waitErr
           -- now write any input
           -> do
            unless (isNullStdin input) $ ignoreSigPipe $ writeStdin inh input
            -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
            ignoreSigPipe $ hClose inh
            -- wait on the output
            waitOut
            waitErr
            hClose outh
            hClose errh
            -- wait on the process
        ex <- waitForProcess ph
        return (ex, out, err)
      (Nothing, _, _) -> error "interactProcess: Failed to get a stdin handle."
      (_, Nothing, _) -> error "interactProcess: Failed to get a stdout handle."
      (_, _, Nothing) -> error "interactProcess: Failed to get a stderr handle."

interactProcessBSS :: CreateProcess -> BS.ByteString -> IO (ExitCode, String, String)
interactProcessBSS = interactProcess BS.null writeBinaryBS hGetContents hGetContents
  where
    writeBinaryBS :: Handle -> BS.ByteString -> IO ()
    writeBinaryBS h stdin = do
      hSetBinaryMode h True
      BS.hPut h stdin

withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe =
  handle $ \e ->
    case e of
      IOError {ioe_type = ResourceVanished, ioe_errno = Just ioe}
        | Errno ioe == ePIPE -> return ()
      _ -> throwIO e
