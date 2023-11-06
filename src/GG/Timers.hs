{-
  This file is part of gg - git (G)UI.
  Copyright (C) 2019-2023  Lefteris Kritikos <eleftherios.kritikos@gmail.com>

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

module GG.Timers
  ( Timers
  , initTimers
  , addAnimation
  , tickEventHandler
  , AnimationCb
  , AnimationEndCb
  , Duration
  ) where

import qualified Brick                   as B
import qualified Brick.BChan             as B
import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_,
                                          newEmptyMVar, newMVar, readMVar,
                                          tryPutMVar, tryTakeMVar)
import           Control.Monad           (foldM, forever, void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Fixed              (Pico)
import           Data.List               (nub)
import           Data.Map                (Map, difference, elems, empty, filter,
                                          insert, null)
import           Data.Time.Clock         (UTCTime, diffUTCTime, getCurrentTime,
                                          nominalDiffTimeToSeconds)
import           Prelude                 hiding (filter, null)

type Resolution = Int

type Duration = Pico

type Interval = Double

data Timer state name =
  AnimationTimer UTCTime Duration (AnimationCb state name) (AnimationEndCb state name)

data Timers state name timerName =
  Timers (MVar (Map timerName (Timer state name))) (MVar ())

type AnimationCb state name = Interval -> state -> IO (state, [name])

type AnimationEndCb state name = state -> IO (state, [name])

initTimers :: event -> Resolution -> B.BChan event -> IO (Timers state name timerName)
initTimers tickEvent resolution bChan = do
  blocker <- newEmptyMVar
  timersMVar <- newMVar empty
  _ <- forkIO $ forever $ loop blocker
  let timers = Timers timersMVar blocker
  pure timers
  where
    loop blocker = do
      _ <- readMVar blocker
      B.writeBChan bChan tickEvent
      threadDelay resolution

tickEventHandler :: (Ord a, Ord timerName) => Timers s a timerName -> B.EventM a s ()
tickEventHandler timers = do
  s <- B.get
  (s', ns') <- liftIO $ timersHandler timers s
  B.put s'
  mapM_ B.invalidateCacheEntry ns'

timersHandler :: (Ord name, Eq name, Ord timerName) => Timers state name timerName -> state -> IO (state, [name])
timersHandler (Timers timersMVar blocker) state = do
  now <- getCurrentTime
  modifyMVar
    timersMVar
    (\timers -> do
       let timers' = filter (timerNotExpired now) timers
       let expiredTimers = difference timers timers'
       (s', ns') <- foldM (handleTimer now) (state, []) (elems timers')
       (s'', ns'') <- foldM handleExpiredTimer (s', ns') (elems expiredTimers)
       if null timers'
         then void $ tryTakeMVar blocker
         else void $ tryPutMVar blocker ()
       pure (timers', (s'', ns'')))
  where
    computeInterval start now duration =
      min 1 $ realToFrac $ nominalDiffTimeToSeconds (diffUTCTime now start) / duration
    handleTimer now (s, ns) (AnimationTimer start duration cb _ecb) = do
      let interval = computeInterval start now duration
      (s', ns') <- cb interval s
      pure (s', nub $ ns ++ ns')
    handleExpiredTimer (s, ns) (AnimationTimer _start _duration _cb ecb) = do
      (s', ns') <- ecb s
      pure (s', nub $ ns ++ ns')
    timerNotExpired now (AnimationTimer start duration _cb _ecb) =
      nominalDiffTimeToSeconds (diffUTCTime now start) < duration

addAnimation ::
     (Ord timerName)
  => Timers state name timerName
  -> timerName
  -> Duration
  -> AnimationCb state name
  -> AnimationEndCb state name
  -> IO ()
addAnimation (Timers timersMVar blocker) name duration cb ecb = do
  now <- getCurrentTime
  let timer = AnimationTimer now duration cb ecb
  modifyMVar_
    timersMVar
    (\timers -> do
       void $ tryPutMVar blocker ()
       pure $ insert name timer timers)
