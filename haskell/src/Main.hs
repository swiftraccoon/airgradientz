module Main (main) where

import Control.Concurrent.MVar (newMVar)
import System.IO (hSetBuffering, stderr, BufferMode(..))
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(CatchOnce))
import System.Exit (exitSuccess)

import Config (loadConfig, Config(..))
import DB (withDB)
import Log (logMsg)
import Poller (startPoller)
import Server (runServer, newServerStats)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering

  cfg <- loadConfig

  withDB (cfgDBPath cfg) $ \db -> do
    dbMVar <- newMVar db

    pollerH <- startPoller dbMVar cfg
    stats <- newServerStats

    let shutdown = do
          logMsg "[server] Shutting down"
          exitSuccess
    _ <- installHandler sigINT (CatchOnce shutdown) Nothing
    _ <- installHandler sigTERM (CatchOnce shutdown) Nothing

    logMsg "[server] Starting AirGradientz (Haskell)"
    runServer dbMVar cfg pollerH stats
