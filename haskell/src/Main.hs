module Main (main) where

import Control.Concurrent.MVar (newMVar)
import System.IO (hPutStrLn, stderr, hSetBuffering, BufferMode(..))
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(CatchOnce))
import System.Exit (exitSuccess)

import Config (loadConfig, Config(..))
import DB (withDB)
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
          hPutStrLn stderr "[server] Shutting down"
          exitSuccess
    _ <- installHandler sigINT (CatchOnce shutdown) Nothing
    _ <- installHandler sigTERM (CatchOnce shutdown) Nothing

    hPutStrLn stderr "[server] Starting AirGradientz (Haskell)"
    runServer dbMVar cfg pollerH stats
