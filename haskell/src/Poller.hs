{-# LANGUAGE OverloadedStrings #-}

module Poller
  ( PollerHandle
  , startPoller
  , getHealthJSON
  , getPollStats
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, withMVar, newMVar, modifyMVar_)
import Control.Exception (SomeException, try)
import Data.Aeson (Value(..), object, (.=), eitherDecodeStrict')
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Config (Config(..), DeviceConfig(..))
import DB (DBHandle, insertReading, checkpoint, nowMillis)

data DeviceHealth = DeviceHealth
  { dhIp                  :: !T.Text
  , dhLabel               :: !T.Text
  , dhStatus              :: !T.Text
  , dhLastSuccess         :: !(Maybe Int64)
  , dhLastError           :: !(Maybe Int64)
  , dhLastErrorMessage    :: !(Maybe T.Text)
  , dhConsecutiveFailures :: !Int
  } deriving (Show)

data PollerHandle = PollerHandle
  { phHealth    :: !(MVar [DeviceHealth])
  , phSuccesses :: !(IORef Int64)
  , phFailures  :: !(IORef Int64)
  }

maxResponseBody :: Int
maxResponseBody = 1 * 1024 * 1024

checkpointIntervalPolls :: Int
checkpointIntervalPolls = 10

startPoller :: MVar DBHandle -> Config -> IO PollerHandle
startPoller dbMVar cfg = do
  let initialHealth =
        [ DeviceHealth
            { dhIp = deviceIp d
            , dhLabel = deviceLabel d
            , dhStatus = "unknown"
            , dhLastSuccess = Nothing
            , dhLastError = Nothing
            , dhLastErrorMessage = Nothing
            , dhConsecutiveFailures = 0
            }
        | d <- cfgDevices cfg
        ]
  healthMVar <- newMVar initialHealth
  succRef <- newIORef 0
  failRef <- newIORef 0
  let h = PollerHandle
        { phHealth = healthMVar
        , phSuccesses = succRef
        , phFailures = failRef
        }
  _ <- forkIO (pollerLoop dbMVar cfg h)
  hPutStrLn stderr $ "[poller] Starting — polling "
    ++ show (length (cfgDevices cfg)) ++ " devices every "
    ++ show (fromIntegral (cfgPollIntervalMs cfg) / 1000 :: Double) ++ "s"
  pure h

pollerLoop :: MVar DBHandle -> Config -> PollerHandle -> IO ()
pollerLoop dbMVar cfg h = do
  pollAll dbMVar cfg h
  go 1
  where
    intervalUs = cfgPollIntervalMs cfg * 1000
    go :: Int -> IO ()
    go pollCount = do
      threadDelay intervalUs
      pollAll dbMVar cfg h
      let newCount = pollCount + 1
      if newCount `mod` checkpointIntervalPolls == 0
        then do
          result <- try $ withMVar dbMVar $ \db -> checkpoint db
          case result of
            Left (e :: SomeException) ->
              hPutStrLn stderr $ "[poller] checkpoint error: " ++ show e
            Right () -> pure ()
        else pure ()
      go newCount

pollAll :: MVar DBHandle -> Config -> PollerHandle -> IO ()
pollAll dbMVar cfg h =
  mapM_ (fetchDevice dbMVar cfg h) (zip [0..] (cfgDevices cfg))

fetchDevice :: MVar DBHandle -> Config -> PollerHandle -> (Int, DeviceConfig) -> IO ()
fetchDevice dbMVar cfg h (idx, dev) = do
  result <- try (fetchHTTP (T.unpack (deviceIp dev)) (cfgFetchTimeoutMs cfg))
  case result of
    Left (e :: SomeException) -> do
      setError h idx dev (show e)
    Right bodyBS -> do
      case eitherDecodeStrict' bodyBS of
        Left err -> setError h idx dev ("JSON parse error: " ++ err)
        Right val -> case val of
          Object _ -> do
            insertResult <- try $ withMVar dbMVar $ \db ->
              insertReading db (deviceIp dev) val
            case insertResult of
              Left (e :: SomeException) ->
                setError h idx dev ("DB insert failed: " ++ show e)
              Right () -> do
                setSuccess h idx
                logPoll dev bodyBS
          _ -> setError h idx dev "Non-object JSON response"

setSuccess :: PollerHandle -> Int -> IO ()
setSuccess h idx = do
  now <- nowMillis
  atomicModifyIORef' (phSuccesses h) (\n -> (n + 1, ()))
  modifyMVar_ (phHealth h) $ \hs ->
    pure (updateAt idx hs $ \dh -> dh
      { dhStatus = "ok"
      , dhLastSuccess = Just now
      , dhLastErrorMessage = Nothing
      , dhConsecutiveFailures = 0
      })

setError :: PollerHandle -> Int -> DeviceConfig -> String -> IO ()
setError h idx dev msg = do
  now <- nowMillis
  hPutStrLn stderr $ "[poller] " ++ T.unpack (deviceLabel dev)
    ++ " (" ++ T.unpack (deviceIp dev) ++ "): " ++ msg
  atomicModifyIORef' (phFailures h) (\n -> (n + 1, ()))
  modifyMVar_ (phHealth h) $ \hs ->
    pure (updateAt idx hs $ \dh -> dh
      { dhStatus = "error"
      , dhLastError = Just now
      , dhLastErrorMessage = Just (T.pack msg)
      , dhConsecutiveFailures = dhConsecutiveFailures dh + 1
      })

updateAt :: Int -> [a] -> (a -> a) -> [a]
updateAt _ [] _ = []
updateAt 0 (x:xs) f = f x : xs
updateAt n (x:xs) f = x : updateAt (n - 1) xs f

getHealthJSON :: PollerHandle -> IO [Value]
getHealthJSON h = do
  hs <- withMVar (phHealth h) pure
  pure (map healthToJSON hs)

healthToJSON :: DeviceHealth -> Value
healthToJSON dh = object
  [ "ip"                  .= dhIp dh
  , "label"               .= dhLabel dh
  , "status"              .= dhStatus dh
  , "lastSuccess"         .= dhLastSuccess dh
  , "lastError"           .= dhLastError dh
  , "lastErrorMessage"    .= dhLastErrorMessage dh
  , "consecutiveFailures" .= dhConsecutiveFailures dh
  ]

getPollStats :: PollerHandle -> IO (Int64, Int64)
getPollStats h = do
  s <- readIORef (phSuccesses h)
  f <- readIORef (phFailures h)
  pure (s, f)

logPoll :: DeviceConfig -> BS.ByteString -> IO ()
logPoll dev _ =
  hPutStrLn stderr $ "[poller] " ++ T.unpack (deviceLabel dev)
    ++ " (" ++ T.unpack (deviceIp dev) ++ "): ok"

-- | Minimal HTTP client using raw sockets with timeout.
fetchHTTP :: String -> Int -> IO BS.ByteString
fetchHTTP ip timeoutMs = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just ip) (Just "80")
  case addrs of
    [] -> fail $ "Cannot resolve: " ++ ip
    (addr:_) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.NoDelay 1
      let timeoutUs = timeoutMs * 1000
      NS.setSocketOption sock NS.RecvTimeOut timeoutUs
      NS.setSocketOption sock NS.SendTimeOut timeoutUs
      result <- try $ do
        NS.connect sock (NS.addrAddress addr)
        let req = BS8.pack $ "GET /measures/current HTTP/1.0\r\nHost: "
                           ++ ip ++ "\r\nConnection: close\r\n\r\n"
        sendAllBS sock req
        recvAll sock 0 BS.empty
      NS.close sock
      case result of
        Left (e :: SomeException) -> fail (show e)
        Right body -> extractBody body

sendAllBS :: NS.Socket -> BS.ByteString -> IO ()
sendAllBS _ bs | BS.null bs = pure ()
sendAllBS sock bs = do
  sent <- NSB.send sock bs
  if sent > 0 && sent < BS.length bs
    then sendAllBS sock (BS.drop sent bs)
    else pure ()

recvAll :: NS.Socket -> Int -> BS.ByteString -> IO BS.ByteString
recvAll sock total acc
  | total > maxResponseBody = fail "Response too large"
  | otherwise = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then pure acc
        else recvAll sock (total + BS.length chunk) (acc <> chunk)

extractBody :: BS.ByteString -> IO BS.ByteString
extractBody raw =
  let marker = BS8.pack "\r\n\r\n"
  in case BS8.breakSubstring marker raw of
       (_, after) | BS.null after -> fail "No HTTP body found"
       (hdr, after) ->
         let statusLine = BS8.takeWhile (/= '\r') hdr
             parts = BS8.words statusLine
         in case parts of
              (_:code:_) ->
                let codeStr = BS8.unpack code
                in case codeStr of
                     "200" -> pure (BS.drop 4 after)
                     _     -> fail $ "HTTP " ++ codeStr
              _ -> fail "Malformed HTTP response"
