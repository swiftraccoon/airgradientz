{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( runServer
  , ServerStats(..)
  , newServerStats
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (when, void)
import Data.Char (toLower, ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, canonicalizePath)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (FileStatus, getFileStatus, fileSize)
import System.Posix.Process (getProcessID)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Config (Config(..), DeviceConfig(..))
import DB (DBHandle, ReadingQuery(..)
          , queryReadings, getLatestReadings, getDevices
          , getReadingsCount, getFilteredCount, nowMillis
          , readingToJSON, deviceSummaryToJSON)
import Poller (PollerHandle, getHealthJSON, getPollStats)

data ServerStats = ServerStats
  { ssRequestsServed    :: !(IORef Int64)
  , ssActiveConnections :: !(IORef Int64)
  , ssStartedAt         :: !Int64
  }

newServerStats :: IO ServerStats
newServerStats = do
  now <- nowMillis
  reqs <- newIORef 0
  conns <- newIORef 0
  pure ServerStats
    { ssRequestsServed = reqs
    , ssActiveConnections = conns
    , ssStartedAt = now
    }

maxRequestSize :: Int
maxRequestSize = 8192

maxStaticFileSize :: Int
maxStaticFileSize = 16 * 1024 * 1024

maxConnections :: Int64
maxConnections = 256

runServer :: MVar DBHandle -> Config -> PollerHandle -> ServerStats -> IO ()
runServer dbMVar cfg pollerH stats = do
  let port = show (cfgPort cfg)
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addr:_ <- NS.getAddrInfo (Just hints) Nothing (Just port)
  bracket (openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 128
    hPutStrLn stderr $ "[server] Listening on http://localhost:" ++ port
    acceptLoop sock dbMVar cfg pollerH stats
  where
    openSocket addr = NS.openSocket addr

acceptLoop :: NS.Socket -> MVar DBHandle -> Config -> PollerHandle -> ServerStats -> IO ()
acceptLoop sock dbMVar cfg pollerH stats = do
  (conn, _peer) <- NS.accept sock
  active <- readIORef (ssActiveConnections stats)
  if active >= maxConnections
    then do
      sendRaw conn "HTTP/1.1 503 Service Unavailable\r\nContent-Type: application/json\r\nConnection: close\r\n\r\n{\"error\":\"Too many connections\"}"
      NS.close conn
    else do
      atomicModifyIORef' (ssActiveConnections stats) (\n -> (n + 1, ()))
      void $ forkFinally
        (handleConnection conn dbMVar cfg pollerH stats)
        (\_ -> do
          atomicModifyIORef' (ssActiveConnections stats) (\n -> (n - 1, ()))
          NS.close conn)
  acceptLoop sock dbMVar cfg pollerH stats

handleConnection :: NS.Socket -> MVar DBHandle -> Config -> PollerHandle -> ServerStats -> IO ()
handleConnection conn dbMVar cfg pollerH stats = do
  NS.setSocketOption conn NS.NoDelay 1
  rawReq <- recvRequest conn
  case rawReq of
    Nothing -> sendErrorResponse conn 400 "Bad request"
    Just req -> do
      atomicModifyIORef' (ssRequestsServed stats) (\n -> (n + 1, ()))
      handleRequest conn dbMVar cfg pollerH stats req

data HTTPRequest = HTTPRequest
  { reqMethod :: !BS.ByteString
  , reqPath   :: !BS.ByteString
  , reqQuery  :: !BS.ByteString
  }

recvRequest :: NS.Socket -> IO (Maybe HTTPRequest)
recvRequest conn = do
  raw <- recvUntilHeaders conn 0 BS.empty
  case raw of
    Nothing -> pure Nothing
    Just hdrBytes -> parseRequestLine hdrBytes

recvUntilHeaders :: NS.Socket -> Int -> BS.ByteString -> IO (Maybe BS.ByteString)
recvUntilHeaders conn total acc
  | total >= maxRequestSize = pure Nothing
  | BS.isInfixOf (BS8.pack "\r\n\r\n") acc = pure (Just acc)
  | otherwise = do
      chunk <- NSB.recv conn 4096
      if BS.null chunk
        then pure Nothing
        else recvUntilHeaders conn (total + BS.length chunk) (acc <> chunk)

parseRequestLine :: BS.ByteString -> IO (Maybe HTTPRequest)
parseRequestLine raw = do
  let firstLine = BS8.takeWhile (/= '\r') raw
      parts = BS8.words firstLine
  case parts of
    (method:pathQuery:_) ->
      let (path, qPart) = BS8.break (== '?') pathQuery
          query = if BS.null qPart then BS.empty else BS.drop 1 qPart
      in pure $ Just HTTPRequest
           { reqMethod = method
           , reqPath   = path
           , reqQuery  = query
           }
    _ -> pure Nothing

handleRequest :: NS.Socket -> MVar DBHandle -> Config -> PollerHandle -> ServerStats -> HTTPRequest -> IO ()
handleRequest conn dbMVar cfg pollerH stats req
  | reqMethod req /= BS8.pack "GET" =
      sendErrorResponse conn 405 "Method not allowed"
  | otherwise = do
      let path = BS8.unpack (reqPath req)
          qs   = reqQuery req
      case path of
        "/api/readings/latest" -> handleLatest conn dbMVar
        "/api/readings/count"  -> handleReadingsCount conn dbMVar cfg qs
        "/api/readings"        -> handleReadings conn dbMVar cfg qs
        "/api/devices"         -> handleDevices conn dbMVar
        "/api/health"          -> handleHealth conn pollerH
        "/api/config"          -> handleConfig conn cfg
        "/api/stats"           -> handleStats conn dbMVar cfg pollerH stats
        _                      -> serveStatic conn path

handleReadings :: NS.Socket -> MVar DBHandle -> Config -> BS.ByteString -> IO ()
handleReadings conn dbMVar cfg qs = do
  now <- nowMillis
  let params = parseQueryString qs
      defaultFrom = now - 24 * 60 * 60 * 1000
      from'  = lookupInt64Param "from" defaultFrom params
      to'    = lookupInt64Param "to" now params
      device = lookupParam "device" params
      rawLim = lookupIntParam "limit" (cfgMaxApiRows cfg) params
      effectiveLimit = min rawLim (cfgMaxApiRows cfg)
      dsParam = lookupParam "downsample" params
  -- Validate downsample param if present
  case dsParam of
    "" -> doQuery conn dbMVar cfg from' to' device effectiveLimit 0
    s  -> case downsampleBucket s of
            Nothing -> sendErrorResponse conn 400 "Invalid downsample value"
            Just bucket -> doQuery conn dbMVar cfg from' to' device effectiveLimit bucket

doQuery :: NS.Socket -> MVar DBHandle -> Config -> Int64 -> Int64 -> String -> Int -> Int64 -> IO ()
doQuery conn dbMVar cfg from' to' device effectiveLimit bucket = do
  let q = ReadingQuery
            { rqDevice       = T.pack (if null device then "all" else device)
            , rqFrom         = from'
            , rqTo           = to'
            , rqLimit        = if effectiveLimit > 0 then effectiveLimit else cfgMaxApiRows cfg
            , rqDownsampleMs = bucket
            }
  result <- try $ withMVar dbMVar $ \h -> queryReadings h q
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "[api] query_readings error: " ++ show e
      sendErrorResponse conn 500 "Internal server error"
    Right readings ->
      sendJSON conn (Aeson.encode (map readingToJSON readings))

handleLatest :: NS.Socket -> MVar DBHandle -> IO ()
handleLatest conn dbMVar = do
  result <- try $ withMVar dbMVar $ \h -> getLatestReadings h
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "[api] get_latest_readings error: " ++ show e
      sendErrorResponse conn 500 "Internal server error"
    Right readings ->
      sendJSON conn (Aeson.encode (map readingToJSON readings))

handleReadingsCount :: NS.Socket -> MVar DBHandle -> Config -> BS.ByteString -> IO ()
handleReadingsCount conn dbMVar cfg qs = do
  now <- nowMillis
  let params = parseQueryString qs
      defaultFrom = now - 24 * 60 * 60 * 1000
      from'  = lookupInt64Param "from" defaultFrom params
      to'    = lookupInt64Param "to" now params
      device = lookupParam "device" params
      devText = T.pack (if null device then "all" else device)
  result <- try $ withMVar dbMVar $ \h -> getFilteredCount h from' to' devText
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "[api] readings_count error: " ++ show e
      sendErrorResponse conn 500 "Internal server error"
    Right count -> do
      let body = Aeson.object
            [ "count"              Aeson..= count
            , "from"               Aeson..= from'
            , "to"                 Aeson..= to'
            , "device"             Aeson..= devText
            , "downsampleThreshold" Aeson..= cfgDownsampleThreshold cfg
            ]
      sendJSON conn (Aeson.encode body)

handleDevices :: NS.Socket -> MVar DBHandle -> IO ()
handleDevices conn dbMVar = do
  result <- try $ withMVar dbMVar $ \h -> getDevices h
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "[api] get_devices error: " ++ show e
      sendErrorResponse conn 500 "Internal server error"
    Right devs ->
      sendJSON conn (Aeson.encode (map deviceSummaryToJSON devs))

handleHealth :: NS.Socket -> PollerHandle -> IO ()
handleHealth conn pollerH = do
  healthJSON <- getHealthJSON pollerH
  sendJSON conn (Aeson.encode healthJSON)

handleConfig :: NS.Socket -> Config -> IO ()
handleConfig conn cfg = do
  let devs = [ Aeson.object
                 [ "ip"    Aeson..= deviceIp d
                 , "label" Aeson..= deviceLabel d
                 ]
             | d <- cfgDevices cfg
             ]
      body = Aeson.object
        [ "pollIntervalMs"      Aeson..= cfgPollIntervalMs cfg
        , "downsampleThreshold" Aeson..= cfgDownsampleThreshold cfg
        , "devices"             Aeson..= devs
        ]
  sendJSON conn (Aeson.encode body)

handleStats :: NS.Socket -> MVar DBHandle -> Config -> PollerHandle -> ServerStats -> IO ()
handleStats conn dbMVar cfg pollerH stats = do
  now <- nowMillis
  let uptimeMs = now - ssStartedAt stats
  readingsCount <- (try $ withMVar dbMVar $ \h -> getReadingsCount h) >>= \case
    Left (_ :: SomeException) -> pure 0
    Right c -> pure c
  dbSize <- getFileSize (cfgDBPath cfg)
  rss <- readRSSBytes
  pid <- fromIntegral <$> getProcessID :: IO Int64
  reqServed <- readIORef (ssRequestsServed stats)
  activeConns <- readIORef (ssActiveConnections stats)
  (successes, failures) <- getPollStats pollerH
  let body = Aeson.object
        [ "implementation"     Aeson..= ("haskell" :: T.Text)
        , "pid"                Aeson..= pid
        , "uptime_ms"          Aeson..= uptimeMs
        , "memory_rss_bytes"   Aeson..= rss
        , "db_size_bytes"      Aeson..= dbSize
        , "readings_count"     Aeson..= readingsCount
        , "requests_served"    Aeson..= reqServed
        , "active_connections" Aeson..= activeConns
        , "poll_successes"     Aeson..= successes
        , "poll_failures"      Aeson..= failures
        , "pool_alloc_count"   Aeson..= (0 :: Int64)
        , "pool_bytes_used"    Aeson..= (0 :: Int64)
        , "started_at"         Aeson..= ssStartedAt stats
        ]
  sendJSON conn (Aeson.encode body)

serveStatic :: NS.Socket -> String -> IO ()
serveStatic conn reqPath = do
  if not (isValidStaticPath reqPath)
    then sendErrorResponse conn 403 "Forbidden"
    else do
      let relative = dropWhile (== '/') reqPath
          filePath = if null relative then "public/index.html" else "public/" ++ relative
      resolved <- try (canonicalizePath filePath) :: IO (Either SomeException FilePath)
      case resolved of
        Left _ -> sendErrorResponse conn 404 "Not found"
        Right absPath -> do
          publicAbs <- try (canonicalizePath "public") :: IO (Either SomeException FilePath)
          case publicAbs of
            Left _ -> sendErrorResponse conn 404 "Not found"
            Right pubDir
              | not (pubDir `isPrefixOf` absPath) ->
                  sendErrorResponse conn 403 "Forbidden"
              | otherwise -> do
                  exists <- doesFileExist absPath
                  if not exists
                    then sendErrorResponse conn 404 "Not found"
                    else do
                      content <- BS.readFile absPath
                      if BS.length content > maxStaticFileSize
                        then sendErrorResponse conn 413 "File too large"
                        else do
                          let ct = contentTypeFor absPath
                              hdr = BS8.pack $ "HTTP/1.1 200 OK\r\n"
                                ++ "Content-Type: " ++ ct ++ "\r\n"
                                ++ "Content-Length: " ++ show (BS.length content) ++ "\r\n"
                                ++ securityHeaders
                                ++ "Cache-Control: public, max-age=600\r\n"
                                ++ "Connection: close\r\n\r\n"
                          sendAll conn (hdr <> content)

isValidStaticPath :: String -> Bool
isValidStaticPath path =
  not (".." `isInfixOfStr` path) &&
  all (\c -> ord c >= 0x20 && ord c /= 0x7F) path
  where
    isInfixOfStr needle haystack = any (isPrefixOf needle) (tails' haystack)
    tails' [] = [[]]
    tails' xs@(_:rest) = xs : tails' rest

securityHeaders :: String
securityHeaders =
  "X-Content-Type-Options: nosniff\r\n\
  \X-Frame-Options: DENY\r\n"

sendJSON :: NS.Socket -> LBS.ByteString -> IO ()
sendJSON conn body = do
  let bodyBS = LBS.toStrict body
      hdr = BS8.pack $ "HTTP/1.1 200 OK\r\n"
        ++ "Content-Type: application/json\r\n"
        ++ "Content-Length: " ++ show (BS.length bodyBS) ++ "\r\n"
        ++ securityHeaders
        ++ "Connection: close\r\n\r\n"
  sendAll conn (hdr <> bodyBS)

sendErrorResponse :: NS.Socket -> Int -> String -> IO ()
sendErrorResponse conn status msg = do
  let bodyStr = "{\"error\":\"" ++ escapeJSON msg ++ "\"}"
      bodyBS = BS8.pack bodyStr
      statusText = case status of
        400 -> "Bad Request"
        403 -> "Forbidden"
        404 -> "Not Found"
        405 -> "Method Not Allowed"
        413 -> "Request Entity Too Large"
        500 -> "Internal Server Error"
        503 -> "Service Unavailable"
        _   -> "Error"
      hdr = BS8.pack $ "HTTP/1.1 " ++ show status ++ " " ++ statusText ++ "\r\n"
        ++ "Content-Type: application/json\r\n"
        ++ "Content-Length: " ++ show (BS.length bodyBS) ++ "\r\n"
        ++ securityHeaders
        ++ "Connection: close\r\n\r\n"
  sendAll conn (hdr <> bodyBS)

escapeJSON :: String -> String
escapeJSON = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c
      | ord c < 0x20 = "\\u" ++ padHex (ord c)
      | otherwise = [c]
    padHex n = let h = showHex' n in replicate (4 - length h) '0' ++ h
    showHex' 0 = "0"
    showHex' n = reverse (go n)
    go 0 = []
    go n = let (q, r) = n `divMod` 16
               c = if r < 10 then toEnum (ord '0' + r) else toEnum (ord 'a' + r - 10)
           in c : go q

sendRaw :: NS.Socket -> String -> IO ()
sendRaw conn s = sendAll conn (BS8.pack s)

sendAll :: NS.Socket -> BS.ByteString -> IO ()
sendAll _ bs | BS.null bs = pure ()
sendAll conn bs = do
  sent <- NSB.send conn bs
  when (sent > 0 && sent < BS.length bs) $
    sendAll conn (BS.drop sent bs)

contentTypeFor :: String -> String
contentTypeFor path =
  case map toLower (takeExtension path) of
    ".html" -> "text/html; charset=utf-8"
    ".css"  -> "text/css; charset=utf-8"
    ".js"   -> "application/javascript; charset=utf-8"
    ".json" -> "application/json; charset=utf-8"
    ".png"  -> "image/png"
    ".jpg"  -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".svg"  -> "image/svg+xml"
    ".ico"  -> "image/x-icon"
    _       -> "application/octet-stream"

takeExtension :: String -> String
takeExtension path =
  let name = reverse (takeWhile (/= '/') (reverse path))
  in case dropWhile (/= '.') (reverse name) of
       [] -> ""
       ext -> reverse ext

-- Downsample bucket lookup
downsampleBucket :: String -> Maybe Int64
downsampleBucket "5m"  = Just 300000
downsampleBucket "10m" = Just 600000
downsampleBucket "15m" = Just 900000
downsampleBucket "30m" = Just 1800000
downsampleBucket "1h"  = Just 3600000
downsampleBucket "1d"  = Just 86400000
downsampleBucket "1w"  = Just 604800000
downsampleBucket _     = Nothing

-- Query string parsing

parseQueryString :: BS.ByteString -> [(String, String)]
parseQueryString bs
  | BS.null bs = []
  | otherwise = map parsePair (BS8.split '&' bs)
  where
    parsePair p =
      let (k, v) = BS8.break (== '=') p
      in (BS8.unpack k, BS8.unpack (if BS.null v then v else BS.drop 1 v))

lookupParam :: String -> [(String, String)] -> String
lookupParam key params = fromMaybe "" (lookup key params)

lookupInt64Param :: String -> Int64 -> [(String, String)] -> Int64
lookupInt64Param key def params = case lookup key params of
  Just s  -> fromMaybe def (readMaybe s)
  Nothing -> def

lookupIntParam :: String -> Int -> [(String, String)] -> Int
lookupIntParam key def params = case lookup key params of
  Just s  -> fromMaybe def (readMaybe s)
  Nothing -> def

-- System introspection

getFileSize :: FilePath -> IO Int64
getFileSize path = do
  result <- try (getFileStatus path) :: IO (Either SomeException FileStatus)
  case result of
    Left _ -> pure 0
    Right st -> pure (fromIntegral (fileSize st))

readRSSBytes :: IO Int64
readRSSBytes = do
  result <- try (BS.readFile "/proc/self/statm") :: IO (Either SomeException BS.ByteString)
  case result of
    Left _ -> pure 0
    Right content ->
      let fields = BS8.words content
      in case fields of
           (_:rssPages:_) -> case readMaybe (BS8.unpack rssPages) :: Maybe Int64 of
             Just pages -> pure (pages * 4096)
             Nothing -> pure 0
           _ -> pure 0
