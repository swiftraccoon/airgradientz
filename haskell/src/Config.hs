{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , DeviceConfig(..)
  , loadConfig
  ) where

import Data.Int (Int64)
import Data.List (intercalate)
import Data.Word (Word16)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import Log (logMsg)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Json (Value(..), JsonNumber(..), decodeStrict, objectLookup)

data DeviceConfig = DeviceConfig
  { deviceIp    :: !T.Text
  , deviceLabel :: !T.Text
  } deriving (Show, Eq)

parseDeviceConfig :: Value -> Maybe DeviceConfig
parseDeviceConfig (Object o) = do
  ip <- case objectLookup "ip" o of
          Just (String s) -> Just s
          _               -> Nothing
  label <- case objectLookup "label" o of
             Just (String s) -> Just s
             _               -> Nothing
  Just DeviceConfig { deviceIp = ip, deviceLabel = label }
parseDeviceConfig _ = Nothing

data Config = Config
  { cfgPort                :: !Word16
  , cfgDBPath              :: !FilePath
  , cfgDevices             :: ![DeviceConfig]
  , cfgPollIntervalMs      :: !Int
  , cfgFetchTimeoutMs      :: !Int
  , cfgMaxApiRows          :: !Int
  , cfgDownsampleBuckets   :: !(Map.Map String Int64)
  } deriving (Show)

data ConfigFile = ConfigFile
  { cfPorts                :: !(Maybe Value)
  , cfDevices              :: !(Maybe [DeviceConfig])
  , cfPollIntervalMs       :: !(Maybe Int)
  , cfFetchTimeoutMs       :: !(Maybe Int)
  , cfMaxApiRows           :: !(Maybe Int)
  , cfDownsampleBuckets    :: !(Maybe (Map.Map String Int))
  }

parseConfigFile :: Value -> Either String ConfigFile
parseConfigFile (Object o) = Right ConfigFile
  { cfPorts             = objectLookup "ports" o
  , cfDevices           = parseDeviceList =<< objectLookup "devices" o
  , cfPollIntervalMs    = jsonToInt =<< objectLookup "pollIntervalMs" o
  , cfFetchTimeoutMs    = jsonToInt =<< objectLookup "fetchTimeoutMs" o
  , cfMaxApiRows        = jsonToInt =<< objectLookup "maxApiRows" o
  , cfDownsampleBuckets = parseStringIntMap =<< objectLookup "downsampleBuckets" o
  }
parseConfigFile _ = Left "config file is not a JSON object"

parseDeviceList :: Value -> Maybe [DeviceConfig]
parseDeviceList (Array xs) = mapM parseDeviceConfig xs
parseDeviceList _ = Nothing

jsonToInt :: Value -> Maybe Int
jsonToInt (Number (JInt n)) = Just (fromIntegral n)
jsonToInt (Number (JDbl d)) = Just (round d)
jsonToInt _ = Nothing

parseStringIntMap :: Value -> Maybe (Map.Map String Int)
parseStringIntMap (Object kvs) =
  let pairs = [(T.unpack k, v) | (k, v) <- kvs]
  in fmap Map.fromList $ mapM (\(k, v) -> case jsonToInt v of
       Just n  -> Just (k, n)
       Nothing -> Nothing) pairs
parseStringIntMap _ = Nothing

maxConfigFileSize :: Int
maxConfigFileSize = 1048576

loadConfig :: IO Config
loadConfig = do
  cf <- loadFromFile
  let cfg = applyConfigFile cf
  validateConfig cfg
  applyEnvOverrides cfg

loadFromFile :: IO ConfigFile
loadFromFile = do
  mContent <- findConfigFile
  case mContent of
    Nothing ->
      die "fatal: config file not found"
    Just (content, path) -> do
      logMsg $ "[config] Loaded config from " ++ path
      case decodeStrict content of
        Left err ->
          die $ "fatal: config file parse error: " ++ err
        Right val -> case parseConfigFile val of
          Left err ->
            die $ "fatal: config file parse error: " ++ err
          Right cf -> pure cf

findConfigFile :: IO (Maybe (BS.ByteString, FilePath))
findConfigFile = do
  envPath <- lookupEnv "CONFIG_PATH"
  let candidates = maybe id (:) envPath
        ["." </> "airgradientz.json", ".." </> "airgradientz.json"]
  go candidates
  where
    go [] = pure Nothing
    go (p:ps) = do
      exists <- doesFileExist p
      if exists
        then do
          content <- BS.readFile p
          if BS.length content > maxConfigFileSize
            then go ps
            else pure (Just (content, p))
        else go ps

applyConfigFile :: ConfigFile -> Config
applyConfigFile cf =
  let port = case cfPorts cf of
               Just (Object portsObj) ->
                 case objectLookup "haskell" portsObj of
                   Just (Number (JInt p)) | p > 0 && p <= 65535 -> fromIntegral p
                   Just (Number (JDbl d)) | d > 0 && d <= 65535 -> round d
                   _ -> 0
               _ -> 0
  in Config
    { cfgPort                = port
    , cfgDBPath              = "./airgradientz.db"
    , cfgDevices             = maybe [] id (cfDevices cf)
    , cfgPollIntervalMs      = maybe 0 id (cfPollIntervalMs cf)
    , cfgFetchTimeoutMs      = maybe 0 id (cfFetchTimeoutMs cf)
    , cfgMaxApiRows          = maybe 0 id (cfMaxApiRows cf)
    , cfgDownsampleBuckets   = case cfDownsampleBuckets cf of
        Just m | not (Map.null m) -> Map.map fromIntegral m
        _ -> Map.empty
    }

validateConfig :: Config -> IO ()
validateConfig cfg = do
  let missing = concat
        [ ["pollIntervalMs"      | cfgPollIntervalMs cfg <= 0]
        , ["fetchTimeoutMs"      | cfgFetchTimeoutMs cfg <= 0]
        , ["maxApiRows"          | cfgMaxApiRows cfg <= 0]
        , ["downsampleBuckets" | Map.null (cfgDownsampleBuckets cfg)]
        , ["devices"             | null (cfgDevices cfg)]
        , ["ports.haskell"       | cfgPort cfg == 0]
        ]
  if null missing
    then pure ()
    else die $ "fatal: missing required config keys: " ++ intercalate ", " missing

applyEnvOverrides :: Config -> IO Config
applyEnvOverrides cfg = do
  mPort <- lookupEnv "PORT"
  mDB   <- lookupEnv "DB_PATH"
  let cfg1 = case mPort >>= readMaybe @Int of
               Just p | p > 0 && p <= 65535 -> cfg { cfgPort = fromIntegral p }
               _ -> cfg
      cfg2 = case mDB of
               Just db | not (null db) -> cfg1 { cfgDBPath = db }
               _ -> cfg1
  logConfig cfg2
  pure cfg2

logConfig :: Config -> IO ()
logConfig cfg = do
  let devList = T.intercalate ", "
        [ deviceLabel d <> "(" <> deviceIp d <> ")"
        | d <- cfgDevices cfg
        ]
  logMsg $ "[config] port=" ++ show (cfgPort cfg)
    ++ " devices=[" ++ T.unpack devList ++ "]"
    ++ " poll=" ++ show (cfgPollIntervalMs cfg) ++ "ms"
