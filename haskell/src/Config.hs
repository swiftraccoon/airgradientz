{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , DeviceConfig(..)
  , loadConfig
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, eitherDecodeStrict')
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Word (Word16)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import Log (logMsg)
import Text.Read (readMaybe)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data DeviceConfig = DeviceConfig
  { deviceIp    :: !T.Text
  , deviceLabel :: !T.Text
  } deriving (Show, Eq)

instance FromJSON DeviceConfig where
  parseJSON = withObject "DeviceConfig" $ \o ->
    DeviceConfig <$> o .: "ip" <*> o .: "label"

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
  { cfPorts                :: !(Maybe (KM.KeyMap Int))
  , cfDevices              :: !(Maybe [DeviceConfig])
  , cfPollIntervalMs       :: !(Maybe Int)
  , cfFetchTimeoutMs       :: !(Maybe Int)
  , cfMaxApiRows           :: !(Maybe Int)
  , cfDownsampleBuckets    :: !(Maybe (Map.Map String Int))
  }

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \o ->
    ConfigFile
      <$> o .:? "ports"
      <*> o .:? "devices"
      <*> o .:? "pollIntervalMs"
      <*> o .:? "fetchTimeoutMs"
      <*> o .:? "maxApiRows"
      <*> o .:? "downsampleBuckets"

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
      case eitherDecodeStrict' content of
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
  let port = case cfPorts cf >>= KM.lookup (Key.fromString "haskell") of
               Just p | p > 0 && p <= 65535 -> fromIntegral p
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
