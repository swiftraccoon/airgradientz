{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , DeviceConfig(..)
  , loadConfig
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, eitherDecodeStrict')
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
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
  , cfgDownsampleThreshold :: !Int
  } deriving (Show)

data ConfigFile = ConfigFile
  { cfPorts                :: !(Maybe (KM.KeyMap Int))
  , cfDevices              :: !(Maybe [DeviceConfig])
  , cfPollIntervalMs       :: !(Maybe Int)
  , cfFetchTimeoutMs       :: !(Maybe Int)
  , cfMaxApiRows           :: !(Maybe Int)
  , cfDownsampleThreshold  :: !(Maybe Int)
  , cfDefaults             :: !(Maybe ConfigFile)
  }

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \o ->
    ConfigFile
      <$> o .:? "ports"
      <*> o .:? "devices"
      <*> o .:? "pollIntervalMs"
      <*> o .:? "fetchTimeoutMs"
      <*> o .:? "maxApiRows"
      <*> o .:? "downsampleThreshold"
      <*> o .:? "defaults"

defaultPort :: Word16
defaultPort = 3019

defaultPollIntervalMs, defaultFetchTimeoutMs, defaultMaxApiRows, defaultDownsampleThreshold :: Int
defaultPollIntervalMs = 15000
defaultFetchTimeoutMs = 5000
defaultMaxApiRows = 10000
defaultDownsampleThreshold = 10000

maxConfigFileSize :: Int
maxConfigFileSize = 1048576

loadConfig :: IO Config
loadConfig = do
  let baseCfg = Config
        { cfgPort                = defaultPort
        , cfgDBPath              = "./airgradientz.db"
        , cfgDevices             = defaultDevices
        , cfgPollIntervalMs      = defaultPollIntervalMs
        , cfgFetchTimeoutMs      = defaultFetchTimeoutMs
        , cfgMaxApiRows          = defaultMaxApiRows
        , cfgDownsampleThreshold = defaultDownsampleThreshold
        }
  cfg <- loadFromFile baseCfg
  applyEnvOverrides cfg
  where
    defaultDevices =
      [ DeviceConfig "192.168.88.6" "outdoor"
      , DeviceConfig "192.168.88.159" "indoor"
      ]

loadFromFile :: Config -> IO Config
loadFromFile cfg = do
  mContent <- findConfigFile
  case mContent of
    Nothing -> do
      hPutStrLn stderr "[config] No config file found, using defaults"
      pure cfg
    Just (content, path) -> do
      hPutStrLn stderr $ "[config] Loaded config from " ++ path
      case eitherDecodeStrict' content of
        Left err -> do
          hPutStrLn stderr $ "[config] JSON parse error: " ++ err
          pure cfg
        Right cf -> pure (applyConfigFile cfg cf)

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

applyConfigFile :: Config -> ConfigFile -> Config
applyConfigFile cfg cf =
  let withDefaults = case cfDefaults cf of
        Just defs -> applyValues cfg defs
        Nothing   -> cfg
      withTopLevel = applyValues withDefaults cf
  in applyPort withTopLevel cf

applyValues :: Config -> ConfigFile -> Config
applyValues cfg cf = cfg
  { cfgDevices             = fromMaybe (cfgDevices cfg) (cfDevices cf)
  , cfgPollIntervalMs      = applyPositive (cfgPollIntervalMs cfg) (cfPollIntervalMs cf)
  , cfgFetchTimeoutMs      = applyPositive (cfgFetchTimeoutMs cfg) (cfFetchTimeoutMs cf)
  , cfgMaxApiRows          = applyPositive (cfgMaxApiRows cfg) (cfMaxApiRows cf)
  , cfgDownsampleThreshold = applyPositive (cfgDownsampleThreshold cfg) (cfDownsampleThreshold cf)
  }

applyPositive :: Int -> Maybe Int -> Int
applyPositive current = \case
  Just v | v > 0 -> v
  _              -> current

applyPort :: Config -> ConfigFile -> Config
applyPort cfg cf = case cfPorts cf >>= KM.lookup (Key.fromString "haskell") of
  Just p | p > 0 && p <= 65535 -> cfg { cfgPort = fromIntegral p }
  _ -> cfg

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
  hPutStrLn stderr $ "[config] port=" ++ show (cfgPort cfg)
    ++ " devices=[" ++ T.unpack devList ++ "]"
    ++ " poll=" ++ show (cfgPollIntervalMs cfg) ++ "ms"
