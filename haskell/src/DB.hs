{-# LANGUAGE OverloadedStrings #-}

module DB
  ( withDB
  , insertReading
  , queryReadings
  , getLatestReadings
  , getDevices
  , getReadingsCount
  , getFilteredCount
  , checkpoint
  , nowMillis
  , Reading(..)
  , DeviceSummary(..)
  , ReadingQuery(..)
  , readingToJSON
  , deviceSummaryToJSON
  , DBHandle
  ) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesFileExist)
import Log (logMsg)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.SQLite3 as SQL

import Json (Value(..), JsonNumber(..), JsonObject, encode, object, (.=),
             objectLookup, objectEmpty)

-- | Opaque database handle wrapping connection + loaded queries.
data DBHandle = DBHandle
  { dbConn       :: !SQL.Database
  , dbInsertSQL  :: !T.Text
  , dbLatestSQL  :: !T.Text
  , dbDevicesSQL :: !T.Text
  , dbCountSQL   :: !T.Text
  , dbQueryCols  :: !T.Text
  }

data Reading = Reading
  { rdId              :: !Int64
  , rdTimestamp       :: !Int64
  , rdDeviceId        :: !T.Text
  , rdDeviceType      :: !T.Text
  , rdDeviceIp        :: !T.Text
  , rdPm01            :: !(Maybe Double)
  , rdPm02            :: !(Maybe Double)
  , rdPm10            :: !(Maybe Double)
  , rdPm02Compensated :: !(Maybe Double)
  , rdRco2            :: !(Maybe Int64)
  , rdAtmp            :: !(Maybe Double)
  , rdAtmpCompensated :: !(Maybe Double)
  , rdRhum            :: !(Maybe Double)
  , rdRhumCompensated :: !(Maybe Double)
  , rdTvocIndex       :: !(Maybe Double)
  , rdNoxIndex        :: !(Maybe Double)
  , rdWifi            :: !(Maybe Int64)
  } deriving (Show, Eq)

data DeviceSummary = DeviceSummary
  { dsDeviceId     :: !T.Text
  , dsDeviceType   :: !T.Text
  , dsDeviceIp     :: !T.Text
  , dsLastSeen     :: !Int64
  , dsReadingCount :: !Int64
  } deriving (Show, Eq)

data ReadingQuery = ReadingQuery
  { rqDevice       :: !T.Text
  , rqFrom         :: !Int64
  , rqTo           :: !Int64
  , rqLimit        :: !Int
  , rqDownsampleMs :: !Int64
  } deriving (Show)

-- | Default query SQL (fallback if queries.sql not found).
defaultInsertSQL :: T.Text
defaultInsertSQL = T.unwords
  [ "INSERT INTO readings ("
  , "  timestamp, device_id, device_type, device_ip,"
  , "  pm01, pm02, pm10, pm02_compensated,"
  , "  rco2, atmp, atmp_compensated, rhum, rhum_compensated,"
  , "  tvoc_index, nox_index, wifi, raw_json"
  , ") VALUES ("
  , "  :timestamp, :device_id, :device_type, :device_ip,"
  , "  :pm01, :pm02, :pm10, :pm02_compensated,"
  , "  :rco2, :atmp, :atmp_compensated, :rhum, :rhum_compensated,"
  , "  :tvoc_index, :nox_index, :wifi, :raw_json"
  , ")"
  ]

defaultQueryCols :: T.Text
defaultQueryCols =
  "id, timestamp, device_id, device_type, device_ip, \
  \pm01, pm02, pm10, pm02_compensated, rco2, \
  \atmp, atmp_compensated, rhum, rhum_compensated, \
  \tvoc_index, nox_index, wifi"

defaultLatestSQL :: T.Text
defaultLatestSQL =
  "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip, \
  \r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2, \
  \r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated, \
  \r.tvoc_index, r.nox_index, r.wifi \
  \FROM readings r \
  \INNER JOIN ( \
  \  SELECT device_id, MAX(id) AS max_id \
  \  FROM readings GROUP BY device_id \
  \) latest ON r.id = latest.max_id"

defaultDevicesSQL :: T.Text
defaultDevicesSQL =
  "SELECT device_id, device_type, device_ip, \
  \MAX(timestamp) AS last_seen, COUNT(*) AS reading_count \
  \FROM readings GROUP BY device_id ORDER BY device_type"

defaultCountSQL :: T.Text
defaultCountSQL = "SELECT COUNT(*) FROM readings"

-- | Open database, apply schema, load queries. Use with bracket pattern.
withDB :: FilePath -> (DBHandle -> IO a) -> IO a
withDB dbPath action = do
  bracket (openDB dbPath) closeDB action

openDB :: FilePath -> IO DBHandle
openDB dbPath = do
  db <- SQL.open (T.pack dbPath)
  SQL.exec db "PRAGMA journal_mode=WAL"
  SQL.exec db "PRAGMA busy_timeout=5000"
  SQL.exec db "PRAGMA foreign_keys=ON"

  -- Apply schema
  schemaContent <- readFileText "../schema.sql"
  SQL.exec db schemaContent

  -- Load queries
  queries <- loadQueries
  let getQ name def = fromMaybe def (lookup name queries)

  pure DBHandle
    { dbConn       = db
    , dbInsertSQL  = getQ "insert_reading" defaultInsertSQL
    , dbLatestSQL  = getQ "select_latest" defaultLatestSQL
    , dbDevicesSQL = getQ "select_devices" defaultDevicesSQL
    , dbCountSQL   = getQ "count_readings" defaultCountSQL
    , dbQueryCols  = getQ "reading_columns" defaultQueryCols
    }

closeDB :: DBHandle -> IO ()
closeDB h = SQL.close (dbConn h)

readFileText :: FilePath -> IO T.Text
readFileText path = TE.decodeUtf8 <$> BS.readFile path

-- | Parse queries.sql into named query pairs.
loadQueries :: IO [(String, T.Text)]
loadQueries = do
  exists <- doesFileExist "../queries.sql"
  if not exists
    then do
      logMsg "[db] queries.sql not found, using defaults"
      pure []
    else do
      content <- readFileText "../queries.sql"
      let qs = parseQueriesSQL content
      logMsg $ "[db] loaded " ++ show (length qs) ++ " queries from queries.sql"
      pure qs

parseQueriesSQL :: T.Text -> [(String, T.Text)]
parseQueriesSQL content = go Nothing [] [] (T.lines content)
  where
    go :: Maybe String -> [T.Text] -> [(String, T.Text)] -> [T.Text] -> [(String, T.Text)]
    go mName lineAcc acc [] = flush mName lineAcc acc
    go mName lineAcc acc (line:rest)
      | Just name <- T.stripPrefix "-- name: " (T.strip line) =
          let acc' = flush mName lineAcc acc
          in go (Just (T.unpack (T.strip name))) [] acc' rest
      | T.isPrefixOf "--" (T.strip line) = go mName lineAcc acc rest
      | T.null (T.strip line) = go mName lineAcc acc rest
      | otherwise = go mName (lineAcc ++ [T.strip line]) acc rest

    flush Nothing _ acc = acc
    flush (Just name) lineAcc acc =
      let joined = T.strip (T.unlines lineAcc)
          sql = T.stripEnd $ fromMaybe joined $ T.stripSuffix ";" joined
      in acc ++ [(name, sql)]

nowMillis :: IO Int64
nowMillis = do
  t <- getPOSIXTime
  pure $ round (t * 1000)

-- | Insert a reading from device JSON data.
insertReading :: DBHandle -> T.Text -> Value -> IO ()
insertReading h ip jsonVal = do
  let obj = case jsonVal of
              Object o -> o
              _        -> objectEmpty
  ts <- nowMillis
  let serial    = getTextOr "unknown" "serialno" obj
      model     = getTextOr "" "model" obj
      devType   = classifyDevice model
      rawJSON   = TE.decodeUtf8 $ encode jsonVal

  bracket (SQL.prepare (dbConn h) (dbInsertSQL h)) SQL.finalize $ \stmt -> do
    SQL.bindNamed stmt
      [ (":timestamp",        SQL.SQLInteger ts)
      , (":device_id",        SQL.SQLText serial)
      , (":device_type",      SQL.SQLText devType)
      , (":device_ip",        SQL.SQLText ip)
      , (":pm01",             optFloat "pm01" obj)
      , (":pm02",             optFloat "pm02" obj)
      , (":pm10",             optFloat "pm10" obj)
      , (":pm02_compensated", optFloat "pm02Compensated" obj)
      , (":rco2",             optInt "rco2" obj)
      , (":atmp",             optFloat "atmp" obj)
      , (":atmp_compensated", optFloat "atmpCompensated" obj)
      , (":rhum",             optFloat "rhum" obj)
      , (":rhum_compensated", optFloat "rhumCompensated" obj)
      , (":tvoc_index",       optFloat "tvocIndex" obj)
      , (":nox_index",        optFloat "noxIndex" obj)
      , (":wifi",             optInt "wifi" obj)
      , (":raw_json",         SQL.SQLText rawJSON)
      ]
    _ <- SQL.step stmt
    pure ()

-- | Query readings with optional device filter, limit, and downsampling.
queryReadings :: DBHandle -> ReadingQuery -> IO [Reading]
queryReadings h q
  | rqDownsampleMs q > 0 = queryDownsampled h q
  | otherwise = queryNormal h q

queryNormal :: DBHandle -> ReadingQuery -> IO [Reading]
queryNormal h q = do
  let wantDevice = rqDevice q /= "" && rqDevice q /= "all"
      cols = T.replace "\n" " " (dbQueryCols h)
      baseSql = "SELECT " <> cols <> " FROM readings WHERE "
      (whereSql, params)
        | wantDevice =
            ( "device_id = :device AND timestamp >= :from AND timestamp <= :to ORDER BY timestamp ASC"
            , [ (":device", SQL.SQLText (rqDevice q))
              , (":from",   SQL.SQLInteger (rqFrom q))
              , (":to",     SQL.SQLInteger (rqTo q))
              ]
            )
        | otherwise =
            ( "timestamp >= :from AND timestamp <= :to ORDER BY timestamp ASC"
            , [ (":from", SQL.SQLInteger (rqFrom q))
              , (":to",   SQL.SQLInteger (rqTo q))
              ]
            )
      limitSql = if rqLimit q > 0
                   then " LIMIT " <> T.pack (show (rqLimit q))
                   else ""
      fullSql = baseSql <> whereSql <> limitSql

  bracket (SQL.prepare (dbConn h) fullSql) SQL.finalize $ \stmt -> do
    SQL.bindNamed stmt params
    collectReadings stmt

queryDownsampled :: DBHandle -> ReadingQuery -> IO [Reading]
queryDownsampled h q = do
  let wantDevice = rqDevice q /= "" && rqDevice q /= "all"
      bucket = T.pack (show (rqDownsampleMs q))
      selectCols = "(timestamp / " <> bucket <> ") * " <> bucket <> " AS timestamp, "
                <> "device_id, device_type, device_ip, "
                <> "CAST(AVG(pm01) AS REAL) AS pm01, CAST(AVG(pm02) AS REAL) AS pm02, CAST(AVG(pm10) AS REAL) AS pm10, "
                <> "CAST(AVG(pm02_compensated) AS REAL) AS pm02_compensated, "
                <> "CAST(AVG(rco2) AS INTEGER) AS rco2, "
                <> "CAST(AVG(atmp) AS REAL) AS atmp, CAST(AVG(atmp_compensated) AS REAL) AS atmp_compensated, "
                <> "CAST(AVG(rhum) AS REAL) AS rhum, CAST(AVG(rhum_compensated) AS REAL) AS rhum_compensated, "
                <> "CAST(AVG(tvoc_index) AS REAL) AS tvoc_index, CAST(AVG(nox_index) AS REAL) AS nox_index, "
                <> "CAST(AVG(wifi) AS INTEGER) AS wifi"
      baseSql = "SELECT " <> selectCols <> " FROM readings WHERE "
      (whereSql, params)
        | wantDevice =
            ( "device_id = :device AND timestamp >= :from AND timestamp <= :to"
            , [ (":device", SQL.SQLText (rqDevice q))
              , (":from",   SQL.SQLInteger (rqFrom q))
              , (":to",     SQL.SQLInteger (rqTo q))
              ]
            )
        | otherwise =
            ( "timestamp >= :from AND timestamp <= :to"
            , [ (":from", SQL.SQLInteger (rqFrom q))
              , (":to",   SQL.SQLInteger (rqTo q))
              ]
            )
      groupSql = " GROUP BY (timestamp / " <> bucket <> "), device_id ORDER BY timestamp ASC"
      limitSql = if rqLimit q > 0
                   then " LIMIT " <> T.pack (show (rqLimit q))
                   else ""
      fullSql = baseSql <> whereSql <> groupSql <> limitSql

  bracket (SQL.prepare (dbConn h) fullSql) SQL.finalize $ \stmt -> do
    SQL.bindNamed stmt params
    collectDownsampledReadings stmt

-- | Collect downsampled readings (no id column, 16 columns instead of 17).
collectDownsampledReadings :: SQL.Statement -> IO [Reading]
collectDownsampledReadings stmt = go []
  where
    go acc = do
      result <- SQL.step stmt
      case result of
        SQL.Done -> pure (reverse acc)
        SQL.Row -> do
          cols <- SQL.columns stmt
          case cols of
            [c0, c1, c2, c3, c4, c5, c6, c7, c8,
             c9, c10, c11, c12, c13, c14, c15] ->
              let r = Reading
                    { rdId              = 0
                    , rdTimestamp       = sqlToInt64 c0
                    , rdDeviceId        = sqlToText c1
                    , rdDeviceType      = sqlToText c2
                    , rdDeviceIp        = sqlToText c3
                    , rdPm01            = sqlToMaybeFloat c4
                    , rdPm02            = sqlToMaybeFloat c5
                    , rdPm10            = sqlToMaybeFloat c6
                    , rdPm02Compensated = sqlToMaybeFloat c7
                    , rdRco2            = sqlToMaybeInt c8
                    , rdAtmp            = sqlToMaybeFloat c9
                    , rdAtmpCompensated = sqlToMaybeFloat c10
                    , rdRhum            = sqlToMaybeFloat c11
                    , rdRhumCompensated = sqlToMaybeFloat c12
                    , rdTvocIndex       = sqlToMaybeFloat c13
                    , rdNoxIndex        = sqlToMaybeFloat c14
                    , rdWifi            = sqlToMaybeInt c15
                    }
              in go (r : acc)
            _ -> go acc

getLatestReadings :: DBHandle -> IO [Reading]
getLatestReadings h =
  bracket (SQL.prepare (dbConn h) (dbLatestSQL h)) SQL.finalize collectReadings

getDevices :: DBHandle -> IO [DeviceSummary]
getDevices h =
  bracket (SQL.prepare (dbConn h) (dbDevicesSQL h)) SQL.finalize $ \stmt -> do
    let go acc = do
          result <- SQL.step stmt
          case result of
            SQL.Done -> pure (reverse acc)
            SQL.Row -> do
              cols <- SQL.columns stmt
              case cols of
                [SQL.SQLText did, SQL.SQLText dtype, SQL.SQLText dip,
                 tsVal, countVal] ->
                  let ds = DeviceSummary
                        { dsDeviceId     = did
                        , dsDeviceType   = dtype
                        , dsDeviceIp     = dip
                        , dsLastSeen     = sqlToInt64 tsVal
                        , dsReadingCount = sqlToInt64 countVal
                        }
                  in go (ds : acc)
                _ -> go acc  -- skip malformed rows
    go []

getReadingsCount :: DBHandle -> IO Int64
getReadingsCount h =
  bracket (SQL.prepare (dbConn h) (dbCountSQL h)) SQL.finalize $ \stmt -> do
    result <- SQL.step stmt
    case result of
      SQL.Row -> do
        cols <- SQL.columns stmt
        case cols of
          [v] -> pure (sqlToInt64 v)
          _   -> pure 0
      SQL.Done -> pure 0

-- | Count readings with optional device filter and time range.
getFilteredCount :: DBHandle -> Int64 -> Int64 -> T.Text -> IO Int64
getFilteredCount h fromTs toTs device = do
  let wantDevice = device /= "" && device /= "all"
      (sql, params)
        | wantDevice =
            ( "SELECT COUNT(*) FROM readings WHERE timestamp >= :from AND timestamp <= :to AND device_id = :device"
            , [ (":from",   SQL.SQLInteger fromTs)
              , (":to",     SQL.SQLInteger toTs)
              , (":device", SQL.SQLText device)
              ]
            )
        | otherwise =
            ( "SELECT COUNT(*) FROM readings WHERE timestamp >= :from AND timestamp <= :to"
            , [ (":from", SQL.SQLInteger fromTs)
              , (":to",   SQL.SQLInteger toTs)
              ]
            )
  bracket (SQL.prepare (dbConn h) sql) SQL.finalize $ \stmt -> do
    SQL.bindNamed stmt params
    result <- SQL.step stmt
    case result of
      SQL.Row -> do
        cols <- SQL.columns stmt
        case cols of
          [v] -> pure (sqlToInt64 v)
          _   -> pure 0
      SQL.Done -> pure 0

checkpoint :: DBHandle -> IO ()
checkpoint h = SQL.exec (dbConn h) "PRAGMA wal_checkpoint(TRUNCATE)"

-- | Convert a Reading to a JSON Value for API responses.
-- Downsampled rows have id=0 and omit the "id" field.
readingToJSON :: Reading -> Value
readingToJSON r =
  let idField = if rdId r == 0 then [] else ["id" .= rdId r]
      fields = idField ++
        [ "timestamp"        .= rdTimestamp r
        , "device_id"        .= rdDeviceId r
        , "device_type"      .= rdDeviceType r
        , "device_ip"        .= rdDeviceIp r
        , "pm01"             .= rdPm01 r
        , "pm02"             .= rdPm02 r
        , "pm10"             .= rdPm10 r
        , "pm02_compensated" .= rdPm02Compensated r
        , "rco2"             .= rdRco2 r
        , "atmp"             .= rdAtmp r
        , "atmp_compensated" .= rdAtmpCompensated r
        , "rhum"             .= rdRhum r
        , "rhum_compensated" .= rdRhumCompensated r
        , "tvoc_index"       .= rdTvocIndex r
        , "nox_index"        .= rdNoxIndex r
        , "wifi"             .= rdWifi r
        ]
  in object fields

deviceSummaryToJSON :: DeviceSummary -> Value
deviceSummaryToJSON d = object
  [ "device_id"     .= dsDeviceId d
  , "device_type"   .= dsDeviceType d
  , "device_ip"     .= dsDeviceIp d
  , "last_seen"     .= dsLastSeen d
  , "reading_count" .= dsReadingCount d
  ]

-- ---- internal helpers ----

classifyDevice :: T.Text -> T.Text
classifyDevice model
  | T.length model >= 2 && T.index model 0 == 'I' && T.index model 1 == '-' = "indoor"
  | otherwise = "outdoor"

getTextOr :: T.Text -> T.Text -> JsonObject -> T.Text
getTextOr def key obj = case objectLookup key obj of
  Just (String s) | not (T.null s) -> s
  _ -> def

optFloat :: T.Text -> JsonObject -> SQL.SQLData
optFloat key obj = case objectLookup key obj of
  Just (Number (JDbl n)) -> SQL.SQLFloat n
  Just (Number (JInt n)) -> SQL.SQLFloat (fromIntegral n)
  _                      -> SQL.SQLNull

optInt :: T.Text -> JsonObject -> SQL.SQLData
optInt key obj = case objectLookup key obj of
  Just (Number (JInt n)) -> SQL.SQLInteger n
  Just (Number (JDbl n)) -> SQL.SQLInteger (round n)
  _                      -> SQL.SQLNull

sqlToInt64 :: SQL.SQLData -> Int64
sqlToInt64 (SQL.SQLInteger i) = i
sqlToInt64 (SQL.SQLFloat f)   = round f
sqlToInt64 _                  = 0

sqlToMaybeFloat :: SQL.SQLData -> Maybe Double
sqlToMaybeFloat (SQL.SQLFloat f)   = Just f
sqlToMaybeFloat (SQL.SQLInteger i) = Just (fromIntegral i)
sqlToMaybeFloat _                  = Nothing

sqlToMaybeInt :: SQL.SQLData -> Maybe Int64
sqlToMaybeInt (SQL.SQLInteger i) = Just i
sqlToMaybeInt (SQL.SQLFloat f)   = Just (round f)
sqlToMaybeInt _                  = Nothing

collectReadings :: SQL.Statement -> IO [Reading]
collectReadings stmt = go []
  where
    go acc = do
      result <- SQL.step stmt
      case result of
        SQL.Done -> pure (reverse acc)
        SQL.Row -> do
          cols <- SQL.columns stmt
          case cols of
            [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9,
             c10, c11, c12, c13, c14, c15, c16] ->
              let r = Reading
                    { rdId              = sqlToInt64 c0
                    , rdTimestamp       = sqlToInt64 c1
                    , rdDeviceId        = sqlToText c2
                    , rdDeviceType      = sqlToText c3
                    , rdDeviceIp        = sqlToText c4
                    , rdPm01            = sqlToMaybeFloat c5
                    , rdPm02            = sqlToMaybeFloat c6
                    , rdPm10            = sqlToMaybeFloat c7
                    , rdPm02Compensated = sqlToMaybeFloat c8
                    , rdRco2            = sqlToMaybeInt c9
                    , rdAtmp            = sqlToMaybeFloat c10
                    , rdAtmpCompensated = sqlToMaybeFloat c11
                    , rdRhum            = sqlToMaybeFloat c12
                    , rdRhumCompensated = sqlToMaybeFloat c13
                    , rdTvocIndex       = sqlToMaybeFloat c14
                    , rdNoxIndex        = sqlToMaybeFloat c15
                    , rdWifi            = sqlToMaybeInt c16
                    }
              in go (r : acc)
            _ -> go acc

sqlToText :: SQL.SQLData -> T.Text
sqlToText (SQL.SQLText t) = t
sqlToText _               = ""
