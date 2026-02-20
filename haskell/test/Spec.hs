{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertBool, assertFailure)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Config (Config(..), loadConfig)
import DB (DBHandle, Reading(..), ReadingQuery(..), DeviceSummary(..)
          , withDB, insertReading, queryReadings, getLatestReadings
          , getDevices, getReadingsCount, getFilteredCount, nowMillis
          , readingToJSON, deviceSummaryToJSON, checkpoint)
import Json (Value(..), JsonNumber(..), decodeStrict, encode, object, (.=),
             objectLookup, objectMember, objectSize, objectEmpty, ToJSON(..))

-- | Shared fixture data loaded from test-fixtures.json
data Fixtures = Fixtures
  { fxIndoorFull      :: !Value
  , fxOutdoorFull     :: !Value
  , fxAfterBoot       :: !Value
  , fxZeroCompensated :: !Value
  }

loadFixtures :: IO Fixtures
loadFixtures = do
  raw <- BS.readFile "../test-fixtures.json"
  case decodeStrict raw of
    Left err -> error $ "Failed to parse test-fixtures.json: " ++ err
    Right (Object km) -> do
      let get k = case objectLookup k km of
            Just v -> v
            Nothing -> error $ "Missing fixture key: " ++ T.unpack k
      pure Fixtures
        { fxIndoorFull      = get "indoorFull"
        , fxOutdoorFull     = get "outdoorFull"
        , fxAfterBoot       = get "afterBoot"
        , fxZeroCompensated = get "zeroCompensated"
        }
    Right _ -> error "test-fixtures.json is not an object"

-- | Downsample bucket spec entry from test-spec.json
data BucketSpec = BucketSpec
  { bsParam    :: !String
  , bsExpectMs :: !Int64
  } deriving (Show)

-- | Response shape spec from test-spec.json
data ShapeSpec = ShapeSpec
  { ssRequiredFields  :: ![String]
  , ssForbiddenFields :: ![String]
  , ssExactFields     :: ![String]
  , ssNoExtraFields   :: !Bool
  } deriving (Show)

-- | Parsed test-spec.json
data TestSpec = TestSpec
  { tsBuckets         :: ![BucketSpec]
  , tsReadingShape    :: !ShapeSpec
  , tsDownsampledShape :: !ShapeSpec
  , tsDeviceShape     :: !ShapeSpec
  , tsCountShape      :: !ShapeSpec
  , tsConfigShape     :: !ShapeSpec
  }

loadTestSpec :: IO TestSpec
loadTestSpec = do
  raw <- BS.readFile "../test-spec.json"
  case decodeStrict raw of
    Left err -> error $ "Failed to parse test-spec.json: " ++ err
    Right (Object km) -> do
      let buckets = case objectLookup "downsampleBuckets" km of
            Just (Array arr) -> map parseBucket arr
            _ -> error "test-spec.json missing downsampleBuckets array"
          shapes = case objectLookup "responseShapes" km of
            Just (Object sm) -> sm
            _ -> error "test-spec.json missing responseShapes object"
      pure TestSpec
        { tsBuckets          = buckets
        , tsReadingShape     = parseShape "reading" shapes
        , tsDownsampledShape = parseShape "readingDownsampled" shapes
        , tsDeviceShape      = parseShape "device" shapes
        , tsCountShape       = parseShape "count" shapes
        , tsConfigShape      = parseShape "config" shapes
        }
    Right _ -> error "test-spec.json is not an object"

parseBucket :: Value -> BucketSpec
parseBucket (Object o) =
  let param = case objectLookup "param" o of
        Just (String s) -> T.unpack s
        _ -> error "bucket missing param"
      expectMs = case objectLookup "expectMs" o of
        Just (Number (JInt n)) -> n
        Just (Number (JDbl n)) -> round n
        _ -> error "bucket missing expectMs"
  in BucketSpec param expectMs
parseBucket _ = error "bucket is not an object"

parseShape :: String -> [(T.Text, Value)] -> ShapeSpec
parseShape name km =
  case objectLookup (T.pack name) km of
    Just (Object o) -> ShapeSpec
      { ssRequiredFields  = getStringList "requiredFields" o
      , ssForbiddenFields = getStringList "forbiddenFields" o
      , ssExactFields     = getStringList "exactFields" o
      , ssNoExtraFields   = case objectLookup "noExtraFields" o of
          Just (Bool b) -> b
          _ -> False
      }
    _ -> error $ "test-spec.json missing responseShapes." ++ name

getStringList :: T.Text -> [(T.Text, Value)] -> [String]
getStringList key km = case objectLookup key km of
  Just (Array arr) -> concatMap extractString arr
  _ -> []
  where
    extractString (String s) = [T.unpack s]
    extractString _          = []

indoorSerial :: T.Text
indoorSerial = "84fce602549c"

withTestDB :: (DBHandle -> IO a) -> IO a
withTestDB action = do
  tmpBase <- getTemporaryDirectory
  let tmpDir = tmpBase </> "airgradientz-test-hs"
  createDirectoryIfMissing True tmpDir
  let dbPath = tmpDir </> "test.db"
  bracket
    (pure tmpDir)
    removeDirectoryRecursive
    (\_ -> withDB dbPath action)

-- | Safe first-element extraction that fails the test on empty list.
firstOr :: [a] -> IO a
firstOr (x:_) = pure x
firstOr [] = assertFailure "expected non-empty list" >> error "unreachable"

main :: IO ()
main = do
  fx <- loadFixtures
  spec <- loadTestSpec
  cfg <- loadConfig
  defaultMain (tests fx spec cfg)

tests :: Fixtures -> TestSpec -> Config -> TestTree
tests fx spec cfg = testGroup "AirGradientz Haskell"
  [ testGroup "DB" (dbTests fx)
  , testGroup "Helpers" helperTests
  , testGroup "Schema" schemaTests
  , testGroup "TestSpec: Downsample Buckets" (downsampleBucketTests spec cfg)
  , testGroup "TestSpec: Query Edge Cases" (queryEdgeCaseTests fx)
  , testGroup "TestSpec: Response Shapes" (responseShapeTests fx spec)
  , testGroup "Json" jsonTests
  ]

dbTests :: Fixtures -> [TestTree]
dbTests fx =
  [ testCase "insert and query reading" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length readings @?= 1
      r <- firstOr readings
      rdDeviceId r @?= indoorSerial
      rdDeviceType r @?= "indoor"
      rdDeviceIp r @?= "192.168.1.1"
      rdPm02 r @?= Just 41.67
      rdRco2 r @?= Just 489
      rdAtmp r @?= Just 20.78

  , testCase "null fields after boot" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxAfterBoot fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length readings @?= 1
      r <- firstOr readings
      isNothing (rdPm01 r) @? "pm01 should be Nothing after boot"
      isNothing (rdPm02 r) @? "pm02 should be Nothing after boot"
      isNothing (rdRco2 r) @? "rco2 should be Nothing after boot"
      isNothing (rdAtmp r) @? "atmp should be Nothing after boot"
      isNothing (rdRhum r) @? "rhum should be Nothing after boot"
      isNothing (rdTvocIndex r) @? "tvoc_index should be Nothing after boot"
      isNothing (rdNoxIndex r) @? "nox_index should be Nothing after boot"
      rdWifi r @?= Just (-59)

  , testCase "zero compensated values preserved" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxZeroCompensated fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdPm02 r @?= Just 10
      rdPm02Compensated r @?= Just 0
      rdRco2 r @?= Just 400
      rdAtmpCompensated r @?= Just 0
      rdRhumCompensated r @?= Just 0

  , testCase "device filtering" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      now <- nowMillis

      r1 <- queryReadings db ReadingQuery
        { rqDevice = indoorSerial, rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length r1 @?= 1
      r1first <- firstOr r1
      rdDeviceId r1first @?= indoorSerial

      r2 <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length r2 @?= 2

      r3 <- queryReadings db ReadingQuery
        { rqDevice = "", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length r3 @?= 2

      r4 <- queryReadings db ReadingQuery
        { rqDevice = "nonexistent", rqFrom = 0, rqTo = now + 1000, rqLimit = 100, rqDownsampleMs = 0 }
      length r4 @?= 0

  , testCase "query limit" $ withTestDB $ \db -> do
      mapM_ (\_ -> insertReading db "192.168.1.1" (fxIndoorFull fx)) [1..5 :: Int]
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 3, rqDownsampleMs = 0 }
      length readings @?= 3

  , testCase "latest readings" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      readings <- getLatestReadings db
      length readings @?= 2

  , testCase "get devices" $ withTestDB $ \db -> do
      mapM_ (\_ -> insertReading db "192.168.1.1" (fxIndoorFull fx)) [1..3 :: Int]
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      devices <- getDevices db
      length devices @?= 2
      let indoor = filter (\d -> dsDeviceId d == indoorSerial) devices
      assertBool "indoor device found" (length indoor == 1)
      indoorDev <- firstOr indoor
      dsReadingCount indoorDev @?= 3
      dsDeviceType indoorDev @?= "indoor"

  , testCase "readings count" $ withTestDB $ \db -> do
      c0 <- getReadingsCount db
      c0 @?= 0
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      c1 <- getReadingsCount db
      c1 @?= 2

  , testCase "checkpoint" $ withTestDB $ \db ->
      checkpoint db

  , testCase "nowMillis reasonable" $ do
      now <- nowMillis
      assertBool "nowMillis seems too low" (now > 1700000000000)

  , testCase "raw_json not in API response" $ do
      let r = Reading 1 1700000000000 "test" "indoor" "1.1.1.1"
                Nothing Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing Nothing
      let json = readingToJSON r
      case json of
        Object km -> assertBool "raw_json should not be in response"
                       (not (objectMember "raw_json" km))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "readingToJSON fields" $ do
      let r = Reading
            { rdId = 1
            , rdTimestamp = 1700000000000
            , rdDeviceId = indoorSerial
            , rdDeviceType = "indoor"
            , rdDeviceIp = "192.168.1.1"
            , rdPm01 = Nothing
            , rdPm02 = Just 12
            , rdPm10 = Nothing
            , rdPm02Compensated = Nothing
            , rdRco2 = Just 450
            , rdAtmp = Just 22.5
            , rdAtmpCompensated = Nothing
            , rdRhum = Nothing
            , rdRhumCompensated = Nothing
            , rdTvocIndex = Nothing
            , rdNoxIndex = Nothing
            , rdWifi = Nothing
            }
      let json = readingToJSON r
      case json of
        Object km -> do
          objectLookup "id" km @?= Just (Number (JInt 1))
          objectLookup "device_id" km @?= Just (String indoorSerial)
          objectLookup "pm02" km @?= Just (Number (JInt 12))
          objectLookup "rco2" km @?= Just (Number (JInt 450))
          objectLookup "pm01" km @?= Just Null
          objectLookup "wifi" km @?= Just Null
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "deviceSummaryToJSON" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km -> do
          objectLookup "device_id" km @?= Just (String indoorSerial)
          objectLookup "reading_count" km @?= Just (Number (JInt 42))
        _ -> assertFailure "deviceSummaryToJSON should return Object"

  , testCase "schema file exists" $ do
      exists <- BS.readFile "../schema.sql"
      assertBool "schema.sql should not be empty" (BS.length exists > 0)

  , testCase "downsampled query" $ withTestDB $ \db -> do
      -- Insert multiple readings that will be grouped into one bucket
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      -- Use a large bucket (1 week) so all readings fall in one bucket
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 604800000 }
      length readings @?= 1
      r <- firstOr readings
      rdId r @?= 0  -- downsampled rows have id=0
      rdDeviceId r @?= indoorSerial
      -- Check that the JSON representation omits "id"
      let json = readingToJSON r
      case json of
        Object km -> assertBool "downsampled reading should not have id field"
                       (not (objectMember "id" km))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "downsampled query with device filter" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = indoorSerial, rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 604800000 }
      length readings @?= 1
      r <- firstOr readings
      rdDeviceId r @?= indoorSerial

  , testCase "filtered count" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      now <- nowMillis
      -- Count all
      c1 <- getFilteredCount db 0 (now + 1000) "all"
      c1 @?= 3
      -- Count by device
      c2 <- getFilteredCount db 0 (now + 1000) indoorSerial
      c2 @?= 2
      -- Count with empty string (same as "all")
      c3 <- getFilteredCount db 0 (now + 1000) ""
      c3 @?= 3
      -- Count nonexistent device
      c4 <- getFilteredCount db 0 (now + 1000) "nonexistent"
      c4 @?= 0

  , testCase "latest selects by MAX(id) not MAX(timestamp)" $ withTestDB $ \db -> do
      -- Insert two readings for same device with identical timestamps
      -- by direct SQL to control timestamps
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      -- Insert another reading for the same device (same timestamp bucket)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      -- Get latest — should be the one with the higher id
      readings <- getLatestReadings db
      let indoor = filter (\r -> rdDeviceId r == indoorSerial) readings
      r <- firstOr indoor
      -- The second insert should have id=2, verify it picks the higher id
      assertBool "latest reading should have id > 1 (picks MAX(id))"
        (rdId r > 1)

  , testCase "count response shape is just count" $ do
      -- Verify that the JSON for count has only the "count" key
      let countJSON = object [ "count" .= (42 :: Int) ]
      case countJSON of
        Object km -> do
          objectSize km @?= 1
          objectLookup "count" km @?= Just (Number (JInt 42))
        _ -> assertFailure "object should produce Object"

  , testCase "deviceSummaryToJSON has no first_seen" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km ->
          assertBool "first_seen should not be in device summary"
            (not (objectMember "first_seen" km))
        _ -> assertFailure "deviceSummaryToJSON should return Object"

  , testCase "readingToJSON omits id when zero" $ do
      let r = Reading 0 1700000000000 "test" "indoor" "1.1.1.1"
                Nothing Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing Nothing
      let json = readingToJSON r
      case json of
        Object km -> do
          assertBool "id should not be present when 0"
            (not (objectMember "id" km))
          objectLookup "timestamp" km @?= Just (Number (JInt 1700000000000))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "readingToJSON includes id when nonzero" $ do
      let r = Reading 42 1700000000000 "test" "indoor" "1.1.1.1"
                Nothing Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing Nothing
      let json = readingToJSON r
      case json of
        Object km ->
          objectLookup "id" km @?= Just (Number (JInt 42))
        _ -> assertFailure "readingToJSON should return Object"
  ]

helperTests :: [TestTree]
helperTests =
  [ testCase "classify indoor model" $ withTestDB $ \db -> do
      let json = object ["model" .= ("I-9PSL" :: T.Text)]
      insertReading db "1.1.1.1" json
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdDeviceType r @?= "indoor"

  , testCase "classify outdoor model" $ withTestDB $ \db -> do
      let json = object ["model" .= ("O-1PST" :: T.Text)]
      insertReading db "1.1.1.1" json
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdDeviceType r @?= "outdoor"

  , testCase "classify no model defaults outdoor" $ withTestDB $ \db -> do
      let json = object []
      insertReading db "1.1.1.1" json
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdDeviceType r @?= "outdoor"

  , testCase "classify short model defaults outdoor" $ withTestDB $ \db -> do
      let json = object ["model" .= ("I" :: T.Text)]
      insertReading db "1.1.1.1" json
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdDeviceType r @?= "outdoor"

  , testCase "missing serialno defaults to unknown" $ withTestDB $ \db -> do
      let json = object []
      insertReading db "1.1.1.1" json
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000, rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      rdDeviceId r @?= "unknown"
  ]

schemaTests :: [TestTree]
schemaTests =
  [ testCase "queries.sql exists" $ do
      raw <- BS.readFile "../queries.sql"
      assertBool "queries.sql should not be empty" (BS.length raw > 0)

  , testCase "test-fixtures.json exists" $ do
      raw <- BS.readFile "../test-fixtures.json"
      assertBool "test-fixtures.json should not be empty" (BS.length raw > 0)
  ]

-- ---- test-spec.json aligned tests ----

-- | Verify all 7 downsample buckets from test-spec.json match config's cfgDownsampleBuckets.
downsampleBucketTests :: TestSpec -> Config -> [TestTree]
downsampleBucketTests spec cfg =
  [ testCase "config has exactly 7 downsample buckets" $
      Map.size (cfgDownsampleBuckets cfg) @?= 7

  , testCase "test-spec.json has exactly 7 downsample buckets" $
      length (tsBuckets spec) @?= 7
  ] ++
  -- One test per bucket: verify the param key exists in config and has the expected ms value
  [ testCase ("bucket " ++ bsParam b ++ " = " ++ show (bsExpectMs b) ++ "ms") $
      case Map.lookup (bsParam b) (cfgDownsampleBuckets cfg) of
        Nothing -> assertFailure $
          "bucket " ++ bsParam b ++ " not found in cfgDownsampleBuckets"
        Just ms -> ms @?= bsExpectMs b
  | b <- tsBuckets spec
  ] ++
  [ testCase "no extra buckets beyond test-spec.json" $
      let specKeys = map bsParam (tsBuckets spec)
          configKeys = Map.keys (cfgDownsampleBuckets cfg)
          extra = filter (`notElem` specKeys) configKeys
      in assertBool ("unexpected extra buckets: " ++ show extra) (null extra)
  ]

-- | Query edge cases from test-spec.json exercised at the DB layer.
queryEdgeCaseTests :: Fixtures -> [TestTree]
queryEdgeCaseTests fx =
  [ testCase "from > to returns empty results" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      -- from=9999999999999, to=1: reversed range should return nothing
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 9999999999999, rqTo = 1
        , rqLimit = 100, rqDownsampleMs = 0 }
      length readings @?= 0

  , testCase "nonexistent device returns empty" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "nonexistent-serial-xyz", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 0 }
      length readings @?= 0

  , testCase "limit=1 returns exactly 1" $ withTestDB $ \db -> do
      mapM_ (\_ -> insertReading db "192.168.1.1" (fxIndoorFull fx)) [1..5 :: Int]
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 1, rqDownsampleMs = 0 }
      length readings @?= 1

  , testCase "count with from > to returns zero" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      c <- getFilteredCount db 9999999999999 1 "all"
      c @?= 0

  , testCase "count with nonexistent device returns zero" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      c <- getFilteredCount db 0 (now + 1000) "nonexistent-serial-xyz"
      c @?= 0

  , testCase "device=all returns all readings" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 0 }
      assertBool "device=all should return results" (length readings > 0)
      length readings @?= 2

  , testCase "empty device param returns all readings" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.2" (fxOutdoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 0 }
      assertBool "empty device should return results" (length readings > 0)
      length readings @?= 2

  , testCase "downsampled from > to returns empty" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 9999999999999, rqTo = 1
        , rqLimit = 100, rqDownsampleMs = 604800000 }
      length readings @?= 0
  ]

-- | Response shape verification per test-spec.json responseShapes.
responseShapeTests :: Fixtures -> TestSpec -> [TestTree]
responseShapeTests fx spec =
  [ testCase "reading has all required fields" $ do
      let r = Reading 1 1700000000000 "test" "indoor" "1.1.1.1"
                (Just 5) (Just 10) (Just 15) (Just 8)
                (Just 400) (Just 22.5) (Just 21.0)
                (Just 50) (Just 48)
                (Just 100) (Just 1) (Just (-50))
      let json = readingToJSON r
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("reading missing required field: " ++ field)
              (objectMember (T.pack field) km)
          ) (ssRequiredFields (tsReadingShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "reading has no forbidden fields" $ do
      let r = Reading 1 1700000000000 "test" "indoor" "1.1.1.1"
                (Just 5) (Just 10) (Just 15) (Just 8)
                (Just 400) (Just 22.5) (Just 21.0)
                (Just 50) (Just 48)
                (Just 100) (Just 1) (Just (-50))
      let json = readingToJSON r
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("reading has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
          ) (ssForbiddenFields (tsReadingShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "downsampled reading has all required fields" $ do
      -- id=0 signals downsampled (no id in output)
      let r = Reading 0 1700000000000 "test" "indoor" "1.1.1.1"
                (Just 5) (Just 10) (Just 15) (Just 8)
                (Just 400) (Just 22.5) (Just 21.0)
                (Just 50) (Just 48)
                (Just 100) (Just 1) (Just (-50))
      let json = readingToJSON r
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("downsampled reading missing required field: " ++ field)
              (objectMember (T.pack field) km)
          ) (ssRequiredFields (tsDownsampledShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "downsampled reading has no forbidden fields" $ do
      let r = Reading 0 1700000000000 "test" "indoor" "1.1.1.1"
                (Just 5) (Just 10) (Just 15) (Just 8)
                (Just 400) (Just 22.5) (Just 21.0)
                (Just 50) (Just 48)
                (Just 100) (Just 1) (Just (-50))
      let json = readingToJSON r
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("downsampled reading has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
          ) (ssForbiddenFields (tsDownsampledShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "device summary has all required fields" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("device summary missing required field: " ++ field)
              (objectMember (T.pack field) km)
          ) (ssRequiredFields (tsDeviceShape spec))
        _ -> assertFailure "deviceSummaryToJSON should return Object"

  , testCase "device summary has no forbidden fields" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km ->
          mapM_ (\field ->
            assertBool ("device summary has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
          ) (ssForbiddenFields (tsDeviceShape spec))
        _ -> assertFailure "deviceSummaryToJSON should return Object"

  , testCase "count response has exact fields only" $ do
      let countJSON = object [ "count" .= (42 :: Int) ]
      case countJSON of
        Object km -> do
          -- Check exact fields present
          mapM_ (\field ->
            assertBool ("count missing exact field: " ++ field)
              (objectMember (T.pack field) km)
            ) (ssExactFields (tsCountShape spec))
          -- noExtraFields: count should have only the exact fields
          assertBool "count response should have no extra fields"
            (ssNoExtraFields (tsCountShape spec) &&
             objectSize km == length (ssExactFields (tsCountShape spec)))
        _ -> assertFailure "object should produce Object"

  , testCase "config response has required fields" $ do
      -- Build config JSON the same way handleConfig does
      let devs = [ object
                     [ "ip"    .= ("192.168.1.1" :: T.Text)
                     , "label" .= ("test" :: T.Text)
                     ]
                 ]
          cfgJSON = object
            [ "pollIntervalMs"      .= (15000 :: Int)
            , "downsampleBuckets"   .= Map.fromList [("5m" :: String, 300000 :: Int64)]
            , "devices"             .= devs
            ]
      case cfgJSON of
        Object km ->
          mapM_ (\field ->
            assertBool ("config missing required field: " ++ field)
              (objectMember (T.pack field) km)
          ) (ssRequiredFields (tsConfigShape spec))
        _ -> assertFailure "object should produce Object"

  , testCase "config response has no forbidden fields" $ do
      let devs = [ object
                     [ "ip"    .= ("192.168.1.1" :: T.Text)
                     , "label" .= ("test" :: T.Text)
                     ]
                 ]
          cfgJSON = object
            [ "pollIntervalMs"      .= (15000 :: Int)
            , "downsampleBuckets"   .= Map.fromList [("5m" :: String, 300000 :: Int64)]
            , "devices"             .= devs
            ]
      case cfgJSON of
        Object km ->
          mapM_ (\field ->
            assertBool ("config has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
          ) (ssForbiddenFields (tsConfigShape spec))
        _ -> assertFailure "object should produce Object"

  , testCase "reading from DB round-trip has correct shape" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 1, rqDownsampleMs = 0 }
      r <- firstOr readings
      let json = readingToJSON r
      case json of
        Object km -> do
          mapM_ (\field ->
            assertBool ("DB reading missing required field: " ++ field)
              (objectMember (T.pack field) km)
            ) (ssRequiredFields (tsReadingShape spec))
          mapM_ (\field ->
            assertBool ("DB reading has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
            ) (ssForbiddenFields (tsReadingShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "downsampled from DB round-trip has correct shape" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      now <- nowMillis
      readings <- queryReadings db ReadingQuery
        { rqDevice = "all", rqFrom = 0, rqTo = now + 1000
        , rqLimit = 100, rqDownsampleMs = 604800000 }
      r <- firstOr readings
      let json = readingToJSON r
      case json of
        Object km -> do
          mapM_ (\field ->
            assertBool ("downsampled DB reading missing required field: " ++ field)
              (objectMember (T.pack field) km)
            ) (ssRequiredFields (tsDownsampledShape spec))
          mapM_ (\field ->
            assertBool ("downsampled DB reading has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
            ) (ssForbiddenFields (tsDownsampledShape spec))
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "device summary from DB round-trip has correct shape" $ withTestDB $ \db -> do
      insertReading db "192.168.1.1" (fxIndoorFull fx)
      devices <- getDevices db
      d <- firstOr devices
      let json = deviceSummaryToJSON d
      case json of
        Object km -> do
          mapM_ (\field ->
            assertBool ("DB device summary missing required field: " ++ field)
              (objectMember (T.pack field) km)
            ) (ssRequiredFields (tsDeviceShape spec))
          mapM_ (\field ->
            assertBool ("DB device summary has forbidden field: " ++ field)
              (not (objectMember (T.pack field) km))
            ) (ssForbiddenFields (tsDeviceShape spec))
        _ -> assertFailure "deviceSummaryToJSON should return Object"
  ]

-- ---- Json module unit tests ----

-- | Helper: decode a UTF-8 string literal via decodeStrict
dec :: String -> Either String Value
dec = decodeStrict . TE.encodeUtf8 . T.pack

-- | Helper: check that decoding fails (returns Left)
assertLeft :: Either String Value -> IO ()
assertLeft (Left _)  = pure ()
assertLeft (Right v) = assertFailure $ "expected Left, got Right: " ++ show v

jsonTests :: [TestTree]
jsonTests =
  -- ---- Encoding ----
  [ testGroup "Encode"
    [ testCase "null" $
        encode Null @?= "null"

    , testCase "true" $
        encode (Bool True) @?= "true"

    , testCase "false" $
        encode (Bool False) @?= "false"

    , testCase "positive integer" $
        encode (Number (JInt 42)) @?= "42"

    , testCase "negative integer" $
        encode (Number (JInt (-7))) @?= "-7"

    , testCase "zero integer" $
        encode (Number (JInt 0)) @?= "0"

    , testCase "large int64" $
        encode (Number (JInt 9223372036854775807)) @?= "9223372036854775807"

    , testCase "positive double" $
        -- Haskell show for Double includes decimal point
        assertBool "should encode to a numeric string" $
          let bs = encode (Number (JDbl 3.14))
          in BS.length bs > 0

    , testCase "negative double" $
        assertBool "should start with minus" $
          let bs = encode (Number (JDbl (-2.5)))
          in case BS.uncons bs of
               Just (c, _) -> c == 45  -- '-'
               Nothing     -> False

    , testCase "NaN encodes as null" $
        encode (Number (JDbl (0/0))) @?= "null"

    , testCase "Infinity encodes as null" $
        encode (Number (JDbl (1/0))) @?= "null"

    , testCase "simple string" $
        encode (String "hello") @?= "\"hello\""

    , testCase "empty string" $
        encode (String "") @?= "\"\""

    , testCase "string with quotes" $
        encode (String "say \"hi\"") @?= "\"say \\\"hi\\\"\""

    , testCase "string with backslash" $
        encode (String "a\\b") @?= "\"a\\\\b\""

    , testCase "string with newline" $
        encode (String "a\nb") @?= "\"a\\nb\""

    , testCase "string with tab" $
        encode (String "a\tb") @?= "\"a\\tb\""

    , testCase "string with carriage return" $
        encode (String "a\rb") @?= "\"a\\rb\""

    , testCase "string with backspace" $
        encode (String "a\bb") @?= "\"a\\bb\""

    , testCase "string with form feed" $
        encode (String "a\fb") @?= "\"a\\fb\""

    , testCase "string with control char (0x01)" $
        -- Control character < 0x20 should be escaped as \u0001
        encode (String (T.singleton '\x01')) @?= "\"\\u0001\""

    , testCase "string with unicode" $
        -- Non-ASCII characters are passed through
        encode (String "\x00e9") @?= TE.encodeUtf8 "\"\x00e9\""

    , testCase "empty object" $
        encode (Object []) @?= "{}"

    , testCase "empty array" $
        encode (Array []) @?= "[]"

    , testCase "single-key object" $
        encode (object ["key" .= ("val" :: T.Text)]) @?= "{\"key\":\"val\"}"

    , testCase "multi-key object preserves order" $ do
        let v = object ["a" .= (1 :: Int), "b" .= (2 :: Int)]
        encode v @?= "{\"a\":1,\"b\":2}"

    , testCase "single-element array" $
        encode (Array [Number (JInt 1)]) @?= "[1]"

    , testCase "multi-element array" $
        encode (Array [Number (JInt 1), Bool True, Null]) @?= "[1,true,null]"

    , testCase "nested object in array" $
        encode (Array [object ["x" .= (1 :: Int)]]) @?= "[{\"x\":1}]"

    , testCase "nested array in object" $
        encode (object ["arr" .= ([1, 2, 3] :: [Int])]) @?= "{\"arr\":[1,2,3]}"

    , testCase "deeply nested structure" $ do
        let v = object ["a" .= object ["b" .= object ["c" .= (1 :: Int)]]]
        encode v @?= "{\"a\":{\"b\":{\"c\":1}}}"
    ]

  -- ---- Decoding ----
  , testGroup "Decode"
    [ testCase "null" $
        dec "null" @?= Right Null

    , testCase "true" $
        dec "true" @?= Right (Bool True)

    , testCase "false" $
        dec "false" @?= Right (Bool False)

    , testCase "positive integer" $
        dec "42" @?= Right (Number (JInt 42))

    , testCase "negative integer" $
        dec "-7" @?= Right (Number (JInt (-7)))

    , testCase "zero" $
        dec "0" @?= Right (Number (JInt 0))

    , testCase "float" $
        dec "3.14" @?= Right (Number (JDbl 3.14))

    , testCase "negative float" $
        dec "-2.5" @?= Right (Number (JDbl (-2.5)))

    , testCase "scientific notation" $
        dec "1e10" @?= Right (Number (JDbl 1e10))

    , testCase "scientific notation with plus" $
        dec "1.5e+2" @?= Right (Number (JDbl 150.0))

    , testCase "scientific notation with minus" $
        dec "1.5e-2" @?= Right (Number (JDbl 0.015))

    , testCase "simple string" $
        dec "\"hello\"" @?= Right (String "hello")

    , testCase "empty string" $
        dec "\"\"" @?= Right (String "")

    , testCase "string with escaped quote" $
        dec "\"say \\\"hi\\\"\"" @?= Right (String "say \"hi\"")

    , testCase "string with escaped backslash" $
        dec "\"a\\\\b\"" @?= Right (String "a\\b")

    , testCase "string with escaped slash" $
        dec "\"a\\/b\"" @?= Right (String "a/b")

    , testCase "string with escaped newline" $
        dec "\"a\\nb\"" @?= Right (String "a\nb")

    , testCase "string with escaped carriage return" $
        dec "\"a\\rb\"" @?= Right (String "a\rb")

    , testCase "string with escaped tab" $
        dec "\"a\\tb\"" @?= Right (String "a\tb")

    , testCase "string with escaped backspace" $
        dec "\"a\\bb\"" @?= Right (String "a\bb")

    , testCase "string with escaped form feed" $
        dec "\"a\\fb\"" @?= Right (String "a\fb")

    , testCase "unicode escape \\u0041 = A" $
        dec "\"\\u0041\"" @?= Right (String "A")

    , testCase "unicode escape \\u00e9 = e-acute" $
        dec "\"\\u00e9\"" @?= Right (String "\x00e9")

    , testCase "surrogate pair \\uD83D\\uDE00 = grinning face" $
        dec "\"\\uD83D\\uDE00\"" @?= Right (String "\x1F600")

    , testCase "empty object" $
        dec "{}" @?= Right (Object [])

    , testCase "empty array" $
        dec "[]" @?= Right (Array [])

    , testCase "simple object" $
        dec "{\"a\":1}" @?= Right (Object [("a", Number (JInt 1))])

    , testCase "multi-key object" $ do
        let result = dec "{\"a\":1,\"b\":2}"
        case result of
          Right (Object pairs) -> do
            objectLookup "a" pairs @?= Just (Number (JInt 1))
            objectLookup "b" pairs @?= Just (Number (JInt 2))
            objectSize pairs @?= 2
          _ -> assertFailure "expected Object"

    , testCase "simple array" $
        dec "[1,2,3]" @?= Right (Array [Number (JInt 1), Number (JInt 2), Number (JInt 3)])

    , testCase "mixed-type array" $
        dec "[1,\"two\",true,null]" @?=
          Right (Array [Number (JInt 1), String "two", Bool True, Null])

    , testCase "nested object" $ do
        let result = dec "{\"a\":{\"b\":1}}"
        case result of
          Right (Object outer) -> do
            case objectLookup "a" outer of
              Just (Object inner) ->
                objectLookup "b" inner @?= Just (Number (JInt 1))
              _ -> assertFailure "expected nested Object"
          _ -> assertFailure "expected Object"

    , testCase "nested array" $
        dec "[[1],[2,3]]" @?=
          Right (Array [Array [Number (JInt 1)], Array [Number (JInt 2), Number (JInt 3)]])

    , testCase "whitespace around values" $
        dec "  {  \"a\"  :  1  }  " @?= Right (Object [("a", Number (JInt 1))])

    , testCase "whitespace in arrays" $
        dec " [ 1 , 2 , 3 ] " @?=
          Right (Array [Number (JInt 1), Number (JInt 2), Number (JInt 3)])

    , testCase "newlines and tabs as whitespace" $
        dec "{\n\t\"a\":\n\t1\n}" @?= Right (Object [("a", Number (JInt 1))])
    ]

  -- ---- Decode error cases ----
  , testGroup "Decode Errors"
    [ testCase "empty input" $
        assertLeft (dec "")

    , testCase "unterminated string" $
        assertLeft (dec "\"hello")

    , testCase "trailing content after valid JSON" $
        assertLeft (dec "true false")

    , testCase "trailing garbage" $
        assertLeft (dec "42 xyz")

    , testCase "invalid token" $
        assertLeft (dec "xyz")

    , testCase "lone minus" $
        assertLeft (dec "-")

    , testCase "unterminated object" $
        assertLeft (dec "{\"a\":1")

    , testCase "unterminated array" $
        assertLeft (dec "[1,2")

    , testCase "missing colon in object" $
        assertLeft (dec "{\"a\" 1}")

    , testCase "invalid escape sequence" $
        assertLeft (dec "\"\\x\"")

    , testCase "lone high surrogate" $
        assertLeft (dec "\"\\uD83D\"")

    , testCase "invalid low surrogate" $
        assertLeft (dec "\"\\uD83D\\u0041\"")
    ]

  -- ---- Object helpers ----
  , testGroup "Object Helpers"
    [ testCase "objectLookup finds existing key" $
        objectLookup "a" [("a", Number (JInt 1)), ("b", Null)] @?= Just (Number (JInt 1))

    , testCase "objectLookup returns Nothing for missing key" $
        objectLookup "c" [("a", Number (JInt 1))] @?= Nothing

    , testCase "objectLookup on empty object" $
        objectLookup "a" objectEmpty @?= Nothing

    , testCase "objectMember true for existing" $
        objectMember "a" [("a", Null)] @?= True

    , testCase "objectMember false for missing" $
        objectMember "b" [("a", Null)] @?= False

    , testCase "objectMember false on empty" $
        objectMember "a" objectEmpty @?= False

    , testCase "objectSize counts pairs" $
        objectSize [("a", Null), ("b", Null), ("c", Null)] @?= 3

    , testCase "objectSize of empty" $
        objectSize objectEmpty @?= 0

    , testCase "objectEmpty is empty list" $
        objectEmpty @?= []

    , testCase "object builds Object value" $ do
        let v = object [("k", String "v")]
        case v of
          Object pairs -> objectSize pairs @?= 1
          _ -> assertFailure "object should produce Object"
    ]

  -- ---- ToJSON instances ----
  , testGroup "ToJSON Instances"
    [ testCase "Int" $
        toJSON (42 :: Int) @?= Number (JInt 42)

    , testCase "Int negative" $
        toJSON ((-3) :: Int) @?= Number (JInt (-3))

    , testCase "Int64" $
        toJSON (100 :: Int64) @?= Number (JInt 100)

    , testCase "Double fractional" $
        toJSON (3.14 :: Double) @?= Number (JDbl 3.14)

    , testCase "Double exact integer becomes JInt" $
        -- toJSON for Double converts exact integers to JInt
        toJSON (5.0 :: Double) @?= Number (JInt 5)

    , testCase "Bool True" $
        toJSON True @?= Bool True

    , testCase "Bool False" $
        toJSON False @?= Bool False

    , testCase "Text" $
        toJSON ("hello" :: T.Text) @?= String "hello"

    , testCase "Maybe Just" $
        toJSON (Just (42 :: Int)) @?= Number (JInt 42)

    , testCase "Maybe Nothing" $
        toJSON (Nothing :: Maybe Int) @?= Null

    , testCase "list of Int" $
        toJSON ([1, 2, 3] :: [Int]) @?= Array [Number (JInt 1), Number (JInt 2), Number (JInt 3)]

    , testCase "empty list" $
        toJSON ([] :: [Int]) @?= Array []

    , testCase "list of Value passthrough" $
        toJSON [Number (JInt 1), Null] @?= Array [Number (JInt 1), Null]

    , testCase "Map String to Int" $ do
        let m = Map.fromList [("a", 1 :: Int), ("b", 2)] :: Map.Map String Int
        let result = toJSON m
        case result of
          Object pairs -> do
            objectLookup "a" pairs @?= Just (Number (JInt 1))
            objectLookup "b" pairs @?= Just (Number (JInt 2))
          _ -> assertFailure "expected Object"

    , testCase "Map Text to Value" $ do
        let m = Map.fromList [("x" :: T.Text, Number (JInt 10))]
        toJSON m @?= Object [("x", Number (JInt 10))]

    , testCase "Value passthrough" $
        toJSON Null @?= Null

    , testCase ".= builds pair" $ do
        let pair = "key" .= (42 :: Int)
        pair @?= ("key", Number (JInt 42))
    ]

  -- ---- Round-trip (encode then decode) ----
  , testGroup "Round-trip"
    [ testCase "null" $
        dec' (encode Null) @?= Right Null

    , testCase "true" $
        dec' (encode (Bool True)) @?= Right (Bool True)

    , testCase "false" $
        dec' (encode (Bool False)) @?= Right (Bool False)

    , testCase "integer" $
        dec' (encode (Number (JInt 42))) @?= Right (Number (JInt 42))

    , testCase "negative integer" $
        dec' (encode (Number (JInt (-99)))) @?= Right (Number (JInt (-99)))

    , testCase "zero" $
        dec' (encode (Number (JInt 0))) @?= Right (Number (JInt 0))

    , testCase "simple string" $
        dec' (encode (String "hello")) @?= Right (String "hello")

    , testCase "string with special chars" $
        let s = String "line1\nline2\ttab\"quote\\backslash"
        in dec' (encode s) @?= Right s

    , testCase "string with control chars" $
        let s = String (T.pack ['\x01', '\x1f'])
        in dec' (encode s) @?= Right s

    , testCase "empty object" $
        dec' (encode (Object [])) @?= Right (Object [])

    , testCase "empty array" $
        dec' (encode (Array [])) @?= Right (Array [])

    , testCase "complex object" $ do
        let v = object
              [ "name"   .= ("test" :: T.Text)
              , "count"  .= (42 :: Int)
              , "active" .= True
              , "tags"   .= (["a", "b"] :: [T.Text])
              , "meta"   .= Null
              ]
        dec' (encode v) @?= Right v

    , testCase "nested structure" $ do
        let v = object
              [ "outer" .= object ["inner" .= ([1,2,3] :: [Int])]
              , "arr"   .= [object ["x" .= (1 :: Int)], object ["x" .= (2 :: Int)]]
              ]
        dec' (encode v) @?= Right v

    , testCase "array of mixed types" $ do
        let v = Array [Number (JInt 1), String "two", Bool True, Null, Array []]
        dec' (encode v) @?= Right v
    ]
  ]
  where
    dec' = decodeStrict
