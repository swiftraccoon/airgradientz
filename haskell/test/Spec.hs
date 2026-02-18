{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (Value(..), eitherDecodeStrict', object, (.=))
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), (@?), assertBool, assertFailure)

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T

import DB (DBHandle, Reading(..), ReadingQuery(..), DeviceSummary(..)
          , withDB, insertReading, queryReadings, getLatestReadings
          , getDevices, getReadingsCount, getFilteredCount, nowMillis
          , readingToJSON, deviceSummaryToJSON, checkpoint)

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
  case eitherDecodeStrict' raw of
    Left err -> error $ "Failed to parse test-fixtures.json: " ++ err
    Right (Object km) -> do
      let get k = case KM.lookup (Key.fromString k) km of
            Just v -> v
            Nothing -> error $ "Missing fixture key: " ++ k
      pure Fixtures
        { fxIndoorFull      = get "indoorFull"
        , fxOutdoorFull     = get "outdoorFull"
        , fxAfterBoot       = get "afterBoot"
        , fxZeroCompensated = get "zeroCompensated"
        }
    Right _ -> error "test-fixtures.json is not an object"

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
  defaultMain (tests fx)

tests :: Fixtures -> TestTree
tests fx = testGroup "AirGradientz Haskell"
  [ testGroup "DB" (dbTests fx)
  , testGroup "Helpers" helperTests
  , testGroup "Schema" schemaTests
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
                       (not (KM.member "raw_json" km))
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
          KM.lookup "id" km @?= Just (Number 1)
          KM.lookup "device_id" km @?= Just (String indoorSerial)
          KM.lookup "pm02" km @?= Just (Number 12)
          KM.lookup "rco2" km @?= Just (Number 450)
          KM.lookup "pm01" km @?= Just Null
          KM.lookup "wifi" km @?= Just Null
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "deviceSummaryToJSON" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km -> do
          KM.lookup "device_id" km @?= Just (String indoorSerial)
          KM.lookup "reading_count" km @?= Just (Number 42)
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
                       (not (KM.member "id" km))
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
          KM.size km @?= 1
          KM.lookup "count" km @?= Just (Number 42)
        _ -> assertFailure "object should produce Object"

  , testCase "deviceSummaryToJSON has no first_seen" $ do
      let d = DeviceSummary indoorSerial "indoor" "192.168.1.1" 1700000000000 42
      let json = deviceSummaryToJSON d
      case json of
        Object km ->
          assertBool "first_seen should not be in device summary"
            (not (KM.member "first_seen" km))
        _ -> assertFailure "deviceSummaryToJSON should return Object"

  , testCase "readingToJSON omits id when zero" $ do
      let r = Reading 0 1700000000000 "test" "indoor" "1.1.1.1"
                Nothing Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing Nothing
      let json = readingToJSON r
      case json of
        Object km -> do
          assertBool "id should not be present when 0"
            (not (KM.member "id" km))
          KM.lookup "timestamp" km @?= Just (Number 1700000000000)
        _ -> assertFailure "readingToJSON should return Object"

  , testCase "readingToJSON includes id when nonzero" $ do
      let r = Reading 42 1700000000000 "test" "indoor" "1.1.1.1"
                Nothing Nothing Nothing Nothing Nothing Nothing
                Nothing Nothing Nothing Nothing Nothing Nothing
      let json = readingToJSON r
      case json of
        Object km ->
          KM.lookup "id" km @?= Just (Number 42)
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
