import unittest
import std/[json, algorithm, os]
import sqlite3_wrapper
import db

proc openTestDb(): Sqlite3 =
  var dbHandle: Sqlite3
  check sqlite3_open(":memory:", addr dbHandle) == SQLITE_OK
  check dbInitialize(dbHandle)
  result = dbHandle

let fixtures = parseJson(readFile(parentDir(currentSourcePath()) / ".." / ".." / "test-fixtures.json"))
let indoorData = fixtures["indoorFull"]
let outdoorData = fixtures["outdoorFull"]
let nullFieldsData = fixtures["afterBoot"]
let zeroCompensatedData = fixtures["zeroCompensated"]

let noSerialData = %*{
  "wifi": -30,
  "model": "I-9PSL",
  "pm02": 5
}


suite "DB operations":
  test "insert and query indoor reading":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "192.168.1.1", indoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check readings[0].deviceType == "indoor"
    check readings[0].deviceId == "84fce602549c"
    check readings[0].deviceIp == "192.168.1.1"
    check readings[0].hasPm02
    check readings[0].pm02 == 41.67
    check readings[0].hasRco2
    check readings[0].rco2 == 489

  test "device type classification":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 2
    var types: seq[string]
    for r in readings:
      types.add r.deviceType
    types.sort()
    check types == @["indoor", "outdoor"]

  test "null fields handling":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", nullFieldsData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check not readings[0].hasPm01
    check not readings[0].hasPm02
    check not readings[0].hasRco2
    check not readings[0].hasAtmp
    check readings[0].hasWifi
    check readings[0].wifi == -59

  test "zero compensated values are not null":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", zeroCompensatedData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check readings[0].hasPm02Compensated
    check readings[0].pm02Compensated == 0.0
    check readings[0].hasAtmpCompensated
    check readings[0].atmpCompensated == 0.0
    check readings[0].hasRhumCompensated
    check readings[0].rhumCompensated == 0.0

  test "missing serialno defaults to unknown":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", noSerialData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check readings[0].deviceId == "unknown"

  test "query with device filter":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "84fce602549c", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check readings[0].deviceId == "84fce602549c"

  test "query with device=all returns all":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 2

  test "query with limit":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 1
    ))

    check readings.len == 1

  test "getLatestReadings returns one per device":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)
    check insertReading(testDb, "10.0.0.1", indoorData)

    let latest = getLatestReadings(testDb)
    check latest.len == 2

  test "getDevices returns unique devices":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    let devices = getDevices(testDb)
    check devices.len == 2

    var ids: seq[string]
    for d in devices:
      ids.add d.deviceId
    ids.sort()
    check ids == @["84fce602549c", "ecda3b1d09d8"]

  test "getReadingsCount":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)
    check insertReading(testDb, "10.0.0.2", outdoorData)

    check getReadingsCount(testDb) == 2

  test "empty query returns empty":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 0

  test "checkpoint does not error":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check dbCheckpoint(testDb)


suite "JSON conversion":
  test "readingToJson with all fields":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    let j = readingToJson(readings[0])

    check j["device_id"].getStr() == "84fce602549c"
    check j["device_type"].getStr() == "indoor"
    check j["device_ip"].getStr() == "10.0.0.1"
    check j["pm02"].getFloat() == 41.67
    check j["rco2"].getInt() == 489
    check j["atmp"].getFloat() == 20.78

  test "readingToJson with null fields":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", nullFieldsData)

    let now = nowMillis()
    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    let j = readingToJson(readings[0])

    check j["pm01"].kind == JNull
    check j["pm02"].kind == JNull
    check j["rco2"].kind == JNull
    check j["atmp"].kind == JNull
    check j["wifi"].getInt() == -59

  test "deviceSummaryToJson":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)

    let devices = getDevices(testDb)
    check devices.len == 1
    let j = deviceSummaryToJson(devices[0])

    check j["device_id"].getStr() == "84fce602549c"
    check j["device_type"].getStr() == "indoor"
    check j["device_ip"].getStr() == "10.0.0.1"
    check j["reading_count"].getInt() == 1
