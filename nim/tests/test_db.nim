import unittest
import std/[json, algorithm]
import sqlite3_wrapper
import db

proc openTestDb(): Sqlite3 =
  var dbHandle: Sqlite3
  check sqlite3_open(":memory:", addr dbHandle) == SQLITE_OK
  check dbInitialize(dbHandle)
  result = dbHandle

let indoorData = %*{
  "wifi": -47,
  "serialno": "abcdef123456",
  "model": "I-9PSL",
  "pm01": 3,
  "pm02": 5,
  "pm10": 7,
  "pm02Compensated": 6,
  "rco2": 450,
  "atmp": 22.5,
  "atmpCompensated": 23.1,
  "rhum": 45.0,
  "rhumCompensated": 44.2,
  "tvocIndex": 120,
  "noxIndex": 15
}

let outdoorData = %*{
  "wifi": -55,
  "serialno": "outdoor123",
  "model": "O-1PST",
  "pm01": 10,
  "pm02": 15,
  "pm10": 20,
  "rco2": 400,
  "atmp": 18.3,
  "rhum": 60.0
}

let nullFieldsData = %*{
  "wifi": -40,
  "serialno": "boot123",
  "model": "I-9PSL"
}

let zeroCompensatedData = %*{
  "wifi": -45,
  "serialno": "zero123",
  "model": "I-9PSL",
  "pm02Compensated": 0,
  "atmpCompensated": 0,
  "rhumCompensated": 0
}

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
    check readings[0].deviceId == "abcdef123456"
    check readings[0].deviceIp == "192.168.1.1"
    check readings[0].hasPm02
    check readings[0].pm02 == 5.0
    check readings[0].hasRco2
    check readings[0].rco2 == 450

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
    check readings[0].wifi == -40

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
      device: "abcdef123456", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 1
    check readings[0].deviceId == "abcdef123456"

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
    check ids == @["abcdef123456", "outdoor123"]

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

    check j["device_id"].getStr() == "abcdef123456"
    check j["device_type"].getStr() == "indoor"
    check j["device_ip"].getStr() == "10.0.0.1"
    check j["pm02"].getFloat() == 5.0
    check j["rco2"].getInt() == 450
    check j["atmp"].getFloat() == 22.5

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
    check j["wifi"].getInt() == -40

  test "deviceSummaryToJson":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)

    let devices = getDevices(testDb)
    check devices.len == 1
    let j = deviceSummaryToJson(devices[0])

    check j["device_id"].getStr() == "abcdef123456"
    check j["device_type"].getStr() == "indoor"
    check j["device_ip"].getStr() == "10.0.0.1"
    check j["reading_count"].getInt() == 1
