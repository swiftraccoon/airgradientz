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


proc runSql(dbHandle: Sqlite3, sql: string) =
  var errmsg: cstring
  let rc = sqlite3_exec(dbHandle, sql.cstring, nil, nil, addr errmsg)
  if rc != SQLITE_OK:
    let msg = if errmsg != nil: $errmsg else: "unknown"
    if errmsg != nil: sqlite3_free(errmsg)
    raise newException(CatchableError, "SQL failed: " & msg)


suite "Downsample":
  test "downsampled query groups readings into buckets":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    let bucketMs = 3600000'i64  # 1 hour

    # Insert 3 readings in the same 1-hour bucket
    for i in 0 ..< 3:
      let ts = now - int64(i * 1000)
      let pm02 = 10.0 + float64(i * 10)  # 10, 20, 30
      runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (" &
        $ts & ", 'dev1', 'indoor', '1.1.1.1', " & $pm02 & ", '{}')")

    # Insert 1 reading in a different bucket (2 hours ago)
    let oldTs = now - 2 * bucketMs
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (" &
      $oldTs & ", 'dev1', 'indoor', '1.1.1.1', 100, '{}')")

    let readings = queryReadingsDownsampled(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ), bucketMs)

    # Should produce 2 buckets
    check readings.len == 2

    # Downsampled rows have id == 0
    for r in readings:
      check r.id == 0

    # Verify averages in the recent bucket (3 readings: pm02 = 10,20,30 => avg 20)
    let recentBucket = readings[1]  # ordered ASC, recent is last
    check recentBucket.hasPm02
    check recentBucket.pm02 == 20.0

  test "readingToJsonDownsampled omits id field":
    var r: Reading
    r.id = 0
    r.timestamp = 1700000000000'i64
    r.deviceId = "dev1"
    r.deviceType = "indoor"
    r.deviceIp = "1.1.1.1"
    r.hasPm02 = true
    r.pm02 = 15.0

    let j = readingToJsonDownsampled(r)
    check not j.hasKey("id")
    check j["timestamp"].getInt() == 1700000000000
    check j["pm02"].getFloat() == 15.0


suite "Regression tests":
  test "readings ordered by timestamp ASC":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (" &
      $(now - 3000) & ", 'dev1', 'indoor', '1.1.1.1', 10.0, '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (" &
      $(now - 2000) & ", 'dev1', 'indoor', '1.1.1.1', 20.0, '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, raw_json) VALUES (" &
      $(now - 1000) & ", 'dev1', 'indoor', '1.1.1.1', 30.0, '{}')")

    let readings = queryReadings(testDb, ReadingQuery(
      device: "all", fromTs: 0, toTs: now + 1000, limit: 100
    ))

    check readings.len == 3
    check readings[0].id < readings[1].id
    check readings[1].id < readings[2].id
    check readings[0].timestamp <= readings[1].timestamp
    check readings[1].timestamp <= readings[2].timestamp

  test "latest by MAX(id) not MAX(timestamp)":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    # Two readings for same device with same timestamp, different pm02
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (" &
      $now & ", 'dev1', 'indoor', '1.1.1.1', 10.0, 400, '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, pm02, rco2, raw_json) VALUES (" &
      $now & ", 'dev1', 'indoor', '1.1.1.1', 99.0, 999, '{}')")

    let latest = getLatestReadings(testDb)
    check latest.len == 1
    # Should be the second insert (higher id) with pm02=99
    check latest[0].hasPm02
    check latest[0].pm02 == 99.0
    check latest[0].hasRco2
    check latest[0].rco2 == 999

  test "devices response does not contain first_seen":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    check insertReading(testDb, "10.0.0.1", indoorData)

    let devices = getDevices(testDb)
    check devices.len == 1
    let j = deviceSummaryToJson(devices[0])

    check not j.hasKey("first_seen")
    check j.hasKey("device_id")
    check j.hasKey("last_seen")
    check j.hasKey("reading_count")


suite "Filtered count":
  test "count all devices":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 10000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 5000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 3000) & ", 'dev2', 'outdoor', '1.1.1.2', '{}')")

    let count = countReadingsFiltered(testDb, 0, now + 1000, "all")
    check count == 3

  test "count single device":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 10000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 5000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 3000) & ", 'dev2', 'outdoor', '1.1.1.2', '{}')")

    let count = countReadingsFiltered(testDb, 0, now + 1000, "dev1")
    check count == 2

  test "count with time range filter":
    let testDb = openTestDb()
    defer: discard sqlite3_close(testDb)

    let now = nowMillis()
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 10000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 5000) & ", 'dev1', 'indoor', '1.1.1.1', '{}')")
    runSql(testDb, "INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json) VALUES (" &
      $(now - 3000) & ", 'dev2', 'outdoor', '1.1.1.2', '{}')")

    # Only readings from last 7 seconds
    let count = countReadingsFiltered(testDb, now - 7000, now + 1000, "all")
    check count == 2
