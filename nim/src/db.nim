import std/[json, times]
import sqlite3_wrapper

type
  Reading* = object
    id*: int64
    timestamp*: int64
    deviceId*: string
    deviceType*: string
    deviceIp*: string
    pm01*: float64
    pm02*: float64
    pm10*: float64
    pm02Compensated*: float64
    rco2*: int64
    atmp*: float64
    atmpCompensated*: float64
    rhum*: float64
    rhumCompensated*: float64
    tvocIndex*: float64
    noxIndex*: float64
    wifi*: int64
    hasPm01*: bool
    hasPm02*: bool
    hasPm10*: bool
    hasPm02Compensated*: bool
    hasRco2*: bool
    hasAtmp*: bool
    hasAtmpCompensated*: bool
    hasRhum*: bool
    hasRhumCompensated*: bool
    hasTvocIndex*: bool
    hasNoxIndex*: bool
    hasWifi*: bool

  DeviceSummary* = object
    deviceId*: string
    deviceType*: string
    deviceIp*: string
    lastSeen*: int64
    readingCount*: int64

  ReadingQuery* = object
    device*: string
    fromTs*: int64
    toTs*: int64
    limit*: int64

proc nowMillis*(): int64 =
  let t = getTime()
  return t.toUnix() * 1000 + int64(t.nanosecond div 1_000_000)

proc dbInitialize*(db: Sqlite3): bool =
  let pragmas = "PRAGMA journal_mode = WAL;" &
                "PRAGMA busy_timeout = 5000;" &
                "PRAGMA foreign_keys = ON;"

  var errmsg: cstring
  var rc = sqlite3_exec(db, pragmas.cstring, nil, nil, addr errmsg)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] pragma error: " & (if errmsg != nil: $errmsg else: "unknown")
    if errmsg != nil: sqlite3_free(errmsg)
    return false

  # Read shared schema
  var schema: string
  try:
    schema = readFile("../schema.sql")
  except IOError:
    stderr.writeLine "[db] failed to read ../schema.sql"
    return false

  rc = sqlite3_exec(db, schema.cstring, nil, nil, addr errmsg)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] schema error: " & (if errmsg != nil: $errmsg else: "unknown")
    if errmsg != nil: sqlite3_free(errmsg)
    return false

  return true

proc colIsNull(stmt: Sqlite3Stmt, col: cint): bool =
  sqlite3_column_type(stmt, col) == SQLITE_NULL

proc colText(stmt: Sqlite3Stmt, col: cint): string =
  let cs = sqlite3_column_text(stmt, col)
  if cs == nil: return ""
  return $cs

proc rowToReading(stmt: Sqlite3Stmt): Reading =
  result.id = sqlite3_column_int64(stmt, 0)
  result.timestamp = sqlite3_column_int64(stmt, 1)
  result.deviceId = colText(stmt, 2)
  result.deviceType = colText(stmt, 3)
  result.deviceIp = colText(stmt, 4)

  result.hasPm01 = not colIsNull(stmt, 5)
  if result.hasPm01: result.pm01 = sqlite3_column_double(stmt, 5)

  result.hasPm02 = not colIsNull(stmt, 6)
  if result.hasPm02: result.pm02 = sqlite3_column_double(stmt, 6)

  result.hasPm10 = not colIsNull(stmt, 7)
  if result.hasPm10: result.pm10 = sqlite3_column_double(stmt, 7)

  result.hasPm02Compensated = not colIsNull(stmt, 8)
  if result.hasPm02Compensated: result.pm02Compensated = sqlite3_column_double(stmt, 8)

  result.hasRco2 = not colIsNull(stmt, 9)
  if result.hasRco2: result.rco2 = sqlite3_column_int64(stmt, 9)

  result.hasAtmp = not colIsNull(stmt, 10)
  if result.hasAtmp: result.atmp = sqlite3_column_double(stmt, 10)

  result.hasAtmpCompensated = not colIsNull(stmt, 11)
  if result.hasAtmpCompensated: result.atmpCompensated = sqlite3_column_double(stmt, 11)

  result.hasRhum = not colIsNull(stmt, 12)
  if result.hasRhum: result.rhum = sqlite3_column_double(stmt, 12)

  result.hasRhumCompensated = not colIsNull(stmt, 13)
  if result.hasRhumCompensated: result.rhumCompensated = sqlite3_column_double(stmt, 13)

  result.hasTvocIndex = not colIsNull(stmt, 14)
  if result.hasTvocIndex: result.tvocIndex = sqlite3_column_double(stmt, 14)

  result.hasNoxIndex = not colIsNull(stmt, 15)
  if result.hasNoxIndex: result.noxIndex = sqlite3_column_double(stmt, 15)

  result.hasWifi = not colIsNull(stmt, 16)
  if result.hasWifi: result.wifi = sqlite3_column_int64(stmt, 16)

const QueryCols = "id, timestamp, device_id, device_type, device_ip, " &
  "pm01, pm02, pm10, pm02_compensated, rco2, " &
  "atmp, atmp_compensated, rhum, rhum_compensated, " &
  "tvoc_index, nox_index, wifi"

proc queryReadings*(db: Sqlite3, q: ReadingQuery): seq[Reading] =
  let wantDevice = q.device.len > 0 and q.device != "all"

  var sql = "SELECT " & QueryCols & " FROM readings WHERE "
  if wantDevice:
    sql.add "device_id = ?3 AND "
  sql.add "timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"

  if q.limit > 0:
    if wantDevice:
      sql.add " LIMIT ?4"
    else:
      sql.add " LIMIT ?3"

  var stmt: Sqlite3Stmt
  let rc = sqlite3_prepare_v2(db, sql.cstring, -1.cint, addr stmt, nil)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] prepare query error: " & $sqlite3_errmsg(db)
    return @[]

  discard sqlite3_bind_int64(stmt, 1, q.fromTs)
  discard sqlite3_bind_int64(stmt, 2, q.toTs)

  if wantDevice and q.limit > 0:
    discard sqlite3_bind_text(stmt, 3, q.device.cstring, -1.cint, SQLITE_TRANSIENT)
    discard sqlite3_bind_int64(stmt, 4, q.limit)
  elif wantDevice:
    discard sqlite3_bind_text(stmt, 3, q.device.cstring, -1.cint, SQLITE_TRANSIENT)
  elif q.limit > 0:
    discard sqlite3_bind_int64(stmt, 3, q.limit)

  while sqlite3_step(stmt) == SQLITE_ROW:
    result.add rowToReading(stmt)

  discard sqlite3_finalize(stmt)

proc getLatestReadings*(db: Sqlite3): seq[Reading] =
  let sql = "SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip, " &
    "r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2, " &
    "r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated, " &
    "r.tvoc_index, r.nox_index, r.wifi " &
    "FROM readings r " &
    "INNER JOIN (" &
    "    SELECT device_id, MAX(id) as max_id " &
    "    FROM readings " &
    "    GROUP BY device_id" &
    ") latest ON r.id = latest.max_id"

  var stmt: Sqlite3Stmt
  let rc = sqlite3_prepare_v2(db, sql.cstring, -1.cint, addr stmt, nil)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] prepare latest error: " & $sqlite3_errmsg(db)
    return @[]

  while sqlite3_step(stmt) == SQLITE_ROW:
    result.add rowToReading(stmt)

  discard sqlite3_finalize(stmt)

proc getDevices*(db: Sqlite3): seq[DeviceSummary] =
  let sql = "SELECT device_id, device_type, device_ip, " &
    "MAX(timestamp) as last_seen, COUNT(*) as reading_count " &
    "FROM readings GROUP BY device_id ORDER BY device_type"

  var stmt: Sqlite3Stmt
  let rc = sqlite3_prepare_v2(db, sql.cstring, -1.cint, addr stmt, nil)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] prepare devices error: " & $sqlite3_errmsg(db)
    return @[]

  while sqlite3_step(stmt) == SQLITE_ROW:
    result.add DeviceSummary(
      deviceId: colText(stmt, 0),
      deviceType: colText(stmt, 1),
      deviceIp: colText(stmt, 2),
      lastSeen: sqlite3_column_int64(stmt, 3),
      readingCount: sqlite3_column_int64(stmt, 4)
    )

  discard sqlite3_finalize(stmt)

proc getReadingsCount*(db: Sqlite3): int64 =
  let sql = "SELECT COUNT(*) FROM readings"
  var stmt: Sqlite3Stmt
  let rc = sqlite3_prepare_v2(db, sql.cstring, -1.cint, addr stmt, nil)
  if rc != SQLITE_OK: return 0

  if sqlite3_step(stmt) == SQLITE_ROW:
    result = sqlite3_column_int64(stmt, 0)

  discard sqlite3_finalize(stmt)

proc dbCheckpoint*(db: Sqlite3): bool =
  var errmsg: cstring
  let rc = sqlite3_exec(db, "PRAGMA wal_checkpoint(TRUNCATE);".cstring, nil, nil, addr errmsg)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] checkpoint error: " & (if errmsg != nil: $errmsg else: "unknown")
    if errmsg != nil: sqlite3_free(errmsg)
    return false
  return true

proc bindOptF64(stmt: Sqlite3Stmt, idx: cint, data: JsonNode, key: string) =
  if data.hasKey(key) and data[key].kind == JFloat:
    discard sqlite3_bind_double(stmt, idx, data[key].getFloat())
  elif data.hasKey(key) and data[key].kind == JInt:
    discard sqlite3_bind_double(stmt, idx, float64(data[key].getInt()))
  else:
    discard sqlite3_bind_null(stmt, idx)

proc bindOptI64(stmt: Sqlite3Stmt, idx: cint, data: JsonNode, key: string) =
  if data.hasKey(key) and data[key].kind == JInt:
    discard sqlite3_bind_int64(stmt, idx, data[key].getInt())
  elif data.hasKey(key) and data[key].kind == JFloat:
    discard sqlite3_bind_int64(stmt, idx, int64(data[key].getFloat()))
  else:
    discard sqlite3_bind_null(stmt, idx)

proc insertReading*(db: Sqlite3, ip: string, data: JsonNode): bool =
  var deviceType = "outdoor"
  if data.hasKey("model") and data["model"].kind == JString:
    let model = data["model"].getStr()
    if model.len >= 2 and model[0] == 'I' and model[1] == '-':
      deviceType = "indoor"

  var serial = "unknown"
  if data.hasKey("serialno") and data["serialno"].kind == JString:
    serial = data["serialno"].getStr()

  let rawJson = $data

  let sql = "INSERT INTO readings (" &
    "    timestamp, device_id, device_type, device_ip," &
    "    pm01, pm02, pm10, pm02_compensated," &
    "    rco2, atmp, atmp_compensated, rhum, rhum_compensated," &
    "    tvoc_index, nox_index, wifi, raw_json" &
    ") VALUES (" &
    "    ?1, ?2, ?3, ?4," &
    "    ?5, ?6, ?7, ?8," &
    "    ?9, ?10, ?11, ?12, ?13," &
    "    ?14, ?15, ?16, ?17" &
    ")"

  var stmt: Sqlite3Stmt
  var rc = sqlite3_prepare_v2(db, sql.cstring, -1.cint, addr stmt, nil)
  if rc != SQLITE_OK:
    stderr.writeLine "[db] prepare insert error: " & $sqlite3_errmsg(db)
    return false

  discard sqlite3_bind_int64(stmt, 1, nowMillis())
  discard sqlite3_bind_text(stmt, 2, serial.cstring, -1.cint, SQLITE_TRANSIENT)
  discard sqlite3_bind_text(stmt, 3, deviceType.cstring, -1.cint, SQLITE_TRANSIENT)
  discard sqlite3_bind_text(stmt, 4, ip.cstring, -1.cint, SQLITE_TRANSIENT)

  bindOptF64(stmt, 5, data, "pm01")
  bindOptF64(stmt, 6, data, "pm02")
  bindOptF64(stmt, 7, data, "pm10")
  bindOptF64(stmt, 8, data, "pm02Compensated")
  bindOptI64(stmt, 9, data, "rco2")
  bindOptF64(stmt, 10, data, "atmp")
  bindOptF64(stmt, 11, data, "atmpCompensated")
  bindOptF64(stmt, 12, data, "rhum")
  bindOptF64(stmt, 13, data, "rhumCompensated")
  bindOptF64(stmt, 14, data, "tvocIndex")
  bindOptF64(stmt, 15, data, "noxIndex")
  bindOptI64(stmt, 16, data, "wifi")

  discard sqlite3_bind_text(stmt, 17, rawJson.cstring, -1.cint, SQLITE_TRANSIENT)

  rc = sqlite3_step(stmt)
  discard sqlite3_finalize(stmt)

  if rc != SQLITE_DONE:
    stderr.writeLine "[db] insert step error: " & $sqlite3_errmsg(db)
    return false

  return true

proc readingToJson*(r: Reading): JsonNode =
  result = newJObject()
  result["id"] = newJInt(r.id)
  result["timestamp"] = newJInt(r.timestamp)
  result["device_id"] = newJString(r.deviceId)
  result["device_type"] = newJString(r.deviceType)
  result["device_ip"] = newJString(r.deviceIp)

  if r.hasPm01: result["pm01"] = newJFloat(r.pm01)
  else: result["pm01"] = newJNull()

  if r.hasPm02: result["pm02"] = newJFloat(r.pm02)
  else: result["pm02"] = newJNull()

  if r.hasPm10: result["pm10"] = newJFloat(r.pm10)
  else: result["pm10"] = newJNull()

  if r.hasPm02Compensated: result["pm02_compensated"] = newJFloat(r.pm02Compensated)
  else: result["pm02_compensated"] = newJNull()

  if r.hasRco2: result["rco2"] = newJInt(r.rco2)
  else: result["rco2"] = newJNull()

  if r.hasAtmp: result["atmp"] = newJFloat(r.atmp)
  else: result["atmp"] = newJNull()

  if r.hasAtmpCompensated: result["atmp_compensated"] = newJFloat(r.atmpCompensated)
  else: result["atmp_compensated"] = newJNull()

  if r.hasRhum: result["rhum"] = newJFloat(r.rhum)
  else: result["rhum"] = newJNull()

  if r.hasRhumCompensated: result["rhum_compensated"] = newJFloat(r.rhumCompensated)
  else: result["rhum_compensated"] = newJNull()

  if r.hasTvocIndex: result["tvoc_index"] = newJFloat(r.tvocIndex)
  else: result["tvoc_index"] = newJNull()

  if r.hasNoxIndex: result["nox_index"] = newJFloat(r.noxIndex)
  else: result["nox_index"] = newJNull()

  if r.hasWifi: result["wifi"] = newJInt(r.wifi)
  else: result["wifi"] = newJNull()

proc deviceSummaryToJson*(d: DeviceSummary): JsonNode =
  result = newJObject()
  result["device_id"] = newJString(d.deviceId)
  result["device_type"] = newJString(d.deviceType)
  result["device_ip"] = newJString(d.deviceIp)
  result["last_seen"] = newJInt(d.lastSeen)
  result["reading_count"] = newJInt(d.readingCount)
