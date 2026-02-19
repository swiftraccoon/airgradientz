import std/[json, os, strutils, tables]
import log

const
  MaxDevices* = 8

type
  DeviceConfig* = object
    ip*: string
    label*: string

  Config* = object
    port*: uint16
    dbPath*: string
    devices*: array[MaxDevices, DeviceConfig]
    deviceCount*: int
    pollIntervalMs*: uint32
    fetchTimeoutMs*: uint32
    maxApiRows*: uint32
    downsampleBuckets*: Table[string, int64]

proc readConfigFile(path: string): string =
  if not fileExists(path):
    return ""
  let size = getFileSize(path)
  if size <= 0 or size > 1_048_576:
    return ""
  try:
    return readFile(path)
  except IOError:
    return ""

proc findConfigFile(): (string, string) =
  ## Returns (content, path) or ("", "") if not found.
  let envPath = getEnv("CONFIG_PATH")
  if envPath.len > 0:
    let content = readConfigFile(envPath)
    if content.len > 0:
      return (content, envPath)
    logTs "[config] CONFIG_PATH set but unreadable: " & envPath

  var content = readConfigFile("./airgradientz.json")
  if content.len > 0:
    return (content, "./airgradientz.json")

  content = readConfigFile("../airgradientz.json")
  if content.len > 0:
    return (content, "../airgradientz.json")

  return ("", "")

proc loadConfig*(): Config =
  result.dbPath = "./airgradientz.db"
  result.downsampleBuckets = initTable[string, int64]()

  # 1. Config file (mandatory)
  let (content, path) = findConfigFile()
  if content.len == 0:
    logTs "fatal: config file not found"
    quit(1)

  logTs "[config] Loaded config from " & path

  var root: JsonNode
  try:
    root = parseJson(content)
  except JsonParsingError:
    logTs "fatal: config file parse error"
    quit(1)

  # 2. Read values directly from top-level JSON keys
  if root.hasKey("ports") and root["ports"].kind == JObject:
    if root["ports"].hasKey("nim"):
      let p = root["ports"]["nim"].getInt()
      if p > 0 and p <= 65535:
        result.port = uint16(p)

  if root.hasKey("devices") and root["devices"].kind == JArray:
    let devArr = root["devices"]
    let count = min(devArr.len, MaxDevices)
    result.deviceCount = count
    for i in 0 ..< count:
      let dev = devArr[i]
      if dev.kind == JObject:
        if dev.hasKey("ip") and dev["ip"].kind == JString:
          result.devices[i].ip = dev["ip"].getStr()
        if dev.hasKey("label") and dev["label"].kind == JString:
          result.devices[i].label = dev["label"].getStr()

  if root.hasKey("pollIntervalMs"):
    let n = root["pollIntervalMs"].getInt()
    if n > 0:
      result.pollIntervalMs = uint32(n)

  if root.hasKey("fetchTimeoutMs"):
    let n = root["fetchTimeoutMs"].getInt()
    if n > 0:
      result.fetchTimeoutMs = uint32(n)

  if root.hasKey("maxApiRows"):
    let n = root["maxApiRows"].getInt()
    if n > 0:
      result.maxApiRows = uint32(n)

  if root.hasKey("downsampleBuckets") and root["downsampleBuckets"].kind == JObject:
    for key, val in root["downsampleBuckets"].pairs:
      if val.kind == JInt:
        let ms = val.getInt()
        if ms > 0:
          result.downsampleBuckets[key] = int64(ms)

  # 3. Env var overrides (highest priority)
  let portStr = getEnv("PORT")
  if portStr.len > 0:
    try:
      let p = parseInt(portStr)
      if p > 0 and p <= 65535:
        result.port = uint16(p)
    except ValueError:
      discard

  let dbEnv = getEnv("DB_PATH")
  if dbEnv.len > 0:
    result.dbPath = dbEnv

  # 4. Validate all required keys
  var missing: seq[string]
  if result.pollIntervalMs <= 0:
    missing.add("pollIntervalMs")
  if result.fetchTimeoutMs <= 0:
    missing.add("fetchTimeoutMs")
  if result.maxApiRows <= 0:
    missing.add("maxApiRows")
  if result.downsampleBuckets.len == 0:
    missing.add("downsampleBuckets")
  if result.deviceCount == 0:
    missing.add("devices")
  if result.port == 0:
    missing.add("ports.nim")

  if missing.len > 0:
    logTs "fatal: missing required config keys: " & missing.join(", ")
    quit(1)
