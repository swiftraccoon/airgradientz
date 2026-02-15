import std/[json, os, strutils]

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

proc findConfigFile(): string =
  let envPath = getEnv("CONFIG_PATH")
  if envPath.len > 0:
    let content = readConfigFile(envPath)
    if content.len > 0:
      stderr.writeLine "[config] Loaded config from CONFIG_PATH: " & envPath
      return content
    stderr.writeLine "[config] CONFIG_PATH set but unreadable: " & envPath

  var content = readConfigFile("./airgradientz.json")
  if content.len > 0:
    stderr.writeLine "[config] Loaded config from ./airgradientz.json"
    return content

  content = readConfigFile("../airgradientz.json")
  if content.len > 0:
    stderr.writeLine "[config] Loaded config from ../airgradientz.json"
    return content

  return ""

proc loadConfig*(): Config =
  # 1. Hardcoded defaults
  result.port = 3015
  result.dbPath = "./airgradientz.db"
  result.devices[0] = DeviceConfig(ip: "192.168.88.6", label: "outdoor")
  result.devices[1] = DeviceConfig(ip: "192.168.88.159", label: "indoor")
  result.deviceCount = 2
  result.pollIntervalMs = 15000
  result.fetchTimeoutMs = 5000
  result.maxApiRows = 10000

  # 2. Config file overrides
  let content = findConfigFile()
  if content.len > 0:
    try:
      let root = parseJson(content)

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

    except JsonParsingError:
      stderr.writeLine "[config] JSON parse error"

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
