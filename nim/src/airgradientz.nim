import std/[json, locks, os, posix, strutils, tables]
import config, db, log, sqlite3_wrapper

# ── Linux epoll FFI ──────────────────────────────────────────────────────

const
  EPOLL_CTL_ADD = 1.cint
  EPOLL_CTL_DEL = 2.cint
  EPOLL_CTL_MOD = 3.cint
  EPOLLIN = 0x001.uint32
  EPOLLOUT = 0x004.uint32
  EPOLLERR = 0x008.uint32
  EPOLLHUP = 0x010.uint32
  EPOLLET = 0x80000000'u32
  EPOLL_CLOEXEC = 0x80000.cint

type
  EpollData {.importc: "epoll_data_t", header: "<sys/epoll.h>", union.} = object
    fd {.importc: "fd".}: cint

  EpollEvent {.importc: "struct epoll_event", header: "<sys/epoll.h>", packed.} = object
    events {.importc: "events".}: uint32
    data {.importc: "data".}: EpollData

proc epoll_create1(flags: cint): cint
  {.cdecl, importc: "epoll_create1", header: "<sys/epoll.h>".}

proc epoll_ctl(epfd: cint, op: cint, fd: cint, event: ptr EpollEvent): cint
  {.cdecl, importc: "epoll_ctl", header: "<sys/epoll.h>".}

proc epoll_wait(epfd: cint, events: ptr EpollEvent, maxevents: cint,
                timeout: cint): cint
  {.cdecl, importc: "epoll_wait", header: "<sys/epoll.h>".}

proc c_signal(sig: cint, handler: proc(sig: cint) {.noconv.})
  {.cdecl, importc: "signal", header: "<signal.h>".}

proc c_read(fd: cint, buf: pointer, count: csize_t): cint
  {.cdecl, importc: "read", header: "<unistd.h>".}

proc c_write(fd: cint, buf: pointer, count: csize_t): cint
  {.cdecl, importc: "write", header: "<unistd.h>".}

proc c_close(fd: cint): cint
  {.cdecl, importc: "close", header: "<unistd.h>".}

proc c_getpid(): cint
  {.cdecl, importc: "getpid", header: "<unistd.h>".}

proc c_sysconf(name: cint): clong
  {.cdecl, importc: "sysconf", header: "<unistd.h>".}

let SC_PAGESIZE {.importc: "_SC_PAGESIZE", header: "<unistd.h>", nodecl.}: cint

proc htonl(x: uint32): uint32
  {.cdecl, importc: "htonl", header: "<arpa/inet.h>".}

proc htons(x: uint16): uint16
  {.cdecl, importc: "htons", header: "<arpa/inet.h>".}

proc gai_strerror(ecode: cint): cstring
  {.cdecl, importc: "gai_strerror", header: "<netdb.h>".}

proc c_getaddrinfo(node: cstring, service: cstring, hints: ptr AddrInfo,
                   res: ptr ptr AddrInfo): cint
  {.cdecl, importc: "getaddrinfo", header: "<netdb.h>".}

proc c_freeaddrinfo(ai: ptr AddrInfo)
  {.cdecl, importc: "freeaddrinfo", header: "<netdb.h>".}

# ── Constants ────────────────────────────────────────────────────────────

const
  MaxEvents = 64
  MaxConns = 4096
  ReadBufSize = 8192
  ConnTimeout = 30  # seconds
  MaxFileSize = 16 * 1024 * 1024  # 16 MB
  MaxResponseSize = 1 * 1024 * 1024  # 1 MB for HTTP client
  CheckpointIntervalPolls = 10

# ── Connection state ─────────────────────────────────────────────────────

type
  ConnPhase = enum
    cpEmpty, cpReading, cpWriting

  ConnData = object
    phase: ConnPhase
    acceptedAt: int64
    readBuf: array[ReadBufSize, char]
    readPos: int
    response: string
    writePos: int

  HttpReq = object
    httpMethod: string
    path: string
    query: string

  HealthStatus = enum
    hsUnknown, hsOk, hsError

  DeviceHealth = object
    ip: string
    label: string
    status: HealthStatus
    lastSuccess: int64
    lastError: int64
    lastErrorMessage: string
    consecutiveFailures: int

  AppState = object
    db: Sqlite3
    cfg: Config
    startedAt: int64
    requestsServed: int64
    activeConnections: int
    pollSuccesses: int64
    pollFailures: int64
    health: seq[DeviceHealth]
    shutdown: bool

var
  connTable: array[MaxConns, ConnData]
  gState: AppState
  dbLock: Lock

# ── Signal handler ───────────────────────────────────────────────────────

proc signalHandler(sig: cint) {.noconv.} =
  gState.shutdown = true

# ── Helpers ──────────────────────────────────────────────────────────────

proc setNonblocking(fd: cint): bool =
  let flags = fcntl(fd, F_GETFL, 0)
  if flags < 0: return false
  return fcntl(fd, F_SETFL, flags or O_NONBLOCK) >= 0

proc getUnixTime(): int64 =
  var t: posix.Time
  discard posix.time(t)
  return int64(t)

proc connReset(fd: cint) =
  if fd >= 0 and fd < MaxConns:
    connTable[fd] = ConnData()

proc hasHeaderEnd(buf: openArray[char], length: int): bool =
  if length < 4: return false
  for i in 0 ..< length - 3:
    if buf[i] == '\r' and buf[i+1] == '\n' and
       buf[i+2] == '\r' and buf[i+3] == '\n':
      return true
  return false

# ── URL decoding ─────────────────────────────────────────────────────────

proc urlDecode(s: string): string =
  result = newStringOfCap(s.len)
  var i = 0
  while i < s.len:
    if s[i] == '%' and i + 2 < s.len:
      let hi = s[i+1]
      let lo = s[i+2]
      var valid = true
      var val: int = 0
      for c in [hi, lo]:
        val = val shl 4
        if c >= '0' and c <= '9': val = val or (ord(c) - ord('0'))
        elif c >= 'a' and c <= 'f': val = val or (ord(c) - ord('a') + 10)
        elif c >= 'A' and c <= 'F': val = val or (ord(c) - ord('A') + 10)
        else: valid = false; break
      if valid and val != 0:
        result.add chr(val)
        i += 3
        continue
    if s[i] == '+':
      result.add ' '
    else:
      result.add s[i]
    i += 1

proc queryParam(query: string, name: string): string =
  if query.len == 0: return ""
  for pair in query.split('&'):
    let eqIdx = pair.find('=')
    if eqIdx >= 0:
      let key = pair[0 ..< eqIdx]
      if key == name:
        return urlDecode(pair[eqIdx + 1 .. ^1])
    else:
      if pair == name:
        return ""
  return ""

proc parseI64Param(query: string, name: string, default: int64): int64 =
  let s = queryParam(query, name)
  if s.len == 0: return default
  try:
    return parseBiggestInt(s)
  except ValueError:
    return default

# ── Content type ─────────────────────────────────────────────────────────

proc contentTypeFor(path: string): string =
  let dot = path.rfind('.')
  if dot < 0: return "application/octet-stream"
  let ext = path[dot .. ^1]
  case ext
  of ".html": "text/html; charset=utf-8"
  of ".css": "text/css; charset=utf-8"
  of ".js": "application/javascript; charset=utf-8"
  of ".json": "application/json; charset=utf-8"
  of ".png": "image/png"
  of ".jpg", ".jpeg": "image/jpeg"
  of ".svg": "image/svg+xml"
  of ".ico": "image/x-icon"
  else: "application/octet-stream"

# ── Response builders ────────────────────────────────────────────────────

proc buildResponse(status: int, statusText: string, contentType: string,
                   body: string, extraHeaders: string = ""): string =
  var hdr = "HTTP/1.1 " & $status & " " & statusText & "\r\n" &
    "Connection: close\r\n" &
    "X-Content-Type-Options: nosniff\r\n" &
    "X-Frame-Options: DENY\r\n" &
    "Content-Type: " & contentType & "\r\n" &
    "Content-Length: " & $body.len & "\r\n"
  if extraHeaders.len > 0:
    hdr.add extraHeaders
  hdr.add "\r\n"
  return hdr & body

proc jsonResponse(status: int, statusText: string, j: JsonNode): string =
  buildResponse(status, statusText, "application/json", $j)

proc errorResponse(status: int, statusText: string, msg: string): string =
  buildResponse(status, statusText, "application/json",
                "{\"error\":\"" & msg & "\"}")

# ── Static file serving ──────────────────────────────────────────────────

proc buildStaticResponse(reqPath: string): string =
  # Path traversal check on decoded path
  let decoded = urlDecode(reqPath)
  if ".." in decoded:
    return errorResponse(403, "Forbidden", "Forbidden")

  var relative = decoded
  while relative.len > 0 and relative[0] == '/':
    relative = relative[1 .. ^1]

  let filepath = if relative.len == 0: "public/index.html"
                 else: "public/" & relative

  # Verify resolved path stays within public/
  let resolvedPath = try: expandFilename(filepath) except OSError: ""
  let publicDir = try: expandFilename("public") except OSError: ""
  if resolvedPath.len == 0 or publicDir.len == 0:
    return errorResponse(404, "Not Found", "Not found")
  if not resolvedPath.startsWith(publicDir):
    return errorResponse(404, "Not Found", "Not found")

  if not fileExists(filepath):
    return errorResponse(404, "Not Found", "Not found")

  let fileSize = getFileSize(filepath)
  if fileSize < 0 or fileSize > MaxFileSize:
    return errorResponse(413, "Payload Too Large", "File too large")

  try:
    let body = readFile(filepath)
    let ct = contentTypeFor(filepath)
    return buildResponse(200, "OK", ct, body,
                         "Cache-Control: public, max-age=600\r\n")
  except IOError:
    return errorResponse(500, "Internal Server Error", "Internal server error")

# ── HTTP client for poller ───────────────────────────────────────────────

proc httpGet(ip: string, path: string, timeoutMs: uint32): (bool, string) =
  ## Returns (success, body_or_error)
  let sock = cint(posix.socket(AF_INET, SOCK_STREAM, 0))
  if sock < 0:
    return (false, "socket: " & $strerror(errno))

  # Set timeouts
  let tvSec = clong(timeoutMs div 1000)
  let tvUsec = clong((timeoutMs mod 1000) * 1000)
  var tv: Timeval
  tv.tv_sec = posix.Time(tvSec)
  tv.tv_usec = Suseconds(tvUsec)
  discard posix.setsockopt(SocketHandle(sock), SOL_SOCKET, SO_SNDTIMEO, addr tv, SockLen(sizeof(tv)))
  discard posix.setsockopt(SocketHandle(sock), SOL_SOCKET, SO_RCVTIMEO, addr tv, SockLen(sizeof(tv)))

  # Resolve and connect
  var hints: AddrInfo
  zeroMem(addr hints, sizeof(hints))
  hints.ai_family = AF_INET
  hints.ai_socktype = SOCK_STREAM

  # Split host and port from ip parameter (supports "host:port" or plain "host")
  let colonIdx = ip.rfind(':')
  let (hostname, portStr) = if colonIdx >= 0:
    (ip[0 ..< colonIdx], ip[colonIdx + 1 .. ^1])
  else:
    (ip, "80")

  var res: ptr AddrInfo
  let gaiRc = c_getaddrinfo(hostname.cstring, portStr.cstring, addr hints, addr res)
  if gaiRc != 0:
    discard c_close(sock)
    return (false, "resolve failed: " & $gai_strerror(gaiRc))

  let connRc = posix.connect(SocketHandle(sock), res.ai_addr, res.ai_addrlen)
  c_freeaddrinfo(res)
  if connRc < 0:
    let err = $strerror(errno)
    discard c_close(sock)
    return (false, "connect: " & err)

  # Send request
  let reqStr = "GET " & path & " HTTP/1.1\r\nHost: " & hostname & "\r\nConnection: close\r\n\r\n"
  var sent = 0
  while sent < reqStr.len:
    let n = c_write(sock, addr reqStr[sent], csize_t(reqStr.len - sent))
    if n <= 0:
      let err = $strerror(errno)
      discard c_close(sock)
      return (false, "write: " & err)
    sent += n

  # Read response (cap at MaxResponseSize)
  var respBuf = ""
  var buf: array[4096, char]
  while true:
    let n = c_read(sock, addr buf[0], csize_t(sizeof(buf)))
    if n < 0:
      let err = $strerror(errno)
      discard c_close(sock)
      return (false, "read: " & err)
    if n == 0: break
    for j in 0 ..< n:
      respBuf.add buf[j]
    if respBuf.len > MaxResponseSize:
      discard c_close(sock)
      return (false, "response too large")

  discard c_close(sock)

  # Parse status line
  let eol = respBuf.find("\r\n")
  if eol < 0:
    return (false, "no status line")

  let statusLine = respBuf[0 ..< eol]
  let sp = statusLine.find(' ')
  if sp < 0:
    return (false, "invalid status line")

  let statusStr = statusLine[sp+1 ..< min(sp+4, statusLine.len)]
  var httpStatus: int
  try:
    httpStatus = parseInt(statusStr)
  except ValueError:
    return (false, "invalid status code")

  if httpStatus < 200 or httpStatus >= 300:
    return (false, "HTTP " & $httpStatus)

  # Find body
  let headerEnd = respBuf.find("\r\n\r\n")
  if headerEnd < 0:
    return (false, "no header terminator")

  return (true, respBuf[headerEnd + 4 .. ^1])

# ── Poller ───────────────────────────────────────────────────────────────

proc fetchDevice(idx: int) =
  let ip = gState.cfg.devices[idx].ip
  let label = gState.cfg.devices[idx].label

  let (ok, bodyOrErr) = httpGet(ip, "/measures/current", gState.cfg.fetchTimeoutMs)

  if not ok:
    logTs "[poller] " & label & " (" & ip & "): fetch failed: " & bodyOrErr
    gState.pollFailures += 1
    gState.health[idx].status = hsError
    gState.health[idx].lastError = db.nowMillis()
    gState.health[idx].lastErrorMessage = bodyOrErr
    gState.health[idx].consecutiveFailures += 1
    return

  var data: JsonNode
  try:
    data = parseJson(bodyOrErr)
  except JsonParsingError:
    let msg = "JSON parse error"
    logTs "[poller] " & label & " (" & ip & "): " & msg
    gState.pollFailures += 1
    gState.health[idx].status = hsError
    gState.health[idx].lastError = db.nowMillis()
    gState.health[idx].lastErrorMessage = msg
    gState.health[idx].consecutiveFailures += 1
    return

  if data.kind != JObject:
    let msg = "unexpected response type: not an object"
    logTs "[poller] " & label & " (" & ip & "): " & msg
    gState.pollFailures += 1
    gState.health[idx].status = hsError
    gState.health[idx].lastError = db.nowMillis()
    gState.health[idx].lastErrorMessage = msg
    gState.health[idx].consecutiveFailures += 1
    return

  var insertOk: bool
  withLock(dbLock):
    insertOk = insertReading(gState.db, ip, data)
  if not insertOk:
    let msg = "DB insert failed"
    logTs "[poller] " & label & " (" & ip & "): " & msg
    gState.pollFailures += 1
    gState.health[idx].status = hsError
    gState.health[idx].lastError = db.nowMillis()
    gState.health[idx].lastErrorMessage = msg
    gState.health[idx].consecutiveFailures += 1
    return

  # Log success
  var pm02s = "N/A"
  var rco2s = "N/A"
  var atmps = "N/A"

  if data.hasKey("pm02"):
    if data["pm02"].kind == JFloat: pm02s = formatFloat(data["pm02"].getFloat(), ffDecimal, 2)
    elif data["pm02"].kind == JInt: pm02s = $data["pm02"].getInt()

  if data.hasKey("rco2"):
    if data["rco2"].kind == JInt: rco2s = $data["rco2"].getInt()
    elif data["rco2"].kind == JFloat: rco2s = $int64(data["rco2"].getFloat())

  if data.hasKey("atmp"):
    if data["atmp"].kind == JFloat: atmps = formatFloat(data["atmp"].getFloat(), ffDecimal, 2)
    elif data["atmp"].kind == JInt: atmps = $data["atmp"].getInt()

  gState.pollSuccesses += 1
  gState.health[idx].status = hsOk
  gState.health[idx].lastSuccess = db.nowMillis()
  gState.health[idx].lastErrorMessage = ""
  gState.health[idx].consecutiveFailures = 0

  logTs "[poller] " & label & " (" & ip & "): OK — PM2.5=" &
    pm02s & ", CO2=" & rco2s & ", T=" & atmps & "°C"

proc pollAll() =
  for i in 0 ..< gState.cfg.deviceCount:
    fetchDevice(i)

# ── API handlers ─────────────────────────────────────────────────────────

proc handleReadings(req: HttpReq): string =
  let now = db.nowMillis()
  let defaultFrom = now - 24 * 60 * 60 * 1000

  let fromTs = parseI64Param(req.query, "from", defaultFrom)
  let toTs = parseI64Param(req.query, "to", now)
  let device = queryParam(req.query, "device")
  let downsampleParam = queryParam(req.query, "downsample")

  # Validate downsample param
  var bucketMs: int64 = 0
  if downsampleParam.len > 0:
    if downsampleParam notin gState.cfg.downsampleBuckets:
      return errorResponse(400, "Bad Request",
        "Invalid downsample value. Valid: 5m, 10m, 15m, 30m, 1h, 1d, 1w")
    bucketMs = gState.cfg.downsampleBuckets[downsampleParam]

  let maxRows = int64(gState.cfg.maxApiRows)
  let requestedLimit = parseI64Param(req.query, "limit", maxRows)
  let effectiveLimit = if requestedLimit > 0 and requestedLimit < maxRows:
                         requestedLimit
                       else:
                         maxRows

  let q = ReadingQuery(
    device: if device.len > 0: device else: "all",
    fromTs: fromTs,
    toTs: toTs,
    limit: effectiveLimit
  )

  var readings: seq[Reading]
  if bucketMs > 0:
    withLock(dbLock):
      readings = queryReadingsDownsampled(gState.db, q, bucketMs)
    var arr = newJArray()
    for r in readings:
      arr.add readingToJsonDownsampled(r)
    return jsonResponse(200, "OK", arr)
  else:
    withLock(dbLock):
      readings = queryReadings(gState.db, q)
    var arr = newJArray()
    for r in readings:
      arr.add readingToJson(r)
    return jsonResponse(200, "OK", arr)

proc handleReadingsCount(req: HttpReq): string =
  let now = db.nowMillis()
  let defaultFrom = now - 24 * 60 * 60 * 1000

  let fromTs = parseI64Param(req.query, "from", defaultFrom)
  let toTs = parseI64Param(req.query, "to", now)
  let device = queryParam(req.query, "device")

  var count: int64
  withLock(dbLock):
    count = countReadingsFiltered(gState.db, fromTs, toTs,
      if device.len > 0: device else: "all")

  var obj = newJObject()
  obj["count"] = newJInt(count)
  return jsonResponse(200, "OK", obj)

proc handleReadingsLatest(): string =
  var readings: seq[Reading]
  withLock(dbLock):
    readings = getLatestReadings(gState.db)
  var arr = newJArray()
  for r in readings:
    arr.add readingToJson(r)
  return jsonResponse(200, "OK", arr)

proc handleDevices(): string =
  var devices: seq[DeviceSummary]
  withLock(dbLock):
    devices = getDevices(gState.db)
  var arr = newJArray()
  for d in devices:
    arr.add deviceSummaryToJson(d)
  return jsonResponse(200, "OK", arr)

proc handleHealth(): string =
  var arr = newJArray()
  for h in gState.health:
    var obj = newJObject()
    obj["ip"] = newJString(h.ip)
    obj["label"] = newJString(h.label)

    let statusStr = case h.status
      of hsUnknown: "unknown"
      of hsOk: "ok"
      of hsError: "error"
    obj["status"] = newJString(statusStr)

    if h.lastSuccess != 0: obj["lastSuccess"] = newJInt(h.lastSuccess)
    else: obj["lastSuccess"] = newJNull()

    if h.lastError != 0: obj["lastError"] = newJInt(h.lastError)
    else: obj["lastError"] = newJNull()

    if h.lastErrorMessage.len > 0: obj["lastErrorMessage"] = newJString(h.lastErrorMessage)
    else: obj["lastErrorMessage"] = newJNull()

    obj["consecutiveFailures"] = newJInt(h.consecutiveFailures)
    arr.add obj

  return jsonResponse(200, "OK", arr)

proc handleConfig(): string =
  var devicesArr = newJArray()
  for i in 0 ..< gState.cfg.deviceCount:
    var d = newJObject()
    d["ip"] = newJString(gState.cfg.devices[i].ip)
    d["label"] = newJString(gState.cfg.devices[i].label)
    devicesArr.add d

  var cfg = newJObject()
  cfg["pollIntervalMs"] = newJInt(int64(gState.cfg.pollIntervalMs))
  var bucketsObj = newJObject()
  for key, val in gState.cfg.downsampleBuckets:
    bucketsObj[key] = newJInt(val)
  cfg["downsampleBuckets"] = bucketsObj
  cfg["devices"] = devicesArr

  return jsonResponse(200, "OK", cfg)

proc handleStats(): string =
  let now = db.nowMillis()
  let uptimeMs = now - gState.startedAt

  # RSS from /proc/self/statm
  var memoryRssBytes: int64 = 0
  try:
    let statm = readFile("/proc/self/statm")
    let parts = statm.splitWhitespace()
    if parts.len >= 2:
      let pages = parseBiggestInt(parts[1])
      let pageSize = c_sysconf(SC_PAGESIZE)
      memoryRssBytes = pages * int64(pageSize)
  except:
    discard

  # DB size
  var dbSizeBytes: int64 = 0
  try:
    dbSizeBytes = getFileSize(gState.cfg.dbPath)
  except OSError:
    discard

  var readingsCount: int64
  withLock(dbLock):
    readingsCount = getReadingsCount(gState.db)

  var obj = newJObject()
  obj["implementation"] = newJString("nim")
  obj["pid"] = newJInt(int64(c_getpid()))
  obj["uptime_ms"] = newJInt(uptimeMs)
  obj["memory_rss_bytes"] = newJInt(memoryRssBytes)
  obj["db_size_bytes"] = newJInt(dbSizeBytes)
  obj["readings_count"] = newJInt(readingsCount)
  obj["requests_served"] = newJInt(gState.requestsServed)
  obj["active_connections"] = newJInt(int64(gState.activeConnections))
  obj["poll_successes"] = newJInt(gState.pollSuccesses)
  obj["poll_failures"] = newJInt(gState.pollFailures)
  obj["pool_alloc_count"] = newJInt(0)
  obj["pool_bytes_used"] = newJInt(0)
  obj["started_at"] = newJInt(gState.startedAt)

  return jsonResponse(200, "OK", obj)

# ── Request parsing & routing ────────────────────────────────────────────

proc parseRequestBuf(buf: openArray[char], length: int): (bool, HttpReq) =
  var req = HttpReq()

  # Find end of request line
  var lineEnd = -1
  for j in 0 ..< length - 1:
    if buf[j] == '\r' and buf[j+1] == '\n':
      lineEnd = j
      break

  if lineEnd < 0:
    return (false, req)

  # Extract request line
  let line = newString(lineEnd)
  copyMem(addr line[0], unsafeAddr buf[0], lineEnd)

  # Parse: METHOD /path?query HTTP/1.x
  let sp1 = line.find(' ')
  if sp1 < 0: return (false, req)

  req.httpMethod = line[0 ..< sp1]

  let rest = line[sp1 + 1 .. ^1]
  let sp2 = rest.find(' ')
  if sp2 < 0: return (false, req)

  let uri = rest[0 ..< sp2]

  let qmark = uri.find('?')
  if qmark >= 0:
    req.path = uri[0 ..< qmark]
    req.query = uri[qmark + 1 .. ^1]
  else:
    req.path = uri

  return (true, req)

proc routeRequest(req: HttpReq): string =
  if req.httpMethod != "GET":
    return errorResponse(405, "Method Not Allowed", "Method not allowed")

  case req.path
  of "/api/readings/count": return handleReadingsCount(req)
  of "/api/readings/latest": return handleReadingsLatest()
  of "/api/readings": return handleReadings(req)
  of "/api/devices": return handleDevices()
  of "/api/health": return handleHealth()
  of "/api/config": return handleConfig()
  of "/api/stats": return handleStats()
  else: return buildStaticResponse(req.path)

# ── Close connection ─────────────────────────────────────────────────────

proc closeConn(epollFd: cint, fd: cint) =
  discard epoll_ctl(epollFd, EPOLL_CTL_DEL, fd, nil)
  connReset(fd)
  discard c_close(fd)
  gState.activeConnections -= 1

# ── Poller thread ────────────────────────────────────────────────────────

proc pollerThread(arg: pointer) {.thread.} =
  {.cast(gcsafe).}:
    logTs "[poller] Starting — polling " & $gState.cfg.deviceCount &
      " devices every " & $(gState.cfg.pollIntervalMs div 1000) & "s"

    # Initial poll
    pollAll()

    var pollCount: uint32 = 0

    while not gState.shutdown:
      # Sleep for poll interval
      let sleepSec = gState.cfg.pollIntervalMs div 1000
      let sleepNsec = (gState.cfg.pollIntervalMs mod 1000) * 1_000_000
      var ts: Timespec
      ts.tv_sec = posix.Time(sleepSec)
      ts.tv_nsec = clong(sleepNsec)
      var rem: Timespec
      discard nanosleep(ts, rem)

      if gState.shutdown: break

      pollAll()
      pollCount += 1

      if pollCount mod CheckpointIntervalPolls == 0:
        withLock(dbLock):
          discard dbCheckpoint(gState.db)

    logTs "[poller] Stopped"

# ── Main ─────────────────────────────────────────────────────────────────

proc main() =
  gState.cfg = loadConfig()
  gState.shutdown = false
  gState.startedAt = db.nowMillis()

  # Init health
  gState.health = newSeq[DeviceHealth](gState.cfg.deviceCount)
  for i in 0 ..< gState.cfg.deviceCount:
    gState.health[i] = DeviceHealth(
      ip: gState.cfg.devices[i].ip,
      label: gState.cfg.devices[i].label,
      status: hsUnknown,
    )

  # Init DB lock
  initLock(dbLock)

  # Open database
  logTs "[server] Opening database at " & gState.cfg.dbPath
  let rc = sqlite3_open(gState.cfg.dbPath.cstring, addr gState.db)
  if rc != SQLITE_OK:
    logTs "[server] Failed to open database: " & $sqlite3_errmsg(gState.db)
    quit(1)

  if not dbInitialize(gState.db):
    logTs "[server] Failed to initialize database"
    discard sqlite3_close(gState.db)
    quit(1)

  # Signal handling
  c_signal(SIGINT, signalHandler)
  c_signal(SIGTERM, signalHandler)
  c_signal(SIGPIPE, cast[proc(sig: cint) {.noconv.}](SIG_IGN))

  # Start poller thread
  var pollerTid: Thread[pointer]
  createThread(pollerTid, pollerThread, nil)

  # Create listen socket
  let listenFd = cint(posix.socket(AF_INET, SOCK_STREAM, 0))
  if listenFd < 0:
    logTs "[server] socket: " & $strerror(errno)
    quit(1)

  var optVal: cint = 1
  discard posix.setsockopt(SocketHandle(listenFd), SOL_SOCKET, SO_REUSEADDR, addr optVal, SockLen(sizeof(optVal)))

  var saddr: Sockaddr_in
  zeroMem(addr saddr, sizeof(saddr))
  saddr.sin_family = TSa_Family(AF_INET)
  saddr.sin_addr.s_addr = htonl(INADDR_ANY)
  saddr.sin_port = htons(gState.cfg.port)

  if bindSocket(SocketHandle(listenFd), cast[ptr SockAddr](addr saddr), SockLen(sizeof(saddr))) < 0:
    logTs "[server] bind: " & $strerror(errno)
    discard c_close(listenFd)
    quit(1)

  if listen(SocketHandle(listenFd), 128) < 0:
    logTs "[server] listen: " & $strerror(errno)
    discard c_close(listenFd)
    quit(1)

  if not setNonblocking(listenFd):
    logTs "[server] set_nonblocking(listen_fd) failed"
    discard c_close(listenFd)
    quit(1)

  let epollFd = epoll_create1(EPOLL_CLOEXEC)
  if epollFd < 0:
    logTs "[server] epoll_create1: " & $strerror(errno)
    discard c_close(listenFd)
    quit(1)

  var ev: EpollEvent
  ev.events = EPOLLIN or EPOLLET
  ev.data.fd = listenFd
  if epoll_ctl(epollFd, EPOLL_CTL_ADD, listenFd, addr ev) < 0:
    logTs "[server] epoll_ctl(listen): " & $strerror(errno)
    discard c_close(epollFd)
    discard c_close(listenFd)
    quit(1)

  logTs "[server] Listening on http://localhost:" & $gState.cfg.port

  # Zero connection table
  for i in 0 ..< MaxConns:
    connTable[i] = ConnData()

  var events: array[MaxEvents, EpollEvent]

  while not gState.shutdown:
    let nfds = epoll_wait(epollFd, addr events[0], MaxEvents.cint, 1000)
    if nfds < 0:
      if errno == EINTR: continue
      logTs "[server] epoll_wait: " & $strerror(errno)
      break

    for i in 0 ..< nfds:
      let fd = events[i].data.fd
      let evFlags = events[i].events

      # Listen socket: accept loop
      if fd == listenFd:
        while true:
          let clientFd = cint(accept(SocketHandle(listenFd), nil, nil))
          if clientFd < 0:
            if errno == EAGAIN or errno == EWOULDBLOCK: break
            logTs "[server] accept: " & $strerror(errno)
            break

          if clientFd >= MaxConns or gState.activeConnections >= MaxConns:
            logTs "[server] connection limit reached (fd=" & $clientFd &
              ", active=" & $gState.activeConnections & "), rejecting"
            discard c_close(clientFd)
            continue

          if not setNonblocking(clientFd):
            logTs "[server] set_nonblocking(client) failed"
            discard c_close(clientFd)
            continue

          connReset(clientFd)
          connTable[clientFd].phase = cpReading
          connTable[clientFd].acceptedAt = getUnixTime()

          var cev: EpollEvent
          cev.events = EPOLLIN or EPOLLET
          cev.data.fd = clientFd
          if epoll_ctl(epollFd, EPOLL_CTL_ADD, clientFd, addr cev) < 0:
            logTs "[server] epoll_ctl(add client): " & $strerror(errno)
            connReset(clientFd)
            discard c_close(clientFd)
            continue

          gState.activeConnections += 1
        continue

      # Error / hangup
      if (evFlags and (EPOLLERR or EPOLLHUP)) != 0:
        closeConn(epollFd, fd)
        continue

      # Client readable (ET)
      if (evFlags and EPOLLIN) != 0 and fd < MaxConns and
         connTable[fd].phase == cpReading:
        var doClose = false

        while true:
          let space = ReadBufSize - connTable[fd].readPos
          if space == 0:
            doClose = true
            break

          let n = c_read(fd, addr connTable[fd].readBuf[connTable[fd].readPos], csize_t(space))
          if n > 0:
            connTable[fd].readPos += n
            continue
          if n == 0:
            doClose = true
            break
          # n < 0
          if errno == EAGAIN or errno == EWOULDBLOCK:
            break
          doClose = true
          break

        if doClose and connTable[fd].readPos == 0:
          closeConn(epollFd, fd)
          continue

        # Wait for full HTTP header
        if not hasHeaderEnd(connTable[fd].readBuf, connTable[fd].readPos):
          if doClose:
            closeConn(epollFd, fd)
          continue

        # Parse request
        let (parseOk, req) = parseRequestBuf(connTable[fd].readBuf, connTable[fd].readPos)
        if parseOk:
          gState.requestsServed += 1

          connTable[fd].response = routeRequest(req)
          connTable[fd].writePos = 0
          connTable[fd].phase = cpWriting

          var wev: EpollEvent
          wev.events = EPOLLOUT or EPOLLET
          wev.data.fd = fd
          if epoll_ctl(epollFd, EPOLL_CTL_MOD, fd, addr wev) < 0:
            closeConn(epollFd, fd)
            continue
        elif doClose:
          closeConn(epollFd, fd)
        continue

      # Client writable (ET)
      if (evFlags and EPOLLOUT) != 0 and fd < MaxConns and
         connTable[fd].phase == cpWriting:
        while true:
          let remaining = connTable[fd].response.len - connTable[fd].writePos
          if remaining == 0:
            closeConn(epollFd, fd)
            break

          let n = c_write(fd, addr connTable[fd].response[connTable[fd].writePos],
                          csize_t(remaining))
          if n > 0:
            connTable[fd].writePos += n
            continue
          if n < 0:
            if errno == EAGAIN or errno == EWOULDBLOCK:
              break
            closeConn(epollFd, fd)
            break
          # n == 0
          closeConn(epollFd, fd)
          break
        continue

    # Sweep stale connections (Slowloris defense)
    let now = getUnixTime()
    for sfd in 0 ..< MaxConns:
      if connTable[sfd].phase != cpEmpty and
         now - connTable[sfd].acceptedAt > ConnTimeout:
        closeConn(epollFd, sfd.cint)

  # Cleanup
  for fd in 0 ..< MaxConns:
    if connTable[fd].phase != cpEmpty:
      closeConn(epollFd, fd.cint)

  discard c_close(epollFd)
  discard c_close(listenFd)

  gState.shutdown = true
  joinThread(pollerTid)

  discard sqlite3_close(gState.db)
  deinitLock(dbLock)
  logTs "[server] Shut down cleanly"

main()
