package main

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

// estimatedReadingJSONSize is the approximate byte size of one reading JSON object.
// Used for pre-allocating output buffers.
const estimatedReadingJSONSize = 350

// estimatedDeviceJSONSize is the approximate byte size of one device summary JSON object.
const estimatedDeviceJSONSize = 120

const deviceAll = "all"

const defaultLatestCap = 16

type Reading struct {
	DeviceType      string
	DeviceIP        string
	DeviceID        string
	PM02Compensated sql.NullFloat64
	ATMP            sql.NullFloat64
	PM01            sql.NullFloat64
	PM02            sql.NullFloat64
	PM10            sql.NullFloat64
	Wifi            sql.NullInt64
	RCO2            sql.NullInt64
	NOXIndex        sql.NullFloat64
	ATMPCompensated sql.NullFloat64
	RHUM            sql.NullFloat64
	RHUMCompensated sql.NullFloat64
	TVOCIndex       sql.NullFloat64
	Timestamp       int64
	ID              int64
}

type DeviceSummary struct {
	DeviceID     string
	DeviceType   string
	DeviceIP     string
	LastSeen     int64
	ReadingCount int64
}

type ReadingQuery struct {
	Device       string
	From         int64
	To           int64
	Limit        int
	DownsampleMs int64
}

// DB wraps a sql.DB with a dedicated connection and cached prepared statements
// to eliminate connection pool overhead and repeated prepare/finalize CGo calls.
type DB struct {
	pool *sql.DB
	conn *sql.Conn

	// Cached prepared statements on the dedicated connection.
	stmtInsert      *sql.Stmt
	stmtLatest      *sql.Stmt
	stmtDevices     *sql.Stmt
	stmtCount       *sql.Stmt
	stmtReadingsAll *sql.Stmt
	stmtReadingsDev *sql.Stmt
	stmtCountAll    *sql.Stmt
	stmtCountDev    *sql.Stmt
	stmtCheckpoint  *sql.Stmt

	mu sync.Mutex
}

// Package-level SQL loaded from queries.sql (or hardcoded fallbacks).
var (
	queryCols  string
	insertSQL  string
	latestSQL  string
	devicesSQL string
	countSQL   string

	loadQueriesOnce sync.Once
)

var placeholderRe = regexp.MustCompile(`:[a-z][a-z0-9_]*`)

func loadQueries() {
	loadQueriesOnce.Do(func() {
		content, err := os.ReadFile("../queries.sql")
		if err != nil {
			logf("[db] queries.sql not found (%v), using defaults", err)
			setDefaultQueries()
			return
		}
		queries := parseQueriesSQL(string(content))

		if v, ok := queries["reading_columns"]; ok {
			queryCols = v
		} else {
			queryCols = defaultQueryCols
		}

		if v, ok := queries["insert_reading"]; ok {
			insertSQL = convertPlaceholders(v)
		} else {
			insertSQL = defaultInsertSQL
		}

		if v, ok := queries["select_latest"]; ok {
			latestSQL = v
		} else {
			latestSQL = defaultLatestSQL
		}

		if v, ok := queries["select_devices"]; ok {
			devicesSQL = v
		} else {
			devicesSQL = defaultDevicesSQL
		}

		if v, ok := queries["count_readings"]; ok {
			countSQL = v
		} else {
			countSQL = defaultCountSQL
		}

		logf("[db] loaded %d queries from queries.sql", len(queries))
	})
}

func setDefaultQueries() {
	queryCols = defaultQueryCols
	insertSQL = defaultInsertSQL
	latestSQL = defaultLatestSQL
	devicesSQL = defaultDevicesSQL
	countSQL = defaultCountSQL
}

const defaultQueryCols = `id, timestamp, device_id, device_type, device_ip,
pm01, pm02, pm10, pm02_compensated, rco2,
atmp, atmp_compensated, rhum, rhum_compensated,
tvoc_index, nox_index, wifi`

const defaultInsertSQL = `INSERT INTO readings (
	timestamp, device_id, device_type, device_ip,
	pm01, pm02, pm10, pm02_compensated,
	rco2, atmp, atmp_compensated, rhum, rhum_compensated,
	tvoc_index, nox_index, wifi, raw_json
) VALUES (
	?, ?, ?, ?,
	?, ?, ?, ?,
	?, ?, ?, ?, ?,
	?, ?, ?, ?
)`

const defaultLatestSQL = `SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip,
	r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2,
	r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated,
	r.tvoc_index, r.nox_index, r.wifi
	FROM readings r
	INNER JOIN (
		SELECT device_id, MAX(id) as max_id
		FROM readings GROUP BY device_id
	) latest ON r.id = latest.max_id`

const defaultDevicesSQL = `SELECT device_id, device_type, device_ip,
	MAX(timestamp) as last_seen, COUNT(*) as reading_count
	FROM readings GROUP BY device_id ORDER BY device_type`

const defaultCountSQL = `SELECT COUNT(*) FROM readings`

func parseQueriesSQL(content string) map[string]string {
	queries := make(map[string]string)
	var name string
	var lines []string

	for _, line := range strings.Split(content, "\n") {
		trimmed := strings.TrimSpace(line)
		switch {
		case strings.HasPrefix(trimmed, "-- name: "):
			if name != "" {
				queries[name] = strings.TrimRight(strings.TrimSpace(strings.Join(lines, "\n")), ";")
			}
			name = strings.TrimSpace(strings.TrimPrefix(trimmed, "-- name: "))
			lines = nil
		case strings.HasPrefix(trimmed, "--"), trimmed == "":
			// skip comments and blank lines
		default:
			lines = append(lines, trimmed)
		}
	}
	if name != "" {
		queries[name] = strings.TrimRight(strings.TrimSpace(strings.Join(lines, "\n")), ";")
	}
	return queries
}

func convertPlaceholders(sqlStr string) string {
	return placeholderRe.ReplaceAllString(sqlStr, "?")
}

func NowMillis() int64 {
	return time.Now().UnixMilli()
}

// buildReadingsSQL builds the readings query for a given filter variant.
func buildReadingsSQL(withDevice bool) string {
	var b strings.Builder
	b.WriteString("SELECT ")
	b.WriteString(queryCols)
	b.WriteString(" FROM readings WHERE ")
	if withDevice {
		b.WriteString("device_id = ? AND ")
	}
	b.WriteString("timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC LIMIT ?")
	return b.String()
}

// buildCountFilteredSQL builds the filtered count query for a given filter variant.
func buildCountFilteredSQL(withDevice bool) string {
	var b strings.Builder
	b.WriteString("SELECT COUNT(*) FROM readings WHERE ")
	if withDevice {
		b.WriteString("device_id = ? AND ")
	}
	b.WriteString("timestamp >= ? AND timestamp <= ?")
	return b.String()
}

func OpenDB(dbPath string) (*DB, error) {
	loadQueries()

	dsn := dbPath + "?_journal_mode=WAL&_busy_timeout=5000&_foreign_keys=1"
	pool, err := sql.Open("sqlite3", dsn)
	if err != nil {
		return nil, fmt.Errorf("open database: %w", err)
	}
	pool.SetMaxOpenConns(2) // one for dedicated conn, one spare for schema init

	if schemaErr := initSchema(pool); schemaErr != nil {
		pool.Close()
		return nil, schemaErr
	}

	// Grab a dedicated connection from the pool for all subsequent operations.
	conn, err := pool.Conn(context.Background())
	if err != nil {
		pool.Close()
		return nil, fmt.Errorf("acquire dedicated connection: %w", err)
	}

	d := &DB{pool: pool, conn: conn}
	if err := d.prepareStatements(); err != nil {
		conn.Close()
		pool.Close()
		return nil, fmt.Errorf("prepare statements: %w", err)
	}

	return d, nil
}

func (d *DB) prepareStatements() error {
	var err error

	d.stmtInsert, err = d.conn.PrepareContext(context.Background(), insertSQL)
	if err != nil {
		return fmt.Errorf("prepare insert: %w", err)
	}

	d.stmtLatest, err = d.conn.PrepareContext(context.Background(), latestSQL)
	if err != nil {
		return fmt.Errorf("prepare latest: %w", err)
	}

	d.stmtDevices, err = d.conn.PrepareContext(context.Background(), devicesSQL)
	if err != nil {
		return fmt.Errorf("prepare devices: %w", err)
	}

	d.stmtCount, err = d.conn.PrepareContext(context.Background(), countSQL)
	if err != nil {
		return fmt.Errorf("prepare count: %w", err)
	}

	d.stmtReadingsAll, err = d.conn.PrepareContext(context.Background(), buildReadingsSQL(false))
	if err != nil {
		return fmt.Errorf("prepare readings all: %w", err)
	}

	d.stmtReadingsDev, err = d.conn.PrepareContext(context.Background(), buildReadingsSQL(true))
	if err != nil {
		return fmt.Errorf("prepare readings dev: %w", err)
	}

	d.stmtCountAll, err = d.conn.PrepareContext(context.Background(), buildCountFilteredSQL(false))
	if err != nil {
		return fmt.Errorf("prepare count all: %w", err)
	}

	d.stmtCountDev, err = d.conn.PrepareContext(context.Background(), buildCountFilteredSQL(true))
	if err != nil {
		return fmt.Errorf("prepare count dev: %w", err)
	}

	d.stmtCheckpoint, err = d.conn.PrepareContext(context.Background(), "PRAGMA wal_checkpoint(TRUNCATE)")
	if err != nil {
		return fmt.Errorf("prepare checkpoint: %w", err)
	}

	return nil
}

// Close releases the dedicated connection and closes the pool.
func (d *DB) Close() error {
	d.mu.Lock()
	defer d.mu.Unlock()

	// Close prepared statements.
	stmts := []*sql.Stmt{
		d.stmtInsert, d.stmtLatest, d.stmtDevices, d.stmtCount,
		d.stmtReadingsAll, d.stmtReadingsDev, d.stmtCountAll, d.stmtCountDev,
		d.stmtCheckpoint,
	}
	for _, s := range stmts {
		if s != nil {
			s.Close()
		}
	}

	if d.conn != nil {
		d.conn.Close()
	}
	return d.pool.Close()
}

// Pool returns the underlying *sql.DB for operations that need direct pool access
// (e.g., schema init in tests that use raw db.Exec).
func (d *DB) Pool() *sql.DB {
	return d.pool
}

func initSchema(pool *sql.DB) error {
	schema, err := os.ReadFile("../schema.sql")
	if err != nil {
		return fmt.Errorf("read schema: %w", err)
	}
	if _, err := pool.Exec(string(schema)); err != nil {
		return fmt.Errorf("apply schema: %w", err)
	}
	return nil
}

func (d *DB) InsertReading(ip string, data map[string]any) error {
	deviceType := classifyDevice(data)
	serial := getSerial(data)

	rawJSON, err := json.Marshal(data)
	if err != nil {
		return fmt.Errorf("marshal raw json: %w", err)
	}

	d.mu.Lock()
	_, err = d.stmtInsert.Exec(
		NowMillis(), serial, deviceType, ip,
		optFloat(data, "pm01"), optFloat(data, "pm02"),
		optFloat(data, "pm10"), optFloat(data, "pm02Compensated"),
		optInt(data, "rco2"), optFloat(data, "atmp"),
		optFloat(data, "atmpCompensated"), optFloat(data, "rhum"),
		optFloat(data, "rhumCompensated"), optFloat(data, "tvocIndex"),
		optFloat(data, "noxIndex"), optInt(data, "wifi"),
		string(rawJSON),
	)
	d.mu.Unlock()

	if err != nil {
		return fmt.Errorf("insert reading: %w", err)
	}
	return nil
}

func (d *DB) QueryReadings(q ReadingQuery) ([]Reading, error) {
	if q.DownsampleMs > 0 {
		return d.queryDownsampled(q)
	}

	wantDevice := q.Device != "" && q.Device != deviceAll

	// Choose scan capacity: use limit when known and reasonable, else 256.
	scanCap := 256
	if q.Limit > 0 && q.Limit < 10000 {
		scanCap = q.Limit
	}

	var rows *sql.Rows
	var err error

	d.mu.Lock()
	if wantDevice {
		rows, err = d.stmtReadingsDev.Query(q.Device, q.From, q.To, q.Limit)
	} else {
		rows, err = d.stmtReadingsAll.Query(q.From, q.To, q.Limit)
	}
	d.mu.Unlock()

	if err != nil {
		return nil, fmt.Errorf("query readings: %w", err)
	}
	defer rows.Close()

	return scanReadings(rows, scanCap)
}

func (d *DB) queryDownsampled(q ReadingQuery) ([]Reading, error) {
	wantDevice := q.Device != "" && q.Device != deviceAll
	bucketMs := q.DownsampleMs

	var b strings.Builder
	var args []any

	// bucket_ms is a trusted constant from DownsampleMap, safe to interpolate
	fmt.Fprintf(&b, "SELECT (timestamp / %d) * %d AS timestamp, ", bucketMs, bucketMs)
	b.WriteString("device_id, device_type, device_ip, ")
	b.WriteString("AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10, ")
	b.WriteString("AVG(pm02_compensated) AS pm02_compensated, ")
	b.WriteString("CAST(AVG(rco2) AS INTEGER) AS rco2, ")
	b.WriteString("AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated, ")
	b.WriteString("AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated, ")
	b.WriteString("AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index, ")
	b.WriteString("CAST(AVG(wifi) AS INTEGER) AS wifi ")
	b.WriteString("FROM readings WHERE ")

	if wantDevice {
		b.WriteString("device_id = ? AND ")
		args = append(args, q.Device)
	}

	b.WriteString("timestamp >= ? AND timestamp <= ? ")
	args = append(args, q.From, q.To)

	fmt.Fprintf(&b, "GROUP BY (timestamp / %d), device_id ORDER BY timestamp ASC", bucketMs)

	if q.Limit > 0 {
		b.WriteString(" LIMIT ?")
		args = append(args, q.Limit)
	}

	d.mu.Lock()
	rows, err := d.conn.QueryContext(context.Background(), b.String(), args...)
	d.mu.Unlock()

	if err != nil {
		return nil, fmt.Errorf("query downsampled readings: %w", err)
	}
	defer rows.Close()

	return scanDownsampledReadings(rows)
}

func (d *DB) GetLatestReadings() ([]Reading, error) {
	d.mu.Lock()
	rows, err := d.stmtLatest.Query()
	d.mu.Unlock()

	if err != nil {
		return nil, fmt.Errorf("get latest readings: %w", err)
	}
	defer rows.Close()

	return scanReadings(rows, defaultLatestCap)
}

func (d *DB) GetDevices() ([]DeviceSummary, error) {
	d.mu.Lock()
	rows, err := d.stmtDevices.Query()
	d.mu.Unlock()

	if err != nil {
		return nil, fmt.Errorf("get devices: %w", err)
	}
	defer rows.Close()

	var devices []DeviceSummary
	for rows.Next() {
		var ds DeviceSummary
		if err := rows.Scan(&ds.DeviceID, &ds.DeviceType, &ds.DeviceIP,
			&ds.LastSeen, &ds.ReadingCount); err != nil {
			return nil, fmt.Errorf("scan device: %w", err)
		}
		devices = append(devices, ds)
	}
	return devices, rows.Err()
}

func (d *DB) GetReadingsCount() (int64, error) {
	var count int64

	d.mu.Lock()
	err := d.stmtCount.QueryRow().Scan(&count)
	d.mu.Unlock()

	return count, err
}

func (d *DB) GetFilteredCount(from, to int64, device string) (int64, error) {
	wantDevice := device != "" && device != deviceAll

	var count int64
	var err error

	d.mu.Lock()
	if wantDevice {
		err = d.stmtCountDev.QueryRow(device, from, to).Scan(&count)
	} else {
		err = d.stmtCountAll.QueryRow(from, to).Scan(&count)
	}
	d.mu.Unlock()

	return count, err
}

func (d *DB) Checkpoint() error {
	d.mu.Lock()
	_, err := d.stmtCheckpoint.Exec()
	d.mu.Unlock()
	return err
}

// Exec runs a raw SQL statement on the dedicated connection (for tests).
func (d *DB) Exec(query string, args ...any) (sql.Result, error) {
	d.mu.Lock()
	result, err := d.conn.ExecContext(context.Background(), query, args...)
	d.mu.Unlock()
	return result, err
}

// QueryRow runs a raw SQL query on the dedicated connection (for tests).
func (d *DB) QueryRow(query string, args ...any) *sql.Row {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.conn.QueryRowContext(context.Background(), query, args...)
}

func ReadingToJSON(r *Reading) map[string]any {
	m := map[string]any{
		"timestamp":        r.Timestamp,
		"device_id":        r.DeviceID,
		"device_type":      r.DeviceType,
		"device_ip":        r.DeviceIP,
		"pm01":             nullFloat(r.PM01),
		"pm02":             nullFloat(r.PM02),
		"pm10":             nullFloat(r.PM10),
		"pm02_compensated": nullFloat(r.PM02Compensated),
		"rco2":             nullInt(r.RCO2),
		"atmp":             nullFloat(r.ATMP),
		"atmp_compensated": nullFloat(r.ATMPCompensated),
		"rhum":             nullFloat(r.RHUM),
		"rhum_compensated": nullFloat(r.RHUMCompensated),
		"tvoc_index":       nullFloat(r.TVOCIndex),
		"nox_index":        nullFloat(r.NOXIndex),
		"wifi":             nullInt(r.Wifi),
	}
	// Downsampled rows have ID=0 (no single row identity); omit id field.
	if r.ID != 0 {
		m["id"] = r.ID
	}
	return m
}

func DeviceSummaryToJSON(d *DeviceSummary) map[string]any {
	return map[string]any{
		"device_id":     d.DeviceID,
		"device_type":   d.DeviceType,
		"device_ip":     d.DeviceIP,
		"last_seen":     d.LastSeen,
		"reading_count": d.ReadingCount,
	}
}

// bufPool reuses bytes.Buffer for JSON serialization to reduce allocations.
var bufPool = sync.Pool{
	New: func() any {
		return bytes.NewBuffer(make([]byte, 0, 4096))
	},
}

// GetBuffer returns a bytes.Buffer from the pool, pre-grown to the requested capacity.
func GetBuffer(capacity int) *bytes.Buffer {
	buf, ok := bufPool.Get().(*bytes.Buffer)
	if !ok {
		buf = bytes.NewBuffer(make([]byte, 0, capacity))
	}
	buf.Reset()
	buf.Grow(capacity)
	return buf
}

// PutBuffer returns a bytes.Buffer to the pool.
func PutBuffer(buf *bytes.Buffer) {
	// Don't pool excessively large buffers (>1MB) to avoid memory bloat.
	if buf.Cap() <= 1024*1024 {
		bufPool.Put(buf)
	}
}

// WriteReadingsJSON writes a JSON array of readings directly to buf,
// bypassing map[string]any and encoding/json for maximum throughput.
func WriteReadingsJSON(buf *bytes.Buffer, readings []Reading) {
	buf.WriteByte('[')
	for i := range readings {
		if i > 0 {
			buf.WriteByte(',')
		}
		writeReadingJSON(buf, &readings[i])
	}
	buf.WriteByte(']')
}

// writeReadingJSON writes a single reading as a JSON object to buf.
func writeReadingJSON(buf *bytes.Buffer, r *Reading) {
	buf.WriteByte('{')
	// Downsampled rows have ID=0 (no single row identity); omit id field.
	if r.ID != 0 {
		buf.WriteString(`"id":`)
		buf.WriteString(strconv.FormatInt(r.ID, 10))
		buf.WriteByte(',')
	}
	buf.WriteString(`"timestamp":`)
	buf.WriteString(strconv.FormatInt(r.Timestamp, 10))
	buf.WriteString(`,"device_id":`)
	writeJSONString(buf, r.DeviceID)
	buf.WriteString(`,"device_type":`)
	writeJSONString(buf, r.DeviceType)
	buf.WriteString(`,"device_ip":`)
	writeJSONString(buf, r.DeviceIP)
	buf.WriteString(`,"pm01":`)
	writeNullFloat(buf, r.PM01)
	buf.WriteString(`,"pm02":`)
	writeNullFloat(buf, r.PM02)
	buf.WriteString(`,"pm10":`)
	writeNullFloat(buf, r.PM10)
	buf.WriteString(`,"pm02_compensated":`)
	writeNullFloat(buf, r.PM02Compensated)
	buf.WriteString(`,"rco2":`)
	writeNullInt(buf, r.RCO2)
	buf.WriteString(`,"atmp":`)
	writeNullFloat(buf, r.ATMP)
	buf.WriteString(`,"atmp_compensated":`)
	writeNullFloat(buf, r.ATMPCompensated)
	buf.WriteString(`,"rhum":`)
	writeNullFloat(buf, r.RHUM)
	buf.WriteString(`,"rhum_compensated":`)
	writeNullFloat(buf, r.RHUMCompensated)
	buf.WriteString(`,"tvoc_index":`)
	writeNullFloat(buf, r.TVOCIndex)
	buf.WriteString(`,"nox_index":`)
	writeNullFloat(buf, r.NOXIndex)
	buf.WriteString(`,"wifi":`)
	writeNullInt(buf, r.Wifi)
	buf.WriteByte('}')
}

// WriteDevicesJSON writes a JSON array of device summaries directly to buf.
func WriteDevicesJSON(buf *bytes.Buffer, devices []DeviceSummary) {
	buf.WriteByte('[')
	for i := range devices {
		if i > 0 {
			buf.WriteByte(',')
		}
		writeDeviceSummaryJSON(buf, &devices[i])
	}
	buf.WriteByte(']')
}

// writeDeviceSummaryJSON writes a single device summary as a JSON object to buf.
func writeDeviceSummaryJSON(buf *bytes.Buffer, ds *DeviceSummary) {
	buf.WriteString(`{"device_id":`)
	writeJSONString(buf, ds.DeviceID)
	buf.WriteString(`,"device_type":`)
	writeJSONString(buf, ds.DeviceType)
	buf.WriteString(`,"device_ip":`)
	writeJSONString(buf, ds.DeviceIP)
	buf.WriteString(`,"last_seen":`)
	buf.WriteString(strconv.FormatInt(ds.LastSeen, 10))
	buf.WriteString(`,"reading_count":`)
	buf.WriteString(strconv.FormatInt(ds.ReadingCount, 10))
	buf.WriteByte('}')
}

const maxASCIIControl = 0x1f // highest ASCII control character

// writeJSONString writes a JSON-escaped string (with quotes) to buf.
func writeJSONString(buf *bytes.Buffer, s string) {
	buf.WriteByte('"')
	for i := range len(s) {
		c := s[i]
		switch c {
		case '"':
			buf.WriteString(`\"`)
		case '\\':
			buf.WriteString(`\\`)
		case '\n':
			buf.WriteString(`\n`)
		case '\r':
			buf.WriteString(`\r`)
		case '\t':
			buf.WriteString(`\t`)
		case '\b':
			buf.WriteString(`\b`)
		case '\f':
			buf.WriteString(`\f`)
		default:
			if c <= maxASCIIControl {
				// Control character — write as \u00XX
				buf.WriteString(`\u00`)
				buf.WriteByte("0123456789abcdef"[c>>4])
				buf.WriteByte("0123456789abcdef"[c&0xf])
			} else {
				buf.WriteByte(c)
			}
		}
	}
	buf.WriteByte('"')
}

// writeNullFloat writes a JSON float or null to buf.
func writeNullFloat(buf *bytes.Buffer, nf sql.NullFloat64) {
	if !nf.Valid {
		buf.WriteString("null")
		return
	}
	buf.WriteString(strconv.FormatFloat(nf.Float64, 'f', -1, 64))
}

// writeNullInt writes a JSON integer or null to buf.
func writeNullInt(buf *bytes.Buffer, ni sql.NullInt64) {
	if !ni.Valid {
		buf.WriteString("null")
		return
	}
	buf.WriteString(strconv.FormatInt(ni.Int64, 10))
}

// --- constants ---

const (
	deviceTypeIndoor  = "indoor"
	deviceTypeOutdoor = "outdoor"
	valueNA           = "N/A"
)

// --- helpers ---

func scanReadings(rows *sql.Rows, initCap int) ([]Reading, error) {
	readings := make([]Reading, 0, initCap)
	for rows.Next() {
		var r Reading
		if err := rows.Scan(
			&r.ID, &r.Timestamp, &r.DeviceID, &r.DeviceType, &r.DeviceIP,
			&r.PM01, &r.PM02, &r.PM10, &r.PM02Compensated,
			&r.RCO2, &r.ATMP, &r.ATMPCompensated,
			&r.RHUM, &r.RHUMCompensated,
			&r.TVOCIndex, &r.NOXIndex, &r.Wifi,
		); err != nil {
			return nil, fmt.Errorf("scan reading: %w", err)
		}
		readings = append(readings, r)
	}
	return readings, rows.Err()
}

func scanDownsampledReadings(rows *sql.Rows) ([]Reading, error) {
	readings := make([]Reading, 0, 256)
	for rows.Next() {
		var r Reading
		// Downsampled query omits id column; ID stays 0 (zero value).
		if err := rows.Scan(
			&r.Timestamp, &r.DeviceID, &r.DeviceType, &r.DeviceIP,
			&r.PM01, &r.PM02, &r.PM10, &r.PM02Compensated,
			&r.RCO2, &r.ATMP, &r.ATMPCompensated,
			&r.RHUM, &r.RHUMCompensated,
			&r.TVOCIndex, &r.NOXIndex, &r.Wifi,
		); err != nil {
			return nil, fmt.Errorf("scan downsampled reading: %w", err)
		}
		readings = append(readings, r)
	}
	return readings, rows.Err()
}

func classifyDevice(data map[string]any) string {
	model, ok := data["model"].(string)
	if ok && len(model) >= 2 && model[0] == 'I' && model[1] == '-' {
		return deviceTypeIndoor
	}
	return deviceTypeOutdoor
}

func getSerial(data map[string]any) string {
	serial, ok := data["serialno"].(string)
	if ok && serial != "" {
		return serial
	}
	return "unknown"
}

func optFloat(data map[string]any, key string) any {
	v, ok := data[key]
	if !ok {
		return nil
	}
	switch n := v.(type) {
	case float64:
		return n
	case int:
		return float64(n)
	default:
		return nil
	}
}

func optInt(data map[string]any, key string) any {
	v, ok := data[key]
	if !ok {
		return nil
	}
	switch n := v.(type) {
	case float64:
		return int64(n)
	case int:
		return int64(n)
	default:
		return nil
	}
}

func nullFloat(nf sql.NullFloat64) any {
	if nf.Valid {
		return nf.Float64
	}
	return nil
}

func nullInt(ni sql.NullInt64) any {
	if ni.Valid {
		return ni.Int64
	}
	return nil
}

// logInsertResult logs a summary of a successful reading insertion.
func logInsertResult(label, ip string, data map[string]any) {
	pm02s := valueNA
	rco2s := valueNA
	atmps := valueNA

	if v, ok := data["pm02"].(float64); ok {
		pm02s = fmt.Sprintf("%.2f", v)
	}
	if v, ok := data["rco2"].(float64); ok {
		rco2s = strconv.FormatInt(int64(v), 10)
	}
	if v, ok := data["atmp"].(float64); ok {
		atmps = fmt.Sprintf("%.1f", v)
	}

	logf("[poller] %s (%s): OK — PM2.5=%s, CO2=%s, T=%s°C", label, ip, pm02s, rco2s, atmps)
}
