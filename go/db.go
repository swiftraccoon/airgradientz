package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

type Reading struct {
	ID              int64
	Timestamp       int64
	DeviceID        string
	DeviceType      string
	DeviceIP        string
	PM01            sql.NullFloat64
	PM02            sql.NullFloat64
	PM10            sql.NullFloat64
	PM02Compensated sql.NullFloat64
	RCO2            sql.NullInt64
	ATMP            sql.NullFloat64
	ATMPCompensated sql.NullFloat64
	RHUM            sql.NullFloat64
	RHUMCompensated sql.NullFloat64
	TVOCIndex       sql.NullFloat64
	NOXIndex        sql.NullFloat64
	Wifi            sql.NullInt64
}

type DeviceSummary struct {
	DeviceID     string
	DeviceType   string
	DeviceIP     string
	LastSeen     int64
	ReadingCount int64
}

type ReadingQuery struct {
	Device string
	From   int64
	To     int64
	Limit  int
}

const queryCols = `id, timestamp, device_id, device_type, device_ip,
	pm01, pm02, pm10, pm02_compensated, rco2,
	atmp, atmp_compensated, rhum, rhum_compensated,
	tvoc_index, nox_index, wifi`

func NowMillis() int64 {
	return time.Now().UnixMilli()
}

func OpenDB(dbPath string) (*sql.DB, error) {
	dsn := dbPath + "?_journal_mode=WAL&_busy_timeout=5000&_foreign_keys=1"
	db, err := sql.Open("sqlite3", dsn)
	if err != nil {
		return nil, fmt.Errorf("open database: %w", err)
	}
	db.SetMaxOpenConns(1)

	if err := InitDB(db); err != nil {
		db.Close()
		return nil, err
	}

	return db, nil
}

func InitDB(db *sql.DB) error {
	schema, err := os.ReadFile("../schema.sql")
	if err != nil {
		return fmt.Errorf("read schema: %w", err)
	}
	if _, err := db.Exec(string(schema)); err != nil {
		return fmt.Errorf("apply schema: %w", err)
	}
	return nil
}

func InsertReading(db *sql.DB, ip string, data map[string]any) error {
	deviceType := classifyDevice(data)
	serial := getSerial(data)

	rawJSON, err := json.Marshal(data)
	if err != nil {
		return fmt.Errorf("marshal raw json: %w", err)
	}

	const insertSQL = `INSERT INTO readings (
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

	_, err = db.Exec(insertSQL,
		NowMillis(), serial, deviceType, ip,
		optFloat(data, "pm01"), optFloat(data, "pm02"),
		optFloat(data, "pm10"), optFloat(data, "pm02Compensated"),
		optInt(data, "rco2"), optFloat(data, "atmp"),
		optFloat(data, "atmpCompensated"), optFloat(data, "rhum"),
		optFloat(data, "rhumCompensated"), optFloat(data, "tvocIndex"),
		optFloat(data, "noxIndex"), optInt(data, "wifi"),
		string(rawJSON),
	)
	if err != nil {
		return fmt.Errorf("insert reading: %w", err)
	}
	return nil
}

func QueryReadings(db *sql.DB, q ReadingQuery) ([]Reading, error) {
	wantDevice := q.Device != "" && q.Device != "all"

	var b strings.Builder
	var args []any

	b.WriteString("SELECT ")
	b.WriteString(queryCols)
	b.WriteString(" FROM readings WHERE ")

	if wantDevice {
		b.WriteString("device_id = ? AND ")
		args = append(args, q.Device)
	}

	b.WriteString("timestamp >= ? AND timestamp <= ? ORDER BY timestamp ASC")
	args = append(args, q.From, q.To)

	if q.Limit > 0 {
		b.WriteString(" LIMIT ?")
		args = append(args, q.Limit)
	}

	rows, err := db.Query(b.String(), args...)
	if err != nil {
		return nil, fmt.Errorf("query readings: %w", err)
	}
	defer rows.Close()

	return scanReadings(rows)
}

func GetLatestReadings(db *sql.DB) ([]Reading, error) {
	const latestSQL = `SELECT r.id, r.timestamp, r.device_id, r.device_type, r.device_ip,
		r.pm01, r.pm02, r.pm10, r.pm02_compensated, r.rco2,
		r.atmp, r.atmp_compensated, r.rhum, r.rhum_compensated,
		r.tvoc_index, r.nox_index, r.wifi
		FROM readings r
		INNER JOIN (
			SELECT device_id, MAX(id) as max_id
			FROM readings GROUP BY device_id
		) latest ON r.id = latest.max_id`

	rows, err := db.Query(latestSQL)
	if err != nil {
		return nil, fmt.Errorf("get latest readings: %w", err)
	}
	defer rows.Close()

	return scanReadings(rows)
}

func GetDevices(db *sql.DB) ([]DeviceSummary, error) {
	const devicesSQL = `SELECT device_id, device_type, device_ip,
		MAX(timestamp) as last_seen, COUNT(*) as reading_count
		FROM readings GROUP BY device_id ORDER BY device_type`

	rows, err := db.Query(devicesSQL)
	if err != nil {
		return nil, fmt.Errorf("get devices: %w", err)
	}
	defer rows.Close()

	var devices []DeviceSummary
	for rows.Next() {
		var d DeviceSummary
		if err := rows.Scan(&d.DeviceID, &d.DeviceType, &d.DeviceIP,
			&d.LastSeen, &d.ReadingCount); err != nil {
			return nil, fmt.Errorf("scan device: %w", err)
		}
		devices = append(devices, d)
	}
	return devices, rows.Err()
}

func GetReadingsCount(db *sql.DB) (int64, error) {
	var count int64
	err := db.QueryRow("SELECT COUNT(*) FROM readings").Scan(&count)
	return count, err
}

func Checkpoint(db *sql.DB) error {
	_, err := db.Exec("PRAGMA wal_checkpoint(TRUNCATE)")
	return err
}

func ReadingToJSON(r *Reading) map[string]any {
	return map[string]any{
		"id":                r.ID,
		"timestamp":         r.Timestamp,
		"device_id":         r.DeviceID,
		"device_type":       r.DeviceType,
		"device_ip":         r.DeviceIP,
		"pm01":              nullFloat(r.PM01),
		"pm02":              nullFloat(r.PM02),
		"pm10":              nullFloat(r.PM10),
		"pm02_compensated":  nullFloat(r.PM02Compensated),
		"rco2":              nullInt(r.RCO2),
		"atmp":              nullFloat(r.ATMP),
		"atmp_compensated":  nullFloat(r.ATMPCompensated),
		"rhum":              nullFloat(r.RHUM),
		"rhum_compensated":  nullFloat(r.RHUMCompensated),
		"tvoc_index":        nullFloat(r.TVOCIndex),
		"nox_index":         nullFloat(r.NOXIndex),
		"wifi":              nullInt(r.Wifi),
	}
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

// --- helpers ---

func scanReadings(rows *sql.Rows) ([]Reading, error) {
	var readings []Reading
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

func classifyDevice(data map[string]any) string {
	model, ok := data["model"].(string)
	if ok && len(model) >= 2 && model[0] == 'I' && model[1] == '-' {
		return "indoor"
	}
	return "outdoor"
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
	pm02s := "N/A"
	rco2s := "N/A"
	atmps := "N/A"

	if v, ok := data["pm02"].(float64); ok {
		pm02s = fmt.Sprintf("%.2f", v)
	}
	if v, ok := data["rco2"].(float64); ok {
		rco2s = fmt.Sprintf("%d", int64(v))
	}
	if v, ok := data["atmp"].(float64); ok {
		atmps = fmt.Sprintf("%.1f", v)
	}

	log.Printf("[poller] %s (%s): OK — PM2.5=%s, CO2=%s, T=%s°C", label, ip, pm02s, rco2s, atmps)
}
