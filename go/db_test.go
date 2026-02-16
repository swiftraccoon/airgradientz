package main

import (
	"database/sql"
	"os"
	"path/filepath"
	"testing"
)

const testDeviceID = "abc123"

func openTestDB(t *testing.T) *sql.DB {
	t.Helper()
	tmp := t.TempDir()
	dbPath := filepath.Join(tmp, "test.db")
	db, err := OpenDB(dbPath)
	if err != nil {
		t.Fatalf("OpenDB: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	return db
}

var indoorFull = map[string]any{
	"serialno":         testDeviceID,
	"model":            "I-9PSL",
	"pm01":             float64(5),
	"pm02":             float64(12),
	"pm10":             float64(18),
	"pm02Compensated":  float64(10),
	"rco2":             float64(450),
	"atmp":             float64(22.5),
	"atmpCompensated":  float64(21.0),
	"rhum":             float64(55.0),
	"rhumCompensated":  float64(53.0),
	"tvocIndex":        float64(100),
	"noxIndex":         float64(1),
	"wifi":             float64(-42),
}

var outdoorFull = map[string]any{
	"serialno":         "xyz789",
	"model":            "O-1PST",
	"pm01":             float64(8),
	"pm02":             float64(15),
	"pm10":             float64(22),
	"pm02Compensated":  float64(14),
	"atmp":             float64(18.5),
	"atmpCompensated":  float64(17.0),
	"rhum":             float64(65.0),
	"rhumCompensated":  float64(63.0),
	"wifi":             float64(-55),
}

var afterBoot = map[string]any{
	"serialno": "boot1",
	"model":    "I-9PSL",
}

var zeroCompensated = map[string]any{
	"serialno":         "zero1",
	"model":            "I-9PSL",
	"pm02":             float64(0),
	"pm02Compensated":  float64(0),
	"rco2":             float64(0),
	"atmp":             float64(0),
	"atmpCompensated":  float64(0),
	"rhum":             float64(0),
	"rhumCompensated":  float64(0),
}

func TestInsertAndQueryReading(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatalf("InsertReading: %v", err)
	}

	readings, err := QueryReadings(db, ReadingQuery{
		Device: "all",
		From:   0,
		To:     NowMillis() + 1000,
		Limit:  100,
	})
	if err != nil {
		t.Fatalf("QueryReadings: %v", err)
	}
	if len(readings) != 1 {
		t.Fatalf("expected 1 reading, got %d", len(readings))
	}

	r := readings[0]
	if r.DeviceID != testDeviceID {
		t.Errorf("device_id = %q, want %q", r.DeviceID, testDeviceID)
	}
	if r.DeviceType != deviceTypeIndoor {
		t.Errorf("device_type = %q, want %q", r.DeviceType, deviceTypeIndoor)
	}
	if r.DeviceIP != "192.168.1.1" {
		t.Errorf("device_ip = %q, want %q", r.DeviceIP, "192.168.1.1")
	}
	if !r.PM02.Valid || r.PM02.Float64 != 12 {
		t.Errorf("pm02 = %v, want 12", r.PM02)
	}
	if !r.RCO2.Valid || r.RCO2.Int64 != 450 {
		t.Errorf("rco2 = %v, want 450", r.RCO2)
	}
	if !r.ATMP.Valid || r.ATMP.Float64 != 22.5 {
		t.Errorf("atmp = %v, want 22.5", r.ATMP)
	}
}

func TestClassifyDeviceType(t *testing.T) {
	tests := []struct {
		name   string
		data   map[string]any
		want   string
	}{
		{"indoor I-9PSL", map[string]any{"model": "I-9PSL"}, deviceTypeIndoor},
		{"outdoor O-1PST", map[string]any{"model": "O-1PST"}, deviceTypeOutdoor},
		{"no model", map[string]any{}, deviceTypeOutdoor},
		{"short model", map[string]any{"model": "I"}, deviceTypeOutdoor},
		{"non-string model", map[string]any{"model": 42}, deviceTypeOutdoor},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := classifyDevice(tt.data)
			if got != tt.want {
				t.Errorf("classifyDevice = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestGetSerial(t *testing.T) {
	tests := []struct {
		name string
		data map[string]any
		want string
	}{
		{"present", map[string]any{"serialno": "abc"}, "abc"},
		{"missing", map[string]any{}, "unknown"},
		{"empty", map[string]any{"serialno": ""}, "unknown"},
		{"non-string", map[string]any{"serialno": 42}, "unknown"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := getSerial(tt.data)
			if got != tt.want {
				t.Errorf("getSerial = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestNullFieldsAfterBoot(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", afterBoot); err != nil {
		t.Fatalf("InsertReading: %v", err)
	}

	readings, err := QueryReadings(db, ReadingQuery{
		Device: "all", From: 0, To: NowMillis() + 1000, Limit: 100,
	})
	if err != nil {
		t.Fatalf("QueryReadings: %v", err)
	}
	if len(readings) != 1 {
		t.Fatalf("expected 1 reading, got %d", len(readings))
	}

	r := readings[0]
	if r.PM01.Valid {
		t.Error("pm01 should be NULL after boot")
	}
	if r.PM02.Valid {
		t.Error("pm02 should be NULL after boot")
	}
	if r.RCO2.Valid {
		t.Error("rco2 should be NULL after boot")
	}
	if r.ATMP.Valid {
		t.Error("atmp should be NULL after boot")
	}
	if r.RHUM.Valid {
		t.Error("rhum should be NULL after boot")
	}
	if r.TVOCIndex.Valid {
		t.Error("tvoc_index should be NULL after boot")
	}
	if r.NOXIndex.Valid {
		t.Error("nox_index should be NULL after boot")
	}
	if r.Wifi.Valid {
		t.Error("wifi should be NULL after boot")
	}
}

func TestZeroCompensatedValues(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", zeroCompensated); err != nil {
		t.Fatalf("InsertReading: %v", err)
	}

	readings, err := QueryReadings(db, ReadingQuery{
		Device: "all", From: 0, To: NowMillis() + 1000, Limit: 100,
	})
	if err != nil {
		t.Fatalf("QueryReadings: %v", err)
	}
	r := readings[0]

	if !r.PM02.Valid || r.PM02.Float64 != 0 {
		t.Errorf("pm02 should be 0 (valid), got %v", r.PM02)
	}
	if !r.PM02Compensated.Valid || r.PM02Compensated.Float64 != 0 {
		t.Errorf("pm02_compensated should be 0 (valid), got %v", r.PM02Compensated)
	}
	if !r.RCO2.Valid || r.RCO2.Int64 != 0 {
		t.Errorf("rco2 should be 0 (valid), got %v", r.RCO2)
	}
	if !r.ATMP.Valid || r.ATMP.Float64 != 0 {
		t.Errorf("atmp should be 0 (valid), got %v", r.ATMP)
	}
}

func TestDeviceFiltering(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	t.Run("filter by device", func(t *testing.T) {
		readings, err := QueryReadings(db, ReadingQuery{
			Device: testDeviceID, From: 0, To: NowMillis() + 1000, Limit: 100,
		})
		if err != nil {
			t.Fatal(err)
		}
		if len(readings) != 1 {
			t.Fatalf("expected 1, got %d", len(readings))
		}
		if readings[0].DeviceID != testDeviceID {
			t.Errorf("device_id = %q, want abc123", readings[0].DeviceID)
		}
	})

	t.Run("all devices", func(t *testing.T) {
		readings, err := QueryReadings(db, ReadingQuery{
			Device: "all", From: 0, To: NowMillis() + 1000, Limit: 100,
		})
		if err != nil {
			t.Fatal(err)
		}
		if len(readings) != 2 {
			t.Fatalf("expected 2, got %d", len(readings))
		}
	})

	t.Run("empty device means all", func(t *testing.T) {
		readings, err := QueryReadings(db, ReadingQuery{
			Device: "", From: 0, To: NowMillis() + 1000, Limit: 100,
		})
		if err != nil {
			t.Fatal(err)
		}
		if len(readings) != 2 {
			t.Fatalf("expected 2, got %d", len(readings))
		}
	})

	t.Run("nonexistent device", func(t *testing.T) {
		readings, err := QueryReadings(db, ReadingQuery{
			Device: "nonexistent", From: 0, To: NowMillis() + 1000, Limit: 100,
		})
		if err != nil {
			t.Fatal(err)
		}
		if len(readings) != 0 {
			t.Fatalf("expected 0, got %d", len(readings))
		}
	})
}

func TestQueryLimit(t *testing.T) {
	db := openTestDB(t)

	for i := 0; i < 5; i++ {
		if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
			t.Fatal(err)
		}
	}

	readings, err := QueryReadings(db, ReadingQuery{
		Device: "all", From: 0, To: NowMillis() + 1000, Limit: 3,
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(readings) != 3 {
		t.Fatalf("expected 3 (limit), got %d", len(readings))
	}
}

func TestQueryTimeRange(t *testing.T) {
	db := openTestDB(t)

	now := NowMillis()

	// Insert directly with a known timestamp
	_, err := db.Exec(`INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json)
		VALUES (?, 'dev1', 'indoor', '1.1.1.1', '{}')`, now-10000)
	if err != nil {
		t.Fatal(err)
	}
	_, err = db.Exec(`INSERT INTO readings (timestamp, device_id, device_type, device_ip, raw_json)
		VALUES (?, 'dev1', 'indoor', '1.1.1.1', '{}')`, now)
	if err != nil {
		t.Fatal(err)
	}

	readings, err := QueryReadings(db, ReadingQuery{
		Device: "all", From: now - 5000, To: now + 1000, Limit: 100,
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(readings) != 1 {
		t.Fatalf("expected 1 in range, got %d", len(readings))
	}
}

func TestGetLatestReadings(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	readings, err := GetLatestReadings(db)
	if err != nil {
		t.Fatal(err)
	}
	if len(readings) != 2 {
		t.Fatalf("expected 2 latest (one per device), got %d", len(readings))
	}
}

func TestGetDevices(t *testing.T) {
	db := openTestDB(t)

	for i := 0; i < 3; i++ {
		if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
			t.Fatal(err)
		}
	}
	if err := InsertReading(db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	devices, err := GetDevices(db)
	if err != nil {
		t.Fatal(err)
	}
	if len(devices) != 2 {
		t.Fatalf("expected 2 devices, got %d", len(devices))
	}

	found := false
	for _, d := range devices {
		if d.DeviceID == testDeviceID {
			found = true
			if d.ReadingCount != 3 {
				t.Errorf("reading_count = %d, want 3", d.ReadingCount)
			}
			if d.DeviceType != deviceTypeIndoor {
				t.Errorf("device_type = %q, want indoor", d.DeviceType)
			}
		}
	}
	if !found {
		t.Error("device abc123 not found")
	}
}

func TestGetReadingsCount(t *testing.T) {
	db := openTestDB(t)

	count, err := GetReadingsCount(db)
	if err != nil {
		t.Fatal(err)
	}
	if count != 0 {
		t.Errorf("expected 0, got %d", count)
	}

	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	count, err = GetReadingsCount(db)
	if err != nil {
		t.Fatal(err)
	}
	if count != 2 {
		t.Errorf("expected 2, got %d", count)
	}
}

func TestCheckpoint(t *testing.T) {
	db := openTestDB(t)
	if err := Checkpoint(db); err != nil {
		t.Errorf("Checkpoint: %v", err)
	}
}

func TestReadingToJSON(t *testing.T) {
	r := Reading{
		ID:        1,
		Timestamp: 1700000000000,
		DeviceID:  testDeviceID,
		DeviceType: deviceTypeIndoor,
		DeviceIP:  "192.168.1.1",
		PM02:      sql.NullFloat64{Float64: 12, Valid: true},
		RCO2:      sql.NullInt64{Int64: 450, Valid: true},
		ATMP:      sql.NullFloat64{Float64: 22.5, Valid: true},
	}

	j := ReadingToJSON(&r)

	if j["id"] != int64(1) {
		t.Errorf("id = %v", j["id"])
	}
	if j["device_id"] != testDeviceID {
		t.Errorf("device_id = %v", j["device_id"])
	}
	if j["pm02"] != 12.0 {
		t.Errorf("pm02 = %v", j["pm02"])
	}
	if j["rco2"] != int64(450) {
		t.Errorf("rco2 = %v", j["rco2"])
	}
	if j["pm01"] != nil {
		t.Errorf("pm01 should be nil, got %v", j["pm01"])
	}
	if j["wifi"] != nil {
		t.Errorf("wifi should be nil, got %v", j["wifi"])
	}
}

func TestDeviceSummaryToJSON(t *testing.T) {
	d := DeviceSummary{
		DeviceID:     testDeviceID,
		DeviceType:   deviceTypeIndoor,
		DeviceIP:     "192.168.1.1",
		LastSeen:     1700000000000,
		ReadingCount: 42,
	}

	j := DeviceSummaryToJSON(&d)

	if j["device_id"] != testDeviceID {
		t.Errorf("device_id = %v", j["device_id"])
	}
	if j["reading_count"] != int64(42) {
		t.Errorf("reading_count = %v", j["reading_count"])
	}
}

func TestOptFloat(t *testing.T) {
	tests := []struct {
		name string
		data map[string]any
		key  string
		want any
	}{
		{"float64", map[string]any{"k": float64(1.5)}, "k", float64(1.5)},
		{"int", map[string]any{"k": int(3)}, "k", float64(3)},
		{"missing", map[string]any{}, "k", nil},
		{"string", map[string]any{"k": "not a number"}, "k", nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := optFloat(tt.data, tt.key)
			if got != tt.want {
				t.Errorf("optFloat = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestOptInt(t *testing.T) {
	tests := []struct {
		name string
		data map[string]any
		key  string
		want any
	}{
		{"float64", map[string]any{"k": float64(42)}, "k", int64(42)},
		{"int", map[string]any{"k": int(7)}, "k", int64(7)},
		{"missing", map[string]any{}, "k", nil},
		{"string", map[string]any{"k": "nope"}, "k", nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := optInt(tt.data, tt.key)
			if got != tt.want {
				t.Errorf("optInt = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestInitDBCreatesTable(t *testing.T) {
	db := openTestDB(t)

	var name string
	err := db.QueryRow("SELECT name FROM sqlite_master WHERE type='table' AND name='readings'").Scan(&name)
	if err != nil {
		t.Fatalf("readings table not created: %v", err)
	}
	if name != "readings" {
		t.Errorf("table name = %q, want readings", name)
	}
}

func TestOpenDBInvalidPath(t *testing.T) {
	_, err := OpenDB("/nonexistent/path/to/db")
	if err == nil {
		t.Error("expected error for invalid path")
	}
}

func TestRawJSONStored(t *testing.T) {
	db := openTestDB(t)

	if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}

	var rawJSON string
	err := db.QueryRow("SELECT raw_json FROM readings WHERE id = 1").Scan(&rawJSON)
	if err != nil {
		t.Fatal(err)
	}
	if rawJSON == "" {
		t.Error("raw_json should not be empty")
	}
	if len(rawJSON) < 10 {
		t.Errorf("raw_json too short: %q", rawJSON)
	}
}

func TestRawJSONNotInAPIResponse(t *testing.T) {
	r := Reading{ID: 1, DeviceID: "test", DeviceType: deviceTypeIndoor, DeviceIP: "1.1.1.1"}
	j := ReadingToJSON(&r)
	if _, exists := j["raw_json"]; exists {
		t.Error("raw_json should not appear in API JSON response")
	}
}

func TestNowMillis(t *testing.T) {
	now := NowMillis()
	if now < 1700000000000 {
		t.Errorf("NowMillis = %d, seems too low", now)
	}
}

func TestSchemaFileExists(t *testing.T) {
	if _, err := os.Stat("../schema.sql"); os.IsNotExist(err) {
		t.Fatal("../schema.sql must exist for DB initialization")
	}
}
