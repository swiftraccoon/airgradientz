package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"
)

func newTestHandler(t *testing.T) *handler {
	t.Helper()
	db := openTestDB(t)

	cfg := &Config{
		Port:           3016,
		Devices:        []DeviceConfig{{IP: "192.168.1.1", Label: "test-indoor"}},
		PollIntervalMs: 15000,
		FetchTimeoutMs: 5000,
		MaxAPIRows:     10000,
	}

	poller := NewPoller(db, cfg)
	stats := &serverStats{startedAt: NowMillis()}

	return &handler{db: db, cfg: cfg, poller: poller, stats: stats}
}

func doGet(h *handler, path string) *httptest.ResponseRecorder {
	req := httptest.NewRequest(http.MethodGet, path, http.NoBody)
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	return rr
}

func parseJSON(t *testing.T, rr *httptest.ResponseRecorder) any {
	t.Helper()
	var result any
	if err := json.Unmarshal(rr.Body.Bytes(), &result); err != nil {
		t.Fatalf("JSON parse failed: %v\nbody: %s", err, rr.Body.String())
	}
	return result
}

func TestReadingsEndpoint(t *testing.T) {
	h := newTestHandler(t)

	if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}

	rr := doGet(h, "/api/readings")
	if rr.Code != 200 {
		t.Fatalf("status = %d, want 200", rr.Code)
	}

	result := parseJSON(t, rr)
	arr, ok := result.([]any)
	if !ok {
		t.Fatal("expected JSON array")
	}
	if len(arr) != 1 {
		t.Fatalf("expected 1 reading, got %d", len(arr))
	}

	reading := arr[0].(map[string]any)
	if reading["device_id"] != testDeviceID {
		t.Errorf("device_id = %v, want %v", reading["device_id"], testDeviceID)
	}
	if reading["device_type"] != "indoor" {
		t.Errorf("device_type = %v", reading["device_type"])
	}
}

func TestReadingsWithDeviceFilter(t *testing.T) {
	h := newTestHandler(t)

	if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(h.db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	rr := doGet(h, "/api/readings?device="+testDeviceID)
	result := parseJSON(t, rr).([]any)
	if len(result) != 1 {
		t.Fatalf("expected 1 filtered reading, got %d", len(result))
	}
}

func TestReadingsWithLimit(t *testing.T) {
	h := newTestHandler(t)

	for range 5 {
		if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
			t.Fatal(err)
		}
	}

	rr := doGet(h, "/api/readings?limit=2")
	result := parseJSON(t, rr).([]any)
	if len(result) != 2 {
		t.Fatalf("expected 2, got %d", len(result))
	}
}

func TestReadingsLimitCannotExceedMax(t *testing.T) {
	h := newTestHandler(t)
	h.cfg.MaxAPIRows = 3

	for range 5 {
		if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
			t.Fatal(err)
		}
	}

	rr := doGet(h, "/api/readings?limit=100")
	result := parseJSON(t, rr).([]any)
	if len(result) != 3 {
		t.Fatalf("expected max 3, got %d", len(result))
	}
}

func TestReadingsEmpty(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/readings")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}
	result := parseJSON(t, rr).([]any)
	if len(result) != 0 {
		t.Fatalf("expected 0, got %d", len(result))
	}
}

func TestReadingsLatest(t *testing.T) {
	h := newTestHandler(t)

	if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
		t.Fatal(err)
	}
	if err := InsertReading(h.db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	rr := doGet(h, "/api/readings/latest")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}
	result := parseJSON(t, rr).([]any)
	if len(result) != 2 {
		t.Fatalf("expected 2 latest, got %d", len(result))
	}
}

func TestDevicesEndpoint(t *testing.T) {
	h := newTestHandler(t)

	for range 3 {
		if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
			t.Fatal(err)
		}
	}
	if err := InsertReading(h.db, "192.168.1.2", outdoorFull); err != nil {
		t.Fatal(err)
	}

	rr := doGet(h, "/api/devices")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}
	result := parseJSON(t, rr).([]any)
	if len(result) != 2 {
		t.Fatalf("expected 2, got %d", len(result))
	}
}

func TestHealthEndpoint(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/health")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}

	result := parseJSON(t, rr).([]any)
	if len(result) != 1 {
		t.Fatalf("expected 1 device in health, got %d", len(result))
	}

	dev := result[0].(map[string]any)
	if dev["status"] != "unknown" {
		t.Errorf("initial status = %v, want unknown", dev["status"])
	}
	if dev["ip"] != "192.168.1.1" {
		t.Errorf("ip = %v", dev["ip"])
	}
}

func TestConfigEndpoint(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/config")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}

	result := parseJSON(t, rr).(map[string]any)
	if result["pollIntervalMs"] != float64(15000) {
		t.Errorf("pollIntervalMs = %v", result["pollIntervalMs"])
	}

	devices := result["devices"].([]any)
	if len(devices) != 1 {
		t.Fatalf("expected 1 device, got %d", len(devices))
	}
}

func TestStatsEndpoint(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/stats")
	if rr.Code != 200 {
		t.Fatalf("status = %d", rr.Code)
	}

	result := parseJSON(t, rr).(map[string]any)
	if result["implementation"] != "go" {
		t.Errorf("implementation = %v", result["implementation"])
	}
	if result["readings_count"] != float64(0) {
		t.Errorf("readings_count = %v", result["readings_count"])
	}
	if _, ok := result["uptime_ms"]; !ok {
		t.Error("missing uptime_ms")
	}
	if _, ok := result["memory_rss_bytes"]; !ok {
		t.Error("missing memory_rss_bytes")
	}
}

func TestMethodNotAllowed(t *testing.T) {
	h := newTestHandler(t)

	req := httptest.NewRequest(http.MethodPost, "/api/readings", http.NoBody)
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)

	if rr.Code != 405 {
		t.Fatalf("status = %d, want 405", rr.Code)
	}
}

func TestSecurityHeaders(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/health")

	if got := rr.Header().Get("X-Content-Type-Options"); got != "nosniff" {
		t.Errorf("X-Content-Type-Options = %q", got)
	}
	if got := rr.Header().Get("X-Frame-Options"); got != "DENY" {
		t.Errorf("X-Frame-Options = %q", got)
	}
	if got := rr.Header().Get("Connection"); got != "close" {
		t.Errorf("Connection = %q", got)
	}
}

func TestContentTypeJSON(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/api/health")

	if got := rr.Header().Get("Content-Type"); got != "application/json" {
		t.Errorf("Content-Type = %q, want application/json", got)
	}
}

func TestStaticPathTraversal(t *testing.T) {
	h := newTestHandler(t)

	tests := []struct {
		name string
		path string
	}{
		{"dotdot", "/../../etc/passwd"},
		{"encoded dotdot", "/%2e%2e/etc/passwd"},
		{"dotdot in middle", "/foo/../../../etc/passwd"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			rr := doGet(h, tt.path)
			if rr.Code != 404 {
				t.Errorf("status = %d, want 404 for path traversal attempt", rr.Code)
			}
		})
	}
}

func TestStaticControlChars(t *testing.T) {
	h := newTestHandler(t)

	// Go's net/http rejects raw control chars before they reach the handler.
	// Test that our handler also rejects them if somehow present via URL encoding.
	// Use a request with a path containing a DEL character (0x7F) which is valid in URLs.
	req := &http.Request{
		Method: http.MethodGet,
		URL:    &url.URL{Path: "/bad\x7fpath"},
		Header: make(http.Header),
	}
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != 404 {
		t.Errorf("status = %d, want 404 for control char path", rr.Code)
	}
}

func TestNotFoundForMissing(t *testing.T) {
	h := newTestHandler(t)

	rr := doGet(h, "/nonexistent.html")
	if rr.Code != 404 {
		t.Errorf("status = %d, want 404", rr.Code)
	}
}

func TestContentTypeForExtensions(t *testing.T) {
	tests := []struct {
		path string
		want string
	}{
		{"file.html", "text/html; charset=utf-8"},
		{"file.css", "text/css; charset=utf-8"},
		{"file.js", "application/javascript; charset=utf-8"},
		{"file.json", "application/json; charset=utf-8"},
		{"file.png", "image/png"},
		{"file.jpg", "image/jpeg"},
		{"file.jpeg", "image/jpeg"},
		{"file.svg", "image/svg+xml"},
		{"file.ico", "image/x-icon"},
		{"file.xyz", "application/octet-stream"},
	}
	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			got := contentTypeFor(tt.path)
			if got != tt.want {
				t.Errorf("contentTypeFor(%q) = %q, want %q", tt.path, got, tt.want)
			}
		})
	}
}

func TestParseInt64Param(t *testing.T) {
	tests := []struct {
		name       string
		query      string
		param      string
		defaultVal int64
		want       int64
	}{
		{"present", "?n=42", "n", 0, 42},
		{"missing", "?other=1", "n", 99, 99},
		{"empty", "?n=", "n", 99, 99},
		{"invalid", "?n=abc", "n", 99, 99},
		{"negative", "?n=-100", "n", 0, -100},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			req := httptest.NewRequest(http.MethodGet, "/test"+tt.query, http.NoBody)
			got := parseInt64Param(req, tt.param, tt.defaultVal)
			if got != tt.want {
				t.Errorf("parseInt64Param = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestRequestCounter(t *testing.T) {
	h := newTestHandler(t)

	doGet(h, "/api/health")
	doGet(h, "/api/health")
	doGet(h, "/api/health")

	rr := doGet(h, "/api/stats")
	result := parseJSON(t, rr).(map[string]any)

	served := result["requests_served"].(float64)
	if served != 4 {
		t.Errorf("requests_served = %v, want 4", served)
	}
}
