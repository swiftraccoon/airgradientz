package main

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"math"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync/atomic"
	"time"
)

const (
	maxStaticFileSize    = 16 * 1024 * 1024
	serverReadTimeout    = 10 * time.Second
	serverWriteTimeout   = 30 * time.Second
	serverIdleTimeout    = 60 * time.Second
	serverMaxHeaderBytes = 8192
	shutdownTimeout      = 5 * time.Second
	msPerSecond          = 1000
)

type serverStats struct {
	requestsServed    int64
	activeConnections int64
	startedAt         int64
}

type handler struct {
	db     *sql.DB
	cfg    *Config
	poller *Poller
	stats  *serverStats
}

func RunServer(ctx context.Context, db *sql.DB, cfg *Config, poller *Poller) {
	stats := &serverStats{startedAt: NowMillis()}

	h := &handler{db: db, cfg: cfg, poller: poller, stats: stats}

	srv := &http.Server{
		Addr:           fmt.Sprintf(":%d", cfg.Port),
		Handler:        h,
		ReadTimeout:    serverReadTimeout,
		WriteTimeout:   serverWriteTimeout,
		IdleTimeout:    serverIdleTimeout,
		MaxHeaderBytes: serverMaxHeaderBytes,
		ConnState: func(_ net.Conn, state http.ConnState) {
			switch state {
			case http.StateNew:
				atomic.AddInt64(&h.stats.activeConnections, 1)
			case http.StateClosed, http.StateHijacked:
				atomic.AddInt64(&h.stats.activeConnections, -1)
			case http.StateActive, http.StateIdle:
				// No action needed for active/idle transitions.
			}
		},
	}

	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), shutdownTimeout)
		defer cancel()
		if err := srv.Shutdown(shutdownCtx); err != nil {
			log.Printf("[server] Shutdown error: %v", err)
		}
	}()

	log.Printf("[server] Listening on http://localhost:%d", cfg.Port)

	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		log.Fatalf("[server] ListenAndServe: %v", err)
	}
}

func (h *handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.Header().Set("X-Frame-Options", "DENY")
	w.Header().Set("Connection", "close")

	atomic.AddInt64(&h.stats.requestsServed, 1)

	if r.Method != http.MethodGet {
		writeError(w, http.StatusMethodNotAllowed, "Method not allowed")
		return
	}

	switch r.URL.Path {
	case "/api/readings":
		h.handleReadings(w, r)
	case "/api/readings/latest":
		h.handleReadingsLatest(w, r)
	case "/api/devices":
		h.handleDevices(w, r)
	case "/api/health":
		h.handleHealth(w, r)
	case "/api/config":
		h.handleConfig(w, r)
	case "/api/stats":
		h.handleStats(w, r)
	default:
		h.serveStatic(w, r)
	}
}

func (h *handler) handleReadings(w http.ResponseWriter, r *http.Request) {
	now := NowMillis()
	defaultFrom := now - 24*60*60*msPerSecond

	from := parseInt64Param(r, "from", defaultFrom)
	to := parseInt64Param(r, "to", now)
	device := r.URL.Query().Get("device")

	rawLimit := parseInt64Param(r, "limit", int64(h.cfg.MaxAPIRows))
	requestedLimit := min(int(min(rawLimit, math.MaxInt)), h.cfg.MaxAPIRows)
	effectiveLimit := h.cfg.MaxAPIRows
	if requestedLimit > 0 && requestedLimit < h.cfg.MaxAPIRows {
		effectiveLimit = requestedLimit
	}

	q := ReadingQuery{
		Device: device,
		From:   from,
		To:     to,
		Limit:  effectiveLimit,
	}
	if q.Device == "" {
		q.Device = "all"
	}

	readings, err := QueryReadings(h.db, q)
	if err != nil {
		log.Printf("[api] query_readings error: %v", err)
		writeError(w, http.StatusInternalServerError, "Internal server error")
		return
	}

	items := make([]map[string]any, len(readings))
	for i := range readings {
		items[i] = ReadingToJSON(&readings[i])
	}
	writeJSON(w, items)
}

func (h *handler) handleReadingsLatest(w http.ResponseWriter, _ *http.Request) {
	readings, err := GetLatestReadings(h.db)
	if err != nil {
		log.Printf("[api] get_latest_readings error: %v", err)
		writeError(w, http.StatusInternalServerError, "Internal server error")
		return
	}

	items := make([]map[string]any, len(readings))
	for i := range readings {
		items[i] = ReadingToJSON(&readings[i])
	}
	writeJSON(w, items)
}

func (h *handler) handleDevices(w http.ResponseWriter, _ *http.Request) {
	devices, err := GetDevices(h.db)
	if err != nil {
		log.Printf("[api] get_devices error: %v", err)
		writeError(w, http.StatusInternalServerError, "Internal server error")
		return
	}

	items := make([]map[string]any, len(devices))
	for i := range devices {
		items[i] = DeviceSummaryToJSON(&devices[i])
	}
	writeJSON(w, items)
}

func (h *handler) handleHealth(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, h.poller.HealthJSON())
}

func (h *handler) handleConfig(w http.ResponseWriter, _ *http.Request) {
	devices := make([]map[string]string, len(h.cfg.Devices))
	for i, d := range h.cfg.Devices {
		devices[i] = map[string]string{"ip": d.IP, "label": d.Label}
	}

	writeJSON(w, map[string]any{
		"pollIntervalMs": h.cfg.PollIntervalMs,
		"devices":        devices,
	})
}

func (h *handler) handleStats(w http.ResponseWriter, _ *http.Request) {
	now := NowMillis()
	uptimeMs := now - h.stats.startedAt

	readingsCount, err := GetReadingsCount(h.db)
	if err != nil {
		log.Printf("[api] get_readings_count error: %v", err)
	}

	var dbSizeBytes int64
	if info, err := os.Stat(h.cfg.DBPath); err == nil {
		dbSizeBytes = info.Size()
	}

	var memStats runtime.MemStats
	runtime.ReadMemStats(&memStats)

	successes, failures := h.poller.PollStats()

	writeJSON(w, map[string]any{
		"implementation":     "go",
		"pid":                os.Getpid(),
		"uptime_ms":          uptimeMs,
		"memory_rss_bytes":   readRSSBytes(),
		"db_size_bytes":      dbSizeBytes,
		"readings_count":     readingsCount,
		"requests_served":    atomic.LoadInt64(&h.stats.requestsServed),
		"active_connections": atomic.LoadInt64(&h.stats.activeConnections),
		"poll_successes":     successes,
		"poll_failures":      failures,
		"pool_alloc_count":   0,
		"pool_bytes_used":    0,
		"started_at":         h.stats.startedAt,
	})
}

func (h *handler) serveStatic(w http.ResponseWriter, r *http.Request) {
	reqPath := r.URL.Path

	if !isValidStaticPath(reqPath) {
		writeError(w, http.StatusNotFound, "Not found")
		return
	}

	resolved, err := resolveStaticPath(reqPath)
	if err != nil {
		writeError(w, http.StatusNotFound, "Not found")
		return
	}

	info, err := os.Stat(resolved)
	if err != nil || info.IsDir() {
		writeError(w, http.StatusNotFound, "Not found")
		return
	}
	if info.Size() > maxStaticFileSize {
		writeError(w, http.StatusRequestEntityTooLarge, "File too large")
		return
	}

	content, err := os.ReadFile(resolved)
	if err != nil {
		writeError(w, http.StatusInternalServerError, "Internal server error")
		return
	}

	w.Header().Set("Content-Type", contentTypeFor(resolved))
	w.Header().Set("Cache-Control", "public, max-age=600")
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write(content)
}

// isValidStaticPath checks for path traversal and control characters.
func isValidStaticPath(reqPath string) bool {
	if strings.Contains(reqPath, "..") {
		return false
	}
	for i := range len(reqPath) {
		c := reqPath[i]
		if c < 0x20 || c == 0x7F {
			return false
		}
	}
	return true
}

// resolveStaticPath resolves a request path to a safe filesystem path under public/.
func resolveStaticPath(reqPath string) (string, error) {
	relative := strings.TrimLeft(reqPath, "/")
	filePath := "public/index.html"
	if relative != "" {
		filePath = filepath.Join("public", relative)
	}

	resolved, err := filepath.EvalSymlinks(filePath)
	if err != nil {
		return "", err
	}
	publicDir, err := filepath.EvalSymlinks("public")
	if err != nil {
		return "", err
	}

	absResolved, err := filepath.Abs(resolved)
	if err != nil {
		return "", err
	}
	absPublic, err := filepath.Abs(publicDir)
	if err != nil {
		return "", err
	}

	if absResolved != absPublic &&
		!strings.HasPrefix(absResolved, absPublic+string(filepath.Separator)) {
		return "", fmt.Errorf("path traversal: %s", reqPath)
	}

	return resolved, nil
}

// --- helpers ---

func writeJSON(w http.ResponseWriter, data any) {
	body, err := json.Marshal(data)
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		_, _ = w.Write([]byte(`{"error":"Internal server error"}`))
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	_, _ = w.Write(body)
}

func writeError(w http.ResponseWriter, status int, msg string) {
	body, err := json.Marshal(map[string]string{"error": msg})
	if err != nil {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusInternalServerError)
		_, _ = w.Write([]byte(`{"error":"Internal server error"}`))
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_, _ = w.Write(body)
}

func parseInt64Param(r *http.Request, name string, defaultVal int64) int64 {
	s := r.URL.Query().Get(name)
	if s == "" {
		return defaultVal
	}
	v, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return defaultVal
	}
	return v
}

func contentTypeFor(path string) string {
	ext := filepath.Ext(path)
	switch ext {
	case ".html":
		return "text/html; charset=utf-8"
	case ".css":
		return "text/css; charset=utf-8"
	case ".js":
		return "application/javascript; charset=utf-8"
	case ".json":
		return "application/json; charset=utf-8"
	case ".png":
		return "image/png"
	case ".jpg", ".jpeg":
		return "image/jpeg"
	case ".svg":
		return "image/svg+xml"
	case ".ico":
		return "image/x-icon"
	default:
		return "application/octet-stream"
	}
}

func readRSSBytes() int64 {
	data, err := os.ReadFile("/proc/self/statm")
	if err != nil {
		return 0
	}
	fields := strings.Fields(string(data))
	if len(fields) < 2 {
		return 0
	}
	pages, err := strconv.ParseInt(fields[1], 10, 64)
	if err != nil {
		return 0
	}
	return pages * int64(os.Getpagesize())
}
