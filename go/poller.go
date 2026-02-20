package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"sync"
	"sync/atomic"
	"time"
)

const (
	maxResponseBody         = 1 * 1024 * 1024
	checkpointIntervalPolls = 10
	pollerMsPerSecond       = 1000
)

type DeviceHealth struct {
	IP                  string
	Label               string
	Status              string
	LastErrorMessage    string
	LastSuccess         int64
	LastError           int64
	ConsecutiveFailures int
}

type Poller struct {
	db        *DB
	cfg       *Config
	health    []DeviceHealth
	successes int64
	failures  int64
	mu        sync.RWMutex
}

func NewPoller(db *DB, cfg *Config) *Poller {
	health := make([]DeviceHealth, len(cfg.Devices))
	for i, d := range cfg.Devices {
		health[i] = DeviceHealth{
			IP:     d.IP,
			Label:  d.Label,
			Status: "unknown",
		}
	}
	return &Poller{db: db, cfg: cfg, health: health}
}

func (p *Poller) Run(ctx context.Context) {
	logf("[poller] Starting — polling %d devices every %.1fs",
		len(p.cfg.Devices), float64(p.cfg.PollIntervalMs)/pollerMsPerSecond)

	p.pollAll()

	ticker := time.NewTicker(time.Duration(p.cfg.PollIntervalMs) * time.Millisecond)
	defer ticker.Stop()

	var pollCount uint32
	for {
		select {
		case <-ctx.Done():
			logf("[poller] Stopped")
			return
		case <-ticker.C:
			p.pollAll()
			pollCount++
			if pollCount%checkpointIntervalPolls == 0 {
				if err := p.db.Checkpoint(); err != nil {
					logf("[poller] checkpoint error: %v", err)
				}
			}
		}
	}
}

func (p *Poller) pollAll() {
	for i := range p.cfg.Devices {
		p.fetchDevice(i)
	}
}

func (p *Poller) fetchDevice(idx int) {
	d := p.cfg.Devices[idx]

	client := &http.Client{
		Timeout: time.Duration(p.cfg.FetchTimeoutMs) * time.Millisecond,
	}

	reqURL := fmt.Sprintf("http://%s/measures/current", d.IP)
	req, err := http.NewRequestWithContext(context.Background(), http.MethodGet, reqURL, http.NoBody)
	if err != nil {
		p.setError(idx, fmt.Sprintf("create request: %v", err))
		return
	}
	resp, err := client.Do(req)
	if err != nil {
		p.setError(idx, fmt.Sprintf("fetch failed: %v", err))
		return
	}
	defer resp.Body.Close()

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		p.setError(idx, fmt.Sprintf("HTTP %d", resp.StatusCode))
		return
	}

	body, err := io.ReadAll(io.LimitReader(resp.Body, maxResponseBody+1))
	if err != nil {
		p.setError(idx, fmt.Sprintf("read body: %v", err))
		return
	}
	if len(body) > maxResponseBody {
		p.setError(idx, "response too large")
		return
	}

	var data map[string]any
	if err := json.Unmarshal(body, &data); err != nil {
		p.setError(idx, "JSON parse error")
		return
	}

	if err := p.db.InsertReading(d.IP, data); err != nil {
		p.setError(idx, fmt.Sprintf("DB insert failed: %v", err))
		return
	}

	logInsertResult(d.Label, d.IP, data)

	atomic.AddInt64(&p.successes, 1)
	p.mu.Lock()
	p.health[idx].Status = "ok"
	p.health[idx].LastSuccess = NowMillis()
	p.health[idx].LastErrorMessage = ""
	p.health[idx].ConsecutiveFailures = 0
	p.mu.Unlock()
}

func (p *Poller) setError(idx int, msg string) {
	d := p.cfg.Devices[idx]
	logf("[poller] %s (%s): %s", d.Label, d.IP, msg)

	atomic.AddInt64(&p.failures, 1)
	p.mu.Lock()
	p.health[idx].Status = "error"
	p.health[idx].LastError = NowMillis()
	p.health[idx].LastErrorMessage = msg
	p.health[idx].ConsecutiveFailures++
	p.mu.Unlock()
}

func (p *Poller) HealthJSON() []map[string]any {
	p.mu.RLock()
	defer p.mu.RUnlock()

	result := make([]map[string]any, len(p.health))
	for i, h := range p.health {
		obj := map[string]any{
			"ip":                  h.IP,
			"label":               h.Label,
			"status":              h.Status,
			"consecutiveFailures": h.ConsecutiveFailures,
		}

		if h.LastSuccess != 0 {
			obj["lastSuccess"] = h.LastSuccess
		} else {
			obj["lastSuccess"] = nil
		}

		if h.LastError != 0 {
			obj["lastError"] = h.LastError
		} else {
			obj["lastError"] = nil
		}

		if h.LastErrorMessage != "" {
			obj["lastErrorMessage"] = h.LastErrorMessage
		} else {
			obj["lastErrorMessage"] = nil
		}

		result[i] = obj
	}
	return result
}

// HealthJSONBytes returns the health status as pre-serialized JSON bytes,
// bypassing encoding/json for maximum throughput.
func (p *Poller) HealthJSONBytes() []byte {
	p.mu.RLock()
	defer p.mu.RUnlock()

	buf := bytes.NewBuffer(make([]byte, 0, len(p.health)*200+2))
	buf.WriteByte('[')
	for i, h := range p.health {
		if i > 0 {
			buf.WriteByte(',')
		}
		buf.WriteString(`{"ip":`)
		writeJSONString(buf, h.IP)
		buf.WriteString(`,"label":`)
		writeJSONString(buf, h.Label)
		buf.WriteString(`,"status":`)
		writeJSONString(buf, h.Status)
		buf.WriteString(`,"lastSuccess":`)
		if h.LastSuccess != 0 {
			buf.WriteString(strconv.FormatInt(h.LastSuccess, 10))
		} else {
			buf.WriteString("null")
		}
		buf.WriteString(`,"lastError":`)
		if h.LastError != 0 {
			buf.WriteString(strconv.FormatInt(h.LastError, 10))
		} else {
			buf.WriteString("null")
		}
		buf.WriteString(`,"lastErrorMessage":`)
		if h.LastErrorMessage != "" {
			writeJSONString(buf, h.LastErrorMessage)
		} else {
			buf.WriteString("null")
		}
		buf.WriteString(`,"consecutiveFailures":`)
		buf.WriteString(strconv.Itoa(h.ConsecutiveFailures))
		buf.WriteByte('}')
	}
	buf.WriteByte(']')
	return buf.Bytes()
}

func (p *Poller) PollStats() (successes, failures int64) {
	return atomic.LoadInt64(&p.successes), atomic.LoadInt64(&p.failures)
}
