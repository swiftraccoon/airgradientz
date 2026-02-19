package main

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

const maxConfigFileSize = 1048576

type DeviceConfig struct {
	IP    string `json:"ip"`
	Label string `json:"label"`
}

type Config struct {
	DBPath            string
	DownsampleBuckets map[string]int64
	Devices           []DeviceConfig
	PollIntervalMs    int
	FetchTimeoutMs    int
	MaxAPIRows        int
	Port              uint16
}

type configFile struct {
	Ports             map[string]int   `json:"ports"`
	PollIntervalMs    *int             `json:"pollIntervalMs"`
	FetchTimeoutMs    *int             `json:"fetchTimeoutMs"`
	MaxAPIRows        *int             `json:"maxApiRows"`
	DownsampleBuckets map[string]int64 `json:"downsampleBuckets"`
	Devices           []DeviceConfig   `json:"devices"`
}

func LoadConfig() Config {
	cfg := Config{
		DBPath: "./airgradientz.db",
	}

	content, path := findConfigFile()
	if content == nil {
		logf("fatal: config file not found")
		os.Exit(1)
	}
	logf("[config] Loaded config from %s", path)
	var cf configFile
	if err := json.Unmarshal(content, &cf); err != nil {
		logf("[config] JSON parse error: %v", err)
		os.Exit(1)
	}
	applyConfigFile(&cfg, &cf)

	applyEnvOverrides(&cfg)

	var missing []string
	if cfg.PollIntervalMs <= 0 {
		missing = append(missing, "pollIntervalMs")
	}
	if cfg.FetchTimeoutMs <= 0 {
		missing = append(missing, "fetchTimeoutMs")
	}
	if cfg.MaxAPIRows <= 0 {
		missing = append(missing, "maxApiRows")
	}
	if len(cfg.DownsampleBuckets) == 0 {
		missing = append(missing, "downsampleBuckets")
	}
	if len(cfg.Devices) == 0 {
		missing = append(missing, "devices")
	}
	if cfg.Port == 0 {
		missing = append(missing, "ports.go")
	}
	if len(missing) > 0 {
		logf("fatal: missing required config keys: %s", strings.Join(missing, ", "))
		os.Exit(1)
	}

	logConfig(&cfg)

	return cfg
}

// safeUint16 converts an int to uint16, returning (value, true) if in range.
func safeUint16(v int) (uint16, bool) {
	if v > 0 && v <= math.MaxUint16 {
		return uint16(v), true
	}
	return 0, false
}

func applyConfigFile(cfg *Config, cf *configFile) {
	applyConfigValues(cfg, cf)

	if p, ok := cf.Ports["go"]; ok {
		if port, valid := safeUint16(p); valid {
			cfg.Port = port
		}
	}
}

func applyConfigValues(cfg *Config, cf *configFile) {
	if len(cf.Devices) > 0 {
		cfg.Devices = cf.Devices
	}
	if cf.PollIntervalMs != nil && *cf.PollIntervalMs > 0 {
		cfg.PollIntervalMs = *cf.PollIntervalMs
	}
	if cf.FetchTimeoutMs != nil && *cf.FetchTimeoutMs > 0 {
		cfg.FetchTimeoutMs = *cf.FetchTimeoutMs
	}
	if cf.MaxAPIRows != nil && *cf.MaxAPIRows > 0 {
		cfg.MaxAPIRows = *cf.MaxAPIRows
	}
	if len(cf.DownsampleBuckets) > 0 {
		cfg.DownsampleBuckets = cf.DownsampleBuckets
	}
}

func applyEnvOverrides(cfg *Config) {
	if portStr := os.Getenv("PORT"); portStr != "" {
		if p, err := strconv.Atoi(portStr); err == nil {
			if port, valid := safeUint16(p); valid {
				cfg.Port = port
			}
		}
	}
	if dbPath := os.Getenv("DB_PATH"); dbPath != "" {
		cfg.DBPath = dbPath
	}
}

func logConfig(cfg *Config) {
	deviceList := ""
	for i, d := range cfg.Devices {
		if i > 0 {
			deviceList += ", "
		}
		deviceList += fmt.Sprintf("%s(%s)", d.Label, d.IP)
	}
	logf("[config] port=%d devices=[%s] poll=%dms", cfg.Port, deviceList, cfg.PollIntervalMs)
}

func findConfigFile() (content []byte, absPath string) {
	var candidates []string
	if envPath := os.Getenv("CONFIG_PATH"); envPath != "" {
		candidates = append(candidates, envPath)
	}
	candidates = append(candidates, "./airgradientz.json", "../airgradientz.json")

	for _, path := range candidates {
		content, err := os.ReadFile(path)
		if err != nil {
			if path == os.Getenv("CONFIG_PATH") && path != "" {
				logf("[config] CONFIG_PATH set but unreadable: %s", path)
			}
			continue
		}
		if len(content) > maxConfigFileSize {
			continue
		}
		absPath, err := filepath.Abs(path)
		if err != nil {
			absPath = path
		}
		return content, absPath
	}
	return nil, ""
}
