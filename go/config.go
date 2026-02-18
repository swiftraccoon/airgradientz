package main

import (
	"encoding/json"
	"fmt"
	"log"
	"math"
	"os"
	"path/filepath"
	"strconv"
)

// Configuration default values.
const (
	defaultPort                 = 3016
	defaultPollIntervalMs       = 15000
	defaultFetchTimeoutMs       = 5000
	defaultMaxAPIRows           = 10000
	defaultDownsampleThreshold  = 10000
	maxConfigFileSize           = 1048576
)

type DeviceConfig struct {
	IP    string `json:"ip"`
	Label string `json:"label"`
}

type Config struct {
	DBPath              string
	Devices             []DeviceConfig
	PollIntervalMs      int
	FetchTimeoutMs      int
	MaxAPIRows          int
	DownsampleThreshold int
	Port                uint16
}

type configFile struct {
	Ports               map[string]int `json:"ports"`
	Defaults            *configFile    `json:"defaults"`
	PollIntervalMs      *int           `json:"pollIntervalMs"`
	FetchTimeoutMs      *int           `json:"fetchTimeoutMs"`
	MaxAPIRows          *int           `json:"maxApiRows"`
	DownsampleThreshold *int           `json:"downsampleThreshold"`
	Devices             []DeviceConfig `json:"devices"`
}

func LoadConfig() Config {
	cfg := Config{
		Port:   defaultPort,
		DBPath: "./airgradientz.db",
		Devices: []DeviceConfig{
			{IP: "192.168.88.6", Label: "outdoor"},
			{IP: "192.168.88.159", Label: "indoor"},
		},
		PollIntervalMs:      defaultPollIntervalMs,
		FetchTimeoutMs:      defaultFetchTimeoutMs,
		MaxAPIRows:          defaultMaxAPIRows,
		DownsampleThreshold: defaultDownsampleThreshold,
	}

	if content, path := findConfigFile(); content != nil {
		log.Printf("[config] Loaded config from %s", path)
		var cf configFile
		if err := json.Unmarshal(content, &cf); err != nil {
			log.Printf("[config] JSON parse error: %v", err)
		} else {
			applyConfigFile(&cfg, &cf)
		}
	} else {
		log.Print("[config] No config file found, using defaults")
	}

	applyEnvOverrides(&cfg)
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
	// Apply defaults first (lower priority)
	if cf.Defaults != nil {
		applyConfigValues(cfg, cf.Defaults)
	}
	// Apply top-level overrides (higher priority)
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
	if cf.DownsampleThreshold != nil && *cf.DownsampleThreshold > 0 {
		cfg.DownsampleThreshold = *cf.DownsampleThreshold
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
	log.Printf("[config] port=%d devices=[%s] poll=%dms", cfg.Port, deviceList, cfg.PollIntervalMs)
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
				log.Printf("[config] CONFIG_PATH set but unreadable: %s", path)
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
