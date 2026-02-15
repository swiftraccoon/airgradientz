package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strconv"
)

type DeviceConfig struct {
	IP    string `json:"ip"`
	Label string `json:"label"`
}

type Config struct {
	Port           uint16
	DBPath         string
	Devices        []DeviceConfig
	PollIntervalMs int
	FetchTimeoutMs int
	MaxAPIRows     int
}

type configFile struct {
	Ports          map[string]int `json:"ports"`
	Devices        []DeviceConfig `json:"devices"`
	PollIntervalMs *int           `json:"pollIntervalMs"`
	FetchTimeoutMs *int           `json:"fetchTimeoutMs"`
	MaxAPIRows     *int           `json:"maxApiRows"`
}

func LoadConfig() Config {
	cfg := Config{
		Port:   3016,
		DBPath: "./airgradientz.db",
		Devices: []DeviceConfig{
			{IP: "192.168.88.6", Label: "outdoor"},
			{IP: "192.168.88.159", Label: "indoor"},
		},
		PollIntervalMs: 15000,
		FetchTimeoutMs: 5000,
		MaxAPIRows:     10000,
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

func applyConfigFile(cfg *Config, cf *configFile) {
	if p, ok := cf.Ports["go"]; ok && p > 0 && p <= 65535 {
		cfg.Port = uint16(p)
	}
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
}

func applyEnvOverrides(cfg *Config) {
	if portStr := os.Getenv("PORT"); portStr != "" {
		if p, err := strconv.Atoi(portStr); err == nil && p > 0 && p <= 65535 {
			cfg.Port = uint16(p)
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

func findConfigFile() ([]byte, string) {
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
		if len(content) > 1048576 {
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
