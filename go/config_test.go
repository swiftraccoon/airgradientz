package main

import (
	"os"
	"path/filepath"
	"testing"
)

func TestLoadConfigDefaults(t *testing.T) {
	// Clear env to ensure defaults
	t.Setenv("PORT", "")
	t.Setenv("DB_PATH", "")
	t.Setenv("CONFIG_PATH", "")

	// Point to a nonexistent config so defaults are used
	t.Setenv("CONFIG_PATH", "/nonexistent/config.json")

	cfg := LoadConfig()

	if cfg.Port != 3016 {
		t.Errorf("Port = %d, want 3016", cfg.Port)
	}
	if cfg.PollIntervalMs != 15000 {
		t.Errorf("PollIntervalMs = %d, want 15000", cfg.PollIntervalMs)
	}
	if cfg.FetchTimeoutMs != 5000 {
		t.Errorf("FetchTimeoutMs = %d, want 5000", cfg.FetchTimeoutMs)
	}
	if cfg.MaxAPIRows != 10000 {
		t.Errorf("MaxAPIRows = %d, want 10000", cfg.MaxAPIRows)
	}
	if len(cfg.Devices) != 2 {
		t.Errorf("len(Devices) = %d, want 2", len(cfg.Devices))
	}
}

func TestLoadConfigFromFile(t *testing.T) {
	tmp := t.TempDir()
	configPath := filepath.Join(tmp, "test.json")

	content := `{
		"ports": {"go": 4000},
		"devices": [{"ip": "10.0.0.1", "label": "sensor1"}],
		"pollIntervalMs": 30000,
		"fetchTimeoutMs": 10000,
		"maxApiRows": 5000
	}`
	if err := os.WriteFile(configPath, []byte(content), 0644); err != nil {
		t.Fatal(err)
	}

	t.Setenv("CONFIG_PATH", configPath)
	t.Setenv("PORT", "")
	t.Setenv("DB_PATH", "")

	cfg := LoadConfig()

	if cfg.Port != 4000 {
		t.Errorf("Port = %d, want 4000", cfg.Port)
	}
	if len(cfg.Devices) != 1 {
		t.Fatalf("len(Devices) = %d, want 1", len(cfg.Devices))
	}
	if cfg.Devices[0].IP != "10.0.0.1" {
		t.Errorf("device IP = %q", cfg.Devices[0].IP)
	}
	if cfg.PollIntervalMs != 30000 {
		t.Errorf("PollIntervalMs = %d, want 30000", cfg.PollIntervalMs)
	}
	if cfg.FetchTimeoutMs != 10000 {
		t.Errorf("FetchTimeoutMs = %d, want 10000", cfg.FetchTimeoutMs)
	}
	if cfg.MaxAPIRows != 5000 {
		t.Errorf("MaxAPIRows = %d, want 5000", cfg.MaxAPIRows)
	}
}

func TestEnvOverrides(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("CONFIG_PATH", filepath.Join(tmp, "nope.json"))
	t.Setenv("PORT", "9999")
	t.Setenv("DB_PATH", "/tmp/custom.db")

	cfg := LoadConfig()

	if cfg.Port != 9999 {
		t.Errorf("Port = %d, want 9999", cfg.Port)
	}
	if cfg.DBPath != "/tmp/custom.db" {
		t.Errorf("DBPath = %q, want /tmp/custom.db", cfg.DBPath)
	}
}

func TestEnvPortValidation(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("CONFIG_PATH", filepath.Join(tmp, "nope.json"))

	tests := []struct {
		name    string
		portEnv string
		want    uint16
	}{
		{"valid", "8080", 8080},
		{"zero", "0", 3016},
		{"negative", "-1", 3016},
		{"too large", "70000", 3016},
		{"not a number", "abc", 3016},
		{"empty", "", 3016},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Setenv("PORT", tt.portEnv)
			t.Setenv("DB_PATH", "")
			cfg := LoadConfig()
			if cfg.Port != tt.want {
				t.Errorf("Port = %d, want %d", cfg.Port, tt.want)
			}
		})
	}
}

func TestConfigFileInvalidJSON(t *testing.T) {
	tmp := t.TempDir()
	configPath := filepath.Join(tmp, "bad.json")
	if err := os.WriteFile(configPath, []byte("{invalid json}"), 0644); err != nil {
		t.Fatal(err)
	}

	t.Setenv("CONFIG_PATH", configPath)
	t.Setenv("PORT", "")
	t.Setenv("DB_PATH", "")

	cfg := LoadConfig()
	// Should fall back to defaults
	if cfg.Port != 3016 {
		t.Errorf("Port = %d, want 3016 (defaults after bad JSON)", cfg.Port)
	}
}

func TestConfigFilePortBounds(t *testing.T) {
	tmp := t.TempDir()

	tests := []struct {
		name    string
		json    string
		want    uint16
	}{
		{"valid port", `{"ports":{"go":8080}}`, 8080},
		{"zero port", `{"ports":{"go":0}}`, 3016},
		{"negative port", `{"ports":{"go":-1}}`, 3016},
		{"too large port", `{"ports":{"go":70000}}`, 3016},
		{"wrong key", `{"ports":{"rust":8080}}`, 3016},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			configPath := filepath.Join(tmp, tt.name+".json")
			if err := os.WriteFile(configPath, []byte(tt.json), 0644); err != nil {
				t.Fatal(err)
			}
			t.Setenv("CONFIG_PATH", configPath)
			t.Setenv("PORT", "")
			t.Setenv("DB_PATH", "")
			cfg := LoadConfig()
			if cfg.Port != tt.want {
				t.Errorf("Port = %d, want %d", cfg.Port, tt.want)
			}
		})
	}
}

func TestConfigFileTooLarge(t *testing.T) {
	tmp := t.TempDir()
	configPath := filepath.Join(tmp, "huge.json")

	// Create a file > 1MB
	huge := make([]byte, 1048577)
	for i := range huge {
		huge[i] = ' '
	}
	if err := os.WriteFile(configPath, huge, 0644); err != nil {
		t.Fatal(err)
	}

	t.Setenv("CONFIG_PATH", configPath)
	t.Setenv("PORT", "")
	t.Setenv("DB_PATH", "")

	cfg := LoadConfig()
	// Should use defaults when config file is too large
	if cfg.Port != 3016 {
		t.Errorf("Port = %d, want 3016 (defaults after oversized config)", cfg.Port)
	}
}
