package main

import (
	"encoding/json"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
)

// testSpec holds the parsed test-spec.json structure.
type testSpec struct {
	ResponseShapes    map[string]responseShape `json:"responseShapes"`
	QueryEdgeCases    []queryEdgeCase          `json:"queryEdgeCases"`
	DownsampleBuckets []downsampleBucket       `json:"downsampleBuckets"`
}

type responseShape struct {
	RequiredFields  []string `json:"requiredFields"`
	ForbiddenFields []string `json:"forbiddenFields"`
	ExactFields     []string `json:"exactFields"`
	NoExtraFields   bool     `json:"noExtraFields"`
}

type queryEdgeCase struct {
	Name     string            `json:"name"`
	Endpoint string            `json:"endpoint"`
	Params   map[string]string `json:"params"`
	Expect   edgeCaseExpect    `json:"expect"`
}

type edgeCaseExpect struct {
	Body         any    `json:"body"`
	ResultCount  *int   `json:"resultCount"`
	HasResults   *bool  `json:"hasResults"`
	ResultCapped string `json:"resultCapped"`
	BodyHasKey   string `json:"bodyHasKey"`
	StatusCode   int    `json:"statusCode"`
}

type downsampleBucket struct {
	Param    string `json:"param"`
	ExpectMs int64  `json:"expectMs"`
}

func loadTestSpec(t *testing.T) testSpec {
	t.Helper()
	data, err := os.ReadFile("../test-spec.json")
	if err != nil {
		t.Fatalf("failed to read test-spec.json: %v", err)
	}
	var spec testSpec
	if err := json.Unmarshal(data, &spec); err != nil {
		t.Fatalf("failed to parse test-spec.json: %v", err)
	}
	return spec
}

func TestResponseShapes(t *testing.T) {
	spec := loadTestSpec(t)

	t.Run("reading required fields", func(t *testing.T) {
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
		if len(readings) == 0 {
			t.Fatal("expected at least 1 reading")
		}

		j := ReadingToJSON(&readings[0])

		shape := spec.ResponseShapes["reading"]
		for _, field := range shape.RequiredFields {
			if _, exists := j[field]; !exists {
				t.Errorf("required field %q missing from reading JSON", field)
			}
		}
		for _, field := range shape.ForbiddenFields {
			if _, exists := j[field]; exists {
				t.Errorf("forbidden field %q present in reading JSON", field)
			}
		}
	})

	t.Run("device no first_seen", func(t *testing.T) {
		db := openTestDB(t)

		if err := InsertReading(db, "192.168.1.1", indoorFull); err != nil {
			t.Fatalf("InsertReading: %v", err)
		}

		devices, err := GetDevices(db)
		if err != nil {
			t.Fatalf("GetDevices: %v", err)
		}
		if len(devices) == 0 {
			t.Fatal("expected at least 1 device")
		}

		j := DeviceSummaryToJSON(&devices[0])

		shape := spec.ResponseShapes["device"]
		for _, field := range shape.RequiredFields {
			if _, exists := j[field]; !exists {
				t.Errorf("required field %q missing from device JSON", field)
			}
		}
		for _, field := range shape.ForbiddenFields {
			if _, exists := j[field]; exists {
				t.Errorf("forbidden field %q present in device JSON", field)
			}
		}
	})
}

func TestQueryEdgeCases(t *testing.T) {
	spec := loadTestSpec(t)

	// Build a test handler with a small MaxAPIRows so we can verify capping.
	h := newTestHandler(t)
	h.cfg.MaxAPIRows = 5

	// Insert enough readings to exercise limit edge cases.
	for range 10 {
		if err := InsertReading(h.db, "192.168.1.1", indoorFull); err != nil {
			t.Fatalf("InsertReading: %v", err)
		}
	}

	for _, tc := range spec.QueryEdgeCases {
		t.Run(tc.Name, func(t *testing.T) {
			path := buildEdgeCasePath(&tc)
			rr := doGet(h, path)

			if rr.Code != tc.Expect.StatusCode {
				t.Fatalf("status = %d, want %d (body: %s)", rr.Code, tc.Expect.StatusCode, rr.Body.String())
			}

			checkResultCapped(t, tc.Expect, rr, h.cfg.MaxAPIRows)
			checkResultCount(t, tc.Expect, rr)
			checkHasResults(t, tc.Expect, rr)
			checkBodyExpect(t, tc.Expect, rr)
			checkBodyHasKey(t, tc.Expect, rr)
		})
	}
}

func buildEdgeCasePath(tc *queryEdgeCase) string {
	path := tc.Endpoint
	if len(tc.Params) > 0 {
		vals := make([]string, 0, len(tc.Params))
		for k, v := range tc.Params {
			vals = append(vals, k+"="+v)
		}
		path += "?" + strings.Join(vals, "&")
	}
	return path
}

func checkResultCapped(t *testing.T, expect edgeCaseExpect, rr *httptest.ResponseRecorder, maxRows int) {
	t.Helper()
	if expect.ResultCapped != "maxApiRows" {
		return
	}
	arr := parseJSONArray(t, rr)
	if len(arr) > maxRows {
		t.Errorf("result count %d exceeds maxApiRows %d", len(arr), maxRows)
	}
}

func checkResultCount(t *testing.T, expect edgeCaseExpect, rr *httptest.ResponseRecorder) {
	t.Helper()
	if expect.ResultCount == nil {
		return
	}
	arr := parseJSONArray(t, rr)
	if len(arr) != *expect.ResultCount {
		t.Errorf("result count = %d, want %d", len(arr), *expect.ResultCount)
	}
}

func checkHasResults(t *testing.T, expect edgeCaseExpect, rr *httptest.ResponseRecorder) {
	t.Helper()
	if expect.HasResults == nil || !*expect.HasResults {
		return
	}
	arr := parseJSONArray(t, rr)
	if len(arr) == 0 {
		t.Error("expected non-empty result array")
	}
}

func checkBodyExpect(t *testing.T, expect edgeCaseExpect, rr *httptest.ResponseRecorder) {
	t.Helper()
	if expect.Body == nil {
		return
	}
	result := parseJSON(t, rr)
	checkBodyMatch(t, expect.Body, result)
}

func checkBodyHasKey(t *testing.T, expect edgeCaseExpect, rr *httptest.ResponseRecorder) {
	t.Helper()
	if expect.BodyHasKey == "" {
		return
	}
	result := parseJSON(t, rr)
	obj, ok := result.(map[string]any)
	if !ok {
		t.Fatalf("expected JSON object for bodyHasKey check, got %T", result)
	}
	if _, exists := obj[expect.BodyHasKey]; !exists {
		t.Errorf("expected key %q in response body", expect.BodyHasKey)
	}
}

func TestAllDownsampleBuckets(t *testing.T) {
	spec := loadTestSpec(t)

	// Use the full bucket set from newTestHandler which mirrors the real config.
	h := newTestHandler(t)
	configBuckets := h.cfg.DownsampleBuckets

	for _, bucket := range spec.DownsampleBuckets {
		t.Run(bucket.Param, func(t *testing.T) {
			ms, ok := configBuckets[bucket.Param]
			if !ok {
				t.Fatalf("downsample bucket %q not found in config", bucket.Param)
			}
			if ms != bucket.ExpectMs {
				t.Errorf("bucket %q = %d ms, want %d ms", bucket.Param, ms, bucket.ExpectMs)
			}
		})
	}
}

func parseJSONArray(t *testing.T, rr *httptest.ResponseRecorder) []any {
	t.Helper()
	result := parseJSON(t, rr)
	arr, ok := result.([]any)
	if !ok {
		t.Fatalf("expected JSON array, got %T", result)
	}
	return arr
}

// checkBodyMatch compares expected body from the spec with the actual parsed JSON.
func checkBodyMatch(t *testing.T, expected, actual any) {
	t.Helper()

	switch exp := expected.(type) {
	case []any:
		arr, ok := actual.([]any)
		if !ok {
			t.Errorf("expected array, got %T", actual)
			return
		}
		if len(exp) != len(arr) {
			t.Errorf("array length = %d, want %d", len(arr), len(exp))
		}
	case map[string]any:
		obj, ok := actual.(map[string]any)
		if !ok {
			t.Errorf("expected object, got %T", actual)
			return
		}
		for k, v := range exp {
			actualVal, exists := obj[k]
			if !exists {
				t.Errorf("expected key %q in response", k)
				continue
			}
			// JSON numbers unmarshal as float64; compare numerically.
			expNum, expIsNum := v.(float64)
			actNum, actIsNum := actualVal.(float64)
			if expIsNum && actIsNum {
				if expNum != actNum {
					t.Errorf("key %q = %v, want %v", k, actNum, expNum)
				}
			} else if v != actualVal {
				t.Errorf("key %q = %v (%T), want %v (%T)", k, actualVal, actualVal, v, v)
			}
		}
	}
}
