# Device Polling Specification

Single source of truth for AirGradient device polling behavior across all implementations.

## Polling Loop

- **Interval**: `pollIntervalMs` from config (default 15000ms)
- **Timeout per device**: `fetchTimeoutMs` from config (default 5000ms)
- Polls all configured devices sequentially each interval
- First poll starts immediately on server startup

## Device Fetch

- **URL**: `GET http://{device.ip}/measures/current`
- **Timeout**: `fetchTimeoutMs`
- **Expected**: JSON object with AirGradient sensor data
- Invalid JSON or non-object response: treat as poll error

## Field Mapping

AirGradient device JSON fields mapped to database columns.

| Device JSON field  | DB column          | Type    | Required                          |
| ------------------ | ------------------ | ------- | --------------------------------- |
| `serialno`         | `device_id`        | TEXT    | Yes (default `"unknown"` if missing) |
| `model`            | _(parsed for device_type)_ | -  | No                                |
| `pm01`             | `pm01`             | REAL    | No                                |
| `pm02`             | `pm02`             | REAL    | No                                |
| `pm10`             | `pm10`             | REAL    | No                                |
| `pm02Compensated`  | `pm02_compensated` | REAL    | No                                |
| `rco2`             | `rco2`             | INTEGER | No                                |
| `atmp`             | `atmp`             | REAL    | No                                |
| `atmpCompensated`  | `atmp_compensated` | REAL    | No                                |
| `rhum`             | `rhum`             | REAL    | No                                |
| `rhumCompensated`  | `rhum_compensated` | REAL    | No                                |
| `tvocIndex`        | `tvoc_index`       | REAL    | No                                |
| `noxIndex`         | `nox_index`        | REAL    | No                                |
| `wifi`             | `wifi`             | INTEGER | No                                |
| _(entire response)_ | `raw_json`        | TEXT    | Yes (full JSON stored)            |

## Computed Fields

These columns are not from the device response:

| DB column     | Source                                                    |
| ------------- | --------------------------------------------------------- |
| `timestamp`   | Current server time in Unix epoch milliseconds            |
| `device_type` | `"indoor"` if `model` starts with `"I-"`, else `"outdoor"` |
| `device_ip`   | The configured IP address used to fetch                   |

## Ignored Fields

Present in device responses but not stored as individual columns:
`pm003Count`, `pm005Count`, `pm01Count`, `pm02Count`, `pm50Count`, `pm10Count`,
`pm01Standard`, `pm02Standard`, `pm10Standard`, `tvocRaw`, `noxRaw`, `boot`,
`bootCount`, `ledMode`, `firmware`.

These are preserved in `raw_json`.

## Health State Machine

Each configured device has a health record initialized to:

```json
{
  "ip": "<device_ip>",
  "label": "<device_label>",
  "status": "unknown",
  "lastSuccess": null,
  "lastError": null,
  "lastErrorMessage": null,
  "consecutiveFailures": 0
}
```

### Status Values

| Status      | Meaning                           |
| ----------- | --------------------------------- |
| `"unknown"` | Initial state, no polls completed |
| `"ok"`      | Last poll succeeded               |
| `"error"`   | Last poll failed                  |

### On Successful Poll

```
status = "ok"
lastSuccess = now_ms
lastErrorMessage = null
consecutiveFailures = 0
```

### On Failed Poll

```
status = "error"
lastError = now_ms
lastErrorMessage = <error description>
consecutiveFailures += 1
```

### Failure Modes

- Network timeout (device unreachable)
- HTTP error (non-200 status)
- Invalid JSON response
- Non-object JSON response
