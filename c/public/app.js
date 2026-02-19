(function () {
  'use strict';

  // --- AQI Calculation (Task 18) ---

  var AQI_BREAKPOINTS = [
    { aqiLo: 0,   aqiHi: 50,  pmLo: 0.0,   pmHi: 9.0,    category: 'Good',                       color: '#00e400' },
    { aqiLo: 51,  aqiHi: 100, pmLo: 9.1,    pmHi: 35.4,   category: 'Moderate',                   color: '#ffff00' },
    { aqiLo: 101, aqiHi: 150, pmLo: 35.5,   pmHi: 55.4,   category: 'Unhealthy for Sensitive',    color: '#ff7e00' },
    { aqiLo: 151, aqiHi: 200, pmLo: 55.5,   pmHi: 125.4,  category: 'Unhealthy',                  color: '#ff0000' },
    { aqiLo: 201, aqiHi: 300, pmLo: 125.5,  pmHi: 225.4,  category: 'Very Unhealthy',             color: '#8f3f97' },
    { aqiLo: 301, aqiHi: 500, pmLo: 225.5,  pmHi: 325.4,  category: 'Hazardous',                  color: '#7e0023' },
  ];

  function calculateAQI(pm25) {
    if (pm25 == null || !Number.isFinite(pm25) || pm25 < 0) return null;
    for (var i = 0; i < AQI_BREAKPOINTS.length; i++) {
      var bp = AQI_BREAKPOINTS[i];
      if (pm25 <= bp.pmHi) {
        var aqi = ((bp.aqiHi - bp.aqiLo) / (bp.pmHi - bp.pmLo)) * (pm25 - bp.pmLo) + bp.aqiLo;
        return { value: Math.round(aqi), category: bp.category, color: bp.color };
      }
    }
    // Beyond 500.4 - cap at Hazardous
    return { value: 500, category: 'Hazardous', color: '#7e0023' };
  }

  function aqiTextColor(bgColor) {
    // Light backgrounds (Good=#00e400, Moderate=#ffff00) get black text; rest get white
    return (bgColor === '#00e400' || bgColor === '#ffff00') ? '#000' : '#fff';
  }

  // --- Color palette for N-device support (Task 19) ---

  var DEVICE_PALETTE = [
    { border: 'rgba(96, 165, 250, 1)',  bg: 'rgba(96, 165, 250, 0.1)' },   // blue (indoor)
    { border: 'rgba(74, 222, 128, 1)',  bg: 'rgba(74, 222, 128, 0.1)' },   // green (outdoor)
    { border: 'rgba(251, 191, 36, 1)',  bg: 'rgba(251, 191, 36, 0.1)' },   // amber
    { border: 'rgba(244, 114, 182, 1)', bg: 'rgba(244, 114, 182, 0.1)' },  // pink
    { border: 'rgba(167, 139, 250, 1)', bg: 'rgba(167, 139, 250, 0.1)' },  // violet
    { border: 'rgba(45, 212, 191, 1)',  bg: 'rgba(45, 212, 191, 0.1)' },   // teal
    { border: 'rgba(251, 146, 60, 1)',  bg: 'rgba(251, 146, 60, 0.1)' },   // orange
    { border: 'rgba(156, 163, 175, 1)', bg: 'rgba(156, 163, 175, 0.1)' },  // gray
  ];

  // --- State ---

  var currentRange = 86400000; // 24h default
  var refreshMs = 30000;       // updated from server config
  var pollIntervalMs = 30000;  // updated from server config
  var refreshTimer = null;
  var charts = {};
  var devices = [];            // discovered devices from /api/devices
  var hiddenDevices = {};      // device_id -> true if hidden

  // Compensated-first field resolution: use compensated value when available,
  // fall back to raw (mirrors python-airgradient's __post_deserialize__ logic).
  function compensatedFirst(row, compensated, raw) {
    return row[compensated] != null ? row[compensated] : row[raw];
  }

  function capitalize(s) {
    return s.charAt(0).toUpperCase() + s.slice(1);
  }

  var CHART_DEFS = [
    { id: 'chart-pm25',     valueFn: function (r) { return compensatedFirst(r, 'pm02_compensated', 'pm02'); } },
    { id: 'chart-co2',      valueFn: function (r) { return r.rco2; } },
    { id: 'chart-temp',     valueFn: function (r) { return compensatedFirst(r, 'atmp_compensated', 'atmp'); } },
    { id: 'chart-humidity', valueFn: function (r) { return compensatedFirst(r, 'rhum_compensated', 'rhum'); } },
    { id: 'chart-tvoc',     valueFn: function (r) { return r.tvoc_index; } },
    { id: 'chart-nox',      valueFn: function (r) { return r.nox_index; } },
  ];

  // --- Chart creation (N-device dynamic datasets) ---

  function makeDatasets() {
    var datasets = [];
    for (var i = 0; i < devices.length; i++) {
      var d = devices[i];
      var color = DEVICE_PALETTE[i % DEVICE_PALETTE.length];
      datasets.push({
        label: capitalize(d.device_type || d.device_id),
        data: [],
        borderColor: color.border,
        backgroundColor: color.bg,
        borderWidth: 1.5,
        pointRadius: 0,
        fill: true,
        tension: 0.3,
        hidden: !!hiddenDevices[d.device_id],
        _deviceId: d.device_id,
      });
    }
    return datasets;
  }

  function makeChart(canvasId) {
    var ctx = document.getElementById(canvasId).getContext('2d');
    return new Chart(ctx, {
      type: 'line',
      data: {
        datasets: makeDatasets(),
      },
      options: {
        responsive: true,
        maintainAspectRatio: true,
        aspectRatio: 2.5,
        interaction: { mode: 'index', intersect: false },
        plugins: {
          legend: {
            labels: { color: '#8b8fa3', boxWidth: 12, padding: 10 },
          },
          tooltip: { mode: 'index', intersect: false },
        },
        scales: {
          x: {
            type: 'time',
            ticks: { color: '#8b8fa3', maxTicksLimit: 8 },
            grid: { color: 'rgba(42, 45, 58, 0.6)' },
          },
          y: {
            ticks: { color: '#8b8fa3' },
            grid: { color: 'rgba(42, 45, 58, 0.6)' },
          },
        },
      },
    });
  }

  function initCharts() {
    for (var i = 0; i < CHART_DEFS.length; i++) {
      var def = CHART_DEFS[i];
      charts[def.id] = makeChart(def.id);
    }
  }

  // --- Data fetching ---

  function fetchTimeRange() {
    var now = Date.now();
    if (currentRange === 0) {
      return { from: 0, to: now };
    }
    return { from: now - currentRange, to: now };
  }

  async function fetchReadings(downsample) {
    var range = fetchTimeRange();
    var url = '/api/readings?from=' + range.from + '&to=' + range.to;
    if (downsample) {
      url += '&downsample=' + downsample;
    }
    var res = await fetch(url);
    if (!res.ok) return [];
    return res.json();
  }

  // --- Downsample dropdown ---

  var LARGE_RANGE_MS = 2592000000; // 30d — warn when loading raw data beyond this
  var previousDownsample = '';     // tracks last confirmed value for revert on cancel

  function getSelectedDownsample() {
    return document.getElementById('downsample-select').value;
  }

  // --- Chart update (N-device) ---

  function updateCharts(readings) {
    if (!Array.isArray(readings)) return;

    // Group readings by device_id
    var byDevice = {};
    for (var i = 0; i < readings.length; i++) {
      var r = readings[i];
      var did = r.device_id;
      if (!byDevice[did]) byDevice[did] = [];
      byDevice[did].push(r);
    }

    for (var j = 0; j < CHART_DEFS.length; j++) {
      var def = CHART_DEFS[j];
      var chart = charts[def.id];

      for (var k = 0; k < chart.data.datasets.length; k++) {
        var ds = chart.data.datasets[k];
        var deviceReadings = byDevice[ds._deviceId] || [];
        ds.data = deviceReadings.map(function (r) {
          return { x: r.timestamp, y: def.valueFn(r) };
        });
      }
      chart.update('none');
    }
  }

  // --- Current values panel (Task 18) ---

  function formatMetricValue(value, isInteger) {
    if (value == null || !Number.isFinite(value)) return '--';
    if (isInteger) return Math.round(value).toString();
    return value.toFixed(1);
  }

  async function updateCurrentValues() {
    var res = await fetch('/api/readings/latest');
    if (!res.ok) return;
    var latest = await res.json();
    if (!Array.isArray(latest)) return;

    var container = document.getElementById('current-values');
    while (container.firstChild) {
      container.removeChild(container.firstChild);
    }

    for (var i = 0; i < latest.length; i++) {
      var r = latest[i];

      var card = document.createElement('div');
      card.className = 'current-card';

      var heading = document.createElement('h3');
      heading.className = 'current-card-label';
      heading.textContent = capitalize(r.device_type || r.device_id);

      card.appendChild(heading);

      // AQI badge
      var pm25 = compensatedFirst(r, 'pm02_compensated', 'pm02');
      var aqi = calculateAQI(pm25);
      if (aqi) {
        var badge = document.createElement('div');
        badge.className = 'aqi-badge';
        badge.style.backgroundColor = aqi.color;
        badge.style.color = aqiTextColor(aqi.color);

        var aqiValue = document.createElement('span');
        aqiValue.className = 'aqi-value';
        aqiValue.textContent = aqi.value;

        var aqiCat = document.createElement('span');
        aqiCat.className = 'aqi-category';
        aqiCat.textContent = aqi.category;

        badge.appendChild(aqiValue);
        badge.appendChild(aqiCat);
        card.appendChild(badge);
      }

      // Metric grid
      var grid = document.createElement('div');
      grid.className = 'metric-grid';

      var metrics = [
        { label: 'PM2.5', value: pm25, integer: false },
        { label: 'CO2', value: r.rco2, integer: true },
        { label: 'Temp', value: compensatedFirst(r, 'atmp_compensated', 'atmp'), integer: false },
        { label: 'Humidity', value: compensatedFirst(r, 'rhum_compensated', 'rhum'), integer: false },
        { label: 'TVOC', value: r.tvoc_index, integer: true },
        { label: 'NOx', value: r.nox_index, integer: true },
      ];

      for (var m = 0; m < metrics.length; m++) {
        var metric = document.createElement('div');
        metric.className = 'metric';

        var mLabel = document.createElement('div');
        mLabel.className = 'metric-label';
        mLabel.textContent = metrics[m].label;

        var mValue = document.createElement('div');
        mValue.className = 'metric-value';
        mValue.textContent = formatMetricValue(metrics[m].value, metrics[m].integer);

        metric.appendChild(mLabel);
        metric.appendChild(mValue);
        grid.appendChild(metric);
      }

      card.appendChild(grid);
      container.appendChild(card);
    }
  }

  // --- Device health badges ---

  async function updateDeviceStatus() {
    var res = await fetch('/api/health');
    if (!res.ok) return;
    var healthDevices = await res.json();
    if (!Array.isArray(healthDevices)) return;

    var container = document.getElementById('device-status');
    while (container.firstChild) {
      container.removeChild(container.firstChild);
    }

    for (var i = 0; i < healthDevices.length; i++) {
      var d = healthDevices[i];
      // Look up device_id from the discovered devices list by matching label
      var matchedDeviceId = findDeviceIdByIp(d.ip) || '';
      var badge = document.createElement('div');
      badge.className = 'device-badge';
      if (hiddenDevices[matchedDeviceId]) {
        badge.classList.add('hidden');
      }
      badge.dataset.deviceId = matchedDeviceId;

      var dot = document.createElement('span');
      if (d.status === 'ok') {
        dot.className = 'dot online';
      } else if (d.status === 'error') {
        dot.className = 'dot offline';
      } else {
        dot.className = 'dot unknown';
      }

      var label = document.createElement('span');
      label.textContent = capitalize(d.label);

      var detail = document.createElement('span');
      detail.style.color = 'var(--text-dim)';

      if (d.status === 'unknown') {
        detail.textContent = 'waiting...';
      } else if (d.status === 'error') {
        var parts = [d.lastErrorMessage || 'error'];
        if (d.consecutiveFailures > 1) {
          parts.push('(' + d.consecutiveFailures + 'x)');
        }
        detail.textContent = parts.join(' ');
      } else if (d.lastSuccess) {
        var age = Date.now() - d.lastSuccess;
        detail.textContent = age < 60000 ? 'just now' : Math.round(age / 60000) + 'm ago';
      }

      badge.append(dot, label, detail);
      container.appendChild(badge);
    }
  }

  // --- Device toggle (Task 19) ---

  function findDeviceIdByIp(ip) {
    for (var i = 0; i < devices.length; i++) {
      if (devices[i].device_ip === ip) {
        return devices[i].device_id;
      }
    }
    return null;
  }

  document.getElementById('device-status').addEventListener('click', function (e) {
    var badge = e.target.closest('.device-badge');
    if (!badge) return;

    var deviceId = badge.dataset.deviceId;
    if (!deviceId) return;

    // Toggle hidden state
    if (hiddenDevices[deviceId]) {
      delete hiddenDevices[deviceId];
      badge.classList.remove('hidden');
    } else {
      hiddenDevices[deviceId] = true;
      badge.classList.add('hidden');
    }

    // Toggle dataset visibility across all charts
    for (var key in charts) {
      var chart = charts[key];
      for (var i = 0; i < chart.data.datasets.length; i++) {
        var ds = chart.data.datasets[i];
        if (ds._deviceId === deviceId) {
          ds.hidden = !!hiddenDevices[deviceId];
        }
      }
      chart.update('none');
    }
  });

  // --- Discover devices ---

  async function discoverDevices() {
    try {
      var res = await fetch('/api/devices');
      if (!res.ok) {
        console.warn('Failed to discover devices:', res.status);
        return;
      }
      var data = await res.json();
      if (Array.isArray(data)) {
        devices = data;
      }
    } catch (err) {
      console.warn('Failed to discover devices:', err);
    }
  }

  // --- Refresh ---

  async function refresh() {
    try {
      var downsample = getSelectedDownsample();
      var results = await Promise.all([
        fetchReadings(downsample || undefined),
        updateDeviceStatus(),
        updateCurrentValues(),
      ]);
      var readings = results[0];
      if (readings !== null) {
        updateCharts(readings);
      }
    } catch (err) {
      console.error('Failed to refresh:', err);
    }
  }

  function startRefreshTimer() {
    if (refreshTimer) clearInterval(refreshTimer);
    refreshTimer = setInterval(refresh, refreshMs);
  }

  // Human-friendly labels for downsample bucket keys
  var BUCKET_LABELS = {
    '5m': '5 min', '10m': '10 min', '15m': '15 min', '30m': '30 min',
    '1h': '1 hour', '1d': '1 day', '1w': '1 week'
  };

  function populateDownsampleDropdown(buckets) {
    var select = document.getElementById('downsample-select');
    // Remove all options except the first "Raw" option
    while (select.options.length > 1) {
      select.remove(1);
    }
    // Sort by ms value ascending
    var entries = Object.keys(buckets).map(function (k) { return { key: k, ms: buckets[k] }; });
    entries.sort(function (a, b) { return a.ms - b.ms; });
    for (var i = 0; i < entries.length; i++) {
      var opt = document.createElement('option');
      opt.value = entries[i].key;
      opt.textContent = BUCKET_LABELS[entries[i].key] || entries[i].key;
      select.appendChild(opt);
    }
  }

  // Fetch server config to sync refresh interval with poll interval
  async function loadConfig() {
    try {
      var res = await fetch('/api/config');
      if (!res.ok) return;
      var cfg = await res.json();
      if (cfg.pollIntervalMs && Number.isFinite(cfg.pollIntervalMs)) {
        pollIntervalMs = cfg.pollIntervalMs;
        refreshMs = cfg.pollIntervalMs;
      }
      if (cfg.downsampleBuckets && typeof cfg.downsampleBuckets === 'object') {
        populateDownsampleDropdown(cfg.downsampleBuckets);
      }
    } catch (_) {
      // use defaults
    }
  }

  // Time range buttons
  document.getElementById('time-range').addEventListener('click', function (e) {
    if (e.target.tagName !== 'BUTTON') return;
    var newRange = Number(e.target.dataset.range);
    var isRaw = getSelectedDownsample() === '';
    var isLargeRange = newRange === 0 || newRange >= LARGE_RANGE_MS;

    if (isRaw && isLargeRange) {
      var ok = confirm(
        'Loading raw 15-second data for this time range may return hundreds of ' +
        'thousands of readings and could slow down or crash your browser. Continue?'
      );
      if (!ok) return;
    }

    var active = document.querySelector('#time-range .active');
    if (active) active.classList.remove('active');
    e.target.classList.add('active');
    currentRange = newRange;
    refresh();
  });

  // Downsample dropdown
  document.getElementById('downsample-select').addEventListener('change', function () {
    var select = this;
    var isRaw = select.value === '';
    var isLargeRange = currentRange === 0 || currentRange >= LARGE_RANGE_MS;

    if (isRaw && isLargeRange) {
      var ok = confirm(
        'Loading raw 15-second data for this time range may return hundreds of ' +
        'thousands of readings and could slow down or crash your browser. Continue?'
      );
      if (!ok) {
        select.value = previousDownsample;
        return;
      }
    }

    previousDownsample = select.value;
    refresh();
  });

  // Init
  async function init() {
    await loadConfig();
    await discoverDevices();
    initCharts();
    refresh();
    startRefreshTimer();
  }

  init();
})();
