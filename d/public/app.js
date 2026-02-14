(function () {
  'use strict';

  const INDOOR_COLOR = 'rgba(96, 165, 250, 1)';
  const INDOOR_BG = 'rgba(96, 165, 250, 0.1)';
  const OUTDOOR_COLOR = 'rgba(74, 222, 128, 1)';
  const OUTDOOR_BG = 'rgba(74, 222, 128, 0.1)';

  let currentRange = 86400000; // 24h default
  let refreshMs = 30_000;      // updated from server config
  let pollIntervalMs = 30_000; // updated from server config
  let refreshTimer = null;
  const charts = {};

  // Compensated-first field resolution: use compensated value when available,
  // fall back to raw (mirrors python-airgradient's __post_deserialize__ logic).
  function compensatedFirst(row, compensated, raw) {
    return row[compensated] != null ? row[compensated] : row[raw];
  }

  const CHART_DEFS = [
    { id: 'chart-pm25',     valueFn: r => compensatedFirst(r, 'pm02_compensated', 'pm02') },
    { id: 'chart-co2',      valueFn: r => r.rco2 },
    { id: 'chart-temp',     valueFn: r => compensatedFirst(r, 'atmp_compensated', 'atmp') },
    { id: 'chart-humidity',  valueFn: r => compensatedFirst(r, 'rhum_compensated', 'rhum') },
    { id: 'chart-tvoc',     valueFn: r => r.tvoc_index },
    { id: 'chart-nox',      valueFn: r => r.nox_index },
  ];

  function makeChart(canvasId) {
    const ctx = document.getElementById(canvasId).getContext('2d');
    return new Chart(ctx, {
      type: 'line',
      data: {
        datasets: [
          {
            label: 'Indoor',
            data: [],
            borderColor: INDOOR_COLOR,
            backgroundColor: INDOOR_BG,
            borderWidth: 1.5,
            pointRadius: 0,
            fill: true,
            tension: 0.3,
          },
          {
            label: 'Outdoor',
            data: [],
            borderColor: OUTDOOR_COLOR,
            backgroundColor: OUTDOOR_BG,
            borderWidth: 1.5,
            pointRadius: 0,
            fill: true,
            tension: 0.3,
          },
        ],
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
    for (const def of CHART_DEFS) {
      charts[def.id] = makeChart(def.id);
    }
  }

  async function fetchReadings() {
    const now = Date.now();
    const from = now - currentRange;
    const res = await fetch(`/api/readings?from=${from}&to=${now}`);
    if (!res.ok) return [];
    return res.json();
  }

  function updateCharts(readings) {
    if (!Array.isArray(readings)) return;

    const indoor = readings.filter(r => r.device_type === 'indoor');
    const outdoor = readings.filter(r => r.device_type === 'outdoor');

    for (const def of CHART_DEFS) {
      const chart = charts[def.id];
      chart.data.datasets[0].data = indoor.map(r => ({ x: r.timestamp, y: def.valueFn(r) }));
      chart.data.datasets[1].data = outdoor.map(r => ({ x: r.timestamp, y: def.valueFn(r) }));
      chart.update('none');
    }
  }

  // Device health uses the poller's health endpoint, not just last-seen timestamp.
  // This distinguishes "never polled yet" from "device erroring" from "device OK".
  async function updateDeviceStatus() {
    const res = await fetch('/api/health');
    if (!res.ok) return;
    const devices = await res.json();
    if (!Array.isArray(devices)) return;

    const container = document.getElementById('device-status');
    container.innerHTML = '';

    for (const d of devices) {
      const badge = document.createElement('div');
      badge.className = 'device-badge';

      const dot = document.createElement('span');
      if (d.status === 'ok') {
        dot.className = 'dot online';
      } else if (d.status === 'error') {
        dot.className = 'dot offline';
      } else {
        dot.className = 'dot unknown';
      }

      const label = document.createElement('span');
      label.textContent = d.label.charAt(0).toUpperCase() + d.label.slice(1);

      const detail = document.createElement('span');
      detail.style.color = 'var(--text-dim)';

      if (d.status === 'unknown') {
        detail.textContent = 'waiting...';
      } else if (d.status === 'error') {
        const parts = [d.lastErrorMessage || 'error'];
        if (d.consecutiveFailures > 1) {
          parts.push(`(${d.consecutiveFailures}x)`);
        }
        detail.textContent = parts.join(' ');
      } else if (d.lastSuccess) {
        const age = Date.now() - d.lastSuccess;
        detail.textContent = age < 60_000 ? 'just now' : Math.round(age / 60_000) + 'm ago';
      }

      badge.append(dot, label, detail);
      container.appendChild(badge);
    }
  }

  async function refresh() {
    try {
      const [readings] = await Promise.all([fetchReadings(), updateDeviceStatus()]);
      updateCharts(readings);
    } catch (err) {
      console.error('Failed to refresh:', err);
    }
  }

  function startRefreshTimer() {
    if (refreshTimer) clearInterval(refreshTimer);
    refreshTimer = setInterval(refresh, refreshMs);
  }

  // Fetch server config to sync refresh interval with poll interval
  async function loadConfig() {
    try {
      const res = await fetch('/api/config');
      if (!res.ok) return;
      const cfg = await res.json();
      if (cfg.pollIntervalMs && Number.isFinite(cfg.pollIntervalMs)) {
        pollIntervalMs = cfg.pollIntervalMs;
        refreshMs = cfg.pollIntervalMs;
      }
    } catch (_) {
      // use defaults
    }
  }

  // Time range buttons
  document.getElementById('time-range').addEventListener('click', e => {
    if (e.target.tagName !== 'BUTTON') return;
    document.querySelector('#time-range .active')?.classList.remove('active');
    e.target.classList.add('active');
    currentRange = Number(e.target.dataset.range);
    refresh();
  });

  // Init
  async function init() {
    await loadConfig();
    initCharts();
    refresh();
    startRefreshTimer();
  }

  init();
})();
