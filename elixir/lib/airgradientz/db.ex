defmodule Airgradientz.DB do
  @moduledoc false
  use GenServer

  # -- Compile-time: load and parse queries.sql --

  @queries_path Path.join([__DIR__, "..", "..", "..", "queries.sql"])
  @external_resource @queries_path

  @queries (
    File.read!(@queries_path)
    |> String.split("\n")
    |> Enum.reduce({%{}, nil, []}, fn line, {map, name, lines} ->
      cond do
        String.starts_with?(line, "-- name: ") ->
          new_name = line |> String.trim_leading("-- name: ") |> String.trim()

          map =
            if name do
              sql = lines |> Enum.reverse() |> Enum.join("\n") |> String.trim() |> String.trim_trailing(";")
              Map.put(map, name, sql)
            else
              map
            end

          {map, new_name, []}

        String.starts_with?(line, "--") ->
          {map, name, lines}

        String.trim(line) == "" ->
          {map, name, lines}

        true ->
          {map, name, [String.trim(line) | lines]}
      end
    end)
    |> then(fn {map, name, lines} ->
      if name do
        sql = lines |> Enum.reverse() |> Enum.join("\n") |> String.trim() |> String.trim_trailing(";")
        Map.put(map, name, sql)
      else
        map
      end
    end)
  )

  # Derive column list from loaded reading_columns
  @reading_columns Map.fetch!(@queries, "reading_columns")
  @query_columns @reading_columns |> String.split(~r/,\s*/) |> Enum.map(&String.trim/1)
  @query_cols Enum.join(@query_columns, ", ")

  # Convert :name placeholders to ?N (1-based positional) for exqlite
  @insert_sql (
    (fn sql ->
      Regex.scan(~r/:[a-z][a-z0-9_]*/, sql)
      |> List.flatten()
      |> Enum.with_index(1)
      |> Enum.reduce(sql, fn {placeholder, idx}, acc ->
        String.replace(acc, placeholder, "?#{idx}", global: false)
      end)
    end).(Map.fetch!(@queries, "insert_reading"))
  )

  @devices_sql Map.fetch!(@queries, "select_devices")
  @latest_sql Map.fetch!(@queries, "select_latest")
  @count_sql Map.fetch!(@queries, "count_readings")

  # Downsampled query columns (no id)
  @ds_columns "device_id, device_type, device_ip"
  @ds_agg_columns """
  AVG(pm01) AS pm01, AVG(pm02) AS pm02, AVG(pm10) AS pm10,
  AVG(pm02_compensated) AS pm02_compensated,
  CAST(AVG(rco2) AS INTEGER) AS rco2,
  AVG(atmp) AS atmp, AVG(atmp_compensated) AS atmp_compensated,
  AVG(rhum) AS rhum, AVG(rhum_compensated) AS rhum_compensated,
  AVG(tvoc_index) AS tvoc_index, AVG(nox_index) AS nox_index,
  CAST(AVG(wifi) AS INTEGER) AS wifi
  """

  # Pre-built query SQL for all variants (dynamic WHERE/LIMIT)
  @q_all_range "SELECT #{@query_cols} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC"
  @q_all_range_limit "SELECT #{@query_cols} FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2 ORDER BY timestamp ASC LIMIT ?3"
  @q_dev_range "SELECT #{@query_cols} FROM readings WHERE device_id = ?1 AND timestamp >= ?2 AND timestamp <= ?3 ORDER BY timestamp ASC"
  @q_dev_range_limit "SELECT #{@query_cols} FROM readings WHERE device_id = ?1 AND timestamp >= ?2 AND timestamp <= ?3 ORDER BY timestamp ASC LIMIT ?4"

  # -- Public API --

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def insert_reading(device_ip, data) do
    GenServer.call(__MODULE__, {:insert_reading, device_ip, data})
  end

  def query_readings(params) do
    GenServer.call(__MODULE__, {:query_readings, params}, 30_000)
  end

  def get_devices do
    GenServer.call(__MODULE__, :get_devices)
  end

  def get_latest_readings do
    GenServer.call(__MODULE__, :get_latest_readings)
  end

  def query_readings_downsampled(params) do
    GenServer.call(__MODULE__, {:query_readings_downsampled, params}, 30_000)
  end

  def query_readings_count(params) do
    GenServer.call(__MODULE__, {:query_readings_count, params}, 30_000)
  end

  def checkpoint do
    GenServer.call(__MODULE__, :checkpoint)
  end

  def get_readings_count do
    GenServer.call(__MODULE__, :get_readings_count)
  end

  # -- GenServer callbacks --

  @impl true
  def init(opts) do
    db_path = Keyword.fetch!(opts, :db_path)

    {:ok, conn} = Exqlite.Sqlite3.open(db_path)

    :ok = Exqlite.Sqlite3.execute(conn, "PRAGMA journal_mode = WAL")
    :ok = Exqlite.Sqlite3.execute(conn, "PRAGMA busy_timeout = 5000")
    :ok = Exqlite.Sqlite3.execute(conn, "PRAGMA foreign_keys = ON")

    schema_path = Path.join([File.cwd!(), "..", "schema.sql"])
    schema = File.read!(schema_path)
    :ok = Exqlite.Sqlite3.execute(conn, schema)

    # Prepare all statements once
    {:ok, insert_stmt} = Exqlite.Sqlite3.prepare(conn, @insert_sql)
    {:ok, devices_stmt} = Exqlite.Sqlite3.prepare(conn, @devices_sql)
    {:ok, latest_stmt} = Exqlite.Sqlite3.prepare(conn, @latest_sql)
    {:ok, count_stmt} = Exqlite.Sqlite3.prepare(conn, @count_sql)
    {:ok, q_all_range} = Exqlite.Sqlite3.prepare(conn, @q_all_range)
    {:ok, q_all_range_limit} = Exqlite.Sqlite3.prepare(conn, @q_all_range_limit)
    {:ok, q_dev_range} = Exqlite.Sqlite3.prepare(conn, @q_dev_range)
    {:ok, q_dev_range_limit} = Exqlite.Sqlite3.prepare(conn, @q_dev_range_limit)

    {:ok,
     %{
       conn: conn,
       insert_stmt: insert_stmt,
       devices_stmt: devices_stmt,
       latest_stmt: latest_stmt,
       count_stmt: count_stmt,
       q_all_range: q_all_range,
       q_all_range_limit: q_all_range_limit,
       q_dev_range: q_dev_range,
       q_dev_range_limit: q_dev_range_limit
     }}
  end

  @impl true
  def handle_call({:insert_reading, device_ip, data}, _from, state) do
    %{conn: conn, insert_stmt: stmt} = state

    model = Map.get(data, "model", "")
    device_type = if String.starts_with?(model, "I-"), do: "indoor", else: "outdoor"
    serial = data |> Map.get("serialno", "unknown") |> to_string()
    now = System.system_time(:millisecond)

    result =
      try do
        :ok =
          Exqlite.Sqlite3.bind(stmt, [
            now,
            serial,
            device_type,
            device_ip,
            Map.get(data, "pm01"),
            Map.get(data, "pm02"),
            Map.get(data, "pm10"),
            Map.get(data, "pm02Compensated"),
            Map.get(data, "rco2"),
            Map.get(data, "atmp"),
            Map.get(data, "atmpCompensated"),
            Map.get(data, "rhum"),
            Map.get(data, "rhumCompensated"),
            Map.get(data, "tvocIndex"),
            Map.get(data, "noxIndex"),
            Map.get(data, "wifi"),
            Jason.encode!(data)
          ])

        :done = Exqlite.Sqlite3.step(conn, stmt)
        :ok
      rescue
        e -> {:error, Exception.message(e)}
      after
        # Reset the statement so it can be reused regardless of success/failure
        Exqlite.Sqlite3.reset(stmt)
      end

    {:reply, result, state}
  end

  def handle_call({:query_readings, params}, _from, state) do
    %{conn: conn} = state

    device = Map.get(params, :device)
    from_ts = Map.get(params, :from)
    to_ts = Map.get(params, :to)
    limit = Map.get(params, :limit)

    want_device = device != nil and device != "all"

    {stmt, binds} =
      case {want_device, limit} do
        {false, nil} ->
          {state.q_all_range, [from_ts, to_ts]}

        {false, lim} ->
          {state.q_all_range_limit, [from_ts, to_ts, lim]}

        {true, nil} ->
          {state.q_dev_range, [device, from_ts, to_ts]}

        {true, lim} ->
          {state.q_dev_range_limit, [device, from_ts, to_ts, lim]}
      end

    rows = exec_prepared(conn, stmt, binds)
    readings = Enum.map(rows, &row_to_reading/1)
    {:reply, readings, state}
  end

  def handle_call(:get_devices, _from, state) do
    %{conn: conn, devices_stmt: stmt} = state
    rows = exec_prepared(conn, stmt, [])

    devices =
      Enum.map(rows, fn [device_id, device_type, device_ip, last_seen, reading_count] ->
        %{
          device_id: device_id,
          device_type: device_type,
          device_ip: device_ip,
          last_seen: last_seen,
          reading_count: reading_count
        }
      end)

    {:reply, devices, state}
  end

  def handle_call(:get_latest_readings, _from, state) do
    %{conn: conn, latest_stmt: stmt} = state
    rows = exec_prepared(conn, stmt, [])
    readings = Enum.map(rows, &row_to_reading/1)
    {:reply, readings, state}
  end

  def handle_call(:checkpoint, _from, %{conn: conn} = state) do
    :ok = Exqlite.Sqlite3.execute(conn, "PRAGMA wal_checkpoint(TRUNCATE)")
    {:reply, :ok, state}
  end

  def handle_call(:get_readings_count, _from, %{conn: conn, count_stmt: stmt} = state) do
    {:row, [count]} = Exqlite.Sqlite3.step(conn, stmt)
    :done = Exqlite.Sqlite3.step(conn, stmt)
    Exqlite.Sqlite3.reset(stmt)
    {:reply, count, state}
  end

  def handle_call({:query_readings_downsampled, params}, _from, %{conn: conn} = state) do
    bucket_ms = Map.fetch!(params, :bucket_ms)
    device = Map.get(params, :device)
    from_ts = Map.get(params, :from)
    to_ts = Map.get(params, :to)
    limit = Map.get(params, :limit)

    want_device = device != nil and device != "all"

    # bucket_ms is a trusted integer from the downsample_map — safe to interpolate
    bucket_str = Integer.to_string(bucket_ms)

    {where, binds} =
      if want_device do
        {"WHERE device_id = ?1 AND timestamp >= ?2 AND timestamp <= ?3",
         [device, from_ts, to_ts]}
      else
        {"WHERE timestamp >= ?1 AND timestamp <= ?2", [from_ts, to_ts]}
      end

    limit_clause =
      if limit do
        next_idx = length(binds) + 1
        " LIMIT ?#{next_idx}"
      else
        ""
      end

    binds = if limit, do: binds ++ [limit], else: binds

    sql =
      "SELECT (timestamp / #{bucket_str}) * #{bucket_str} AS timestamp, " <>
        "#{@ds_columns}, #{String.trim(@ds_agg_columns)} " <>
        "FROM readings #{where} " <>
        "GROUP BY (timestamp / #{bucket_str}), device_id " <>
        "ORDER BY timestamp ASC" <> limit_clause

    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, sql)
    rows = exec_prepared(conn, stmt, binds)
    Exqlite.Sqlite3.release(conn, stmt)
    readings = Enum.map(rows, &row_to_reading_no_id/1)
    {:reply, readings, state}
  end

  def handle_call({:query_readings_count, params}, _from, %{conn: conn} = state) do
    device = Map.get(params, :device)
    from_ts = Map.get(params, :from)
    to_ts = Map.get(params, :to)

    want_device = device != nil and device != "all"

    {sql, binds} =
      if want_device do
        {"SELECT COUNT(*) FROM readings WHERE device_id = ?1 AND timestamp >= ?2 AND timestamp <= ?3",
         [device, from_ts, to_ts]}
      else
        {"SELECT COUNT(*) FROM readings WHERE timestamp >= ?1 AND timestamp <= ?2",
         [from_ts, to_ts]}
      end

    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, sql)
    :ok = Exqlite.Sqlite3.bind(stmt, binds)
    {:row, [count]} = Exqlite.Sqlite3.step(conn, stmt)
    :done = Exqlite.Sqlite3.step(conn, stmt)
    Exqlite.Sqlite3.release(conn, stmt)
    {:reply, count, state}
  end

  @impl true
  def terminate(_reason, state) do
    %{
      conn: conn,
      insert_stmt: insert_stmt,
      devices_stmt: devices_stmt,
      latest_stmt: latest_stmt,
      count_stmt: count_stmt,
      q_all_range: q1,
      q_all_range_limit: q2,
      q_dev_range: q3,
      q_dev_range_limit: q4
    } = state

    for stmt <- [insert_stmt, devices_stmt, latest_stmt, count_stmt, q1, q2, q3, q4] do
      Exqlite.Sqlite3.release(conn, stmt)
    end

    Exqlite.Sqlite3.close(conn)
    :ok
  end

  # -- Helpers --

  defp exec_prepared(conn, stmt, binds) do
    :ok = Exqlite.Sqlite3.bind(stmt, binds)
    rows = fetch_all_rows(conn, stmt, [])
    Exqlite.Sqlite3.reset(stmt)
    rows
  end

  defp fetch_all_rows(conn, stmt, acc) do
    case Exqlite.Sqlite3.step(conn, stmt) do
      {:row, row} -> fetch_all_rows(conn, stmt, [row | acc])
      :done -> Enum.reverse(acc)
    end
  end

  # SQLite REAL columns return floats even for whole numbers (1.0 instead of 1).
  # Convert whole-number floats to integers to match the canonical JSON output.
  defp maybe_int(nil), do: nil
  defp maybe_int(v) when is_float(v) and trunc(v) == v, do: trunc(v)
  defp maybe_int(v), do: v

  defp row_to_reading([id, ts, dev_id, dev_type, dev_ip | rest]) do
    [pm01, pm02, pm10, pm02c, rco2, atmp, atmpc, rhum, rhumc, tvoc, nox, wifi] = rest

    %{
      id: id,
      timestamp: ts,
      device_id: dev_id,
      device_type: dev_type,
      device_ip: dev_ip,
      pm01: maybe_int(pm01),
      pm02: maybe_int(pm02),
      pm10: maybe_int(pm10),
      pm02_compensated: maybe_int(pm02c),
      rco2: rco2,
      atmp: maybe_int(atmp),
      atmp_compensated: maybe_int(atmpc),
      rhum: maybe_int(rhum),
      rhum_compensated: maybe_int(rhumc),
      tvoc_index: maybe_int(tvoc),
      nox_index: maybe_int(nox),
      wifi: wifi
    }
  end

  defp row_to_reading_no_id([ts, dev_id, dev_type, dev_ip | rest]) do
    [pm01, pm02, pm10, pm02c, rco2, atmp, atmpc, rhum, rhumc, tvoc, nox, wifi] = rest

    %{
      timestamp: ts,
      device_id: dev_id,
      device_type: dev_type,
      device_ip: dev_ip,
      pm01: maybe_int(pm01),
      pm02: maybe_int(pm02),
      pm10: maybe_int(pm10),
      pm02_compensated: maybe_int(pm02c),
      rco2: rco2,
      atmp: maybe_int(atmp),
      atmp_compensated: maybe_int(atmpc),
      rhum: maybe_int(rhum),
      rhum_compensated: maybe_int(rhumc),
      tvoc_index: maybe_int(tvoc),
      nox_index: maybe_int(nox),
      wifi: wifi
    }
  end
end
