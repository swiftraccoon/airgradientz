defmodule Airgradientz.DB do
  @moduledoc false
  use GenServer

  @query_columns ~w(
    id timestamp device_id device_type device_ip
    pm01 pm02 pm10 pm02_compensated rco2
    atmp atmp_compensated rhum rhum_compensated
    tvoc_index nox_index wifi
  )

  @query_cols Enum.join(@query_columns, ", ")
  @query_cols_aliased @query_columns |> Enum.map_join(", ", &"r.#{&1}")

  @insert_sql """
  INSERT INTO readings (
    timestamp, device_id, device_type, device_ip,
    pm01, pm02, pm10, pm02_compensated,
    rco2, atmp, atmp_compensated, rhum, rhum_compensated,
    tvoc_index, nox_index, wifi, raw_json
  ) VALUES (
    ?1, ?2, ?3, ?4,
    ?5, ?6, ?7, ?8,
    ?9, ?10, ?11, ?12, ?13,
    ?14, ?15, ?16, ?17
  )
  """

  @devices_sql """
  SELECT device_id, device_type, device_ip,
         MAX(timestamp) as last_seen,
         COUNT(*) as reading_count
  FROM readings
  GROUP BY device_id
  ORDER BY device_type
  """

  @latest_sql """
  SELECT #{@query_cols_aliased}
  FROM readings r
  INNER JOIN (
    SELECT device_id, MAX(id) as max_id
    FROM readings
    GROUP BY device_id
  ) latest ON r.id = latest.max_id
  """

  # Pre-built query SQL for all variants
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

  def handle_call(:get_readings_count, _from, %{conn: conn} = state) do
    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, "SELECT COUNT(*) FROM readings")
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
      q_all_range: q1,
      q_all_range_limit: q2,
      q_dev_range: q3,
      q_dev_range_limit: q4
    } = state

    for stmt <- [insert_stmt, devices_stmt, latest_stmt, q1, q2, q3, q4] do
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

  defp row_to_reading([id, ts, dev_id, dev_type, dev_ip | rest]) do
    [pm01, pm02, pm10, pm02c, rco2, atmp, atmpc, rhum, rhumc, tvoc, nox, wifi] = rest

    %{
      id: id,
      timestamp: ts,
      device_id: dev_id,
      device_type: dev_type,
      device_ip: dev_ip,
      pm01: pm01,
      pm02: pm02,
      pm10: pm10,
      pm02_compensated: pm02c,
      rco2: rco2,
      atmp: atmp,
      atmp_compensated: atmpc,
      rhum: rhum,
      rhum_compensated: rhumc,
      tvoc_index: tvoc,
      nox_index: nox,
      wifi: wifi
    }
  end
end
