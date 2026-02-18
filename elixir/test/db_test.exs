defmodule Airgradientz.DBTest do
  use ExUnit.Case, async: false

  @fixtures Jason.decode!(File.read!(Path.join([__DIR__, "..", "..", "test-fixtures.json"])))
  @indoor_data Map.get(@fixtures, "indoorFull")
  @outdoor_data Map.get(@fixtures, "outdoorFull")
  @null_fields_data Map.get(@fixtures, "afterBoot")
  @zero_compensated_data Map.get(@fixtures, "zeroCompensated")

  @no_serial_data %{
    "wifi" => -30,
    "model" => "I-9PSL",
    "pm02" => 5
  }

  setup do
    tmp_dir = System.tmp_dir!()
    db_path = Path.join(tmp_dir, "test_#{:erlang.unique_integer([:positive])}.db")

    {:ok, _pid} = Airgradientz.DB.start_link(db_path: db_path)

    on_exit(fn ->
      try do
        GenServer.stop(Airgradientz.DB)
      catch
        :exit, _reason -> :ok
      end

      File.rm(db_path)
      File.rm(db_path <> "-wal")
      File.rm(db_path <> "-shm")
    end)

    :ok
  end

  test "insert and query indoor reading" do
    :ok = Airgradientz.DB.insert_reading("192.168.1.1", @indoor_data)

    now = System.system_time(:millisecond)
    readings = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert length(readings) == 1
    r = hd(readings)
    assert r.device_type == "indoor"
    assert r.device_id == "84fce602549c"
    assert r.device_ip == "192.168.1.1"
    assert r.pm02 == 41.67
    assert r.rco2 == 489
    assert r.atmp == 20.78
  end

  test "device type classification" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)
    readings = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    types = readings |> Enum.map(& &1.device_type) |> Enum.sort()
    assert types == ["indoor", "outdoor"]
  end

  test "null fields handling" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @null_fields_data)

    now = System.system_time(:millisecond)
    [r] = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert r.pm01 == nil
    assert r.pm02 == nil
    assert r.rco2 == nil
    assert r.atmp == nil
    assert r.wifi == -59
  end

  test "zero compensated values are not null" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @zero_compensated_data)

    now = System.system_time(:millisecond)
    [r] = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert r.pm02_compensated == 0.0 or r.pm02_compensated == 0
    assert r.atmp_compensated == 0.0 or r.atmp_compensated == 0
    assert r.rhum_compensated == 0.0 or r.rhum_compensated == 0
  end

  test "missing serialno defaults to unknown" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @no_serial_data)

    now = System.system_time(:millisecond)
    [r] = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert r.device_id == "unknown"
  end

  test "query with device filter" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)

    readings =
      Airgradientz.DB.query_readings(%{
        device: "84fce602549c",
        from: 0,
        to: now + 1000,
        limit: 100
      })

    assert length(readings) == 1
    assert hd(readings).device_id == "84fce602549c"
  end

  test "query with device=all returns all" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)

    readings =
      Airgradientz.DB.query_readings(%{device: "all", from: 0, to: now + 1000, limit: 100})

    assert length(readings) == 2
  end

  test "query with limit" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)
    readings = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 1})

    assert length(readings) == 1
  end

  test "getLatestReadings returns one per device" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

    latest = Airgradientz.DB.get_latest_readings()
    assert length(latest) == 2
  end

  test "getDevices returns unique devices" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    devices = Airgradientz.DB.get_devices()
    assert length(devices) == 2

    ids = devices |> Enum.map(& &1.device_id) |> Enum.sort()
    assert ids == ["84fce602549c", "ecda3b1d09d8"]
  end

  test "getReadingsCount" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    assert Airgradientz.DB.get_readings_count() == 2
  end

  test "empty query returns empty" do
    now = System.system_time(:millisecond)
    readings = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert readings == []
  end

  test "checkpoint does not error" do
    assert Airgradientz.DB.checkpoint() == :ok
  end

  test "downsampled query groups readings into buckets" do
    # Insert two readings for the same device with timestamps in the same 1h bucket
    bucket_ms = 3_600_000

    # We need to directly insert with controlled timestamps, so use the DB GenServer
    # Insert two readings manually via insert_reading (timestamps are auto-generated)
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    Process.sleep(10)
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

    now = System.system_time(:millisecond)

    # Without downsampling, we get 2 rows
    readings =
      Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    assert length(readings) == 2

    # With downsampling at 1h bucket, both should collapse into 1 row
    downsampled =
      Airgradientz.DB.query_readings_downsampled(%{
        from: 0,
        to: now + 1000,
        limit: 100,
        bucket_ms: bucket_ms
      })

    assert length(downsampled) == 1
    r = hd(downsampled)
    # Downsampled rows should not have an :id field
    refute Map.has_key?(r, :id)
    # Should have averaged values
    assert r.device_id == "84fce602549c"
    assert r.pm02 != nil
  end

  test "query_readings_count returns filtered count" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

    now = System.system_time(:millisecond)

    # Count all
    count =
      Airgradientz.DB.query_readings_count(%{from: 0, to: now + 1000})

    assert count == 3

    # Count filtered by device
    count_filtered =
      Airgradientz.DB.query_readings_count(%{
        device: "84fce602549c",
        from: 0,
        to: now + 1000
      })

    assert count_filtered == 2
  end

  test "count with from=0 returns all readings (not just last 24h)" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)

    # With from=0 (API default), all readings should be counted
    count = Airgradientz.DB.query_readings_count(%{from: 0, to: now + 1000})
    assert count == 2

    # With from=now (simulates what the old from=now-24h bug would return for old data)
    count_recent = Airgradientz.DB.query_readings_count(%{from: now + 500, to: now + 1000})
    assert count_recent == 0
  end

  test "devices response has no first_seen field" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

    devices = Airgradientz.DB.get_devices()
    assert length(devices) == 1
    device = hd(devices)

    # API spec: devices have device_id, device_type, device_ip, last_seen, reading_count
    assert Map.has_key?(device, :device_id)
    assert Map.has_key?(device, :last_seen)
    assert Map.has_key?(device, :reading_count)
    refute Map.has_key?(device, :first_seen)
  end

  test "query_readings_count with device=all returns all" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)

    count =
      Airgradientz.DB.query_readings_count(%{
        device: "all",
        from: 0,
        to: now + 1000
      })

    assert count == 2
  end
end
