defmodule Airgradientz.DBTest do
  use ExUnit.Case, async: false

  @indoor_data %{
    "wifi" => -47,
    "serialno" => "abcdef123456",
    "model" => "I-9PSL",
    "pm01" => 3,
    "pm02" => 5,
    "pm10" => 7,
    "pm02Compensated" => 6,
    "rco2" => 450,
    "atmp" => 22.5,
    "atmpCompensated" => 23.1,
    "rhum" => 45.0,
    "rhumCompensated" => 44.2,
    "tvocIndex" => 120,
    "noxIndex" => 15
  }

  @outdoor_data %{
    "wifi" => -55,
    "serialno" => "outdoor123",
    "model" => "O-1PST",
    "pm01" => 10,
    "pm02" => 15,
    "pm10" => 20,
    "rco2" => 400,
    "atmp" => 18.3,
    "rhum" => 60.0
  }

  @null_fields_data %{
    "wifi" => -40,
    "serialno" => "boot123",
    "model" => "I-9PSL"
  }

  @zero_compensated_data %{
    "wifi" => -45,
    "serialno" => "zero123",
    "model" => "I-9PSL",
    "pm02Compensated" => 0,
    "atmpCompensated" => 0,
    "rhumCompensated" => 0
  }

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
        :exit, _ -> :ok
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
    assert r.device_id == "abcdef123456"
    assert r.device_ip == "192.168.1.1"
    assert r.pm02 == 5
    assert r.rco2 == 450
    assert r.atmp == 22.5
  end

  test "device type classification" do
    :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
    :ok = Airgradientz.DB.insert_reading("10.0.0.2", @outdoor_data)

    now = System.system_time(:millisecond)
    readings = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 100})

    types = Enum.map(readings, & &1.device_type) |> Enum.sort()
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
    assert r.wifi == -40
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
        device: "abcdef123456",
        from: 0,
        to: now + 1000,
        limit: 100
      })

    assert length(readings) == 1
    assert hd(readings).device_id == "abcdef123456"
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

    ids = Enum.map(devices, & &1.device_id) |> Enum.sort()
    assert ids == ["abcdef123456", "outdoor123"]
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
end
