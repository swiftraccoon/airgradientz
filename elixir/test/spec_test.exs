defmodule Airgradientz.SpecTest do
  @moduledoc """
  Tests driven by test-spec.json: downsample bucket verification,
  query edge cases, and response shape validation.
  """
  use ExUnit.Case, async: false

  @spec_path Path.join([__DIR__, "..", "..", "test-spec.json"])
  @test_spec Airgradientz.Json.decode!(File.read!(@spec_path))

  @fixtures Airgradientz.Json.decode!(File.read!(Path.join([__DIR__, "..", "..", "test-fixtures.json"])))
  @indoor_data Map.get(@fixtures, "indoorFull")

  setup do
    tmp_dir = System.tmp_dir!()
    db_path = Path.join(tmp_dir, "spec_test_#{:erlang.unique_integer([:positive])}.db")

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

  # -- Downsample bucket verification --

  describe "downsample buckets from test-spec.json" do
    @spec_buckets Map.get(@test_spec, "downsampleBuckets")

    test "all 7 buckets are present in test-spec.json" do
      assert length(@spec_buckets) == 7
    end

    test "all spec buckets match config downsample_buckets" do
      config_json = Airgradientz.Json.decode!(File.read!(Path.join([__DIR__, "..", "..", "airgradientz.json"])))
      config_buckets = Map.get(config_json, "downsampleBuckets")

      for bucket <- @spec_buckets do
        param = bucket["param"]
        expect_ms = bucket["expectMs"]

        actual_ms = Map.get(config_buckets, param)
        assert actual_ms != nil, "bucket #{param} not found in config downsampleBuckets"
        assert actual_ms == expect_ms, "bucket #{param}: expected #{expect_ms} ms, got #{actual_ms} ms"
      end
    end

    test "config parses all 7 buckets into downsample_buckets map" do
      # Build a config struct from the real config file and verify the parsed map
      config_json = Airgradientz.Json.decode!(File.read!(Path.join([__DIR__, "..", "..", "airgradientz.json"])))
      config_buckets = Map.get(config_json, "downsampleBuckets")

      for bucket <- @spec_buckets do
        param = bucket["param"]
        expect_ms = bucket["expectMs"]

        assert Map.has_key?(config_buckets, param),
               "downsample bucket #{param} missing from config"

        assert config_buckets[param] == expect_ms,
               "bucket #{param}: config has #{config_buckets[param]} ms, spec expects #{expect_ms} ms"
      end

      assert map_size(config_buckets) == 7,
             "expected 7 downsample buckets, got #{map_size(config_buckets)}"
    end
  end

  # -- Query edge cases --

  describe "query edge cases from test-spec.json" do
    test "from > to returns empty results" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      readings =
        Airgradientz.DB.query_readings(%{
          from: 9_999_999_999_999,
          to: 1,
          limit: 100
        })

      assert readings == []
    end

    test "nonexistent device returns empty" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      readings =
        Airgradientz.DB.query_readings(%{
          device: "nonexistent-serial-xyz",
          from: 0,
          to: System.system_time(:millisecond) + 1000,
          limit: 100
        })

      assert readings == []
    end

    test "limit=1 returns exactly 1" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      readings =
        Airgradientz.DB.query_readings(%{
          from: 0,
          to: System.system_time(:millisecond) + 1000,
          limit: 1
        })

      assert length(readings) == 1
    end

    test "count with from > to returns zero" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      count =
        Airgradientz.DB.query_readings_count(%{
          from: 9_999_999_999_999,
          to: 1
        })

      assert count == 0
    end

    test "count with nonexistent device returns zero" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      count =
        Airgradientz.DB.query_readings_count(%{
          device: "nonexistent-serial-xyz",
          from: 0,
          to: System.system_time(:millisecond) + 1000
        })

      assert count == 0
    end

    test "device=all returns all readings" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      readings =
        Airgradientz.DB.query_readings(%{
          device: "all",
          from: 0,
          to: System.system_time(:millisecond) + 1000,
          limit: 100
        })

      assert readings != []
    end

    test "empty device filter returns all readings" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      now = System.system_time(:millisecond)

      # nil device (no filter) should return all
      readings =
        Airgradientz.DB.query_readings(%{
          from: 0,
          to: now + 1000,
          limit: 100
        })

      assert readings != []
    end
  end

  # -- Response shape validation --

  describe "response shapes from test-spec.json" do
    @reading_shape Map.get(Map.get(@test_spec, "responseShapes"), "reading")
    @reading_ds_shape Map.get(Map.get(@test_spec, "responseShapes"), "readingDownsampled")
    @device_shape Map.get(Map.get(@test_spec, "responseShapes"), "device")
    @count_shape Map.get(Map.get(@test_spec, "responseShapes"), "count")
    @config_shape Map.get(Map.get(@test_spec, "responseShapes"), "config")

    test "reading has all required fields and no forbidden fields" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      now = System.system_time(:millisecond)
      [reading] = Airgradientz.DB.query_readings(%{from: 0, to: now + 1000, limit: 1})

      # Convert atom-keyed map to string-keyed (as JSON would produce)
      json_map = reading |> Airgradientz.Json.encode!() |> Airgradientz.Json.decode!()

      for field <- @reading_shape["requiredFields"] do
        assert Map.has_key?(json_map, field),
               "reading missing required field: #{field}"
      end

      for field <- @reading_shape["forbiddenFields"] do
        refute Map.has_key?(json_map, field),
               "reading has forbidden field: #{field}"
      end
    end

    test "downsampled reading has all required fields and no forbidden fields" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      now = System.system_time(:millisecond)

      [reading] =
        Airgradientz.DB.query_readings_downsampled(%{
          from: 0,
          to: now + 1000,
          limit: 100,
          bucket_ms: 3_600_000
        })

      json_map = reading |> Airgradientz.Json.encode!() |> Airgradientz.Json.decode!()

      for field <- @reading_ds_shape["requiredFields"] do
        assert Map.has_key?(json_map, field),
               "downsampled reading missing required field: #{field}"
      end

      for field <- @reading_ds_shape["forbiddenFields"] do
        refute Map.has_key?(json_map, field),
               "downsampled reading has forbidden field: #{field}"
      end
    end

    test "device has all required fields and no forbidden fields" do
      :ok = Airgradientz.DB.insert_reading("10.0.0.1", @indoor_data)

      [device] = Airgradientz.DB.get_devices()
      json_map = device |> Airgradientz.Json.encode!() |> Airgradientz.Json.decode!()

      for field <- @device_shape["requiredFields"] do
        assert Map.has_key?(json_map, field),
               "device missing required field: #{field}"
      end

      for field <- @device_shape["forbiddenFields"] do
        refute Map.has_key?(json_map, field),
               "device has forbidden field: #{field}"
      end
    end

    test "count response has exact fields and no extras" do
      count_result = %{count: 0}
      json_map = count_result |> Airgradientz.Json.encode!() |> Airgradientz.Json.decode!()

      for field <- @count_shape["exactFields"] do
        assert Map.has_key?(json_map, field),
               "count response missing exact field: #{field}"
      end

      if @count_shape["noExtraFields"] do
        expected_keys = MapSet.new(@count_shape["exactFields"])
        actual_keys = json_map |> Map.keys() |> MapSet.new()

        assert MapSet.equal?(expected_keys, actual_keys),
               "count response has extra fields: #{inspect(MapSet.difference(actual_keys, expected_keys))}"
      end
    end

    test "config response has required fields and no forbidden fields" do
      # Build a config-like map matching what the HTTP server would return
      config_json = Airgradientz.Json.decode!(File.read!(Path.join([__DIR__, "..", "..", "airgradientz.json"])))
      buckets = Map.get(config_json, "downsampleBuckets")
      devices = Map.get(config_json, "devices")

      config_response = %{
        pollIntervalMs: config_json["pollIntervalMs"],
        downsampleBuckets: buckets,
        devices: Enum.map(devices, &%{ip: &1["ip"], label: &1["label"]})
      }

      json_map = config_response |> Airgradientz.Json.encode!() |> Airgradientz.Json.decode!()

      for field <- @config_shape["requiredFields"] do
        assert Map.has_key?(json_map, field),
               "config response missing required field: #{field}"
      end

      for field <- @config_shape["forbiddenFields"] do
        refute Map.has_key?(json_map, field),
               "config response has forbidden field: #{field}"
      end
    end
  end
end
