defmodule Airgradientz.Config do
  @moduledoc false

  require Logger

  defstruct port: nil,
            db_path: nil,
            devices: nil,
            poll_interval_ms: nil,
            fetch_timeout_ms: nil,
            max_api_rows: nil,
            downsample_buckets: nil

  @type t :: %__MODULE__{
          port: pos_integer(),
          db_path: String.t(),
          devices: [%{ip: String.t(), label: String.t()}],
          poll_interval_ms: pos_integer(),
          fetch_timeout_ms: pos_integer(),
          max_api_rows: pos_integer(),
          downsample_buckets: %{String.t() => pos_integer()}
        }

  @spec load() :: t()
  def load do
    json = load_config_file!()

    json
    |> build_config()
    |> validate!()
    |> merge_env()
    |> log_config()
  end

  defp load_config_file! do
    candidates =
      Enum.filter(
        [
          System.get_env("CONFIG_PATH"),
          Path.join(File.cwd!(), "airgradientz.json"),
          Path.join([File.cwd!(), "..", "airgradientz.json"])
        ],
        &(&1 != nil)
      )

    result =
      Enum.reduce_while(candidates, :none, fn path, acc ->
        case File.read(path) do
          {:ok, content} ->
            case Jason.decode(content) do
              {:ok, json} when is_map(json) ->
                Logger.info("[config] Loaded config from #{Path.expand(path)}")
                {:halt, {:ok, json}}

              _invalid ->
                {:cont, acc}
            end

          {:error, _reason} ->
            if path == System.get_env("CONFIG_PATH") do
              Logger.warning("[config] CONFIG_PATH set but unreadable: #{path}")
            end

            {:cont, acc}
        end
      end)

    case result do
      {:ok, json} ->
        json

      :none ->
        IO.write(:stderr, "fatal: no config file found (searched: #{Enum.join(candidates, ", ")})\n")
        System.halt(1)
    end
  end

  defp build_config(json) do
    port = get_port(json)
    devices = get_devices(json)
    poll = get_positive_int(json, "pollIntervalMs")
    fetch = get_positive_int(json, "fetchTimeoutMs")
    max_rows = get_positive_int(json, "maxApiRows")
    buckets = get_downsample_buckets(json)

    %__MODULE__{
      port: port,
      db_path: Path.join(File.cwd!(), "airgradientz.db"),
      devices: devices,
      poll_interval_ms: poll,
      fetch_timeout_ms: fetch,
      max_api_rows: max_rows,
      downsample_buckets: buckets
    }
  end

  defp get_port(json) do
    case json do
      %{"ports" => %{"elixir" => port}} when is_number(port) and port > 0 and port <= 65_535 ->
        trunc(port)

      _other ->
        nil
    end
  end

  defp get_devices(%{"devices" => devices}) when is_list(devices) and devices != [] do
    valid =
      Enum.all?(devices, fn d ->
        is_map(d) and is_binary(Map.get(d, "ip")) and Map.get(d, "ip") != "" and
          is_binary(Map.get(d, "label")) and Map.get(d, "label") != ""
      end)

    if valid do
      Enum.map(devices, fn d -> %{ip: d["ip"], label: d["label"]} end)
    else
      nil
    end
  end

  defp get_devices(_json), do: nil

  defp get_positive_int(json, key) do
    case Map.get(json, key) do
      val when is_number(val) and val > 0 -> trunc(val)
      _other -> nil
    end
  end

  defp get_downsample_buckets(%{"downsampleBuckets" => buckets}) when is_map(buckets) and map_size(buckets) > 0 do
    valid = Enum.all?(buckets, fn {k, v} -> is_binary(k) and is_number(v) and v > 0 end)
    if valid, do: Map.new(buckets, fn {k, v} -> {k, trunc(v)} end), else: nil
  end
  defp get_downsample_buckets(_json), do: nil

  defp validate!(config) do
    missing =
      []
      |> check_key(config.poll_interval_ms, "pollIntervalMs")
      |> check_key(config.fetch_timeout_ms, "fetchTimeoutMs")
      |> check_key(config.max_api_rows, "maxApiRows")
      |> check_key(config.downsample_buckets, "downsampleBuckets")
      |> check_key(config.devices, "devices")
      |> check_key(config.port, "ports.elixir")
      |> Enum.reverse()

    if missing != [] do
      IO.write(:stderr, "fatal: missing required config keys: #{Enum.join(missing, ", ")}\n")
      System.halt(1)
    end

    config
  end

  defp check_key(acc, nil, key), do: [key | acc]
  defp check_key(acc, _val, _key), do: acc

  defp merge_env(config) do
    config
    |> maybe_env_port()
    |> maybe_env_db_path()
  end

  defp maybe_env_port(config) do
    case System.get_env("PORT") do
      nil ->
        config

      val ->
        case Integer.parse(val) do
          {port, ""} when port > 0 and port <= 65_535 -> %{config | port: port}
          _invalid -> config
        end
    end
  end

  defp maybe_env_db_path(config) do
    case System.get_env("DB_PATH") do
      nil -> config
      path -> %{config | db_path: path}
    end
  end

  defp log_config(config) do
    device_list = Enum.map_join(config.devices, ", ", &"#{&1.label}(#{&1.ip})")

    Logger.info(
      "[config] port=#{config.port} devices=[#{device_list}] poll=#{config.poll_interval_ms}ms"
    )

    config
  end
end
