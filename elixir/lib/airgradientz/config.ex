defmodule Airgradientz.Config do
  @moduledoc false

  defstruct port: 3013,
            db_path: Path.join(File.cwd!(), "airgradientz.db"),
            devices: [
              %{ip: "192.168.88.6", label: "outdoor"},
              %{ip: "192.168.88.159", label: "indoor"}
            ],
            poll_interval_ms: 15_000,
            fetch_timeout_ms: 5_000,
            max_api_rows: 10_000

  @type t :: %__MODULE__{
          port: pos_integer(),
          db_path: String.t(),
          devices: [%{ip: String.t(), label: String.t()}],
          poll_interval_ms: pos_integer(),
          fetch_timeout_ms: pos_integer(),
          max_api_rows: pos_integer()
        }

  @spec load() :: t()
  def load do
    config = %__MODULE__{}

    config
    |> merge_file()
    |> merge_env()
    |> log_config()
  end

  defp merge_file(config) do
    case find_config_file() do
      {:ok, path, json} ->
        :logger.info(~c"[config] Loaded config from #{path}")
        apply_json(config, json)

      :none ->
        :logger.info(~c"[config] No config file found, using defaults")
        config
    end
  end

  defp find_config_file do
    candidates =
      [
        System.get_env("CONFIG_PATH"),
        Path.join(File.cwd!(), "airgradientz.json"),
        Path.join([File.cwd!(), "..", "airgradientz.json"])
      ]
      |> Enum.filter(&(&1 != nil))

    Enum.reduce_while(candidates, :none, fn path, acc ->
      case File.read(path) do
        {:ok, content} ->
          case Jason.decode(content) do
            {:ok, json} when is_map(json) -> {:halt, {:ok, path, json}}
            _ -> {:cont, acc}
          end

        {:error, _} ->
          if path == System.get_env("CONFIG_PATH") do
            :logger.warning(~c"[config] CONFIG_PATH set but unreadable: #{path}")
          end

          {:cont, acc}
      end
    end)
  end

  defp apply_json(config, json) do
    config
    |> maybe_put_port(json)
    |> maybe_put_devices(json)
    |> maybe_put_positive_int(:poll_interval_ms, json, "pollIntervalMs")
    |> maybe_put_positive_int(:fetch_timeout_ms, json, "fetchTimeoutMs")
    |> maybe_put_positive_int(:max_api_rows, json, "maxApiRows")
  end

  defp maybe_put_port(config, %{"ports" => %{"elixir" => port}})
       when is_number(port) and port > 0 and port <= 65535 do
    %{config | port: trunc(port)}
  end

  defp maybe_put_port(config, _json), do: config

  defp maybe_put_devices(config, %{"devices" => devices}) when is_list(devices) and devices != [] do
    parsed =
      Enum.map(devices, fn d ->
        %{ip: Map.get(d, "ip", ""), label: Map.get(d, "label", "")}
      end)

    %{config | devices: parsed}
  end

  defp maybe_put_devices(config, _json), do: config

  defp maybe_put_positive_int(config, key, json, json_key) do
    case Map.get(json, json_key) do
      val when is_number(val) and val > 0 -> Map.put(config, key, trunc(val))
      _ -> config
    end
  end

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
          {port, ""} when port > 0 and port <= 65535 -> %{config | port: port}
          _ -> config
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

    :logger.info(
      ~c"[config] port=#{config.port} devices=[#{device_list}] poll=#{config.poll_interval_ms}ms"
    )

    config
  end
end
