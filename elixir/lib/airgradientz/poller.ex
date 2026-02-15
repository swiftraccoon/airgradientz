defmodule Airgradientz.Poller do
  @moduledoc false
  use GenServer

  require Logger

  @checkpoint_every 20

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    config = Keyword.fetch!(opts, :config)
    :inets.start()

    Logger.info(
      "[poller] Starting — polling #{length(config.devices)} devices every #{config.poll_interval_ms / 1000}s"
    )

    send(self(), :poll)

    {:ok,
     %{
       config: config,
       poll_count: 0
     }}
  end

  @impl true
  def handle_info(:poll, state) do
    %{config: config} = state

    # Spawn unlinked tasks so a crash in one doesn't take down the Poller
    tasks =
      Enum.map(config.devices, fn device ->
        Task.Supervisor.async_nolink(Airgradientz.TaskSupervisor, fn ->
          fetch_device(device, config.fetch_timeout_ms)
        end)
      end)

    Task.yield_many(tasks, config.fetch_timeout_ms + 5_000)
    |> Enum.each(fn {task, result} ->
      case result do
        nil -> Task.shutdown(task, :brutal_kill)
        {:exit, reason} -> Logger.error("[poller] Task crashed: #{inspect(reason)}")
        _ -> :ok
      end
    end)

    poll_count = state.poll_count + 1

    if rem(poll_count, @checkpoint_every) == 0 do
      try do
        Airgradientz.DB.checkpoint()
      rescue
        e -> Logger.error("[poller] WAL checkpoint failed: #{Exception.message(e)}")
      end
    end

    Process.send_after(self(), :poll, config.poll_interval_ms)
    {:noreply, %{state | poll_count: poll_count}}
  end

  # Ignore unexpected messages
  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # -- Device fetching --

  defp fetch_device(device, timeout_ms) do
    url = ~c"http://#{device.ip}/measures/current"

    case :httpc.request(:get, {url, []}, [timeout: timeout_ms, connect_timeout: timeout_ms], []) do
      {:ok, {{_, status, _}, _headers, body}} when status in 200..299 ->
        handle_response(device, body)

      {:ok, {{_, status, _}, _headers, _body}} ->
        msg = "HTTP #{status}"
        Airgradientz.Health.record_failure(device.ip, msg)
        Airgradientz.Stats.increment_poll_failures()
        Logger.error("[poller] #{device.label} (#{device.ip}): #{msg}")

      {:error, reason} ->
        msg = format_error(reason, timeout_ms)
        Airgradientz.Health.record_failure(device.ip, msg)
        Airgradientz.Stats.increment_poll_failures()
        Logger.error("[poller] #{device.label} (#{device.ip}): fetch failed: #{msg}")
    end
  end

  defp handle_response(device, body) do
    body_str = IO.iodata_to_binary(body)

    case Jason.decode(body_str) do
      {:ok, data} when is_map(data) ->
        case Airgradientz.DB.insert_reading(device.ip, data) do
          :ok ->
            Airgradientz.Health.record_success(device.ip)
            Airgradientz.Stats.increment_poll_successes()

            pm02 = Map.get(data, "pm02", "?")
            rco2 = Map.get(data, "rco2", "?")
            atmp = Map.get(data, "atmp", "?")

            Logger.info(
              "[poller] #{device.label} (#{device.ip}): OK — PM2.5=#{pm02}, CO2=#{rco2}, T=#{atmp}°C"
            )

          {:error, reason} ->
            msg = "DB insert failed: #{reason}"
            Airgradientz.Health.record_failure(device.ip, msg)
            Airgradientz.Stats.increment_poll_failures()
            Logger.error("[poller] #{device.label} (#{device.ip}): #{msg}")
        end

      {:error, %Jason.DecodeError{} = err} ->
        msg = "JSON decode failed: #{Exception.message(err)}"
        Airgradientz.Health.record_failure(device.ip, msg)
        Airgradientz.Stats.increment_poll_failures()
        Logger.error("[poller] #{device.label} (#{device.ip}): #{msg}")

      {:ok, _non_map} ->
        msg = "unexpected response type: expected JSON object"
        Airgradientz.Health.record_failure(device.ip, msg)
        Airgradientz.Stats.increment_poll_failures()
        Logger.error("[poller] #{device.label} (#{device.ip}): #{msg}")
    end
  end

  defp format_error({:failed_connect, _}, timeout_ms), do: "timeout after #{timeout_ms}ms"
  defp format_error(:timeout, timeout_ms), do: "timeout after #{timeout_ms}ms"
  defp format_error(reason, _timeout_ms), do: inspect(reason)
end
