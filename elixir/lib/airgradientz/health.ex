defmodule Airgradientz.Health do
  @moduledoc false
  use Agent

  def start_link(opts) do
    devices = Keyword.fetch!(opts, :devices)

    initial =
      Map.new(devices, fn %{ip: ip, label: label} ->
        {ip, new_entry(ip, label)}
      end)

    Agent.start_link(fn -> initial end, name: __MODULE__)
  end

  def record_success(ip) do
    Agent.update(__MODULE__, fn state ->
      case Map.fetch(state, ip) do
        {:ok, h} ->
          Map.put(state, ip, %{
            h
            | status: "ok",
              lastSuccess: System.system_time(:millisecond),
              lastErrorMessage: nil,
              consecutiveFailures: 0
          })

        :error ->
          state
      end
    end)
  end

  def record_failure(ip, message) do
    Agent.update(__MODULE__, fn state ->
      case Map.fetch(state, ip) do
        {:ok, h} ->
          Map.put(state, ip, %{
            h
            | status: "error",
              lastError: System.system_time(:millisecond),
              lastErrorMessage: message,
              consecutiveFailures: h.consecutiveFailures + 1
          })

        :error ->
          state
      end
    end)
  end

  def get_all do
    Agent.get(__MODULE__, fn state -> Map.values(state) end)
  end

  defp new_entry(ip, label) do
    %{
      ip: ip,
      label: label,
      status: "unknown",
      lastSuccess: nil,
      lastError: nil,
      lastErrorMessage: nil,
      consecutiveFailures: 0
    }
  end
end
