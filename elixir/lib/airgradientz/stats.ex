defmodule Airgradientz.Stats do
  @moduledoc false
  use Agent

  def start_link(_opts) do
    Agent.start_link(
      fn ->
        %{
          started_at: System.system_time(:millisecond),
          requests_served: 0,
          active_connections: 0,
          poll_successes: 0,
          poll_failures: 0
        }
      end,
      name: __MODULE__
    )
  end

  def get_started_at do
    Agent.get(__MODULE__, & &1.started_at)
  end

  def increment_requests_served do
    Agent.update(__MODULE__, fn state ->
      %{state | requests_served: state.requests_served + 1}
    end)
  end

  def increment_active_connections do
    Agent.update(__MODULE__, fn state ->
      %{state | active_connections: state.active_connections + 1}
    end)
  end

  def decrement_active_connections do
    Agent.update(__MODULE__, fn state ->
      %{state | active_connections: max(state.active_connections - 1, 0)}
    end)
  end

  def increment_poll_successes do
    Agent.update(__MODULE__, fn state ->
      %{state | poll_successes: state.poll_successes + 1}
    end)
  end

  def increment_poll_failures do
    Agent.update(__MODULE__, fn state ->
      %{state | poll_failures: state.poll_failures + 1}
    end)
  end

  def get_all do
    Agent.get(__MODULE__, & &1)
  end
end
