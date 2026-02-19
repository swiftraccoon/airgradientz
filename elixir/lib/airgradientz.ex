defmodule Airgradientz do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    Logger.configure_backend(:console,
      format: {Airgradientz.LogFormatter, :format},
      metadata: []
    )

    config = Airgradientz.Config.load()

    children = [
      {Airgradientz.DB, [db_path: config.db_path]},
      {Airgradientz.Health, [devices: config.devices]},
      {Airgradientz.Stats, []},
      {Task.Supervisor, name: Airgradientz.TaskSupervisor},
      {Airgradientz.Poller, [config: config]},
      {Airgradientz.HttpServer, [port: config.port, config: config]}
    ]

    opts = [strategy: :one_for_one, name: Airgradientz.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
