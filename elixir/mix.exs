defmodule Airgradientz.MixProject do
  use Mix.Project

  def project do
    [
      app: :airgradientz,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    apps = [:logger, :inets, :ssl, :public_key]

    if Mix.env() == :test do
      [extra_applications: apps]
    else
      [extra_applications: apps, mod: {Airgradientz, []}]
    end
  end

  defp deps do
    [
      {:exqlite, "~> 0.27"},
      {:jason, "~> 1.4"}
    ]
  end
end
