defmodule Airgradientz.HttpServer do
  @moduledoc false
  use GenServer

  require Logger

  @max_request_bytes 8192
  @max_connections 128
  @recv_timeout_ms 10_000
  @max_static_file_bytes 16 * 1024 * 1024
  @status_texts %{
    400 => "Bad Request",
    403 => "Forbidden",
    404 => "Not Found",
    405 => "Method Not Allowed",
    413 => "Content Too Large",
    500 => "Internal Server Error"
  }

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(opts) do
    port = Keyword.fetch!(opts, :port)
    config = Keyword.fetch!(opts, :config)

    {:ok, listen_socket} =
      :gen_tcp.listen(port, [
        :binary,
        packet: :raw,
        active: false,
        reuseaddr: true,
        backlog: 128
      ])

    Logger.info("[server] Listening on http://localhost:#{port}")

    pid = self()
    spawn_link(fn -> accept_loop(listen_socket, pid) end)

    {:ok,
     %{
       listen_socket: listen_socket,
       config: config,
       active_connections: 0
     }}
  end

  @impl true
  def handle_info({:new_conn, client_socket}, state) do
    if state.active_connections >= @max_connections do
      :gen_tcp.close(client_socket)
      {:noreply, state}
    else
      config = state.config
      server = self()
      Airgradientz.Stats.increment_active_connections()

      Task.start(fn ->
        try do
          handle_connection(client_socket, config)
          Airgradientz.Stats.increment_requests_served()
        after
          :gen_tcp.close(client_socket)
          Airgradientz.Stats.decrement_active_connections()
          send(server, :conn_closed)
        end
      end)

      {:noreply, %{state | active_connections: state.active_connections + 1}}
    end
  end

  @impl true
  def handle_info(:conn_closed, state) do
    {:noreply, %{state | active_connections: max(state.active_connections - 1, 0)}}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %{listen_socket: ls}) do
    :gen_tcp.close(ls)
    :ok
  end

  # -- Accept loop --

  defp accept_loop(listen_socket, server_pid) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, client_socket} ->
        :gen_tcp.controlling_process(client_socket, server_pid)
        send(server_pid, {:new_conn, client_socket})
        accept_loop(listen_socket, server_pid)

      {:error, :closed} ->
        :ok

      {:error, reason} ->
        Logger.error("[server] Accept error: #{inspect(reason)}")
        accept_loop(listen_socket, server_pid)
    end
  end

  # -- Connection handling --

  defp handle_connection(socket, config) do
    case recv_request(socket) do
      {:ok, method, path, query} ->
        route(socket, method, path, query, config)

      {:error, _reason} ->
        :ok
    end
  end

  # Recv loop: accumulate data until we see \r\n\r\n or hit size cap
  defp recv_request(socket) do
    recv_loop(socket, <<>>, @recv_timeout_ms)
  end

  defp recv_loop(socket, buf, timeout) do
    case :gen_tcp.recv(socket, 0, timeout) do
      {:ok, data} ->
        buf = buf <> data

        cond do
          byte_size(buf) > @max_request_bytes ->
            {:error, :request_too_large}

          String.contains?(buf, "\r\n") ->
            # We have at least the request line -- that's all we need for routing
            parse_request(buf)

          true ->
            recv_loop(socket, buf, timeout)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_request(data) do
    case String.split(data, "\r\n", parts: 2) do
      [request_line | _rest] ->
        case String.split(request_line, " ", parts: 3) do
          [method, raw_path | _version] ->
            {path, query} =
              case String.split(raw_path, "?", parts: 2) do
                [p, q] -> {p, q}
                [p] -> {p, ""}
              end

            {:ok, method, path, query}

          _malformed ->
            {:error, :bad_request}
        end

      _incomplete ->
        {:error, :bad_request}
    end
  end

  # -- Routing --

  defp route(socket, "GET", "/api/readings/latest", _query, _config) do
    readings = Airgradientz.DB.get_latest_readings()
    send_json(socket, readings)
  end

  defp route(socket, "GET", "/api/readings/count", query, _config) do
    params = parse_query_string(query)

    now = System.system_time(:millisecond)
    from_ts = parse_int(Map.get(params, "from")) || 0
    to_ts = parse_int(Map.get(params, "to")) || now
    device = Map.get(params, "device", "all")

    count =
      Airgradientz.DB.query_readings_count(%{
        device: device,
        from: from_ts,
        to: to_ts
      })

    send_json(socket, %{count: count})
  end

  defp route(socket, "GET", "/api/readings", query, config) do
    params = parse_query_string(query)

    now = System.system_time(:millisecond)
    from_ts = parse_int(Map.get(params, "from")) || 0
    to_ts = parse_int(Map.get(params, "to")) || now

    requested_limit = parse_int(Map.get(params, "limit")) || config.max_api_rows

    effective_limit =
      if requested_limit > 0,
        do: min(requested_limit, config.max_api_rows),
        else: config.max_api_rows

    device = Map.get(params, "device", "all")
    downsample_param = Map.get(params, "downsample")

    ds_map = config.downsample_buckets

    cond do
      downsample_param != nil and not Map.has_key?(ds_map, downsample_param) ->
        send_json_error(socket, 400, "Invalid downsample interval")

      downsample_param != nil ->
        bucket_ms = Map.fetch!(ds_map, downsample_param)

        readings =
          Airgradientz.DB.query_readings_downsampled(%{
            device: device,
            from: from_ts,
            to: to_ts,
            limit: effective_limit,
            bucket_ms: bucket_ms
          })

        send_json(socket, readings)

      true ->
        readings =
          Airgradientz.DB.query_readings(%{
            device: device,
            from: from_ts,
            to: to_ts,
            limit: effective_limit
          })

        send_json(socket, readings)
    end
  end

  defp route(socket, "GET", "/api/devices", _query, _config) do
    devices = Airgradientz.DB.get_devices()
    send_json(socket, devices)
  end

  defp route(socket, "GET", "/api/health", _query, _config) do
    health = Airgradientz.Health.get_all()
    send_json(socket, health)
  end

  defp route(socket, "GET", "/api/config", _query, config) do
    payload = %{
      pollIntervalMs: config.poll_interval_ms,
      downsampleBuckets: config.downsample_buckets,
      devices: Enum.map(config.devices, &%{ip: &1.ip, label: &1.label})
    }

    send_json(socket, payload)
  end

  defp route(socket, "GET", "/api/stats", _query, config) do
    counters = Airgradientz.Stats.get_all()
    now = System.system_time(:millisecond)

    memory_rss_bytes =
      case File.read("/proc/self/statm") do
        {:ok, content} ->
          content
          |> String.trim()
          |> String.split()
          |> case do
            [_size, rss | _rest] ->
              case Integer.parse(rss) do
                {pages, _remainder} -> pages * 4096
                :error -> 0
              end

            _other ->
              0
          end

        {:error, _reason} ->
          0
      end

    db_size_bytes =
      case File.stat(config.db_path) do
        {:ok, %{size: size}} -> size
        {:error, _reason} -> 0
      end

    readings_count = Airgradientz.DB.get_readings_count()

    pid =
      case Integer.parse(System.pid()) do
        {n, _remainder} -> n
        :error -> 0
      end

    stats = %{
      implementation: "elixir",
      pid: pid,
      uptime_ms: now - counters.started_at,
      memory_rss_bytes: memory_rss_bytes,
      db_size_bytes: db_size_bytes,
      readings_count: readings_count,
      requests_served: counters.requests_served,
      active_connections: counters.active_connections,
      poll_successes: counters.poll_successes,
      poll_failures: counters.poll_failures,
      pool_alloc_count: 0,
      pool_bytes_used: 0,
      started_at: counters.started_at
    }

    send_json(socket, stats)
  end

  defp route(socket, "GET", "/api/" <> _rest, _query, _config) do
    send_json_error(socket, 404, "Not found")
  end

  defp route(socket, "GET", path, _query, _config) do
    serve_static(socket, path)
  end

  defp route(socket, _method, _path, _query, _config) do
    send_json_error(socket, 405, "Method not allowed")
  end

  # -- Static file serving --

  defp serve_static(socket, path) do
    file_path =
      case path do
        "/" -> "index.html"
        _other -> String.trim_leading(path, "/")
      end

    # URL-decode before path traversal check
    decoded = URI.decode(file_path)

    # Reject path traversal after decoding
    if String.contains?(decoded, "..") or String.contains?(decoded, "\0") do
      send_json_error(socket, 403, "Forbidden")
    else
      full_path = Path.join(public_dir(), decoded)

      # Verify resolved path is still under public_dir
      resolved = Path.expand(full_path)

      if String.starts_with?(resolved, Path.expand(public_dir()) <> "/") or
           resolved == Path.expand(public_dir()) do
        serve_file(socket, resolved, decoded)
      else
        send_json_error(socket, 403, "Forbidden")
      end
    end
  end

  defp serve_file(socket, full_path, file_path) do
    case File.stat(full_path) do
      {:ok, %{size: size}} when size > @max_static_file_bytes ->
        send_json_error(socket, 413, "File too large")

      {:ok, %{type: :regular}} ->
        case File.read(full_path) do
          {:ok, content} ->
            content_type = content_type_for(file_path)
            send_response(socket, 200, "OK", content_type, content, "Cache-Control: public, max-age=600\r\n")

          {:error, _reason} ->
            send_json_error(socket, 404, "Not found")
        end

      _not_found ->
        send_json_error(socket, 404, "Not found")
    end
  end

  defp public_dir do
    Path.join(File.cwd!(), "public")
  end

  # -- Response helpers --

  defp send_response(socket, status_code, status_text, content_type, body, extra_headers \\ "") do
    header =
      "HTTP/1.1 #{status_code} #{status_text}\r\n" <>
        "Connection: close\r\n" <>
        "X-Content-Type-Options: nosniff\r\n" <>
        "X-Frame-Options: DENY\r\n" <>
        "Referrer-Policy: no-referrer\r\n" <>
        extra_headers <>
        "Content-Type: #{content_type}\r\n" <>
        "Content-Length: #{byte_size(body)}\r\n" <>
        "\r\n"

    :gen_tcp.send(socket, [header, body])
  end

  defp send_json(socket, data) do
    body = Airgradientz.Json.encode!(data)
    send_response(socket, 200, "OK", "application/json", body)
  end

  defp send_json_error(socket, status_code, message) do
    body = Airgradientz.Json.encode!(%{error: message})
    status_text = Map.get(@status_texts, status_code, "Error")
    send_response(socket, status_code, status_text, "application/json", body)
  end

  # -- Query string parsing --

  defp parse_query_string(""), do: %{}

  defp parse_query_string(qs) do
    qs
    |> String.split("&")
    |> Enum.reduce(%{}, fn pair, acc ->
      case String.split(pair, "=", parts: 2) do
        [key, value] -> Map.put(acc, URI.decode(key), URI.decode(value))
        [key] -> Map.put(acc, URI.decode(key), "")
      end
    end)
  end

  defp parse_int(nil), do: nil

  defp parse_int(str) do
    case Integer.parse(str) do
      {n, ""} -> n
      {n, _suffix} when is_integer(n) -> n
      _not_a_number -> nil
    end
  end

  # -- Content type detection --

  defp content_type_for(path) do
    case Path.extname(path) do
      ".html" -> "text/html; charset=utf-8"
      ".css" -> "text/css; charset=utf-8"
      ".js" -> "application/javascript; charset=utf-8"
      ".json" -> "application/json; charset=utf-8"
      ".png" -> "image/png"
      ".jpg" -> "image/jpeg"
      ".jpeg" -> "image/jpeg"
      ".svg" -> "image/svg+xml"
      ".ico" -> "image/x-icon"
      _other -> "application/octet-stream"
    end
  end
end
