defmodule Airgradientz.LogFormatter do
  @moduledoc false

  @doc """
  Formats log messages as: [YYYY-MM-DD HH:MM:SS] message\\n
  """
  def format(_level, message, _timestamp, _metadata) do
    now = :calendar.local_time()
    {{year, month, day}, {hour, minute, second}} = now

    ts =
      :io_lib.format("[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B]", [
        year,
        month,
        day,
        hour,
        minute,
        second
      ])

    [ts, " ", message, "\n"]
  rescue
    _err -> [message, "\n"]
  end
end
