defmodule Airgradientz.Json do
  @moduledoc false

  # -- Encoding --
  #
  # All internal encode functions return iodata (nested lists of binaries and
  # integers).  Only the public `encode/1` and `encode!/1` flatten to a binary
  # at the very end via `IO.iodata_to_binary/1`.  This avoids O(n^2) binary
  # concatenation when building large JSON payloads.

  @spec encode!(term()) :: String.t()
  def encode!(value) do
    case encode(value) do
      {:ok, json} -> json
      {:error, reason} -> raise ArgumentError, "JSON encode error: #{inspect(reason)}"
    end
  end

  @spec encode(term()) :: {:ok, String.t()} | {:error, term()}
  def encode(value) do
    {:ok, IO.iodata_to_binary(encode_iodata(value))}
  rescue
    e -> {:error, Exception.message(e)}
  end

  @compile {:inline, encode_iodata: 1, encode_key_iodata: 1}

  defp encode_iodata(nil), do: "null"
  defp encode_iodata(true), do: "true"
  defp encode_iodata(false), do: "false"
  defp encode_iodata(:null), do: "null"

  defp encode_iodata(v) when is_atom(v), do: encode_string_iodata(Atom.to_string(v))

  defp encode_iodata(v) when is_integer(v), do: Integer.to_string(v)

  defp encode_iodata(v) when is_float(v) do
    if v == Float.round(v, 0) and abs(v) < 1.0e15 do
      int_part = trunc(v)

      if v == int_part * 1.0 do
        [Integer.to_string(int_part), ".0"]
      else
        float_to_string(v)
      end
    else
      float_to_string(v)
    end
  end

  defp encode_iodata(v) when is_binary(v), do: encode_string_iodata(v)

  defp encode_iodata(v) when is_map(v) do
    [?{, encode_map_pairs(Map.to_list(v)), ?}]
  end

  defp encode_iodata(v) when is_list(v) do
    [?[, encode_list_elems(v), ?]]
  end

  # Encode map key-value pairs with comma separators (no leading comma).
  defp encode_map_pairs([]), do: []

  defp encode_map_pairs([{k, val} | rest]) do
    pair = [encode_key_iodata(k), ?:, encode_iodata(val)]
    encode_map_pairs_rest(rest, [pair])
  end

  defp encode_map_pairs_rest([], acc), do: Enum.reverse(acc)

  defp encode_map_pairs_rest([{k, val} | rest], acc) do
    pair = [?,, encode_key_iodata(k), ?:, encode_iodata(val)]
    encode_map_pairs_rest(rest, [pair | acc])
  end

  # Encode list elements with comma separators.
  defp encode_list_elems([]), do: []

  defp encode_list_elems([head | tail]) do
    encode_list_elems_rest(tail, [encode_iodata(head)])
  end

  defp encode_list_elems_rest([], acc), do: Enum.reverse(acc)

  defp encode_list_elems_rest([head | tail], acc) do
    encode_list_elems_rest(tail, [[?,, encode_iodata(head)] | acc])
  end

  # Convert a map key to an iodata-quoted JSON string.
  defp encode_key_iodata(k) when is_binary(k), do: encode_string_iodata(k)
  defp encode_key_iodata(k) when is_atom(k), do: encode_string_iodata(Atom.to_string(k))
  defp encode_key_iodata(k), do: encode_string_iodata(to_string(k))

  # Wrap a string value in quotes, escaping special characters.
  # Returns iodata.
  defp encode_string_iodata(str) do
    [?", escape_string_iodata(str, 0, 0), ?"]
  end

  # Chunk-based string escaping.  We scan forward counting safe bytes and emit
  # them in a single binary slice, only stopping when we hit a byte that needs
  # escaping.  This is O(n) with minimal allocations.
  #
  # `original` is the full input binary (never mutated).
  # `skip`     is the byte offset of the current safe-chunk start.
  # `len`      is the number of safe bytes accumulated so far.

  defp escape_string_iodata(original, skip, len) do
    remaining = byte_size(original) - skip - len

    if remaining <= 0 do
      # No more bytes -- emit the final safe chunk.
      if skip == 0 and len == byte_size(original) do
        # Entire string is safe -- return it directly, zero copies.
        original
      else
        binary_part(original, skip, len)
      end
    else
      <<_prefix::binary-size(skip + len), byte, _rest::binary>> = original
      escape_byte(byte, original, skip, len)
    end
  end

  defp escape_byte(?", original, skip, len), do: emit_escape(original, skip, len, "\\\"")
  defp escape_byte(?\\, original, skip, len), do: emit_escape(original, skip, len, "\\\\")
  defp escape_byte(?\n, original, skip, len), do: emit_escape(original, skip, len, "\\n")
  defp escape_byte(?\r, original, skip, len), do: emit_escape(original, skip, len, "\\r")
  defp escape_byte(?\t, original, skip, len), do: emit_escape(original, skip, len, "\\t")

  defp escape_byte(byte, original, skip, len) when byte < 0x20 do
    emit_escape(original, skip, len, escape_control(byte))
  end

  defp escape_byte(_byte, original, skip, len) do
    # Safe byte -- extend the current chunk.
    escape_string_iodata(original, skip, len + 1)
  end

  # Emit the accumulated safe chunk (if any) followed by the escape sequence,
  # then continue scanning from the byte after the escaped one.
  defp emit_escape(original, skip, 0, replacement) do
    [replacement, escape_string_iodata(original, skip + 1, 0)]
  end

  defp emit_escape(original, skip, len, replacement) do
    [binary_part(original, skip, len), replacement, escape_string_iodata(original, skip + len + 1, 0)]
  end

  defp escape_control(c) do
    hex = String.pad_leading(Integer.to_string(c, 16), 4, "0")
    <<"\\u", hex::binary>>
  end

  defp float_to_string(v) do
    :erlang.float_to_binary(v, [:short])
  end

  # -- Decoding --

  @spec decode!(String.t()) :: term()
  def decode!(str) do
    case decode(str) do
      {:ok, value} -> value
      {:error, reason} -> raise ArgumentError, "JSON decode error: #{inspect(reason)}"
    end
  end

  @spec decode(String.t()) :: {:ok, term()} | {:error, term()}
  def decode(str) when is_binary(str) do
    trimmed = String.trim(str)

    case parse_value(trimmed) do
      {:ok, value, rest} ->
        if String.trim(rest) == "" do
          {:ok, value}
        else
          {:error, "unexpected trailing content"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def decode(_other), do: {:error, "expected string input"}

  defp parse_value(<<>>), do: {:error, "unexpected end of input"}

  defp parse_value(str) do
    str = skip_whitespace(str)

    case str do
      <<"\"", _rest::binary>> -> parse_string(str)
      <<"{", _rest::binary>> -> parse_object(str)
      <<"[", _rest::binary>> -> parse_array(str)
      <<"true", rest::binary>> -> {:ok, true, rest}
      <<"false", rest::binary>> -> {:ok, false, rest}
      <<"null", rest::binary>> -> {:ok, nil, rest}
      <<c, _rest::binary>> when c in [?-, ?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9] ->
        parse_number(str)
      <<>> -> {:error, "unexpected end of input"}
      _other -> {:error, "unexpected character: #{String.first(str)}"}
    end
  end

  defp parse_string(<<"\"", rest::binary>>) do
    parse_string_contents(rest, <<>>)
  end

  defp parse_string_contents(<<>>, _acc), do: {:error, "unterminated string"}

  defp parse_string_contents(<<"\"", rest::binary>>, acc) do
    {:ok, acc, rest}
  end

  defp parse_string_contents(<<"\\", rest::binary>>, acc) do
    parse_escape(rest, acc)
  end

  defp parse_string_contents(<<c, rest::binary>>, acc) do
    parse_string_contents(rest, <<acc::binary, c>>)
  end

  defp parse_escape(<<>>, _acc), do: {:error, "unterminated escape"}

  defp parse_escape(<<c, rest::binary>>, acc) do
    case c do
      ?" -> parse_string_contents(rest, <<acc::binary, ?">>)
      ?\\ -> parse_string_contents(rest, <<acc::binary, ?\\>>)
      ?/ -> parse_string_contents(rest, <<acc::binary, ?/>>)
      ?n -> parse_string_contents(rest, <<acc::binary, ?\n>>)
      ?r -> parse_string_contents(rest, <<acc::binary, ?\r>>)
      ?t -> parse_string_contents(rest, <<acc::binary, ?\t>>)
      ?b -> parse_string_contents(rest, <<acc::binary, ?\b>>)
      ?f -> parse_string_contents(rest, <<acc::binary, ?\f>>)
      ?u -> parse_unicode_escape(rest, acc)
      _other -> {:error, "invalid escape: \\#{<<c>>}"}
    end
  end

  defp parse_unicode_escape(str, acc) when byte_size(str) >= 4 do
    <<hex::binary-size(4), rest::binary>> = str

    case Integer.parse(hex, 16) do
      {codepoint, ""} ->
        if codepoint >= 0xD800 and codepoint <= 0xDBFF do
          parse_surrogate_pair(codepoint, rest, acc)
        else
          char = <<codepoint::utf8>>
          parse_string_contents(rest, <<acc::binary, char::binary>>)
        end

      _invalid ->
        {:error, "invalid unicode escape: \\u#{hex}"}
    end
  end

  defp parse_unicode_escape(_str, _acc), do: {:error, "incomplete unicode escape"}

  defp parse_surrogate_pair(high, <<"\\u", rest::binary>>, acc) when byte_size(rest) >= 4 do
    <<hex::binary-size(4), rest2::binary>> = rest

    case Integer.parse(hex, 16) do
      {low, ""} when low >= 0xDC00 and low <= 0xDFFF ->
        codepoint = 0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)
        char = <<codepoint::utf8>>
        parse_string_contents(rest2, <<acc::binary, char::binary>>)

      _invalid ->
        {:error, "invalid surrogate pair"}
    end
  end

  defp parse_surrogate_pair(_high, _rest, _acc), do: {:error, "incomplete surrogate pair"}

  defp parse_number(str) do
    {num_str, rest} = consume_number(str, <<>>)

    if String.contains?(num_str, ".") or String.contains?(num_str, "e") or
         String.contains?(num_str, "E") do
      case Float.parse(num_str) do
        {f, ""} -> {:ok, f, rest}
        _err -> {:error, "invalid number: #{num_str}"}
      end
    else
      case Integer.parse(num_str) do
        {i, ""} -> {:ok, i, rest}
        _err -> {:error, "invalid number: #{num_str}"}
      end
    end
  end

  defp consume_number(<<c, rest::binary>>, acc)
       when c in [?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?., ?-, ?+, ?e, ?E] do
    consume_number(rest, <<acc::binary, c>>)
  end

  defp consume_number(rest, acc), do: {acc, rest}

  defp parse_object(<<"{", rest::binary>>) do
    rest = skip_whitespace(rest)

    case rest do
      <<"}", rest2::binary>> ->
        {:ok, %{}, rest2}

      _other ->
        parse_object_pairs(rest, %{})
    end
  end

  defp parse_object_pairs(str, map) do
    str = skip_whitespace(str)

    with {:ok, key, rest} <- parse_string(str),
         rest <- skip_whitespace(rest),
         <<":", rest::binary>> <- rest,
         rest <- skip_whitespace(rest),
         {:ok, value, rest} <- parse_value(rest) do
      map = Map.put(map, key, value)
      rest = skip_whitespace(rest)

      case rest do
        <<",", rest2::binary>> ->
          parse_object_pairs(skip_whitespace(rest2), map)

        <<"}", rest2::binary>> ->
          {:ok, map, rest2}

        _other ->
          {:error, "expected ',' or '}' in object"}
      end
    else
      {:error, reason} -> {:error, reason}
      _other -> {:error, "expected ':' after object key"}
    end
  end

  defp parse_array(<<"[", rest::binary>>) do
    rest = skip_whitespace(rest)

    case rest do
      <<"]", rest2::binary>> ->
        {:ok, [], rest2}

      _other ->
        parse_array_elements(rest, [])
    end
  end

  defp parse_array_elements(str, acc) do
    str = skip_whitespace(str)

    case parse_value(str) do
      {:ok, value, rest} ->
        acc = [value | acc]
        rest = skip_whitespace(rest)

        case rest do
          <<",", rest2::binary>> ->
            parse_array_elements(skip_whitespace(rest2), acc)

          <<"]", rest2::binary>> ->
            {:ok, Enum.reverse(acc), rest2}

          _other ->
            {:error, "expected ',' or ']' in array"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp skip_whitespace(<<c, rest::binary>>) when c in [?\s, ?\t, ?\n, ?\r] do
    skip_whitespace(rest)
  end

  defp skip_whitespace(str), do: str
end
