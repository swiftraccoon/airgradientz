defmodule Airgradientz.JsonTest do
  @moduledoc """
  Unit tests for the hand-rolled Airgradientz.Json encoder/decoder.
  """
  use ExUnit.Case, async: true

  alias Airgradientz.Json

  # ---------------------------------------------------------------------------
  # Encoding - primitives
  # ---------------------------------------------------------------------------

  describe "encode - nil / booleans" do
    test "nil encodes to null" do
      assert Json.encode!(nil) == "null"
    end

    test "true encodes to true" do
      assert Json.encode!(true) == "true"
    end

    test "false encodes to false" do
      assert Json.encode!(false) == "false"
    end
  end

  describe "encode - atoms" do
    test ":null encodes to null" do
      assert Json.encode!(:null) == "null"
    end

    test ":true atom (not boolean) encodes via atom-to-string path" do
      # Elixir's true is the atom :true, so this is the boolean path
      assert Json.encode!(:true) == "true"
    end

    test ":false atom (not boolean) encodes via atom-to-string path" do
      assert Json.encode!(:false) == "false"
    end

    test "arbitrary atom encodes as quoted string" do
      assert Json.encode!(:hello) == "\"hello\""
    end

    test "atom with underscores encodes as quoted string" do
      assert Json.encode!(:some_key) == "\"some_key\""
    end
  end

  describe "encode - integers" do
    test "positive integer" do
      assert Json.encode!(42) == "42"
    end

    test "negative integer" do
      assert Json.encode!(-7) == "-7"
    end

    test "zero" do
      assert Json.encode!(0) == "0"
    end

    test "large integer" do
      assert Json.encode!(999_999_999) == "999999999"
    end
  end

  describe "encode - floats" do
    test "positive float" do
      result = Json.encode!(3.14)
      {parsed, ""} = Float.parse(result)
      assert_in_delta parsed, 3.14, 1.0e-10
    end

    test "negative float" do
      result = Json.encode!(-2.5)
      {parsed, ""} = Float.parse(result)
      assert_in_delta parsed, -2.5, 1.0e-10
    end

    test "zero float encodes with decimal" do
      result = Json.encode!(0.0)
      assert result == "0.0"
    end

    test "whole-number float has .0 suffix" do
      result = Json.encode!(1.0)
      assert result == "1.0"
    end

    test "large whole-number float" do
      result = Json.encode!(100.0)
      assert result == "100.0"
    end

    test "fractional float" do
      result = Json.encode!(0.123)
      {parsed, ""} = Float.parse(result)
      assert_in_delta parsed, 0.123, 1.0e-10
    end
  end

  describe "encode - strings" do
    test "simple string" do
      assert Json.encode!("hello") == "\"hello\""
    end

    test "empty string" do
      assert Json.encode!("") == "\"\""
    end

    test "string with double quotes" do
      assert Json.encode!("say \"hi\"") == "\"say \\\"hi\\\"\""
    end

    test "string with backslash" do
      assert Json.encode!("a\\b") == "\"a\\\\b\""
    end

    test "string with newline" do
      assert Json.encode!("line1\nline2") == "\"line1\\nline2\""
    end

    test "string with carriage return" do
      assert Json.encode!("a\rb") == "\"a\\rb\""
    end

    test "string with tab" do
      assert Json.encode!("a\tb") == "\"a\\tb\""
    end

    test "string with control character (bell)" do
      # 0x07 = bell, should become \u0007
      assert Json.encode!(<<7>>) == "\"\\u0007\""
    end

    test "string with null byte" do
      assert Json.encode!(<<0>>) == "\"\\u0000\""
    end

    test "string with multiple special chars" do
      assert Json.encode!("a\"b\\c\nd") == "\"a\\\"b\\\\c\\nd\""
    end

    test "string with unicode characters (pass-through)" do
      # Non-ASCII UTF-8 is not escaped, just passed through
      assert Json.encode!("cafe\u0301") == "\"cafe\u0301\""
    end
  end

  # ---------------------------------------------------------------------------
  # Encoding - collections
  # ---------------------------------------------------------------------------

  describe "encode - lists" do
    test "empty list" do
      assert Json.encode!([]) == "[]"
    end

    test "list of integers" do
      assert Json.encode!([1, 2, 3]) == "[1,2,3]"
    end

    test "list of strings" do
      assert Json.encode!(["a", "b"]) == "[\"a\",\"b\"]"
    end

    test "list with mixed types" do
      assert Json.encode!([1, "two", true, nil]) == "[1,\"two\",true,null]"
    end

    test "nested lists" do
      assert Json.encode!([[1, 2], [3, 4]]) == "[[1,2],[3,4]]"
    end
  end

  describe "encode - maps" do
    test "empty map" do
      assert Json.encode!(%{}) == "{}"
    end

    test "map with string keys" do
      result = Json.decode!(Json.encode!(%{"name" => "test"}))
      assert result == %{"name" => "test"}
    end

    test "map with atom keys" do
      result = Json.decode!(Json.encode!(%{name: "test"}))
      assert result == %{"name" => "test"}
    end

    test "nested maps" do
      input = %{"outer" => %{"inner" => 42}}
      result = Json.decode!(Json.encode!(input))
      assert result == input
    end

    test "map values include all types" do
      input = %{
        "str" => "hello",
        "int" => 42,
        "float" => 3.14,
        "bool" => true,
        "null" => nil,
        "list" => [1, 2],
        "map" => %{"nested" => true}
      }

      result = Json.decode!(Json.encode!(input))
      assert result["str"] == "hello"
      assert result["int"] == 42
      assert result["bool"] == true
      assert result["null"] == nil
      assert result["list"] == [1, 2]
      assert result["map"] == %{"nested" => true}
    end
  end

  describe "encode - mixed nested structures" do
    test "map containing list containing map" do
      input = %{"items" => [%{"id" => 1}, %{"id" => 2}]}
      result = Json.decode!(Json.encode!(input))
      assert result == input
    end

    test "list of maps" do
      input = [%{"a" => 1}, %{"b" => 2}]
      result = Json.decode!(Json.encode!(input))
      assert result == input
    end

    test "deeply nested structure" do
      input = %{"a" => %{"b" => %{"c" => [1, [2, [3]]]}}}
      result = Json.decode!(Json.encode!(input))
      assert result == input
    end
  end

  describe "encode/1 returns ok/error tuple" do
    test "successful encode returns {:ok, json}" do
      assert {:ok, "42"} = Json.encode(42)
    end

    test "successful encode of map" do
      assert {:ok, json} = Json.encode(%{"a" => 1})
      assert is_binary(json)
    end
  end

  # ---------------------------------------------------------------------------
  # Decoding - primitives
  # ---------------------------------------------------------------------------

  describe "decode - literals" do
    test "true" do
      assert Json.decode!("true") == true
    end

    test "false" do
      assert Json.decode!("false") == false
    end

    test "null" do
      assert Json.decode!("null") == nil
    end
  end

  describe "decode - numbers" do
    test "positive integer" do
      assert Json.decode!("42") == 42
    end

    test "negative integer" do
      assert Json.decode!("-7") == -7
    end

    test "zero" do
      assert Json.decode!("0") == 0
    end

    test "positive float" do
      assert_in_delta Json.decode!("3.14"), 3.14, 1.0e-10
    end

    test "negative float" do
      assert_in_delta Json.decode!("-2.5"), -2.5, 1.0e-10
    end

    test "float with leading zero" do
      assert_in_delta Json.decode!("0.5"), 0.5, 1.0e-10
    end

    test "scientific notation integer-like" do
      result = Json.decode!("1e5")
      assert_in_delta result, 100_000.0, 1.0e-10
    end

    test "scientific notation with decimal" do
      result = Json.decode!("1.5e3")
      assert_in_delta result, 1500.0, 1.0e-10
    end

    test "scientific notation negative exponent" do
      result = Json.decode!("1.5e-3")
      assert_in_delta result, 0.0015, 1.0e-10
    end

    test "scientific notation with E uppercase" do
      result = Json.decode!("2.5E2")
      assert_in_delta result, 250.0, 1.0e-10
    end

    test "scientific notation with positive exponent sign" do
      result = Json.decode!("1e+2")
      assert_in_delta result, 100.0, 1.0e-10
    end
  end

  describe "decode - strings" do
    test "simple string" do
      assert Json.decode!("\"hello\"") == "hello"
    end

    test "empty string" do
      assert Json.decode!("\"\"") == ""
    end

    test "string with escaped double quote" do
      assert Json.decode!(~S("say \"hi\"")) == ~s(say "hi")
    end

    test "string with escaped backslash" do
      assert Json.decode!("\"a\\\\b\"") == "a\\b"
    end

    test "string with escaped forward slash" do
      assert Json.decode!("\"a\\/b\"") == "a/b"
    end

    test "string with escaped newline" do
      assert Json.decode!("\"line1\\nline2\"") == "line1\nline2"
    end

    test "string with escaped carriage return" do
      assert Json.decode!("\"a\\rb\"") == "a\rb"
    end

    test "string with escaped tab" do
      assert Json.decode!("\"a\\tb\"") == "a\tb"
    end

    test "string with escaped backspace" do
      assert Json.decode!("\"a\\bb\"") == "a\bb"
    end

    test "string with escaped form feed" do
      assert Json.decode!("\"a\\fb\"") == "a\fb"
    end

    test "string with unicode escape" do
      # \u0041 = 'A'
      assert Json.decode!("\"\\u0041\"") == "A"
    end

    test "string with unicode escape for non-ASCII" do
      # \u00E9 = e with acute accent
      assert Json.decode!("\"caf\\u00E9\"") == "caf\u00E9"
    end

    test "string with unicode escape for CJK character" do
      # \u4E16 = Chinese character for 'world'
      assert Json.decode!("\"\\u4E16\"") == "\u4E16"
    end

    test "surrogate pair decoding" do
      # \uD83D\uDE00 = U+1F600 (grinning face emoji)
      result = Json.decode!("\"\\uD83D\\uDE00\"")
      assert result == <<0x1F600::utf8>>
    end

    test "string with multiple escape sequences" do
      assert Json.decode!("\"a\\nb\\tc\\\\d\\\"e\"") == "a\nb\tc\\d\"e"
    end
  end

  # ---------------------------------------------------------------------------
  # Decoding - collections
  # ---------------------------------------------------------------------------

  describe "decode - arrays" do
    test "empty array" do
      assert Json.decode!("[]") == []
    end

    test "array of integers" do
      assert Json.decode!("[1,2,3]") == [1, 2, 3]
    end

    test "array of strings" do
      assert Json.decode!(~S(["a","b"])) == ["a", "b"]
    end

    test "array with mixed types" do
      assert Json.decode!("[1,\"two\",true,null]") == [1, "two", true, nil]
    end

    test "nested arrays" do
      assert Json.decode!("[[1,2],[3,4]]") == [[1, 2], [3, 4]]
    end

    test "array with whitespace" do
      assert Json.decode!("[ 1 , 2 , 3 ]") == [1, 2, 3]
    end
  end

  describe "decode - objects" do
    test "empty object" do
      assert Json.decode!("{}") == %{}
    end

    test "simple object" do
      assert Json.decode!(~S({"name":"test"})) == %{"name" => "test"}
    end

    test "object with multiple keys" do
      result = Json.decode!(~S({"a":1,"b":2}))
      assert result == %{"a" => 1, "b" => 2}
    end

    test "object keys are strings" do
      result = Json.decode!(~S({"key":"value"}))
      assert Map.keys(result) == ["key"]
    end

    test "nested objects" do
      result = Json.decode!(~S({"outer":{"inner":42}}))
      assert result == %{"outer" => %{"inner" => 42}}
    end

    test "object with array value" do
      result = Json.decode!("{\"items\":[1,2,3]}")
      assert result == %{"items" => [1, 2, 3]}
    end

    test "object with all value types" do
      json = """
      {"str":"hello","int":42,"float":3.14,"bool":true,"nil":null,"arr":[1],"obj":{"a":1}}
      """

      result = Json.decode!(json)
      assert result["str"] == "hello"
      assert result["int"] == 42
      assert_in_delta result["float"], 3.14, 1.0e-10
      assert result["bool"] == true
      assert result["nil"] == nil
      assert result["arr"] == [1]
      assert result["obj"] == %{"a" => 1}
    end
  end

  # ---------------------------------------------------------------------------
  # Decoding - whitespace handling
  # ---------------------------------------------------------------------------

  describe "decode - whitespace handling" do
    test "leading whitespace" do
      assert Json.decode!("   42") == 42
    end

    test "trailing whitespace" do
      assert Json.decode!("42   ") == 42
    end

    test "whitespace around object braces" do
      assert Json.decode!("  {  \"a\"  :  1  }  ") == %{"a" => 1}
    end

    test "whitespace around array brackets" do
      assert Json.decode!("  [  1  ,  2  ]  ") == [1, 2]
    end

    test "newlines between tokens" do
      json = "{\n  \"a\"\n:\n1\n,\n\"b\"\n:\n2\n}"
      assert Json.decode!(json) == %{"a" => 1, "b" => 2}
    end

    test "tabs between tokens" do
      assert Json.decode!("{\t\"a\"\t:\t1\t}") == %{"a" => 1}
    end

    test "carriage returns between tokens" do
      assert Json.decode!("{\r\"a\"\r:\r1\r}") == %{"a" => 1}
    end
  end

  # ---------------------------------------------------------------------------
  # Decoding - error cases
  # ---------------------------------------------------------------------------

  describe "decode/1 error cases" do
    test "unterminated string" do
      assert {:error, _reason} = Json.decode("\"hello")
    end

    test "trailing comma in object raises (unhandled by parser)" do
      # The parser's parse_object_pairs calls parse_string on "}" which
      # raises FunctionClauseError -- decode/1 does not rescue
      assert_raise FunctionClauseError, fn ->
        Json.decode("{\"a\":1,}")
      end
    end

    test "trailing comma in array" do
      assert {:error, _reason} = Json.decode("[1,2,]")
    end

    test "missing closing brace" do
      assert {:error, _reason} = Json.decode("{\"a\":1")
    end

    test "missing closing bracket" do
      assert {:error, _reason} = Json.decode("[1,2")
    end

    test "invalid token" do
      assert {:error, _reason} = Json.decode("undefined")
    end

    test "empty input" do
      assert {:error, _reason} = Json.decode("")
    end

    test "whitespace-only input" do
      assert {:error, _reason} = Json.decode("   ")
    end

    test "unexpected trailing content" do
      assert {:error, _reason} = Json.decode("42 43")
    end

    test "non-string input returns error" do
      assert {:error, _reason} = Json.decode(42)
    end

    test "incomplete surrogate pair" do
      assert {:error, _reason} = Json.decode("\"\\uD83D\"")
    end

    test "invalid unicode escape" do
      assert {:error, _reason} = Json.decode("\"\\uZZZZ\"")
    end

    test "incomplete unicode escape (too few hex digits)" do
      assert {:error, _reason} = Json.decode("\"\\u00\"")
    end

    test "unterminated escape sequence" do
      assert {:error, _reason} = Json.decode("\"hello\\")
    end

    test "missing colon in object" do
      assert {:error, _reason} = Json.decode("{\"a\" 1}")
    end
  end

  describe "decode!/1 raises on error" do
    test "raises ArgumentError on invalid JSON" do
      assert_raise ArgumentError, fn ->
        Json.decode!("not valid json")
      end
    end

    test "raises ArgumentError on unterminated string" do
      assert_raise ArgumentError, fn ->
        Json.decode!("\"unterminated")
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Round-trip tests
  # ---------------------------------------------------------------------------

  describe "round-trip (encode then decode)" do
    test "integer round-trip" do
      assert Json.decode!(Json.encode!(42)) == 42
    end

    test "negative integer round-trip" do
      assert Json.decode!(Json.encode!(-99)) == -99
    end

    test "string round-trip" do
      assert Json.decode!(Json.encode!("hello world")) == "hello world"
    end

    test "string with special chars round-trip" do
      original = "line1\nline2\ttab\"quote\\backslash"
      assert Json.decode!(Json.encode!(original)) == original
    end

    test "boolean round-trip" do
      assert Json.decode!(Json.encode!(true)) == true
      assert Json.decode!(Json.encode!(false)) == false
    end

    test "nil round-trip" do
      assert Json.decode!(Json.encode!(nil)) == nil
    end

    test "list round-trip" do
      original = [1, "two", true, nil, [3, 4]]
      assert Json.decode!(Json.encode!(original)) == original
    end

    test "empty collections round-trip" do
      assert Json.decode!(Json.encode!([])) == []
      assert Json.decode!(Json.encode!(%{})) == %{}
    end

    test "complex nested structure round-trip" do
      original = %{
        "users" => [
          %{
            "name" => "Alice",
            "age" => 30,
            "active" => true,
            "tags" => ["admin", "user"],
            "meta" => %{"score" => 9.5, "notes" => nil}
          },
          %{
            "name" => "Bob",
            "age" => 25,
            "active" => false,
            "tags" => [],
            "meta" => %{"score" => 7.2, "notes" => "new"}
          }
        ],
        "total" => 2,
        "empty" => %{}
      }

      result = Json.decode!(Json.encode!(original))
      assert result["total"] == 2
      assert result["empty"] == %{}
      assert length(result["users"]) == 2

      alice = Enum.find(result["users"], &(&1["name"] == "Alice"))
      assert alice["age"] == 30
      assert alice["active"] == true
      assert alice["tags"] == ["admin", "user"]
      assert alice["meta"]["score"] == 9.5
      assert alice["meta"]["notes"] == nil

      bob = Enum.find(result["users"], &(&1["name"] == "Bob"))
      assert bob["age"] == 25
      assert bob["active"] == false
      assert bob["tags"] == []
      assert bob["meta"]["notes"] == "new"
    end

    test "float round-trip preserves value" do
      original = 3.14159
      result = Json.decode!(Json.encode!(original))
      assert_in_delta result, original, 1.0e-10
    end

    test "map with atom keys round-trips to string keys" do
      encoded = Json.encode!(%{foo: "bar", baz: 42})
      decoded = Json.decode!(encoded)
      assert decoded["foo"] == "bar"
      assert decoded["baz"] == 42
    end
  end

  # ---------------------------------------------------------------------------
  # Decode then encode (input-driven round-trip)
  # ---------------------------------------------------------------------------

  describe "round-trip (decode then encode then decode)" do
    test "object survives double round-trip" do
      json = "{\"a\":1,\"b\":\"two\",\"c\":true,\"d\":null,\"e\":[1,2]}"
      first = Json.decode!(json)
      second = Json.decode!(Json.encode!(first))
      assert first == second
    end

    test "array survives double round-trip" do
      json = "[1,\"two\",true,null,[3,4],{\"a\":1}]"
      first = Json.decode!(json)
      second = Json.decode!(Json.encode!(first))
      assert first == second
    end
  end
end
