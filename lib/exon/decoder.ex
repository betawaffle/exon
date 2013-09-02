import Kernel, except: [is_even: 1]

defmodule Exon.Decoder do
  use Bitwise

  defexception InvalidChar, bin: nil do
    def message(exception) do
      << char :: utf8, _ :: binary >> = exception.bin
      "invalid character (#{<< char :: utf8 >>}) in JSON"
    end
  end

  @compile :native

  defmacrop after_quote(bin) do
    quote do: << ?\", unquote(bin) :: binary >>
  end

  defmacrop is_digit(char) do
    quote do: unquote(char) in ?0..?9
  end

  defmacrop is_even(int) do
    quote do: band(unquote(int), 1) === 0
  end

  defmacrop is_whitespace(char) do
    quote do: unquote(char) in ' \t\n\r'
  end

  def decode(bin) do
    value(bin)
  end

  defp after_pair("," <> rest, obj), do: members(rest, obj)
  defp after_pair("}" <> rest, obj), do: { leave_object(obj), rest }
  defp after_pair(<< ws, rest :: binary >>, obj) when is_whitespace(ws) do
    whitespace(rest) |> after_pair(obj)
  end

  defp after_element("," <> rest, arr), do: elements(rest, arr)
  defp after_element("]" <> rest, arr), do: { leave_array(arr), rest }
  defp after_element(<< ws, rest :: binary >>, arr) when is_whitespace(ws) do
    whitespace(rest) |> after_element(arr)
  end

  Enum.each Stream.concat([0x20..0x21, 0x23..0x5B, 0x5D..0x7E]), fn
    char ->
      defp chars(<< unquote(char), rest :: binary >>, iolist) do
        chars(rest, [iolist, unquote(char)])
      end
  end

  defp chars(<< char :: utf8, rest :: binary >>, iolist) when char > 0x7E do
    chars(rest, [iolist, char])
  end

  defp chars(after_quote(rest), iolist) do
    { leave_string(iolist), rest }
  end

  lc { escape, char } inlist Enum.zip('"\\bfnrt', '"\\\b\f\n\r\t') do
    defp chars(<< ?\\, unquote(escape), rest :: binary >>, iolist) do
      chars(rest, [iolist, unquote(char)])
    end
  end

  defp chars(<< ?\\, ?u, a, b, c, d, rest :: binary >>, iolist) do
    codepoint = list_to_integer([a, b, c, d], 16)
    chars(rest, [iolist, << codepoint :: utf8 >>])
  end

  defp chars(<<>>, _), do: throw(:partial)
  defp chars(<< bin :: binary >>, _) do
    raise InvalidChar, bin: bin
  end

  defp digits(<< digit, rest :: binary >>) when is_digit(digit) do
    { digits, rest } = digits(rest)
    { [digit | digits], rest }
  end
  defp digits(rest), do: { [], rest }

  defp elements(<< bin :: binary >>, arr) do
    { val, rest } = value(bin)
    after_element(rest, [val | arr])
  end

  defp leave_array(arr),  do: arr #:lists.reverse(arr)
  defp leave_object(obj), do: obj #:lists.reverse(obj)
  defp leave_string(str), do: iolist_to_binary(str)

  defp members(after_quote(rest), obj) do
    { key, rest } = chars(rest, [])
    pair(rest, obj, key)
  end

  defp members("}" <> rest, obj) do
    { leave_object(obj), rest }
  end

  defp members(<< ws, rest :: binary >>, obj) when is_whitespace(ws) do
    whitespace(rest) |> members(obj)
  end
  defp members(<<>>, _), do: throw(:partial)
  defp members(<< bin :: binary >>, _) do
    raise InvalidChar, bin: bin
  end

  defp number(<< rest :: binary >>, first) do
    case first do
      ?- ->
        { digits, rest } = digits(rest)
        int = [?- | digits]
      ?0 ->
        int = '+0'
      _  ->
        { digits, rest } = digits(rest)
        int = [?+, first | digits]
    end

    [frac, exp | rest] = number_frac(rest)
    { number(int, frac, exp), rest }
  end

  defp number(int, nil, nil), do: list_to_integer(int, 10)
  defp number(int, nil, exp) do
    int = list_to_integer(int, 10)
    exp = list_to_integer(exp, 10)

    pow(int, exp)
  end
  defp number(int, frac, nil), do: list_to_float(int ++ '.' ++ frac)
  defp number(int, frac, exp), do: list_to_float(int ++ '.' ++ frac ++ 'e' ++ exp)

  defp number_frac("." <> rest) do
    { digits, rest } = digits(rest)
    [digits | number_exp(rest)]
  end
  defp number_frac(rest), do: [nil | number_exp(rest)]

  defp number_exp(<< e, rest :: binary >>) when e in 'eE' do
    case rest do
      "-" <> rest -> sign = ?-
      "+" <> rest -> sign = ?+
      _           -> sign = ?+
    end

    { digits, rest } = digits(rest)
    [[sign | digits] | rest]
  end
  defp number_exp(rest), do: [nil | rest]

  defp pair(":" <> rest, obj, key) do
    { val, rest } = value(rest)

    obj = [{ key, val } | obj] #Dict.put_new(obj, key, val)
    after_pair(rest, obj)
  end

  defp pair(<< ws, rest :: binary>>, obj, key) when is_whitespace(ws) do
    whitespace(rest) |> pair(obj, key)
  end
  defp pair(<<>>, _, _), do: throw(:partial)
  defp pair(bin, _, _),  do: raise(InvalidChar, bin: bin)

  defp pow(_, 0), do: 1
  defp pow(x, 1), do: x
  defp pow(x, y) when is_even(y), do: pow(x * x, div(y, 2))
  defp pow(x, y) when y > 1, do: x * pow(x, y - 1)
  defp pow(x, y) when y < 0, do: pow(1 / x, -y)

  defp value(after_quote(rest)), do: chars(rest, [])
  defp value("{"     <> rest), do: members(rest, [])
  defp value("["     <> rest), do: elements(rest, [])
  defp value("true"  <> rest), do: { true,  rest }
  defp value("false" <> rest), do: { false, rest }
  defp value("null"  <> rest), do: { nil,   rest }

  lc char inlist '-0123456789' do
    defp value(<< unquote(char), rest :: binary >>) do
      number(rest, unquote(char))
    end
  end

  defp value(<< ws, rest :: binary >>) when is_whitespace(ws) do
    whitespace(rest) |> value
  end
  defp value(<<>>), do: throw(:partial)
  defp value(<< bin :: binary >>) do
    raise InvalidChar, bin: bin
  end

  defp whitespace("    " <> rest), do: whitespace(rest)
  lc char inlist ' \t\n\r' do
    defp whitespace(<< unquote(char), rest :: binary >>), do: whitespace(rest)
  end
  defp whitespace(rest), do: rest
end
