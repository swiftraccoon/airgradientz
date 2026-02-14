use std::fmt;

#[derive(Debug, Clone)]
pub(crate) enum JsonError {
    UnexpectedEof,
    UnexpectedChar(char, usize),
    InvalidEscape(usize),
    InvalidNumber(usize),
    TrailingData(usize),
}

impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::UnexpectedChar(c, pos) => {
                write!(f, "unexpected character '{c}' at position {pos}")
            }
            Self::InvalidEscape(pos) => write!(f, "invalid escape at position {pos}"),
            Self::InvalidNumber(pos) => write!(f, "invalid number at position {pos}"),
            Self::TrailingData(pos) => write!(f, "trailing data at position {pos}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Self>),
    Object(Vec<(String, Self)>),
}

impl JsonValue {
    pub(crate) fn get(&self, key: &str) -> Option<&Self> {
        match self {
            Self::Object(pairs) => pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    pub(crate) const fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Number(n) => Some(*n),
            _ => None,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub(crate) const fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Number(n) => Some(*n as i64),
            _ => None,
        }
    }

    #[allow(clippy::cast_precision_loss)] // i64 → f64: JSON spec uses f64 for all numbers
    pub(crate) const fn from_i64(n: i64) -> Self {
        Self::Number(n as f64)
    }

    pub(crate) fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn as_array(&self) -> Option<&Vec<Self>> {
        match self {
            Self::Array(arr) => Some(arr),
            _ => None,
        }
    }

    #[cfg(test)]
    pub(crate) const fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    pub(crate) const fn is_object(&self) -> bool {
        matches!(self, Self::Object(_))
    }
}

pub(crate) fn json_object(pairs: Vec<(&str, JsonValue)>) -> JsonValue {
    JsonValue::Object(pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect())
}

pub(crate) const fn json_array(items: Vec<JsonValue>) -> JsonValue {
    JsonValue::Array(items)
}

// --- Serializer ---

#[allow(clippy::cast_possible_truncation, clippy::cast_precision_loss)]
impl fmt::Display for JsonValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Self::Number(n) => {
                if n.fract() == 0.0 && n.abs() < (i64::MAX as f64) {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{n}")
                }
            }
            Self::String(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '"' => write!(f, "\\\"")?,
                        '\\' => write!(f, "\\\\")?,
                        '\n' => write!(f, "\\n")?,
                        '\r' => write!(f, "\\r")?,
                        '\t' => write!(f, "\\t")?,
                        c if c < '\x20' => write!(f, "\\u{:04x}", c as u32)?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            Self::Array(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Self::Object(pairs) => {
                write!(f, "{{")?;
                for (i, (key, val)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", Self::String(key.clone()))?;
                    write!(f, ":{val}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

// --- Parser ---

pub(crate) fn parse(input: &str) -> Result<JsonValue, JsonError> {
    let bytes = input.as_bytes();
    let mut pos = 0;
    let value = parse_value(bytes, &mut pos)?;
    skip_whitespace(bytes, &mut pos);
    if pos < bytes.len() {
        return Err(JsonError::TrailingData(pos));
    }
    Ok(value)
}

fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && matches!(bytes[*pos], b' ' | b'\t' | b'\n' | b'\r') {
        *pos += 1;
    }
}

fn peek(bytes: &[u8], pos: usize) -> Result<u8, JsonError> {
    bytes.get(pos).copied().ok_or(JsonError::UnexpectedEof)
}

fn parse_value(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    skip_whitespace(bytes, pos);
    let b = peek(bytes, *pos)?;
    match b {
        b'"' => parse_string(bytes, pos).map(JsonValue::String),
        b'{' => parse_object(bytes, pos),
        b'[' => parse_array(bytes, pos),
        b't' | b'f' => parse_bool(bytes, pos),
        b'n' => parse_null(bytes, pos),
        b'-' | b'0'..=b'9' => parse_number(bytes, pos),
        _ => Err(JsonError::UnexpectedChar(b as char, *pos)),
    }
}

fn parse_object(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    *pos += 1; // skip '{'
    skip_whitespace(bytes, pos);
    let mut pairs = Vec::new();

    if peek(bytes, *pos)? == b'}' {
        *pos += 1;
        return Ok(JsonValue::Object(pairs));
    }

    loop {
        skip_whitespace(bytes, pos);
        if peek(bytes, *pos)? != b'"' {
            return Err(JsonError::UnexpectedChar(bytes[*pos] as char, *pos));
        }
        let key = parse_string(bytes, pos)?;

        skip_whitespace(bytes, pos);
        if peek(bytes, *pos)? != b':' {
            return Err(JsonError::UnexpectedChar(bytes[*pos] as char, *pos));
        }
        *pos += 1; // skip ':'

        let value = parse_value(bytes, pos)?;
        pairs.push((key, value));

        skip_whitespace(bytes, pos);
        match peek(bytes, *pos)? {
            b',' => *pos += 1,
            b'}' => {
                *pos += 1;
                break;
            }
            c => return Err(JsonError::UnexpectedChar(c as char, *pos)),
        }
    }

    Ok(JsonValue::Object(pairs))
}

fn parse_array(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    *pos += 1; // skip '['
    skip_whitespace(bytes, pos);
    let mut items = Vec::new();

    if peek(bytes, *pos)? == b']' {
        *pos += 1;
        return Ok(JsonValue::Array(items));
    }

    loop {
        let value = parse_value(bytes, pos)?;
        items.push(value);

        skip_whitespace(bytes, pos);
        match peek(bytes, *pos)? {
            b',' => *pos += 1,
            b']' => {
                *pos += 1;
                break;
            }
            c => return Err(JsonError::UnexpectedChar(c as char, *pos)),
        }
    }

    Ok(JsonValue::Array(items))
}

fn parse_string(bytes: &[u8], pos: &mut usize) -> Result<String, JsonError> {
    *pos += 1; // skip opening '"'
    let mut s = String::new();

    loop {
        if *pos >= bytes.len() {
            return Err(JsonError::UnexpectedEof);
        }
        let b = bytes[*pos];
        match b {
            b'"' => {
                *pos += 1;
                return Ok(s);
            }
            b'\\' => {
                *pos += 1;
                if *pos >= bytes.len() {
                    return Err(JsonError::UnexpectedEof);
                }
                match bytes[*pos] {
                    b'"' => s.push('"'),
                    b'\\' => s.push('\\'),
                    b'/' => s.push('/'),
                    b'n' => s.push('\n'),
                    b'r' => s.push('\r'),
                    b't' => s.push('\t'),
                    b'b' => s.push('\x08'),
                    b'f' => s.push('\x0C'),
                    b'u' => {
                        *pos += 1;
                        let cp = parse_hex4(bytes, pos)?;
                        if (0xD800..=0xDBFF).contains(&cp) {
                            if *pos + 1 < bytes.len()
                                && bytes[*pos] == b'\\'
                                && bytes[*pos + 1] == b'u'
                            {
                                *pos += 2;
                                let low = parse_hex4(bytes, pos)?;
                                if (0xDC00..=0xDFFF).contains(&low) {
                                    let combined =
                                        0x10000 + ((cp - 0xD800) << 10) + (low - 0xDC00);
                                    if let Some(c) = char::from_u32(combined) {
                                        s.push(c);
                                    } else {
                                        return Err(JsonError::InvalidEscape(*pos));
                                    }
                                } else {
                                    return Err(JsonError::InvalidEscape(*pos));
                                }
                            } else {
                                return Err(JsonError::InvalidEscape(*pos));
                            }
                        } else if let Some(c) = char::from_u32(cp) {
                            s.push(c);
                        } else {
                            return Err(JsonError::InvalidEscape(*pos));
                        }
                        continue;
                    }
                    _ => return Err(JsonError::InvalidEscape(*pos)),
                }
                *pos += 1;
            }
            _ => {
                let start = *pos;
                let ch_len = utf8_char_len(b);
                if *pos + ch_len > bytes.len() {
                    return Err(JsonError::UnexpectedEof);
                }
                let ch = std::str::from_utf8(&bytes[start..start + ch_len])
                    .map_err(|_| JsonError::UnexpectedChar(b as char, *pos))?;
                s.push_str(ch);
                *pos += ch_len;
            }
        }
    }
}

const fn utf8_char_len(first_byte: u8) -> usize {
    if first_byte < 0x80 {
        1
    } else if first_byte < 0xE0 {
        2
    } else if first_byte < 0xF0 {
        3
    } else {
        4
    }
}

fn parse_hex4(bytes: &[u8], pos: &mut usize) -> Result<u32, JsonError> {
    if *pos + 4 > bytes.len() {
        return Err(JsonError::UnexpectedEof);
    }
    let hex_str =
        std::str::from_utf8(&bytes[*pos..*pos + 4]).map_err(|_| JsonError::InvalidEscape(*pos))?;
    let val = u32::from_str_radix(hex_str, 16).map_err(|_| JsonError::InvalidEscape(*pos))?;
    *pos += 4;
    Ok(val)
}

fn parse_number(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    let start = *pos;

    if *pos < bytes.len() && bytes[*pos] == b'-' {
        *pos += 1;
    }

    if *pos < bytes.len() && bytes[*pos] == b'0' {
        *pos += 1;
    } else if *pos < bytes.len() && bytes[*pos].is_ascii_digit() {
        while *pos < bytes.len() && bytes[*pos].is_ascii_digit() {
            *pos += 1;
        }
    } else {
        return Err(JsonError::InvalidNumber(start));
    }

    if *pos < bytes.len() && bytes[*pos] == b'.' {
        *pos += 1;
        if *pos >= bytes.len() || !bytes[*pos].is_ascii_digit() {
            return Err(JsonError::InvalidNumber(start));
        }
        while *pos < bytes.len() && bytes[*pos].is_ascii_digit() {
            *pos += 1;
        }
    }

    if *pos < bytes.len() && (bytes[*pos] == b'e' || bytes[*pos] == b'E') {
        *pos += 1;
        if *pos < bytes.len() && (bytes[*pos] == b'+' || bytes[*pos] == b'-') {
            *pos += 1;
        }
        if *pos >= bytes.len() || !bytes[*pos].is_ascii_digit() {
            return Err(JsonError::InvalidNumber(start));
        }
        while *pos < bytes.len() && bytes[*pos].is_ascii_digit() {
            *pos += 1;
        }
    }

    let num_str =
        std::str::from_utf8(&bytes[start..*pos]).map_err(|_| JsonError::InvalidNumber(start))?;
    let n: f64 = num_str.parse().map_err(|_| JsonError::InvalidNumber(start))?;
    Ok(JsonValue::Number(n))
}

fn parse_bool(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    if bytes[*pos..].starts_with(b"true") {
        *pos += 4;
        Ok(JsonValue::Bool(true))
    } else if bytes[*pos..].starts_with(b"false") {
        *pos += 5;
        Ok(JsonValue::Bool(false))
    } else {
        Err(JsonError::UnexpectedChar(bytes[*pos] as char, *pos))
    }
}

fn parse_null(bytes: &[u8], pos: &mut usize) -> Result<JsonValue, JsonError> {
    if bytes[*pos..].starts_with(b"null") {
        *pos += 4;
        Ok(JsonValue::Null)
    } else {
        Err(JsonError::UnexpectedChar(bytes[*pos] as char, *pos))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_null() {
        assert_eq!(parse("null").unwrap(), JsonValue::Null);
    }

    #[test]
    fn test_parse_bools() {
        assert_eq!(parse("true").unwrap(), JsonValue::Bool(true));
        assert_eq!(parse("false").unwrap(), JsonValue::Bool(false));
    }

    #[test]
    fn test_parse_numbers() {
        assert_eq!(parse("42").unwrap(), JsonValue::Number(42.0));
        assert_eq!(parse("-7").unwrap(), JsonValue::Number(-7.0));
        assert_eq!(parse("3.14").unwrap(), JsonValue::Number(3.14));
        assert_eq!(parse("1e10").unwrap(), JsonValue::Number(1e10));
        assert_eq!(parse("2.5E-3").unwrap(), JsonValue::Number(2.5e-3));
        assert_eq!(parse("0").unwrap(), JsonValue::Number(0.0));
        assert_eq!(parse("-0.5").unwrap(), JsonValue::Number(-0.5));
    }

    #[test]
    fn test_parse_strings() {
        assert_eq!(parse(r#""hello""#).unwrap(), JsonValue::String("hello".into()));
        assert_eq!(parse(r#""with \"quotes\"""#).unwrap(), JsonValue::String("with \"quotes\"".into()));
        assert_eq!(parse(r#""line\nbreak""#).unwrap(), JsonValue::String("line\nbreak".into()));
        assert_eq!(parse(r#""tab\there""#).unwrap(), JsonValue::String("tab\there".into()));
        assert_eq!(parse(r#""\u0041""#).unwrap(), JsonValue::String("A".into()));
    }

    #[test]
    fn test_parse_empty_array() {
        assert_eq!(parse("[]").unwrap(), JsonValue::Array(vec![]));
    }

    #[test]
    fn test_parse_array() {
        let JsonValue::Array(items) = parse("[1, 2, 3]").unwrap() else {
            panic!("expected array");
        };
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], JsonValue::Number(1.0));
        assert_eq!(items[2], JsonValue::Number(3.0));
    }

    #[test]
    fn test_parse_empty_object() {
        assert_eq!(parse("{}").unwrap(), JsonValue::Object(vec![]));
    }

    #[test]
    fn test_parse_object() {
        let val = parse(r#"{"a": 1, "b": "two", "c": null}"#).unwrap();
        assert_eq!(val.get("a").unwrap().as_f64(), Some(1.0));
        assert_eq!(val.get("b").unwrap().as_str(), Some("two"));
        assert!(val.get("c").unwrap().is_null());
        assert!(val.get("d").is_none());
    }

    #[test]
    fn test_parse_nested() {
        let val = parse(r#"{"arr": [1, {"x": true}], "obj": {"n": null}}"#).unwrap();
        let arr = val.get("arr").unwrap().as_array().unwrap();
        assert_eq!(arr.len(), 2);
        assert_eq!(arr[1].get("x").unwrap(), &JsonValue::Bool(true));
        assert!(val.get("obj").unwrap().get("n").unwrap().is_null());
    }

    #[test]
    fn test_serialize_roundtrip() {
        let cases = [
            "null", "true", "false", "42", "-7", r#""hello""#, "[]", "[1,2,3]", "{}", r#"{"a":1,"b":"two"}"#,
        ];
        for input in cases {
            let val = parse(input).unwrap();
            let output = val.to_string();
            let val2 = parse(&output).unwrap();
            assert_eq!(val, val2, "roundtrip failed for: {input}");
        }
    }

    #[test]
    fn test_serialize_escapes() {
        let val = JsonValue::String("quote\"back\\slash".to_string());
        assert_eq!(val.to_string(), r#""quote\"back\\slash""#);
    }

    #[test]
    fn test_serialize_number_integer() {
        assert_eq!(JsonValue::Number(42.0).to_string(), "42");
        assert_eq!(JsonValue::Number(-7.0).to_string(), "-7");
        assert_eq!(JsonValue::Number(0.0).to_string(), "0");
    }

    #[test]
    fn test_serialize_number_float() {
        assert_eq!(JsonValue::Number(3.14).to_string(), "3.14");
    }

    #[test]
    fn test_json_object_builder() {
        let obj = json_object(vec![
            ("name", JsonValue::String("test".into())),
            ("count", JsonValue::Number(5.0)),
        ]);
        assert_eq!(obj.get("name").unwrap().as_str(), Some("test"));
        assert_eq!(obj.get("count").unwrap().as_f64(), Some(5.0));
    }

    #[test]
    fn test_whitespace_handling() {
        let val = parse("  {  \"a\"  :  1  ,  \"b\"  :  2  }  ").unwrap();
        assert_eq!(val.get("a").unwrap().as_f64(), Some(1.0));
    }

    #[test]
    fn test_trailing_data_error() {
        assert!(parse("42 extra").is_err());
    }

    #[test]
    fn test_airgradient_indoor_fixture() {
        let input = r#"{"wifi":-51,"serialno":"84fce602549c","rco2":489,"pm01":23.83,"pm02":41.67,"pm10":54.5,"pm003Count":5006.5,"pm02Compensated":31.18,"atmp":20.78,"atmpCompensated":20.78,"rhum":32.19,"rhumCompensated":32.19,"tvocIndex":423,"tvocRaw":35325.92,"noxIndex":1,"noxRaw":21638.25,"boot":138,"bootCount":138,"ledMode":"off","firmware":"3.6.0","model":"I-9PSL"}"#;
        let val = parse(input).unwrap();
        assert_eq!(val.get("wifi").unwrap().as_i64(), Some(-51));
        assert_eq!(val.get("serialno").unwrap().as_str(), Some("84fce602549c"));
        assert_eq!(val.get("rco2").unwrap().as_i64(), Some(489));
        assert_eq!(val.get("pm02").unwrap().as_f64(), Some(41.67));
        assert_eq!(val.get("pm02Compensated").unwrap().as_f64(), Some(31.18));
        assert_eq!(val.get("model").unwrap().as_str(), Some("I-9PSL"));
        assert_eq!(val.get("firmware").unwrap().as_str(), Some("3.6.0"));
    }

    #[test]
    fn test_airgradient_null_fields() {
        let input = r#"{"wifi":-59,"serialno":"84fce602549c","rco2":null,"pm01":null,"pm02":null,"pm10":null,"atmp":null,"model":"I-9PSL"}"#;
        let val = parse(input).unwrap();
        assert!(val.get("rco2").unwrap().is_null());
        assert!(val.get("pm01").unwrap().is_null());
        assert_eq!(val.get("wifi").unwrap().as_i64(), Some(-59));
    }

    #[test]
    fn test_airgradient_zero_compensated() {
        let input = r#"{"pm02Compensated":0,"atmpCompensated":0,"rhumCompensated":0}"#;
        let val = parse(input).unwrap();
        assert_eq!(val.get("pm02Compensated").unwrap().as_f64(), Some(0.0));
        assert_eq!(val.get("atmpCompensated").unwrap().as_f64(), Some(0.0));
    }
}
