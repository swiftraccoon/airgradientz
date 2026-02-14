module jsonutil;

import std.json : JSONValue, JSONType;
import std.format : format;
import std.array : Appender;
import std.math : floor;

/// Serialize a JSONValue to a compact string with clean number formatting.
/// std.json uses excessive decimal precision; this matches the C/Rust versions
/// by outputting integers without decimals and floats with minimal precision.
string jsonToString(JSONValue v) {
    Appender!string buf;
    writeValue(buf, v);
    return buf[];
}

private void writeValue(ref Appender!string buf, JSONValue v) {
    final switch (v.type) {
        case JSONType.null_:
            buf ~= "null";
            break;
        case JSONType.string:
            writeString(buf, v.get!string);
            break;
        case JSONType.integer:
            buf ~= format!"%d"(v.get!long);
            break;
        case JSONType.uinteger:
            buf ~= format!"%d"(v.get!ulong);
            break;
        case JSONType.float_:
            writeNumber(buf, v.get!double);
            break;
        case JSONType.true_:
            buf ~= "true";
            break;
        case JSONType.false_:
            buf ~= "false";
            break;
        case JSONType.array:
            buf ~= '[';
            bool first = true;
            foreach (ref e; v.array) {
                if (!first) buf ~= ',';
                first = false;
                writeValue(buf, e);
            }
            buf ~= ']';
            break;
        case JSONType.object:
            buf ~= '{';
            bool firstO = true;
            foreach (key, val; v.object) {
                if (!firstO) buf ~= ',';
                firstO = false;
                writeString(buf, key);
                buf ~= ':';
                writeValue(buf, val);
            }
            buf ~= '}';
            break;
    }
}

private void writeNumber(ref Appender!string buf, double d) {
    import std.math : isNaN, isInfinity;

    if (isNaN(d)) {
        buf ~= "null";
        return;
    }
    if (isInfinity(d)) {
        buf ~= "null";
        return;
    }

    // If it's a whole number that fits in a long, output as integer
    if (d == floor(d) && d >= -9.22e18 && d <= 9.22e18) {
        buf ~= format!"%d"(cast(long) d);
        return;
    }

    // Use shortest representation that round-trips
    auto s = format!"%.17g"(d);
    // Try shorter representations
    foreach (prec; 1 .. 17) {
        auto shorter = format!"%.*g"(prec, d);
        // Parse back and check round-trip
        try {
            import std.conv : to;
            auto parsed = shorter.to!double;
            if (parsed == d) {
                buf ~= shorter;
                return;
            }
        } catch (Exception) {}
    }
    buf ~= s;
}

private void writeString(ref Appender!string buf, string s) {
    buf ~= '"';
    foreach (char c; s) {
        switch (c) {
            case '"':  buf ~= `\"`; break;
            case '\\': buf ~= `\\`; break;
            case '\n': buf ~= `\n`; break;
            case '\r': buf ~= `\r`; break;
            case '\t': buf ~= `\t`; break;
            case '\b': buf ~= `\b`; break;
            case '\f': buf ~= `\f`; break;
            default:
                if (c < 0x20) {
                    buf ~= format!`\u%04x`(cast(uint) c);
                } else {
                    buf ~= c;
                }
                break;
        }
    }
    buf ~= '"';
}
