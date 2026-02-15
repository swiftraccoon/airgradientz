module http.request;

import std.socket;
import std.string : indexOf;
import std.algorithm : splitter;
import core.time : dur;

enum Method { get, other }

struct HttpRequest {
    Method method;
    string path;
    string query;

    string queryParam(string name) {
        if (query.length == 0)
            return null;

        foreach (pair; query.splitter('&')) {
            auto eq = indexOf(pair, '=');
            if (eq >= 0) {
                if (pair[0 .. eq] == name)
                    return urlDecode(pair[eq + 1 .. $]);
            } else if (pair == name) {
                return "";
            }
        }
        return null;
    }

    static HttpRequest parseFromBuf(string data) {
        if (data.length == 0)
            return HttpRequest(Method.other, "", "");

        auto lineEnd = indexOf(data, "\r\n");
        if (lineEnd < 0)
            return HttpRequest(Method.other, "", "");

        auto requestLine = data[0 .. lineEnd];
        auto firstSpace = indexOf(requestLine, ' ');
        if (firstSpace < 0)
            return HttpRequest(Method.other, "", "");

        auto methodStr = requestLine[0 .. firstSpace];
        auto afterMethod = requestLine[firstSpace + 1 .. $];
        auto secondSpace = indexOf(afterMethod, ' ');
        string rawPath = (secondSpace >= 0) ? afterMethod[0 .. secondSpace] : afterMethod;

        auto m = (methodStr == "GET") ? Method.get : Method.other;

        string path, queryStr;
        auto qmark = indexOf(rawPath, '?');
        if (qmark >= 0) {
            path = rawPath[0 .. qmark];
            queryStr = rawPath[qmark + 1 .. $];
        } else {
            path = rawPath;
            queryStr = "";
        }
        return HttpRequest(m, path, queryStr);
    }

    static HttpRequest parse(Socket sock) {
        sock.setOption(SocketOptionLevel.SOCKET, SocketOption.RCVTIMEO, dur!"seconds"(10));

        ubyte[8192] buf;
        string accumulated;
        ptrdiff_t totalRead = 0;

        // Read until we have the full header (double CRLF)
        while (totalRead < buf.length) {
            auto received = sock.receive(buf[totalRead .. $]);
            if (received <= 0)
                break;
            totalRead += received;
            accumulated = cast(string) buf[0 .. totalRead];
            if (indexOf(accumulated, "\r\n\r\n") >= 0)
                break;
        }

        if (accumulated.length == 0)
            return HttpRequest(Method.other, "", "");

        // Parse request line
        auto lineEnd = indexOf(accumulated, "\r\n");
        if (lineEnd < 0)
            return HttpRequest(Method.other, "", "");

        auto requestLine = accumulated[0 .. lineEnd];
        // Split "METHOD /path HTTP/1.1"
        auto firstSpace = indexOf(requestLine, ' ');
        if (firstSpace < 0)
            return HttpRequest(Method.other, "", "");

        auto methodStr = requestLine[0 .. firstSpace];
        auto afterMethod = requestLine[firstSpace + 1 .. $];
        auto secondSpace = indexOf(afterMethod, ' ');
        string rawPath;
        if (secondSpace >= 0)
            rawPath = afterMethod[0 .. secondSpace];
        else
            rawPath = afterMethod;

        auto m = (methodStr == "GET") ? Method.get : Method.other;

        // Split path and query
        string path, queryStr;
        auto qmark = indexOf(rawPath, '?');
        if (qmark >= 0) {
            path = rawPath[0 .. qmark];
            queryStr = rawPath[qmark + 1 .. $];
        } else {
            path = rawPath;
            queryStr = "";
        }

        return HttpRequest(m, path, queryStr);
    }
}

string urlDecode(string s) {
    import std.array : Appender;

    Appender!string result;
    size_t i = 0;
    while (i < s.length) {
        if (s[i] == '%' && i + 2 < s.length) {
            auto hi = hexVal(s[i + 1]);
            auto lo = hexVal(s[i + 2]);
            if (hi >= 0 && lo >= 0) {
                result ~= cast(char)(hi * 16 + lo);
                i += 3;
                continue;
            }
        }
        if (s[i] == '+') {
            result ~= ' ';
        } else {
            result ~= s[i];
        }
        i++;
    }
    return result[];
}

private int hexVal(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + c - 'a';
    if (c >= 'A' && c <= 'F') return 10 + c - 'A';
    return -1;
}
