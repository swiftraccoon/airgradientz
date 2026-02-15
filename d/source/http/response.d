module http.response;

import std.socket : Socket;
import std.format : format;
import std.json : JSONValue;
import std.conv : to;
import jsonutil : jsonToString;

struct Header {
    string name;
    string value;
}

struct HttpResponse {
    ushort status;
    string statusText;
    Header[] headers;
    const(ubyte)[] body_;

    static HttpResponse make(ushort status, string statusText) {
        HttpResponse r;
        r.status = status;
        r.statusText = statusText;
        r.headers = [
            Header("Connection", "close"),
            Header("X-Content-Type-Options", "nosniff"),
            Header("X-Frame-Options", "DENY"),
        ];
        return r;
    }

    static HttpResponse okJson(JSONValue json) {
        auto bodyStr = jsonToString(json);
        auto bodyBytes = cast(const(ubyte)[]) bodyStr;
        auto r = make(200, "OK");
        r.headers ~= Header("Content-Type", "application/json");
        r.headers ~= Header("Content-Length", bodyBytes.length.to!string);
        r.body_ = bodyBytes;
        return r;
    }

    static HttpResponse okStatic(const(ubyte)[] content, string contentType) {
        auto r = make(200, "OK");
        r.headers ~= Header("Content-Type", contentType);
        r.headers ~= Header("Content-Length", content.length.to!string);
        r.headers ~= Header("Cache-Control", "public, max-age=600");
        r.body_ = content;
        return r;
    }

    static HttpResponse notFound() {
        enum bodyStr = `{"error":"Not found"}`;
        auto r = make(404, "Not Found");
        r.headers ~= Header("Content-Type", "application/json");
        r.headers ~= Header("Content-Length", bodyStr.length.to!string);
        r.body_ = cast(const(ubyte)[]) bodyStr;
        return r;
    }

    static HttpResponse methodNotAllowed() {
        enum bodyStr = `{"error":"Method not allowed"}`;
        auto r = make(405, "Method Not Allowed");
        r.headers ~= Header("Content-Type", "application/json");
        r.headers ~= Header("Content-Length", bodyStr.length.to!string);
        r.body_ = cast(const(ubyte)[]) bodyStr;
        return r;
    }

    static HttpResponse internalError(string msg) {
        auto bodyStr = format!`{"error":"%s"}`(msg);
        auto bodyBytes = cast(const(ubyte)[]) bodyStr;
        auto r = make(500, "Internal Server Error");
        r.headers ~= Header("Content-Type", "application/json");
        r.headers ~= Header("Content-Length", bodyBytes.length.to!string);
        r.body_ = bodyBytes;
        return r;
    }

    const(ubyte)[] toBytes() {
        import std.array : Appender;
        Appender!(ubyte[]) buf;
        auto statusLine = format!"HTTP/1.1 %d %s\r\n"(status, statusText);
        buf ~= cast(ubyte[]) statusLine;
        foreach (ref h; headers) {
            auto line = format!"%s: %s\r\n"(h.name, h.value);
            buf ~= cast(ubyte[]) line;
        }
        buf ~= cast(ubyte[]) "\r\n";
        buf ~= body_;
        return buf[];
    }

    void writeTo(Socket sock) {
        import std.array : Appender;

        Appender!(ubyte[]) buf;
        auto statusLine = format!"HTTP/1.1 %d %s\r\n"(status, statusText);
        buf ~= cast(ubyte[]) statusLine;

        foreach (ref h; headers) {
            auto line = format!"%s: %s\r\n"(h.name, h.value);
            buf ~= cast(ubyte[]) line;
        }

        buf ~= cast(ubyte[]) "\r\n";
        buf ~= body_;

        auto data = buf[];
        while (data.length > 0) {
            auto sent = sock.send(data);
            if (sent <= 0) break;
            data = data[sent .. $];
        }
    }
}

// ---- unit tests ----

unittest {
    import std.algorithm : canFind;

    // notFound response
    auto r = HttpResponse.notFound();
    assert(r.status == 404);
    assert(r.statusText == "Not Found");

    auto bytes = cast(string) r.toBytes();
    assert(bytes.canFind("HTTP/1.1 404 Not Found"));
    assert(bytes.canFind("Content-Type: application/json"));
    assert(bytes.canFind(`"error"`));

    // methodNotAllowed response
    r = HttpResponse.methodNotAllowed();
    assert(r.status == 405);

    // internalError response
    r = HttpResponse.internalError("test error");
    assert(r.status == 500);
    auto body = cast(string) r.body_;
    assert(body.canFind("test error"));

    // security headers
    r = HttpResponse.make(200, "OK");
    auto allBytes = cast(string) r.toBytes();
    assert(allBytes.canFind("X-Content-Type-Options: nosniff"));
    assert(allBytes.canFind("X-Frame-Options: DENY"));
    assert(allBytes.canFind("Connection: close"));

    // okJson
    import std.json : JSONValue;
    auto json = JSONValue(["key": JSONValue(42)]);
    r = HttpResponse.okJson(json);
    assert(r.status == 200);
    auto jsonBody = cast(string) r.body_;
    assert(jsonBody.canFind(`"key"`));
    assert(jsonBody.canFind("42"));

    // okStatic
    auto content = cast(const(ubyte)[]) "body content";
    r = HttpResponse.okStatic(content, "text/plain");
    assert(r.status == 200);
    allBytes = cast(string) r.toBytes();
    assert(allBytes.canFind("Content-Type: text/plain"));
    assert(allBytes.canFind("Cache-Control: public, max-age=600"));
}
