module http.client;

import std.socket;
import std.format : format;
import std.string : indexOf, toLower, strip;
import std.conv : to;
import std.array : Appender;
import core.time : dur, Duration;

string httpGet(string ip, string path, Duration timeout) {
    auto addr = new InternetAddress(ip, 80);
    auto sock = new TcpSocket();

    sock.setOption(SocketOptionLevel.SOCKET, SocketOption.SNDTIMEO, timeout);
    sock.setOption(SocketOptionLevel.SOCKET, SocketOption.RCVTIMEO, timeout);

    sock.connect(addr);

    auto request = format!"GET %s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n\r\n"(path, ip);
    sock.send(cast(const(ubyte)[]) request);

    // Read full response (cap at 1 MB to prevent OOM from rogue devices)
    enum MAX_RESPONSE = 1024 * 1024;
    Appender!(ubyte[]) response;
    ubyte[4096] buf;
    while (true) {
        auto received = sock.receive(buf[]);
        if (received <= 0) break;
        response ~= buf[0 .. received];
        if (response[].length > MAX_RESPONSE) {
            sock.close();
            throw new Exception("response too large from " ~ ip);
        }
    }
    sock.close();

    auto raw = cast(string) response[];
    if (raw.length == 0)
        throw new Exception("empty response from " ~ ip);

    // Parse status line
    auto lineEnd = raw.indexOf("\r\n");
    if (lineEnd < 0)
        throw new Exception("invalid response from " ~ ip);

    auto statusLine = raw[0 .. lineEnd];
    // "HTTP/1.1 200 OK" → extract status code
    auto firstSpace = statusLine.indexOf(' ');
    if (firstSpace < 0)
        throw new Exception("invalid status line: " ~ statusLine);

    auto rest = statusLine[firstSpace + 1 .. $];
    auto secondSpace = rest.indexOf(' ');
    string codeStr = (secondSpace >= 0) ? rest[0 .. secondSpace] : rest;

    int statusCode;
    try {
        statusCode = codeStr.to!int;
    } catch (Exception) {
        throw new Exception("invalid status code: " ~ codeStr);
    }

    if (statusCode < 200 || statusCode >= 300)
        throw new Exception(format!"HTTP %d"(statusCode));

    // Find end of headers
    auto headerEnd = raw.indexOf("\r\n\r\n");
    if (headerEnd < 0)
        throw new Exception("malformed response: no header terminator");

    return raw[headerEnd + 4 .. $];
}
