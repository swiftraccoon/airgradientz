module log;

import std.stdio : stderr;
import std.datetime : Clock;
import std.format : format;

/// Format the current local time as [YYYY-MM-DD HH:MM:SS].
private string timestamp() {
    auto now = Clock.currTime();
    return format!"[%04d-%02d-%02d %02d:%02d:%02d]"(
        now.year, now.month, now.day,
        now.hour, now.minute, now.second);
}

/// Write a timestamped log line to stderr.
/// Usage: logf("[tag] message %s", arg);
void logf(Args...)(string fmt, Args args) {
    stderr.writef("%s ", timestamp());
    stderr.writefln(fmt, args);
}

/// Write a timestamped log line to stderr (no format args).
/// Usage: logln("[tag] message");
void logln(string msg) {
    stderr.writefln("%s %s", timestamp(), msg);
}
