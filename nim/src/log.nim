import std/times

proc logTs*(msg: string) =
  ## Write a timestamped log line to stderr: [YYYY-MM-DD HH:MM:SS] msg
  stderr.writeLine "[" & format(now(), "yyyy-MM-dd HH:mm:ss") & "] " & msg
