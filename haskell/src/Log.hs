module Log (logMsg) where

import Data.Time (formatTime, defaultTimeLocale, getCurrentTime, utcToLocalZonedTime)
import System.IO (hPutStrLn, stderr)

-- | Write a timestamped log message to stderr.
-- Format: [YYYY-MM-DD HH:MM:SS] <message>
logMsg :: String -> IO ()
logMsg msg = do
  now <- getCurrentTime
  zt  <- utcToLocalZonedTime now
  let ts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zt
  hPutStrLn stderr $ "[" ++ ts ++ "] " ++ msg
