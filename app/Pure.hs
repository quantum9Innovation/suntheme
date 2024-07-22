module Pure where

import Data.Time (ZonedTime)
import Data.Time.RFC3339 (formatTimeRFC3339)

readLines :: [String] -> Maybe (String, Double, Double, String)
readLines [msg, latStr, lonStr, tz] = Just (msg, read latStr, read lonStr, tz)
readLines _ = Nothing

kill :: String -> String
kill = (++) "atrm "

after :: (Eq a) => a -> [a] -> [a]
after c = drop 1 . dropWhile (/= c)

formatTime :: ZonedTime -> String
formatTime = take 5 . after 'T' . formatTimeRFC3339

buildCmd :: String -> ZonedTime -> String
buildCmd script time = "echo \"" ++ script ++ "\" | at " ++ formatTime time
