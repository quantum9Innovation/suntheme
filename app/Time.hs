module Time where

import Pure (sunriseNext)

import Data.Time (ZonedTime, getCurrentTime)
import Data.Time.Solar (Location(Location), sunrise, sunset)
import Data.Time.LocalTime (getTimeZone, utcToZonedTime)

now :: IO ZonedTime
now = do
    utcTime <- getCurrentTime
    timeZone <- getTimeZone utcTime
    return (utcToZonedTime timeZone utcTime)

sunriseNow :: Double -> Double -> IO ZonedTime
sunriseNow lat lon = do
    time <- now
    return (sunrise time here)
    where here = Location lat lon

sunsetNow :: Double -> Double -> IO ZonedTime
sunsetNow lat lon = do
    time <- now
    return (sunset time here)
    where here = Location lat lon

activateOnSunrise :: ZonedTime -> ZonedTime -> IO Bool
activateOnSunrise sunriseTime sunsetTime = sunriseNext sunriseTime sunsetTime <$> now
