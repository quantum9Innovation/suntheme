module Time where

import Pure (buildCmd, sunriseNext)
import Cache (pathToConfig)
import Const (darkModeScript, lightModeScript)
import Sugar (exec)

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

activate :: String -> ZonedTime -> String -> IO String
activate msg time script = do
    putStr msg
    print time
    scriptPath <- pathToConfig script
    exec (buildCmd scriptPath time) terminate
    where terminate _ = print "Scheduling process failed" >> return "" 

chooseActivation :: Bool -> ZonedTime -> ZonedTime -> IO String
chooseActivation lightMode sunriseTime sunsetTime
    | lightMode = activate "Light mode activation script scheduled for " sunriseTime lightModeScript
    | otherwise = activate "Dark mode activation script scheduled for " sunsetTime darkModeScript
