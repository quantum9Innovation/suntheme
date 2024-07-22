-- requires 'at' for running a command at a certain time
-- requires 'date' for converting unix time to human readable time
-- run on boot and at noon and midnight
-- don't add commands to at while this script is running (monadic purity must be preserved)

module Main where

import Pure (buildCmd, kill, readLines)
import Const (darkModeScript, lightModeScript, prog, query)
import Types (ResponseMsg(..), ResponseCode(..), Status(..), genericErr, toResponseMsg)

import Data.Time (ZonedTime, getCurrentTime)
import Data.Time.Solar (Location(Location), sunrise, sunset)
import Data.Time.LocalTime (getTimeZone, utcToZonedTime, zonedTimeToUTC)
import Data.List.Extra ((!?))
import Data.ByteString.Char8 (unpack)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.Process (readProcess)
import System.FilePath ((</>), takeDirectory)
import System.Directory (XdgDirectory(XdgCache, XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import Control.Exception (SomeException, catch, try)
import Network.HTTP.Request (Response(responseBody, responseStatus), get)

-- error handling
-- refactor into modules
-- clean up jank (reduce lines in functions, more atomic/functional, etc)
-- eliminate as many do blocks as possible
-- introduce liquid types and checking
-- whitepaper!

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

pathToCache :: String -> IO String
pathToCache str = do
    dir <- getXdgDirectory XdgCache prog
    return (dir </> str)

pathToConfig :: String -> IO String
pathToConfig str = do
    dir <- getXdgDirectory XdgConfig prog
    return (dir </> str)

fetch :: String -> IO (Either SomeException Response)
fetch = try . get

cont :: (Status s) => s -> IO () -> IO ()
cont e err = (putStrLn . disp) e >> err

destruct :: (Status s) => s -> IO () -> IO () -> IO ()
destruct status success failure
    | ok status = success
    | otherwise = cont status failure

ping :: (Response -> IO ()) -> IO () -> IO ()
ping run err = do
    res <- fetch query
    case res of
        Left e -> cont (toResponseMsg e) err
        Right r ->
            let status = (ResponseCode . responseStatus) r
                runner = run r
            in destruct status runner err

process :: Response -> (Double -> Double -> String -> IO ()) -> IO () -> IO ()
process r run err =
    case info of
        Nothing -> cont genericErr err
        Just (msg, lat, lon, tz) ->
            let status = ResponseMsg msg
                runner = run lat lon tz
            in destruct status runner err
    where
        body = responseBody r
        parts = (lines . unpack) body
        info = (readLines . take 4) parts

dumpCache :: Double -> Double -> String -> IO ()
dumpCache lat lon tz = do
    cache <- pathToCache "data.txt"
    catch (writer cache) failure
    prepareScripts lat lon
    where
        writer dir = writeFile dir (show lat ++ "\n" ++ show lon ++ "\n" ++ tz)
        failure = print :: SomeException -> IO ()

readCache :: IO (Maybe (Double, Double, String))
readCache = do
    cache <- pathToCache "data.txt"
    contents <- readFile cache
    let entries = lines contents
    case (entries !? 0, entries !? 1, entries !? 2) of
        (Just lat, Just lon, Just tz) -> return (Just (read lat, read lon, tz))
        _ -> return Nothing

processRunner :: Response -> IO ()
processRunner r = process r dumpCache backupRunner

crash :: IO ()
crash = exitWith (ExitFailure 1)

backupRunner :: IO ()
backupRunner = do
    contents <- readCache
    case contents of
        Nothing -> do
            putStrLn "Failed to read cache after IP location failed"
            crash
        Just (lat, lon, _) -> prepareScripts lat lon

exec :: String -> (SomeException -> IO String) -> IO String
exec cmd err = do catch (readProcess "bash" ["-c", cmd] "") err

killall :: [String] -> IO ()
killall = foldr ((>>) . dispatch . kill) (return ())
    where
        dispatch cmd = exec cmd failure
        failure :: SomeException -> IO String
        failure e = print e >> return []

start :: IO () -> IO ()
start act = do
    logFile <- pathToCache "log.txt"
    existsLog <- doesFileExist logFile
    if existsLog then do
        contents <- readFile logFile
        let entries = lines contents
        killall entries
    else createDirectoryIfMissing True (takeDirectory logFile)
    act

finish :: String -> IO ()
finish queue = do
    cache <- pathToCache "log.txt"
    catch (writeFile cache num) failure
    where
        num = getId queue
        getId = head . words . last . lines
        failure = print :: SomeException -> IO ()

activateOnSunrise :: ZonedTime -> ZonedTime -> IO Bool
activateOnSunrise sunriseTime sunsetTime = do
    timeNow <- now
    let utcTimeNow = zonedTimeToUTC timeNow
        utcSunrise = zonedTimeToUTC sunriseTime
        utcSunset = zonedTimeToUTC sunsetTime
    if utcTimeNow < utcSunrise || utcTimeNow > utcSunset then return True
    else return False

prepareScripts :: Double -> Double -> IO ()
prepareScripts lat lon = do
    sunriseTime <- sunriseNow lat lon
    sunsetTime <- sunsetNow lat lon
    lightMode <- activateOnSunrise sunriseTime sunsetTime
    _ <- if lightMode then do
        putStr "Light mode activation script scheduled for "
        print sunriseTime
        script <- pathToConfig lightModeScript
        exec (buildCmd script sunriseTime) terminate
    else do
        putStrLn "Dark mode activation script scheduled for "
        print sunsetTime
        script <- pathToConfig darkModeScript
        exec (buildCmd script sunsetTime) terminate
    queue <- exec "atq" noQueue
    finish queue
    where
        terminate _ = print "Scheduling process failed" >> return ""
        noQueueMsg = "Scheduled process could not be retrieved (try rerunning if 'atq' fails)"
        noQueue _ = print noQueueMsg >> return ""

routine :: IO ()
routine = ping processRunner backupRunner

main :: IO ()
main = start routine
