-- requires 'at' for running a command at a certain time
-- requires 'date' for converting unix time to human readable time
-- run on boot and at noon and midnight
-- don't add commands to at while this script is running (monadic purity must be preserved)

module Main where

import Pure (buildCmd, readLines)
import Time (activateOnSunrise, sunriseNow, sunsetNow)
import Const (darkModeScript, lightModeScript, query)
import Sugar (continue, crash, destruct, exec, killall)
import Types (ResponseCode(..), ResponseMsg(..), genericErr, toResponseMsg)
import Getters (fetch, pathToCache, pathToConfig)

import Data.List.Extra ((!?))
import Data.ByteString.Char8 (unpack)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Control.Exception (SomeException, catch)
import Network.HTTP.Request (Response(responseBody, responseStatus))

-- error handling
-- refactor into modules
-- clean up jank (reduce lines in functions, more atomic/functional, etc)
-- eliminate as many do blocks as possible
-- introduce liquid types and checking
-- whitepaper!

ping :: (Response -> IO ()) -> IO () -> IO ()
ping run err = do
    res <- fetch query
    case res of
        Left e -> continue (toResponseMsg e) err
        Right r ->
            let status = (ResponseCode . responseStatus) r
                runner = run r
            in destruct status runner err

process :: Response -> (Double -> Double -> String -> IO ()) -> IO () -> IO ()
process r run err =
    case info of
        Nothing -> continue genericErr err
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

backupRunner :: IO ()
backupRunner = do
    contents <- readCache
    case contents of
        Nothing -> do
            putStrLn "Failed to read cache after IP location failed"
            crash
        Just (lat, lon, _) -> prepareScripts lat lon

start :: IO () -> IO ()
start act = do
    logFile <- pathToCache "log.txt"
    existsLog <- doesFileExist logFile
    if existsLog then do
        contents <- readFile logFile
        let entries = lines contents
        (sequence_ . killall) entries
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
