module Cache where

import Const (cacheFile, logFile, prog)
import Sugar (killall, failure)

import Data.List.Extra ((!?))
import System.FilePath ((</>), takeDirectory)
import System.Directory (
        XdgDirectory(XdgCache, XdgConfig),
        createDirectoryIfMissing, doesFileExist, getXdgDirectory
    )
import Control.Exception (catch)

pathToCache :: String -> IO String
pathToCache str = (</> str) <$> getXdgDirectory XdgCache prog

pathToConfig :: String -> IO String
pathToConfig str = (</> str) <$> getXdgDirectory XdgConfig prog

readCache :: IO (Maybe (Double, Double, String))
readCache = do
    cache <- pathToCache cacheFile
    contents <- readFile cache
    let entries = lines contents
    case (entries !? 0, entries !? 1, entries !? 2) of
        (Just lat, Just lon, Just tz) -> return (Just (read lat, read lon, tz))
        _ -> return Nothing

dumpCache :: Double -> Double -> String -> IO (Double, Double)
dumpCache lat lon tz = do
    cache <- pathToCache cacheFile
    catch (writer cache) failure
    return (lat, lon)
    where writer dir = writeFile dir (show lat ++ "\n" ++ show lon ++ "\n" ++ tz)

start :: IO ()
start = do
    logs <- pathToCache logFile
    existsLog <- doesFileExist logs
    if existsLog then do
        contents <- readFile logs
        (sequence_ . killall . lines) contents
    else createDirectoryIfMissing True (takeDirectory logs)

finish :: String -> IO ()
finish queue = do
    cache <- pathToCache logFile
    catch (writeFile cache num) failure
    where
        getId = head . words . last . lines
        num = getId queue
