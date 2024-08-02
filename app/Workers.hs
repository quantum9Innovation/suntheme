module Workers where

import Time (chooseActivation, activateOnSunrise, sunriseNow, sunsetNow)
import Cache (finish, readCache)
import Sugar (crash, exec)

prepareScripts :: (Double, Double) -> IO ()
prepareScripts (lat, lon) = do
    sunriseTime <- sunriseNow lat lon
    sunsetTime <- sunsetNow lat lon
    lightMode <- activateOnSunrise sunriseTime sunsetTime
    _ <- chooseActivation lightMode sunriseTime sunsetTime
    queue <- exec "atq" noQueue
    finish queue
    where
        noQueueMsg = "Scheduled process could not be retrieved (try rerunning if 'atq' fails)"
        noQueue _ = print noQueueMsg >> return ""

backupRunner :: IO ()
backupRunner = do
    contents <- readCache
    case contents of
        Nothing -> do
            putStrLn "Failed to read cache after IP location failed"
            crash
        Just (lat, lon, _) -> prepareScripts (lat, lon)
