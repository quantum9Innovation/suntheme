-- run on boot and at noon and midnight
-- don't add commands to `at` while this script is running (monadic purity must be preserved)

module Main where

import Pure (readLines)
import Cache (dumpCache, start)
import Const (query)
import Sugar (continue, destruct)
import Types (ResponseCode(..), ResponseMsg(..), genericErr, toResponseMsg)
import Getters (fetch)
import Workers (backupRunner, prepareScripts)

import Data.ByteString.Char8 (unpack)
import Network.HTTP.Request (Response(responseBody, responseStatus))

-- error handling
-- clean up jank (
--  create types,
--  more pure functions,
--  reduce lines in functions,
--  more atomic/functional,
--  etc
-- )
-- eliminate as many do blocks as possible
-- introduce liquid types and checking
-- whitepaper!

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

processRunner :: Response -> IO ()
processRunner r = process r run backupRunner
    where run lat lon tz = dumpCache lat lon tz >>= prepareScripts

ping :: (Response -> IO ()) -> IO () -> IO ()
ping run err = do
    res <- fetch query
    case res of
        Left e -> continue (toResponseMsg e) err
        Right r ->
            let status = (ResponseCode . responseStatus) r
                runner = run r
            in destruct status runner err

routine :: IO ()
routine = ping processRunner backupRunner

main :: IO ()
main = start >> routine
