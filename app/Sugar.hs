module Sugar where

import Pure (kill)
import Types (Status(..))

import System.Exit (ExitCode(ExitFailure), exitWith)
import System.Process (readProcess)
import Control.Exception (SomeException, catch)

throw :: (Monoid m) => SomeException -> IO m
throw e = print e >> return mempty

continue :: (Status s) => s -> IO () -> IO ()
continue e err = (putStrLn . disp) e >> err

destruct :: (Status s) => s -> IO () -> IO () -> IO ()
destruct status success failure
    | ok status = success
    | otherwise = continue status failure

crash :: IO ()
crash = (exitWith . ExitFailure) 1

exec :: String -> (SomeException -> IO String) -> IO String
exec cmd = catch (readProcess "bash" ["-c", cmd] "")

killall :: [String] -> [IO String]
killall = map (dispatch . kill) where dispatch cmd = exec cmd throw
