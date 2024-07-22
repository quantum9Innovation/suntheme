module Getters where

import Const (prog)

import System.FilePath ((</>))
import System.Directory (XdgDirectory(XdgCache, XdgConfig), getXdgDirectory)
import Control.Exception (SomeException, try)
import Network.HTTP.Request (Response, get)

pathToCache :: String -> IO String
pathToCache str = (</> str) <$> getXdgDirectory XdgCache prog

pathToConfig :: String -> IO String
pathToConfig str = (</> str) <$> getXdgDirectory XdgConfig prog

fetch :: String -> IO (Either SomeException Response)
fetch = try . get
