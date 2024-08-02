module Getters where

import Control.Exception (SomeException, try)
import Network.HTTP.Request (Response, get)

fetch :: String -> IO (Either SomeException Response)
fetch = try . get
