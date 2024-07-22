module Types where

import Control.Exception (SomeException)

class Status s where
    ok :: s -> Bool
    ok = const False

    disp :: s -> String
    disp = const "encountered bad response (expected OK)"

newtype ResponseCode = ResponseCode Int
instance Status ResponseCode where
    ok (ResponseCode code) = code == 200
    disp (ResponseCode code) =
        "encountered bad response code: "
        ++ show code
        ++ " (expected 200 'OK')"

newtype ResponseMsg = ResponseMsg String
instance Status ResponseMsg where
    ok (ResponseMsg msg) = msg == "success"
    disp (ResponseMsg msg) =
        "encountered bad response message: "
        ++ msg
        ++ " (expected 'success')"

toResponseMsg :: SomeException -> ResponseMsg
toResponseMsg = ResponseMsg . show

genericErr :: ResponseMsg
genericErr = ResponseMsg "encountered unknown error"
