module Api.Utils
  ( send
  , toJson
  )
where

import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import qualified Network.WebSockets as WS
import Data.Text.Lazy.Encoding as TLE
import Data.Aeson as Json

import Result

send :: ToJSON a => a -> WS.Connection -> ResultT IO ()
send a conn = wrap . WS.sendTextData conn $ toJson a

toJson :: ToJSON a => a -> Text
toJson a = (toStrict . TLE.decodeUtf8) (Json.encode a)