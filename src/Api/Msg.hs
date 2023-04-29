{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Msg
  ( Msg (..)
  , parseMsgFromJson) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.Text.Encoding as TE
import Api.Update (Result (..), Error (..))

data Msg
  = Login Text Text
  | Send Text
  deriving (Show)

instance FromJSON Msg where
  parseJSON = Json.withObject "Msg" $ \o -> do
    msgType <- o .: "type" :: Json.Parser Text
    case msgType of
      "login" -> Login <$> o .: "username" <*> o .: "password"
      "send" -> Send <$> o .: "text"
      t -> Json.parserThrowError [] ("invalid message type '" ++ T.unpack t ++ "'")

parseMsgFromJson :: Text -> Result Msg
parseMsgFromJson inp = case parseValue inp >>= Json.fromJSON of
   Json.Success msg -> Success msg
   Json.Error e -> Failure (BadRequest (T.pack e))

parseValue :: FromJSON a => Text -> Json.Result a
parseValue inp =
  case Json.decodeStrict (TE.encodeUtf8 inp) :: Maybe Json.Value of
    Just value -> Json.fromJSON value
    Nothing -> Json.Error "invalid json"