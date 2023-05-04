{-# LANGUAGE OverloadedStrings #-}

module Api.Question
  ( Question (..)
  , receiveQuestion
  , parseQuestionFromJson) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Id (Id)
import Data.User (User)
import Data.Message (Message)

import Data.Aeson (FromJSON, (.:))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.Text.Encoding as TE
import qualified Network.WebSockets as WS

import Result

data Question
  = Login Text Text
  | CreateUser Text Text
  | Send Text (Id User)
  | Received (Id Message)
  | LoadChat (Id User)
  deriving (Show)

instance FromJSON Question where
  parseJSON = Json.withObject "Msg" $ \o -> do
    msgType <- o .: "type" :: Json.Parser Text
    case msgType of
      "login" -> Login <$> o .: "username" <*> o .: "password"
      "create_user" -> CreateUser <$> o .: "username" <*> o .: "password"
      "send" -> Send <$> o .: "text" <*> o .: "receiver_id"
      "received" -> Received <$> o .: "message_id"
      "load_chat" -> LoadChat <$> o .: "user_id"
      t -> Json.parserThrowError [] ("invalid message type '" ++ T.unpack t ++ "'")

receiveQuestion :: WS.Connection -> ResultT IO Question
receiveQuestion conn = (wrap . WS.receiveData) conn >>= (wrap . parseQuestionFromJson)

parseQuestionFromJson :: Text -> Result Question
parseQuestionFromJson inp = case parseValue inp >>= Json.fromJSON of
   Json.Success q -> Success q
   Json.Error e -> Failure (BadRequest (T.pack e))

parseValue :: FromJSON a => Text -> Json.Result a
parseValue inp =
  case Json.decodeStrict (TE.encodeUtf8 inp) :: Maybe Json.Value of
    Just value -> Json.fromJSON value
    Nothing -> Json.Error "invalid json"
