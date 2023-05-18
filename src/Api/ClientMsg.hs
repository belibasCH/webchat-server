{-# LANGUAGE OverloadedStrings #-}

module Api.ClientMsg
  ( ClientMsg (..)
  , UnprotectedClientMsg (..)
  , receiveClientMsg
  )
where

import Api.Action
import Data.Aeson (FromJSON, (.:))
import Data.Id (Id)
import Data.Message (Message)
import Data.Text (Text)
import Data.Text.Encoding as TE
import Data.User (User)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data ClientMsg
  = Send Text (Id User)
  | Received (Id Message)
  | Read (Id Message)
  | LoadUsers
  | LoadChats
  | LoadChat (Id User)
  | ChangeUsername Text
  | ChangePassword Text
  | ChangeAvatar (Maybe Text)
  | DeleteUser
    
  -- | Attempt to authenticate with the given username and password.
  -- | This message is only usable when the current connection is not yet logged in.
  -- 
  -- Note that this is not an `Unprotected` message,
  -- as unprotected messages are usable with and without an authenticated connection.
  | Login Text Text
  | Unprotected UnprotectedClientMsg
  deriving (Show)

data UnprotectedClientMsg
  = CreateUser Text Text (Maybe Text)
  deriving (Show)

instance FromJSON ClientMsg where
  parseJSON = Json.withObject "Msg" $ \o -> do
    msgType <- o .: "type" :: Json.Parser Text
    case msgType of
      "send" -> Send <$> o .: "text" <*> o .: "receiver_id"
      "received" -> Received <$> o .: "message_id"
      "read" -> Read <$> o .: "message_id"
      "load_users" -> pure LoadUsers
      "load_chats" -> pure LoadChats
      "load_chat" -> LoadChat <$> o .: "user_id"
      "login" -> Login <$> o .: "username" <*> o .: "password"
      "change_username" -> ChangeUsername <$> o .: "username"
      "change_password" -> ChangePassword <$> o .: "password"
      "change_avatar" -> ChangeAvatar <$> o .: "avatar"
      "delete_user" -> pure DeleteUser
      "create_user" -> Unprotected <$> (CreateUser <$> o .: "username" <*> o .: "password" <*> o .: "avatar")
      t -> Json.parserThrowError [] ("invalid message type '" ++ T.unpack t ++ "'")

receiveClientMsg :: WS.Connection -> Action ClientMsg
receiveClientMsg conn = (liftIO . WS.receiveData) conn >>= parseClientMsg

parseClientMsg :: Text -> Action ClientMsg
parseClientMsg inp = case parseValue inp >>= Json.fromJSON of
   Json.Success q -> pure q
   Json.Error e -> failWith (BadRequest (T.pack e))

parseValue :: FromJSON a => Text -> Json.Result a
parseValue inp =
  case Json.decodeStrict (TE.encodeUtf8 inp) :: Maybe Json.Value of
    Just value -> Json.fromJSON value
    Nothing -> Json.Error "invalid json"
