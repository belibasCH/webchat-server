{-# LANGUAGE OverloadedStrings #-}

module Api.ServerMsg
  ( ServerMsg (..)
  , UserItem
  , ChatItem
  )
where

import Data.Aeson (ToJSON, toJSONList, (.=))
import Data.Functor ((<&>))
import Data.Message (Message)
import Data.Text (Text)
import Data.User (User)
import Data.Id (Id)
import qualified Data.Aeson as Json
import qualified Data.Message as Message
import qualified Data.User as User

data ServerMsg
  = LoginSucceeded User
  | UserLoggedIn (Id User)
  | UserLoggedOut (Id User)
  | UserCreated User
  | UsernameChanged User
  | PasswordChanged
  | UserDeleted (Id User)
  | Sent Message
  | Receive Message
  | MessageReceived Message
  | MessageRead Message
  | UsersLoaded [UserItem]
  | ChatsLoaded [ChatItem]
  | ChatLoaded [Message]
  | ConnectionClosed
  deriving (Show)

type UserItem = (User, IsOnline)
type IsOnline = Bool

type ChatItem = (User, LatestMessage, TotalMessageCount, UnreadMessageCount)
type LatestMessage = Message
type TotalMessageCount = Int
type UnreadMessageCount = Int

instance ToJSON ServerMsg where
  toJSON (LoginSucceeded u) = Json.object
    [ "type" .= ("login_succeeded" :: Text)
    , "user" .= Json.object (jsonUser u)
    ]

  toJSON (UserLoggedIn uId) = Json.object
    [ "type" .= ("user_logged_in" :: Text)
    , "user_id" .= uId
    ]

  toJSON (UserLoggedOut uId) = Json.object
    [ "type" .= ("user_logged_out" :: Text)
    , "user_id" .= uId
    ]
    
  toJSON (UserCreated u) = Json.object
    [ "type" .= ("user_created" :: Text)
    , "user" .= Json.object (jsonUser u)
    ]

  toJSON (UsernameChanged u) = Json.object
    [ "type" .= ("username_changed" :: Text)
    , "user" .= Json.object (jsonUser u)
    ]

  toJSON PasswordChanged = Json.object
    [ "type" .= ("password_changed" :: Text)
    ]

  toJSON (UserDeleted uId) = Json.object
    [ "type" .= ("user_deleted" :: Text)
    , "user_id" .= uId
    ]

  toJSON (Sent msg) = Json.object
    [ "type"    .= ("message_sent" :: Text)
    , "message" .= Json.object (jsonMessage msg)
    ]

  toJSON (Receive msg) = Json.object $
    ( "type" .= ("receive_message" :: Text)
    ) : jsonMessage msg

  toJSON (MessageReceived msg) = Json.object
    [ "type" .= ("message_received" :: Text)
    , "message" .= Json.object (jsonMessage msg)
    ]
    
  toJSON (MessageRead msg) = Json.object
    [ "type" .= ("message_read" :: Text)
    , "message" .= Json.object (jsonMessage msg)
    ]

  toJSON (UsersLoaded us) = Json.object
    [ "type" .= ("users_loaded" :: Text)
    , "users" .= toJSONList (us <&> \(u, o) -> Json.object $ ("is_online" .= o) : jsonUser u)
    ]

  toJSON (ChatsLoaded is) = Json.object
    [ "type" .= ("chats_loaded" :: Text)
    , "chats" .= toJSONList (is <&> \(u, msg, nt, nu) -> Json.object
      [ "user_id" .= User.id u
      , "last_message" .= Json.object (jsonMessage msg)
      , "total_message_count" .= nt
      , "unread_message_count" .= nu
      ])
    ]

  toJSON (ChatLoaded msgs) = Json.object
    [ "type" .= ("chat_loaded" :: Text)
    , "messages" .= toJSONList (msgs <&> Json.object . jsonMessage)
    ]

  toJSON ConnectionClosed = Json.object
    [ "type" .= ("connection_closed" :: Text)
    ]

jsonUser :: User -> [(Json.Key, Json.Value)]
jsonUser u =
  [ "id" .= User.id u
  , "name" .= User.name u
  ]

jsonMessage :: Message -> [(Json.Key, Json.Value)]
jsonMessage msg =
  [ "id" .= Message.id msg
  , "sender_id" .= Message.senderId msg
  , "receiver_id" .= Message.receiverId msg
  , "text" .= Message.text msg
  , "sent_at" .= Message.sentAt msg
  , "received_at" .= Message.receivedAt msg
  , "read_at" .= Message.readAt msg
  ]