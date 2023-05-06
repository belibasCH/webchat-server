{-# LANGUAGE OverloadedStrings #-}

module Api.ServerMsg
  ( ServerMsg (..)
  , ChatItem
  )
where

import Data.Aeson (ToJSON, toJSONList, (.=))
import Data.Functor ((<&>))
import Data.Message (Message)
import Data.Text (Text)
import Data.User (User)
import qualified Data.Aeson as Json
import qualified Data.Message as Message
import qualified Data.User as User

data ServerMsg
  = LoginSucceeded User
  | UserCreated User
  | Sent Message
  | Receive Message
  | UsersLoaded [User]
  | ChatsLoaded [ChatItem]
  | ChatLoaded [Message]
  deriving (Show)

type ChatItem = (User, LatestMessage, TotalMessageCount, UnreadMessageCount)
type LatestMessage = Message
type TotalMessageCount = Int
type UnreadMessageCount = Int

instance ToJSON ServerMsg where
  toJSON (LoginSucceeded u) = Json.object
    [ "type" .= ("login_succeeded" :: Text)
    , "user" .= Json.object
      [ "id" .= User.id u
      , "name" .= User.name u
      ]
    ]
    
  toJSON (UserCreated u) = Json.object
    [ "type" .= ("user_created" :: Text)
    , "id" .= User.id u
    , "name" .= User.name u
    ]

  toJSON (Sent msg) = Json.object $
    ( "type" .= ("message_sent" :: Text)
    ) : jsonMessage msg

  toJSON (Receive msg) = Json.object $
    ( "type" .= ("receive_message" :: Text)
    ) : jsonMessage msg

  toJSON (UsersLoaded us) = Json.object
    [ "type" .= ("users_loaded" :: Text)
    , "users" .= toJSONList (us <&> \u -> Json.object
      [ "id" .= User.id u
      , "name" .= User.name u
      ])
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

jsonMessage :: Message -> [(Json.Key, Json.Value)]
jsonMessage msg =
  [ "id" .= Message.id msg
  , "sender_id" .= Message.senderId msg
  , "receiver_id" .= Message.receiverId msg
  , "text" .= Message.text msg
  , "sent_at" .= Message.sentAt msg
  ]