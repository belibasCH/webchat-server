{-# LANGUAGE OverloadedStrings #-}

module Api.ServerMsg
  ( ServerMsg (..)
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
  | ChatLoaded [Message]
  deriving (Show)

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

  toJSON (Sent msg) = Json.object
    [ "type" .= ("message_sent" :: Text)
    , "id" .= Message.id msg
    , "sender_id" .= Message.senderId msg
    , "sent_at" .= Message.sentAt msg
    ]

  toJSON (Receive msg) = Json.object
    [ "type" .= ("receive_message" :: Text)
    , "id" .= Message.id msg
    , "sender_id" .= Message.senderId msg
    , "text" .= Message.text msg
    , "sent_at" .= Message.sentAt msg
    ]

  toJSON (UsersLoaded us) = Json.object
    [ "type" .= ("users_loaded" :: Text)
    , "users" .= toJSONList (us <&> \u -> Json.object
      [ "id" .= User.id u
      , "name" .= User.name u
      ])
    ]

  toJSON (ChatLoaded msgs) = Json.object
    [ "type" .= ("chat_loaded" :: Text)
    , "messages" .= toJSONList (msgs <&> \msg -> Json.object
      [ "id" .= Message.id msg
      , "sender_id" .= Message.senderId msg
      , "receiver_id" .= Message.receiverId msg
      , "text" .= Message.text msg
      , "sent_at" .= Message.sentAt msg
      ])
    ]
