{-# LANGUAGE OverloadedStrings #-}

module Api.Answer
  ( Answer (..)
  ) where

import Data.Text (Text)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Json

import Data.User (User)
import qualified Data.User as User

import Data.Message (Message)
import qualified Data.Message as Message

data Answer
  = LoginSucceeded Text
  | UserCreated User
  | Receive Message
  deriving (Show)

instance ToJSON Answer where
  toJSON (LoginSucceeded u) = Json.object
    [ "type" .= ("login_succeeded" :: Text)
    , "username" .= u
    ]
    
  toJSON (UserCreated u) = Json.object
    [ "type" .= ("user_created" :: Text)
    , "id" .= User.id u
    , "name" .= User.name u
    ]

  toJSON (Receive msg) = Json.object
    [ "type" .= ("receive_message" :: Text)
    , "id" .= Message.id msg
    , "sender_id" .= Message.senderId msg
    ]
