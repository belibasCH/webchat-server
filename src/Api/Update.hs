{-# LANGUAGE OverloadedStrings #-}

module Api.Update
  ( Update (..)
  ) where

import Data.Text (Text)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Json

import Data.User (User)
import qualified Data.User as User

data Update
  = LoginSucceeded Text
  | UserCreated User
  deriving (Show)

instance ToJSON Update where
  toJSON (LoginSucceeded u) = Json.object
    [ "type" .= ("login_succeeded" :: Text)
    , "username" .= u
    ]
    
  toJSON (UserCreated u) = Json.object
    [ "type" .= ("user_created" :: Text)
    , "id" .= User.id u
    , "name" .= User.name u
    ]
