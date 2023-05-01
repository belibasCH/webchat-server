{-# LANGUAGE OverloadedStrings #-}

module Data.Message
  ( Message (..)
  ) where

import Data.Text (Text)
import Data.Id (Id)
import Data.User (User)

import qualified Db.Conn as Db
import Database.MongoDB ((=:), at)

data Message = Message
  { id :: Id Message
  , text :: Text
  , senderId :: Id User
  , receiverId :: Id User
  , isSent :: Bool
  } deriving (Show)

instance Db.Write Message where
  write msg =
    [ "_id" =: Data.Message.id msg
    , "text" =: text msg
    , "sender_id" =: senderId msg
    , "receiver_id" =: receiverId msg
    , "is_sent" =: isSent msg
    ]

instance Db.Read Message where
  read doc = Message
    { Data.Message.id = at "_id" doc
    , text = at "text" doc
    , senderId = at "sender_id" doc
    , receiverId = at "receiver_id" doc
    , isSent = at "is_sent" doc
    }
