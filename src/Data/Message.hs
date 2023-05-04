{-# LANGUAGE OverloadedStrings #-}

module Data.Message
  ( Message (..)
  ) where

import Prelude hiding (id)
import Text.Read (readMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Id (Id)
import Data.User (User)

import Data.Time (UTCTime)

import qualified Db.Conn as Db
import Database.MongoDB ((=:), at)
import qualified Database.MongoDB as Mongo

data Message = Message
  { id :: Id Message
  , text :: Text
  , senderId :: Id User
  , receiverId :: Id User
  , sentAt :: UTCTime
  , receivedAt :: Maybe UTCTime
  , readAt :: Maybe UTCTime
  } deriving (Show)

instance Db.Write Message where
  write msg =
    [ "_id" =: id msg
    , "text" =: text msg
    , "sender_id" =: senderId msg
    , "receiver_id" =: receiverId msg
    , "sent_at" =: sentAt msg
    , "received_at" =: receivedAt msg
    , "read_at" =: readAt msg
    ]

  writeId msg = id msg

instance Db.Read Message where
  read doc = Message
    { id = at "_id" doc
    , text = at "text" doc
    , senderId = at "sender_id" doc
    , receiverId = at "receiver_id" doc
    , sentAt = at "sent_at" doc
    , receivedAt = at "received_at" doc
    , readAt = at "read_at" doc
    }
    
  order _ = ["sent_at" =: Db.asc]
