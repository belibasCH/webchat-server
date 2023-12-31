{-# LANGUAGE OverloadedStrings #-}

module Data.Message
  ( Message (..)
  )
where

import Data.Id (Id)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.User (User, MessageKey)
import Database.MongoDB (at, (=:))
import Prelude hiding (id)
import qualified Db.Conn as Db

data Message = Message
  { id :: Id Message
  , text :: Text
  , key :: MessageKey
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
    , "key" =: key msg
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
    , key = at "key" doc
    , senderId = at "sender_id" doc
    , receiverId = at "receiver_id" doc
    , sentAt = at "sent_at" doc
    , receivedAt = at "received_at" doc
    , readAt = at "read_at" doc
    }

  order d _ = ["sent_at" =: Db.asc * d]
