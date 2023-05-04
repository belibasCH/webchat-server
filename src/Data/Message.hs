{-# LANGUAGE OverloadedStrings #-}

module Data.Message
  ( Message (..)
  , SendState (..)
  ) where
    
import Prelude hiding (id)
import Text.Read (readMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Id (Id)
import Data.User (User)

import qualified Db.Conn as Db
import Database.MongoDB ((=:), at)
import qualified Database.MongoDB as Mongo

data Message = Message
  { id :: Id Message
  , text :: Text
  , senderId :: Id User
  , receiverId :: Id User
  , state :: SendState
  } deriving (Show)
  
data SendState
  = Sent
  | Received
  | Read
  deriving (Show, Read, Eq)

instance Db.Write Message where
  write msg =
    [ "_id" =: id msg
    , "text" =: text msg
    , "sender_id" =: senderId msg
    , "receiver_id" =: receiverId msg
    , "state" =: state msg
    ]

  writeId msg = id msg

instance Db.Read Message where
  read doc = Message
    { id = at "_id" doc
    , text = at "text" doc
    , senderId = at "sender_id" doc
    , receiverId = at "receiver_id" doc
    , state = at "state" doc
    }


instance Mongo.Val SendState where
  val s = Mongo.String (Text.pack (show s))
  
  cast' (Mongo.String str) = readMaybe (Text.unpack str)
  cast' _ = Nothing