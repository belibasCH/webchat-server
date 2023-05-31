{-# LANGUAGE OverloadedStrings #-}

module Data.User
  ( User (User, id, name, avatar, private_key, public_key, message_key)
  , make
  , isPassword
  , Username
  , Password
  , Avatar
  , PublicKey
  , PrivateKey
  , MessageKey
  )
where

import Prelude hiding (id)
import Data.Text (Text)
import qualified Db.Conn as Db
import Database.MongoDB ((=:), at)

import Data.Id (Id)
import qualified Data.Id as Id

data User = User
  { id :: Id User
  , name :: Username
  , password :: Password
  , avatar :: Maybe Avatar
  , public_key :: PublicKey
  , private_key :: PrivateKey
  , message_key :: MessageKey
  } deriving (Show)

type Username = Text
type Password = Text
type Avatar   = Text

type PublicKey  = Text
type PrivateKey = Text
type MessageKey = Text


new :: Maybe Avatar -> Username -> Password -> PublicKey -> PrivateKey -> MessageKey -> Id User -> User
new av un pw pk sk mk i = User
  { id = i
  , name = un
  , password = pw
  , avatar = av
  , public_key = pk
  , private_key = sk
  , message_key = mk
  }

make :: Maybe Avatar -> Username -> Password -> PublicKey -> PrivateKey -> MessageKey -> IO User
make av un pw pk sk mk = new av un pw pk sk mk <$> Id.make

isPassword :: Text -> User -> Bool
isPassword t u = t == password u

instance Db.Write User where
  write u =
    [ "_id"         =: id u
    , "name"        =: name u
    , "password"    =: password u
    , "avatar"      =: avatar u
    , "public_key"  =: public_key u
    , "private_key" =: private_key u
    , "message_key" =: message_key u
    ]

  writeId u = id u

instance Db.Read User where
  read doc = User
    { id          = at "_id" doc
    , name        = at "name" doc
    , password    = at "password" doc
    , avatar      = at "avatar" doc
    , public_key  = at "public_key" doc
    , private_key = at "private_key" doc
    , message_key = at "message_key" doc
    }

  order d _ = ["name" =: Db.asc * d]
