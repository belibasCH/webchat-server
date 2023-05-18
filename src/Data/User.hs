{-# LANGUAGE OverloadedStrings #-}

module Data.User
  ( User (User, id, name, avatar)
  , make
  , isPassword
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
  , name :: Text
  , password :: Text
  , avatar :: Maybe Text
  } deriving (Show)

new :: Text -> Text -> Id User -> User
new n p i = User { Data.User.id = i, name = n, password = p, avatar = Nothing }

make :: Text -> Text -> IO User
make n p = new n p <$> Id.make

isPassword :: Text -> User -> Bool
isPassword t u = t == password u

instance Db.Write User where
  write u =
    [ "_id"      =: Data.User.id u
    , "name"     =: name u
    , "password" =: password u
    , "avatar"   =: avatar u
    ]

  writeId u = id u

instance Db.Read User where
  read doc = User
    { id       = at "_id" doc
    , name     = at "name" doc
    , password = at "password" doc
    , avatar   = at "avatar" doc
    }

  order d _ = ["name" =: Db.asc * d]
