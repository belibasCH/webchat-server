module Data.User
  ( User (User, id, name)
  , new
  , make
  , isPassword
  ) where

import Data.Id (Id)
import qualified Data.Id as Id

import Data.Text (Text)

data User = User
  { id :: Id User
  , name :: Text
  , password :: Text
  } deriving (Show)

new :: Id User -> Text -> Text -> User
new i n p = User { Data.User.id = i, name = n, password = p }

make :: Text -> Text -> IO User
make n p = (\i -> new i n p) <$> Id.new

isPassword :: Text -> User -> Bool
isPassword t u = t == password u