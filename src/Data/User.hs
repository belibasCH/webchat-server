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
  }

new :: Text -> Text -> IO User
new n p = (\i -> make i n p) <$> Id.make

make :: Id User -> Text -> Text -> User
make i n p = User { Data.User.id = i, name = n, password = p }

isPassword :: Text -> User -> Bool
isPassword t u = t == password u