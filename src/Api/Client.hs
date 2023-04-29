module Api.Client
  ( Client
  , make
  , Api.Client.id
  , userId
  , conn
  ) where

import Data.Functor ((<&>))
  
import qualified Network.WebSockets as WS

import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User

data Client = Client
  { id :: Id Client
  , userId :: Id User
  , conn :: WS.Connection
  }

make :: Id User -> WS.Connection -> IO Client
make uId c = Id.make <&> \cId -> Client
  { Api.Client.id = cId, userId = uId, conn = c }