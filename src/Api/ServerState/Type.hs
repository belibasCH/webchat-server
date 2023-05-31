module Api.ServerState.Type
  ( ServerState (..)
  , Client (..)
  )
where

import Data.HashMap as Map
import Data.Id (Id)
import Data.Message (Message)
import Data.User (User)
import qualified Db.Repo as Db
import qualified Network.WebSockets as WS

data ServerState = ServerState
  { clients :: Map (Id Client) Client
  , users :: Db.Repo User
  , messages :: Db.Repo Message
  }

data Client = Client (Id Client) (Id User) WS.Connection
