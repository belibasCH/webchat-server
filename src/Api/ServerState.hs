module Api.ServerState
  ( ServerState
  , new
  , addClient
  , removeClient
  , findUserByName
  ) where

import Data.HashMap (Map, (!))
import Data.HashMap as Map
   
import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User as User

import Api.Client (Client)
import qualified Api.Client as Client

import Data.List as List
import Data.Text as Text

data ServerState = ServerState
  { clients :: Map (Id Client) Client
  , users :: Map (Id User) User
  }
  
new :: ServerState
new = ServerState
  { clients = Map.empty
  , users = Map.empty
  }

addClient :: Client -> ServerState -> ServerState
addClient c s = s { clients = Map.insert (Client.id c) c (clients s) }

removeClient :: Id Client -> ServerState -> ServerState
removeClient cId s = s { clients = Map.delete cId (clients s) }

findUserByName :: Text -> ServerState -> Maybe User
findUserByName n s = let n' = Text.toLower n in List.find (\u -> (Text.toLower . User.name $ u) == n') (users s)