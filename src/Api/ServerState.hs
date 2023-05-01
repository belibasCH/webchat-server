{-# LANGUAGE OverloadedStrings #-}

module Api.ServerState
  ( ServerState
  , make
  , addClient
  , removeClient
  , addUser
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

import qualified Db.Conn as Db
import qualified Db.Repo as Db

import Data.List as List
import Data.Text as Text
import Result

data ServerState = ServerState
  { clients :: Map (Id Client) Client
  , users :: Db.Repo User
  , db :: Db.Conn
  }
  
-- TODO close db
  
make :: IO ServerState
make = do
  db <- Db.connect
  pure ServerState
    { clients = Map.empty
    , users = Db.Repo "users" db
    , db = db
    }

addClient :: Client -> ServerState -> ServerState
addClient c s = s { clients = Map.insert (Client.id c) c (clients s) }

addUser :: User -> ServerState -> ResultT IO ()
addUser u s = wrap $ Db.create u (users s)

removeClient :: Id Client -> ServerState -> ServerState
removeClient cId s = s { clients = Map.delete cId (clients s) }

findUserByName :: Text -> ServerState -> ResultT IO (Maybe User)
findUserByName n s = wrap $ Db.findUserByName n (users s)

