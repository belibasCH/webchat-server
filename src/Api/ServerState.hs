{-# LANGUAGE OverloadedStrings #-}

module Api.ServerState
  ( ServerState
  , make
  , addClient
  , removeClient
  , saveUser
  , findUserByName
  , saveMessage
  , sendMessage
  ) where

import Data.HashMap (Map, (!))
import Data.HashMap as Map
import Control.Monad as Monad
   
import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User as User

import Data.Message (Message)
import qualified Data.Message as Message

import Api.Client (Client)
import qualified Api.Client as Client

import qualified Db.Conn as Db
import qualified Db.Repo as Db

import Api.Update
import Data.List as List
import Data.Text as Text
import Result

data ServerState = ServerState
  { clients :: Map (Id Client) Client
  , users :: Db.Repo User
  , messages :: Db.Repo Message
  }
  
-- TODO close db
  
make :: IO ServerState
make = do
  db <- Db.connect
  pure ServerState
    { clients = Map.empty
    , users = Db.Repo "users" db
    , messages = Db.Repo "messages" db
    }

addClient :: Client -> ServerState -> ServerState
addClient c s = s { clients = Map.insert (Client.id c) c (clients s) }

removeClient :: Id Client -> ServerState -> ServerState
removeClient cId s = s { clients = Map.delete cId (clients s) }

findUserByName :: Text -> ServerState -> ResultT IO (Maybe User)
findUserByName n s = wrap $ Db.findUserByName n (users s)

saveUser :: User -> ServerState -> ResultT IO ()
saveUser u s = wrap $ Db.create u (users s)

saveMessage :: Message -> ServerState -> ResultT IO ()
saveMessage msg s = wrap $ Db.create msg (messages s)

sendMessage :: Message -> ServerState -> ResultT IO Message
sendMessage msg s = do
  let cs = flip List.filter (Map.elems (clients s)) $ \c -> Client.userId c == Message.receiverId msg
  Monad.mapM_ (Client.send (Receive msg)) cs
  pure msg { Message.isSent = (not . List.null) cs }