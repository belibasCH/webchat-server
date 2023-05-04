{-# LANGUAGE OverloadedStrings #-}

module Api.ServerState
  ( ServerState
  , make
  , addClient
  , removeClient
  , listUsers
  , saveUser
  , findUserByName
  , findMessage
  , listMessages
  , saveMessage
  , sendMessage
  , listUnreceivedMessages
  )
where

import Data.HashMap as Map
import Control.Monad (mapM_)

import Data.Message (Message)
import qualified Data.Message as Message

import Api.Client (Client)
import qualified Api.Client as Client

import qualified Db.Conn as Db
import qualified Db.Repo as Db

import Api.Answer
import qualified Data.List as List
import Data.Text (Text)
import Data.Id (Id)
import Data.User (User)
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

listUsers :: ServerState -> ResultT IO [User]
listUsers s = wrap $ Db.list (users s)

saveUser :: User -> ServerState -> ResultT IO ()
saveUser u s = wrap $ Db.save u (users s)

findMessage :: Id Message -> ServerState -> ResultT IO (Maybe Message)
findMessage msgId s = wrap $ Db.find msgId (messages s)

listMessages :: Id User -> Id User -> ServerState -> ResultT IO [Message]
listMessages uId1 uId2 s = wrap $ Db.listMessages uId1 uId2 (messages s)

saveMessage :: Message -> ServerState -> ResultT IO ()
saveMessage msg s = wrap $ Db.save msg (messages s)

sendMessage :: Message -> ServerState -> ResultT IO ()
sendMessage msg s = do
  let cs = flip List.filter (Map.elems (clients s)) $ \c -> Client.userId c == Message.receiverId msg
  mapM_ (Client.send (Receive msg)) cs

listUnreceivedMessages :: Id User -> ServerState -> ResultT IO [Message]
listUnreceivedMessages recId s = wrap $ Db.listUnreceivedMessages recId (messages s)