{-# LANGUAGE OverloadedStrings #-}

module Api.ServerState
  ( ServerState (..)
  , Client (..)
  , make
  , addClient
  , removeClient
  , countClients
  , broadcast
  , sendToUser
  , sendMessage
  , isOnline
  )
where

import Api.Action
import Api.ServerMsg
import Data.Id (Id)
import Data.Message (Message)
import Api.ServerState.Type
import Data.User (User)
import Data.Aeson (ToJSON)
import qualified Data.List as List
import qualified Data.HashMap as Map
import qualified Data.Message as Message
import qualified Db.Conn as Db
import qualified Db.Repo as Db
import qualified Network.WebSockets as WS

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
addClient c s =
  s { clients = Map.insert (clientId c) c (clients s) }
  where
    clientId :: Client -> Id Client
    clientId (Client i _ _) = i

removeClient :: Id Client -> ServerState -> ServerState
removeClient cId s = s { clients = Map.delete cId (clients s) }

countClients :: Id User -> ServerState -> Int
countClients uId s = length (flip List.filter (Map.elems (clients s)) $ \c -> userId c == uId)

broadcast :: ToJSON a => a -> ServerState -> Action ()
broadcast a s = liftIO $ mapM_ (\c -> WS.sendTextData (conn c) $ toJson a) (clients s)

sendToUser :: ToJSON a => Id User -> a -> ServerState -> Action ()
sendToUser uId a s = do
  let cs = flip List.filter (Map.elems (clients s)) $ \c -> userId c == uId
  liftIO $ mapM_ (\c -> WS.sendTextData (conn c) $ toJson a) cs

sendMessage :: Message -> ServerState -> Action ()
sendMessage msg = sendToUser (Message.receiverId msg) (Receive msg)

isOnline :: Id User -> ServerState -> Bool
isOnline uId s = List.any (\c -> userId c == uId) (Map.elems (clients s))

userId :: Client -> Id User
userId (Client _ i _) = i
      
conn :: Client -> WS.Connection
conn (Client _ _ c) = c
  
