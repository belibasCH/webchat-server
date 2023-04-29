{-# LANGUAGE OverloadedStrings #-}

module Api (run) where

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_)
import Control.Monad (forever)
import Debug.Trace (traceShow, trace)

import Api.Msg
import Api.Update

run :: IO ()
run = do
  print ("run server" :: Text)
  state <- newMVar newServerState
  let opts = WS.defaultServerOptions
  WS.runServerWithOptions opts $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ sendFailure conn $ handleRequest conn state
    

handleRequest :: WS.Connection -> MVar ServerState -> ResultT IO ()
handleRequest conn state = do
  msg <- wrap $ WS.receiveData conn
  msg <- wrap $ parseMsgFromJson msg
  client <- case msg of
    Login u p -> login conn state (u, p)
    _ -> wrap $ Failure (BadRequest "unexpected message type")
  let talk' = talk client state
  let disconnect' = disconnect client state
  talk' `finally` disconnect'

disconnect :: Client -> MVar ServerState -> ResultT IO ()
disconnect client state = do
  wrap $ modifyMVar_ state $ return . removeClient client
  return ()

talk :: Client -> MVar ServerState -> ResultT IO ()
talk client state = forever $ wrap . sendFailure (conn client) $ do
  msg <- wrap $ WS.receiveData (conn client)
  msg <- wrap $ parseMsgFromJson msg
  case msg of
    Login _ _ -> wrap . Failure $ BadRequest "already logged in"
    Send text -> wrap $ print ("sending '" <> text <> "'")
   
   
sendFailure :: WS.Connection -> ResultT IO () -> IO ()
sendFailure conn m = do
  r <- runResultT m
  case r of
    Success () -> pure ()
    Failure e -> WS.sendTextData conn $ toJson e

login :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO Client
login conn state (u, p) = if u == "Username" && p == "Password"
  then do
    let client = Client { conn = conn, name = u }
    wrap $ modifyMVar_ state (pure . addClient client)
    wrap . WS.sendTextData conn $ toJson . LoginSucceeded $ u
    pure client
  else wrap . Failure . LoginFailed $ u

-- A single client's connection.
data Client = Client
  { conn :: WS.Connection
  , name :: Text
  }

-- The in-memory server state.
newtype ServerState = ServerState
  { clients :: [Client]
  }

newServerState :: ServerState
newServerState = ServerState
  { clients = []
  }

addClient :: Client -> ServerState -> ServerState
addClient client state = state
  { clients = client : clients state
  }

removeClient :: Client -> ServerState -> ServerState
removeClient client state = state
  { clients = filter (\c -> name c /= name client) (clients state)
  }

