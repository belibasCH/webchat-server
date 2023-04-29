{-# LANGUAGE OverloadedStrings #-}

module Api (run) where

import Data.Text (Text)
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import Control.Monad (forever)
import Data.Functor ((<&>))

import qualified Network.WebSockets as WS

import Data.User (User)
import qualified Data.User

import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User as User

import Api.Client (Client)
import qualified Api.Client as Client

import Api.ServerState (ServerState)
import qualified Api.ServerState as ServerState

import Api.Msg
import Api.Update

run :: IO ()
run = do
  print ("run server" :: Text)
  state <- newMVar ServerState.new
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
disconnect c s = do
  wrap $ modifyMVar_ s $ return . ServerState.removeClient (Client.id c)
  return ()

talk :: Client -> MVar ServerState -> ResultT IO ()
talk c s = forever $ wrap . sendFailure (Client.conn c) $ do
  msg <- wrap $ WS.receiveData (Client.conn c)
  msg <- wrap $ parseMsgFromJson msg
  case msg of
    Login _ _ -> wrap . Failure $ BadRequest "already logged in"
    CreateUser u p -> error "nyi" -- wrap . modifyMVar_ state $ \s -> createUser User {  }
    Send text -> wrap $ print ("sending '" <> text <> "'")


sendFailure :: WS.Connection -> ResultT IO () -> IO ()
sendFailure conn m = do
  r <- runResultT m
  case r of
    Success () -> pure ()
    Failure e -> WS.sendTextData conn $ toJson e

login :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO Client
login conn s (n, p) = do
  s' <- wrap $ readMVar s
  case ServerState.findUserByName n s' of
    Nothing -> wrap . Failure $ LoginFailed n
    Just u -> if User.isPassword p u
      then wrap $ modifyMVar s (\s -> Client.make (User.id u) conn <&> \c -> (ServerState.addClient c s, c))
      else wrap . Failure $ LoginFailed n

--  if u == "Username" && p == "Password"
--    then do
--      let client = Client { conn = conn, name = u }
--      wrap $ modifyMVar_ state (pure . addClient client)
--      wrap . WS.sendTextData conn $ toJson . LoginSucceeded $ u
--      pure client
--    else wrap . Failure . LoginFailed $ u
