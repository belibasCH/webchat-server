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

import Result
import Api.Msg
import Api.Update
import Api.Utils

run :: IO ()
run = do
  putStrLn "run server"
  state <- ServerState.make >>= newMVar
  let opts = WS.defaultServerOptions
  WS.runServerWithOptions opts { WS.serverHost = "0.0.0.0" } $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ unwrap conn $ handleRequest conn state


handleRequest :: WS.Connection -> MVar ServerState -> ResultT IO ()
handleRequest conn s = do
  msg <- wrap $ WS.receiveData conn
  msg <- wrap $ parseMsgFromJson msg
  case msg of
    Login n p -> loginAndTalk n p
    CreateUser n p -> do
      createUser conn s (n, p)
      handleRequest conn s
    _ -> wrap $ Failure (BadRequest "unexpected message type")
  where
    loginAndTalk :: Text -> Text -> ResultT IO ()
    loginAndTalk n p =  do
      c <- login conn s (n, p)
      Client.send (LoginSucceeded n) c
      let talk' = talk c s
      let disconnect' = disconnect c s
      talk' `finally` disconnect'

disconnect :: Client -> MVar ServerState -> ResultT IO ()
disconnect c s = do
  wrap $ modifyMVar_ s $ return . ServerState.removeClient (Client.id c)
  return ()

unwrap :: WS.Connection -> ResultT IO () -> IO ()
unwrap conn m = do
  r <- runResultT m
  case r of
    Success () -> pure ()
    Failure e -> WS.sendTextData conn $ toJson e

talk :: Client -> MVar ServerState -> ResultT IO ()
talk c s = forever $ wrap . unwrap (Client.conn c) $ do
  msg <- wrap $ WS.receiveData (Client.conn c)
  msg <- wrap $ parseMsgFromJson msg
  case msg of
    Login _ _ -> wrap . Failure $ BadRequest "already logged in"
    CreateUser n p -> createUser (Client.conn c) s (n, p)
    Send text -> wrap $ print ("sending '" <> text <> "'")

createUser :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO ()
createUser conn ms (n, p) = do
  u <- wrap $ User.make n p
  s <- wrap $ readMVar ms
  ServerState.addUser u s
  send (UserCreated u) conn

login :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO Client
login conn ms (n, p) = do
  mu <- ServerState.findUserByName n =<< (wrap $ readMVar ms)
  case mu of
    Nothing -> wrap . Failure $ LoginFailed n
    Just u -> if User.isPassword p u
      then wrap $ modifyMVar ms (\s -> Client.make (User.id u) conn <&> \c -> (ServerState.addClient c s, c))
      else wrap . Failure $ LoginFailed n

--  if u == "Username" && p == "Password"
--    then do
--      let client = Client { conn = conn, name = u }
--      wrap $ modifyMVar_ state (pure . addClient client)
--      wrap . WS.sendTextData conn $ toJson . LoginSucceeded $ u
--      pure client
--    else wrap . Failure . LoginFailed $ u
