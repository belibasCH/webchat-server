{-# LANGUAGE OverloadedStrings #-}

module Api (run) where

import Data.Text (Text)
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import Control.Monad (forever, mapM_)
import Data.Functor ((<&>))

import qualified Network.WebSockets as WS

import Data.User (User)
import qualified Data.User

import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User as User

import Data.Message (Message (..))
import qualified Data.Message as Message

import Api.Client (Client)
import qualified Api.Client as Client

import Api.ServerState (ServerState)
import qualified Api.ServerState as ServerState

import Data.Time as Time
import Result
import Api.Question
import Api.Answer
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
handleRequest conn ms = do
  q <- receiveQuestion conn
  case q of
    Login n p -> loginAndTalk n p
    CreateUser n p -> do
      createUser conn ms (n, p)
      handleRequest conn ms
    _ -> wrap $ Failure (BadRequest "unexpected message type")
  where
    loginAndTalk :: Text -> Text -> ResultT IO ()
    loginAndTalk n p =  do
      c <- login conn ms (n, p)
      Client.send (LoginSucceeded n) c
      s <- (wrap . readMVar) ms
      mapM_ (`ServerState.sendMessage` s) =<< ServerState.listUnreceivedMessages (Client.userId c) s
      let talk' = talk c ms
      let disconnect' = disconnect c ms
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
talk c ms = forever $ wrap . unwrap (Client.conn c) $ do
  q <- receiveQuestion (Client.conn c)
  case q of
    Login _ _ -> wrap . Failure $ BadRequest "already logged in"
    CreateUser n p -> createUser (Client.conn c) ms (n, p)
    Send txt recId -> doSend txt recId
    Received msgId -> do
      s <- wrap $ readMVar ms
      msgOpt <- ServerState.findMessage msgId s
      case msgOpt of
        Nothing  -> error "message not found" -- TODO report this error to the client
        Just msg -> wrap Time.getCurrentTime >>= \now -> ServerState.saveMessage msg { Message.receivedAt = Just now } s
    LoadChat uId -> do
      s <- wrap $ readMVar ms
      msgs <- ServerState.listMessages (Client.userId c) uId s
      Client.send (ChatLoaded msgs) c
  where
    doSend :: Text -> Id User -> ResultT IO ()
    doSend txt recId = do
      msgId <- wrap Id.new
      s <- wrap $ readMVar ms
      now <- wrap Time.getCurrentTime
      let msg = Message { Message.id = msgId
                       , text = txt
                       , senderId = Client.userId c
                       , receiverId = recId
                       , sentAt = now
                       , receivedAt = Nothing
                       , readAt = Nothing
                       }
      ServerState.saveMessage msg s
      ServerState.sendMessage msg s
      Client.send (Sent msg) c

createUser :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO ()
createUser conn ms (n, p) = do
  u <- wrap $ User.make n p
  s <- wrap $ readMVar ms
  ServerState.saveUser u s
  send (UserCreated u) conn

login :: WS.Connection -> MVar ServerState -> (Text, Text) -> ResultT IO Client
login conn ms (n, p) = do
  mu <- ServerState.findUserByName n =<< wrap (readMVar ms)
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
