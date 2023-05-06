{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Api
  ( run
  )
where

import Api.Action
import Api.ClientMsg
import Api.ServerMsg
import Api.ServerState as ServerState
import Control.Concurrent (newMVar)
import Control.Monad (forever, when, unless)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Id (Id)
import Data.Message (Message (Message))
import Data.User (User)
import Data.Text (Text)
import qualified Api.ServerState as ServerState
import qualified Db.Repo as Db
import qualified Network.WebSockets as WS
import qualified Data.Id as Id
import qualified Data.Message as Message
import qualified Data.User as User
import qualified Data.Text as Text
import qualified Data.Time as Time

run :: IO ()
run = do
  putStrLn "run server"
  let opts = WS.defaultServerOptions
  ms <- newMVar =<< ServerState.make
  WS.runServerWithOptions opts { WS.serverHost = "0.0.0.0" } (application (ActionState ms))

application :: (WS.Connection -> ActionState) -> WS.ServerApp
application fs pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 mempty $ runAction (fs conn) $
    readConn >>= receiveClientMsg >>= \case
      Login un pw -> login un pw >>= \u -> initializeClient u >>= talk u
      Unprotected um -> handleUnprotectedClientMsg um
      _ -> failWith (BadRequest "this message type requires authentication")

  where
    initializeClient :: User -> Action (Id Client)
    initializeClient u = do
      cId <- liftIO Id.make
      conn <- readConn
      modifyState $ pure . ServerState.addClient (Client cId (User.id u) conn)
      ss <- readState
      mapM_ (`ServerState.sendMessage` ss) =<< runIO (Db.listUnreceivedMessages (User.id u) <$> readMessages)
      pure cId

talk :: User -> Id Client -> Action ()
talk u cId = loop $
  readConn >>= receiveClientMsg >>= handleClientMsg u
  where
    loop :: Action () -> Action ()
    loop a = forever (runNestedAction a) `finally` disconnect

    disconnect :: Action ()
    disconnect = do
      modifyState $ pure . ServerState.removeClient cId

handleClientMsg :: User -> ClientMsg -> Action ()
handleClientMsg _ (Login _ _) = failWith (BadRequest "already logged in")
handleClientMsg u (Send t recId) = do
  msgId <- liftIO Id.make
  now   <- liftIO Time.getCurrentTime
  let msg = Message {
       Message.id         = msgId
     , Message.text       = t
     , Message.senderId   = User.id u
     , Message.receiverId = recId
     , Message.sentAt     = now
     , Message.receivedAt = Nothing
     , Message.readAt     = Nothing
     }
  runIO $ Db.save msg <$> readMessages
  ServerState.sendMessage msg =<< readState
  send (Sent msg)

handleClientMsg _ (Received msgId) = do
  now <- liftIO Time.getCurrentTime
  unlessM
    (runIO $ Db.updateMessageReceivedAt now msgId <$> readMessages)
    (failWith $ MessageNotFound msgId)

handleClientMsg _ LoadUsers = do
  us <- runIO $ Db.list <$> readUsers
  send (UsersLoaded us)

handleClientMsg u (LoadChat uId) = do
  ms <- runIO $ Db.listMessages uId (User.id u) <$> readMessages
  send (ChatLoaded ms)

handleClientMsg _ (Unprotected um) = handleUnprotectedClientMsg um

handleUnprotectedClientMsg :: UnprotectedClientMsg -> Action ()
handleUnprotectedClientMsg (CreateUser un pw) = do
  let pw' = Text.strip pw
  whenM isDuplicate $ failWith (UsernameTaken un)
  when (Text.null pw') $ failWith BlankPassword
  u <- liftIO $ User.make un pw'
  runIO $ Db.save u <$> readUsers
  send (UserCreated u)
  pure ()
  where
    isDuplicate :: Action Bool
    isDuplicate = runIO $ Db.existsUserByName un <$> readUsers

login :: Text -> Text -> Action User
login un pw = do
  u <- runIO (Db.findUserByName un <$> readUsers) & (`whenNothingM` (failWith $ LoginFailed un))
  unless (User.isPassword pw u) (failWith $ LoginFailed un)
  send (LoginSucceeded u)
  pure u

readUsers :: Action (Db.Repo User)
readUsers = readState <&> ServerState.users

readMessages :: Action (Db.Repo Message)
readMessages = readState <&> ServerState.messages
