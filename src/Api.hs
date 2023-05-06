{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Api
  ( run
  )
where

import Api.Action
import Api.ClientMsg
import Api.ServerMsg
import Api.ServerState as ServerState
import Control.Concurrent (newMVar)
import Control.Exception (throwIO)
import Control.Monad (forever, when, unless)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Id (Id)
import Data.Message (Message (Message))
import Data.User (User)
import Data.Text (Text)
import Data.Maybe (catMaybes)
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
  WS.withPingThread conn 30 mempty $ runAction (fs conn) talkUnauthorized
  where
    talkUnauthorized :: Action ()
    talkUnauthorized =
      readConn >>= receiveClientMsg >>= \case
        Login un pw -> login un pw >>= \uId -> initializeClient uId >>= talk uId
        Unprotected um -> do
          runNestedAction (handleUnprotectedClientMsg um)
          talkUnauthorized
        _ -> failWith (BadRequest "this message type requires authentication")
          
    initializeClient :: Id User -> Action (Id Client)
    initializeClient uId = do
      readState >>= ServerState.broadcast (UserLoggedIn uId)
      cId <- liftIO Id.make
      conn <- readConn
      modifyState $ pure . ServerState.addClient (Client cId uId conn)
      ss <- readState
      mapM_ (`ServerState.sendMessage` ss) =<< runIO (Db.listUnreceivedMessages uId <$> readMessages)
      pure cId

talk :: Id User -> Id Client -> Action ()
talk uId cId = loop $
  readConn >>= receiveClientMsg >>= handleClientMsg uId
  where
    loop :: Action () -> Action ()
    loop a = forever (runNestedAction a) `finally` disconnect

    disconnect :: Action ()
    disconnect = do
      modifyState $ pure . ServerState.removeClient cId
      readState >>= ServerState.broadcast (UserLoggedOut uId)

handleClientMsg :: Id User -> ClientMsg -> Action ()
handleClientMsg _ (Login _ _) = failWith (BadRequest "already logged in")

handleClientMsg uId (RenameUser un) = do
  u' <- whenNothingM
    (runIO $ Db.updateUserName un uId <$> readUsers)
    (error "current user missing in database")
  ServerState.broadcast (UserNameChanged u') =<< readState

handleClientMsg uId DeleteUser = do
  unlessM
    (runIO $ Db.delete uId <$> readUsers)
    (error "current user missing in database")
  runIO $ Db.deleteMessagesOfUser uId <$> readMessages
  ServerState.broadcast (UserDeleted uId) =<< readState
  liftIO $ throwIO (WS.CloseRequest 1000 "user deleted")

handleClientMsg uId (Send t recId) = do
  unlessM receiverExists $ failWith (UserNotFound recId)
  msgId <- liftIO Id.make
  now   <- liftIO Time.getCurrentTime
  let msg = Message {
       Message.id         = msgId
     , Message.text       = t
     , Message.senderId   = uId
     , Message.receiverId = recId
     , Message.sentAt     = now
     , Message.receivedAt = Nothing
     , Message.readAt     = Nothing
     }
  runIO $ Db.save msg <$> readMessages
  ServerState.sendMessage msg =<< readState
  send (Sent msg)
  where
    receiverExists :: Action Bool
    receiverExists = runIO $ Db.exists recId <$> readUsers

handleClientMsg uId (Received msgId) = do
  now <- liftIO Time.getCurrentTime
  msg <- whenNothingM
    (runIO $ Db.updateMessageReceivedAt now uId msgId <$> readMessages)
    (failWith $ MessageNotFound msgId)
  ServerState.sendToUser (Message.senderId msg) (MessageReceived msgId) =<< readState

handleClientMsg uId (Read msgId) = do
  now <- liftIO Time.getCurrentTime
  msg <- whenNothingM
    (runIO $ Db.updateMessageReadAt now uId msgId <$> readMessages)
    (failWith $ MessageNotFound msgId)
  ServerState.sendToUser (Message.senderId msg) (MessageRead msgId) =<< readState

handleClientMsg _ LoadUsers = do
  us <- runIO $ Db.list <$> readUsers
  is <- mapM makeItem us
  send (UsersLoaded is)
  where
    makeItem :: User -> Action UserItem
    makeItem u = readState >>= \s -> pure (u, ServerState.isOnline (User.id u) s)

handleClientMsg uId LoadChats = do
  ms <- runIO $ Db.list <$> readUsers
  is <- mapM loadChat ms <&> catMaybes
  send (ChatsLoaded is)
  where
    loadChat :: User -> Action (Maybe ChatItem)
    loadChat u2 = do
      msgs <- readMessages
      liftIO $ do
        mm <- Db.findLatestChatMessage uId (User.id u2) msgs
        nt <- Db.countTotalChatMessages uId (User.id u2) msgs
        nu <- Db.countUnreadChatMessages (User.id u2) uId msgs
        pure $ mm <&> (u2, , nt, nu)


handleClientMsg uId (LoadChat uId2) = do
  is <- runIO $ Db.listChatMessages uId uId2 <$> readMessages
  send (ChatLoaded is)

handleClientMsg _ (Unprotected um) = handleUnprotectedClientMsg um

handleUnprotectedClientMsg :: UnprotectedClientMsg -> Action ()
handleUnprotectedClientMsg (CreateUser un pw) = do
  let pw' = Text.strip pw
  whenM isDuplicate $ failWith (UsernameTaken un)
  when (Text.null pw') $ failWith BlankPassword
  u <- liftIO $ User.make un pw'
  runIO $ Db.save u <$> readUsers
  send (UserCreated u)
  ServerState.broadcast (UserCreated u) =<< readState
  pure ()
  where
    isDuplicate :: Action Bool
    isDuplicate = runIO $ Db.existsUserByName un <$> readUsers

login :: Text -> Text -> Action (Id User)
login un pw = do
  u <- runIO (Db.findUserByName un <$> readUsers) & (`whenNothingM` (failWith $ LoginFailed un))
  unless (User.isPassword pw u) (failWith $ LoginFailed un)
  send (LoginSucceeded u)
  pure (User.id u)

readUsers :: Action (Db.Repo User)
readUsers = readState <&> ServerState.users

readMessages :: Action (Db.Repo Message)
readMessages = readState <&> ServerState.messages
