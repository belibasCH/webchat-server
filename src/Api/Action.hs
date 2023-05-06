{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Action
  ( Action
  , ActionState (..)
  , runAction
  , runNestedAction
  , liftIO
  , liftIO2
  , runIO
  , Error (..)
  , failWith
  , toJson
  , readState
  , modifyState
  , readConn
  , send
  , whenM
  , unlessM
  , whenNothing
  , whenNothingM
  , finally
  )
where

import Api.ServerState.Type
import Control.Concurrent (MVar, readMVar, modifyMVar)
import Control.Monad (when, unless, (>=>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON, (.=))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding as TLE
import Data.Id (Id)
import Data.User (User)
import Data.Message (Message)
import qualified Control.Exception as Ex
import qualified Data.Aeson as Json
import qualified Network.WebSockets as WS

newtype Action a = Action (ActionState -> IO (ActionResult a))

data ActionState = ActionState (MVar ServerState) WS.Connection

runAction :: ActionState -> Action a -> IO ()
runAction s (Action a) = a s >>= \case
  Success _ -> pure ()
  Failure e  -> WS.sendTextData (conn s) (toJson e)
  where
    conn :: ActionState -> WS.Connection
    conn (ActionState _ c) = c
      
runNestedAction :: Action () -> Action ()
runNestedAction a = Action $ \s -> runAction s a <&> pure

liftIO2 :: MonadIO m => IO (a -> IO b) -> m a -> m b
liftIO2 mf ma = liftIO mf <*> ma >>= liftIO

runIO :: Action (IO a) -> Action a
runIO (Action aa) = Action $ aa >=> \case
    Success a -> a <&> Success
    Failure e -> pure $ Failure e

instance Functor Action where
  fmap f (Action aa) = Action $ \s -> do
    ra <- aa s
    pure $ fmap f ra

instance Applicative Action where
  pure a = Action $ \_ -> (pure.pure) a

  Action fa <*> Action aa = Action $ \s -> do
    rf <- fa s
    ra <- aa s
    pure $ rf <*> ra

instance Monad Action where
  Action aa >>= f = Action $ \s -> do
     ra <- aa s
     case ra <&> \a -> f a of
        Success (Action ba) -> ba s
        Failure e           -> pure (Failure e)

instance MonadIO Action where
  liftIO ma = Action $ \_ -> pure <$> ma


data ActionResult a
  = Success a
  | Failure Error

instance Functor ActionResult where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative ActionResult where
  pure a = Success a

  Success f <*> Success a = Success (f a)
  Failure e <*> _         = Failure e
  _         <*> Failure e = Failure e

instance Monad ActionResult where
  Success a >>= f = f a
  Failure e >>= _ = Failure e


data Error
  = BadRequest Text
  | LoginFailed Text
  | UsernameTaken Text
  | BlankPassword
  | UserNotFound (Id User)
  | MessageNotFound (Id Message)
  deriving (Show)

failWith :: Error -> Action a
failWith e = Action $ \_ -> (pure . Failure) e

instance ToJSON Error where
  toJSON (BadRequest e) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("bad_request" :: Text)
    , "message" .= e
    ]

  toJSON (LoginFailed u) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("login_failed" :: Text)
    , "username" .= u
    ]

  toJSON (UsernameTaken u) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("name_taken" :: Text)
    , "username" .= u
    ]

  toJSON BlankPassword = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("blank_password" :: Text)
    ]

  toJSON (UserNotFound uId) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("not_found" :: Text)
    , "data_type" .= ("User" :: Text)
    , "id" .= uId
    ]

  toJSON (MessageNotFound msgId) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("not_found" :: Text)
    , "data_type" .= ("Message" :: Text)
    , "id" .= msgId
    ]

toJson :: ToJSON a => a -> Text
toJson a = (toStrict . TLE.decodeUtf8) (Json.encode a)

readState :: Action ServerState
readState = Action $ \(ActionState ms _) -> readMVar ms <&> pure

modifyState :: (ServerState -> Action ServerState) -> Action ()
modifyState f = Action $ \s@(ActionState ms _) -> modifyMVar ms (modify s)
  where
    modify :: ActionState -> ServerState -> IO (ServerState, ActionResult ())
    modify s ss = do
      let (Action aa) = f ss
      aa s <&> \case
        Success ss' -> (ss', pure ())
        Failure e   -> (ss, Failure e)

readConn :: Action WS.Connection
readConn = Action $ \(ActionState _ conn) -> pure (pure conn)

send :: ToJSON a => a -> Action ()
send a = readConn >>= \conn -> liftIO (WS.sendTextData conn $ toJson a)

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb thing = do { b <- mb
                    ; when b thing }
                    
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb thing = do { b <- mb
                    ; unless b thing }
   
whenNothing :: Applicative f => Maybe a -> f a -> f a
whenNothing (Just x) _ = pure x
whenNothing Nothing  m = m
                    
whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM mm action = mm >>= \m -> whenNothing m action

finally :: Action a -> Action b -> Action a
finally (Action ma) (Action mb) = Action $ \s -> liftIO $ Ex.finally (ma s) (mb s)