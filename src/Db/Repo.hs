{-# LANGUAGE OverloadedStrings #-}

module Db.Repo
  ( Repo (..)
  , save
  , find
  , findUserByName
  , listUnreceivedMessages
  ) where

import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Id (Id)
import Control.Monad.IO.Class (liftIO)
import Data.Message (Message)
import Data.Message as Message
import Data.User (User)
import Data.Data (Typeable)

import qualified Db.Conn as Db

import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

data Repo a = Repo Text Db.Conn

save :: Typeable a => Db.Write a => a -> Repo a -> IO ()
save a (Repo col conn) = Db.run conn $ Mongo.upsert (Mongo.select ["_id" =: Db.writeId a] col) (Db.write a)

find :: Typeable a => Db.Read a => Id a -> Repo a -> IO (Maybe a)
find aId (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (Mongo.select ["_id" =: aId] col)
  pure $ doc <&> Db.read

findUserByName :: Text -> Repo User -> IO (Maybe User)
findUserByName n (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (Mongo.select ["name" =: Mongo.Regex ("\\Q" <> n <> "\\E") "i"] col)
  pure $ doc <&> Db.read

listUnreceivedMessages :: Id User -> Repo Message -> IO [Message]
listUnreceivedMessages recId (Repo col conn) = Db.run conn $ do
  docs <- Mongo.rest =<< Mongo.findCommand
    (Mongo.select ["receiver_id" =: recId, "received_at" =: Mongo.Null] col)
    { Mongo.sort = ["sent_at" =: (1 :: Int)] }
  pure $ docs <&> Db.read