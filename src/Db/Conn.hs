{-# LANGUAGE OverloadedStrings #-}

module Db.Conn
  ( Conn
  , Db.Conn.connect
  , run
  , Write (write)
  , Db.Conn.Read (read)
  ) where

import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import Data.Functor ((<&>))
import Data.Text (Text)

newtype Conn = Conn Mongo.Pipe

connect :: IO Conn
connect = do
  conn <- Mongo.connect (Mongo.host "db")
  isLoggedIn <- run (Conn conn) $ Mongo.auth "webchat" "webchat"
  if isLoggedIn
    then return ()
    else error "database login failed"
  s <- run (Conn conn) $ Mongo.findOne (Mongo.select [] "team")
  pure $ Conn conn

run :: Conn -> Mongo.Action IO a -> IO a
run (Conn pp) = Mongo.access pp Mongo.master "webchat"

class Write a where
  write :: a -> Mongo.Document

class Read a where
  read :: Mongo.Document -> a