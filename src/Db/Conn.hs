{-# LANGUAGE OverloadedStrings #-}

module Db.Conn
  ( Conn
  , Db.Conn.connect
  , run
  , Write (write, writeId)
  , Db.Conn.Read (read, order)
  , asc
  , desc
  ) where

import qualified Database.MongoDB as Mongo
import Control.Exception (catch)

import Data.Id (Id)

newtype Conn = Conn Mongo.Pipe

connect :: IO Conn
connect = do
  pp <- Mongo.connect (Mongo.host "db")
  isLoggedIn <- run (Conn pp) $ Mongo.auth "webchat" "webchat"
  if isLoggedIn
    then return ()
    else error "database login failed"
  pure $ Conn pp

run :: Conn -> Mongo.Action IO a -> IO a
run (Conn pp) ac = do
  catch (Mongo.access pp Mongo.master "webchat" ac) handle
  where
    handle :: Mongo.Failure -> IO a
    handle f = error (show f)

class Write a where
  write :: a -> Mongo.Document
  writeId :: a -> Id a

asc :: Int
asc = 1

desc :: Int
desc = -1

class Read a where
  read :: Mongo.Document -> a

  order :: [a] -> Mongo.Order
  order _ = []