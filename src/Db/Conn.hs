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

import Control.Exception (finally, catch)

data Conn = Conn

connect :: IO Conn
connect = do
--  pp <- Mongo.connect (Mongo.host "db")
--  isLoggedIn <- run (Conn pp) $ Mongo.auth "webchat" "webchat"
--  if isLoggedIn
--    then return ()
--    else error "database login failed"
  pure $ Conn

run :: Conn -> Mongo.Action IO a -> IO a
run Conn ac = do
  pp <- Mongo.connect (Mongo.host "db")
  isLoggedIn <- Mongo.access pp Mongo.master "webchat" $ Mongo.auth "webchat" "webchat"
  _ <- if isLoggedIn
    then return ()
    else error "database login failed"
  let q = catch (Mongo.access pp Mongo.master "webchat" ac) handle
  q `finally` Mongo.close pp
  where
    handle :: Mongo.Failure -> IO a
    handle f = error (show f)

class Write a where
  write :: a -> Mongo.Document

class Read a where
  read :: Mongo.Document -> a