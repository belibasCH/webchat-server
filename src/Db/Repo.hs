{-# LANGUAGE OverloadedStrings #-}

module Db.Repo
  ( Repo (..)
  , create
  , findUserByName
  ) where
    
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.User (User)
   
import qualified Db.Conn as Db

import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
    
data Repo a = Repo Text Db.Conn

create :: Db.Write a => a -> Repo a -> IO ()
create a (Repo col conn) = Db.run conn $ Mongo.insert_ col (Db.write a)

findUserByName :: Text -> Repo User -> IO (Maybe User)
findUserByName n (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (Mongo.select ["name" =: Mongo.Regex ("\\Q" <> n <> "\\E") "i" ] col)
  pure $ doc <&> Db.read