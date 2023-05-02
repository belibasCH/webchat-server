module Api.Client
  ( Client
  , make
  , Api.Client.id
  , userId
  , conn
  , send
  ) where

import Data.Functor ((<&>))
import Data.Aeson (ToJSON)
  
import qualified Network.WebSockets as WS

import Data.Id (Id)
import qualified Data.Id as Id

import Data.User (User)
import qualified Data.User

import Api.Question
import Api.Answer
import Result
import Api.Utils (toJson)

data Client = Client
  { id :: Id Client
  , userId :: Id User
  , conn :: WS.Connection
  }

make :: Id User -> WS.Connection -> IO Client
make uId c = Id.new <&> \cId -> Client
  { Api.Client.id = cId, userId = uId, conn = c }

send :: ToJSON a => a -> Client -> ResultT IO ()
send a c = wrap . WS.sendTextData (conn c) $ toJson a