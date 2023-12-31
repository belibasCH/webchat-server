{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db.Repo
  ( Repo (..)
  , save
  , list
  , find
  , exists
  , delete
  , findUserByName
  , existsUserByName
  , updateUserName
  , updateUserPassword
  , updateUserAvatar
  , listChatMessages
  , listUnreceivedMessages
  , findLatestChatMessage
  , countTotalChatMessages
  , countUnreadChatMessages
  , updateMessageReceivedAt
  , updateMessageReadAt
  , deleteMessagesOfUser
  )
where

import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.Id (Id)
import Data.Message (Message)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.User (User, Username, Password, Avatar, PrivateKey, MessageKey)
import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import qualified Db.Conn as Db

data Repo a = Repo Text Db.Conn

save :: Typeable a => Db.Write a => a -> Repo a -> IO ()
save a (Repo col conn) = Db.run conn $ Mongo.upsert (Mongo.select ["_id" =: Db.writeId a] col) (Db.write a)

list :: forall a. Db.Read a => Repo a -> IO [a]
list (Repo col conn) = Db.run conn $ do
  docs <- Mongo.rest =<< Mongo.findCommand
    (Mongo.select [] col)
    { Mongo.sort = Db.order Db.asc ([] :: [a]) }
  pure $ docs <&> Db.read

find :: Typeable a => Db.Read a => Id a -> Repo a -> IO (Maybe a)
find aId (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (Mongo.select ["_id" =: aId] col)
  pure $ doc <&> Db.read

exists :: Typeable a => Id a -> Repo a -> IO Bool
exists aId (Repo col conn) = Db.run conn $ do
  n <- Mongo.count (Mongo.select ["_id" =: aId] col)
  pure $ n /= 0

delete :: Typeable a => Id a -> Repo a -> IO Bool
delete aId (Repo col conn) = Db.run conn $ do
  n <- Mongo.count (Mongo.select ["_id" =: aId] col)
  if n == 0
    then pure False
    else do
      Mongo.deleteOne (Mongo.select ["_id" =: aId] col)
      pure True

findUserByName :: Username -> Repo User -> IO (Maybe User)
findUserByName un (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (userByNameQuery un col)
  pure $ doc <&> Db.read

existsUserByName :: Username -> Repo User -> IO Bool
existsUserByName un (Repo col conn) = Db.run conn $ do
  isEmpty <- Mongo.findCommand (userByNameQuery un col) { Mongo.limit = 1 } >>= Mongo.isCursorClosed
  pure $ not isEmpty

userByNameQuery :: Username -> Mongo.Collection -> Mongo.Query
userByNameQuery un = Mongo.select ["name" =: Mongo.Regex ("\\Q" <> un <> "\\E") "i"]

updateUserName :: Username -> Id User -> Repo User -> IO (Maybe User)
updateUserName un uId (Repo col conn) =  Db.run conn $ do
  doc <- Mongo.findAndModify
    (Mongo.select ["_id" =: uId] col)
    ["$set" =: ["name" =: un]]
  pure $ case doc of
    Left _ -> Nothing
    Right u -> Just (Db.read u)

updateUserPassword :: Password -> PrivateKey -> MessageKey -> Id User -> Repo User -> IO (Maybe User)
updateUserPassword pw sk mk uId (Repo col conn) =  Db.run conn $ do
  doc <- Mongo.findAndModify
    (Mongo.select ["_id" =: uId] col)
    ["$set" =: ["password" =: pw, "private_key" =: sk, "message_key" =: mk]]
  pure $ case doc of
    Left _ -> Nothing
    Right u -> Just (Db.read u)

updateUserAvatar :: Maybe Avatar -> Id User -> Repo User -> IO (Maybe User)
updateUserAvatar av uId (Repo col conn) =  Db.run conn $ do
  doc <- Mongo.findAndModify
    (Mongo.select ["_id" =: uId] col)
    ["$set" =: ["avatar" =: av]]
  pure $ case doc of
    Left _ -> Nothing
    Right u -> Just (Db.read u)

listChatMessages :: Id User -> Id User -> Repo Message -> IO [Message]
listChatMessages uId1 uId2 (Repo col conn) = Db.run conn $ do
  docs <- Mongo.rest =<< Mongo.findCommand
    (chatMessageQuery uId1 uId2 col)
    { Mongo.sort = Db.order Db.asc ([] :: [Message]) }
  pure $ docs <&> Db.read

listUnreceivedMessages :: Id User -> Repo Message -> IO [Message]
listUnreceivedMessages recId (Repo col conn) = Db.run conn $ do
  docs <- Mongo.rest =<< Mongo.findCommand
    (Mongo.select ["receiver_id" =: recId, "received_at" =: Mongo.Null] col)
    { Mongo.sort = Db.order Db.asc ([] :: [Message]) }
  pure $ docs <&> Db.read

findLatestChatMessage :: Id User -> Id User -> Repo Message -> IO (Maybe Message)
findLatestChatMessage uId1 uId2 (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findOne (chatMessageQuery uId1 uId2 col) { Mongo.sort = Db.order Db.desc ([] :: [Message]) }
  pure $ doc <&> Db.read

countTotalChatMessages :: Id User -> Id User -> Repo Message -> IO Int
countTotalChatMessages uId1 uId2 (Repo col conn) = Db.run conn $
  Mongo.count (chatMessageQuery uId1 uId2 col)

countUnreadChatMessages :: Id User -> Id User -> Repo Message -> IO Int
countUnreadChatMessages sendId recId (Repo col conn) = Db.run conn $
  Mongo.count (Mongo.select ["sender_id" =: sendId, "receiver_id" =: recId, "read_at" =: Mongo.Null] col)

updateMessageReceivedAt :: UTCTime -> Id User -> Id Message -> Repo Message -> IO (Maybe Message)
updateMessageReceivedAt at recId msgId (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findAndModify
    (Mongo.select ["_id" =: msgId, "receiver_id" =: recId] col)
    ["$set" =: ["received_at" =: at]]
  pure $ case doc of
    Left _ -> Nothing
    Right msg -> Just (Db.read msg)

updateMessageReadAt :: UTCTime -> Id User -> Id Message -> Repo Message -> IO (Maybe Message)
updateMessageReadAt at recId msgId (Repo col conn) = Db.run conn $ do
  doc <- Mongo.findAndModify
    (Mongo.select ["_id" =: msgId, "receiver_id" =: recId] col)
    ["$set" =: ["read_at" =: at]]
  pure $ case doc of
    Left _ -> Nothing
    Right msg -> Just (Db.read msg)

chatMessageQuery :: Id User -> Id User -> Mongo.Collection -> Mongo.Query
chatMessageQuery uId1 uId2 = Mongo.select
  ["$or" =:
    [ ["sender_id" =: uId1, "receiver_id" =: uId2]
    , ["sender_id" =: uId2, "receiver_id" =: uId1]
    ]
  ]

deleteMessagesOfUser :: Id User -> Repo Message -> IO ()
deleteMessagesOfUser uId (Repo col conn) = Db.run conn $ do
  let q = Mongo.select ["$or" =: [["sender_id" =: uId], ["receiver_id" =: uId]]]
  Mongo.delete (q col)