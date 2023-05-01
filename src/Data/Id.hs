module Data.Id
  ( Id
  , new
  ) where
   
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

import Data.Typeable (Typeable)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

import qualified Database.MongoDB as Mongo

newtype Id a = Id UUID
  deriving (Show, Eq, Ord, Typeable)

new :: IO (Id a)
new = Id <$> nextRandom

instance Hashable (Id a) where
  hashWithSalt i (Id uuid) = hashWithSalt i uuid

instance ToJSON (Id a) where
  toJSON (Id uuid) = toJSON uuid

instance FromJSON (Id a) where
  parseJSON v = parseJSON v

instance Typeable a => Mongo.Val (Id a) where
  val (Id uuid) = Mongo.String (UUID.toText uuid)