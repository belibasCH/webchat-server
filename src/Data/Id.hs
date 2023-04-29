module Data.Id
  ( Id
  , new
  ) where
   
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Data.Hashable (Hashable, hashWithSalt)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

newtype Id a = Id UUID
  deriving (Show, Eq, Ord)

new :: IO (Id a)
new = Id <$> nextRandom

instance Hashable (Id a) where
  hashWithSalt i (Id uuid) = hashWithSalt i uuid

instance ToJSON (Id a) where
  toJSON (Id uuid) = toJSON uuid

instance FromJSON (Id a) where
  parseJSON v = parseJSON v