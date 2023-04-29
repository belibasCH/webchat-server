module Data.Id
  ( Id
  , make
  ) where
   
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Data.Hashable (Hashable, hashWithSalt)

newtype Id a = Id UUID
  deriving (Show, Eq, Ord)

make :: IO (Id a)
make = Id <$> nextRandom

instance Hashable (Id a) where
  hashWithSalt i (Id uuid) = hashWithSalt i uuid