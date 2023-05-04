module Data.Id
  ( Id
  , new
  )
where
   
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID

import Data.ByteString.Lazy.Internal as ByteString
import Data.Functor ((<&>))
import Data.Typeable (Typeable)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Aeson as Json
import Data.Aeson.Types as Json
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)

import qualified Database.MongoDB as Mongo
import Control.Monad (mzero)

newtype Id a = Id UUID
  deriving (Eq, Ord, Typeable)

new :: IO (Id a)
new = Id <$> nextRandom

instance Hashable (Id a) where
  hashWithSalt i (Id uuid) = hashWithSalt i uuid

instance ToJSON (Id a) where
  toJSON (Id uuid) = toJSON uuid

instance FromJSON (Id a) where
  parseJSON = Json.withText "Id" $ \str -> case UUID.fromText str of
    Just uuid -> pure (Id uuid)
    Nothing -> Json.parseFail "invalid id"

instance Typeable a => Mongo.Val (Id a) where
  val (Id uuid) = Mongo.Uuid . Mongo.UUID . ByteString.toStrict . UUID.toByteString $ uuid
  
  cast' (Mongo.Uuid (Mongo.UUID bstr)) = UUID.fromByteString (ByteString.fromStrict bstr) <&> Id
  cast' _ = Nothing
  
instance Show (Id a) where
  show (Id uuid) = show uuid