module Data.Id
  ( Id
  , make
  )
where
  
import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson as Json
import Data.Aeson.Types as Json
import Data.ByteString.Lazy.Internal as ByteString
import Data.Functor ((<&>))
import Data.Hashable (Hashable, hashWithSalt)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import qualified Database.MongoDB as Mongo

newtype Id a = Id UUID
  deriving (Eq, Ord, Typeable)

make :: IO (Id a)
make = Id <$> nextRandom

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