{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Result
  ( Error (..)
  , Result (..)
  , ResultT (..)
  , Result.finally
  , IOWrap (..)
  ) where

import Control.Exception as Ex
import Data.Text (Text)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson.Types as Json

data Error
  = BadRequest Text
  | LoginFailed Text
  | UsernameTaken Text
  deriving (Show)

instance ToJSON Error where
  toJSON (BadRequest e) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("bad_request" :: Text)
    , "message" .= e
    ]

  toJSON (LoginFailed u) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("login_failed" :: Text)
    , "username" .= u
    ]

  toJSON (UsernameTaken u) = Json.object
    [ "type" .= ("error" :: Text)
    , "error" .= ("name_taken" :: Text)
    , "username" .= u
    ]
    
    
data Result a
  = Success a
  | Failure Error
  deriving (Show)

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative Result where
   pure = Success

   Failure e <*> _ = Failure e
   _ <*> Failure e = Failure e
   Success f <*> Success a = Success (f a)

instance Monad Result where
  Failure e >>= _ = Failure e
  Success a >>= f = f a


-- | The parameterizable result monad, obtained by composing an arbitrary
---- monad with the Result' monad.
--- 
--- Based on `MaybeT` from https://hackage.haskell.org/package/transformers-0.6.1.0/docs/src/Control.Monad.Trans.Maybe.html#MaybeT.
newtype ResultT m a = ResultT { runResultT :: m (Result a) }

mapResultT :: (m (Result a) -> n (Result b)) -> ResultT m a -> ResultT n b
mapResultT f = ResultT . f . runResultT

finally :: ResultT IO a -> ResultT IO b -> ResultT IO a
finally ma mb =
  ResultT $ Ex.finally (runResultT ma) (runResultT mb)

class IOWrap m where
   wrap :: m a -> ResultT IO a

instance IOWrap IO where
  wrap m = ResultT { runResultT = fmap Success m }
  
instance IOWrap Result where
  wrap m = ResultT { runResultT = pure m }

instance Functor m => Functor (ResultT m) where
   fmap f = mapResultT (fmap (fmap f))

instance Monad m => Applicative (ResultT m) where
    pure = ResultT . return . Success

    mf <*> mx = ResultT $ do
        mb_f <- runResultT mf
        case mb_f of
            Failure e -> return (Failure e)
            Success f  -> do
                mb_x <- runResultT mx
                case mb_x of
                    Failure e -> return (Failure e)
                    Success x  -> return (Success (f x))

    m *> k = m >> k

instance Monad m => Monad (ResultT m) where
    x >>= f = ResultT $ do
        v <- runResultT x
        case v of
            Failure e -> return (Failure e)
            Success y  -> runResultT (f y)
