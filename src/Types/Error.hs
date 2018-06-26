{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Error (AppError(..), AsAppError(..)) where

import           Control.Lens
import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))

data AppError = None
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

class AsAppError t where
  _AppError :: Prism' t AppError
  _None :: Prism' t ()

  _None = _AppError . _None

instance AsAppError AppError where
  _AppError = id
  _None =
    prism (const None) $ \x -> case x of
                                None -> Right ()
                                _ -> Left x

-- makeClassyPrisms ''AppError
