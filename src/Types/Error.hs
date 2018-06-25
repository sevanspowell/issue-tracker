{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Error (AppError(..)) where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))

data AppError = None
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
