{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Error (Error(..)) where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))

data Error = None
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
