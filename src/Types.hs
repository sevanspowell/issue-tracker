{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))

data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum, Generic, ToJSON, FromJSON)
