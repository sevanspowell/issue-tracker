{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where

import           GHC.Generics               (Generic)

import           Data.Text                  (Text)
import           GHC.Word                   (Word16)

import           Data.Aeson                 (FromJSON, ToJSON (toJSON))
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)

import           Types.Error


data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum, Generic, ToJSON, FromJSON)

newtype AppDb = AppDb
  { dbConn :: Pool Connection
  }
