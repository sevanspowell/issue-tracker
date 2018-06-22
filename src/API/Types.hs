{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module API.Types where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))
import           Data.Text    (Text)

import           Types.User   (UserId)

data IssueBlueprint = IssueBlueprint
  { issueBlueprintTitle     :: Text
  , issueBlueprintSubmitter :: UserId
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

