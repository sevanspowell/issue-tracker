{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module API.Types where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON, ToJSON (toJSON))
import           Data.Text    (Text)

import           Types.Issue  (IssueId)
import           Types.User   (UserId)

data IssueBlueprint = IssueBlueprint
  { issueBlueprintTitle :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CommentBlueprint = CommentBlueprint
  { commentBlueprintForIssue :: IssueId
  , commentBlueprintAuthor   :: UserId
  , commentBlueprintBody     :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
