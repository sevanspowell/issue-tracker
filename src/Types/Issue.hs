{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Issue where

import           GHC.Generics  (Generic)

import           Control.Lens

import           Data.Aeson    (ToJSON (toJSON))
import           Data.Text     (Text)
import           Data.Time     (TimeZone, UTCTime, localTimeToUTC)

import           Database.Beam
import           DB            (DbIssue, DbIssueT (..), dbIssueId,
                                dbIssueStatus, dbIssueSubmissionTimestamp,
                                dbIssueSubmitter, dbIssueTitle)
import           Types         (IssueStatus (..))
import           Types.Error   (Error (..))
import           Types.User    (User (..), UserId, mkUserId)

newtype IssueId = IssueId Int
  deriving (Eq, Show, ToJSON)

newtype IssueTitle = IssueTitle Text
  deriving (Show, ToJSON)

mkIssueTitle :: Text -> Either Error IssueTitle
mkIssueTitle = Right . IssueTitle

getIssueTitle :: IssueTitle -> Text
getIssueTitle (IssueTitle txt) = txt

data Issue = Issue
  { issueId                  :: IssueId
  , issueTitle               :: IssueTitle
  , issueSubmitter           :: UserId
  , issueSubmissionTimestamp :: UTCTime
  , issueStatus              :: IssueStatus
  }
  deriving (Show, Generic)

-- Could maybe read timezone from config (server timezone)
fromDbIssue :: DbIssue -> TimeZone -> Either Error Issue
fromDbIssue issue tz =
  Issue (IssueId $ issue ^. dbIssueId)
  <$> mkIssueTitle (issue ^. dbIssueTitle)
  <*> mkUserId (issue ^. dbIssueSubmitter)
  <*> pure (localTimeToUTC tz $ issue ^. dbIssueSubmissionTimestamp)
  <*> pure (issue ^. dbIssueStatus)
