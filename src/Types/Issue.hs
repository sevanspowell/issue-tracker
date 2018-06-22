{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types.Issue (IssueId
                   , mkIssueId
                   , getIssueId
                   , mkIssueTitle
                   , getIssueTitle
                   , fromDbIssue
                   , Issue(..)
                   ) where

import           GHC.Generics  (Generic)

import           Control.Lens

import           Data.Aeson    (FromJSON, ToJSON (toJSON), defaultOptions,
                                genericToEncoding, parseJSON, toEncoding,
                                withObject, (.:))
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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype IssueTitle = IssueTitle Text
  deriving (Show, Generic, ToJSON, FromJSON)

mkIssueId :: Int -> Either Error IssueId
mkIssueId = Right . IssueId

getIssueId :: IssueId -> Int
getIssueId (IssueId id) = id

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

instance ToJSON Issue where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Issue where
    parseJSON = withObject "Issue" $ \v -> Issue
        <$> v .: "issueId"
        <*> v .: "issueTitle"
        <*> v .: "issueSubmitter"
        <*> v .: "issueSubmissionTimestamp"
        <*> v .: "issueStatus"

-- Could maybe read timezone from config (server timezone)
fromDbIssue :: TimeZone -> DbIssue -> Either Error Issue
fromDbIssue tz issue =
  Issue (IssueId $ issue ^. dbIssueId)
  <$> mkIssueTitle (issue ^. dbIssueTitle)
  <*> mkUserId (issue ^. dbIssueSubmitter)
  <*> pure (localTimeToUTC tz $ issue ^. dbIssueSubmissionTimestamp)
  <*> pure (issue ^. dbIssueStatus)
