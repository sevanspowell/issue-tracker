
module DB ( DbUserT(..)
          , DbUser
          , dbUserEmail
          , dbUserFirstName
          , dbUserId
          , dbUserLastName
          , dbUserPassword
          , DbIssueT(..)
          , DbIssue
          , dbIssueId
          , dbIssueTitle
          , dbIssueSubmitter
          , dbIssueSubmissionTimestamp
          , dbIssueStatus
          , DbCommentT(..)
          , DbComment
          , dbCommentId
          , dbCommentForIssue
          , dbCommentAuthor
          , dbCommentPostedTimestamp
          , dbCommentBody
          , fromDbUser
          , fromDbIssue
          , fromDbComment
          ) where

import qualified Database.Beam                            as B
import qualified Database.Beam.Postgres                   as B

import           Control.Lens
import           Data.Time                                (TimeZone, UTCTime,
                                                           localTimeToUTC)

import           Database.PostgreSQL.Simple               (Connection, Query)

import qualified Database.Beam.Backend.SQL.BeamExtensions as BE

import           DB.Comment                               (DbComment,
                                                           DbCommentT (..),
                                                           dbCommentAuthor,
                                                           dbCommentBody,
                                                           dbCommentForIssue,
                                                           dbCommentId,
                                                           dbCommentPostedTimestamp)
import           DB.Issue                                 (DbIssue,
                                                           DbIssueT (..),
                                                           dbIssueId,
                                                           dbIssueStatus,
                                                           dbIssueSubmissionTimestamp,
                                                           dbIssueSubmitter,
                                                           dbIssueTitle)
import           DB.User                                  (DbUser, DbUserT (..),
                                                           dbUserEmail,
                                                           dbUserFirstName,
                                                           dbUserId,
                                                           dbUserLastName,
                                                           dbUserPassword)

import           Types

fromDbUser :: DbUser -> Either AppError User
fromDbUser user =
  User
  <$> mkUserId (user ^. dbUserId)
  <*> mkUserEmail (user ^. dbUserEmail)
  <*> pure (user ^. dbUserFirstName)
  <*> pure (user ^. dbUserLastName)
  <*> mkUserPassword (user ^. dbUserPassword)

fromDbIssue :: TimeZone -> DbIssue -> Either AppError Issue
fromDbIssue tz issue =
  Issue (IssueId $ issue ^. dbIssueId)
  <$> mkIssueTitle (issue ^. dbIssueTitle)
  <*> mkUserId (issue ^. dbIssueSubmitter)
  <*> pure (localTimeToUTC tz $ issue ^. dbIssueSubmissionTimestamp)
  <*> pure (issue ^. dbIssueStatus)

fromDbComment :: DbComment -> TimeZone -> Either AppError Comment
fromDbComment comment tz =
  Comment (CommentId $ comment ^. dbCommentId)
  <$> mkIssueId (comment ^. dbCommentForIssue)
  <*> mkUserId  (comment ^. dbCommentAuthor)
  <*> pure (localTimeToUTC tz $ comment ^. dbCommentPostedTimestamp)
  <*> mkCommentBody (comment ^. dbCommentBody)

data DbError = PostgresError B.PgError
  deriving (Show)
