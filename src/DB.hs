
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
          ) where

import           Database.Beam
import           Database.Beam.Postgres

import           Control.Lens

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

-- addCommentToIssue :: Connection -> Issue -> Comment -> IO (Either Error ())
-- addCommentToIssue conn issue comment
