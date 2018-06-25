{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Comment (Comment
                     , mkCommentBody
                     , getCommentBody
                     , fromDbComment
                     , CommentBody
                     ) where

import           GHC.Generics  (Generic)

import           Control.Lens

import           Data.Aeson    (ToJSON (toJSON))
import           Data.Text     (Text)
import           Data.Time     (TimeZone, UTCTime, localTimeToUTC)

import           Database.Beam
import           DB            (DbComment, DbCommentT (..), dbCommentAuthor,
                                dbCommentBody, dbCommentForIssue, dbCommentId,
                                dbCommentPostedTimestamp)
import           Types.Error   (AppError (..))
import           Types.Issue   (IssueId, mkIssueId)
import           Types.User    (UserId, mkUserId)

newtype CommentId = CommentId Int
  deriving (Eq, Show, ToJSON)

newtype CommentBody = CommentBody Text
  deriving (Eq, Show, ToJSON)

data Comment = Comment
  { commentId              :: CommentId
  , commentForIssue        :: IssueId
  , commentAuthor          :: UserId
  , commentPostedTimestamp :: UTCTime
  , commentBody            :: CommentBody
  }
  deriving (Show, Generic)

mkCommentBody :: Text -> Either AppError CommentBody
mkCommentBody = Right . CommentBody

getCommentBody :: CommentBody -> Text
getCommentBody (CommentBody body) = body

fromDbComment :: DbComment -> TimeZone -> Either AppError Comment
fromDbComment comment tz =
  Comment (CommentId $ comment ^. dbCommentId)
  <$> mkIssueId (comment ^. dbCommentForIssue)
  <*> mkUserId  (comment ^. dbCommentAuthor)
  <*> pure (localTimeToUTC tz $ comment ^. dbCommentPostedTimestamp)
  <*> mkCommentBody (comment ^. dbCommentBody)
