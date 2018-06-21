{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module DB.Comment where

import           Data.Text     (Text)
import           Data.Time     (LocalTime)
import           Database.Beam

import           DB.Issue
import           DB.User

data DbCommentT f
  = DbComment
  { _commentId              :: Columnar f Int
  , _commentForIssue        :: PrimaryKey DbIssueT f
  , _commentAuthor          :: PrimaryKey DbUserT f
  , _commentPostedTimestamp :: Columnar f LocalTime
  , _commentBody            :: Columnar f Text
  } deriving Generic

type DbComment = DbCommentT Identity
type DbCommentId = PrimaryKey DbCommentT Identity

deriving instance Show DbComment

DbComment (LensFor dbCommentId) (DbIssueId (LensFor dbCommentForIssue))
          (DbUserId (LensFor dbCommentAuthor)) (LensFor dbCommentPostedTimestamp)
          (LensFor dbCommentBody) =
  tableLenses

instance Table DbCommentT where
  data PrimaryKey DbCommentT f = DbCommentId (Columnar f Int) deriving Generic
  primaryKey = DbCommentId . _commentId

instance Beamable DbCommentT
instance Beamable (PrimaryKey DbCommentT)
