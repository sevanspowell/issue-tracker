{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module DB.Comment where

import           Data.Text                            (Text)
import           Data.Time                            (LocalTime)
import           Database.Beam

import           DB.Issue
import           DB.User

data CommentT f
  = Comment
  { _commentId              :: Columnar f Int
  , _commentForIssue        :: PrimaryKey IssueT f
  , _commentAuthor          :: PrimaryKey UserT f
  , _commentPostedTimestamp :: Columnar f LocalTime
  , _commentBody            :: Columnar f Text
  } deriving Generic

type Comment = CommentT Identity
type CommentId = PrimaryKey CommentT Identity

deriving instance Show Comment

Comment (LensFor commentId) (IssueId (LensFor commentForIssue))
        (UserId (LensFor commentAuthor)) (LensFor commentPostedTimestamp)
        (LensFor commentBody) =
  tableLenses

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId (Columnar f Int) deriving Generic
  primaryKey = CommentId . _commentId

instance Beamable CommentT
instance Beamable (PrimaryKey CommentT)
