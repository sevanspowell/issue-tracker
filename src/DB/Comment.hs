{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

module DB.Comment where

import           Control.Lens
import           Data.Text                            (Text, unpack)
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend                as B
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Text.Read

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
