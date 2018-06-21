{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module DB.Issue where

import           Data.Text                 (Text, unpack)
import           Data.Time                 (LocalTime)
import           Database.Beam
import           Database.Beam.Backend.SQL (BeamBackend, HasSqlValueSyntax,
                                            autoSqlValueSyntax, sqlValueSyntax)

import           DB.User
import           Types.Issue               (IssueStatus (..))

data DbIssueT f
  = DbIssue
  { _issueId                  :: Columnar f Int
  , _issueTitle               :: Columnar f Text
  , _issueSubmitter           :: PrimaryKey DbUserT f
  , _issueSubmissionTimestamp :: Columnar f LocalTime
  , _issueStatus              :: Columnar f IssueStatus
  } deriving Generic

type DbIssue = DbIssueT Identity
type DbIssueId = PrimaryKey DbIssueT Identity

deriving instance Show DbIssue

instance Table DbIssueT where
  data PrimaryKey DbIssueT f = DbIssueId (Columnar f Int) deriving Generic
  primaryKey = DbIssueId . _issueId

instance Beamable DbIssueT
instance Beamable (PrimaryKey DbIssueT)
deriving instance Show (PrimaryKey DbIssueT Identity)

DbIssue (LensFor issueId) (LensFor issueTitle)
        (DbUserId (LensFor issueSubmitter)) (LensFor issueSubmissionTimestamp)
        (LensFor issueStatus) =
  tableLenses

-- Issue has a custom 'IssueStatus' type, have to tell Beam how to deserialize it.
instance HasSqlValueSyntax be String => HasSqlValueSyntax be IssueStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be IssueStatus where
  fromBackendRow = do
    val <- fromBackendRow
    case (val :: Text) of
      "Open"   -> pure Open
      "Closed" -> pure Closed
      _        -> fail ("Invalid value for IssueStatus: " ++ unpack val)
