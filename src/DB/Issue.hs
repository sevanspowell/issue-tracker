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

module DB.Issue where

import           Control.Lens
import           Data.Text
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL

import           DB.User

data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum)

data IssueT f
  = Issue
  { _issueId                  :: Columnar f Int
  , _issueTitle               :: Columnar f Text
  , _issueSubmitter           :: PrimaryKey UserT f
  , _issueSubmissionTimestamp :: Columnar f LocalTime
  , _issueStatus              :: Columnar f IssueStatus
  } deriving Generic

type Issue = IssueT Identity
type IssueId = PrimaryKey IssueT Identity

deriving instance Show Issue

instance Table IssueT where
  data PrimaryKey IssueT f = IssueId (Columnar f Int) deriving Generic
  primaryKey = IssueId . _issueId

instance Beamable IssueT
instance Beamable (PrimaryKey IssueT)
deriving instance Show (PrimaryKey IssueT Identity)

Issue (LensFor issueId) (LensFor issueTitle)
      (UserId (LensFor issueSubmitter)) (LensFor issueSubmissionTimestamp)
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
