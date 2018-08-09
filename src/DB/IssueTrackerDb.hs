{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}

module DB.IssueTrackerDb where

import           Database.Beam
import           Database.Beam.Postgres

import qualified Data.Time.LocalTime                      as T (getCurrentTimeZone)

import           Control.Lens

import           Database.PostgreSQL.Simple               (Connection, Query)

import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions

import           DB                                       (fromDbIssue)
import           DB.Comment
import           DB.Issue
import           DB.User
import           Types

data IssueTrackerDb f = IssueTrackerDb
                      { _issueTrackerUsers    :: f (TableEntity DbUserT)
                      , _issueTrackerIssues   :: f (TableEntity DbIssueT)
                      , _issueTrackerComments :: f (TableEntity DbCommentT)
                      } deriving Generic

instance Database be IssueTrackerDb

IssueTrackerDb (TableLens issueTrackerUsers) (TableLens issueTrackerIssues)
               (TableLens issueTrackerComments) = dbLenses

issueTrackerDb :: DatabaseSettings be IssueTrackerDb
issueTrackerDb = defaultDbSettings `withDbModification`
  dbModification
  { _issueTrackerUsers  = modifyTable (\_ -> "users")  tableModification
  , _issueTrackerIssues = modifyTable (\_ -> "issues") tableModification
  , _issueTrackerComments  = modifyTable (\_ -> "comments") tableModification
  }

getIssues :: Connection -> IO (Either AppError [Issue])
getIssues conn = do
  dbIssues <-
    runBeamPostgresDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          all_ (issueTrackerDb ^. issueTrackerIssues)

  tz <- T.getCurrentTimeZone

  pure . traverse (fromDbIssue tz) $ dbIssues
