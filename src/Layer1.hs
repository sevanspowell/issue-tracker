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

module Layer1 where

import           Control.Lens
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader       (MonadReader, ask, lift)
import           Data.Pool                  (withResource)
import           Data.Text                  (unpack, Text)
import qualified Data.Time.LocalTime        as T (getCurrentTimeZone)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple (Connection, Query)

import           API.Types                  (IssueBlueprint (..))
import           DB.Issue                   (DbIssueT (..))
import           DB.IssueTrackerDb          (issueTrackerDb, issueTrackerIssues,
                                             issueTrackerUsers)
import           DB.User
import           Layer2                     (MonadIssue (..), MonadUser (..))
import           Types
import           Types.Error
import           Types.Issue                (Issue, fromDbIssue)
import           Types.User                 (UserEmail, fromDbUser,
                                             getUserEmail, getUserId)

instance (MonadIO m) => MonadUser (AppT m) where
  getUserByEmail email = do
    conns <- getDbConn <$> view dbConn

    user <- liftIO $ withResource conns $ \conn ->
      runBeamPostgresDebug putStrLn conn $
        runSelectReturningOne $
          select $
          filter_ (\user -> (_userEmail user) ==. (val_ $ getUserEmail email)) $
          all_ (issueTrackerDb ^. issueTrackerUsers)

    case user of
      Nothing    -> throwError None
      (Just usr) -> either throwError (pure . Just) . fromDbUser $ usr

instance (MonadIO m) => MonadIssue (AppT m) where
  addIssue (IssueBlueprint title submitter) = do
    conns <- getDbConn <$> view dbConn

    liftIO $ withResource conns $ \conn ->
      runBeamPostgresDebug putStrLn conn $
        runInsert $
        insert (issueTrackerDb ^. issueTrackerIssues) $
          insertExpressions [DbIssue default_
                              (val_ title)
                              (val_ (DbUserId $ getUserId submitter))
                              currentTimestamp_
                              (val_ (Open))
                            ]

  getIssues = do
    conns <- getDbConn <$> view dbConn

    dbIssues <- liftIO $ withResource conns $ \conn ->
      runBeamPostgresDebug putStrLn conn $
        runSelectReturningList $
          select $ do
            all_ (issueTrackerDb ^. issueTrackerIssues)

    tz <- liftIO $ T.getCurrentTimeZone

    either throwError pure
      . traverse (fromDbIssue tz)
      $ dbIssues
