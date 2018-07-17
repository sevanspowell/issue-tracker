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

import           Control.Exception.Safe     (tryJust)
import           Control.Lens
import           Control.Monad.Except       (MonadError, join, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ask, lift)
import           Data.Pool                  (withResource)
import           Data.Text                  (Text, unpack)
import qualified Data.Time.LocalTime        as T (getCurrentTimeZone)
import qualified Database.Beam              as B
import qualified Database.Beam.Postgres     as B
import           Database.PostgreSQL.Simple (Connection, Query)

import           DB                         (fromDbIssue, fromDbUser)
import           DB.Issue                   (DbIssueT (..))
import           DB.IssueTrackerDb          (issueTrackerDb, issueTrackerIssues,
                                             issueTrackerUsers)
import           DB.User
import           Layer2                     (MonadIssue (..), MonadUser (..))
import           Types                      (AppError (..), AppT,
                                             DbConnection (..), DbError (..),
                                             Issue, IssueBlueprint (..),
                                             IssueStatus (..), User (..),
                                             UserEmail, dbConn, getUserEmail,
                                             getUserId)


beamErrors :: B.PgError -> Maybe AppError
beamErrors err@(B.PgRowParseError _) = Just . DbError . BeamPostgresError $ err
beamErrors err@(B.PgInternalError _) = Just . DbError . BeamPostgresError $ err

sqlErrors :: B.SqlError-> Maybe AppError
sqlErrors err@(B.SqlError _ _ _ _ _) = Just . DbError . PostgresSqlError $ err

-- sqlErrors err@(B.Incompatible _ _ _ _ _) = Just . DbError . PostgresResultError $ err
-- sqlErrors err@(B.UnexpectedNull _ _ _ _ _) = Just . DbError . PostgresResultError $ err
-- sqlErrors err@(B.ConversionFailed _ _ _ _ _) = Just . DbError . PostgresResultError $ err
-- sqlErrors _ = Nothing

instance (MonadIO m) => MonadUser (AppT m) where
  getUserByEmail email = do
    conns <- getDbConn <$> view dbConn

    let
      query =
        B.runSelectReturningOne $
          B.select $
          B.filter_ (\user -> (_userEmail user) B.==. (B.val_ $ getUserEmail email)) $
          B.all_ (issueTrackerDb ^. issueTrackerUsers)

    x <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
        B.runBeamPostgresDebug putStrLn conn $
          query

    user <- either throwError (pure) x

    case user of
      Nothing    -> pure $ Nothing
      (Just usr) -> either throwError (pure . Just) . fromDbUser $ usr

instance (MonadIO m) => MonadIssue (AppT m) where
  addIssue userId (IssueBlueprint title) = do
    conns <- getDbConn <$> view dbConn

    eUnit <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runInsert $
        B.insert (issueTrackerDb ^. issueTrackerIssues) $
          B.insertExpressions [DbIssue B.default_
                               (B.val_ title)
                               (B.val_ (DbUserId $ getUserId userId))
                               B.currentTimestamp_
                               (B.val_ (Open))
                              ]

    either throwError pure eUnit

  getIssues = do
    conns <- getDbConn <$> view dbConn

    eDbIssues <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runSelectReturningList $
        B.select $
        B.all_ (issueTrackerDb ^. issueTrackerIssues)

    dbIssues <- either throwError pure eDbIssues

    tz <- liftIO $ T.getCurrentTimeZone

    either throwError pure
      . traverse (fromDbIssue tz)
      $ dbIssues
