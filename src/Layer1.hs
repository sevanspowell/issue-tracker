{-# LANGUAGE InstanceSigs #-}
module Layer1 where

import           Control.Lens
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Reader       (MonadReader, ask, lift)
import           Data.Pool                  (withResource)
import qualified Data.Time.LocalTime        as T (getCurrentTimeZone)
import           Database.Beam
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple (Connection, Query)

import           API.Types                  (IssueBlueprint (..))
import           DB.Issue                   (DbIssueT (..))
import           DB.IssueTrackerDb          (issueTrackerDb, issueTrackerIssues)
import           DB.User
import           Layer2                     (MonadIssue (..))
import           Types
import           Types.Issue                (Issue, fromDbIssue)
import           Types.User                 (getUserId)

instance (MonadIO m) => MonadIssue (AppT m) where
  addIssue :: MonadIO m => IssueBlueprint -> AppT m ()
  addIssue (IssueBlueprint title submitter) = do
    env <- ask

    let conns = dbConn . appDb $ env

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

  getIssues :: (MonadIO m) => AppT m [Issue]
  getIssues = do
    env <- ask

    let conns = dbConn . appDb $ env

    dbIssues <- liftIO $ withResource conns $ \conn ->
      runBeamPostgresDebug putStrLn conn $
        runSelectReturningList $
          select $ do
            all_ (issueTrackerDb ^. issueTrackerIssues)

    tz <- liftIO $ T.getCurrentTimeZone

    either throwError pure
      . traverse (fromDbIssue tz)
      $ dbIssues
