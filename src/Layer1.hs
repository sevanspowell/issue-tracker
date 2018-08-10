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
import qualified Crypto.BCrypt              as BCR
import           Data.Pool                  (withResource)
import           Data.Text                  (Text, unpack)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Time.LocalTime        as T (getCurrentTimeZone)
import qualified Database.Beam              as B
import qualified Database.Beam.Postgres     as B
import           Database.PostgreSQL.Simple (Connection, Query)

import           DB                         (fromDbComment, fromDbIssue,
                                             fromDbUser)
import           DB.Comment
import           DB.Issue
import           DB.IssueTrackerDb          (issueTrackerComments,
                                             issueTrackerDb, issueTrackerIssues,
                                             issueTrackerUsers)
import           DB.User
import           Layer2                     (MonadComment (..), MonadIssue (..),
                                             MonadUser (..))
import           Types                      (AppError (..), AppT,
                                             CommentBlueprint (..),
                                             DbConnection (..), DbError (..),
                                             Issue, IssueBlueprint (..),
                                             IssueStatus (..), User (..),
                                             UserBlueprint (..), UserEmail,
                                             dbConn, getIssueId, getUserEmail,
                                             getUserId, getUserPassword)


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
  addUser (UserBlueprint email fName lName pw) = do
    conns <- getDbConn <$> view dbConn

    mPass <- liftIO $ BCR.hashPasswordUsingPolicy BCR.slowerBcryptHashingPolicy (encodeUtf8 pw)
    pass <- maybe (throwError CouldntHash) (pure . decodeUtf8) mPass

    eUnit <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runInsert $
        B.insert (issueTrackerDb ^. issueTrackerUsers) $
          B.insertExpressions [DbUser B.default_
                               (B.val_ email)
                               (B.val_ fName)
                               (B.val_ lName)
                               (B.val_ pass)
                              ]

    either throwError pure eUnit

  authenticateUser email password = do
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

    mUser <- either throwError (pure) x

    case mUser of
      Nothing -> pure Nothing
      (Just dbUsr) -> do
        u <- either throwError pure $ fromDbUser dbUsr

        case BCR.validatePassword (encodeUtf8 $ getUserPassword $ userPassword u) (encodeUtf8 $ getUserPassword password) of
          False -> throwError IncorrectPassword
          -- TODO on correct, rehash if not using correct policy
          True  -> pure (Just u)

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

  updIssueStatus issueId status = do
    conns <- getDbConn <$> view dbConn

    eUnit <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runUpdate $ B.update (issueTrackerDb ^. issueTrackerIssues)
                    (\i -> [ (i ^. dbIssueStatus) B.<-. B.val_ (status) ])
                    (\i -> (i ^. dbIssueId) B.==. B.val_ (getIssueId issueId))

    either throwError pure eUnit

instance (MonadIO m) => MonadComment (AppT m) where
  addComment author (CommentBlueprint issueId body) = do
    conns <- getDbConn <$> view dbConn

    eUnit <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runInsert $
        B.insert (issueTrackerDb ^. issueTrackerComments) $
          B.insertExpressions [DbComment B.default_
                               (B.val_ (DbIssueId $ getIssueId issueId))
                               (B.val_ (DbUserId $ getUserId author))
                               B.currentTimestamp_
                               (B.val_ body)
                              ]

    either throwError pure eUnit

  getComments issueId = do

    conns <- getDbConn <$> view dbConn

    eComments <- liftIO $ fmap join $ tryJust sqlErrors $ withResource conns $ \conn ->
      tryJust beamErrors $
      B.runBeamPostgresDebug putStrLn conn $
        B.runSelectReturningList $
        B.select $
        B.filter_ (\c -> (_commentForIssue c) B.==. (B.val_ $ (DbIssueId $ getIssueId issueId))) $
        B.all_ (issueTrackerDb ^. issueTrackerComments)

    dbComments <- either throwError pure eComments

    tz <- liftIO $ T.getCurrentTimeZone

    either throwError pure
      . traverse (fromDbComment tz)
      $ dbComments
