{-# LANGUAGE OverloadedStrings #-}

module DB.PostgreSQL where

import           Text.Read                            (readMaybe)

import           Database.PostgreSQL.Simple           (Connection, Query)
import qualified Database.PostgreSQL.Simple           as PG
import           Database.PostgreSQL.Simple.FromField (FromField,
                                                       ResultError (..),
                                                       fromField, returnError)

import           DB.IssueTrackerDb

import           Types                                (IssueStatus)

instance FromField IssueStatus where
  fromField f bs = do x <- readMaybe <$> (fromField f bs)
                      case x of
                        Nothing -> returnError ConversionFailed f "Could not 'read' value for 'IssueStatus'"
                        Just x -> pure x


closeDb :: Connection -> IO ()
closeDb = PG.close

createDbTables :: IO ()
createDbTables = do
  let info = PG.defaultConnectInfo
             { PG.connectUser = "postgres"
             , PG.connectDatabase = "issue_tracker"
             , PG.connectPassword = "abc"
             }
  conn <- PG.connect info

  _ <- PG.execute_ conn createUsersTableQ
  _ <- PG.execute_ conn createIssuesTableQ
  _ <- PG.execute_ conn createCommentsTableQ

  pure ()

rollbackDbTables = do
  let info = PG.defaultConnectInfo
             { PG.connectUser = "postgres"
             , PG.connectDatabase = "issue_tracker"
             , PG.connectPassword = "abc"
             }
  conn <- PG.connect info

  _ <- PG.execute_ conn deleteCommentsTableQ
  _ <- PG.execute_ conn deleteIssuesTableQ
  _ <- PG.execute_ conn deleteUsersTableQ

  pure ()

createUsersTableQ :: PG.Query
createUsersTableQ =
  "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, email TEXT UNIQUE NOT NULL, first_name TEXT NOT NULL, last_name TEXT NOT NULL, password TEXT NOT NULL)"

deleteUsersTableQ :: PG.Query
deleteUsersTableQ =
  "DROP TABLE IF EXISTS users"

createIssuesTableQ :: PG.Query
createIssuesTableQ =
  "CREATE TABLE IF NOT EXISTS issues (id SERIAL PRIMARY KEY, title TEXT NOT NULL, submitter__id integer REFERENCES users NOT NULL, submission_timestamp timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, status text NOT NULL)"

deleteIssuesTableQ :: PG.Query
deleteIssuesTableQ =
  "DROP TABLE IF EXISTS issues"

createCommentsTableQ :: PG.Query
createCommentsTableQ =
  "CREATE TABLE IF NOT EXISTS comments (id SERIAL PRIMARY KEY, for_issue__id integer REFERENCES issues NOT NULL, author__id integer REFERENCES users NOT NULL, posted_timestamp timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP, body text NOT NULL)"

deleteCommentsTableQ :: PG.Query
deleteCommentsTableQ =
  "DROP TABLE IF EXISTS comments"

testInsert :: IO ()
testInsert = do
  let info = PG.defaultConnectInfo
             { PG.connectUser = "postgres"
             , PG.connectDatabase = "issue_tracker"
             , PG.connectPassword = "abc"
             }
  conn <- PG.connect info

  insertUsers conn
  issues <- insertAndReturnIssues conn
  insertComments conn issues

  PG.close conn

testGetIssues :: IO ()
testGetIssues = do
  let info = PG.defaultConnectInfo
             { PG.connectUser = "postgres"
             , PG.connectDatabase = "issue_tracker"
             , PG.connectPassword = "abc"
             }
  conn <- PG.connect info

  issues <- getIssues conn

  mapM_ print issues

  PG.close conn
