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

import           Control.Lens

import           Database.PostgreSQL.Simple               (Connection, Query)

import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions

import           DB.Comment
import           DB.Issue
import           DB.User
import           Types                                    (IssueStatus(..))

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

users :: [DbUser]
users@[james, betty, sam] = [ DbUser 1 "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
                            , DbUser 2 "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
                            , DbUser 3 "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"]

insertAndReturnIssues :: Connection -> IO ([DbIssue])
insertAndReturnIssues conn = do
  let issues =
        [ DbIssue default_ (val_ "Big problem") (val_ (pk james)) currentTimestamp_ (val_ Open)
        , DbIssue default_ (val_ "Small problem") (val_ (pk betty)) currentTimestamp_ (val_ Open)
        , DbIssue default_ (val_ "Med. problem") (val_ (pk sam)) currentTimestamp_ (val_ Closed)
        ]

  runBeamPostgresDebug putStrLn conn $
    BeamExtensions.runInsertReturningList (issueTrackerDb ^. issueTrackerIssues) $
      insertExpressions issues

insertUsers :: Connection -> IO ()
insertUsers conn =
  runBeamPostgresDebug putStrLn conn $
    runInsert $
      insert (_issueTrackerUsers issueTrackerDb) $
        insertValues users

insertComments :: Connection -> [DbIssue] -> IO ()
insertComments conn issues =
  runBeamPostgresDebug putStrLn conn $
    runInsert $
      insert (issueTrackerDb ^. issueTrackerComments) $
        insertExpressions $
          [ DbComment default_ (val_ (pk (issues !! 0))) (val_ (pk james)) currentTimestamp_ (val_ "This is a big problem")
          , DbComment default_ (val_ (pk (issues !! 1))) (val_ (pk betty)) currentTimestamp_ (val_ "This is a small problem")
          , DbComment default_ (val_ (pk (issues !! 2))) (val_ (pk sam)) currentTimestamp_ (val_ "This is a medium problem")
          , DbComment default_ (val_ (pk (issues !! 2))) (val_ (pk betty)) currentTimestamp_ (val_ "Agreed, fixed in PR #253")
          ]
