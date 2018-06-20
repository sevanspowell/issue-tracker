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

module DB.IssueTrackerDb where

import           Control.Lens
import           Data.Text
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend
import           Database.PostgreSQL.Simple       (Connection, Query)
import qualified Database.PostgreSQL.Simple       as PG

import           DB.Comment
import           DB.Issue
import           DB.User

data IssueTrackerDb f = IssueTrackerDb
                      { _issueTrackerUsers    :: f (TableEntity UserT)
                      , _issueTrackerIssues   :: f (TableEntity IssueT)
                      , _issueTrackerComments :: f (TableEntity CommentT)
                      } deriving Generic

instance Database be IssueTrackerDb

IssueTrackerDb (TableLens issueTrackerUsers) (TableLens issueTrackerIssues)
               (TableLens issueTrackerComments) = dbLenses

issueTrackerDb :: DatabaseSettings be IssueTrackerDb
issueTrackerDb = defaultDbSettings
