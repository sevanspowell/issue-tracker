
module DB (DbUserT(..), DbUser) where

import           Database.Beam
import           Database.Beam.Postgres

import           Control.Lens

import           Database.PostgreSQL.Simple               (Connection, Query)

import qualified Database.Beam.Backend.SQL.BeamExtensions as BeamExtensions

import           DB.User                                  (DbUserT(..), DbUser)

-- addCommentToIssue :: Connection -> Issue -> Comment -> IO (Either Error ())
-- addCommentToIssue conn issue comment
