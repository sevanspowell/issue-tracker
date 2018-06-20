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

module DB.PostgreSQL where

import           Database.PostgreSQL.Simple       (Connection, Query)
import qualified Database.PostgreSQL.Simple       as PG

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
  PG.execute_ conn createUserTableQ
  pure ()

createUserTableQ :: PG.Query
createUserTableQ =
  "CREATE TABLE IF NOT EXISTS users (email TEXT PRIMARY KEY, user_first_name TEXT NOT NULL, user_last_name TEXT NOT NULL, user_password TEXT NOT NULL)"
