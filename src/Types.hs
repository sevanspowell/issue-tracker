{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           GHC.Generics               (Generic)

import           Control.Lens
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import           GHC.Word                   (Word16)

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             withObject)
import           Data.Pool                  (Pool, destroyAllResources)
import           Database.PostgreSQL.Simple (Connection)

import           Conf.Types
import           Types.Error


data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum, Generic)

instance ToJSON IssueStatus
instance FromJSON IssueStatus

newtype AppDb = AppDb
  { dbConn :: Pool Connection
  }

data AppEnv = AppEnv
  { appConf :: AppConf
  , appDb   :: AppDb
  }

destroyEnv :: AppEnv -> IO ()
destroyEnv = destroyAllResources . dbConn . appDb

newtype AppT m a = AppT {
    unAppT :: ReaderT AppEnv (ExceptT AppError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppEnv
             , MonadError AppError
             , MonadIO
             )

class HasDatabaseConf t where
  dbConfig :: Lens' t AppDatabaseConf
  dbPort :: Lens' t Port
  dbPath :: Lens' t DbPath
  dbUser :: Lens' t DbUser
  dbPassword :: Lens' t DbPassword

  dbPort = dbConfig . dbPort
  dbPath = dbConfig . dbPath
  dbUser = dbConfig . dbUser
  dbPassword = dbConfig . dbPassword

instance HasDatabaseConf AppDatabaseConf where
  dbConfig = id
  dbPort = lens databasePort (\d c -> d { databasePort = c })
  dbPath = lens databasePath (\d c -> d { databasePath = c })
  dbUser = lens databaseUser (\d c -> d { databaseUser = c })
  dbPassword = lens databasePassword (\d c -> d { databasePassword = c })

class HasNetworkConf t where
  netConfig :: Lens' t AppNetworkConf
  netPort :: Lens' t Port

  netPort = netConfig . netPort

instance HasNetworkConf AppNetworkConf where
  netConfig = id
  netPort = lens networkPort (\d c -> d { networkPort = c })

instance HasDatabaseConf AppConf where
  dbConfig = lens databaseConf (\d c -> d { databaseConf = c })

instance HasNetworkConf AppConf where
  netConfig = lens networkConf (\d c -> d { networkConf = c })

instance HasDatabaseConf AppEnv where
  dbConfig = lens appConf (\d c -> d { appConf = c }) . dbConfig
