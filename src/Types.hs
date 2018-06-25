{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           GHC.Generics               (Generic)

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import           GHC.Word                   (Word16)

import           Data.Aeson                 (FromJSON(..), ToJSON(..), withObject, )
import           Data.Pool                  (Pool)
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

newtype AppT m a = AppT {
    unAppT :: ReaderT AppEnv (ExceptT AppError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppEnv
             , MonadError AppError
             , MonadIO
             )

newtype App a = App {
    unApp :: AppT IO a
  } deriving (Functor
             , Applicative
             , Monad
             , MonadReader AppEnv
             , MonadError AppError
             , MonadIO)
