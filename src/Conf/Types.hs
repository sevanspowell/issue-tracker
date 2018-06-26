{-# LANGUAGE OverloadedStrings #-}

module Conf.Types where

import           Data.Aeson      (FromJSON (..), ToJSON, (.:?), (.:))
import qualified Data.Aeson      as A
import           Data.Monoid     (Last (Last), getLast)
import           Data.Semigroup  (Semigroup ((<>)))
import           Data.Text       (Text)
import           GHC.Word        (Word16)
import           System.IO.Error (IOError)

newtype Port = Port
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype DbPath = DbPath
  { getDbPath :: Text }
  deriving (Eq, Show)

newtype DbUser = DbUser
  { getDbUser :: Text }
  deriving (Eq, Show)

newtype DbPassword = DbPassword
  { getDbPassword :: Text }
  deriving (Eq, Show)

data AppNetworkConf = AppNetworkConf
  { networkPort :: Port }
  deriving (Eq, Show)

data AppDatabaseConf = AppDatabaseConf
  { databasePort     :: Port
  , databasePath     :: DbPath
  , databaseUser     :: DbUser
  , databasePassword :: DbPassword
  }
  deriving (Eq, Show)

data AppConf = AppConf
  { networkConf  :: AppNetworkConf
  , databaseConf :: AppDatabaseConf
  }
  deriving (Eq, Show)

data NetworkConfigError
  = MissingNetworkPort
  deriving Show

data DatabaseConfigError
  = MissingDatabasePort
  | MissingDatabasePath
  | MissingDatabaseUser
  | MissingDatabasePassword
  deriving Show

data ConfigError
  = MissingNetworkConfig
  | MissingDatabaseConfig
  | ConfigErrorNetwork NetworkConfigError
  | ConfigErrorDatabase DatabaseConfigError
  | JSONDecodeError String
  | ConfigFileReadError IOError
  deriving Show

data PartialNetworkConf = PartialNetworkConf
  { pcNetworkPort :: Last Port }
  deriving Show

data PartialDatabaseConf = PartialDatabaseConf
  { pcDatabasePort     :: Last Port
  , pcDatabasePath     :: Last DbPath
  , pcDatabaseUser     :: Last DbUser
  , pcDatabasePassword :: Last DbPassword
  }
  deriving Show

data PartialAppConf = PartialAppConf
 { pcNetworkConf  :: Maybe PartialNetworkConf
 , pcDatabaseConf :: Maybe PartialDatabaseConf
 }
 deriving Show

instance Semigroup PartialNetworkConf where
  a <> b = PartialNetworkConf
    { pcNetworkPort = pcNetworkPort a <> pcNetworkPort b
    }

instance Semigroup PartialDatabaseConf where
  a <> b = PartialDatabaseConf
    { pcDatabasePort = pcDatabasePort a <> pcDatabasePort b
    , pcDatabasePath = pcDatabasePath a <> pcDatabasePath b
    , pcDatabaseUser = pcDatabaseUser a <> pcDatabaseUser b
    , pcDatabasePassword = pcDatabasePassword a <> pcDatabasePassword b
    }

instance Semigroup PartialAppConf where
  a <> b = PartialAppConf
    { pcNetworkConf  = pcNetworkConf a <> pcNetworkConf b
    , pcDatabaseConf = pcDatabaseConf a <> pcDatabaseConf b
    }

instance Monoid PartialNetworkConf where
  mempty = PartialNetworkConf mempty
  mappend = (<>)

instance Monoid PartialDatabaseConf where
  mempty = PartialDatabaseConf mempty mempty mempty mempty
  mappend = (<>)

instance Monoid PartialAppConf where
  mempty = PartialAppConf mempty mempty
  mappend = (<>)

-- parseToLast :: String -> a -> b -> Last b
parseToLast k c o = Last . fmap c <$> o .:? k

instance FromJSON PartialNetworkConf where
  parseJSON = A.withObject "PartialNetworkConf" $ \o -> PartialNetworkConf
    <$> parseToLast "networkPort" Port o

instance FromJSON PartialDatabaseConf where
  parseJSON = A.withObject "PartialDatabaseConf" $ \o -> PartialDatabaseConf
    <$> parseToLast "databasePort" Port o
    <*> parseToLast "databasePath" DbPath o
    <*> parseToLast "databaseUser" DbUser o
    <*> parseToLast "databasePassword" DbPassword o

instance FromJSON PartialAppConf where
  parseJSON = A.withObject "PartialAppConf" $ \o -> PartialAppConf
    <$> (o .:? "networkConf")
    <*> (o .:? "databaseConf")
