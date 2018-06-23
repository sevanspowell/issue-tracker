{-# LANGUAGE OverloadedStrings #-}

module Conf.Types where

import           Data.Aeson      (FromJSON (..), ToJSON, (.:?))
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

data AppConf = AppConf
  { port       :: Port
  , dbPath     :: DbPath
  , dbUser     :: DbUser
  , dbPassword :: DbPassword
  }
  deriving (Eq, Show)

data ConfigError
  = MissingPort
  | MissingDbPath
  | MissingDbUser
  | MissingDbPassword
  | JSONDecodeError String
  | ConfigFileReadError IOError
  deriving Show

data PartialAppConf = PartialAppConf
 { pcPort       :: Last Port
 , pcDbPath     :: Last DbPath
 , pcDbUser     :: Last DbUser
 , pcDbPassword :: Last DbPassword
 }

instance Semigroup PartialAppConf where
  a <> b = PartialAppConf
    { pcPort       = pcPort a <> pcPort b
    , pcDbPath     = pcDbPath a <> pcDbPath b
    , pcDbUser     = pcDbUser a <> pcDbUser b
    , pcDbPassword = pcDbPassword a <> pcDbPassword b
    }

instance Monoid PartialAppConf where
  mempty = PartialAppConf mempty mempty mempty mempty
  mappend = (<>)

instance FromJSON PartialAppConf where
  parseJSON = A.withObject "PartialAppConf" $ \o -> PartialAppConf
    <$> parseToLast "port" Port o
    <*> parseToLast "dbPath" DbPath o
    <*> parseToLast "dbUser" DbUser o
    <*> parseToLast "dbPassword" DbPassword o
    where
      parseToLast k c o = Last . fmap c <$> o .:? k
