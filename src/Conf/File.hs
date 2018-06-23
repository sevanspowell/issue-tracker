module Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (Text)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import           Data.Aeson                 (FromJSON, Object, (.:))

import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A

import           Conf.Types

-- | Read a config file into a bytestring.
readConfFile
  :: FilePath
  -> IO (Either ConfigError ByteString)
readConfFile =
  fmap (first ConfigFileReadError) . try . LBS.readFile

-- | Construct a partial config from a config file.
parseJSONConfigFile
  :: FilePath
  -> IO (Either ConfigError PartialAppConf)
parseJSONConfigFile =
  fmap (first JSONDecodeError . A.eitherDecode =<<) . readConfFile
