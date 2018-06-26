{-# LANGUAGE OverloadedStrings #-}

module Conf where

import           Data.Bifunctor (first)
import           Data.Monoid    (Last (Last), getLast)
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Text      (Text)

import           Conf.File      (parseJSONConfigFile)
import           Conf.Types

defaultNetworkConf :: PartialNetworkConf
defaultNetworkConf = PartialNetworkConf
  (pure $ Port 3000)

defaultDatabaseConf :: PartialDatabaseConf
defaultDatabaseConf = PartialDatabaseConf
  (pure $ Port 5432)
  (pure $ DbPath "app_db")
  (pure $ DbUser "admin")
  (pure $ DbPassword "admin")

defaultConf :: PartialAppConf
defaultConf = PartialAppConf
  (pure defaultNetworkConf)
  (pure defaultDatabaseConf)

makeNetworkConfig
  :: PartialNetworkConf
  -> Either NetworkConfigError AppNetworkConf
makeNetworkConfig pc = AppNetworkConf
  <$> lastToEither MissingNetworkPort pcNetworkPort
  where
    lastToEither
      :: NetworkConfigError
      -> (PartialNetworkConf -> Last b)
      -> Either NetworkConfigError b
    lastToEither e g =
      (maybe (Left e) Right . getLast . g) pc

makeDatabaseConfig
  :: PartialDatabaseConf
  -> Either DatabaseConfigError AppDatabaseConf
makeDatabaseConfig pc = AppDatabaseConf
  <$> lastToEither MissingDatabasePort pcDatabasePort
  <*> lastToEither MissingDatabasePath pcDatabasePath
  <*> lastToEither MissingDatabaseUser pcDatabaseUser
  <*> lastToEither MissingDatabasePassword pcDatabasePassword
  where
    lastToEither
      :: DatabaseConfigError
      -> (PartialDatabaseConf -> Last b)
      -> Either DatabaseConfigError b
    lastToEither e g =
      (maybe (Left e) Right . getLast . g) pc

makeConfig
  :: PartialAppConf
  -> Either ConfigError AppConf
makeConfig pc = do
  pcNet <- lastToEither MissingNetworkConfig pcNetworkConf
  pcDb  <- lastToEither MissingDatabaseConfig pcDatabaseConf
  let netConf = first ConfigErrorNetwork $ makeNetworkConfig pcNet
      dbConf  = first ConfigErrorDatabase $ makeDatabaseConfig pcDb
  AppConf <$> netConf <*> dbConf
  where
    lastToEither
      :: ConfigError
      -> (PartialAppConf -> Last b)
      -> Either ConfigError b
    lastToEither e g =
      (maybe (Left e) (Right) . getLast . g) pc

parseOptions
  :: FilePath
  -> IO (Either ConfigError AppConf)
parseOptions fp =
  let mkCfg file = makeConfig (defaultConf <> file)
  in do
    ( >>= mkCfg ) <$> parseJSONConfigFile fp
