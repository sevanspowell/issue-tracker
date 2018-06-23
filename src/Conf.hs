{-# LANGUAGE OverloadedStrings #-}

module Conf where

import           Data.Monoid    (Last (Last), getLast)
import           Data.Semigroup (Semigroup ((<>)))
import           Data.Text      (Text)

import           Conf.File      (parseJSONConfigFile)
import           Conf.Types

defaultConf :: PartialAppConf
defaultConf = PartialAppConf
  (pure (Port 3000))
  (pure (DbPath "app_db"))
  (pure (DbUser "admin"))
  (pure (DbPassword "admin"))

makeConfig
  :: PartialAppConf
  -> Either ConfigError AppConf
makeConfig pc = AppConf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingDbPath pcDbPath
  <*> lastToEither MissingDbUser pcDbUser
  <*> lastToEither MissingDbPassword pcDbPassword
  where
    lastToEither
      :: ConfigError
      -> (PartialAppConf -> Last b)
      -> Either ConfigError b
    lastToEither e g =
      (maybe (Left e) Right . getLast . g) pc

parseOptions
  :: FilePath
  -> IO (Either ConfigError AppConf)
parseOptions fp =
  let mkCfg file = makeConfig (defaultConf <> file)
  in do
    ( >>= mkCfg ) <$> parseJSONConfigFile fp
