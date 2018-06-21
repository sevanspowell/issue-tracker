{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module DB.User where

import           Data.Text (Text)
import           Database.Beam

data DbUserT f
  = DbUser
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving Generic

type DbUser = DbUserT Identity
type DbUserId = PrimaryKey DbUserT Identity

deriving instance Show DbUser
deriving instance Show (PrimaryKey DbUserT Identity)
deriving instance Eq DbUser

instance Table DbUserT where
    data PrimaryKey DbUserT f = DbUserId (Columnar f Text) deriving Generic
    primaryKey = DbUserId . _userEmail

instance Beamable DbUserT
instance Beamable (PrimaryKey DbUserT)

DbUser (LensFor userEmail)    (LensFor userFirstName)
       (LensFor userLastName) (LensFor userPassword) =
  tableLenses

