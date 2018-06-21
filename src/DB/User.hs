{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module DB.User where

import           Data.Text (Text)
import           Database.Beam

data UserT f
  = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text
  } deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Eq User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

User (LensFor userEmail)    (LensFor userFirstName)
     (LensFor userLastName) (LensFor userPassword) =
  tableLenses

