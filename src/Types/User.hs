{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.User (UserId
                  , UserEmail
                  , mkUserEmail
                  , getUserEmail
                  , UserPassword
                  , mkUserPassword
                  , getUserPassword
                  , User(..)
                  ) where

import           GHC.Generics (Generic)

import           Data.Aeson   (ToJSON (toJSON))
import           Data.Text    (Text)

import           DB           (DbUser, DbUserT (..))
import           Types.Error  (Error (..))

newtype UserId = UserId Int
  deriving (Eq, Show, ToJSON)

newtype UserEmail = UserEmail Text
  deriving (Show, ToJSON)

newtype UserPassword = UserPassword Text
  deriving (Show, ToJSON)

data User = User
  { userId        :: UserId
  , userEmail     :: UserEmail
  , userFirstName :: Text
  , userLastName  :: Text
  , userPassword  :: UserPassword
  }
  deriving (Show, Generic)

mkUserEmail :: Text -> Either Error UserEmail
mkUserEmail = Right . UserEmail

getUserEmail :: UserEmail -> Text
getUserEmail (UserEmail txt) = txt

mkUserPassword :: Text -> Either Error UserPassword
mkUserPassword = Right . UserPassword

getUserPassword :: UserPassword -> Text
getUserPassword (UserPassword txt) = txt

fromDbUser :: DbUser -> Either Error User
fromDbUser (DbUser dbId dbEmail dbFName dbLName dbPw) =
  User (UserId dbId)
  <$> mkUserEmail dbEmail
  <*> pure dbFName
  <*> pure dbLName
  <*> mkUserPassword dbPw
