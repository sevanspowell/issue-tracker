{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.User (UserId
                  , mkUserId
                  , getUserId
                  , UserEmail
                  , mkUserEmail
                  , getUserEmail
                  , UserPassword
                  , mkUserPassword
                  , getUserPassword
                  , User(..)
                  ) where

import           GHC.Generics (Generic)

import           Control.Lens

import           Data.Aeson   (ToJSON (toJSON), FromJSON)
import           Data.Text    (Text)

import           DB           (DbUser, DbUserT (..), dbUserEmail,
                               dbUserFirstName, dbUserId, dbUserLastName,
                               dbUserPassword)
import           Types.Error  (Error (..))

newtype UserId = UserId Int
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserEmail = UserEmail Text
  deriving (Show, ToJSON, FromJSON)

newtype UserPassword = UserPassword Text
  deriving (Show, ToJSON, FromJSON)

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

mkUserId :: Int -> Either Error UserId
mkUserId = Right . UserId

getUserId :: UserId -> Int
getUserId (UserId id) = id

fromDbUser :: DbUser -> Either Error User
fromDbUser user =
  User
  <$> mkUserId (user ^. dbUserId)
  <*> mkUserEmail (user ^. dbUserEmail)
  <*> pure (user ^. dbUserFirstName)
  <*> pure (user ^. dbUserLastName)
  <*> mkUserPassword (user ^. dbUserPassword)
