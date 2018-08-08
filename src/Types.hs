{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           GHC.Generics               (Generic)

import           Control.Lens
import           Control.Lens.TH
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader       (MonadReader)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import qualified Database.Beam.Postgres     as B
import           GHC.Word                   (Word16)

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             withObject)
import           Data.Pool                  (Pool, destroyAllResources)
import           Database.PostgreSQL.Simple (Connection)

import           Conf.Types
import           Servant.Auth.Server (AuthResult, ToJWT, FromJWT)


data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum, Generic)

instance ToJSON IssueStatus
instance FromJSON IssueStatus

newtype DbConnection = DbConnection
  { getDbConn :: Pool Connection
  }

newtype AppDb = AppDb
  { appDbConnection :: DbConnection
  }

data AppEnv = AppEnv
  { appConf :: AppConf
  , appDb   :: AppDb
  }

class HasAppDatabase t where
  db :: Lens' t (AppDb)
  dbConn :: Lens' t (DbConnection)
  dbConn = db . dbConn

instance HasAppDatabase AppDb where
  db = id
  dbConn = lens appDbConnection (\d c -> d { appDbConnection = c })

instance HasAppDatabase AppEnv where
  db = lens appDb (\d c -> d { appDb = c })

destroyEnv :: AppEnv -> IO ()
destroyEnv = destroyAllResources . getDbConn . appDbConnection . appDb

newtype AppT m a = AppT {
    unAppT :: ReaderT AppEnv (ExceptT AppError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppEnv
             , MonadError AppError
             , MonadIO
             )

data AuthenticatedUser = AUser { auId :: UserId
                               } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-- Errors
data DbError = BeamPostgresError B.PgError
             | PostgresResultError B.ResultError
             | PostgresSqlError B.SqlError
  deriving (Show)

data AppError = NoUser
              | DbError DbError
              | AuthenticationError (AuthResult AuthenticatedUser)
  deriving (Show)

-- User

newtype UserId = UserId Int
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserEmail = UserEmail Text
  deriving (Eq, Show, ToJSON, FromJSON)

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

mkUserEmail :: Text -> Either AppError UserEmail
mkUserEmail = Right . UserEmail

getUserEmail :: UserEmail -> Text
getUserEmail (UserEmail txt) = txt

mkUserPassword :: Text -> Either AppError UserPassword
mkUserPassword = Right . UserPassword

getUserPassword :: UserPassword -> Text
getUserPassword (UserPassword txt) = txt

mkUserId :: Int -> Either AppError UserId
mkUserId = Right . UserId

getUserId :: UserId -> Int
getUserId (UserId id) = id

-- Issue

newtype IssueId = IssueId Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype IssueTitle = IssueTitle Text
  deriving (Show, Generic, ToJSON, FromJSON)

mkIssueId :: Int -> Either AppError IssueId
mkIssueId = Right . IssueId

getIssueId :: IssueId -> Int
getIssueId (IssueId id) = id

mkIssueTitle :: Text -> Either AppError IssueTitle
mkIssueTitle = Right . IssueTitle

getIssueTitle :: IssueTitle -> Text
getIssueTitle (IssueTitle txt) = txt

data Issue = Issue
  { issueId                  :: IssueId
  , issueTitle               :: IssueTitle
  , issueSubmitter           :: UserId
  , issueSubmissionTimestamp :: UTCTime
  , issueStatus              :: IssueStatus
  }
  deriving (Show, Generic)

instance ToJSON Issue where
instance FromJSON Issue

-- Comment

newtype CommentId = CommentId Int
  deriving (Eq, Show, ToJSON)

newtype CommentBody = CommentBody Text
  deriving (Eq, Show, ToJSON)

data Comment = Comment
  { commentId              :: CommentId
  , commentForIssue        :: IssueId
  , commentAuthor          :: UserId
  , commentPostedTimestamp :: UTCTime
  , commentBody            :: CommentBody
  }
  deriving (Show, Generic)

mkCommentBody :: Text -> Either AppError CommentBody
mkCommentBody = Right . CommentBody

getCommentBody :: CommentBody -> Text
getCommentBody (CommentBody body) = body

-- Blueprints
data IssueBlueprint = IssueBlueprint
  { issueBlueprintTitle :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON IssueBlueprint
instance FromJSON IssueBlueprint

data CommentBlueprint = CommentBlueprint
  { commentBlueprintForIssue :: IssueId
  , commentBlueprintAuthor   :: UserId
  , commentBlueprintBody     :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CommentBlueprint
instance FromJSON CommentBlueprint
