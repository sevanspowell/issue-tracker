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
import qualified Data.Text                  as T
import           Data.Time                  (UTCTime)
import qualified Database.Beam.Postgres     as B
import           GHC.Word                   (Word16)
import           Text.Read                  (readEither)

import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             withObject)
import           Data.Pool                  (Pool, destroyAllResources)
import           Database.PostgreSQL.Simple (Connection)

import           Conf.Types
import           Servant.Auth.Server        (AuthResult, FromJWT, ToJWT)
import           Web.Internal.HttpApiData   (FromHttpApiData (..),
                                             ToHttpApiData (..),
                                             defaultParseError)


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
              | IncorrectPassword
              | CouldntHash
              | DbError DbError
              | AuthenticationError (AuthResult AuthenticatedUser)
  deriving (Show)

-- User

newtype UserId = UserId Int
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserEmail = UserEmail T.Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserPassword = UserPassword T.Text
  deriving (Show, ToJSON, FromJSON)

data User = User
  { userId        :: UserId
  , userEmail     :: UserEmail
  , userFirstName :: T.Text
  , userLastName  :: T.Text
  , userPassword  :: UserPassword
  }
  deriving (Show, Generic)

mkUserEmail :: T.Text -> Either AppError UserEmail
mkUserEmail = Right . UserEmail

getUserEmail :: UserEmail -> T.Text
getUserEmail (UserEmail txt) = txt

mkUserPassword :: T.Text -> Either AppError UserPassword
mkUserPassword = Right . UserPassword

getUserPassword :: UserPassword -> T.Text
getUserPassword (UserPassword txt) = txt

mkUserId :: Int -> Either AppError UserId
mkUserId = Right . UserId

getUserId :: UserId -> Int
getUserId (UserId id) = id

-- Issue

newtype IssueId = IssueId Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromHttpApiData IssueId where
  parseUrlPiece = bimap T.pack IssueId . readEither . T.unpack

instance ToHttpApiData IssueId where
  toUrlPiece (IssueId i) = T.pack $ show i

newtype IssueTitle = IssueTitle T.Text
  deriving (Show, Generic, ToJSON, FromJSON)

mkIssueId :: Int -> Either AppError IssueId
mkIssueId = Right . IssueId

getIssueId :: IssueId -> Int
getIssueId (IssueId id) = id

mkIssueTitle :: T.Text -> Either AppError IssueTitle
mkIssueTitle = Right . IssueTitle

getIssueTitle :: IssueTitle -> T.Text
getIssueTitle (IssueTitle txt) = txt

data Issue = Issue
  { issueId                  :: IssueId
  , issueTitle               :: IssueTitle
  , issueSubmitter           :: UserId
  , issueSubmissionTimestamp :: UTCTime
  , issueStatus              :: IssueStatus
  , issueAssignedTo          :: Maybe UserId
  }
  deriving (Show, Generic)

instance ToJSON Issue where
instance FromJSON Issue

-- Comment

newtype CommentId = CommentId Int
  deriving (Eq, Show, ToJSON, FromJSON)

newtype CommentBody = CommentBody T.Text
  deriving (Eq, Show, ToJSON, FromJSON)

data Comment = Comment
  { commentId              :: CommentId
  , commentForIssue        :: IssueId
  , commentAuthor          :: UserId
  , commentPostedTimestamp :: UTCTime
  , commentBody            :: CommentBody
  }
  deriving (Show, Generic)

instance ToJSON Comment where
instance FromJSON Comment

mkCommentBody :: T.Text -> Either AppError CommentBody
mkCommentBody = Right . CommentBody

getCommentBody :: CommentBody -> T.Text
getCommentBody (CommentBody body) = body

-- Blueprints
data IssueBlueprint = IssueBlueprint
  { issueBlueprintTitle      :: T.Text
  , issueBlueprintAssignedTo :: Maybe UserId
  }
  deriving (Show, Eq, Generic)

instance ToJSON IssueBlueprint
instance FromJSON IssueBlueprint

data CommentBlueprint = CommentBlueprint
  { commentBlueprintForIssue :: IssueId
  , commentBlueprintBody     :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CommentBlueprint
instance FromJSON CommentBlueprint

data UserBlueprint = UserBlueprint
  { userBlueprintEmail     :: T.Text
  , userBlueprintFirstName :: T.Text
  , userBlueprintLastName  :: T.Text
  , userBlueprintPassword  :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserBlueprint
instance FromJSON UserBlueprint
