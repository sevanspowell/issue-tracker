module Layer2 where

import           Control.Lens
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader (MonadReader)

import           API.Types            (IssueBlueprint (..))
import           Types
import           Types.Comment
import           Types.Error
import           Types.Issue
import           Types.User

class Monad m => MonadUser m where
  getUserByEmail :: UserEmail -> m (Maybe User)

class Monad m => MonadIssue m where
  addIssue :: UserId -> IssueBlueprint -> m ()
  getIssues :: m [Issue]

class Monad m => MonadComment m where
  addComment :: IssueId -> UserId -> CommentBody -> m ()
  getComments :: IssueId -> m [Comment]
