module Layer2 where

import           Control.Lens
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader (MonadReader)

import           Types

class Monad m => MonadUser m where
  authenticateUser :: UserEmail -> UserPassword -> m (Maybe User)

class Monad m => MonadIssue m where
  addIssue :: UserId -> IssueBlueprint -> m ()
  getIssues :: m [Issue]

class Monad m => MonadComment m where
  addComment :: IssueId -> UserId -> CommentBody -> m ()
  getComments :: IssueId -> m [Comment]
