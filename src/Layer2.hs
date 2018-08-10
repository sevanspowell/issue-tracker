module Layer2 where

import           Control.Lens
import           Control.Monad.Except (MonadError)
import           Control.Monad.Reader (MonadReader)

import           Types

class Monad m => MonadUser m where
  addUser :: UserBlueprint -> m ()
  authenticateUser :: UserEmail -> UserPassword -> m (Maybe User)

class Monad m => MonadIssue m where
  addIssue :: UserId -> IssueBlueprint -> m ()
  getIssues :: m [Issue]
  updIssueStatus :: IssueId -> IssueStatus -> m ()

class Monad m => MonadComment m where
  addComment :: UserId -> CommentBlueprint -> m ()
  -- getComments :: IssueId -> m [Comment]
