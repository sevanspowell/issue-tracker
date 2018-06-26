module Layer2 where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.Reader       (MonadReader)

import Types.Issue
import Types.Comment
import Types.User
import Types.Error
import Types
import API.Types (IssueBlueprint)

class Monad m => MonadIssue m where
  addIssue :: (MonadError e m, MonadReader r m,
               AsAppError e, HasDatabaseConf r)
    => IssueBlueprint -> m ()
  getIssues :: m [Issue]

class Monad m => MonadComment m where
  addComment :: IssueId -> UserId -> CommentBody -> m ()
  getComments :: IssueId -> m [Comment]

-- class Monad m => MonadUser m where
