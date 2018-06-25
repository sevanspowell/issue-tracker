module Layer2 where

import Types.Issue
import Types.Comment
import Types.User
import API.Types (IssueBlueprint)

class Monad m => MonadIssue m where
  addIssue :: IssueBlueprint -> m ()
  getIssues :: m [Issue]

class Monad m => MonadComment m where
  addComment :: IssueId -> UserId -> CommentBody -> m ()
  getComments :: IssueId -> m [Comment]

-- class Monad m => MonadUser m where
