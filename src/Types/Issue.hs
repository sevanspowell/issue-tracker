module Types.Issue where

data IssueStatus = Open | Closed
                 deriving (Show, Read, Eq, Ord, Enum)
