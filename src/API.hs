{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           API.Types
import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class
import           Data.ByteString            (ByteString)
import           Data.Pool
import qualified Data.Time.Clock            as T
import           Database.PostgreSQL.Simple
import qualified DB.IssueTrackerDb          as DB
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Types
import           Types.Error
import           Types.Issue
import           Types.User

type API = ReqBody '[JSON] IssueBlueprint :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Either Error [Issue])

api :: Proxy API
api = Proxy

initDB :: ConnectInfo -> IO ()
initDB info = bracket (connect info) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text NOT NULL)"
  pure ()

server :: Pool Connection -> Server API
server conns = postIssueBlueprint :<|> getIssues

  where
    postIssueBlueprint :: IssueBlueprint -> Handler NoContent
    postIssueBlueprint issueBlueprint = do
      liftIO . withResource conns $ \conn ->
        DB.insertNewIssue conn issueBlueprint

      pure NoContent

    getIssues :: Handler (Either Error [Issue])
    getIssues = do
      liftIO . withResource conns $ \conn ->
        DB.getIssues conn

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info =
  createPool (connect info)
             close
             1
             60
             10

postIssue :: IssueBlueprint -> ClientM NoContent
getIssues :: ClientM (Either Error [Issue])
postIssue :<|> getIssues = client api

main :: IO ()
main = do
  let connectionInfo = defaultConnectInfo
                       { connectUser = "postgres"
                       , connectPassword = "abc"
                       , connectDatabase = "issue_tracker"
                       }
  pool <- initConnectionPool connectionInfo
  initDB connectionInfo
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      traverse postIssue (IssueBlueprint "Testing blueprint" <$> (mkUserId 1))
      getIssues
    print ms
