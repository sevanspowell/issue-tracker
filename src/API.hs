{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class
import           Data.ByteString            (ByteString)
import           Data.Pool
import qualified Data.Time.Clock            as T
import           Database.PostgreSQL.Simple
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (runExceptT)

import qualified DB.IssueTrackerDb          as DB
import           API.Types
import           Types
import           Types.Error
import           Types.Issue
import           Types.User

type API = ReqBody '[JSON] IssueBlueprint :> Post '[JSON] NoContent
      :<|> Get '[JSON] (Either AppError [Issue])

api :: Proxy API
api = Proxy

initDB :: ConnectInfo -> IO ()
initDB info = bracket (connect info) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text NOT NULL)"
  pure ()

type AppM = AppT Handler

server :: Pool Connection -> ServerT API AppM
server conns = postIssueBlueprint :<|> getIssues

  where
    postIssueBlueprint :: IssueBlueprint -> AppM NoContent
    postIssueBlueprint issueBlueprint = do
      liftIO . withResource conns $ \conn ->
        DB.insertNewIssue conn issueBlueprint

      pure NoContent

    getIssues :: AppM [Issue]
    getIssues = do
      liftIO . withResource conns $ \conn ->
        DB.getIssues conn

nt :: AppEnv -> (AppM :~> Handler)
nt env = NT $ (flip (>>=) (either (throwError . toServantErr) pure)) . runExceptT . flip runReaderT env . unAppT
  where
    toServantErr :: AppError -> ServantErr
    toServantErr None = err404 { errBody = "Unknown Error"}

app :: AppEnv -> Application
app env = let
    conns = dbConn . appDb $ env
  in
    serve api $ enter (nt env) (server conns)

runApp :: AppEnv -> IO ()
runApp env = let
    conns = dbConn . appDb $ env
  in
    run 8080 (app env)

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info =
  createPool (connect info)
             close
             1
             60
             10

postIssue :: IssueBlueprint -> ClientM NoContent
getIssues :: ClientM (Either AppError [Issue])
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
