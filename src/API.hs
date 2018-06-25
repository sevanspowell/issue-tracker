{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.ByteString            (ByteString)
import           Data.Pool
import           Data.Text                  (unpack)
import qualified Data.Time.Clock            as T
import           Database.PostgreSQL.Simple
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

import           API.Types
import           Conf.Types
import qualified DB.IssueTrackerDb          as DB
import           Types
import           Types.Error
import           Types.Issue
import           Types.User

import           Layer1
import           Layer2

type API = ReqBody '[JSON] IssueBlueprint :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Issue]

api :: Proxy API
api = Proxy

initDB :: ConnectInfo -> IO ()
initDB info = bracket (connect info) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text NOT NULL)"
  pure ()

type AppM = AppT Handler

server :: ServerT API AppM
server = postIssueHandler :<|> getIssuesHandler
  where
    postIssueHandler :: IssueBlueprint -> AppM NoContent
    postIssueHandler = fmap (const NoContent) . addIssue

    getIssuesHandler :: AppM [Issue]
    getIssuesHandler = getIssues

nt :: AppEnv -> (AppM :~> Handler)
nt env = NT $ ((either (throwError . toServantErr) pure) =<<)
  . runExceptT
  . flip runReaderT env
  . unAppT
  where
    toServantErr :: AppError -> ServantErr
    toServantErr None = err404 { errBody = "Unknown Error"}

app :: AppEnv -> Application
app env = serve api $ enter (nt env) server

runApp :: AppEnv -> IO ()
runApp env = run 8080 (app env)

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info =
  createPool (connect info)
             close
             1
             60
             10

postIssueC :: IssueBlueprint -> ClientM NoContent
getIssuesC :: ClientM [Issue]
postIssueC :<|> getIssuesC = client api

main :: IO ()
main = do
  let
    pth = DbPath "issue_tracker"
    usr = DbUser "postgres"
    pw  = DbPassword "abc"
    port = Port 5432

    conf = AppConf port pth usr pw

    connectionInfo = defaultConnectInfo
                       { connectUser     = unpack . getDbUser $ usr
                       , connectPassword = unpack . getDbPassword $ pw
                       , connectDatabase = unpack . getDbPath $ pth
                       , connectPort = Conf.Types.getPort port
                       }

  pool <- initConnectionPool connectionInfo

  let
    appDb = AppDb pool
    env = AppEnv conf appDb

  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp env) killThread $ \_ -> do
    ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      traverse postIssueC (IssueBlueprint "Testing blueprint" <$> (mkUserId 1))
      getIssuesC
    print ms
