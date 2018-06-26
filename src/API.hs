{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Bifunctor             (first)
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
import           Conf
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

postIssueC :: IssueBlueprint -> ClientM NoContent
getIssuesC :: ClientM [Issue]
postIssueC :<|> getIssuesC = client api

data StartupError
  = ConfError ConfigError
  | DbInitErr
  deriving Show

prepareAppEnv :: ExceptT StartupError IO AppEnv
prepareAppEnv = do
  conf <- initConf
  pool <- initPool conf

  pure (AppEnv conf (AppDb pool))

  where
    initConf :: ExceptT StartupError IO AppConf
    initConf = ExceptT . fmap (first ConfError) $ parseOptions "config.json"

    initPool :: AppConf -> ExceptT StartupError IO (Pool Connection)
    initPool conf =
      let
        dbConf = databaseConf conf
        connectionInfo = defaultConnectInfo
          { connectUser = unpack . getDbUser . databaseUser $ dbConf
          , connectPassword = unpack . getDbPassword . databasePassword $ dbConf
          , connectDatabase = unpack . getDbPath . databasePath $ dbConf
          , connectPort = Conf.Types.getPort . databasePort $ dbConf
          }
      in
        liftIO $ createPool (connect connectionInfo) close 1 60 10

destroyEnv :: AppEnv -> IO ()
destroyEnv = destroyAllResources . dbConn . appDb

main :: IO ()
main = do
  eEnv <- runExceptT prepareAppEnv

  case eEnv of
    (Left err) -> print err
    (Right env) -> do
      mgr <- newManager defaultManagerSettings
      bracket (forkIO $ runApp env) killThread $ \_ -> do
        ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
          traverse postIssueC (IssueBlueprint "Testing blueprint" <$> (mkUserId 1))
          getIssuesC
        print ms

      destroyEnv env
