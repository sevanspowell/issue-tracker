{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Data.Bifunctor (first)
import           Control.Exception          (bracket)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
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
import qualified Conf                       as Conf
import qualified Conf.Types                 as Conf
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

data StartupError
  = ConfError Conf.ConfigError
  | DbInitErr
  deriving Show

-- prepareAppEnv :: IO (Either StartupError AppEnv)
-- prepareAppEnv =
--   where

initConf :: ExceptT StartupError IO Conf.AppConf
initConf = ExceptT . fmap (first ConfError) $ Conf.parseOptions "config.json"

-- initPool :: Conf.AppConf -> ExceptT StartupError IO (Pool Connection)
-- initPool = ExceptT . _f $ createPool ()

main :: IO ()
main = do
  conf <- runExceptT initConf
  print conf

-- main :: IO ()
-- main = do
--   eAppConf <- parseOptions "config.json"

--   pure $ do
--     conf <- eAppConf
--     let connectionInfo
--           = defaultConnectInfo
--             { connectUser = unpack . getDbUser . dbUser $ conf
--             , connectPassword = unpack . getDbPassword . dbPassword $ conf
--             , connectDatabase = unpack . getDbPath . dbPath $ conf
--             , connectPort = Conf.Types.getPort . port $ conf
--             }

--   pool <- initConnectionPool connectionInfo

--   let
--     appDb = AppDb pool
--     env = AppEnv conf appDb

--   mgr <- newManager defaultManagerSettings
--   bracket (forkIO $ runApp env) killThread $ \_ -> do
--     ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
--       traverse postIssueC (IssueBlueprint "Testing blueprint" <$> (mkUserId 1))
--       getIssuesC
--     print ms
