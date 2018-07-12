{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Control.Exception          (bracket, try)
import           Control.Monad.IO.Class
import           Control.Monad (join)
import           Control.Monad.Reader       (ask)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import           Data.Pool
import           Data.Text                  (unpack)
import           Data.Text.Encoding                  (decodeUtf8)
import qualified Data.Time.Clock            as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Client        (defaultManagerSettings, newManager)
import           Network.Wai (Request, requestHeaders)
import           Network.Wai.Handler.Warp
import           Servant                    as S
import           Servant.Auth               as SA
import           Servant.Auth.Server        as SAS
import           Servant.Client
import           Servant.Common.Req as SC

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

import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Servant.Server.Experimental.Auth()

data AuthenticatedUser = AUser { auId :: UserId
                               } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

type API = ReqBody '[JSON] IssueBlueprint :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Issue]

type APIServer =
  AuthProtect "cookie-auth" :> API

genAuthAPI :: Proxy APIServer
genAuthAPI = Proxy

type instance AuthServerData (AuthProtect "cookie-auth") = AuthenticatedUser

type APIClient =
  AuthProtect "cookie-auth" :> API

-- api :: Proxy API
-- api = Proxy

type AppM = AppT Handler

-- BasicAuthCheck :: BasicAuthData -> IO (BasicAuthResult usr)
basicAuthServerContext :: Context (BasicAuthCheck AuthenticatedUser ': '[])
basicAuthServerContext = authCheck S.:. EmptyContext

authHandler :: AppEnv -> AuthHandler Request AuthenticatedUser
authHandler env =
  let
    handler :: Request -> AppM AuthenticatedUser
    handler req = do
      let
        email = lookup "user-email" (requestHeaders req)
      emailText <- maybe (throwError None) (pure) $ decodeUtf8 <$> email
      userEmail <- either throwError (pure) $ mkUserEmail emailText
      mUsr <- getUserByEmail userEmail
      usr <- maybe (throwError None) (pure . AUser . userId) mUsr
      pure usr
  in mkAuthHandler ((enter $ nt env) . handler)

-- AuthHandler r a :: r -> Handler a
-- AuthHandler Request AppEnv :: Request -> Handler AppEnv
genAuthServerContext :: AppEnv -> Context (AuthHandler Request AuthenticatedUser ': '[])
genAuthServerContext env = authHandler env S.:. EmptyContext

authCheckNew :: BasicAuthData -> IO (AuthResult AuthenticatedUser)
authCheckNew (BasicAuthData username password) = pure $
  if username == "servant" && password == "server"
  then either (const SAS.Indefinite) (Authenticated . AUser) (mkUserId 1)
  else SAS.Indefinite

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: BasicAuthCheck AuthenticatedUser
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then either (const $ pure Unauthorized) pure $ fmap (Authorized . AUser) (mkUserId 1)
        else pure Unauthorized
  in BasicAuthCheck check

server :: ServerT APIServer AppM
server (AUser userId) = postIssueHandler :<|> getIssuesHandler
  where
    postIssueHandler :: IssueBlueprint -> AppM NoContent
    postIssueHandler = fmap (const NoContent) . addIssue

    getIssuesHandler :: AppM [Issue]
    getIssuesHandler = getIssues
server _ = error1 :<|> error2
  where
    error1 :: IssueBlueprint -> AppM NoContent
    error1 _ = do
      throwError None
      pure NoContent

    error2 :: AppM [Issue]
    error2 = do
      throwError None
      pure []

nt :: AppEnv -> (AppM :~> Handler)
nt env = NT $ ((either (throwError . toServantErr) pure) =<<)
  . runExceptT
  . flip runReaderT env
  . unAppT
  where
    toServantErr :: AppError -> ServantErr
    toServantErr None = err404 { errBody = "Unknown Error"}

app :: AppEnv -> IO Application
app env = do
  myKey <- generateKey
  let -- jwtCfg = defaultJWTSettings myKey
      -- authCfg = authCheckNew
      -- cfg = jwtCfg S.:. defaultCookieSettings S.:. authCfg S.:. EmptyContext
      -- api = Proxy :: Proxy APIServer
  pure $ serveWithContext genAuthAPI (genAuthServerContext env) $ enter (nt env) server

runApp :: AppEnv -> IO ()
runApp env = do
  a <- app env
  run 8080 a

postIssueC :: IssueBlueprint -> ClientM NoContent
getIssuesC :: ClientM [Issue]
postIssueC :<|> getIssuesC = client (Proxy :: Proxy APIClient) (mkAuthenticateReq "james@example.com" authenticateReq)

data StartupError
  = ConfError ConfigError
  | DbInitErr SqlError
  deriving Show

prepareAppEnv :: ExceptT StartupError IO AppEnv
prepareAppEnv = do
  conf <- initConf
  pool <- AppDb . DbConnection <$> initPool conf

  pure (AppEnv conf pool)

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
      in do
        conn <- ExceptT . fmap (first DbInitErr) . try $ connect connectionInfo
        liftIO $ createPool (pure conn) close 1 60 10

type instance AuthClientData (AuthProtect "cookie-auth") = String

authenticateReq :: String -> Req -> Req
authenticateReq s req = SC.addHeader "user-email" s req

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

runServer :: IO ()
runServer = do
  eEnv <- runExceptT prepareAppEnv

  case eEnv of
    (Left err)  -> print err
    (Right env) -> do
      runApp env
