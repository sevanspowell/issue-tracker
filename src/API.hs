{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Control.Concurrent
import           Control.Exception                (bracket, try)
import           Control.Monad                    (join)
import           Control.Monad.IO.Class
import           Control.Monad.Reader             (ask)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT, except)
import           Control.Monad.Trans.Reader       (runReaderT)
import           Data.Aeson
import           Data.Bifunctor                   (first)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Pool
import           Data.Text                        (unpack)
import           Data.Text.Encoding               (decodeUtf8)
import qualified Data.Time.Clock                  as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.Wai                      (Request, requestHeaders)
import           Network.Wai.Handler.Warp
import           Servant                          as S
import           Servant.Auth                     as SA
import           Servant.Auth.Server              as SAS
import           Servant.Client
import           Servant.Common.Req               as SC

import           Conf
import           Conf.Types
import qualified DB.IssueTrackerDb                as DB
import           Types                            (AppDb(..), AppEnv (..),
                                                   AppError (..), AppT (..),
                                                   DbConnection(..), Issue,
                                                   IssueBlueprint (..), UserId,
                                                    destroyEnv, mkUserEmail, mkUserPassword,
                                                   userId, AuthenticatedUser(..))

import           Layer1
import           Layer2

import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Servant.Server.Experimental.Auth ()

type API = ReqBody '[JSON] IssueBlueprint :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Issue]

type APIServer =
  Auth '[SA.BasicAuth, SA.JWT] AuthenticatedUser :> API

type APIClient =
  S.BasicAuth "test" AuthenticatedUser :> API

type instance AuthServerData (AuthProtect "cookie-auth") = AuthenticatedUser

type AppM = AppT Handler

basicAuthCheck :: AppEnv -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
basicAuthCheck env =
  let
    check :: BasicAuthData -> AppM (AuthResult AuthenticatedUser)
    check (BasicAuthData username password) = do
      emailText <- pure . decodeUtf8 $ username
      userEmail <- either throwError (pure) $ mkUserEmail emailText

      passwordText <- pure . decodeUtf8 $ password
      userPassword <- either throwError (pure) $ mkUserPassword passwordText

      mUsr <- authenticateUser userEmail userPassword
      maybe (throwError $ NoUser) (pure . Authenticated . AUser . userId) mUsr

    toAuthResponse :: ServantErr -> AuthResult AuthenticatedUser
    toAuthResponse ServantErr { errHTTPCode = _, errReasonPhrase = _, errBody = _, errHeaders = _} = SAS.Indefinite

  in (fmap (either (toAuthResponse) id) . runHandler . (enter $ nt env) . check)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

server :: ServerT APIServer AppM
server (Authenticated user) = postIssueHandler :<|> getIssuesHandler
  where
    postIssueHandler :: IssueBlueprint -> AppM NoContent
    postIssueHandler = fmap (const NoContent) . addIssue (auId user)

    getIssuesHandler :: AppM [Issue]
    getIssuesHandler = getIssues
server err = error1 :<|> error2
  where
    error1 :: IssueBlueprint -> AppM NoContent
    error1 _ = throwError $ AuthenticationError err

    error2 :: AppM [Issue]
    error2 = throwError $ AuthenticationError err

nt :: AppEnv -> (AppM :~> Handler)
nt env = NT $ ((either (throwError . toServantErr) pure) =<<)
  . runExceptT
  . flip runReaderT env
  . unAppT
  where
    toServantErr :: AppError -> ServantErr
    toServantErr err@(NoUser) = err404 { errBody = pack $ show err }
    toServantErr err@(DbError _)         = err500 { errBody = pack $ show err }
    toServantErr err@(AuthenticationError _) = err403 { errBody = pack $ show err }

mkApp :: AppEnv -> IO Application
mkApp env = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = basicAuthCheck env
      cfg = jwtCfg S.:. defaultCookieSettings S.:. authCfg S.:. EmptyContext
      api = Proxy :: Proxy APIServer
  pure $ serveWithContext api cfg $ enter (nt env) server

runApp :: AppEnv -> IO ()
runApp env = do
  app <- mkApp env
  run port app
    where port = (fromIntegral . Conf.Types.getPort . networkPort . networkConf . appConf) env

postIssueC :: IssueBlueprint -> ClientM NoContent
getIssuesC :: ClientM [Issue]
postIssueC :<|> getIssuesC = client (Proxy :: Proxy APIClient) (BasicAuthData "james@example.com" "b4cc344d25a2efe540adbf2678e2304c")

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

main :: IO ()
main = do
  eEnv <- runExceptT prepareAppEnv

  case eEnv of
    (Left err) -> print err
    (Right env) -> do
      mgr <- newManager defaultManagerSettings
      bracket (forkIO $ runApp env) killThread $ \_ -> do
        ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 3008 "")) $ do
          postIssueC (IssueBlueprint "Testing blueprint")
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

test :: IO ()
test = do
  eEnv <- runExceptT prepareAppEnv

  case eEnv of
    (Left err)  -> print err
    (Right env) -> do
      eUnit <- runExceptT . flip runReaderT env . unAppT $ do
        email <- either throwError pure $ mkUserEmail "null@example.com"
        pw <- either throwError pure $ mkUserPassword "pw"
        mUser <- authenticateUser email pw
        liftIO $ case mUser of
          Nothing  -> print "No user"
          (Just u) -> print u

      either print pure eUnit
