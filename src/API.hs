{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

type Message = String

type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy

initDB :: ConnectInfo -> IO ()
initDB info = bracket (connect info) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text NOT NULL)"
  pure ()

server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages

  where
    postMessage :: Message -> Handler NoContent
    postMessage msg = do
      liftIO . withResource conns $ \conn ->
        execute conn
                "INSERT INTO messages VALUES (?)"
                (Only msg)
      pure NoContent

    getMessages :: Handler [Message]
    getMessages = fmap (map fromOnly) . liftIO $
      withResource conns $ \conn ->
        query_ conn "SELECT msg FROM messages"

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info =
  createPool (connect info)
             close
             1
             60
             10

postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

main :: IO ()
main = do
  let connectionInfo = defaultConnectInfo
                       { connectUser = "postgres"
                       , connectPassword = "abc"
                       , connectDatabase = "servant_cookbook"
                       }
  pool <- initConnectionPool connectionInfo
  initDB connectionInfo
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (ClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      postMsg "hello"
      postMsg "world"
      getMsgs
    print ms
