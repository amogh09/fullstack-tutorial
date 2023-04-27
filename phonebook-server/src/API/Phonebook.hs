{-# LANGUAGE UndecidableInstances #-}

module API.Phonebook (mkApp, Env (..), Login (..), token) where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad.Catch (MonadThrow (throwM), handle)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT, runReaderT)
import DB.SQLite.Contacts
  ( addContact,
    deleteContactById,
    getAllContacts,
    getContact,
    getContactByName,
    initDB,
  )
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database.SQLite.Simple (Connection, close, open)
import GHC.Generics (Generic)
import Log
import Servant
import Servant.Auth.Server
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Log.FastLogger
import System.Random (Random (random), StdGen, getStdGen)
import Types

newtype AuthToken = AuthToken {token :: String}
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Login = Login {username :: String, password :: String}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Env = Env
  { jwtSettings :: JWTSettings,
    connPool :: Pool Connection,
    logger :: FastLogger,
    requestId :: RequestId
  }

data AppEnv = AppEnv
  { jwtSettings :: JWTSettings,
    connPool :: Pool Connection,
    stdGen :: MVar StdGen,
    logger :: FastLogger
  }

genReqId :: AppEnv -> IO RequestId
genReqId AppEnv {..} = do
  modifyMVar stdGen $ \g -> do
    let (w1, g') = random g
        (w2, g'') = random g'
    return (g'', UUID.fromWords64 w1 w2)

logReq :: (MonadReader Env m, MonadIO m) => Log.Context -> Text -> m ()
logReq ctx msg = do
  Env {..} <- ask
  let logCtx = requestIdCtx requestId
  logInfo logger (ctx <> logCtx) msg

getContactHandler ::
  (MonadReader Env m, MonadIO m, MonadThrow m) => Int -> m Contact
getContactHandler cid = do
  Env {..} <- ask
  logReq emptyCtx "getting contact"
  contact <- liftIO (withResource connPool $ flip getContact cid)
  maybe
    (logReq emptyCtx "failed to find contact")
    (flip logReq "contact found" . contactCtx)
    contact
  maybe (throwM $ err404 {errBody = "404 Not Found"}) pure contact

deleteContactHandler :: (MonadReader Env m, MonadIO m, MonadThrow m) => User -> Int -> m Contact
deleteContactHandler _ cid = do
  Env {..} <- ask
  liftIO (withResource connPool $ flip deleteContactById cid)
    >>= maybe (throwM $ err404 {errBody = "Contact does not exist"}) pure

addContactHandler :: (MonadReader Env m, MonadIO m, MonadThrow m) => User -> AddContact -> m Contact
addContactHandler user ac = do
  Env {..} <- ask
  logReq (userCtx user <> addContactCtx ac) "Adding a new contact"
  liftIO (withResource connPool $ flip getContactByName ac.name)
    >>= maybe
      (liftIO $ withResource connPool $ flip addContact ac)
      (const $ throwM $ err409 {errBody = "Contact already exists"})

phonebookHandler :: (MonadReader Env m, MonadIO m) => m [Contact]
phonebookHandler = do
  Env {..} <- ask
  liftIO $ withResource connPool getAllContacts

loginHandler :: (MonadReader Env m, MonadIO m, MonadThrow m) => Login -> m AuthToken
loginHandler Login {..} =
  if username == "kaka" && password == "nana"
    then do
      Env {..} <- ask
      let user = User "kaka"
      logReq (userCtx user) "user logged in"
      result <- liftIO $ makeJWT user jwtSettings Nothing
      case result of
        Right token -> return $ AuthToken $ BSL.unpack token
        Left e -> throwM $ err500 {errBody = BSL.pack $ show e}
    else throwM err401 {errBody = "Username or password is invalid."}

phonebookServer :: (MonadReader Env m, MonadIO m, MonadThrow m) => ServerT PhonebookAPI m
phonebookServer = loginHandler :<|> publicAPIHandler :<|> privateAPIHandler
  where
    publicAPIHandler = phonebookHandler :<|> getContactHandler
    privateAPIHandler (Authenticated user) =
      addContactHandler user :<|> deleteContactHandler user
    privateAPIHandler _ = const (throwM err401) :<|> const (throwM err401)

type PhonebookAPI =
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] AuthToken
    :<|> "api" :> "persons" :> PersonsAPI

type PersonsAPI =
  PersonsAPIPublic :<|> Auth '[JWT] User :> PersonsAPIPrivate

type PersonsAPIPublic =
  Get '[JSON] [Contact]
    :<|> Capture "id" Int :> Get '[JSON] Contact

type PersonsAPIPrivate =
  ReqBody '[JSON] AddContact :> Post '[JSON] Contact
    :<|> Capture "id" Int :> DeleteAccepted '[JSON] Contact

type instance AuthServerData (AuthProtect "cookie-auth") = User

phonebookAPI :: Proxy PhonebookAPI
phonebookAPI = Proxy

contextProxy :: Proxy '[JWTSettings, CookieSettings]
contextProxy = Proxy

type App = ReaderT Env IO

transformation :: AppEnv -> App a -> Handler a
transformation appEnv app = do
  reqId <- liftIO $ genReqId appEnv
  let env =
        Env
          { jwtSettings = appEnv.jwtSettings,
            connPool = appEnv.connPool,
            logger = appEnv.logger,
            requestId = reqId
          }
  handle handler $ liftIO $ runReaderT app env
  where
    handler :: ServerError -> Handler a
    handler = throwError

mkApp :: FilePath -> IO Application
mkApp dbPath = do
  jwtKey <- generateKey
  connPool <- newPool $ defaultPoolConfig (open dbPath) close 10 4
  (logger, _) <- newFastLogger1 $ LogStdout defaultBufSize
  stdGen <- getStdGen >>= newMVar
  let jwtCfg = defaultJWTSettings jwtKey
      ctx = jwtCfg :. defaultCookieSettings :. EmptyContext
      env = AppEnv jwtCfg connPool stdGen logger
  withResource connPool initDB
  return
    . serveWithContext phonebookAPI ctx
    . hoistServerWithContext phonebookAPI contextProxy (transformation env)
    $ phonebookServer
