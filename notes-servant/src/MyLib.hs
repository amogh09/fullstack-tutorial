{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MyLib (mkApp) where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (Auth, AuthResult (Authenticated), FromJWT, JWT, JWTSettings, ThrowAll (throwAll), ToJWT, cookieIsSecure, cookieXsrfSetting, defaultCookieSettings, defaultJWTSettings, generateKey, makeJWT)
import Prelude hiding (id)

data Note = Note {id :: Int, content :: Text, important :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AddNote = AddNote {content :: Text, important :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Env = Env {notesMvar :: MVar Notes, jwtSettings :: JWTSettings}

type Notes = [Note]

data LoginCreds = LoginCreds {username :: Text, password :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype User = User {name :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

data LoginResult = LoginResult {name :: Text, token :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

notes :: [Note]
notes =
  [ Note 1 "my first note" False,
    Note 2 "my second note" False
  ]

allNotesHandler :: Env -> Handler Notes
allNotesHandler Env {..} = liftIO $ readMVar notesMvar

createHandler :: Env -> AddNote -> Handler Note
createHandler Env {..} AddNote {..} = do
  liftIO $ modifyMVar notesMvar $ \ns -> do
    let maxid = if null ns then 0 else maximum (fmap (\n -> n.id) ns)
        note = Note {id = maxid + 1, content = content, important = important}
    pure (note : ns, note)

updateHandler :: Env -> Int -> Note -> Handler Note
updateHandler Env {..} nid updatedNote = do
  note <- liftIO $ modifyMVar notesMvar (pure . updateNote nid updatedNote)
  maybe (throwError err404) pure note

updateNote :: Int -> Note -> Notes -> (Notes, Maybe Note)
updateNote nid updatedNote ns =
  if any (\n -> n.id == nid) ns
    then (fmap update ns, Just updatedNote)
    else (ns, Nothing)
  where
    update note = if note.id == nid then updatedNote else note

loginHandler :: Env -> LoginCreds -> Handler LoginResult
loginHandler Env {..} LoginCreds {..} =
  if username == "kaka" && password == "nana"
    then do
      let user = User "kaka"
      result <- liftIO $ makeJWT user jwtSettings Nothing
      case result of
        Right token ->
          return
            LoginResult
              { name = user.name,
                token = decodeUtf8 token
              }
        Left _ -> throwError err401
    else throwError err401

server :: Env -> Server API
server env = loginHandler env :<|> notesServer
  where
    notesServer = notesPublicServer :<|> notesPrivateServer
    notesPublicServer = allNotesHandler env
    notesPrivateServer (Authenticated _) =
      createHandler env :<|> updateHandler env
    notesPrivateServer _ = throwAll err401

type API =
  "api" :> "login" :> ReqBody '[JSON] LoginCreds :> Post '[JSON] LoginResult
    :<|> "api" :> "notes" :> NotesAPI

type NotesAPI = NotesPublicAPI :<|> Auth '[JWT] User :> NotesPrivateAPI

type NotesPublicAPI = Get '[JSON] [Note]

type NotesPrivateAPI =
  ReqBody '[JSON] AddNote :> Post '[JSON] Note
    :<|> Capture "id" Int :> ReqBody '[JSON] Note :> Put '[JSON] Note

mkApp :: IO Application
mkApp = do
  jwtCfg <- fmap defaultJWTSettings generateKey
  notesMvar <- newMVar notes
  let env = Env {notesMvar = notesMvar, jwtSettings = jwtCfg}
      cookieCfg =
        defaultCookieSettings
          { cookieIsSecure = NotSecure,
            cookieXsrfSetting = Nothing
          }
      ctx = jwtCfg :. cookieCfg :. EmptyContext
  return $ serveWithContext (Proxy :: Proxy API) ctx (server env)
