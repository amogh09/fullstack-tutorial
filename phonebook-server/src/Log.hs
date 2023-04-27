module Log
  ( logInfo,
    userCtx,
    contactCtx,
    requestIdCtx,
    addContactCtx,
    emptyCtx,
    Context,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text
import Data.Time
import GHC.Generics
import System.Log.FastLogger
import Types

data LogLevel = LogInfo deriving stock (Eq)

data Log ctx = Log
  { timestamp :: UTCTime,
    message :: Text,
    level :: LogLevel,
    context :: ctx
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

type Context = Object

instance Show LogLevel where
  show LogInfo = "INFO"

instance ToJSON LogLevel where
  toJSON = toJSON . show

mkLog :: MonadIO m => LogLevel -> ctx -> Text -> m (Log ctx)
mkLog lvl ctx msg = do
  t <- liftIO getCurrentTime
  return
    Log
      { timestamp = t,
        message = msg,
        level = lvl,
        context = ctx
      }

logIO :: (MonadIO m, ToJSON ctx) => FastLogger -> Log ctx -> m ()
logIO logger = liftIO . logger . (<> "\n") . toLogStr . encode

logInfo :: (MonadIO m, ToJSON ctx) => FastLogger -> ctx -> Text -> m ()
logInfo logger ctx msg = logIO logger =<< mkLog LogInfo ctx msg

userCtx :: User -> Context
userCtx = KM.singleton "user" . toJSON

contactCtx :: Contact -> Context
contactCtx = KM.singleton "contact" . toJSON

addContactCtx :: AddContact -> Context
addContactCtx = KM.singleton "addContact" . toJSON

requestIdCtx :: RequestId -> Context
requestIdCtx = KM.singleton "requestId" . toJSON

emptyCtx :: Context
emptyCtx = KM.empty
