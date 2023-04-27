module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Database.SQLite.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Servant.Auth.JWT

data Contact = Contact
  { id :: Int,
    name :: String,
    number :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToRow, FromRow)

data AddContact = AddContact
  { name :: String,
    number :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToRow, FromRow)

newtype User = User String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJWT, FromJWT)

type RequestId = UUID
