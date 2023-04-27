module DB.SQLite.Contacts
  ( getContact,
    getContactByName,
    deleteContactById,
    getAllContacts,
    addContact,
    initDB,
  )
where

import Control.Monad.RWS (MonadIO (liftIO))
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
  ( Connection,
    Only (Only),
    execute_,
    query,
    query_,
  )
import Types (AddContact, Contact)

getContact :: MonadIO m => Connection -> Int -> m (Maybe Contact)
getContact conn cid = do
  results <- liftIO $ query conn "SELECT * from contacts WHERE id=?" (Only cid)
  return $ listToMaybe results

getContactByName :: MonadIO m => Connection -> String -> m (Maybe Contact)
getContactByName conn cname = do
  results <- liftIO $ query conn "SELECT * from contacts WHERE name=?" (Only cname)
  return $ listToMaybe results

deleteContactById :: MonadIO m => Connection -> Int -> m (Maybe Contact)
deleteContactById conn cid = do
  results <- liftIO $ query conn "DELETE FROM contacts WHERE id=? RETURNING *" (Only cid)
  return $ listToMaybe results

getAllContacts :: MonadIO m => Connection -> m [Contact]
getAllContacts conn = liftIO $ query_ conn "SELECT * FROM contacts"

addContact :: MonadIO m => Connection -> AddContact -> m Contact
addContact conn ac = liftIO $ do
  results <- query conn "INSERT INTO contacts (name, number) VALUES (?, ?) RETURNING id, name, number" ac
  return $ head results

initDB :: MonadIO m => Connection -> m ()
initDB conn = liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS contacts (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE, number TEXT)"
