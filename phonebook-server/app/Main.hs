module Main (main) where

import API.Phonebook (mkApp)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setHost,
    setPort,
  )

main :: IO ()
main = do
  let dbPath = "phonebook.db"
      settings = setPort 3001 . setHost "*" $ defaultSettings
  app <- mkApp dbPath
  runSettings settings app
