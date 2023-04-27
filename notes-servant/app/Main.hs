{-# LANGUAGE OverloadedStrings #-}

module Main where

import MyLib (mkApp)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

main :: IO ()
main = do
  app <- mkApp
  let settings = setPort 3001 . setHost "*" $ defaultSettings
  runSettings settings app
