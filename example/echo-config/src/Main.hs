{-# Language
          TemplateHaskell
        , DeriveDataTypeable
        , DeriveGeneric #-}
module Main where

import Data.Data
import GHC.Generics

import System.ConfigApp

data Config = Config {
      appFieldA :: !String
    , appFieldB :: !Int
  } deriving (Show,Eq,Generic,Data)

$(deriveFromJSON defaultOptions ''Config)

main = configApp desc runConfig
  where
    desc = AppDesc {
                appName = "echo-config"
              , appDesc = "An app to echo parsed YAML-files"
            }

runConfig :: Config -> IO ()
runConfig = print