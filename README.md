# config-app

Creates CLI-apps with config files in yaml format.
The app has only one field `conf`:

~~~
> app-name --conf config.yaml
~~~

It reads the data from the YAML-file. If the file is missing it tires
to find it at the directory `$(HOME)/.{app-name}/$({APP_NAME}_ENV)/config.yaml`

An example:

~~~haskell
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
~~~

To run:

~~~
> echo-config --conf config.yaml
~~~

Config file contains fields with stripped prefix:

~~~yaml
fieldA:  'first'
fieldB:  2
~~~

## Using custom CLI-arguments

The function `configApp` uses only one argument `--conf` if it's not enough
we can use more generic function. To use custom arguments there is a function:

~~~haskell
configAppWith :: (MonadIO m, Data config, FromJSON config)
  => Parser args
  -> AppDesc -> (args -> config -> m ()) -> m ()
~~~

It's almost the same as `configApp` but it also takes in an Parser for the rest arguments.
And the callback `runConfig` now takes parsed result as the firt argument.
