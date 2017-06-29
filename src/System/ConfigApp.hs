-- | Skeleton for application that reads a config file with --conf.
module System.ConfigApp
  ( AppDesc(..)
  , ConfigPath(..)
  , readConfigBy
  , configApp
  , configAppWith
  -- * Config deriving
  , deriveFromJSON
  , defaultOptions
  ) where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)

import GHC.Generics

import System.AesonOptions

-- | CLI options
optionsParser :: Parser (Maybe FilePath)
optionsParser =
  (optional . strOption) (
       long "conf"
    <> metavar "CONFIG"
    <> help "Configuration file"
  )

-- | Application description.
data AppDesc = AppDesc {
    appName :: String     -- ^ App name
  , appDesc :: String     -- ^ Long description for CLI help
  } deriving (Show, Eq)

-- | Runs CLI-application that reads config's from YAML file with interface
--
-- > app-name --conf config.yaml
--
-- or if file is missing tries to find the config file at the path:
--
-- > $(HOME)/.{app-name}/$({APP_NAME}_ENV)/config.yaml
configApp :: (MonadIO m, Data config, FromJSON config) => AppDesc -> (config -> m ()) -> m ()
configApp desc runConfig = configAppWith (pure ()) desc (const runConfig)

-- | Extends simple config app with constom options.
-- The aux arguments are passed in the first argument as parsed with optparse-applicative.
configAppWith :: (MonadIO m, Data config, FromJSON config) => Parser args -> AppDesc -> (args -> config -> m ()) -> m ()
configAppWith argsParser AppDesc{..} runConfig = (liftIO $ execParser opts) >>= runOptions
  where
    runOptions (args, configFile) = runConfig args =<< readConfigBy appName configFile

    fullParser = liftA2 (,) argsParser optionsParser

    opts = info (helper <*> fullParser)
        ( fullDesc
       <> progDesc ("CLI interface for " ++ appName)
       <> header (appName ++ " - " ++ appDesc))

-- | Read config from given path or from $(HOME)/.{app-name}/$({APP_NAME}_ENV)/config.yaml
readConfigBy :: (FromJSON a, Data a, MonadIO m) => String -> Maybe FilePath -> m a
readConfigBy appName mCfgPath = do
    path <- getAppConfigFile appName mCfgPath
    cfg <- liftIO $ loadYamlSettings [path] [] useEnv
    absolutize (takeDirectory path) cfg

-- | Returns normalized given config path or $(HOME)/.{app-name}/$({APP_NAME}_ENV)/config.yaml
getAppConfigFile :: MonadIO m => String -> Maybe FilePath -> m FilePath
getAppConfigFile appname mCfgPath =
    case mCfgPath of
        Just path' -> checkConfigFileExists =<< liftIO (do
            home <- getUserDocumentsDirectory
            canonicalizePath $ joinPath
                             $ case splitPath path' of
                                    ("~/":ps) -> home:ps
                                    ps'       -> ps')
        Nothing -> getEnvConfigFile appname


-- | Define path to a file relative to the config
newtype ConfigPath = ConfigPath { unConfigPath :: FilePath }
  deriving (Eq, Show, Generic, Data, FromJSON, ToJSON)

instance IsString ConfigPath where
  fromString = ConfigPath

-- | Transform all 'SitePath' to absolute path with given prefix
absolutize :: (Uniplate a, Data a, MonadIO m) => FilePath -> a -> m a
absolutize prefix a =  liftIO $ do
  p <- canonicalizePath prefix
  transformBiM (mkAbs p) a
  where
    mkAbs :: FilePath -> ConfigPath -> IO ConfigPath
    mkAbs pref (ConfigPath p) = pure . ConfigPath $ if isAbsolute p then p else pref </> p

getEnvConfigFile :: MonadIO m => String -> m FilePath
getEnvConfigFile appname = do
    env <- getEnvName appname
    liftIO $ do
        home <- getHomeDirectory
        checkConfigFileExists $ home </> "." <> appname </> env </> "config.yaml"

checkConfigFileExists :: MonadIO m => FilePath -> m FilePath
checkConfigFileExists path = liftIO $ do
    exists <- doesFileExist path
    if exists then return path
      else putStrLn [qc|Config file not found: {path}|] >> exitFailure

getEnvName :: MonadIO m => String -> m String
getEnvName appname = liftIO $ do
    let envVarName = fmap ((replaceChar '-' '_') . toUpper) appname <> "_ENV"
    -- putStrLn [qc|envVarName: {envVarName}|]
    envName <- (fromMaybe (fmap toLower "PROD") . lookup envVarName) <$> getEnvironment
    -- putStrLn [qc|envName: {envName}|]
    return envName
  where replaceChar f t c = if c == f then t else c
