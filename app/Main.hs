{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Main entrypoint for the `runnah` application.
-}

module Main (main) where

-- | Imports.

import Runnah (someFunc)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (putStrLn, readFile)
import qualified Iris
import qualified Options.Applicative as O
import qualified Paths_runnah as Autogen

-- | Entrypoint.

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app

app :: App ()
app = do
    Config{..} <- Iris.asksCliEnv Iris.cliEnvCmd

    fileContent <- getFileContent configFile

    output fileContent

-------------------------------------------------------------------------------
-- Data types.
-------------------------------------------------------------------------------

data Config = Config
  { configFile :: FilePath
  , command :: T.Text
  }

newtype App a = App
    { unApp :: Iris.CliApp Config () a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv Config ())
        )

-------------------------------------------------------------------------------
-- Internal functions.
-------------------------------------------------------------------------------

getFileContent :: FilePath -> App T.Text
getFileContent = liftIO . T.readFile

output :: T.Text -> App ()
output fileContent = liftIO $ T.putStrLn fileContent

parser :: O.Parser Config
parser = do
    configFile <- O.strOption $ mconcat
        [ O.long "file"
        , O.short 'f'
        , O.metavar "FILE_PATH"
        , O.help "Path to the project configuration file"
        ]

    command <- O.strOption $ mconcat
        [ O.long "command"
        , O.short 'c'
        , O.metavar "COMMAND"
        , O.help "The command to execute"
        ]

    pure Config{..}

appSettings :: Iris.CliEnvSettings Config ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Containerized development environments"
    , Iris.cliEnvSettingsProgDesc = "A tool for running containerized applications in development environments"
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
        { Iris.versionSettingsMkDesc = \v -> "Simple grep utility v" <> v
        }
    , Iris.cliEnvSettingsCmdParser = parser
    }
