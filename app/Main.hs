{-# OPTIONS -Wno-unused-top-binds #-}

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

import qualified Data.Codec as Codec

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import System.FilePath

import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Iris
import qualified Options.Applicative as O
import qualified Paths_runnah as Autogen
import qualified Toml

-- | Entrypoint.

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app

app :: App ()
app = do
  Config{..} <- Iris.asksCliEnv Iris.cliEnvCmd

  result <- Toml.decodeFileEither Codec.config (projectPath </> projectFile)
  case result of
    Left  errors -> liftIO $ (T.putStrLn $ Toml.prettyTomlDecodeErrors errors)
    Right config -> liftIO $ (T.putStrLn $ Toml.encode Codec.config config)

-------------------------------------------------------------------------------
-- Data types.
-------------------------------------------------------------------------------

-- @I Rename to Options
--    type     : task
--    priority : low
--    labels   : cli, config, readability
--    notes    : To avoid confusion with Data.Config.
data Config = Config
  { projectPath :: FilePath
  , projectFile :: FilePath
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

{-
  @I Support loading option values from environment variables, where applicable
     type     : feature
     priority : low
     labels   : cli, config, environment
     notes    : If both CLI option and environment variable exist, the CLI
                option should take precedence.

  @I Support providing the name of an environment variable to load value from
     type     : feature
     priority : low
     labels   : cli, config, environment
     notes    : For example, `--project-path ${PWD}` would instruct `runnah` to
                set the value for the `project-path` option from the `PWD`
                environment variable.

  @I Support providing the name of an environment variable to load value from
     type     : feature
     priority : low
     labels   : cli, config, environment
     notes    : For example, `--project-path ${PWD}` would instruct `runnah` to
                set the value for the `project-path` option from the `PWD`
                environment variable.
-}
parser :: O.Parser Config
parser = do
    -- @I Default project path to the current directory
    --    type     : feature
    --    priority : normal
    --    labels   : cli, config
    projectPath <- O.strOption $ mconcat
        [ O.long "project-path"
        , O.metavar "PROJECT_PATH"
        , O.help "The path to the project root directory"
        ]

    projectFile <- O.strOption $ mconcat
        [ O.long "project-file"
        , O.metavar "FILE_PATH"
        , O.value "runnah.toml"
        , O.showDefault
        , O.help "The name of the project configuration file"
        ]

    -- @I Use `optparse-applicative` subparser for commands
    --    type     : feature
    --    priority : low
    --    labels   : cli
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
