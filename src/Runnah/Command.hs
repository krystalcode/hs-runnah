{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types for projects.
A command is a sub-command passed as the first argument of the main runnah
entrypoint.
-}

module Runnah.Command
  ( ProjectOptions(..)
  , Command(..)
  , BuildOptions(..)
  , DownOptions(..)
  , UpOptions(..)
  , commandParser
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Text as T (Text)
import qualified Options.Applicative as O

-------------------------------------------------------------------------------
-- Data types.
-------------------------------------------------------------------------------

data ProjectOptions = ProjectOptions
  { projectPath :: FilePath
  , projectFile :: FilePath
  } deriving stock (Generic, Show)

data Command
  = Build BuildOptions
  | Down DownOptions
  | Up UpOptions
  deriving stock (Generic, Show)

data BuildOptions = BuildOptions
  { services :: Maybe T.Text
  , projectOptions :: ProjectOptions
  } deriving stock (Generic, Show)

data DownOptions = DownOptions
  { services :: Maybe T.Text
  , projectOptions :: ProjectOptions
  } deriving stock (Generic, Show)

data UpOptions = UpOptions
  { services :: Maybe T.Text
  , projectOptions :: ProjectOptions
  } deriving stock (Generic, Show)

-------------------------------------------------------------------------------
-- Public API.
-------------------------------------------------------------------------------

commandParser :: O.Parser Command
commandParser = O.subparser
  ( O.command "build" (O.info (O.helper <*> buildParser) (O.progDesc "Build one or more images required to run the project"))
 <> O.command "down" (O.info (O.helper <*> downParser) (O.progDesc "Stop a project"))
 <> O.command "up" (O.info (O.helper <*> upParser) (O.progDesc "Start a project"))
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
-}

projectOptionsParser :: O.Parser ProjectOptions
projectOptionsParser = do
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

    pure ProjectOptions{..}

buildParser :: O.Parser Command
buildParser = do
  services <- O.optional (servicesParser "The services to build; build all services if not provided.")
  projectOptions <- projectOptionsParser

  pure $ Build $ BuildOptions{..}

downParser :: O.Parser Command
downParser = do
  services <- O.optional (servicesParser "The services to stop; stop all services if not provided.")
  projectOptions <- projectOptionsParser

  pure $ Down $ DownOptions{..}

upParser :: O.Parser Command
upParser = do
  services <- O.optional (servicesParser "The services to start; start all services if not provided.")
  projectOptions <- projectOptionsParser

  pure $ Up $ UpOptions{..}

servicesParser :: String -> O.Parser T.Text
servicesParser helpText =  O.strOption $ mconcat
  [ O.long "services"
  , O.short 's'
  , O.metavar "SERVICES..."
  , O.help helpText
  ]
