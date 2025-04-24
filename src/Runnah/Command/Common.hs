{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- | 

module Runnah.Command.Common
  ( ProjectOptions(..)
  , projectOptionsParser
  , servicesParser
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Text as T (Text)
import qualified Options.Applicative as O

data ProjectOptions = ProjectOptions
  { projectPath :: FilePath
  , projectFile :: FilePath
  } deriving stock (Generic, Show)

-- General options.
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

-- Command specific options.
servicesParser :: String -> O.Parser T.Text
servicesParser helpText =  O.strOption $ mconcat
  [ O.long "services"
  , O.short 's'
  , O.metavar "SERVICES..."
  , O.help helpText
  ]
