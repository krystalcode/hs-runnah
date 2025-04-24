{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types related to the `build` command.

The `build` command prepares the images required for running the containers
defined by a project.
-}

module Runnah.Command.Build
  ( runBuild
  ) where

-- | Imports.

import qualified Data.Codec as Codec
import qualified Runnah.Command as RC ( BuildOptions(..)
                                      , ProjectOptions(..)
                                      )

import System.FilePath

import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Toml

runBuild :: RC.BuildOptions -> IO ()
runBuild options = do
  T.putStrLn $ T.pack "[INFO] Running the `build` command"

  result <- Toml.decodeFileEither Codec.config (RC.projectPath (RC.projectOptions options) </> (RC.projectFile (RC.projectOptions options)))

  case result of
    Left  errors -> T.putStrLn $ Toml.prettyTomlDecodeErrors errors
    Right config -> T.putStrLn $ Toml.encode Codec.config config
