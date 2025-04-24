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

import qualified Runnah.Command as RC (Command(..), commandParser)
import qualified Runnah.Command.Build as RCB (runBuild)
import qualified Runnah.Command.Down as RCD (runDown)
import qualified Runnah.Command.Up as RCU (runUp)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)

import qualified Iris
import qualified Paths_runnah as Autogen

-- | Entrypoint.

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app

-------------------------------------------------------------------------------
-- Data types.
-------------------------------------------------------------------------------

newtype App a = App
    { unApp :: Iris.CliApp RC.Command () a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv RC.Command ())
        )

-------------------------------------------------------------------------------
-- Internal functions.
-------------------------------------------------------------------------------

app :: App ()
app = do
  command <- Iris.asksCliEnv Iris.cliEnvCmd
  case command of
    (RC.Build options) -> liftIO $ (RCB.runBuild options)
    (RC.Down options) -> liftIO $ (RCD.runDown options)
    (RC.Up options) -> liftIO $ (RCU.runUp options)

appSettings :: Iris.CliEnvSettings RC.Command ()
appSettings = Iris.defaultCliEnvSettings
    { Iris.cliEnvSettingsHeaderDesc = "Containerized development environments"
    , Iris.cliEnvSettingsProgDesc = "A tool for running containerized applications in development environments"
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
        { Iris.versionSettingsMkDesc = \v -> "Simple grep utility v" <> v
        }
    , Iris.cliEnvSettingsCmdParser = RC.commandParser
    }
