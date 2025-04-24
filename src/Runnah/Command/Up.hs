{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types related to the `up` command.

The `up` command runs the containers defined by a project.
-}

module Runnah.Command.Up
  ( runUp
  ) where

-- | Imports.

import qualified Runnah.Command as RC

import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)

-------------------------------------------------------------------------------
-- Public API.
-------------------------------------------------------------------------------

runUp :: RC.UpOptions -> IO ()
runUp options = T.putStrLn $ T.pack "[INFO] Running the `up` command"
