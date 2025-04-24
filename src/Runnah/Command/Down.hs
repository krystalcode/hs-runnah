{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types related to the `down` command.

The `down` command stops the containers defined by a project.
-}

module Runnah.Command.Down
  ( runDown
  ) where

-- | Imports.

import qualified Runnah.Command as RC

import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)

runDown :: RC.DownOptions -> IO ()
runDown options = T.putStrLn $ T.pack "[INFO] Running the `down` command"
