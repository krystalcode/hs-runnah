{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Main entrypoint for the `runnah` application.
-}

module Main (main) where

-- | Imports.

import Runnah (someFunc)

-- | Entrypoint.

main :: IO ()
main = someFunc
