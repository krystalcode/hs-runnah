{-# LANGUAGE DeriveGeneric, DerivingStrategies #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types for configuration.
The configuration in this context holds the entire project configuration as
defined by mergin the project configuration file, options provided by CLI, and
environment variables. It include project properties and also all other
properties and settings, for example, for the environment, the services etc.
-}

module Data.Config
  ( Config(..)
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Environment as RDE
import qualified Data.Project as RDP
import qualified Data.Service as RDS

-------------------------------------------------------------------------------
-- Data types for configuration.
-------------------------------------------------------------------------------

-- | Data type for the entire project configuration.
data Config = Config
  { project :: RDP.Project
  , environment :: RDE.Environment
  , services :: [RDS.Service]
  } deriving stock (Generic, Show)
