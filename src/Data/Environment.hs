{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types for environments.
An environment provides the context within which a project runs. A project can
have multiple environments sharing common configuration and variables, each
having its own overrides as well.
-}

module Data.Environment
  ( Environment(..)
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Text as T (Text)

-------------------------------------------------------------------------------
-- Data types for environments.
-------------------------------------------------------------------------------

-- | Data type for an environment.
-- @I Support overriding configuration and variables per environment
--    type     : feature
--    priority : low
--    labels   : config, environment
data Environment = Environment
  -- The unique identifier of the environment within the project.
  { id :: T.Text
  } deriving (Generic, Show)
  -- The human-friendly name of the environment.
  , name :: Maybe T.Text
