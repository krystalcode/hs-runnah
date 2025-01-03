{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types for services.
A service represents a specific program that runs in a container. A project can
have multiple services.
-}

module Data.Service
  ( Service(..)
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Text as T (Text)

-------------------------------------------------------------------------------
-- Data types for services.
-------------------------------------------------------------------------------

-- | Data type for a service.
-- @I Support overriding configuration and variables per service
--    type     : feature
--    priority : low
--    labels   : config, service
data Service = Service
  -- The unique identifier of the service within the project.
  { id :: T.Text
  } deriving (Generic, Show)
  -- The human-friendly name of the service.
  , name :: Maybe T.Text
