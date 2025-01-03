{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions and types for projects.
A project represents a collection of services that run in a pod.
-}

module Data.Project
  ( Project(..)
  ) where

-- | Imports.

import GHC.Generics (Generic)

import qualified Data.Text as T (Text)

-------------------------------------------------------------------------------
-- Data types for projects.
-------------------------------------------------------------------------------

-- | Data type for a project.
-- @I Define project type data type
--    type     : feature
--    priority : high
--    labels   : project
--    notes    : Expected project types are `application` and `meta`. Might just
--               need to be a text property?
-- @I Define configuration data type
--    type     : feature
--    priority : high
--    labels   : config, project
-- @I Define group data type
--    type     : feature
--    priority : low
--    labels   : group, project
-- @I Determine the relationship between the project and the environment
--    type     : feature
--    priority : high
--    labels   : environment, project
--    notes    : Should we have a separate `RuntimeContext` type that will
--               reference both the project and the environment?
data Project = Project
  -- The unique identifier of the project within the host/user.
  { id :: T.Text
  -- The human-friendly name of the project.
  , name :: Maybe T.Text
  -- The filesystem path of the directory where the project is located.
  , path  :: FilePath
  } deriving (Generic, Show)
