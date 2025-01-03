{-# LANGUAGE DeriveGeneric, DerivingStrategies, OverloadedStrings #-}

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
  -- A project should always have a directory. We have made optional since it is
  -- optional in the configuration file and it would make it more complicate to
  -- pass the path from the CLI options to the Toml codec before constructing
  -- the project. Also, it may be optional anyways in the long run when we
  -- support remote project using, for example, the Podman API. We therefore
  -- keep it as optional and we should be merging in the path provided by the
  -- CLI options.
  --
  -- @I Merge CLI `project-path` option into the path field
  --    type     : improvement
  --    priority : low
  --    labels   : config, project
  , path :: Maybe FilePath
  } deriving stock (Generic, Show)
