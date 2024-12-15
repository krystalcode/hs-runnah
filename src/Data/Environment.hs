{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | Functions and types for environments.
-- An environment provides the context within which a project runs. A project
-- can have multiple environments sharing common configuration and variables,
-- each having its own overrides as well.

module Data.Environment
  ( Environment(..)
  ) where

-- | Imports.

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
  -- The human-friendly label of the environment.
  , label :: T.Text
  } deriving (Generic, Show)
