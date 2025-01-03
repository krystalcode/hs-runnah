{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2024 Dimitris Bozelos
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Dimitris Bozelos <dbozelos@gmail.com>

Functions for Toml coding/decoding.

@I Review folder structure so that it can be extended to support other formats
   type     : task
   priority : low
   labels   : structure
   notes    : Maybe the following:
              - Runnah/Codec/Toml
              - Runnah/Codec/Yaml
-}

module Data.Codec
  ( environment
  , config
  , project
  , service
  ) where

-- | Imports.

import Toml (TomlCodec, (.=))
import Toml.Codec.Combinator.Custom (textBy)
import Toml.Type.Key (Key)

import qualified Data.Config as RDC
import qualified Data.Environment as RDE
import qualified Data.Project as RDP
import qualified Data.Service as RDS
import qualified Data.Text as T (pack)
import qualified Toml

-------------------------------------------------------------------------------
-- Functions for Toml coding/decoding.
-------------------------------------------------------------------------------

-- | Toml codec for the global configuration.
config :: TomlCodec RDC.Config
config = RDC.Config
  <$> Toml.table project "project" .= RDC.project
  <*> Toml.table environment "environment" .= RDC.environment
  <*> Toml.list service "service" .= RDC.services

-- | Toml codec for a project.
project :: TomlCodec RDP.Project
project = RDP.Project
  <$> Toml.text "id" .= RDP.id
  <*> Toml.dioptional (Toml.text "name") .= RDP.name
  <*> Toml.dioptional (filePath "path") .= RDP.path

-- | Toml codec for an environment.
environment :: TomlCodec RDE.Environment
environment = RDE.Environment
  <$> Toml.text "id" .= RDE.id
  <*> Toml.dioptional (Toml.text "name") .= RDE.name

-- | Toml codec for a service.
service :: TomlCodec RDS.Service
service = RDS.Service
  <$> Toml.text "id" .= RDS.id
  <*> Toml.dioptional (Toml.text "name") .= RDS.name

-- | Toml codec for a file path.
filePath :: Key -> TomlCodec FilePath
filePath = textBy T.pack (Right . show)
