-- | Scalar API documentation UI for Servant.
--
-- Serves a <https://github.com/scalar/scalar Scalar> API reference page
-- alongside your Servant API, loading the UI from a CDN.
--
-- Minimal example:
--
-- @
-- type API = ScalarUI \"docs\" \"openapi.json\"
--       :\<|> \"users\" :> Get '[JSON] [User]
--
-- server :: Server API
-- server = scalarUIServer myOpenApiSpec :\<|> getUsers
-- @
--
-- Using a custom schema endpoint type:
--
-- @
-- type API = ScalarUI' \"docs\" (\"openapi.json\" :> Get '[JSON] OpenApi)
--       :\<|> \"users\" :> Get '[JSON] [User]
--
-- server :: Server API
-- server = scalarUIServer' (pure myOpenApiSpec) :\<|> getUsers
-- @
module Servant.Scalar
  ( -- * API types
    ScalarUI
  , ScalarUI'

    -- * Content type
  , HTML

    -- * Server
  , scalarUIServer
  , scalarUIServerWith
  , scalarUIServer'
  , scalarUIServerWith'

    -- * Configuration
  , ScalarConfig (..)
  , defaultScalarConfig
  , ScalarTheme (..)
  , ScalarLayout (..)

    -- * Re-exports
  , OpenApi
  ) where

import Data.OpenApi (OpenApi)
import Servant.Scalar.Config
import Servant.Scalar.Html (HTML)
import Servant.Scalar.Server
