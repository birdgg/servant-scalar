module Servant.Scalar.Server
  ( ScalarUI
  , ScalarUI'
  , scalarUIServer
  , scalarUIServerWith
  , scalarUIServer'
  , scalarUIServerWith'
  ) where

import Data.Kind (Type)
import Data.OpenApi (OpenApi)
import GHC.TypeLits (Symbol)
import Servant
import Servant.Scalar.Config
import Servant.Scalar.Html

-- | Convenience type that bundles an OpenAPI JSON endpoint (returning
-- generic 'Value') with a Scalar documentation page.
--
-- @
-- type API = ScalarUI \"docs\" \"openapi.json\"
--       :\<|> \"users\" :> Get '[JSON] [User]
-- @
--
-- This creates two routes:
--
-- * @GET \/openapi.json@ -- serves the OpenAPI spec as JSON 'Value'
-- * @GET \/docs@         -- serves the Scalar UI HTML page
type ScalarUI (dir :: Symbol) (schema :: Symbol) =
  ScalarUI' dir (schema :> Get '[JSON] OpenApi)

-- | Flexible variant that lets you specify the schema endpoint type.
-- Use this when you want to serve a concrete spec type such as
-- @OpenApi@ from @openapi3@ or @Swagger@ from @swagger2@.
--
-- @
-- type API = ScalarUI' \"docs\" (\"openapi.json\" :> Get '[JSON] OpenApi)
--       :\<|> \"users\" :> Get '[JSON] [User]
-- @
type ScalarUI' (dir :: Symbol) (api :: Type) =
  api :<|> dir :> Get '[HTML] (ScalarHtml dir api)

-- | Serve Scalar UI with 'defaultScalarConfig'.
--
-- @
-- server :: Server (ScalarUI \"docs\" \"openapi.json\")
-- server = scalarUIServer myOpenApiSpec
-- @
scalarUIServer ::
  OpenApi ->
  Server (ScalarUI dir schema)
scalarUIServer = scalarUIServerWith defaultScalarConfig

-- | Serve Scalar UI with a custom 'ScalarConfig'.
scalarUIServerWith ::
  ScalarConfig ->
  OpenApi ->
  Server (ScalarUI dir schema)
scalarUIServerWith config spec =
  pure spec :<|> pure (mkScalarHtml config)

-- | Serve Scalar UI with 'defaultScalarConfig'.
-- You provide the server for the schema endpoint directly.
--
-- @
-- server :: Server (ScalarUI' \"docs\" (\"openapi.json\" :> Get '[JSON] OpenApi))
-- server = scalarUIServer' (pure myOpenApiSpec)
-- @
scalarUIServer' ::
  (Monad m) =>
  ServerT api m ->
  ServerT (ScalarUI' dir api) m
scalarUIServer' = scalarUIServerWith' defaultScalarConfig

-- | Like 'scalarUIServer'' but with a custom 'ScalarConfig'.
scalarUIServerWith' ::
  (Monad m) =>
  ScalarConfig ->
  ServerT api m ->
  ServerT (ScalarUI' dir api) m
scalarUIServerWith' config apiServer =
  apiServer :<|> pure (mkScalarHtml config)
