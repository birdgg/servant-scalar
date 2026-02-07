{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.OpenApi hiding (Server, server)
import Data.Proxy
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.OpenApi
import Servant.Scalar (ScalarUI, scalarUIServer)

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

-- | The API of a Todo service.
type TodoAPI =
  "todo" :> Get '[JSON] [Todo]
    :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoId
    :<|> "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
    :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] TodoId

-- | Combined API of a Todo service with Scalar documentation.
type API = ScalarUI "docs" "openapi.json" :<|> TodoAPI

-- | A single Todo entry.
data Todo = Todo
  { created :: UTCTime
  , summary :: Text
  }
  deriving (Generic, Show)

-- | A unique Todo entry ID.
newtype TodoId = TodoId Int
  deriving (FromHttpApiData, Generic, Show, ToJSON)

instance ToJSON Todo
instance FromJSON Todo

instance ToSchema Todo where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is some real Todo right here"
      & mapped . schema . example ?~ toJSON (Todo (UTCTime (fromGregorian 2015 12 31) 0) "get milk")

instance ToParamSchema TodoId
instance ToSchema TodoId

-- | OpenApi spec for Todo API.
todoOpenApi :: OpenApi
todoOpenApi =
  toOpenApi todoAPI
    & info . title .~ "Todo API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API that tests servant-openapi3 integration"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

-- | Combined server of a Todo service with Swagger documentation.
server :: Server API
server = scalarUIServer todoOpenApi :<|> error "not implemented"

-- | Output generated @openapi.json@ file for the @'TodoAPI'@.
writeOpenApiJSON :: IO ()
writeOpenApiJSON = BL8.writeFile "example/openapi.json" (encodePretty todoOpenApi)

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  putStrLn "Scalar docs at http://localhost:8080/docs"
  putStrLn "OpenApi spec at http://localhost:8080/openapi.json"
  run 8080 $ serve (Proxy @API) server
