{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Scalar.Html
  ( ScalarHtml (..)
  , HTML
  , mkScalarHtml
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import Network.URI (uriPath)
import Servant.HTML.Blaze (HTML)
import Servant.Links (HasLink, IsElem, Link, MkLink, linkURI, safeLink)
import Servant.Scalar.Config
import Text.Blaze (ToMarkup (..), preEscapedText)

-- | HTML wrapper that renders a Scalar API reference page.
--
-- The @dir@ type parameter identifies the UI route.
-- The @api@ type parameter is the schema endpoint type; its link is
-- resolved via 'safeLink' so the generated page always points at
-- the correct spec URL.
data ScalarHtml (dir :: Symbol) (api :: Type) = ScalarHtml Text

instance (HasLink api, Link ~ MkLink api Link, IsElem api api)
    => ToMarkup (ScalarHtml dir api)
  where
    toMarkup (ScalarHtml template) = preEscapedText
        $ T.replace "SERVANT_SCALAR_SCHEMA"
            (T.pack $ uriPath $ linkURI $ safeLink (Proxy :: Proxy api) (Proxy :: Proxy api))
        $ template

-- | Build the full Scalar HTML page as a template with a
-- @SERVANT_SCALAR_SCHEMA@ placeholder that gets replaced at
-- render time via the 'ToMarkup' instance above.
scalarHtmlTemplate :: ScalarConfig -> Text
scalarHtmlTemplate config = T.concat
  [ "<!doctype html><html><head>"
  , "<title>API Reference</title>"
  , "<meta charset=\"utf-8\">"
  , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<style>body{margin:0;}</style>"
  , "</head><body>"
  , "<div id=\"app\"></div>"
  , "<script src=\"" <> scalarCdnUrl config <> "\"></script>"
  , "<script>"
  , "Scalar.createApiReference('#app',{"
  , "url:'" <> escapeJsString "SERVANT_SCALAR_SCHEMA" <> "',"
  , "theme:'" <> themeToText (scalarTheme config) <> "',"
  , "darkMode:" <> boolJs (scalarDarkMode config) <> ","
  , "layout:'" <> layoutToText (scalarLayout config) <> "',"
  , "showSidebar:" <> boolJs (scalarShowSidebar config)
  , "});"
  , "</script>"
  , "</body></html>"
  ]
 where
  boolJs True  = "true"
  boolJs False = "false"

-- | Escape a text value for safe inclusion inside a JS single-quoted string.
escapeJsString :: Text -> Text
escapeJsString =
  T.replace "\\" "\\\\"
    . T.replace "'" "\\'"
    . T.replace "</" "<\\/"
    . T.replace "\n" "\\n"
    . T.replace "\r" "\\r"

-- | Smart constructor: build a 'ScalarHtml' from a 'ScalarConfig'.
mkScalarHtml :: ScalarConfig -> ScalarHtml dir api
mkScalarHtml = ScalarHtml . scalarHtmlTemplate
