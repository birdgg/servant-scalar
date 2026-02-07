module Servant.Scalar.Config
  ( ScalarConfig (..)
  , defaultScalarConfig
  , ScalarTheme (..)
  , ScalarLayout (..)
  , themeToText
  , layoutToText
  ) where

import Data.Text (Text)

-- | Scalar UI theme options.
--
-- See: https://github.com/scalar/scalar#themes
data ScalarTheme
  = ThemeDefault
  | ThemeAlternate
  | ThemeMoon
  | ThemePurple
  | ThemeSolarized
  | ThemeBluePlanet
  | ThemeSaturn
  | ThemeKepler
  | ThemeMars
  | ThemeDeepSpace
  | ThemeNone
  deriving (Show, Eq, Enum, Bounded)

-- | Layout mode for Scalar UI.
data ScalarLayout
  = LayoutModern
  | LayoutClassic
  deriving (Show, Eq)

-- | Configuration for the Scalar API reference UI.
data ScalarConfig = ScalarConfig
  { scalarTheme :: !ScalarTheme
  , scalarDarkMode :: !Bool
  , scalarLayout :: !ScalarLayout
  , scalarShowSidebar :: !Bool
  , scalarCdnUrl :: !Text
  }
  deriving (Show, Eq)

-- | Sensible defaults: default theme, light mode, modern layout, sidebar visible.
defaultScalarConfig :: ScalarConfig
defaultScalarConfig =
  ScalarConfig
    { scalarTheme = ThemeDefault
    , scalarDarkMode = False
    , scalarLayout = LayoutModern
    , scalarShowSidebar = True
    , scalarCdnUrl = "https://cdn.jsdelivr.net/npm/@scalar/api-reference"
    }

themeToText :: ScalarTheme -> Text
themeToText = \case
  ThemeDefault -> "default"
  ThemeAlternate -> "alternate"
  ThemeMoon -> "moon"
  ThemePurple -> "purple"
  ThemeSolarized -> "solarized"
  ThemeBluePlanet -> "bluePlanet"
  ThemeSaturn -> "saturn"
  ThemeKepler -> "kepler"
  ThemeMars -> "mars"
  ThemeDeepSpace -> "deepSpace"
  ThemeNone -> "none"

layoutToText :: ScalarLayout -> Text
layoutToText = \case
  LayoutModern -> "modern"
  LayoutClassic -> "classic"
