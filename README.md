# servant-scalar

[Scalar](https://scalar.com) API documentation UI for [Servant](https://www.servant.dev/).

Serves a Scalar API reference page alongside your Servant API. The UI is loaded from a CDN, so the library adds no embedded static files to your binary.

## Usage

Add `servant-scalar` to your `build-depends`, then wire it into your API type:

```haskell
import Servant
import Servant.Scalar (ScalarUI, scalarUIServer)

type MyAPI =
  "users" :> Get '[JSON] [User]

type API = ScalarUI "docs" "openapi.json" :<|> MyAPI

server :: Server API
server = scalarUIServer myOpenApiSpec :<|> usersHandler
```

This creates two additional routes:

- `GET /openapi.json` -- serves the OpenAPI spec as JSON
- `GET /docs` -- serves the Scalar UI HTML page

### Custom configuration

```haskell
import Servant.Scalar

server :: Server API
server = scalarUIServerWith config myOpenApiSpec :<|> usersHandler
  where
    config = defaultScalarConfig
      { scalarTheme = ThemeMoon
      , scalarDarkMode = True
      , scalarLayout = LayoutClassic
      }
```

Available themes: `ThemeDefault`, `ThemeAlternate`, `ThemeMoon`, `ThemePurple`, `ThemeSolarized`, `ThemeBluePlanet`, `ThemeSaturn`, `ThemeKepler`, `ThemeMars`, `ThemeDeepSpace`, `ThemeNone`.

### Custom schema endpoint

Use `ScalarUI'` and `scalarUIServer'` when you need full control over the schema endpoint type:

```haskell
type API = ScalarUI' "docs" ("openapi.json" :> Get '[JSON] OpenApi)
      :<|> MyAPI

server :: Server API
server = scalarUIServer' (pure myOpenApiSpec) :<|> usersHandler
```

## Example

See [example/Main.hs](example/Main.hs) for a complete working example with a Todo API.

```bash
cabal run servant-scalar-example
# Scalar docs at http://localhost:8080/docs
```

## License

MIT
