let exampleTokenEndpoint =
      "https://mockbin.org/bin/9c0c15d9-ef69-4fb9-b7ef-86d4e71104e7"

let T = ./Type.dhall

in    toMap
        { `mockbin.org` =
          { clientId = "abc"
          , clientSec = T.Secret.Plain "sec"
          , serverAud = "aud"
          , tokenEndpoint = exampleTokenEndpoint
          , port = +443
          , isSecure = True
          }
        }
    : T.Configs
