let exampleTokenEndpoint =
      "https://mockbin.org/bin/9c0c15d9-ef69-4fb9-b7ef-86d4e71104e7"

in  toMap
      { `mockbin.org` =
        { clientId = "abc"
        , clientSec = "def"
        , serverAud = "aud"
        , tokenEndpoint = exampleTokenEndpoint
        , port = +443
        , isSecure = True
        }
      }
