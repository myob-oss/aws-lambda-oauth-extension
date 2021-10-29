let Map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v21.0.0/Prelude/Map/Type.dhall sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let Secret = < Plain : Text | KmsEncrypted : Text >

let Config =
      { clientId : Text
      , clientSec : Secret
      , serverAud : Text
      , tokenEndpoint : Text
      , port : Integer
      , isSecure : Bool
      }

let Configs = Map Text Config

in  { Secret, Config, Configs }
