module ExtensionClient where
import           App
import           Data.Text (Text)
-- TODO: https://github.com/aws-samples/aws-lambda-extensions/blob/main/go-example-extension/extension/client.go#L74
register :: Env -> IO Text
register env = pure "extension id"
