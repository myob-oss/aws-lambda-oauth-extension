module ExtensionClient (register, nextEvent) where
import           Control.Concurrent          (MVar, putMVar)
import           Control.Monad               (when)
import           Data.Aeson                  (FromJSON, encode)
import           Data.Aeson.QQ.Simple        (aesonQQ)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text, unpack)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           GHC.Generics                (Generic)
import           Network.HTTP.Client.Conduit (RequestBody (RequestBodyLBS),
                                              responseTimeoutNone)
import           Network.HTTP.Conduit        (Request (..), responseBody)
import           Network.HTTP.Simple         (getResponseHeader, httpJSON,
                                              httpNoBody, parseRequest)
import           Text.Shakespeare.Text       (st)
-- TODO: https://github.com/aws-samples/aws-lambda-extensions/blob/main/go-example-extension/extension/client.go#L74

register :: Text -> IO ByteString
register runtimeApi = do
  req <- parseRequest $ unpack [st|POST http://#{runtimeApi}/2020-01-01/extension/register|]
  resp <- httpNoBody req
    { requestHeaders = [("Lambda-Extension-Name", "aws-lambda-oauth-extension")]
    , requestBody = RequestBodyLBS (encode [aesonQQ|{"events": ["SHUTDOWN"]}|])
    }
  pure $ head $ getResponseHeader "Lambda-Extension-Identifier" resp

data Event = Event
  { eventType      :: Text
  , shutdownReason :: Text
  } deriving Generic

instance FromJSON Event

nextEvent :: Text -> ByteString -> MVar () -> IO ()
nextEvent runtimeApi id exitSignal = do
  req <- parseRequest $ unpack [st|GET http://#{runtimeApi}/2020-01-01/extension/event/next|]
  resp <- httpJSON req
    { requestHeaders = [("Lambda-Extension-Identifier", id)]
    , responseTimeout = responseTimeoutNone
    }
  when (eventType (responseBody resp) == "SHUTDOWN") $
    putMVar exitSignal ()
