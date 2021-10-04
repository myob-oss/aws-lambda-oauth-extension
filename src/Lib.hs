module Lib where
import           App
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Reader.Class  (asks)
import           Data.Aeson                  (FromJSON, decode, eitherDecode)
import qualified Data.ByteString             as BS
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.HashMap                as HM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text, unpack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           GHC.Generics                (Generic)
import qualified Network.HTTP.Client.Conduit as Client
import qualified Network.HTTP.Simple         as Client
import           Network.HTTP.Types          (hAuthorization, status200,
                                              status404)
import           Network.Wai                 (Request (..), Response,
                                              ResponseReceived, defaultRequest,
                                              lazyRequestBody, responseLBS)
import           Text.Shakespeare.Text       (st)

-- https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata
data TokenResp = TokenResp
  { access_token :: Text  } deriving (Generic, Show)

instance FromJSON TokenResp

proxy :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
proxy req respond = do
  configs <- asks eConfig
  let host = requestHeaderHost req
  case ( host, host >>= flip Map.lookup configs . decodeUtf8) of
    (Just host, Just config) -> do
      hcReq <- liftIO $ toHttpClientReq config host req
      token <- liftIO $ exchangeToken config
      case token of
        Left resp -> liftIO $ respond $ toWaiResp resp
        Right t -> do
          resp <- Client.httpLBS (Client.setRequestHeader hAuthorization [encodeUtf8 t] hcReq)
          liftIO $ respond $ toWaiResp resp
    _ -> liftIO $ respond $ responseLBS status404  [] ""
  where
    toWaiResp resp = responseLBS (Client.responseStatus resp)
          (Client.responseHeaders resp)
          (Client.responseBody resp)

toHttpClientReq :: Config -> BS.ByteString -> Request -> IO Client.Request
toHttpClientReq  Config{App.isSecure,port} host req = do
  body <- lazyRequestBody req
  pure $ Client.defaultRequest
    { Client.method = requestMethod req
    , Client.host = host
    , Client.secure = isSecure
    , Client.path  = rawPathInfo req
    , Client.queryString = rawQueryString req
    , Client.port = port
    , Client.requestHeaders = requestHeaders req
    , Client.requestBody = Client.RequestBodyLBS body
    }

exchangeToken :: Config -> IO (Either (Client.Response ByteString) Text)
exchangeToken Config{tokenEndpoint, clientId , clientSec, serverAud } = do
  req <- Client.parseRequest $ unpack [st|POST #{tokenEndpoint}|]
  resp <- Client.httpLBS
    ( Client.setRequestBodyURLEncoded
      [("grant_type", "client_credentials")
      ,("client_id", encodeUtf8 clientId)
      ,("client_secret", encodeUtf8 clientSec)
      ,("audience", encodeUtf8 serverAud)
      ] req
    )
  pure $ if Client.responseStatus resp == status200 then
   case eitherDecode (Client.responseBody resp) of
     Right TokenResp{access_token} -> Right access_token
     Left e                        -> Left resp
  else Left resp
