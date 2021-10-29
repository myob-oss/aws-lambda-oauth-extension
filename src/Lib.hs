module Lib where

import           App
import           Aws
import           Control.Lens
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Reader.Class  (MonadReader (ask), asks)
import           Data.Aeson                  (FromJSON, decode, eitherDecode)
import qualified Data.ByteString             as BS
import           Data.ByteString.Base64      (decodeBase64)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.HashMap                as HM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text, unpack)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding          as Text
import           Data.Time                   (getCurrentTime)
import           GHC.Conc                    (atomically, readTVar, readTVarIO,
                                              writeTVar)
import           GHC.Generics                (Generic)
import           Network.AWS                 (send)
import           Network.AWS.KMS             (drsPlaintext)
import           Network.AWS.KMS.Decrypt     (decrypt)
import qualified Network.HTTP.Client.Conduit as Client
import qualified Network.HTTP.Simple         as Client
import           Network.HTTP.Types          (hAuthorization, status200,
                                              status400, status404)
import           Network.Wai                 (Request (..), Response,
                                              ResponseReceived, defaultRequest,
                                              lazyRequestBody, responseLBS)
import           Text.Shakespeare.Text       (st)

data TokenResp = TokenResp
  {access_token :: Text}
  deriving (Generic, Show)

instance FromJSON TokenResp

proxy :: Request -> (Response -> IO ResponseReceived) -> App ResponseReceived
proxy req respond = do
  Env {eCache, eConfig} <- ask
  let host = requestHeaderHost req
  case (host, host >>= flip Map.lookup eConfig . decodeUtf8) of
    (Just host, Just config) -> do
      hcReq <- liftIO $ toHttpClientReq config host req
      token <- exchangeToken config
      case token of
        Left resp -> liftIO $ respond $ resp
        Right t -> do
          resp <- Client.httpLBS (Client.setRequestHeader hAuthorization [encodeUtf8 t] hcReq)
          liftIO $ respond $ toWaiResp resp
    _ -> liftIO $ respond $ responseLBS status404 [] ""

toHttpClientReq :: Config -> BS.ByteString -> Request -> IO Client.Request
toHttpClientReq Config {App.isSecure, port} host req = do
  body <- lazyRequestBody req
  pure $
    Client.defaultRequest
      { Client.method = requestMethod req,
        Client.host = host,
        Client.secure = isSecure,
        Client.path = rawPathInfo req,
        Client.queryString = rawQueryString req,
        Client.port = port,
        Client.requestHeaders = requestHeaders req,
        Client.requestBody = Client.RequestBodyLBS body
      }

exchangeToken :: Config -> App (Either Response Text)
exchangeToken Config {tokenEndpoint, clientId, clientSec, serverAud} = do
  cache <- asks eCache
  cachedToken <- liftIO $ HM.lookup (clientId, serverAud) <$> readTVarIO cache
  now <- liftIO getCurrentTime
  case cachedToken of
    Just CacheValue {token, expired} | expired < now -> pure $ Right token
    _ -> do
      decrptedClientSec <- decryptClientSec clientSec
      req <- Client.parseRequest $ unpack [st|POST #{tokenEndpoint}|]
      case decrptedClientSec of
        Just sec -> do
          resp <-
            Client.httpLBS
              ( Client.setRequestBodyURLEncoded
                  [ ("grant_type", "client_credentials"),
                    ("client_id", encodeUtf8 clientId),
                    ("client_secret", encodeUtf8 sec),
                    ("audience", encodeUtf8 serverAud)
                  ]
                  req
              )
          if Client.responseStatus resp == status200
            then case eitherDecode (Client.responseBody resp) of
              Right TokenResp {access_token} -> liftIO $
                atomically $ do
                  m <- readTVar cache
                  writeTVar cache $ HM.insert (clientId, serverAud) (CacheValue access_token now) m
                  pure (Right access_token)
              Left e -> pure (Left (toWaiResp resp))
            else pure (Left (toWaiResp resp))
        Nothing -> pure (Left (responseLBS status400 [] ""))

decryptClientSec :: Secret -> App (Maybe Text)
decryptClientSec (Plain t) = pure $ Just t
decryptClientSec (KmsEncrypted encrypted) = case decodeBase64 (Text.encodeUtf8 encrypted) of
  Left e -> pure Nothing
  Right v -> fmap Text.decodeUtf8 . view drsPlaintext <$> (aws . send . decrypt) v

toWaiResp resp =
  responseLBS
    (Client.responseStatus resp)
    (Client.responseHeaders resp)
    (Client.responseBody resp)
