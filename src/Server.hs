module Server where
import           App
import           Colog                       (simpleMessageAction)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ReaderT (runReaderT))
import qualified Data.HashMap                as HM
import           Data.Map                    (Map)
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text, pack)
import           Dhall                       (auto, input)
import           GHC.Conc                    (newTVarIO)
import           Lib
import           Network.HTTP.Client.Conduit (Request, newManager)
import           Network.Wai.Handler.Warp    (run)
import           System.Environment          (getEnv, lookupEnv)
startServer :: Int -> IO ()
startServer port = do
  env <- mkEnv
  run port $ \req resp -> runReaderT (proxy req resp) env

mkEnv :: IO Env
mkEnv = do
  configLoc <- fromMaybe "./config.dhall" <$> lookupEnv "ALOE_CONFIG"
  envs <- EnvVar . pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
  config <- input auto (pack configLoc)
  cache <- newTVarIO mempty
  return $ Env
    { eLog = simpleMessageAction
    , eReqCtx = EnvReqCtx Nothing
    , eCache = cache
    , eConfig = config
    , eEnvVar = envs
    }
