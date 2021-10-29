module App where

import           Colog                       (HasLog (..), LogAction, Message,
                                              Msg (msgText), hoistLogAction)
import qualified Colog                       as CL
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Reader        (ReaderT)
import           Data.Functor.Contravariant  (contramap)
import qualified Data.HashMap                as HM
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime)
import           Dhall                       (FromDhall, Generic)
import           GHC.Conc                    (TVar)
import qualified Network.AWS                 as AWS
import           Network.HTTP.Client.Conduit (Manager)

data Secret = Plain Text | KmsEncrypted Text deriving Generic

data Config = Config
  { clientId      :: Text
  , clientSec     :: Secret
  , serverAud     :: Text
  , port          :: Int
  , isSecure      :: Bool
  , tokenEndpoint :: Text
  } deriving Generic

instance FromDhall Secret
instance FromDhall Config

type CacheKeyAud = Text
type CacheKeySub = Text
type CacheKey = (CacheKeySub, CacheKeyAud)
data CacheValue = CacheValue
  { token   :: Text
  , expired :: UTCTime}
type EnvCache = TVar (HM.Map CacheKey CacheValue)

data EnvVar = EnvVar
  { lambdaRuntimeApi :: Maybe Text
  }
data EnvReqCtx = EnvReqCtx
  {
    ercTraceId   :: Maybe Text
  }

data Env = Env
  {
    eLog    :: !(LogAction App Message)
  , eReqCtx :: !EnvReqCtx
  , eCache  :: !EnvCache
  , eConfig :: !(Map Text Config)
  , eEnvVar :: !EnvVar
  , eAws    :: !AWS.Env
  }

type App = ReaderT Env IO

instance HasLog Env Message App where
    getLogAction Env{eLog, eReqCtx} =
      contramap (\m -> m {msgText = "[" <> fromMaybe "" (ercTraceId eReqCtx) <> "]: " <> msgText m}) eLog
    {-# INLINE getLogAction #-}

    setLogAction newLogAction env = env { eLog = newLogAction }
    {-# INLINE setLogAction #-}
