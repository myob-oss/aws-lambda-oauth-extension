module Aws where

import           App
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (asks)
import           Network.AWS            (AWS, HasEnv (envLogger),
                                         Region (Sydney), runAWS, runResourceT,
                                         within)

aws :: AWS a -> App a
aws action = do
  eaEnv <- asks eAws
  liftIO $ (runResourceT . runAWS eaEnv) action
