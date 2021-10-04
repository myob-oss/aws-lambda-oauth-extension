module Main where

import           Data.Maybe         (fromMaybe)
import           Server
import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

main :: IO ()
main = do
  port <- lookupEnv "ALOE_PORT"
  startServer $ fromMaybe 8081 (port >>= readMaybe)
