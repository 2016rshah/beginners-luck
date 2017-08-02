{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Web request stuff
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Coinbase stuff
import Coinbase.Exchange.Types

-- Haskell stuff
import Data.Either
import Control.Concurrent
import Control.Monad
import Control.Monad.Loops

import Lib
import Types

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for trades etc. so you don't lose money
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

-- | One round of waiting and requesting the next set of data
keepGettingWindowsWithDelay :: Int -> World -> IO World
keepGettingWindowsWithDelay delayDuration prevWindow = do
  threadDelay delayDuration
  getNextWindow prevWindow 

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  let sandboxConfig = sandboxConf mgr

  {- Request first round of info from GDAX API -}
  firstWorld <- getFirstWorld liveConfig
  
  {- Infinite loop: every period we get the most recent candle and EMAs -}
  iterateM_ (keepGettingWindowsWithDelay 15000000) firstWorld
  
  putStrLn "done!"

