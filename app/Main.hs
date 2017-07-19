{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Coinbase.Exchange.MarketData
import Control.Monad.IO.Class 

import Lib

btcUSDticker :: ProductId
btcUSDticker = ProductId "BTC-USD"

sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let config = sandboxConf mgr 
  x <- liftIO $ runExchange config (getProductTicker btcUSDticker)
  putStrLn (show x)
  putStrLn "Hello World!"
