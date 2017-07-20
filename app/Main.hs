{-# LANGUAGE OverloadedStrings #-}

module Main where

import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Coinbase.Exchange.MarketData
import Control.Monad.IO.Class 
import Data.Time.Clock

import Lib

-- | Useful constant 
ethUSDticker :: ProductId
ethUSDticker = ProductId "ETH-USD"

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for actual trades etc. 
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

nominalDay :: NominalDiffTime
nominalDay = 86400

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  liveConfig <- liveConf <$> newManager tlsManagerSettings
  sandboxConfig <- sandboxConf <$> newManager tlsManagerSettings

  {- Request info from GDAX API -}
  -- For historical data you *must* use the liveConfig or you'll get bogus values
  -- For actual trades you probably want to use sandboxConfig so you don't lose a ton of money
  ticker <- runExchange liveConfig (getProductTicker ethUSDticker)
  -- Three parameters are startTime, endTime, and granularity but Nothing leaves default
  candles <- runExchange liveConfig (getHistory ethUSDticker Nothing Nothing Nothing)

  {- Output results -}
  putStrLn (show ticker)
  putStrLn (show (head <$> candles))
  putStrLn "Success"  
