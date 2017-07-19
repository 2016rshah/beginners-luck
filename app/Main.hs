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

btcUSDticker :: ProductId
btcUSDticker = ProductId "BTC-USD"

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
  liveConfig <- liveConf <$> newManager tlsManagerSettings
  sandboxConfig <- sandboxConf <$> newManager tlsManagerSettings
  ticker <- runExchange sandboxConfig (getProductTicker btcUSDticker)
  -- Usually using sandbox is probably usually better, but for some things you *must* use live config
  -- Three parameters are startTime, endTime, and granularity but Nothing leaves default
  candles <- runExchange liveConfig (getHistory btcUSDticker Nothing Nothing Nothing)
  putStrLn (show ticker)
  putStrLn (show (head <$> candles))
  putStrLn "Success"
  -- now <- getCurrentTime
  -- let hourAgo = addUTCTime (-(nominalDay / 24)) now
  -- let twoHoursAgo = addUTCTime (2 * (-(nominalDay / 24))) now    
  -- x <- liftIO $ runExchange config (getProductTicker btcUSDticker)
  -- xs <- liftIO $ runExchange config (getTrades btcUSDticker)

  
  -- putStrLn (show xs)
  -- putStrLn "Hello World!"
