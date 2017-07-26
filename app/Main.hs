{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Web request stuff
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Coinbase stuff
import Coinbase.Exchange.Types

-- Haskell stuff
import Data.Either

import Lib
import Types

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for trades etc. so you don't lose money 
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  let sandboxConfig = sandboxConf mgr

  {- Request info from GDAX API -}
  eitherCandles <- getMostRecentCandles liveConfig (NumCandles 30) (Minutes 2)

  {- Plot/display the data -}
  print (length <$> eitherCandles)
  either (putStrLn . show) (displayCandles) eitherCandles
                    
