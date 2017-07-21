{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Web request stuff
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Coinbase stuff
import Coinbase.Exchange.MarketData
import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core

-- Haskell stuff
import Data.Either
import Control.Monad.IO.Class 

-- Clock stuff
import Data.Time.Clock

-- Graphics stuff
import Graphics.Rendering.Chart.Easy as E
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)

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

toChartCandle :: Coinbase.Exchange.MarketData.Candle -> E.Candle Integer Double
toChartCandle (Coinbase.Exchange.MarketData.Candle utcTime l h o c v) = 
  E.Candle time low open ((open + close) / 2) close high
  where
    time = floor $ utctDayTime utcTime :: Integer
    low = unLow l
    open = unOpen o
    close = unClose c
    high = unHigh h

candlePlot :: [E.Candle Integer Double] -> EC l2 (PlotCandle Integer Double)
candlePlot cs = liftEC $ do
  plot_candle_values .= cs

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
  eitherCandles <- (runExchange liveConfig (getHistory ethUSDticker Nothing Nothing Nothing))

  case eitherCandles of
    Right candles -> do 
      {- Output results -}
      putStrLn (show ticker)
      putStrLn (show (head candles))
      putStrLn "Success"
      let cs = map toChartCandle (take 75 candles)
      toFile def "mychart.svg" $ do
        plotLeft (candlePlot (cs))
        plotRight (candlePlot (cs))
    Left _ -> putStrLn "yikes"

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,((sin (x*3.14159/5)))) | x <- xs]
