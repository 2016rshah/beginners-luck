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
import Data.Time.LocalTime

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

-- | Converts from the candle type given by the API to the candle type needed for the chart
toChartCandle :: Coinbase.Exchange.MarketData.Candle -> E.Candle LocalTime Double
toChartCandle (Coinbase.Exchange.MarketData.Candle utcTime l h o c v) = 
  E.Candle time low open ((open + close) / 2) close high
  where
    -- | Convert a UTCTime to a LocalTime with the default time zone
    time = utcToLocalTime utc utcTime
    -- | Extract values from their type wrappers
    low = unLow l
    open = unOpen o
    close = unClose c
    high = unHigh h

-- | Fills in the aesthetic details to make the candle plot look nice
mkCandlePlot :: [E.Candle a b] -> EC l2 (PlotCandle a b)
mkCandlePlot cs = liftEC $ do
                 plot_candle_fill .= True
                 plot_candle_rise_fill_style .= solidFillStyle (opaque green)
                 plot_candle_fall_fill_style .= solidFillStyle (opaque red)
                 plot_candle_line_style .= LineStyle 1 (opaque black) [] LineCapButt LineJoinMiter
                 plot_candle_tick_length .= 0
                 plot_candle_width .= 2
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

  {- Plot/display the data -}
  putStrLn (show ticker)
  case eitherCandles of
    Right candles@(candle:_) -> do 
      putStrLn (show candle)
      print (length candles)
      let plotPoints = map toChartCandle (take 100 candles)
      let renderedPlot = plot (mkCandlePlot plotPoints)
      toFile def "mychart.svg" renderedPlot
    Left failure -> do
      putStrLn "API query for candles did not succeed. Failed with error:"
      putStrLn (show failure)
