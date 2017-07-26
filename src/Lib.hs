module Lib
    ( getMostRecentCandles,
      displayCandles
    ) where

-- Coinbase stuff
import Coinbase.Exchange.MarketData
import Coinbase.Exchange.Types
import Coinbase.Exchange.Types.Core

-- Clock stuff
import Data.Time.Clock
import Data.Time.LocalTime

-- Graphics stuff
import Graphics.Rendering.Chart.Easy as E
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)

-- Beginners luck stuff
import Types

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


displayCandles :: [Coinbase.Exchange.MarketData.Candle] -> IO ()
displayCandles candles = do
  let plotPoints = map toChartCandle candles
  let renderedPlot = plot (mkCandlePlot plotPoints)
  toFile def "candlestick-chart.svg" renderedPlot


-- | Delay requests to the api by a bit to make sure computer clock isn't ahead of API clock
apiDelayDuration :: NominalDiffTime
apiDelayDuration = 10

-- | Number of candles you want -> candle interval in minutes -> (timestamp interval, granularity)
-- | For example 20 candles with 5 minute candles gives 
getCandleRequestParams :: NumCandles -> Minutes -> (NominalDiffTime, Int)
getCandleRequestParams (NumCandles numCandles) (Minutes candleIntervalMinutes) =
  (realToFrac (numSeconds * numCandles), numSeconds)
  where
    numSeconds = candleIntervalMinutes * 60

-- | ExchangeConf: which exchange to run request on
-- | NumCandles: number of candles to get
-- | Minutes: duration of each candle
-- | For example you could say `getMostRecentCandles liveConfig (NumCandles 30) (Minutes 2)`
-- | Which would give the 30 most recent candles each spanning two minutes over a total of an hour long window
getMostRecentCandles :: ExchangeConf -> NumCandles -> Minutes
                     -> IO (Either Coinbase.Exchange.Types.ExchangeFailure [Coinbase.Exchange.MarketData.Candle])
getMostRecentCandles conf numCandles candleIntervalMinutes = do
  let (timeWindow, granularity) = getCandleRequestParams numCandles candleIntervalMinutes
  endTime <- (addUTCTime (-apiDelayDuration)) <$> getCurrentTime
  let startTime = addUTCTime (-timeWindow) endTime
  runExchange conf (getHistory
                          ethUSDticker
                          (Just startTime)
                          (Just endTime)
                          (Just granularity))  
