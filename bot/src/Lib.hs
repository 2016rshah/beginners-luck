module Lib
    ( getMostRecentCandles
    , getNextWindow
    , getFirstWindow
    ) where

{----- IMPORTS -----}

-- Coinbase stuff
import Coinbase.Exchange.MarketData
  ( Candle (Candle)
  , unClose
  , getHistory)
import Coinbase.Exchange.Types
  ( ExchangeConf
  , ExchangeFailure
  , runExchange)

-- Clock stuff
import Data.Time.Clock

-- Haskell stuff
import Control.Exception (catch)
import Network.HTTP.Client (HttpException)

-- Beginners luck stuff
import Types

{----- FUNCTIONS -----}

-- | Number of candles you want -> candle interval in seconds -> (timestamp interval, granularity)
getCandleRequestParams :: NumCandles -> Seconds -> (Int, Int)
getCandleRequestParams (NumCandles numCandles) (Seconds candleIntervalSeconds) =
  ((candleIntervalSeconds * numCandles), candleIntervalSeconds)

-- | ExchangeConf: which exchange to run request on
-- | NumCandles: number of candles to get
-- | Duration: duration of each candle
-- | For example you could say `getMostRecentCandles liveConfig (NumCandles 30) (Minutes 2)`
-- | Which would give the 30 newest candles each spanning 2 minutes over a total of an hour long window
getMostRecentCandles :: ExchangeConf -> NumCandles -> Seconds
                     -> IO (Either Coinbase.Exchange.Types.ExchangeFailure [CoinbaseCandle])
getMostRecentCandles conf numCandles candleIntervalSeconds = do
  let (timeWindow, granularity) = getCandleRequestParams numCandles candleIntervalSeconds
  endTime <- (addUTCTime (-apiDelayDuration)) <$> getCurrentTime
  let startTime = addUTCTime (realToFrac (-timeWindow)) endTime
  runExchange conf (getHistory
                          ethUSDticker
                          (Just startTime)
                          (Just endTime)
                          (Just granularity))
    where
      -- | Delay requests to the api by a bit to make sure computer clock isn't ahead of API clock
      apiDelayDuration :: NominalDiffTime
      apiDelayDuration = 10

-- | Helper function that extracts the most recent close price along with adjusting types
getClosePrice :: CoinbaseCandle -> Rational
getClosePrice (Coinbase.Exchange.MarketData.Candle _ _ _ _ c _) = toRational (unClose c)

-- | Exponential moving average over a list of candles and the previous EMA
-- | Note: computes over the entire passed in array
ema :: EMA -> [CoinbaseCandle] -> EMA
ema (EMA prevEMA) candles@(candle:_) = EMA ((getClosePrice candle - prevEMA) * multiplier + prevEMA)
  where
    multiplier :: Rational
    multiplier = 2 / (toRational (length candles) + 1)
ema (EMA _) _ = EMA 0 -- Empty array of candles

-- | Simple moving average over an array of candles
sma :: [CoinbaseCandle] -> SMA
sma [] = SMA 0
sma candles = SMA (totalSum candles / numCandles candles)
  where
    totalSum = (sum . map getClosePrice)
    numCandles = (fromIntegral . length)

-- | Using the old window data computes the next exponential moving average values
getNextWindow :: ExchangeConf -> Window -> IO Window
getNextWindow config oldWindow@(Window (shortEMA, longEMA) _) =
  {- Making request to API will throw exception if over rate limit, just try again -}
  flip catch handler $ do
    {- Request info from GDAX API -}
    eitherShortCandles <- getMostRecentCandles config shortNumCandles candleLength
    eitherLongCandles <- getMostRecentCandles config longNumCandles candleLength
    case (eitherShortCandles, eitherLongCandles) of
      (Right shortCandles@(recentCandle:_), Right longCandles) -> do
        {- Calculate next EMAs -}
        let shortEMA' = ema shortEMA shortCandles
        let longEMA' = ema longEMA longCandles
        let recentClosingPrice = getClosePrice recentCandle
        putStrLn $ ("---" ++ ['\n'] ++ "Short EMA: " ++ showCost shortEMA' ++ ['\n'] ++ "Long EMA: " ++ showCost longEMA' ++ ['\n'] ++ "Closing Price: " ++ showCost recentClosingPrice)
        return (Window (shortEMA', longEMA') (Price recentClosingPrice))
      (Left err, _) -> failedRequest (show err)
      (_, Left err) -> failedRequest (show err)
      _ -> failedRequest "No candles returned, make sure you are not using sandbox config."
  where
    handler :: HttpException -> IO Window
    handler _ = putStrLn "Failed Request; Retrying" >> return oldWindow

    failedRequest :: String -> IO Window
    failedRequest err = do
      putStrLn err
      putStrLn "got an error, but chugging along anyways"
      return oldWindow

-- | Helper for what to do if the rest of the computations in the program depend on this success
failEither :: Show a => Either a b -> IO b
failEither (Left err) = fail (show err)
failEither (Right b) = return b

getFirstWindow :: ExchangeConf -> IO Window
getFirstWindow config = do
  shortCandles <- failEither =<< getMostRecentCandles config shortNumCandles candleLength
  longCandles <- failEither =<< getMostRecentCandles config longNumCandles candleLength
  let shortSMA = sma shortCandles
  let longSMA = sma longCandles
  putStrLn $ "Short SMA: " ++ showCost shortSMA
  putStrLn $ "Long SMA: " ++ showCost longSMA
  case shortCandles of
    (candle:_) -> do
      let window = case (shortSMA, longSMA) of
            ((SMA short), (SMA long)) ->
              (Window (EMA short, EMA long) (Price (getClosePrice candle)))
      return window
    _ -> error "No candles returned, make sure you are not using sandbox config."
