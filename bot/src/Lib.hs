module Lib
    (
      getNextWindowFromNow
    , getFirstWindowFromNow
    , getNextWindow
    , getFirstWindow
    , getTimeIntervals
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

-- Get the specified number of candles of specified width that go back from the endTime
getCandlesInPeriod ::
  ExchangeConf ->
  NumCandles ->
  Seconds ->
  UTCTime ->
  IO (Either Coinbase.Exchange.Types.ExchangeFailure [CoinbaseCandle])
getCandlesInPeriod conf (NumCandles numCandles) (Seconds granularity) endTime = do
  let timeWindow = granularity * numCandles
  let startTime = addUTCTime (realToFrac (-timeWindow)) endTime
  runExchange conf (getHistory
                          ethUSDticker
                          (Just startTime)
                          (Just endTime)
                          (Just granularity))

getFirstWindowFromNow :: ExchangeConf -> IO Window
getFirstWindowFromNow config = do
  endTime <- (addUTCTime (-apiDelayDuration)) <$> getCurrentTime
  getFirstWindow config endTime
  where
    apiDelayDuration :: NominalDiffTime
    apiDelayDuration = 10

getFirstWindow ::
  ExchangeConf ->
  UTCTime ->
  IO Window
getFirstWindow config endTime = do
  shortCandles <- failEither =<< getCandlesInPeriod config shortNumCandles candleLength endTime
  longCandles <- failEither =<< getCandlesInPeriod config longNumCandles candleLength endTime
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
    _ -> fail "No candles returned, make sure you aren't using sandbox config"
  where
    failEither :: Show a => Either a b -> IO b
    failEither (Left err) = fail (show err)
    failEither (Right b) = return b

-- | Using the old window data computes the next exponential moving average values
getNextWindowFromNow :: ExchangeConf -> Window -> IO Window
getNextWindowFromNow config oldWindow = do
  endTime <- (addUTCTime (-apiDelayDuration)) <$> getCurrentTime
  getNextWindow config oldWindow endTime
  where
    -- | Delay requests to the api by a bit to make sure computer clock isn't ahead of API clock
    apiDelayDuration :: NominalDiffTime
    apiDelayDuration = 10

getNextWindow ::
  ExchangeConf ->
  Window ->
  UTCTime ->
  IO Window
getNextWindow config oldWindow@(Window (shortEMA, longEMA) _) nextTimeStamp = flip catch handler $ do
  eitherShortCandles <- getCandlesInPeriod config shortNumCandles candleLength nextTimeStamp
  eitherLongCandles <- getCandlesInPeriod config shortNumCandles candleLength nextTimeStamp
  case (eitherShortCandles, eitherLongCandles) of
    (Right shortCandles, Right longCandles) -> do
      case shortCandles of
        (recentCandle:_) -> do
          let shortEMA' = ema shortEMA shortCandles
          let longEMA' = ema longEMA longCandles
          let recentClosingPrice = getClosePrice recentCandle
          putStrLn $ ("---" ++ ['\n'] ++
                      "Short EMA: " ++ showCost shortEMA' ++ ['\n'] ++
                      "Long EMA: " ++ showCost longEMA' ++ ['\n'] ++
                      "Closing Price: " ++ showCost recentClosingPrice)
          return (Window (shortEMA', longEMA') (Price recentClosingPrice))
        _ -> failedRequest "No candles returned, make sure you aren't using sandbox config."
    _ -> failedRequest "Some type of ExchangeFailure"
  where
    handler :: HttpException -> IO Window
    handler _ = putStrLn "Failed Request; Ignoring" >> return oldWindow

    failedRequest :: String -> IO Window
    failedRequest err = do
      putStrLn err
      putStrLn "got an error, but chugging along anyways"
      return oldWindow

getTimeIntervals :: NominalDiffTime -> UTCTime -> UTCTime -> [UTCTime]
getTimeIntervals interval start end
  | start > end = []
  | otherwise = nextStart : (getTimeIntervals interval nextStart end)
  where nextStart = addUTCTime interval start
