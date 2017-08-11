import Coinbase.Exchange.Types.MarketData

import Data.Time.Clock
import Data.Time.Format

import Test.Tasty
import Test.Tasty.HUnit

import Types
import Lib

defTime :: UTCTime
defTime = parseTimeOrError True defaultTimeLocale "%-d %B %Y" "6 January 2012" 

defLow :: Low
defLow = Low 0

defHigh :: High
defHigh = High 0

defOpen :: Open
defOpen = Open 0

defVolume :: Volume
defVolume = Volume 0

prices :: [CoinbaseCandle]
prices = [
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 1) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 2) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 3) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 4) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 5) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 6) defVolume,
  Coinbase.Exchange.Types.MarketData.Candle defTime defLow defHigh defOpen (Close 7) defVolume
         ]

firstWindow :: [CoinbaseCandle]
firstWindow = take 5 prices

secondWindow :: [CoinbaseCandle]
secondWindow = take 5 . drop 1 $ prices

thirdWindow :: [CoinbaseCandle]
thirdWindow = take 5 . drop 2 $ prices

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [smaTests]

smaTests = testGroup "Simple Moving Average tests"
           [ testCase "First window" $
             sma firstWindow @?= 3
           , testCase "Second window" $
             sma secondWindow @?= 4
           , testCase "Third window" $
             sma thirdWindow @?= 5
           ]

emaTests = testGroup "Exponential Moving Average tests"
           [ testCase "First window" $
             firstEMA @?= 3
           , testCase "Second window" $
             secondEMA @?= 4
           , testCase "Third window" $
             thirdEMA @?= 5
           ]
  where
    firstEMA = case sma firstWindow of
      SMA firstSMA -> ema (EMA firstSMA) firstWindow
    secondEMA = ema firstEMA secondWindow
    thirdEMA = ema secondEMA thirdWindow
