{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Coinbase.Exchange.Types.Core
import Coinbase.Exchange.MarketData

newtype NumCandles = NumCandles Int
newtype Minutes = Minutes Int

-- Not sure what the difference between ratio and rational are and when I should use which ?
newtype SMA = SMA Rational
  deriving (Show, Eq, Num)
newtype EMA = EMA Rational
  deriving (Show, Eq, Num)

type CoinbaseCandle = Candle

-- | Useful constant 
ethUSDticker :: ProductId
ethUSDticker = ProductId "ETH-USD"
