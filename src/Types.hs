{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Coinbase.Exchange.Types.Core
import Coinbase.Exchange.MarketData (Candle)
import Coinbase.Exchange.Types (ExchangeConf) 

newtype NumCandles = NumCandles Int
newtype Minutes = Minutes Int

-- Not sure what the difference between ratio and rational are and when I should use which ?
newtype SMA = SMA Rational
  deriving (Show, Eq, Num)
newtype EMA = EMA Rational
  deriving (Show, Eq, Num)

type CoinbaseCandle = Candle

type Window = (EMA, EMA)

data World = World ExchangeConf Window

{- Constants -} 
ethUSDticker :: ProductId
ethUSDticker = ProductId ("ETH-USD")

candleLength :: Minutes
candleLength = Minutes 2

shortNumCandles :: NumCandles
shortNumCandles = NumCandles 10

longNumCandles :: NumCandles
longNumCandles = NumCandles 30
