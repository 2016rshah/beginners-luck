{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Coinbase.Exchange.Types.Core (ProductId (ProductId))
import Coinbase.Exchange.MarketData (Candle)

newtype NumCandles = NumCandles Int
newtype Minutes = Minutes Int

-- Not sure what the difference between ratio and rational are and when I should use which ?
-- Another option is the "scientific" type from Data.Scientific ?
type NumberRepresentation = Rational

newtype SMA = SMA NumberRepresentation
  deriving (Show, Eq, Num)
newtype EMA = EMA NumberRepresentation
  deriving (Show, Eq, Num)
newtype Price = Price NumberRepresentation
  deriving (Show, Eq, Num)

type CoinbaseCandle = Candle

data Action = Buy | Sell
  deriving Show
data LookingTo = LookingTo Action
data Decision = Decision Action | Hold
  deriving Show

-- (Short, Long) ClosingPrice
data Window = Window {
  unEMAs :: (EMA, EMA),
  unPrice :: Price }

{- Constants -}
ethUSDticker :: ProductId
ethUSDticker = ProductId ("ETH-USD")

candleLength :: Minutes
candleLength = Minutes 2

shortNumCandles :: NumCandles
shortNumCandles = NumCandles 10

longNumCandles :: NumCandles
longNumCandles = NumCandles 30
