{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Coinbase.Exchange.Types.Core (ProductId (ProductId))
import Coinbase.Exchange.MarketData (Candle)

-- Haskell Stuff
import Numeric
import Text.Printf

newtype NumCandles = NumCandles Int
newtype Minutes = Minutes Int

{----- COST REPRESENTATIONS -----}
type NumberRepresentation = Rational

newtype SMA = SMA NumberRepresentation
  deriving (Show, Eq, Num)
newtype EMA = EMA NumberRepresentation
  deriving (Show, Eq, Num)
newtype Price = Price NumberRepresentation
  deriving (Show, Eq, Num)

{----- PRETTY PRINTING NUMBERS -----}
class ShowCost a where
  showCost :: a -> String

instance ShowCost SMA where
  showCost (SMA a) = printf "%.2f" (truncated a)
    where
      truncated :: NumberRepresentation -> Double
      truncated = fromRat

instance ShowCost EMA where
  showCost (EMA a) = printf "%.2f" (truncated a)
    where
      truncated :: NumberRepresentation -> Double
      truncated = fromRat

instance ShowCost Price where
  showCost (Price a) = printf "%.2f" (truncated a)
    where
      truncated :: NumberRepresentation -> Double
      truncated = fromRat

instance ShowCost Rational where
  showCost = printf "%.2f" . truncated
    where
      truncated :: Rational -> Double
      truncated = fromRat

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

{----- CONSTANTS -----}
ethUSDticker :: ProductId
ethUSDticker = ProductId ("ETH-USD")

candleLength :: Minutes
candleLength = Minutes 2

shortNumCandles :: NumCandles
shortNumCandles = NumCandles 10

longNumCandles :: NumCandles
longNumCandles = NumCandles 30

pollLength :: Seconds
pollLength = Seconds 120

newtype Seconds = Seconds { unSeconds :: Int }
