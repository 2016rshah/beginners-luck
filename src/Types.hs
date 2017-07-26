{-# LANGUAGE OverloadedStrings #-}
module Types where

import Coinbase.Exchange.Types.Core

data NumCandles = NumCandles Int
data Minutes = Minutes Int

-- | Useful constant 
ethUSDticker :: ProductId
ethUSDticker = ProductId "ETH-USD"
