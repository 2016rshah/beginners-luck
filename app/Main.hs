{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Web request stuff
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Coinbase stuff
import Coinbase.Exchange.Types

-- Streaming stuff
import Streaming
import qualified Streaming.Prelude as S

-- Haskell stuff
import Data.Either
import Control.Concurrent
import Control.Monad

import Lib
import Types

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for trades etc. so you don't lose money
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

makeDecision :: World -> LookingTo -> Decision
makeDecision (World conf (EMA short, EMA long)) (LookingTo Buy) =
  if short > long
  then Decision Buy
  else Hold
makeDecision (World conf (EMA short, EMA long)) (LookingTo Sell) =
  if short < long -- threshold here to sell sooner and be risk averse
  then Decision Sell
  else Hold

-- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  let sandboxConfig = sandboxConf mgr

  {- Request first round of info from GDAX API -}
  firstWorld <- getFirstWorld liveConfig
  
  {- Infinite loop: every period we get the most recent candle and EMAs -}
  S.print $
    S.for
      (S.delay 5 (S.iterateM getNextWindow (return firstWorld)))
      (\world -> S.yield (makeDecision world (LookingTo Buy)))
    
  
  putStrLn "done!"

