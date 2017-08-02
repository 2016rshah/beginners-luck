module Main where

{----- IMPORTS -----}

-- Web request stuff
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Coinbase stuff
import Coinbase.Exchange.Types
  ( ExchangeConf (ExchangeConf)
  , ApiType (Live, Sandbox))

-- Streaming stuff
import Streaming
import qualified Streaming.Prelude as S

-- Haskell stuff


-- Project stuff
import Lib
import Types

{----- FUNCTIONS -----}

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for trades etc. so you don't lose money
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

makeDecision :: World -> Decision
makeDecision (World conf (Window (EMA short, EMA long) _) (LookingTo Buy)) =
  if short > long
  then Decision Buy
  else Hold
makeDecision (World conf (Window (EMA short, EMA long) _) (LookingTo Sell)) =
  if short < long -- threshold here to sell sooner and be risk averse
  then Decision Sell
  else Hold

-- | Primarily gonna edit the LookingTo field in the World
executeDecision :: (Decision, World) -> IO World
executeDecision (Hold, (World config window lookingTo))= do
  putStrLn ("Held with market price at: " ++ show (unPrice window))
  return (World config window lookingTo)
executeDecision (Decision Sell, (World config window lookingTo))= do
  putStrLn ("Sold at price: " ++ show (unPrice window))
  return (World config window (LookingTo Buy))
executeDecision (Decision Buy, (World config window lookingTo))= do
  putStrLn ("Bought at price: " ++ show (unPrice window))
  return (World config window (LookingTo Sell))

-- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order

{----- MAIN -----}

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  let sandboxConfig = sandboxConf mgr

  {- Request first round of info from GDAX API -}
  firstWorld <- getFirstWorld liveConfig

  

  let worlds = S.delay 5 (S.iterateM getNextWorld (return firstWorld))
  let decisionWorlds = S.map (\w -> (makeDecision w, w)) worlds
  let e = S.map (\dw -> executeDecision dw) decisionWorlds 
  
  -- let decisions = S.map (\w -> makeDecision w executedDecisions) worlds 

  -- let decisionWorlds = S.zip decisions worlds

  -- let executedDecisions = S.map (executeDecision) decisionWorlds
  
  --S.map (\w -> makeDecision w (LookingTo Buy)) (S.delay 5 (S.iterateM getNextWorld (return firstWorld)))
  --S.map (\(d, w) lt -> executeDecision 
  {- Infinite loop: every period we get the most recent candle and EMAs -}
  --S.effects $
   -- S.map
    --(\x -> (executeDecision x))
  -- S.effects $
  --     ((S.map
  --       (\world -> ((makeDecision world (LookingTo Buy), world)))
  --        (S.delay 5 (S.iterateM getNextWorld (return firstWorld)))))

  putStrLn "done!"

