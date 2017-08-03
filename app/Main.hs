module Main where

{----- IMPORTS -----}

-- Web request stuff
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Coinbase stuff
import Coinbase.Exchange.Types
  ( ExchangeConf (ExchangeConf)
  , ApiType (Live, Sandbox))

-- Haskell stuff
import Control.Monad.State

-- Streaming stuff
import Streaming
import qualified Streaming.Prelude as S

-- Project stuff
import Lib
import Types

{----- CONFIGURATIONS -----}

-- | Must use this for getting candles or historical data
liveConf :: Manager -> ExchangeConf
liveConf mgr = ExchangeConf mgr Nothing Live

-- | Use this for trades etc. so you don't lose money
sandboxConf :: Manager -> ExchangeConf
sandboxConf mgr = ExchangeConf mgr Nothing Sandbox

{----- ALGORITHM AND LOOP -----}

-- | Trading algorithm 
makeDecision :: World -> LookingTo -> Decision
makeDecision (World _ (Window (EMA short, EMA long) _)) (LookingTo Buy) =
  if short > long
  then Decision Buy
  else Hold
makeDecision (World _ (Window (EMA short, EMA long) _))  (LookingTo Sell) =
  if short < long -- threshold here to sell sooner and be risk averse
  then Decision Sell
  else Hold

-- | Fill in with actual API calls,
-- | but for now just print to see what bot does
executeDecision :: (Decision, World) -> LookingTo -> IO LookingTo
executeDecision (Hold, (World _ window)) lookingTo= do
  putStrLn ("Held with market price at: " ++ show (unPrice window))
  return lookingTo
executeDecision (Decision Sell, (World _ window)) _ = do
  putStrLn ("Sold at price: " ++ show (unPrice window))
  return (LookingTo Buy)
executeDecision (Decision Buy, (World _ window)) _ = do
  putStrLn ("Bought at price: " ++ show (unPrice window))
  return (LookingTo Sell)

  -- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order

-- | Tie everything together 
makeAndExecuteDecisions :: Stream (Of World) IO () -> Stream (Of LookingTo) (StateT LookingTo IO) ()
makeAndExecuteDecisions worlds = do
  worldEither <- liftIO $ S.next worlds
  case worldEither of
    Left _ -> error "yikes my world ended"
    Right (world, remainingWorlds) -> do
      lastLookingTo <- get
      let decision = makeDecision world lastLookingTo
      nextLookingTo <- liftIO $ executeDecision (decision, world) lastLookingTo
      put nextLookingTo
      makeAndExecuteDecisions remainingWorlds 


{----- MAIN -----}

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  -- let sandboxConfig = sandboxConf mgr

  {- Request first round of info from GDAX API -}
  firstWorld <- getFirstWorld liveConfig

  {- Construct the stream of worlds based on some delay -}
  let worlds = S.delay 5 (S.iterateM getNextWorld (return firstWorld))

  {- Run an infinite loop to make and execute decisions based on market data -}
  _ <- runStateT (S.effects (makeAndExecuteDecisions worlds)) (LookingTo Buy)

  putStrLn "done!"

