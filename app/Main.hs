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
import Data.Void

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
makeDecision :: Window -> LookingTo -> Decision
makeDecision (Window (EMA short, EMA long) _) (LookingTo Buy) =
  if short > long
  then Decision Buy
  else Hold
makeDecision (Window (EMA short, EMA long) _) (LookingTo Sell) =
  if short < long -- threshold here to sell sooner and be risk averse
  then Decision Sell
  else Hold

-- | Fill in with actual API calls,
-- | but for now just print to see what bot does
executeDecision :: ExchangeConf -> (Decision, Window) -> LookingTo -> IO LookingTo
executeDecision _ (Hold, window) lookingTo = do
  putStrLn ("Held with market price at: " ++ showCost (unPrice window))
  return lookingTo
executeDecision _ (Decision Sell, window) _ = do
  putStrLn ("Sold at price: " ++ showCost (unPrice window))
  return (LookingTo Buy)
executeDecision _ (Decision Buy, window) _ = do
  putStrLn ("Bought at price: " ++ showCost (unPrice window))
  return (LookingTo Sell)

  -- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order
  -- polling time delay

-- | Tie everything together
makeAndExecuteDecisions ::
  ExchangeConf
  -> Stream (Of Window) IO Void
  -> StateT LookingTo IO Void
makeAndExecuteDecisions config windows = do
  windowsEither <- liftIO $ S.next windows
  case windowsEither of
    Left v -> do
      liftIO $ putStrLn "what"
      absurd v
    Right (window, remainingWindows) -> do
      lastLookingTo <- get
      let decision = makeDecision window lastLookingTo
      nextLookingTo <- liftIO $ executeDecision config (decision, window) lastLookingTo
      put nextLookingTo
      makeAndExecuteDecisions config remainingWindows

{----- MAIN -----}

main :: IO ()
main = do
  {- GDAX API setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  -- let sandboxConfig = sandboxConf mgr

  {- Request first round of info from GDAX API -}
  firstWindow <- getFirstWindow liveConfig

  {- Construct the stream of windows based on some delay -}
  let windows = S.delay
                (fromIntegral (unSeconds pollLength))
                (S.iterateM (getNextWindow liveConfig) (return firstWindow))

  {- Run an infinite loop to make and execute decisions based on market data -}
  _ <- runStateT (makeAndExecuteDecisions liveConfig windows) (LookingTo Buy)

  putStrLn "done!"
