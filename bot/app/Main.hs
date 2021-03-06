{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text

-- Database stuff
import Database.SQLite.Simple

-- Streaming stuff
import Streaming
import qualified Streaming.Prelude as S

-- Clock stuff
import Data.Time

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
executeDecision :: ExchangeConf -> Connection -> UTCTime -> (Decision, Window) -> LookingTo -> IO LookingTo
executeDecision _ conn runID (Hold, window) lookingTo = do
  putStrLn ("Held with market price at: " ++ showCost (unPrice window))
  insertEntryIntoDatabase conn runID window lookingTo "hold"
  return lookingTo
executeDecision _ conn runID (Decision Sell, window) lookingTo = do
  putStrLn ("Sold at price: " ++ showCost (unPrice window))
  insertEntryIntoDatabase conn runID window lookingTo "sold"
  return (LookingTo Buy)
executeDecision _ conn runID (Decision Buy, window) lookingTo = do
  putStrLn ("Bought at price: " ++ showCost (unPrice window))
  insertEntryIntoDatabase conn runID window lookingTo "bought"
  return (LookingTo Sell)

  -- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order
  -- polling time delay

insertEntryIntoDatabase :: Connection -> UTCTime -> Window -> LookingTo -> Text -> IO ()
insertEntryIntoDatabase conn runID (Window (EMA short, EMA long) (Price price)) lookingTo action = do
  timestamp <- getCurrentTime
  execute conn "INSERT INTO database (timestamp, runID, short, long, price, position, action) VALUES (?, ?, ?, ?, ?, ?, ?)" (entry timestamp)
  where
    entry :: UTCTime -> (Text, Text, Double, Double, Double, Text, Text)
    entry ts = (pack (show ts), pack (show runID), fromRational short, fromRational long, fromRational price, pack (show lookingTo), action)

-- | Tie everything together
makeAndExecuteDecisions ::
  ExchangeConf
  -> Connection
  -> UTCTime
  -> Stream (Of Window) IO Void
  -> StateT LookingTo IO Void
makeAndExecuteDecisions config conn runID windows = do
  windowsEither <- liftIO $ S.next windows
  case windowsEither of
    Left v -> do
      liftIO $ putStrLn "what"
      absurd v
    Right (window, remainingWindows) -> do
      lastLookingTo <- get
      let decision = makeDecision window lastLookingTo
      nextLookingTo <- liftIO $ executeDecision config conn runID (decision, window) lastLookingTo
      put nextLookingTo
      makeAndExecuteDecisions config conn runID remainingWindows

{----- DATABASE -----}

-- insertWindowIntoDatabase :: Window -> IO ()
-- insertWindowIntoDatabase


{----- MAIN -----}

main :: IO ()
main = do
  {- setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  programStartTime <- getCurrentTime
  -- let sandboxConfig = sandboxConf mgr
  conn <- open "database.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS database (timestamp TEXT PRIMARY KEY, runID TEXT, long REAL, short REAL, price REAL, position TEXT, action TEXT)"

  {- Request first round of info from GDAX API -}
  firstWindow <- getFirstWindow liveConfig

  {- Construct the stream of windows based on some delay -}
  let windows = S.delay
                (fromIntegral (unSeconds pollLength))
                (S.iterateM (getNextWindow liveConfig) (return firstWindow))

  {- Run an infinite loop to make and execute decisions based on market data -}
  _ <- runStateT (makeAndExecuteDecisions liveConfig conn programStartTime windows) (LookingTo Buy)

  putStrLn "done!"
