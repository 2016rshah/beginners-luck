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
executeDecision :: ExchangeConf -> Connection -> (Decision, Window) -> LookingTo -> IO LookingTo
executeDecision _ conn (Hold, window) lookingTo = do
  putStrLn ("Held with market price at: " ++ show (unPrice window))
  insertEntryIntoDatabase conn window lookingTo
  return lookingTo
executeDecision _ conn (Decision Sell, window) lookingTo = do
  putStrLn ("Sold at price: " ++ show (unPrice window))
  insertEntryIntoDatabase conn window lookingTo
  return (LookingTo Buy)
executeDecision _ conn (Decision Buy, window) lookingTo = do
  putStrLn ("Bought at price: " ++ show (unPrice window))
  insertEntryIntoDatabase conn window lookingTo
  return (LookingTo Sell)

  -- Variables
  -- percentage for limit order: 1/200
  -- time per window: 3 minutes
  -- short length: 10
  -- long length: 30
  -- threshold for putting the sell order
  -- polling time delay

insertEntryIntoDatabase :: Connection -> Window -> LookingTo -> IO ()
insertEntryIntoDatabase conn (Window (EMA short, EMA long) (Price price)) lookingTo = do
  timestamp <- getCurrentTime
  execute conn "INSERT INTO database (timestamp, short, long, price, position) VALUES (?, ?, ?, ?, ?)" (entry timestamp)
  where
    entry :: UTCTime -> (Text, Double, Double, Double, Text)
    entry ts = (pack (show ts), fromRational short, fromRational long, fromRational price, pack (show lookingTo))

-- | Tie everything together
makeAndExecuteDecisions ::
  ExchangeConf
  -> Connection
  -> Stream (Of Window) IO Void
  -> StateT LookingTo IO Void
makeAndExecuteDecisions config conn windows = do
  windowsEither <- liftIO $ S.next windows
  case windowsEither of
    Left v -> do
      liftIO $ putStrLn "what"
      absurd v
    Right (window, remainingWindows) -> do
      lastLookingTo <- get
      let decision = makeDecision window lastLookingTo
      nextLookingTo <- liftIO $ executeDecision config conn (decision, window) lastLookingTo
      put nextLookingTo
      makeAndExecuteDecisions config conn remainingWindows

{----- DATABASE -----}

-- insertWindowIntoDatabase :: Window -> IO ()
-- insertWindowIntoDatabase


{----- MAIN -----}

main :: IO ()
main = do
  {- setup stuff -}
  mgr <- newManager tlsManagerSettings
  let liveConfig = liveConf mgr
  -- let sandboxConfig = sandboxConf mgr
  conn <- open "database.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS database (timestamp TEXT PRIMARY KEY, long REAL, short REAL, price REAL, position TEXT)"

  {- Request first round of info from GDAX API -}
  firstWindow <- getFirstWindow liveConfig

  {- Construct the stream of windows based on some delay -}
  let windows = S.delay
                (fromIntegral (unSeconds pollLength))
                (S.iterateM (getNextWindow liveConfig) (return firstWindow))

  {- Run an infinite loop to make and execute decisions based on market data -}
  _ <- runStateT (makeAndExecuteDecisions liveConfig conn windows) (LookingTo Buy)

  putStrLn "done!"