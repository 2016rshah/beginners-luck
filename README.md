# Beginners Luck

> "Every search begins with beginner’s luck."

# Requirements

 - [The Haskell Platform](https://www.haskell.org/platform/)

# Getting started

After downloading the Haskell platform run the following from the terminal:

 - `git clone https://github.com/2016rshah/beginners-luck`
 - `cd beginners-luck`
 - `stack build`
   - If there are errors read them because you might have to do `stack setup` or `stack install` or something
 - `stack exec beginners-luck-exe`

Then take a look at `app/Main.hs` and `src/Lib.hs

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── beginners-luck.cabal
├── src
│   ├── Lib.hs
│   └── Types.hs
├── stack.yaml
└── test
    └── Spec.hs
```

# Testing and Liquid Haskell

Some program properties will be checked with [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell-blog/). If you're curious, install Liquid Haskell and run

```
stack exec liquid -- --prune-unsorted src/Types.hs src/Lib.hs app/Main.hs
```

Run the tests with `stack build --test`

# Roadmap
 - ~~Streams: Refactor to use [Streams](https://hackage.haskell.org/package/streaming) instead of explicit recursion and stuff~~
 - ~~Trades: Start using the computed data to make trade decisions~~
  - > ~~A trader enters buy orders when the short-term EMA crosses above the long-term EMA or enters sell orders when the short-term EMA crosses below the long-term EMA.~~
 - Profits: Keep track of how much money we would make or lose if we ran the bot
  - Right now it prints rationals to the console, make it print human readable numbers instead and perhaps in a better format (the kind of thing you can plug into excel)
 - Persistence: Start writing the data to a database or something rather than the console
 - Compute/print the stop-limit order prices that you will place trades with
 - Sell early by implementing the threshold limit in the decision making process
 - Obtain API keys, etc.
 - Start making actual API calls to the sandbox to see progress

# See also

- API Reference: https://docs.gdax.com/
- API Wrapper Reference: https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/
 - Market data functions: https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/docs/Coinbase-Exchange-MarketData.html
 - Market data types (example): https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/docs/Coinbase-Exchange-Types-MarketData.html#t:Candle
- Ethereum White Paper: https://github.com/ethereum/wiki/wiki/White-Paper
- API response example: https://api.gdax.com/products/ETH-USD/candles
