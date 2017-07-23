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

Then take a look at `app/Main.hs`

```
.
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── beginners-luck.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs
```

# See also

- API Reference: https://docs.gdax.com/
- API Wrapper Reference: https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/
 - Market data functions: https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/docs/Coinbase-Exchange-MarketData.html
 - Market data types (example): https://hackage.haskell.org/package/coinbase-exchange-0.3.0.0/docs/Coinbase-Exchange-Types-MarketData.html#t:Candle
- Ethereum White Paper: https://github.com/ethereum/wiki/wiki/White-Paper
- API response example: https://api.gdax.com/products/ETH-USD/candles
