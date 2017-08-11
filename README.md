# Beginners Luck

> "Every search begins with beginner’s luck."  
> Paulo Coelho

This project is a bot that automatically buys and sell the [ethereum crypto-currency](https://github.com/ethereum/wiki/wiki/White-Paper) in an effort to make a profit (or at least minimize loss). It uses a [double exponential moving average crossover](http://www.investopedia.com/articles/trading/10/double-exponential-moving-average.asp) strategy.

You can find the code for the bot itself in the [`bot/`](./bot/) subdirectory. It is written in Haskell, and instructions for running or developing the bot can be found in that folder.

The bot's performance is logged in a database and the [`analysis/`](./analysis) subdirectory contains some data visualization.
