# Data

This project uses daily market and implied-volatility data for:

- S&P 500
- VIX
- FTSE 100
- FTSE implied volatility index

The original data were obtained from Bloomberg.

Raw Bloomberg data are not included in this public repository because of data-licensing restrictions.

The R scripts in this repository expect the relevant datasets to contain:

- `Date`
- `PX_LAST`

The analysis uses these data to calculate returns, estimate volatility models, generate Value-at-Risk forecasts, and perform out-of-sample backtesting.
