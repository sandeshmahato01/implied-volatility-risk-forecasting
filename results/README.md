# Results

This folder contains the main empirical results from the market risk forecasting models used in this project.

The models evaluated are:

1. Historical Simulation
2. Symmetric GARCH(1,1)
3. EGARCH(1,1)
4. Standalone Implied Volatility Model
5. EGARCH-X with Implied Volatility

Model performance is evaluated using out-of-sample Value-at-Risk forecasts at the:

- 95% confidence level
- 99% confidence level

The main backtesting measures include:

- Expected VaR exceedances
- Actual VaR exceedances
- VaR failure rate
- Kupiec unconditional coverage test
- Conditional coverage test

Results are evaluated separately for:

- S&P 500
- FTSE 100
