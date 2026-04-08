# Full pipeline test script
# Run interactively to verify the complete data pipeline with real data.

# 1. Primary production & commodity balance sheets
prod <- build_primary_production(start_year = 1961)
cbs <- build_commodity_balances(prod, start_year = 1961)

# 2. Trade
trade <- build_detailed_trade(cbs = cbs, extend_time = FALSE)

# 3. Prices
trade_prices <- build_trade_prices()
primary_prices <- build_primary_prices(
  primary_prod = prod,
  trade_prices = trade_prices
)
cbs_prices <- build_cbs_prices(
  cbs = cbs,
  trade_prices = trade_prices
)
