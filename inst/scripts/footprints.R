# Supply-use tables (who produces and uses what)
supply_use <- build_supply_use()

# Bilateral trade matrices (who trades with whom)
bilateral_trade <- get_bilateral_trade()

# Commodity balance sheets (production, consumption, trade)
cbs <- get_wide_cbs()

# Build IO model for all years
io <- build_io_model(supply_use, bilateral_trade, cbs)


# --- Example: Build IO model for a single year ---

# Find available years
years <- sort(unique(supply_use$year))
example_year <- 2021

# Filter data to one year
su_yr <- dplyr::filter(supply_use, year == example_year)
btd_yr <- dplyr::filter(bilateral_trade, year == example_year)
cbs_yr <- dplyr::filter(cbs, year == example_year)

# Detect final demand columns
fd_cols <- c("food", "other_uses", "feed")[
  c("food", "other_uses", "feed") %in% names(cbs)
]

# Build IO for this one year (not exported, so need ::: to call private func)
# This would require: whep:::.build_io_year(su_yr, btd_yr, cbs_yr, fd_cols)
# But private functions aren't accessible; use build_io_model with subset instead:
io_single <- build_io_model(su_yr, btd_yr, cbs_yr)

# Extract matrices and labels
if (nrow(io_single) > 0) {
  z_mat <- io_single$Z[[1]]
  y_mat <- io_single$Y[[1]]
  x_vec <- io_single$X[[1]]
  labels <- io_single$labels[[1]]

  cat("IO model for year", example_year, "\n")
  cat("Dimensions: ", nrow(labels), "sectors\n")
  cat("Z matrix:", nrow(z_mat), "x", ncol(z_mat), "\n")
  cat("Y matrix:", nrow(y_mat), "x", ncol(y_mat), "\n")
  cat("X vector: length", length(x_vec), "\n")
}
