# Rainfed/irrigated split of the gridded nitrogen balance (issue #1233).
#
# Every grid row of the balance is one crop in one cell. It is split into a
# rainfed and an irrigated part with two shares per row:
# - `irrigated_area_share`: the irrigated fraction of the crop's cell area.
#   Area-proportional terms (every N input except synthetic fertiliser,
#   harvested area, grazed weeds, SOM sequestration) split by it.
# - `irrigated_yield_share`: the irrigated fraction of the crop's cell
#   production, A_i * Y_i / P from split_regime_yield(). Synthetic N,
#   production N and harvested residue N split by it, on the assumption that
#   N availability per unit of yield is the same in both regimes.
# A row with no share is booked wholly rainfed and stamped
# `no_regime_share`, never dropped and never split on a guess.

# Split the numeric columns of `x` into rainfed and irrigated rows. Columns in
# `area_cols` split by the area share, columns in `yield_cols` by the
# production share. The two parts of every column sum to the original value.
.nb_split_regime <- function(x, shares, key, area_cols, yield_cols = NULL) {
  .check_columns(
    shares,
    c(key, "irrigated_area_share", "irrigated_yield_share"),
    "shares"
  )
  .check_columns(x, c(key, area_cols, yield_cols), "x")
  joined <- dplyr::left_join(
    x,
    dplyr::select(
      shares,
      dplyr::all_of(c(key, "irrigated_area_share", "irrigated_yield_share"))
    ),
    by = key,
    relationship = "many-to-one"
  ) |>
    .nb_fill_missing_share()
  dplyr::bind_rows(
    .nb_regime_part(joined, "rainfed", area_cols, yield_cols),
    .nb_regime_part(joined, "irrigated", area_cols, yield_cols)
  ) |>
    dplyr::select(-"irrigated_area_share", -"irrigated_yield_share")
}

# A row with no share is booked wholly rainfed and says so. Shares outside
# [0, 1] are a contract violation, not a quantity, so they abort.
.nb_fill_missing_share <- function(joined) {
  bad <- (joined$irrigated_area_share < 0 |
    joined$irrigated_area_share > 1 |
    joined$irrigated_yield_share < 0 |
    joined$irrigated_yield_share > 1) %in%
    TRUE
  if (any(bad)) {
    cli::cli_abort(
      "{sum(bad)} row{?s} carr{?ies/y} a regime share outside [0, 1].",
      class = "whep_regime_share_range"
    )
  }
  missing <- is.na(joined$irrigated_area_share) |
    is.na(joined$irrigated_yield_share)
  dplyr::mutate(
    joined,
    method_regime_split = dplyr::if_else(
      missing,
      "no_regime_share",
      "regime_shares"
    ),
    irrigated_area_share = dplyr::if_else(
      missing,
      0,
      .data$irrigated_area_share
    ),
    irrigated_yield_share = dplyr::if_else(
      missing,
      0,
      .data$irrigated_yield_share
    )
  )
}

# One regime's part of every row: the irrigated part takes the share, the
# rainfed part its complement.
.nb_regime_part <- function(joined, regime, area_cols, yield_cols) {
  irrigated <- regime == "irrigated"
  area_w <- if (irrigated) {
    joined$irrigated_area_share
  } else {
    1 - joined$irrigated_area_share
  }
  yield_w <- if (irrigated) {
    joined$irrigated_yield_share
  } else {
    1 - joined$irrigated_yield_share
  }
  joined |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(area_cols), \(v) v * area_w),
      dplyr::across(dplyr::all_of(yield_cols), \(v) v * yield_w),
      water_regime = regime
    )
}
