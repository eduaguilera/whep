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

# The two shares per grid key (`lon`, `lat`, `area_code`, `item_cbs_code`,
# `year`). Built at the production-item grain, where the regime areas and the
# yield ratio live, then summed to the balance's CBS item:
# - area share: irrigated hectares over all hectares of the item's cell rows;
# - yield share: irrigated production over all production, the irrigated
#   production being A_i * Y_i from split_regime_yield(). Where production is
#   zero the yield share falls back to the area share, stamped.
# `data` members, each read or built when absent: `.npp_cache` (the NPP-N
# table with `production_t` and `area_ha` per cell and production item),
# `regime_areas` (`lon`, `lat`, `area_code`, `item_prod_code`, `year`,
# `rainfed_ha`, `irrigated_ha`), `regime_ratio` (build_regime_yield_ratio()
# output) and `regime_production` (get_primary_production() output, for the
# yield bounds).
.nb_regime_shares <- function(data, key) {
  cells <- .nb_regime_cells(data)
  if (is.null(cells)) {
    return(NULL)
  }
  ratio <- data$regime_ratio %||% build_regime_yield_ratio(cells = cells)
  cells |>
    dplyr::left_join(
      dplyr::select(
        ratio,
        dplyr::all_of(c(.nb_regime_item_key(), "ratio_unbounded"))
      ),
      by = .nb_regime_item_key(),
      relationship = "many-to-one"
    ) |>
    split_regime_yield(production = data$regime_production) |>
    .nb_regime_shares_by_key(key)
}

# The production-item grain the regime layer is keyed on.
.nb_regime_item_key <- function() {
  c("lon", "lat", "area_code", "item_prod_code", "year")
}

# NPP rows (production and the N chain's own area) joined to the regime
# areas, whose split is rescaled onto the N chain's area so both chains
# agree on each cell-crop's hectares.
.nb_regime_cells <- function(data) {
  npp <- .n_balance_npp(data)
  if (is.null(npp)) {
    return(NULL)
  }
  areas <- data$regime_areas %||%
    .spatialized_regime_areas(
      sort(unique(npp$year)),
      data$country_grid %||% .sci_read_country_grid()
    )
  item_key <- .nb_regime_item_key()
  npp |>
    dplyr::summarise(
      production_t = sum(.data$production_t, na.rm = TRUE),
      area_ha = sum(.data$area_ha, na.rm = TRUE),
      .by = dplyr::all_of(c(item_key, "item_cbs_code"))
    ) |>
    .nb_round_cell_coords() |>
    dplyr::left_join(
      .nb_round_cell_coords(areas) |>
        dplyr::summarise(
          rainfed_share_ha = sum(.data$rainfed_ha, na.rm = TRUE),
          irrigated_share_ha = sum(.data$irrigated_ha, na.rm = TRUE),
          .by = dplyr::all_of(item_key)
        ),
      by = item_key,
      relationship = "many-to-one"
    ) |>
    .nb_rescale_regime_area()
}

# Cell centres and item codes are normalised the way
# build_regime_yield_ratio() normalises its keys (two-decimal centres,
# integer codes), so the ratio joins back. Centres sit on .25/.75, which are
# exact in binary, so the rounding never moves a grid key.
.nb_round_cell_coords <- function(x) {
  x |>
    dplyr::mutate(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year)
    )
}

# The regime layer supplies the irrigated fraction; the N chain supplies the
# hectares. A cell-crop the regime layer does not cover keeps NA areas, so
# split_regime_yield() reports it as `no_area` rather than a guessed split.
.nb_rescale_regime_area <- function(cells) {
  total <- cells$rainfed_share_ha + cells$irrigated_share_ha
  irrigated_frac <- dplyr::if_else(
    total > 0,
    cells$irrigated_share_ha / total,
    NA_real_
  )
  cells |>
    dplyr::mutate(
      irrigated_ha = .data$area_ha * .env$irrigated_frac,
      rainfed_ha = .data$area_ha - .data$irrigated_ha
    ) |>
    dplyr::select(-"rainfed_share_ha", -"irrigated_share_ha")
}

# Sum the production-item split to the balance key and turn it into shares.
.nb_regime_shares_by_key <- function(split, key) {
  split |>
    dplyr::mutate(
      irrigated_production_t = dplyr::coalesce(
        .data$irrigated_ha * .data$yield_irrigated,
        0
      )
    ) |>
    dplyr::summarise(
      area_total = sum(.data$rainfed_ha + .data$irrigated_ha, na.rm = TRUE),
      area_irrigated = sum(.data$irrigated_ha, na.rm = TRUE),
      production_total = sum(.data$production_t, na.rm = TRUE),
      production_irrigated = sum(.data$irrigated_production_t, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(
      irrigated_area_share = dplyr::if_else(
        .data$area_total > 0,
        .data$area_irrigated / .data$area_total,
        NA_real_
      ),
      irrigated_yield_share = dplyr::if_else(
        .data$production_total > 0,
        .data$production_irrigated / .data$production_total,
        .data$irrigated_area_share
      ),
      method_regime_share = dplyr::if_else(
        .data$production_total > 0,
        "yield_ratio",
        "area_no_production"
      )
    ) |>
    dplyr::select(
      dplyr::all_of(key),
      "irrigated_area_share",
      "irrigated_yield_share",
      "method_regime_share"
    )
}

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
