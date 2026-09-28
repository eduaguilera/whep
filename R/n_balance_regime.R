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

# Balance columns split by the production share: the synthetic input and the
# terms that scale with the harvest (product and the residue destinies).
.nb_regime_yield_cols <- function() {
  c(
    "synthetic",
    "prod_n_t",
    "used_residue_n_t",
    "bedding_residue_n_t",
    "burnt_residue_n_t"
  )
}

# Balance columns split by the area share: every other N input, the
# harvested area and the land-based terms.
.nb_regime_area_cols <- function() {
  c(
    "bnf",
    "recycling",
    "manure_solid",
    "manure_liquid",
    "excreta",
    "deposition",
    "urban",
    "som_mineralization",
    "area_ha",
    "grazed_weeds_n_t",
    "som_sequestration_n_t"
  )
}

# Build the unsplit inputs and outputs, then split them into rainfed and
# irrigated rows at grid resolution. Returns the wide table, the long inputs
# the losses are computed from, and the key both now carry. `method` is
# "yield_split", "area_split" (the production share replaced by the area
# share) or "none" (the unsplit balance).
.nb_split_balance <- function(n_inputs, data, key, resolution, method) {
  x <- .nb_inputs(n_inputs, key) |>
    .nb_outputs(data, key)
  if (method == "none" || resolution != "grid") {
    return(list(x = x, n_inputs = n_inputs, key = key))
  }
  shares <- data$regime_shares %||% .nb_regime_shares(data, key)
  if (is.null(shares)) {
    cli::cli_abort(
      c(
        "The rainfed/irrigated split needs the NPP-N table to build its
         shares.",
        i = "Supply {.code data$npp_n_input} or {.code data$regime_shares},
             or select {.code methods$regime = \"none\"}."
      ),
      class = "whep_regime_shares_missing"
    )
  }
  if (method == "area_split") {
    shares$irrigated_yield_share <- shares$irrigated_area_share
  }
  .nb_split_tables(x, n_inputs, shares, key, method)
}

# Apply the shares to both tables. Every numeric flow column of the wide
# table must have a rule, so a new column cannot pass through unsplit.
.nb_split_tables <- function(x, n_inputs, shares, key, method) {
  yield_cols <- intersect(.nb_regime_yield_cols(), names(x))
  area_cols <- intersect(.nb_regime_area_cols(), names(x))
  .nb_check_split_rules(x, key, c(yield_cols, area_cols))
  wide <- .nb_split_regime(x, shares, key, area_cols, yield_cols) |>
    .nb_input_totals() |>
    dplyr::mutate(method_regime = method)
  synthetic <- n_inputs$fert_type == "synthetic"
  long <- dplyr::bind_rows(
    .nb_split_regime(
      n_inputs[synthetic, ],
      shares,
      key,
      area_cols = character(),
      yield_cols = "n_input_t"
    ),
    .nb_split_regime(
      n_inputs[!synthetic, ],
      shares,
      key,
      area_cols = "n_input_t"
    )
  )
  list(x = wide, n_inputs = long, key = c(key, "water_regime"))
}

# The input totals are recomputed after the split, so they need no rule;
# anything else numeric without one is a contract violation.
.nb_check_split_rules <- function(x, key, ruled) {
  totals <- c(
    "n_input_full_t",
    "n_input_full_nosom_t",
    "n_input_std_t",
    "n_input_som_t",
    "n_input_for_n2o_t"
  )
  numeric_cols <- names(x)[purrr::map_lgl(x, is.numeric)]
  unruled <- setdiff(numeric_cols, c(key, ruled, totals))
  if (length(unruled) > 0L) {
    cli::cli_abort(
      "No rainfed/irrigated split rule for column{?s} {.field {unruled}}.",
      class = "whep_regime_split_rule"
    )
  }
  invisible(x)
}

# A driver table without `water_regime` applies to both regimes, so it joins
# on the key without it; one that carries it joins regime by regime.
.nb_driver_key <- function(key, drivers) {
  if (rlang::has_name(drivers, "water_regime")) {
    key
  } else {
    setdiff(key, "water_regime")
  }
}

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
  # Only cell-crops the regime layer covers get a share; the rest are booked
  # wholly rainfed downstream, stamped `no_regime_share`.
  covered <- dplyr::filter(
    cells,
    !is.na(.data$rainfed_ha),
    !is.na(.data$irrigated_ha)
  )
  if (nrow(covered) == 0L) {
    return(NULL)
  }
  # split_regime_yield() needs a production on every row; one the NPP chain
  # cannot express in fresh weight keeps its area and falls back to the area
  # share for its yield share.
  with_production <- !is.na(covered$production_t) & covered$production_t >= 0
  ratio <- data$regime_ratio %||%
    build_regime_yield_ratio(cells = covered[with_production, ])
  split <- covered[with_production, ] |>
    dplyr::left_join(
      dplyr::select(
        ratio,
        dplyr::all_of(c(.nb_regime_item_key(), "ratio_unbounded"))
      ),
      by = .nb_regime_item_key(),
      relationship = "many-to-one"
    ) |>
    split_regime_yield(production = data$regime_production)
  dplyr::bind_rows(
    split,
    dplyr::mutate(
      covered[!with_production, ],
      production_t = NA_real_,
      yield_irrigated = NA_real_
    )
  ) |>
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
    .nb_fresh_production() |>
    dplyr::summarise(
      production_t = sum(.data$production_t),
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

# The NPP chain carries the harvested product as dry matter; the yield bounds
# of split_regime_yield() are FAOSTAT fresh-weight yields. Fresh production is
# recovered with the same product dry-matter fraction the NPP chain used to
# go the other way (`bio_coefs`' `product_dm_kgfm`, see
# .residue_dm_conversions()). A table that already carries `production_t`
# keeps it; an item with no fraction keeps NA, so its split falls back to
# the area share rather than a guessed yield.
.nb_fresh_production <- function(npp) {
  if (rlang::has_name(npp, "production_t")) {
    return(npp)
  }
  dm <- whep::whep_coef_table("bio_coefs") |>
    dplyr::transmute(
      item_prod_code = as.integer(.data$item_prod_code),
      product_dm_kgfm = as.numeric(.data$product_dm_kgfm)
    ) |>
    dplyr::distinct(.data$item_prod_code, .keep_all = TRUE)
  npp |>
    dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code)) |>
    dplyr::left_join(dm, by = "item_prod_code", relationship = "many-to-one") |>
    dplyr::mutate(
      production_t = dplyr::if_else(
        .data$product_dm_kgfm > 0,
        .data$product_dm_t / .data$product_dm_kgfm,
        NA_real_
      )
    ) |>
    dplyr::select(-"product_dm_kgfm")
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
      production_known = !anyNA(.data$production_t) &
        !anyNA(.data$yield_irrigated[.data$irrigated_ha > 0]),
      production_total = sum(.data$production_t, na.rm = TRUE),
      production_irrigated = sum(.data$irrigated_production_t, na.rm = TRUE),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(
      by_yield = .data$production_known & .data$production_total > 0,
      irrigated_area_share = dplyr::if_else(
        .data$area_total > 0,
        .data$area_irrigated / .data$area_total,
        NA_real_
      ),
      irrigated_yield_share = dplyr::if_else(
        .data$by_yield,
        .data$production_irrigated / .data$production_total,
        .data$irrigated_area_share
      ),
      method_regime_share = dplyr::if_else(
        .data$by_yield,
        "yield_ratio",
        "area_no_production"
      ),
      irrigated_area_share = .nb_snap_share(.data$irrigated_area_share),
      irrigated_yield_share = .nb_snap_share(.data$irrigated_yield_share)
    ) |>
    dplyr::select(
      dplyr::all_of(key),
      "irrigated_area_share",
      "irrigated_yield_share",
      "method_regime_share"
    )
}

# A share is a ratio of sums whose numerator is part of its denominator, so
# it lies in [0, 1] exactly; only floating-point residue (a wholly irrigated
# cell-crop at 1 + 2e-16) can put it outside. That residue, within 1e-9, is
# snapped to the bound. Anything further out is left for
# .nb_fill_missing_share() to refuse.
.nb_snap_share <- function(share) {
  tol <- 1e-9
  dplyr::case_when(
    share < 0 & share >= -tol ~ 0,
    share > 1 & share <= 1 + tol ~ 1,
    .default = share
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
    worst <- max(
      abs(c(joined$irrigated_area_share, joined$irrigated_yield_share) - 0.5),
      na.rm = TRUE
    ) -
      0.5
    cli::cli_abort(
      c(
        "{sum(bad)} row{?s} carr{?ies/y} a regime share outside [0, 1].",
        i = "The furthest lies {signif(worst, 3)} beyond the interval."
      ),
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
