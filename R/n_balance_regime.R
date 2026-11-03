# Rainfed/irrigated split of the gridded nitrogen balance (issue #1233).
#
# Every grid row of the balance is one crop in one cell. It is split into a
# rainfed and an irrigated part with two shares per row:
# - `irrigated_area_share`: the irrigated fraction of the crop's cell area.
#   Area-proportional terms (BNF, manure, excreta and the other N inputs that
#   do not scale with the harvest, harvested area, grazed weeds, SOM
#   sequestration) split by it.
# - `irrigated_yield_share`: the irrigated fraction of the crop's cell
#   production, A_i * Y_i / P from split_regime_yield(). Synthetic N and
#   production N split by it, on the assumption that N availability per unit
#   of yield is the same in both regimes.
# The residue terms -- the residue and root N returned to soil (`recycling`)
# and the used, bedding and burnt residue N -- are evaluated per part
# instead: at each production item of the row, each part's residue and root
# are the package's residue model at the part's own yield and water regime
# (`.nb_item_part_factors()`, R/regime_residue.R), so the model's irrigation
# terms apply to the irrigated part, and the parts are summed to the row.
# The bedding the manure input brings back follows the same evaluation
# (`.nb_attach_regime_layer()`), so the bedded N the parts book as leaving the
# field is the bedded N that returns as manure. The carbon chain re-evaluates
# residue and root carbon the same way (`.ci_part_factors()`). A row with no
# share is booked wholly rainfed and stamped `no_regime_share`, never dropped
# and never split on a guess.

# Balance columns split by the production share: the synthetic input and
# the product.
.nb_regime_yield_cols <- function() {
  c("synthetic", "prod_n_t")
}

# Balance columns evaluated per part at the part's yield: the residue and
# root N returned to soil and the residue removals.
.nb_regime_residue_cols <- function() {
  c(
    "recycling",
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
    "manure_solid",
    "manure_liquid",
    "excreta",
    "deposition",
    "human",
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
# share, and the residue terms kept at the crop's per-hectare values) or
# "none" (the unsplit balance).
.nb_split_balance <- function(n_inputs, data, key, resolution, method) {
  x <- .nb_inputs(n_inputs, key) |>
    .nb_outputs(data, key)
  if (method == "none" || resolution != "grid") {
    return(list(x = x, n_inputs = n_inputs, key = key))
  }
  layer <- data$.regime_layer %||% .nb_regime_layer(data, key, method)
  if (is.null(layer$shares)) {
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
  shares <- layer$shares
  if (method == "area_split") {
    shares$irrigated_yield_share <- shares$irrigated_area_share
  }
  .nb_split_tables(x, n_inputs, shares, key, method, layer$residue)
}

# The shares of every grid key, and, where the parts can be re-evaluated
# (under "yield_split", from the production-item split), the per-part
# residue weights and each production item's bedding factor
# (.nb_bedding_factors()). Shares supplied as `data$regime_shares` carry no
# production item, so no part can be re-evaluated and the residue terms
# follow the production share (stamped "yield_share").
.nb_regime_layer <- function(data, key, method) {
  if (!is.null(data$regime_shares)) {
    return(list(shares = data$regime_shares, residue = NULL, bedding = NULL))
  }
  items <- .nb_regime_items(data)
  shares <- .nb_regime_shares(data, key, items)
  if (method != "yield_split" || is.null(shares)) {
    return(list(shares = shares, residue = NULL, bedding = NULL))
  }
  parts <- .nb_item_part_factors(items, .nb_residue_item_key())
  list(
    shares = shares,
    residue = .nb_residue_weights(parts, data, key),
    bedding = .nb_bedding_factors(parts)
  )
}

# The regime layer, built before the balance's own inputs when they bed any
# straw, so the manure input can bring back the bedding the parts book as
# leaving the field. Under "yield_split" each part's bedding removal is the
# residue model at the part's yield (.nb_residue_weights()), so the parts'
# bedding sums to the re-evaluated bedding, not the unsplit one; the manure
# input reads each production item's factor (`data$.regime_bedding_factors`,
# .ni_manure_bedding()) and returns that same mass. The split then reuses this
# layer (`data$.regime_layer`) rather than building it again. Nothing is
# attached when the split does not re-evaluate parts (another method or
# resolution, or shares supplied as `data$regime_shares`), when the inputs
# are the caller's (`data$n_inputs`) or when nothing is bedded.
.nb_attach_regime_layer <- function(data, key, resolution, method) {
  re_evaluated <- resolution == "grid" &&
    method == "yield_split" &&
    is.null(data$regime_shares)
  beds <- !is.null(data$residue_destiny_input) &&
    is.null(data$n_inputs) &&
    any(.nb_residue_destiny_args(data)$bedding_fraction > 0)
  if (!re_evaluated || !beds) {
    return(data)
  }
  data$.regime_layer <- .nb_regime_layer(data, key, method)
  data$.regime_bedding_factors <- data$.regime_layer$bedding
  data
}

# Apply the shares to both tables. Every numeric flow column of the wide
# table must have a rule, so a new column cannot pass through unsplit.
# `residue` holds the per-part weights of the residue terms
# (.nb_residue_weights()); without it they follow the yield share, which
# under "area_split" is the area share.
.nb_split_tables <- function(x, n_inputs, shares, key, method, residue = NULL) {
  yield_cols <- intersect(.nb_regime_yield_cols(), names(x))
  area_cols <- intersect(.nb_regime_area_cols(), names(x))
  residue_cols <- intersect(.nb_regime_residue_cols(), names(x))
  .nb_check_split_rules(x, key, c(yield_cols, area_cols, residue_cols))
  wide <- .nb_split_regime(
    x,
    shares,
    key,
    area_cols,
    yield_cols,
    residue = list(
      cols = rlang::set_names(residue_cols),
      weights = residue,
      stamp = if (method == "area_split") "crop_density" else "yield_share"
    )
  ) |>
    .nb_input_totals() |>
    dplyr::mutate(method_regime = method)
  synthetic <- n_inputs$fert_type == "synthetic"
  recycling <- n_inputs$fert_type == "recycling"
  long <- dplyr::bind_rows(
    .nb_split_regime(
      n_inputs[synthetic, ],
      shares,
      key,
      area_cols = character(),
      yield_cols = "n_input_t"
    ),
    .nb_split_regime(
      n_inputs[recycling, ],
      shares,
      key,
      area_cols = character(),
      residue = list(cols = c(n_input_t = "recycling"), weights = residue)
    ),
    .nb_split_regime(
      n_inputs[!synthetic & !recycling, ],
      shares,
      key,
      area_cols = "n_input_t"
    )
  ) |>
    dplyr::select(-dplyr::any_of("method_residue_regime"))
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
# yield bounds). `items` is the production-item split when already built.
.nb_regime_shares <- function(data, key, items = .nb_regime_items(data)) {
  if (is.null(items)) {
    return(NULL)
  }
  .nb_regime_shares_by_key(items, key)
}

# The regime split at the production-item grain, through the item split the
# carbon chain shares (`.ryr_item_split()`): one row per cell and production
# item, with the bounded `ratio` and the regime yields where it applies.
# NULL when there is no NPP table or the regime layer covers none of it.
.nb_regime_items <- function(data) {
  cells <- .nb_regime_cells(data)
  if (is.null(cells)) {
    return(NULL)
  }
  # A grid key the regime layer does not cover at all gets no share and is
  # booked wholly rainfed downstream, stamped `no_regime_share`. Within a key
  # it does cover, a production item it does not cover counts as rainfed, so
  # the covered items' split is not stretched over the whole key.
  has_areas <- !is.na(cells$rainfed_ha) & !is.na(cells$irrigated_ha)
  covered <- cells[has_areas, ]
  if (nrow(covered) == 0L) {
    return(NULL)
  }
  uncovered <- dplyr::mutate(
    cells[!has_areas, ],
    rainfed_ha = .data$area_ha,
    irrigated_ha = 0,
    yield_irrigated = NA_real_,
    covered_row = FALSE
  )
  # split_regime_yield() needs a production on every row; one the NPP chain
  # cannot express in fresh weight keeps its area and falls back to the area
  # share for its yield share.
  with_production <- !is.na(covered$production_t) & covered$production_t >= 0
  split <- .ryr_item_split(
    covered[with_production, ],
    data$regime_ratio,
    data$regime_production
  )
  dplyr::bind_rows(
    dplyr::mutate(split, covered_row = TRUE),
    dplyr::mutate(
      covered[!with_production, ],
      production_t = NA_real_,
      yield_irrigated = NA_real_,
      covered_row = TRUE
    ),
    uncovered
  )
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
    .sci_engine_crops(
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
  # Only numeric codes are production items; a text code would coerce to NA
  # and could then match an NPP row with a missing code.
  dm <- whep::whep_coef_table("bio_coefs") |>
    dplyr::filter(stringr::str_detect(.data$item_prod_code, "^[0-9]+$")) |>
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
      any_covered = any(.data$covered_row),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::filter(.data$any_covered) |>
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
# production share; the two parts of each of these sum to the original
# value. `residue`, when given, is a list: `cols`, the residue columns of
# `x`, named by column and valued by the residue term whose part weights
# they take (a long table's `n_input_t` takes the "recycling" weights);
# `weights`, the per-part weights of .nb_residue_weights() (NULL: the
# columns follow the production share); and `stamp`, the
# `method_residue_regime` of a row split by the production share. The parts
# of a residue column sum to its part evaluation, not to the unsplit value.
.nb_split_regime <- function(
  x,
  shares,
  key,
  area_cols,
  yield_cols = NULL,
  residue = NULL
) {
  .check_columns(
    shares,
    c(key, "irrigated_area_share", "irrigated_yield_share"),
    "shares"
  )
  .check_columns(x, c(key, area_cols, yield_cols, names(residue$cols)), "x")
  joined <- dplyr::left_join(
    x,
    dplyr::select(
      shares,
      dplyr::all_of(c(key, "irrigated_area_share", "irrigated_yield_share")),
      dplyr::any_of("method_regime_share")
    ),
    by = key,
    relationship = "many-to-one"
  ) |>
    .nb_fill_missing_share() |>
    .nb_join_residue_weights(residue$weights, key) |>
    .nb_check_residue_weights(residue, key)
  dplyr::bind_rows(
    .nb_regime_part(joined, "rainfed", area_cols, yield_cols, residue),
    .nb_regime_part(joined, "irrigated", area_cols, yield_cols, residue)
  ) |>
    dplyr::select(
      -"irrigated_area_share",
      -"irrigated_yield_share",
      -dplyr::any_of("method_regime_share"),
      -dplyr::starts_with(".rw_")
    )
}

# The per-part residue weights of each row's key, when there are any.
.nb_join_residue_weights <- function(joined, weights, key) {
  if (is.null(weights)) {
    return(joined)
  }
  dplyr::left_join(joined, weights, by = key, relationship = "many-to-one")
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
  stamp <- if (rlang::has_name(joined, "method_regime_share")) {
    joined$method_regime_share
  } else {
    NA_character_
  }
  dplyr::mutate(
    joined,
    # How each row's shares were obtained: "yield_ratio" or
    # "area_no_production" from .nb_regime_shares(), "regime_shares" for
    # shares supplied without that stamp, "no_regime_share" for a row booked
    # wholly rainfed.
    method_regime_split = dplyr::if_else(
      missing,
      "no_regime_share",
      dplyr::coalesce(.env$stamp, "regime_shares")
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
.nb_regime_part <- function(
  joined,
  regime,
  area_cols,
  yield_cols,
  residue = NULL
) {
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
    .nb_residue_part(residue, regime, yield_w) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(area_cols), \(v) v * area_w),
      dplyr::across(dplyr::all_of(yield_cols), \(v) v * yield_w),
      water_regime = regime
    )
}

# A residue column that is non-zero at a key the part weights do not cover
# has no part evaluation behind it. The weights and the balance's residue
# columns are built from the same NPP-N table and residue-destiny input, so
# on consistent inputs every non-zero residue value has a weight: a missing
# one means the two disagree (a caller-supplied `n_inputs` or a residue
# table keyed differently), a contract breach rather than a quantity, and no
# share is a defensible stand-in for the part evaluation. Without weights at
# all (shares supplied as `data$regime_shares`, or "area_split") the columns
# follow the production share by rule, and nothing is checked.
.nb_check_residue_weights <- function(joined, residue, key) {
  if (is.null(residue$weights) || length(residue$cols) == 0L) {
    return(joined)
  }
  unweighted <- purrr::imap(residue$cols, \(term, col) {
    weight_cols <- paste0(".rw_", term, "_", c("rainfed", "irrigated"))
    no_weight <- purrr::map(
      weight_cols,
      \(w) is.na(joined[[w]] %||% rep(NA_real_, nrow(joined)))
    ) |>
      purrr::reduce(`|`)
    no_weight & (joined[[col]] != 0) %in% TRUE
  }) |>
    purrr::reduce(`|`)
  if (any(unweighted)) {
    .nb_abort_residue_weight(joined[unweighted, ], residue, key)
  }
  joined
}

.nb_abort_residue_weight <- function(rows, residue, key) {
  examples <- rows |>
    dplyr::select(dplyr::all_of(key)) |>
    utils::head(3L) |>
    purrr::pmap_chr(\(...) paste(c(...), collapse = "/"))
  cli::cli_abort(
    c(
      "{nrow(rows)} row{?s} carr{?ies/y} residue N the part weights do not
       cover.",
      i = "The balance's residue columns ({.field {names(residue$cols)}})
           and the per-item residue evaluation (the NPP-N table and the
           residue-destiny input) disagree there.",
      i = "Keys as {paste(key, collapse = \"/\")}, first three:
           {.val {examples}}."
    ),
    class = "whep_regime_residue_weight"
  )
}

# One regime's part of the residue columns: each times its part weight
# (the part evaluation over the unsplit value at the row's key, checked
# present wherever the value is non-zero by .nb_check_residue_weights()). A
# key with no weight for a column holds zero there, which stays zero.
# Without weights the columns take the production share, stamped
# `residue$stamp`. `method_residue_regime` is the union of the key's part
# stamps, NA on a row with no residue term at all.
.nb_residue_part <- function(joined, residue, regime, yield_w) {
  cols <- residue$cols
  if (length(cols) == 0L) {
    return(joined)
  }
  has_residue <- rowSums(
    as.matrix(joined[names(cols)]) != 0,
    na.rm = TRUE
  ) >
    0
  scaled <- purrr::imap(cols, \(term, col) {
    w <- joined[[paste0(".rw_", term, "_", regime)]] %||%
      rep(NA_real_, nrow(joined))
    joined[[col]] * dplyr::coalesce(w, yield_w)
  })
  joined <- dplyr::mutate(joined, !!!scaled)
  if (is.null(residue$stamp)) {
    return(joined)
  }
  stamp <- if (is.null(residue$weights)) {
    dplyr::if_else(has_residue, residue$stamp, NA_character_)
  } else {
    joined[[paste0(".rw_stamp_", regime)]] %||%
      rep(NA_character_, nrow(joined))
  }
  dplyr::mutate(joined, method_residue_regime = .env$stamp)
}

# ---- Residue terms re-evaluated per part ------------------------------------

# The part weights of the residue terms at each balance key, wide: for every
# residue column c and regime r, `.rw_<c>_<r>` is the part's value from the
# per-item evaluation over the key's unsplit value (NA where that is zero),
# and `.rw_stamp_<r>` the union of its items' part stamps. The part value is
# the sum over the key's production items of each item's part: its area
# fraction times its residue (root) N per hectare times the part's residue
# (root) factor. `parts` is .nb_item_part_factors()' result.
.nb_residue_weights <- function(parts, data, key) {
  item_key <- .nb_residue_item_key()
  values <- .nb_item_residue_n(data, item_key)
  unsplit_items <- dplyr::anti_join(values, parts, by = item_key)
  parts <- dplyr::bind_rows(parts, .nb_rainfed_parts(unsplit_items, item_key))
  cols <- .nb_regime_residue_cols()
  part_values <- values |>
    dplyr::inner_join(parts, by = item_key, relationship = "one-to-many") |>
    dplyr::mutate(
      recycling = .data$area_frac *
        (.data$root_factor *
          .data$root_n +
          .data$residue_factor * .data$residue_n),
      dplyr::across(
        dplyr::all_of(setdiff(cols, "recycling")),
        \(v) v * .data$area_frac * .data$residue_factor
      )
    ) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), sum),
      .rw_stamp = .nb_method_stamp(
        .data$method_residue_regime[.data$area_frac > 0]
      ),
      .by = dplyr::all_of(c(key, "water_regime"))
    )
  unsplit <- values |>
    dplyr::mutate(recycling = .data$root_n + .data$residue_n) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols), sum),
      .by = dplyr::all_of(key)
    )
  .nb_residue_weight_table(part_values, unsplit, key, cols)
}

# The production-item grain the residue terms are re-evaluated at.
.nb_residue_item_key <- function() {
  c(.nb_regime_item_key(), "item_cbs_code")
}

# Each production item's bedding over both parts as a multiple of its
# unsplit bedding: the sum over its parts of the part's area fraction times
# its residue factor, the same product the parts' bedding removals carry
# (.nb_residue_weights()). 1 where no part is re-evaluated.
.nb_bedding_factors <- function(parts) {
  parts |>
    dplyr::summarise(
      bedding_factor = sum(.data$area_frac * .data$residue_factor),
      .by = dplyr::all_of(.nb_residue_item_key())
    )
}

# Part value over unsplit value, one column per residue term and regime,
# named `.rw_<term>_<regime>`, with the part stamps as `.rw_stamp_<regime>`.
.nb_residue_weight_table <- function(part_values, unsplit, key, cols) {
  joined <- dplyr::inner_join(
    part_values,
    unsplit,
    by = key,
    suffix = c("", ".unsplit"),
    relationship = "many-to-one"
  )
  ratios <- purrr::map(rlang::set_names(cols), \(col) {
    total <- joined[[paste0(col, ".unsplit")]]
    dplyr::if_else(total != 0, joined[[col]] / total, NA_real_)
  })
  joined <- dplyr::mutate(joined, !!!ratios)
  dplyr::full_join(
    .nb_regime_weight_cols(joined, "rainfed", key, cols),
    .nb_regime_weight_cols(joined, "irrigated", key, cols),
    by = key,
    relationship = "one-to-one"
  )
}

.nb_regime_weight_cols <- function(joined, regime, key, cols) {
  part <- joined[joined$water_regime == regime, c(key, cols, ".rw_stamp")]
  names(part) <- c(key, paste0(".rw_", c(cols, "stamp"), "_", regime))
  part
}

# The residue and root N of every production item of the balance, keyed by
# `item_key` on the regime layer's typed coordinates: the root N and the
# recycled residue N of the NPP-N table (the basis `.ni_recycling_basis()`
# picks) and the used, bedding and burnt residue N of the residue-destiny
# input, zero where an item is in one table and not the other.
.nb_item_residue_n <- function(data, item_key) {
  npp <- .n_balance_npp(data)
  basis <- .ni_recycling_basis(npp, data)
  removal_cols <- setdiff(.nb_regime_residue_cols(), "recycling")
  recycled <- npp |>
    .nb_round_cell_coords() |>
    dplyr::summarise(
      root_n = sum(.data$root_n_t, na.rm = TRUE),
      residue_n = sum(.data[[basis]], na.rm = TRUE),
      .by = dplyr::all_of(item_key)
    )
  if (is.null(data$residue_destiny_input)) {
    recycled[removal_cols] <- 0
    return(recycled)
  }
  removed <- .nb_residue_destiny_n(data, item_key) |>
    .nb_round_cell_coords()
  dplyr::full_join(
    recycled,
    removed,
    by = item_key,
    relationship = "one-to-one"
  ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("root_n", "residue_n", removal_cols)),
      \(v) dplyr::coalesce(v, 0)
    ))
}

# The two parts of every production item of the regime split, long: the
# part's area fraction, its residue and root factors and how they were set
# (`method_residue_regime`). A part is re-evaluated where the item's split
# knows its yield (.rr_reevaluated_rules(), a covered row with a production
# and an area) and the part has area; every other part keeps factor 1, the
# crop's per-hectare residue and root on its share of the area.
.nb_item_part_factors <- function(items, item_key) {
  area <- items$rainfed_ha + items$irrigated_ha
  share <- dplyr::if_else(area > 0, items$irrigated_ha / area, 0)
  yield_mean <- dplyr::if_else(area > 0, items$production_t / area, NA_real_)
  reevaluate <- items$covered_row %in%
    TRUE &
    items$method_regime_split %in% .rr_reevaluated_rules() &
    !is.na(yield_mean) &
    !is.na(items$ratio)
  yields <- .rr_part_yields(yield_mean, share, items$ratio)
  base <- dplyr::select(items, dplyr::all_of(item_key))
  parts <- dplyr::bind_rows(
    dplyr::mutate(
      base,
      water_regime = "rainfed",
      area_frac = 1 - .env$share,
      yield_mean = .env$yield_mean,
      yield_part = .env$yields$rainfed,
      reevaluate = .env$reevaluate & .env$share < 1
    ),
    dplyr::mutate(
      base,
      water_regime = "irrigated",
      area_frac = .env$share,
      yield_mean = .env$yield_mean,
      yield_part = .env$yields$irrigated,
      reevaluate = .env$reevaluate & .env$share > 0
    )
  )
  .nb_evaluate_parts(parts)
}

# The residue and root factors of the parts flagged `reevaluate`, 1 on the
# others, and the part stamps.
.nb_evaluate_parts <- function(parts) {
  residue_factor <- rep(1, nrow(parts))
  root_factor <- rep(1, nrow(parts))
  todo <- parts$reevaluate
  if (any(todo)) {
    evaluated <- .rr_part_factors(parts[todo, ])
    residue_factor[todo] <- evaluated$residue_factor
    root_factor[todo] <- evaluated$root_factor
  }
  parts |>
    dplyr::mutate(
      residue_factor = .env$residue_factor,
      root_factor = .env$root_factor,
      method_residue_regime = .rr_residue_stamp(
        .data$reevaluate,
        .data$water_regime == "irrigated"
      )
    ) |>
    dplyr::select(
      -"yield_mean",
      -"yield_part",
      -"reevaluate"
    )
}

# Items with residue N but no row in the regime split (in the residue-destiny
# input and not in the NPP-N table) are booked wholly rainfed at the crop's
# per-hectare values, as an item the regime layer does not cover is.
.nb_rainfed_parts <- function(values, item_key) {
  base <- dplyr::select(values, dplyr::all_of(item_key))
  dplyr::bind_rows(
    dplyr::mutate(base, water_regime = "rainfed", area_frac = 1),
    dplyr::mutate(base, water_regime = "irrigated", area_frac = 0)
  ) |>
    dplyr::mutate(
      residue_factor = 1,
      root_factor = 1,
      method_residue_regime = "crop_density"
    )
}
