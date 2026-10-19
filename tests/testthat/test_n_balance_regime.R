# Tests for R/n_balance_regime.R: the rainfed/irrigated row split.

.regime_key <- c("lon", "lat", "area_code", "item_cbs_code", "year")

.regime_rows <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_cbs_code, ~year, ~synthetic, ~bnf, ~prod_n_t,
    0.25, 40.25,        203,           2511,  2010,        100,   10,        80,
    0.75, 40.25,        203,           2511,  2010,         50,    0,        20,
    1.25, 40.25,        203,           2514,  2010,         30,    6,        12
  )
}

.regime_shares <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_cbs_code, ~year,
    ~irrigated_area_share, ~irrigated_yield_share,
    0.25, 40.25,        203,           2511,  2010, 0.2, 0.5,
    0.75, 40.25,        203,           2511,  2010, 0.0, 0.0
  )
}

testthat::test_that(".nb_split_regime conserves every split column", {
  x <- .regime_rows()
  out <- whep:::.nb_split_regime(
    x,
    .regime_shares(),
    .regime_key,
    area_cols = "bnf",
    yield_cols = c("synthetic", "prod_n_t")
  )
  back <- out |>
    dplyr::summarise(
      dplyr::across(c("synthetic", "bnf", "prod_n_t"), sum),
      .by = dplyr::all_of(.regime_key)
    ) |>
    dplyr::arrange(.data$lon)
  testthat::expect_equal(back$synthetic, x$synthetic)
  testthat::expect_equal(back$bnf, x$bnf)
  testthat::expect_equal(back$prod_n_t, x$prod_n_t)
  pointblank::expect_col_vals_in_set(
    out,
    "water_regime",
    c("rainfed", "irrigated")
  )
})

testthat::test_that(".nb_split_regime weights yield and area columns apart", {
  out <- whep:::.nb_split_regime(
    .regime_rows(),
    .regime_shares(),
    .regime_key,
    area_cols = "bnf",
    yield_cols = c("synthetic", "prod_n_t")
  )
  irrigated <- out |>
    dplyr::filter(.data$lon == 0.25, .data$water_regime == "irrigated")
  testthat::expect_equal(irrigated$synthetic, 100 * 0.5)
  testthat::expect_equal(irrigated$prod_n_t, 80 * 0.5)
  testthat::expect_equal(irrigated$bnf, 10 * 0.2)
})

testthat::test_that(".nb_split_regime books a row with no share as rainfed", {
  out <- whep:::.nb_split_regime(
    .regime_rows(),
    .regime_shares(),
    .regime_key,
    area_cols = "bnf",
    yield_cols = c("synthetic", "prod_n_t")
  )
  unshared <- dplyr::filter(out, .data$item_cbs_code == 2514)
  testthat::expect_equal(
    unique(unshared$method_regime_split),
    "no_regime_share"
  )
  testthat::expect_equal(
    unshared$synthetic[unshared$water_regime == "rainfed"],
    30
  )
  testthat::expect_equal(
    unshared$synthetic[unshared$water_regime == "irrigated"],
    0
  )
  shared <- dplyr::filter(out, .data$item_cbs_code == 2511)
  testthat::expect_equal(
    unique(shared$method_regime_split),
    "regime_shares"
  )
})

testthat::test_that(".nb_split_regime aborts on a share outside [0, 1]", {
  shares <- dplyr::mutate(.regime_shares(), irrigated_yield_share = 1.2)
  testthat::expect_error(
    whep:::.nb_split_regime(
      .regime_rows(),
      shares,
      .regime_key,
      area_cols = "bnf",
      yield_cols = "synthetic"
    ),
    class = "whep_regime_share_range"
  )
})

.regime_share_data <- function() {
  npp <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_prod_code, ~item_cbs_code, ~year,
    ~production_t, ~area_ha,
    0.25, 40.25,        203,              15,           2511,  2010, 100, 40,
    0.75, 40.25,        203,              15,           2511,  2010,   0, 10
  )
  areas <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_prod_code, ~year, ~rainfed_ha, ~irrigated_ha,
    0.25, 40.25,        203,              15,  2010,          60,           20,
    0.75, 40.25,        203,              15,  2010,           5,            5
  )
  ratio <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_prod_code, ~year, ~ratio_unbounded,
    0.25, 40.25,        203,              15,  2010,                2,
    0.75, 40.25,        203,              15,  2010,                2
  )
  # National yields spanning 1 to 11 t/ha, so neither bound binds at 2 or 4.
  production <- tidyr::expand_grid(
    year = 2000:2010,
    unit = c("tonnes", "ha")
  ) |>
    dplyr::mutate(
      area_code = 203L,
      item_prod_code = 15L,
      value = dplyr::if_else(.data$unit == "ha", 1, .data$year - 1999)
    )
  list(
    .npp_cache = npp,
    regime_areas = areas,
    regime_ratio = ratio,
    regime_production = production
  )
}

testthat::test_that(".nb_regime_shares turns the yield split into shares", {
  shares <- whep:::.nb_regime_shares(.regime_share_data(), .regime_key)
  first <- dplyr::filter(shares, .data$lon == 0.25)
  # 40 ha, 25% irrigated, R = 2: Y_r = 100 / (30 + 2 * 10) = 2, Y_i = 4, so
  # irrigated production is 10 * 4 = 40 of 100.
  testthat::expect_equal(first$irrigated_area_share, 0.25)
  testthat::expect_equal(first$irrigated_yield_share, 0.4)
  testthat::expect_equal(first$method_regime_share, "yield_ratio")
})

testthat::test_that(".nb_regime_shares falls back to area without production", {
  shares <- whep:::.nb_regime_shares(.regime_share_data(), .regime_key)
  second <- dplyr::filter(shares, .data$lon == 0.75)
  testthat::expect_equal(second$irrigated_area_share, 0.5)
  testthat::expect_equal(second$irrigated_yield_share, 0.5)
  testthat::expect_equal(second$method_regime_share, "area_no_production")
})

testthat::test_that(".nb_regime_shares returns NULL without an NPP table", {
  testthat::expect_null(whep:::.nb_regime_shares(list(), .regime_key))
})

testthat::test_that(".nb_split_regime aborts on a missing share column", {
  shares <- dplyr::select(.regime_shares(), -"irrigated_yield_share")
  testthat::expect_error(
    whep:::.nb_split_regime(
      .regime_rows(),
      shares,
      .regime_key,
      area_cols = "bnf"
    )
  )
})

testthat::test_that(".nb_check_split_rules aborts on a flow with no rule", {
  x <- dplyr::mutate(.regime_rows(), mystery_n_t = 1)
  testthat::expect_error(
    whep:::.nb_check_split_rules(x, .regime_key, c("synthetic", "bnf")),
    class = "whep_regime_split_rule"
  )
})

testthat::test_that(".nb_driver_key keeps water_regime only when drivers carry it", {
  key <- c(.regime_key, "water_regime")
  testthat::expect_equal(
    whep:::.nb_driver_key(key, tibble::tibble(lon = 1)),
    .regime_key
  )
  testthat::expect_equal(
    whep:::.nb_driver_key(key, tibble::tibble(water_regime = "rainfed")),
    key
  )
})

testthat::test_that(".nb_fresh_production converts dry matter to fresh weight", {
  npp <- tibble::tibble(item_prod_code = 15L, product_dm_t = 87, area_ha = 1)
  dm <- whep::whep_coef_table("bio_coefs") |>
    dplyr::filter(as.character(.data$item_prod_code) == "15") |>
    dplyr::pull("product_dm_kgfm")
  out <- whep:::.nb_fresh_production(npp)
  testthat::expect_equal(out$production_t, 87 / as.numeric(dm[[1]]))
})

testthat::test_that(".nb_regime_shares handles missing production and uncovered cells", {
  data <- .regime_share_data()
  # One cell-crop without a fresh-weight production, one outside the regime
  # layer: the first keeps an area share, the second gets no share at all.
  data$.npp_cache <- dplyr::bind_rows(
    data$.npp_cache,
    tibble::tribble(
      ~lon, ~lat, ~area_code, ~item_prod_code, ~item_cbs_code, ~year,
      ~production_t, ~area_ha,
      1.25, 40.25, 203, 15, 2511, 2010, NA, 20,
      1.75, 40.25, 203, 15, 2511, 2010, 50, 20
    )
  )
  data$regime_areas <- dplyr::bind_rows(
    data$regime_areas,
    tibble::tribble(
      ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~rainfed_ha,
      ~irrigated_ha,
      1.25, 40.25, 203, 15, 2010, 10, 10
    )
  )
  shares <- whep:::.nb_regime_shares(data, .regime_key)
  no_production <- dplyr::filter(shares, .data$lon == 1.25)
  testthat::expect_equal(no_production$irrigated_area_share, 0.5)
  testthat::expect_equal(no_production$irrigated_yield_share, 0.5)
  testthat::expect_equal(
    no_production$method_regime_share,
    "area_no_production"
  )
  testthat::expect_false(any(shares$lon == 1.75))
})

testthat::test_that(".nb_snap_share snaps only floating-point residue", {
  testthat::expect_equal(
    whep:::.nb_snap_share(c(1 + 2e-16, -1e-12, 0.4, 1.2, -0.1)),
    c(1, 0, 0.4, 1.2, -0.1)
  )
})

testthat::test_that(".nb_regime_shares counts an uncovered item as rainfed within its key", {
  data <- .regime_share_data()
  # A second production item under the same CBS item and cell, outside the
  # regime layer: its 40 ha and 100 t count as rainfed, halving the shares.
  data$.npp_cache <- dplyr::bind_rows(
    data$.npp_cache,
    tibble::tribble(
      ~lon, ~lat, ~area_code, ~item_prod_code, ~item_cbs_code, ~year,
      ~production_t, ~area_ha,
      0.25, 40.25, 203, 44, 2511, 2010, 100, 40
    )
  )
  shares <- whep:::.nb_regime_shares(data, .regime_key)
  first <- dplyr::filter(shares, .data$lon == 0.25)
  testthat::expect_equal(first$irrigated_area_share, 10 / 80)
  testthat::expect_equal(first$irrigated_yield_share, 40 / 200)
})

testthat::test_that("the split carries how each row's shares were obtained", {
  shares <- dplyr::mutate(
    .regime_shares(),
    method_regime_share = c("yield_ratio", "area_no_production")
  )
  out <- whep:::.nb_split_regime(
    .regime_rows(),
    shares,
    .regime_key,
    area_cols = "bnf",
    yield_cols = "synthetic"
  )
  stamps <- dplyr::distinct(out, .data$lon, .data$method_regime_split)
  testthat::expect_equal(
    stamps$method_regime_split[order(stamps$lon)],
    c("yield_ratio", "area_no_production", "no_regime_share")
  )
  testthat::expect_false(rlang::has_name(out, "method_regime_share"))
})

# ---- Residue N returned to soil follows the yield share ---------------------

# The wide balance rows with every input column, so the totals can be
# recomputed after the split, and the long inputs they were pivoted from.
.regime_wide <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_cbs_code, ~year, ~synthetic, ~recycling,
    ~bnf, ~prod_n_t, ~area_ha,
    0.25, 40.25,        203,           2511,  2010,        100,         40,
    10,        80,       50,
    0.75, 40.25,        203,           2511,  2010,         50,         20,
    0,        20,       30
  ) |>
    whep:::.nb_ensure_fert_cols() |>
    whep:::.nb_input_totals()
}

.regime_long <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~area_code, ~item_cbs_code, ~year, ~fert_type,  ~n_input_t,
    0.25, 40.25,        203,           2511,  2010, "synthetic",        100,
    0.25, 40.25,        203,           2511,  2010, "recycling",         40,
    0.25, 40.25,        203,           2511,  2010, "bnf",               10,
    0.75, 40.25,        203,           2511,  2010, "synthetic",         50,
    0.75, 40.25,        203,           2511,  2010, "recycling",         20
  )
}

.regime_split_tables <- function() {
  whep:::.nb_split_tables(
    .regime_wide(),
    .regime_long(),
    .regime_shares(),
    .regime_key,
    "yield_split"
  )
}

testthat::test_that("with supplied shares, residue N follows the yield share, stamped", {
  # Shares supplied as a table carry no production item, so no part can be
  # re-evaluated: the residue terms follow the production share and say so.
  out <- .regime_split_tables()
  testthat::expect_identical(
    unique(out$x$method_residue_regime),
    "yield_share"
  )
  wide <- dplyr::filter(out$x, .data$lon == 0.25)
  irrigated <- wide[wide$water_regime == "irrigated", ]
  # Yield share 0.5, area share 0.2 at this key.
  testthat::expect_equal(irrigated$recycling, 40 * 0.5)
  testthat::expect_equal(irrigated$synthetic, 100 * 0.5)
  testthat::expect_equal(irrigated$bnf, 10 * 0.2)
  testthat::expect_equal(irrigated$area_ha, 50 * 0.2)
  # The N2O input total carries the recycling term, so it is recomputed from
  # the split columns.
  testthat::expect_equal(irrigated$n_input_for_n2o_t, 100 * 0.5 + 40 * 0.5)
  long <- dplyr::filter(
    out$n_inputs,
    .data$lon == 0.25,
    .data$water_regime == "irrigated"
  )
  testthat::expect_equal(
    long$n_input_t[long$fert_type == "recycling"],
    40 * 0.5
  )
  testthat::expect_equal(long$n_input_t[long$fert_type == "bnf"], 10 * 0.2)
  testthat::expect_true("recycling" %in% whep:::.nb_regime_residue_cols())
  testthat::expect_false("recycling" %in% whep:::.nb_regime_yield_cols())
  testthat::expect_false("recycling" %in% whep:::.nb_regime_area_cols())
})

testthat::test_that("every split column sums back over water_regime", {
  out <- .regime_split_tables()
  wide <- .regime_wide()
  value_cols <- setdiff(
    names(wide)[purrr::map_lgl(wide, is.numeric)],
    .regime_key
  )
  back <- out$x |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), sum),
      .by = dplyr::all_of(.regime_key)
    ) |>
    dplyr::arrange(.data$lon)
  for (col in value_cols) {
    testthat::expect_equal(back[[col]], wide[[col]], tolerance = 1e-12)
  }
  long_back <- out$n_inputs |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t),
      .by = dplyr::all_of(c(.regime_key, "fert_type"))
    )
  joined <- dplyr::inner_join(
    .regime_long(),
    long_back,
    by = c(.regime_key, "fert_type"),
    suffix = c("", "_split")
  )
  testthat::expect_identical(nrow(joined), nrow(.regime_long()))
  testthat::expect_equal(joined$n_input_t_split, joined$n_input_t)
  pointblank::expect_col_vals_in_set(
    out$n_inputs,
    "water_regime",
    c("rainfed", "irrigated")
  )
})

# ---- Residue terms re-evaluated at each part's yield -------------------------

# Two crops in one Spanish cell in 2010, each with its NPP computed by the
# package's own chain at its mean yield: maize (item 56, CBS 2514) at 6 t/ha,
# half irrigated with R = 1.5, and wheat (item 15, CBS 2511) at 4 t/ha, a
# quarter irrigated with R = 1.8. Neither yield bound of .rs_production()
# binds at these yields.
.regime_crops <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~item_cbs_code, ~year,
    ~production_t, ~area_ha, ~irrigated_share, ~ratio,
    0.25, 40.25, 203L, "56", 2514L, 2010L, 600, 100, 0.5, 1.5,
    0.25, 40.25, 203L, "15", 2511L, 2010L, 160, 40, 0.25, 1.8
  )
}

.regime_with_regions <- function(x) {
  regions <- dplyr::select(
    whep:::.sci_crop_regions(),
    "area_code",
    "region_krausmann",
    "region_hanpp",
    "region_un_sub"
  )
  dplyr::left_join(x, regions, by = "area_code")
}

# The crop rows through the NPP chain, at the mean yield (no regime term).
.regime_npp_input <- function() {
  .regime_crops() |>
    dplyr::select(-"irrigated_share", -"ratio") |>
    .regime_with_regions() |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()
}

.regime_residue_data <- function() {
  crops <- .regime_crops()
  npp <- .regime_npp_input()
  cells <- dplyr::transmute(
    crops,
    lon = .data$lon,
    lat = .data$lat,
    area_code = .data$area_code,
    item_prod_code = as.integer(.data$item_prod_code),
    year = .data$year
  )
  data <- list(
    npp_n_input = npp,
    residue_destiny_input = npp,
    regime_areas = dplyr::mutate(
      cells,
      irrigated_ha = crops$area_ha * crops$irrigated_share,
      rainfed_ha = crops$area_ha - .data$irrigated_ha
    ),
    regime_ratio = dplyr::mutate(cells, ratio_unbounded = crops$ratio),
    regime_production = .rs_production()
  )
  data$.npp_cache <- whep:::.n_balance_npp(data)
  data
}

# Long inputs: the recycling term the balance builds, plus a synthetic and a
# BNF row per crop, so yield-, area- and residue-split columns all occur.
.regime_residue_inputs <- function(data) {
  dplyr::bind_rows(
    whep:::.n_inputs_recycling(data),
    tibble::tribble(
      ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~fert_type, ~n_input_t,
      0.25, 40.25, 203L, 2514L, 2010L, "synthetic", 15,
      0.25, 40.25, 203L, 2514L, 2010L, "bnf", 2,
      0.25, 40.25, 203L, 2511L, 2010L, "synthetic", 6,
      0.25, 40.25, 203L, 2511L, 2010L, "bnf", 1
    )
  )
}

.regime_split_residue <- function(method = "yield_split") {
  data <- .regime_residue_data()
  n_inputs <- .regime_residue_inputs(data)
  whep:::.nb_split_balance(n_inputs, data, .regime_key, "grid", method)
}

# Each part through the same chain, written out in absolute form: its own
# area, its own production at its own yield, and its water regime, so the
# model's irrigation terms apply to the irrigated part. Then the residue
# destinies and the C/N partition, as the balance applies them: the
# package's default bedding rule (Wirsenius), or, given `bedding_fraction`,
# that share of the recovered non-feed residue bedded.
.regime_parts_by_hand <- function(bedding_fraction = NULL) {
  crops <- dplyr::mutate(
    .regime_crops(),
    yield_mean = .data$production_t / .data$area_ha,
    yield_rainfed = .data$yield_mean /
      (1 + .data$irrigated_share * (.data$ratio - 1))
  )
  dplyr::bind_rows(
    dplyr::mutate(
      crops,
      water_regime = "Rainfed",
      part_area = .data$area_ha * (1 - .data$irrigated_share),
      part_yield = .data$yield_rainfed
    ),
    dplyr::mutate(
      crops,
      water_regime = "Irrigated",
      part_area = .data$area_ha * .data$irrigated_share,
      part_yield = .data$ratio * .data$yield_rainfed
    )
  ) |>
    dplyr::mutate(
      production_t = .data$part_area * .data$part_yield,
      area_ha = .data$part_area
    ) |>
    dplyr::select(-"irrigated_share", -"ratio") |>
    .regime_with_regions() |>
    whep::calculate_crop_npp() |>
    whep::calculate_residue_destinies(
      bedding = if (is.null(bedding_fraction)) "wirsenius" else "fraction",
      bedding_fraction = bedding_fraction %||% 0
    ) |>
    whep::calculate_npp_carbon_nitrogen() |>
    dplyr::mutate(
      water_regime = stringr::str_to_lower(.data$water_regime),
      n_per_dm = .data$residue_n_t / .data$residue_dm_t,
      recycling = .data$root_n_t + .data$residue_soil_n_t,
      used_residue_n_t = .data$residue_feed_dm_t * .data$n_per_dm,
      bedding_residue_n_t = .data$residue_bedding_dm_t * .data$n_per_dm,
      burnt_residue_n_t = .data$residue_burn_dm_t * .data$n_per_dm
    )
}

.regime_residue_cols <- c(
  "recycling",
  "used_residue_n_t",
  "bedding_residue_n_t",
  "burnt_residue_n_t"
)

testthat::test_that("each part keeps the crop's production", {
  hand <- .regime_parts_by_hand()
  back <- hand |>
    dplyr::summarise(
      production_t = sum(.data$production_t),
      .by = "item_prod_code"
    )
  crops <- .regime_crops()
  testthat::expect_equal(
    back$production_t[match(crops$item_prod_code, back$item_prod_code)],
    crops$production_t,
    tolerance = 1e-12
  )
  # The bounded ratio is the unbounded one here, so the hand split is the
  # balance's split.
  items <- whep:::.nb_regime_items(.regime_residue_data())
  testthat::expect_equal(
    items$ratio[match(as.integer(crops$item_prod_code), items$item_prod_code)],
    crops$ratio
  )
})

testthat::test_that("recycling and the residue removals of each part are the model at its yield", {
  split <- .regime_split_residue()
  hand <- .regime_parts_by_hand()
  joined <- split$x |>
    dplyr::inner_join(
      dplyr::select(
        hand,
        "item_cbs_code",
        "water_regime",
        dplyr::all_of(.regime_residue_cols)
      ),
      by = c("item_cbs_code", "water_regime"),
      suffix = c("", "_hand")
    )
  testthat::expect_identical(nrow(joined), 4L)
  for (col in .regime_residue_cols) {
    testthat::expect_equal(
      joined[[col]],
      joined[[paste0(col, "_hand")]],
      tolerance = 1e-10
    )
  }
  # Not vacuous: the removals are non-zero, and the irrigated maize part
  # recycles less per hectare, against its rainfed part, than its yield
  # ratio of 1.5 would give it.
  testthat::expect_true(all(joined$used_residue_n_t > 0))
  testthat::expect_true(all(joined$burnt_residue_n_t > 0))
  maize <- joined[joined$item_cbs_code == 2514L, ]
  per_ha <- maize$recycling / maize$area_ha
  testthat::expect_lt(
    per_ha[maize$water_regime == "irrigated"] /
      per_ha[maize$water_regime == "rainfed"],
    1.5
  )
  # The long recycling rows the losses read carry the same split.
  long <- split$n_inputs |>
    dplyr::filter(.data$fert_type == "recycling") |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t),
      .by = c("item_cbs_code", "water_regime")
    ) |>
    dplyr::inner_join(
      dplyr::select(split$x, "item_cbs_code", "water_regime", "recycling"),
      by = c("item_cbs_code", "water_regime")
    )
  testthat::expect_identical(nrow(long), 4L)
  testthat::expect_equal(long$n_input_t, long$recycling, tolerance = 1e-12)
  # How each part's residue terms were set.
  stamps <- split$x[order(split$x$item_cbs_code, split$x$water_regime), ]
  testthat::expect_identical(
    stamps$method_residue_regime,
    rep(c("part_yield_irrigation_terms", "part_yield"), 2)
  )
})

testthat::test_that("every balance column sums over water_regime to the part evaluation (residue terms) or the unsplit balance (all else)", {
  split <- .regime_split_residue()
  data <- .regime_residue_data()
  unsplit <- whep:::.nb_inputs(.regime_residue_inputs(data), .regime_key) |>
    whep:::.nb_outputs(data, .regime_key)
  # The totals the part evaluation implies: the unsplit balance with its
  # residue terms replaced by their sum over the parts, and the input totals
  # that read them recomputed.
  parts_sum <- .regime_parts_by_hand() |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(.regime_residue_cols), sum),
      .by = "item_cbs_code"
    )
  expected <- unsplit |>
    dplyr::select(-dplyr::all_of(.regime_residue_cols)) |>
    dplyr::left_join(parts_sum, by = "item_cbs_code") |>
    whep:::.nb_input_totals() |>
    dplyr::arrange(.data$item_cbs_code)
  value_cols <- setdiff(
    names(expected)[purrr::map_lgl(expected, is.numeric)],
    .regime_key
  )
  back <- split$x |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), sum),
      .by = dplyr::all_of(.regime_key)
    ) |>
    dplyr::arrange(.data$item_cbs_code)
  testthat::expect_identical(nrow(back), nrow(expected))
  for (col in value_cols) {
    testthat::expect_equal(back[[col]], expected[[col]], tolerance = 1e-10)
  }
  # The residue terms are the ones that moved: the irrigation terms take
  # residue and root off the irrigated part.
  base <- dplyr::arrange(unsplit, .data$item_cbs_code)
  testthat::expect_true(all(back$recycling < base$recycling))
})

testthat::test_that("with the irrigation terms neutral, the parts' residue N sums to the unsplit crop's", {
  .rs_neutral_irrigation()
  split <- .regime_split_residue()
  data <- .regime_residue_data()
  unsplit <- whep:::.nb_inputs(.regime_residue_inputs(data), .regime_key) |>
    whep:::.nb_outputs(data, .regime_key) |>
    dplyr::arrange(.data$item_cbs_code)
  back <- split$x |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(.regime_residue_cols), sum),
      .by = dplyr::all_of(.regime_key)
    ) |>
    dplyr::arrange(.data$item_cbs_code)
  for (col in .regime_residue_cols) {
    # Bedding is zero at the default bedding fraction; zero must stay zero.
    rel <- abs(back[[col]] - unsplit[[col]]) /
      pmax(abs(unsplit[[col]]), .Machine$double.xmin)
    testthat::expect_lt(max(rel), 1e-10)
  }
  testthat::expect_gt(sum(back$used_residue_n_t), 0)
  # Not vacuous: the irrigated hectare still carries more than the rainfed.
  maize <- split$x[split$x$item_cbs_code == 2514L, ]
  per_ha <- maize$recycling / maize$area_ha
  testthat::expect_gt(
    per_ha[maize$water_regime == "irrigated"],
    per_ha[maize$water_regime == "rainfed"]
  )
})

# Part weights for the first key of .regime_wide() only.
.regime_first_key_weights <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year,
    ~.rw_recycling_rainfed, ~.rw_recycling_irrigated,
    ~.rw_stamp_rainfed, ~.rw_stamp_irrigated,
    0.25, 40.25, 203, 2511, 2010, 0.4, 0.5,
    "part_yield", "part_yield_irrigation_terms"
  )
}

.regime_split_recycling <- function(x, weights) {
  whep:::.nb_split_regime(
    x,
    .regime_shares(),
    .regime_key,
    area_cols = character(),
    residue = list(
      cols = c(recycling = "recycling"),
      weights = weights,
      stamp = "yield_share"
    )
  )
}

testthat::test_that("a non-zero residue term with no part weight aborts", {
  # The second key recycles 20 t N but the part weights stop at the first:
  # the two tables the weights and the balance are built from disagree, and
  # no share stands in for the missing part evaluation.
  x <- dplyr::select(.regime_wide(), dplyr::all_of(.regime_key), "recycling")
  testthat::expect_identical(x$recycling[x$lon == 0.75], 20)
  err <- testthat::expect_error(
    .regime_split_recycling(x, .regime_first_key_weights()),
    class = "whep_regime_residue_weight"
  )
  # The message names the count and the key.
  msg <- gsub("\\s+", " ", conditionMessage(err))
  testthat::expect_match(msg, "1 row carries residue N", fixed = TRUE)
  testthat::expect_match(msg, "0.75/40.25/203/2511/2010", fixed = TRUE)
})

testthat::test_that("a zero residue term with no part weight stays zero and passes", {
  x <- dplyr::select(.regime_wide(), dplyr::all_of(.regime_key), "recycling")
  x$recycling[x$lon == 0.75] <- 0
  out <- .regime_split_recycling(x, .regime_first_key_weights())
  first <- out[out$lon == 0.25, ]
  testthat::expect_equal(
    first$recycling[order(first$water_regime)],
    40 * c(0.5, 0.4)
  )
  testthat::expect_identical(
    first$method_residue_regime[order(first$water_regime)],
    c("part_yield_irrigation_terms", "part_yield")
  )
  second <- out[out$lon == 0.75, ]
  testthat::expect_identical(second$recycling, c(0, 0))
  testthat::expect_true(all(is.na(second$method_residue_regime)))
  testthat::expect_false(any(startsWith(names(out), ".rw_")))
})

testthat::test_that("area_split keeps the crop's residue per hectare on both parts", {
  split <- .regime_split_residue("area_split")
  testthat::expect_identical(
    unique(split$x$method_residue_regime),
    "crop_density"
  )
  maize <- split$x[split$x$item_cbs_code == 2514L, ]
  per_ha <- maize$recycling / maize$area_ha
  testthat::expect_equal(per_ha[1], per_ha[2], tolerance = 1e-12)
})

# The residue fixture with a bedding share, its NPP cache rebuilt on it: the
# recycled residue is what the destiny split leaves on the field, so it moves
# with the bedding share too.
.regime_bedded_data <- function(fraction = 0.3) {
  data <- .regime_residue_data()
  data$.npp_cache <- NULL
  data$residue_bedding <- "fraction"
  data$residue_bedding_fraction <- fraction
  data$.npp_cache <- whep:::.n_balance_npp(data)
  data
}

testthat::test_that("the manure brings back the bedding the regime parts remove", {
  data <- .regime_bedded_data()
  attached <- whep:::.nb_attach_regime_layer(
    data,
    .regime_key,
    "grid",
    "yield_split"
  )
  split <- whep:::.nb_split_balance(
    .regime_residue_inputs(attached),
    attached,
    .regime_key,
    "grid",
    "yield_split"
  )
  parts_bedding <- sum(split$x$bedding_residue_n_t)
  returned <- whep:::.ni_manure_bedding(attached)
  testthat::expect_equal(
    sum(returned$bedding_n_t),
    parts_bedding,
    tolerance = 1e-10
  )
  # Not vacuous: the irrigation terms take bedding off the irrigated parts,
  # so the parts' bedding is below the unsplit bedding, which is what the
  # manure input would return without the factors.
  unsplit <- whep:::.ni_manure_bedding(data)
  testthat::expect_lt(parts_bedding, sum(unsplit$bedding_n_t))
  # The split reuses the layer it was handed, which is the one it would build.
  testthat::expect_identical(
    attached$.regime_layer,
    whep:::.nb_regime_layer(data, .regime_key, "yield_split")
  )
})

testthat::test_that("the parts' residue N stays and leaves in full", {
  # Per part, the soil-returned residue N plus the used, bedded and burnt
  # residue N is the part's gross residue N, so the parts' four residue
  # destinies sum to the gross residue N of the parts' own evaluation.
  hand <- .regime_parts_by_hand(0.3)
  testthat::expect_true(all(hand$bedding_residue_n_t > 0))
  testthat::expect_equal(
    hand$residue_soil_n_t +
      hand$used_residue_n_t +
      hand$bedding_residue_n_t +
      hand$burnt_residue_n_t,
    hand$residue_n_t,
    tolerance = 1e-10
  )
  split <- whep:::.nb_split_balance(
    .regime_residue_inputs(.regime_bedded_data()),
    .regime_bedded_data(),
    .regime_key,
    "grid",
    "yield_split"
  )
  testthat::expect_equal(
    sum(split$x$recycling) +
      sum(split$x$used_residue_n_t) +
      sum(split$x$bedding_residue_n_t) +
      sum(split$x$burnt_residue_n_t),
    sum(hand$root_n_t + hand$residue_n_t),
    tolerance = 1e-10
  )
})

testthat::test_that("an unbedded or unsplit balance attaches no regime layer", {
  data <- .regime_residue_data()
  testthat::expect_identical(
    whep:::.nb_attach_regime_layer(data, .regime_key, "grid", "yield_split"),
    data
  )
  bedded <- .regime_bedded_data()
  for (method in c("area_split", "none")) {
    testthat::expect_identical(
      whep:::.nb_attach_regime_layer(bedded, .regime_key, "grid", method),
      bedded
    )
  }
  supplied <- bedded
  supplied$n_inputs <- .regime_residue_inputs(bedded)
  testthat::expect_identical(
    whep:::.nb_attach_regime_layer(
      supplied,
      .regime_key,
      "grid",
      "yield_split"
    ),
    supplied
  )
})

testthat::test_that("the balance hands its manure input the parts' bedding", {
  data <- .regime_bedded_data()
  data$.npp_cache <- NULL
  captured <- NULL
  testthat::local_mocked_bindings(
    build_n_inputs = function(..., data) {
      captured <<- data
      rlang::abort("captured", class = "test_captured_inputs")
    },
    .package = "whep"
  )
  testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = list(nh3 = "ipcc", leaching = "ipcc_fracleach"),
      resolution = "grid",
      data = data
    ),
    class = "test_captured_inputs"
  )
  testthat::expect_false(is.null(captured$.regime_bedding_factors))
  expected <- .regime_bedded_data() |>
    whep:::.nb_attach_regime_layer(.regime_key, "grid", "yield_split")
  testthat::expect_equal(
    whep:::.ni_manure_bedding(captured),
    whep:::.ni_manure_bedding(expected)
  )
})
