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
    dplyr::filter(as.integer(.data$item_prod_code) == 15L) |>
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
