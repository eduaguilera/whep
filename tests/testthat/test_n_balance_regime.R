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
