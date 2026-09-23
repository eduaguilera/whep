# Tests for R/n_balance_water_regime.R (whep#1233): the rainfed/irrigated
# split of build_nitrogen_balance() rows.

.nbw_inputs <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~fert_type, ~n_input_t,
    0.25, 50.25, 10L, 2511L, 2010L, "synthetic", 100,
    0.25, 50.25, 10L, 2511L, 2010L, "bnf", 10,
    0.25, 50.25, 10L, 2513L, 2010L, "synthetic", 20,
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 40
  )
}

# Wheat (item_prod_code 15 -> item_cbs_code 2511) is a quarter irrigated;
# barley (44 -> 2513) is wholly rainfed. Grass has no regime layer at all.
.nbw_regime_area <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 10L, "15", 2010L, 30, 10,
    0.25, 50.25, 10L, "44", 2010L, 5, 0
  )
}

.nbw_drivers <- function(n_inputs, by_regime = FALSE) {
  out <- n_inputs |>
    dplyr::filter(.data$fert_type %in% c("synthetic", "excreta")) |>
    dplyr::mutate(fert_type = whep:::.nb_loss_fert_type(.data$fert_type)) |>
    dplyr::select(-"n_input_t") |>
    dplyr::distinct()
  if (!by_regime) {
    return(dplyr::mutate(out, climate = "ATL"))
  }
  # The Aguilera method keys the direct EF on irrigation type: the regime is
  # exactly what this driver table carries per row.
  tidyr::crossing(out, water_regime = c("rainfed", "irrigated", NA)) |>
    dplyr::mutate(
      climate = "MED",
      irrig_type = dplyr::case_match(
        .data$water_regime,
        "rainfed" ~ "Rainfed",
        "irrigated" ~ "Sprinkler",
        .default = "Med_average"
      )
    )
}

.nbw_run <- function(water_regime = "none", data = list(), n2o = "ipcc2019") {
  n_inputs <- .nbw_inputs()
  # Not modifyList(): it merges a tibble column by column, so a replacement
  # driver table would be spliced into the default one.
  defaults <- list(
    n_inputs = n_inputs,
    n_balance_drivers = .nbw_drivers(n_inputs)
  )
  data <- c(data, defaults[setdiff(names(defaults), names(data))])
  whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = n2o,
      leaching = "ipcc_fracleach",
      water_regime = water_regime
    ),
    data = data
  )
}

.nbw_split <- function(data = list(), n2o = "ipcc2019") {
  data$crop_regime_area <- data$crop_regime_area %||% .nbw_regime_area()
  suppressMessages(.nbw_run("area_share", data, n2o))
}

testthat::test_that("the default keeps one row per crop and adds no column", {
  out <- .nbw_run()
  testthat::expect_false(rlang::has_name(out, "water_regime"))
  testthat::expect_false(rlang::has_name(out, "method_water_regime"))
  testthat::expect_equal(nrow(out), 3L)
})

testthat::test_that("an explicit none is the same balance as the default", {
  testthat::expect_identical(.nbw_run("none"), .nbw_run())
})

testthat::test_that("crop rows split by irrigated share of harvested area", {
  out <- .nbw_split()
  wheat <- dplyr::filter(out, .data$item_cbs_code == 2511L)
  irrigated <- dplyr::filter(wheat, .data$water_regime == "irrigated")
  rainfed <- dplyr::filter(wheat, .data$water_regime == "rainfed")
  testthat::expect_equal(irrigated$n_input_full_t, 110 * 0.25)
  testthat::expect_equal(rainfed$n_input_full_t, 110 * 0.75)
  testthat::expect_setequal(wheat$method_water_regime, "area_share")
})

testthat::test_that("a wholly rainfed crop yields one rainfed row", {
  barley <- .nbw_split() |> dplyr::filter(.data$item_cbs_code == 2513L)
  testthat::expect_equal(barley$water_regime, "rainfed")
  testthat::expect_equal(barley$n_input_full_t, 20)
})

testthat::test_that("rows with no regime basis stay whole and are stamped", {
  grass <- .nbw_split() |> dplyr::filter(.data$item_cbs_code == 3000L)
  testthat::expect_equal(nrow(grass), 1L)
  testthat::expect_true(is.na(grass$water_regime))
  testthat::expect_equal(grass$method_water_regime, "unsplit_no_basis")
  testthat::expect_equal(grass$n_input_full_t, 40)
})

testthat::test_that("every tonnage term conserves across the split", {
  base <- .nbw_run()
  split <- .nbw_split()
  extensive <- grep("_t$", names(base), value = TRUE)
  totals <- function(x) {
    x |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(extensive), sum),
        .by = "item_cbs_code"
      ) |>
      dplyr::arrange(.data$item_cbs_code)
  }
  testthat::expect_equal(totals(split), totals(base))
})

testthat::test_that("the split conserves at polity resolution too", {
  n_inputs <- .nbw_inputs()
  polity_inputs <- dplyr::select(n_inputs, -"lon", -"lat")
  run <- function(water_regime, extra = list()) {
    whep::build_nitrogen_balance(
      methods = list(
        nh3 = "ipcc",
        leaching = "ipcc_fracleach",
        water_regime = water_regime
      ),
      resolution = "polity",
      data = c(
        list(
          n_inputs = polity_inputs,
          n_balance_drivers = dplyr::select(
            .nbw_drivers(n_inputs),
            -"lon",
            -"lat"
          )
        ),
        extra
      )
    )
  }
  base <- run("none")
  split <- suppressMessages(
    run("area_share", list(crop_regime_area = .nbw_regime_area()))
  )
  testthat::expect_equal(
    sum(split$n_input_full_t),
    sum(base$n_input_full_t)
  )
  testthat::expect_setequal(
    split$water_regime,
    c("irrigated", "rainfed", NA)
  )
})

testthat::test_that("regime-keyed drivers reach each regime separately", {
  n_inputs <- .nbw_inputs()
  out <- .nbw_split(
    list(n_balance_drivers = .nbw_drivers(n_inputs, by_regime = TRUE)),
    n2o = "aguilera"
  )
  wheat <- dplyr::filter(out, .data$item_cbs_code == 2511L) |>
    dplyr::mutate(ef = .data$n2o_direct_n_t / .data$n_input_for_n2o_t)
  ef <- rlang::set_names(wheat$ef, wheat$water_regime)
  # The ratio of the MED sprinkler and rainfed EFs of n2o_efs_disaggregated
  # (the fertiliser modifier is common to both and cancels), which one
  # blended row per crop could not carry at once.
  efs <- whep::n2o_efs_disaggregated |>
    dplyr::filter(.data$climate == "MED")
  expected <- efs$ef[efs$irrig_type == "Sprinkler"] /
    efs$ef[efs$irrig_type == "Rainfed"]
  testthat::expect_equal(ef[["irrigated"]] / ef[["rainfed"]], expected)
})

testthat::test_that("the split needs the regime area layer", {
  testthat::expect_error(
    .nbw_run("area_share"),
    class = "whep_missing_regime_area"
  )
})

testthat::test_that("a layer with no positive hectares is refused", {
  # Zero hectares everywhere would split nothing while every total still
  # reconciled, so the guard must fire even though conservation holds.
  zero <- dplyr::mutate(.nbw_regime_area(), rainfed_ha = 0, irrigated_ha = 0)
  expect_supplied_guard(
    identity = isTRUE(all.equal(
      sum(zero$irrigated_ha) + sum(zero$rainfed_ha),
      sum(zero$rainfed_ha + zero$irrigated_ha)
    )),
    guard = .nbw_run("area_share", list(crop_regime_area = zero))
  )
})

testthat::test_that("a layer that matches no crop row warns", {
  elsewhere <- dplyr::mutate(.nbw_regime_area(), area_code = 99L)
  testthat::expect_warning(
    .nbw_run("area_share", list(crop_regime_area = elsewhere)),
    class = "whep_regime_area_unmatched"
  )
})

testthat::test_that("an unknown water_regime method is refused", {
  testthat::expect_error(.nbw_run("drip"), "water_regime")
})

testthat::test_that("the share pools hectares across crops of one item", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  # Two production items of one CBS item: the share is 10 / 40, not the
  # mean of the two crops' own shares (0.5 and 0).
  layer <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 10L, "15", 2010L, 10, 10,
    0.25, 50.25, 10L, "15", 2010L, 20, 0
  )
  shares <- whep:::.nb_regime_shares(
    "area_share",
    list(crop_regime_area = layer),
    key
  )
  testthat::expect_equal(shares$irrigated_share, 0.25)
})
