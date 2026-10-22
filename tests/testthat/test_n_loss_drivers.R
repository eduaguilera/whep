# build_n_loss_drivers(): the IPCC 2019 wet/dry drivers of the nitrogen loss
# cascade on the balance grid key. Offline: every layer comes from
# helper_n_loss_drivers.R through `data`.

.nld_build <- function(rows = .nld_rows(), window = "annual", ...) {
  suppressMessages(whep::build_n_loss_drivers(
    rows,
    window = window,
    data = .nld_inputs(2010L),
    ...
  ))
}

.nld_row <- function(tbl, lon, item, regime) {
  tbl[
    tbl$lon == lon &
      tbl$item_cbs_code %in% item &
      tbl$water_regime == regime,
  ]
}

testthat::test_that("the builder emits both driver tables with every column", {
  testthat::skip_if_not_installed("pointblank")
  out <- .nld_build()
  testthat::expect_named(
    out,
    c("n_balance_drivers", "n_balance_leaching_drivers")
  )
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year", "water_regime")
  out$n_balance_drivers |>
    pointblank::expect_col_exists(dplyr::all_of(c(
      key,
      "fert_type",
      "moisture_regime",
      "method_moisture_regime",
      "climate_fill_km",
      "climate",
      "method_climate",
      "irrig_type"
    )))
  out$n_balance_leaching_drivers |>
    pointblank::expect_col_exists(dplyr::all_of(c(
      key,
      "moisture_regime",
      "method_moisture_regime",
      "climate_fill_km"
    )))
  # One leaching row per balance key, one loss row per key and loss fert_type.
  rows <- .nld_rows()
  testthat::expect_identical(nrow(out$n_balance_leaching_drivers), nrow(rows))
  testthat::expect_identical(
    anyDuplicated(out$n_balance_leaching_drivers[key]),
    0L
  )
  testthat::expect_identical(nrow(out$n_balance_drivers), nrow(rows) * 7L)
  testthat::expect_setequal(
    unique(out$n_balance_drivers$fert_type),
    c("Excreta", "Liquid", "Solid", "SOM", "Synthetic", "Human", "Recycling")
  )
  for (tbl in out) {
    tbl |>
      pointblank::expect_col_vals_in_set("moisture_regime", c("wet", "dry"))
    tbl |> pointblank::expect_col_vals_not_null("method_moisture_regime")
  }
  out$n_balance_drivers |>
    pointblank::expect_col_vals_in_set("climate", c("ATL", "MED")) |>
    pointblank::expect_col_vals_in_set(
      "method_climate",
      "from_moisture_regime"
    ) |>
    pointblank::expect_col_vals_in_set(
      "irrig_type",
      c("Tier_1", "Flooded", "Med_average", "Rainfed")
    )
})

testthat::test_that("a fert_type column in the rows sets the loss rows", {
  rows <- dplyr::bind_rows(
    dplyr::mutate(.nld_rows()[1:2, ], fert_type = "Synthetic"),
    dplyr::mutate(.nld_rows()[3, ], fert_type = NA_character_)
  )
  out <- .nld_build(rows)
  testthat::expect_identical(nrow(out$n_balance_drivers), 2L)
  testthat::expect_true(all(out$n_balance_drivers$fert_type == "Synthetic"))
  # The row without a fert_type is a balance row only: it gets a leaching
  # driver and no loss driver.
  testthat::expect_identical(nrow(out$n_balance_leaching_drivers), 3L)
})

testthat::test_that("crop rows take the class build_cell_moisture_regime gives", {
  out <- .nld_build()$n_balance_leaching_drivers
  classes <- suppressMessages(whep::build_cell_moisture_regime(
    2010L,
    window = "annual",
    data = .nld_inputs(2010L)
  ))
  for (regime in c("irrigated", "rainfed")) {
    own <- .nld_row(out, 9.25, 2514L, regime)
    ref <- .nld_row(classes, 9.25, 2514L, regime)
    testthat::expect_identical(own$moisture_regime, ref$moisture_regime)
    testthat::expect_identical(
      own$method_moisture_regime,
      ref$method_moisture_regime
    )
  }
  # A's irrigated maize: 600 mm over 1000 mm of PET, dry.
  irrigated <- .nld_row(out, 9.25, 2514L, "irrigated")
  testthat::expect_identical(irrigated$moisture_regime, "dry")
  testthat::expect_identical(
    irrigated$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip"
  )
  testthat::expect_true(is.na(irrigated$climate_fill_km))
})

# Issue #1531: irrigated land in a cell dry on precipitation stays on the
# dry factors by default (IPCC 2019 Vol 4 Ch 11 Table 11.1 footnote 4, p.
# 11.13), and on the Mediterranean irrigated drivers; 500 mm of irrigation
# would otherwise lift A's maize to 1100 mm over 1000 mm of PET, wet.
testthat::test_that("irrigation does not move a dry row to wet by default", {
  inputs <- .nld_inputs(2010L)
  inputs$crop_depth$irrig_mm_irrigated[inputs$crop_depth$lon == 9.25] <- 500
  build <- function(...) {
    suppressMessages(whep::build_n_loss_drivers(
      .nld_rows(),
      window = "annual",
      data = inputs,
      ...
    ))
  }
  default <- build()$n_balance_drivers |>
    .nld_row(9.25, 2514L, "irrigated") |>
    dplyr::distinct(
      .data$moisture_regime,
      .data$climate,
      .data$irrig_type,
      .data$method_moisture_regime
    )
  testthat::expect_identical(default$moisture_regime, "dry")
  testthat::expect_identical(default$climate, "MED")
  testthat::expect_identical(default$irrig_type, "Med_average")
  testthat::expect_identical(
    default$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip"
  )
  with_irrig <- build(water = "precip_irrig")$n_balance_leaching_drivers |>
    .nld_row(9.25, 2514L, "irrigated")
  testthat::expect_identical(with_irrig$moisture_regime, "wet")
})

testthat::test_that("rows without a crop take the cell's rainfed class", {
  out <- .nld_build()$n_balance_leaching_drivers
  # No crop item (the SOM sequestration rows), in both water regimes: the
  # cell's precipitation-only class, since no crop means no irrigation.
  no_item <- out[is.na(out$item_cbs_code), ]
  testthat::expect_identical(nrow(no_item), 2L)
  testthat::expect_true(all(no_item$moisture_regime == "dry"))
  testthat::expect_true(all(
    no_item$method_moisture_regime ==
      "ipcc2019_wet_dry;annual;precip;cell_rainfed_class=no_crop_item"
  ))
  # An item WHEP has no regime land of (grass in B): the same cell class.
  grass <- .nld_row(out, 9.75, 3000L, "rainfed")
  testthat::expect_identical(grass$moisture_regime, "wet")
  testthat::expect_identical(
    grass$method_moisture_regime,
    "ipcc2019_wet_dry;annual;precip;cell_rainfed_class=no_regime_area"
  )
})

testthat::test_that("irrigated rows without N keep their cell's class", {
  out <- .nld_build()$n_balance_leaching_drivers
  rice <- out[out$item_cbs_code %in% 2807L & out$water_regime == "irrigated", ]
  testthat::expect_identical(nrow(rice), 2L)
  testthat::expect_true(all(stringr::str_detect(
    rice$method_moisture_regime,
    "cell_rainfed_class=no_regime_area"
  )))
})

testthat::test_that("an irrigated row with N cannot take its cell's class", {
  # Irrigated N on rice, an item WHEP holds no irrigated land of in A or B:
  # the balance and the moisture regime disagree on the item's regime land,
  # so the precipitation-only class would price irrigated N as rainfed.
  rows <- .nld_rows()
  wet_rice <- rows$item_cbs_code %in% 2807L & rows$water_regime == "irrigated"
  rows$n_input_t[wet_rice] <- c(5, 2)
  err <- testthat::expect_error(
    .nld_build(rows),
    class = "whep_n_loss_drivers_irrigated_cell_class"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "2 irrigated balance rows", fixed = TRUE)
  testthat::expect_match(msg, "2807: 7 t N", fixed = TRUE)
})

testthat::test_that("irrigated rows without an N column are refused", {
  # Without N, an irrigated row taking its cell's class cannot be shown to
  # carry none: an absent column is not a column of zeros.
  rows <- dplyr::select(.nld_rows(), -"n_input_t")
  err <- testthat::expect_error(
    .nld_build(rows),
    class = "whep_n_loss_drivers_irrigated_cell_class"
  )
  testthat::expect_match(
    cli::ansi_strip(conditionMessage(err)),
    "3 irrigated balance rows",
    fixed = TRUE
  )
})

testthat::test_that("a cell without CRU climate takes its nearest CRU cell's", {
  testthat::expect_message(
    out <- whep::build_n_loss_drivers(
      .nld_rows(),
      window = "annual",
      data = .nld_inputs(2010L)
    ),
    class = "whep_moisture_regime_nearest_climate"
  )
  c_row <- .nld_row(out$n_balance_leaching_drivers, 10.25, 2514L, "rainfed")
  # C's nearest CRU land cell is B (wet), not A (dry).
  testthat::expect_identical(c_row$moisture_regime, "wet")
  testthat::expect_equal(
    c_row$climate_fill_km,
    .nld_km(10.25, 47.75, 9.75, 47.75),
    tolerance = 1e-9
  )
  testthat::expect_identical(
    c_row$method_moisture_regime,
    paste0(
      "ipcc2019_wet_dry;annual;precip;",
      "nearest_cru_cell=mat_c+frost_days+prec_mm+pet_mm"
    )
  )
  # Rows in cells CRU covers keep their own climate and no distance.
  own <- out$n_balance_leaching_drivers[
    out$n_balance_leaching_drivers$lon != 10.25,
  ]
  testthat::expect_true(all(is.na(own$climate_fill_km)))
  testthat::expect_false(any(stringr::str_detect(
    own$method_moisture_regime,
    "nearest_cru_cell"
  )))
  # Nothing is left without a class.
  testthat::expect_false(anyNA(out$n_balance_leaching_drivers$moisture_regime))
  testthat::expect_false(anyNA(out$n_balance_drivers$moisture_regime))
})

testthat::test_that("the ATL/MED drivers follow the wet/dry class, rice flooded", {
  out <- .nld_build()$n_balance_drivers |>
    dplyr::filter(.data$fert_type == "Synthetic")
  get <- function(lon, item, regime, col) {
    .nld_row(out, lon, item, regime)[[col]]
  }
  # Dry A -> MED; wet B and C (filled from B) -> ATL.
  testthat::expect_identical(get(9.25, 2514L, "irrigated", "climate"), "MED")
  testthat::expect_identical(get(9.75, 3000L, "rainfed", "climate"), "ATL")
  testthat::expect_identical(get(10.25, 2514L, "rainfed", "climate"), "ATL")
  # MED irrigated -> Med_average, MED rainfed -> Rainfed, other ATL -> Tier_1.
  testthat::expect_identical(
    get(9.25, 2514L, "irrigated", "irrig_type"),
    "Med_average"
  )
  testthat::expect_identical(
    get(9.25, 2514L, "rainfed", "irrig_type"),
    "Rainfed"
  )
  testthat::expect_identical(
    get(9.75, 3000L, "rainfed", "irrig_type"),
    "Tier_1"
  )
  # Rice (2807) is Flooded in both climates and both water regimes.
  rice <- out[out$item_cbs_code %in% 2807L, ]
  testthat::expect_identical(nrow(rice), 4L)
  testthat::expect_setequal(rice$climate, c("ATL", "MED"))
  testthat::expect_setequal(rice$water_regime, c("irrigated", "rainfed"))
  testthat::expect_true(all(rice$irrig_type == "Flooded"))
  # Every combination the ATL/MED options read has a factor in both tables.
  combos <- dplyr::distinct(out, .data$irrig_type, .data$climate)
  testthat::expect_false(anyNA(
    dplyr::left_join(
      combos,
      whep::n2o_efs_disaggregated,
      by = c("irrig_type", "climate")
    )$ef
  ))
  testthat::expect_false(anyNA(
    dplyr::left_join(
      combos,
      whep::n2o_efs_ipcc2006,
      by = c("irrig_type", "climate")
    )$ef
  ))
})

testthat::test_that("the trailing 30-year default runs on a full window", {
  out <- suppressMessages(whep::build_n_loss_drivers(
    .nld_rows(),
    data = .nld_inputs(1981:2010)
  ))
  irrigated <- .nld_row(
    out$n_balance_leaching_drivers,
    9.25,
    2514L,
    "irrigated"
  )
  testthat::expect_identical(
    irrigated$method_moisture_regime,
    "ipcc2019_wet_dry;trailing_30;precip"
  )
  testthat::expect_false(anyNA(out$n_balance_leaching_drivers$moisture_regime))
})

testthat::test_that("a row the classifier cannot class aborts", {
  # No regime areas for 1985: whether A's maize was irrigated that year is
  # unknown, so its irrigated row's 30-year water term (with irrigation) is
  # missing.
  data <- .nld_inputs(1981:2010)
  data$regime_areas <- data$regime_areas[data$regime_areas$year != 1985L, ]
  err <- testthat::expect_error(
    suppressMessages(whep::build_n_loss_drivers(
      .nld_rows(),
      water = "precip_irrig",
      data = data
    )),
    class = "whep_n_loss_drivers_unclassified"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "missing=irrig_mm", fixed = TRUE)
  testthat::expect_match(msg, "1 balance row", fixed = TRUE)
})

testthat::test_that("the rows are checked", {
  rows <- .nld_rows()
  testthat::expect_error(
    whep::build_n_loss_drivers(),
    class = "whep_n_loss_drivers_rows"
  )
  testthat::expect_error(
    whep::build_n_loss_drivers(dplyr::select(rows, -"water_regime")),
    class = "whep_n_loss_drivers_rows"
  )
  bad <- rows
  bad$water_regime[1] <- "flooded"
  testthat::expect_error(
    whep::build_n_loss_drivers(bad),
    class = "whep_n_loss_drivers_rows"
  )
  testthat::expect_error(
    whep::build_n_loss_drivers(rows[0, ]),
    class = "whep_n_loss_drivers_rows"
  )
})

testthat::test_that("an absent precipitation layer aborts though every row classes", {
  # A layer of zeros classes every row (all dry, ratio 0 < 1), so a check
  # that each row got a class is satisfied by the vacuous input.
  data <- .nld_inputs(2010L)
  data$climate$annual_precip_mm <- 0
  vacuous <- whep:::.cmr_rename_climate(data$climate)
  classes <- whep:::.apply_wet_dry(vacuous, "precip")
  expect_supplied_guard(
    identity = !anyNA(classes$moisture_regime) &&
      nrow(classes) == nrow(vacuous),
    guard = whep::build_n_loss_drivers(
      .nld_rows(),
      window = "annual",
      data = data
    )
  )
})

testthat::test_that("the example returns both tables with the built columns", {
  example <- whep::build_n_loss_drivers(example = TRUE)
  built <- .nld_build()
  testthat::expect_named(example, names(built))
  for (nm in names(built)) {
    testthat::expect_setequal(names(example[[nm]]), names(built[[nm]]))
    testthat::expect_gt(nrow(example[[nm]]), 0L)
  }
})
