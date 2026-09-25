# Fixtures ---------------------------------------------------------------------

# One polity (area_code 10) with two cells. The engine intake is keyed by cell
# for the grid tests and by country (sub_territory NA) for the polity tests.
.nmf_intake <- function(sub_territory = "0.25_50.25") {
  tibble::tribble(
    ~year, ~territory, ~livestock_category, ~item_cbs_code, ~feed_quality,
    ~intake_dm_t,
    2010L, "10", "Cattle_milk", 2513L, "high_quality", 200,
    2010L, "10", "Cattle_milk", NA, "grass", 600,
    2010L, "10", "Pigs", 2513L, "high_quality", 300
  ) |>
    dplyr::mutate(sub_territory = sub_territory, .after = "territory")
}

.nmf_gridded <- function(sub_territory = "0.25_50.25") {
  list(
    crops = tibble::tribble(
      ~year, ~territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
      2010L, "10", "15", 6, 500
    ) |>
      dplyr::mutate(sub_territory = sub_territory, .after = "territory")
  )
}

.nmf_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2010L, 10L, 2511L, "ha", 300,
    2010L, 10L, 2514L, "ha", 100
  )
}

.nmf_row <- function(
  item_code,
  item,
  element,
  value,
  area = 10L,
  source = "FAO TIER 1"
) {
  tibble::tibble(
    `Area Code` = area,
    Area = "Somewhere",
    `Item Code` = item_code,
    Item = item,
    Element = element,
    Year = 2010L,
    Source = source,
    Unit = "kg",
    Value = value
  )
}

# The FAOSTAT pin as it is shaped: leaves, the aggregates that sum them, a
# second reporting source and a continental rollup that must all be ignored.
.nmf_manure_pin <- function() {
  applied <- "Manure applied to soils (N content)"
  pasture <- "Manure left on pasture (N content)"
  dplyr::bind_rows(
    .nmf_row(1755L, "All Animals", applied, 20000),
    .nmf_row(1755L, "All Animals", applied, 99999, source = "UNFCCC"),
    .nmf_row(1755L, "All Animals", applied, 88888, area = 5100L),
    .nmf_row(960L, "Cattle, dairy", pasture, 3000),
    .nmf_row(976L, "Sheep", pasture, 1000),
    .nmf_row(1052L, "Chickens, layers", pasture, 500),
    .nmf_row(1177L, "Llamas", pasture, 200),
    .nmf_row(1757L, "Cattle", pasture, 3000),
    .nmf_row(1749L, "Sheep and Goats", pasture, 1000),
    .nmf_row(1054L, "Chickens", pasture, 500),
    .nmf_row(2029L, "Poultry Birds", pasture, 500),
    .nmf_row(1760L, "Camels and Llamas", pasture, 200),
    .nmf_row(1755L, "All Animals", pasture, 4700),
    .nmf_row(1755L, "All Animals", pasture, 77777, source = "UNFCCC")
  )
}

.nmf_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 50.25, 10L, 1, 3000,
    0.75, 50.25, 10L, 1, 3000
  )
}

.nmf_livestock_spatial <- function() {
  list(
    gridded_pasture = tibble::tribble(
      ~lon, ~lat, ~year, ~pasture_ha, ~rangeland_ha,
      0.25, 50.25, 2010L, 300, 100,
      0.75, 50.25, 2010L, 100, 100
    ),
    gridded_cropland = tibble::tribble(
      ~lon, ~lat, ~year, ~cropland_ha,
      0.25, 50.25, 2010L, 100,
      0.75, 50.25, 2010L, 300
    ),
    manure_pattern = NULL
  )
}

.nmf_polity_data <- function() {
  list(
    livestock_intake = .nmf_intake(NA_character_),
    gridded = .nmf_gridded(NA_character_),
    methods = list(allocation = list(cap_method = "potential_uptake")),
    primary_prod = .nmf_primary_prod(),
    manure = .nmf_manure_pin(),
    manure_method = "faostat"
  )
}

.nmf_grid_data <- function() {
  c(
    .nmf_polity_data(),
    list(
      cell_polity = .nmf_cell_polity(),
      livestock_spatial = .nmf_livestock_spatial(),
      crop_patterns = tibble::tribble(
        ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
        0.25, 50.25, 15L, 1,
        0.75, 50.25, 56L, 1
      ),
      type_cropland = tibble::tribble(
        ~lon, ~lat, ~year, ~luh2_type, ~type_ha,
        0.25, 50.25, 2010L, "c3ann", 100,
        0.75, 50.25, 2010L, "c3ann", 300
      ),
      nhx = tibble::tribble(
        ~lon, ~lat, ~year, ~value_g,
        0.25, 50.25, 2010L, 1e9,
        0.75, 50.25, 2010L, 1e9
      ),
      noy = tibble::tribble(
        ~lon, ~lat, ~year, ~value_g,
        0.25, 50.25, 2010L, 1e9,
        0.75, 50.25, 2010L, 1e9
      ),
      ag_land_support = tibble::tribble(
        ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~land_use, ~area_ha,
        0.25, 50.25, 10L, 2511L, 2010L, "cropland", 100,
        0.75, 50.25, 10L, 2514L, 2010L, "cropland", 300,
        0.25, 50.25, 10L, 3000L, 2010L, "grassland", 400,
        0.75, 50.25, 10L, 3000L, 2010L, "grassland", 200
      )
    )
  )
}

# The engine's own solid share, computed here independently of the helper.
.nmf_engine_solid_share <- function(data) {
  applied <- .nmf_quiet_engine(whep::build_livestock_nutrient_flows(
    data$livestock_intake,
    resolution = "national",
    methods = data$methods,
    gridded = data$gridded
  ))$applied
  solid <- sum(applied$applied_n[applied$manure_type == "Solid"])
  liquid <- sum(applied$applied_n[applied$manure_type == "Liquid"])
  solid / (solid + liquid)
}

.nmf_mass <- function(out, types) {
  sum(out$n_input_t[out$fert_type %in% types])
}

# Muffles the unmapped-species warning every fixture raises (llamas) and the
# manure engine's own "Unknown or uninitialised column: `method_bedding_mms`"
# warning, which it raises on every call without bedding (pre-existing).
.nmf_quiet <- function(expr) {
  suppressMessages(
    withCallingHandlers(
      .nmf_quiet_engine(expr),
      whep_manure_faostat_unmapped = \(w) invokeRestart("muffleWarning")
    )
  )
}

.nmf_quiet_engine <- function(expr) {
  withCallingHandlers(
    expr,
    warning = \(w) {
      if (grepl("method_bedding_mms", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Tests ------------------------------------------------------------------------

testthat::test_that("an unknown manure_method is refused", {
  testthat::expect_error(
    whep::build_n_inputs(data = list(), manure_method = "gleam")
  )
  testthat::expect_error(
    whep::build_n_inputs(data = list(manure_method = "gleam", manure = 1)),
    class = "rlang_error"
  )
})

testthat::test_that("faostat applied manure keeps its national mass", {
  data <- .nmf_polity_data()
  out <- .nmf_quiet(whep::build_n_inputs(resolution = "polity", data = data))
  # 20,000 kg of the FAO TIER 1 row only: the UNFCCC duplicate and the
  # continental rollup never reach the balance.
  testthat::expect_equal(
    .nmf_mass(out, c("manure_solid", "manure_liquid")),
    20,
    tolerance = 1e-10
  )
  share <- .nmf_engine_solid_share(data)
  testthat::expect_gt(share, 0)
  testthat::expect_lt(share, 1)
  testthat::expect_equal(
    .nmf_mass(out, "manure_solid"),
    20 * share,
    tolerance = 1e-10
  )
})

testthat::test_that("faostat applied manure is split over crops by area", {
  out <- .nmf_quiet(
    whep::build_n_inputs(resolution = "polity", data = .nmf_polity_data())
  )
  applied <- out |>
    dplyr::filter(.data$fert_type %in% c("manure_solid", "manure_liquid")) |>
    dplyr::summarise(n = sum(.data$n_input_t), .by = "item_cbs_code") |>
    dplyr::arrange(.data$item_cbs_code)
  testthat::expect_equal(applied$item_cbs_code, c(2511L, 2514L))
  testthat::expect_equal(applied$n, c(15, 5), tolerance = 1e-10)
})

testthat::test_that("faostat pasture manure sums the mapped species", {
  testthat::expect_warning(
    out <- suppressMessages(.nmf_quiet_engine(
      whep::build_n_inputs(resolution = "polity", data = .nmf_polity_data())
    )),
    class = "whep_manure_faostat_unmapped"
  )
  excreta <- dplyr::filter(out, .data$fert_type == "excreta")
  # Cattle dairy 3000 + sheep 1000 + layers 500 kg; the aggregates are not
  # added again and llamas, which the mapping does not name, are excluded
  # with the warning above.
  testthat::expect_equal(sum(excreta$n_input_t), 4.5, tolerance = 1e-10)
  testthat::expect_true(all(excreta$item_cbs_code == 3000L))
})

testthat::test_that("the unmapped-species warning names item and tonnage", {
  testthat::expect_warning(
    suppressMessages(.nmf_quiet_engine(
      whep::build_n_inputs(resolution = "polity", data = .nmf_polity_data())
    )),
    "Llamas \\(1177\\): 0.2 t N"
  )
})

testthat::test_that("faostat rows are stamped with their source", {
  out <- .nmf_quiet(
    whep::build_n_inputs(resolution = "polity", data = .nmf_polity_data())
  )
  manure <- c("excreta", "manure_solid", "manure_liquid")
  testthat::expect_true(all(
    out$method_manure[out$fert_type %in% manure] == "faostat"
  ))
  testthat::expect_true(all(is.na(out$method_manure[
    !out$fert_type %in% manure
  ])))
})

testthat::test_that("a country-year with no engine split is booked as solid", {
  data <- .nmf_polity_data()
  data$manure <- dplyr::bind_rows(
    data$manure,
    .nmf_row(
      1755L,
      "All Animals",
      "Manure applied to soils (N content)",
      8000,
      area = 203L
    )
  )
  data$primary_prod <- dplyr::bind_rows(
    data$primary_prod,
    tibble::tibble(
      year = 2010L,
      area_code = 203L,
      item_cbs_code = 2511L,
      unit = "ha",
      value = 50
    )
  )
  testthat::expect_warning(
    out <- .nmf_quiet(whep::build_n_inputs(resolution = "polity", data = data)),
    class = "whep_manure_faostat_all_solid"
  )
  spain <- dplyr::filter(out, .data$area_code == 203L)
  testthat::expect_equal(unique(spain$fert_type), "manure_solid")
  testthat::expect_equal(unique(spain$method_manure), "faostat_all_solid")
  testthat::expect_equal(sum(spain$n_input_t), 8, tolerance = 1e-10)
  # The country the engine does split keeps the plain stamp.
  testthat::expect_true(all(
    out$method_manure[out$area_code == 10L & !is.na(out$method_manure)] ==
      "faostat"
  ))
})

testthat::test_that("faostat pasture manure follows the livestock proxies", {
  out <- .nmf_quiet(
    whep::build_n_inputs(resolution = "grid", data = .nmf_grid_data())
  )
  excreta <- out |>
    dplyr::filter(.data$fert_type == "excreta") |>
    dplyr::arrange(.data$lon)
  # Cattle and sheep (pasture proxy) follow pasture + rangeland, 400:200;
  # layers (cropland proxy) follow cropland, 100:300.
  testthat::expect_equal(
    excreta$n_input_t,
    c(4 * 400 / 600 + 0.5 * 100 / 400, 4 * 200 / 600 + 0.5 * 300 / 400),
    tolerance = 1e-10
  )
  testthat::expect_equal(sum(excreta$n_input_t), 4.5, tolerance = 1e-10)
})

testthat::test_that("faostat grid applied manure re-aggregates to national", {
  # With a local-grain (per-cell) engine intake, the grain the default manure
  # term needs to reach cells at all.
  data <- .nmf_grid_data()
  data$livestock_intake <- .nmf_intake("0.25_50.25")
  data$gridded <- .nmf_gridded("0.25_50.25")
  out <- .nmf_quiet(whep::build_n_inputs(resolution = "grid", data = data))
  testthat::expect_false(anyNA(out$lon))
  testthat::expect_equal(
    .nmf_mass(out, c("manure_solid", "manure_liquid")),
    20,
    tolerance = 1e-10
  )
  testthat::expect_equal(
    .nmf_mass(out, "manure_solid"),
    20 * .nmf_engine_solid_share(data),
    tolerance = 1e-10
  )
})

testthat::test_that("the faostat grid path runs on a national engine intake", {
  # A national intake cannot be placed on cells by the engine itself, but under
  # "faostat" it only supplies the solid share, so the grid build succeeds.
  out <- .nmf_quiet(
    whep::build_n_inputs(resolution = "grid", data = .nmf_grid_data())
  )
  testthat::expect_true(all(
    c("excreta", "manure_solid", "manure_liquid") %in% out$fert_type
  ))
  testthat::expect_false(anyNA(out$lon))
})

testthat::test_that("pasture manure with no proxy cell is reported, not lost", {
  data <- .nmf_grid_data()
  data$livestock_spatial$gridded_pasture$pasture_ha <- 0
  data$livestock_spatial$gridded_pasture$rangeland_ha <- 0
  caught <- character()
  withCallingHandlers(
    .nmf_quiet(whep::build_n_inputs(resolution = "grid", data = data)),
    warning = function(w) {
      caught <<- c(caught, class(w))
      invokeRestart("muffleWarning")
    }
  )
  testthat::expect_true("whep_manure_faostat_unplaced" %in% caught)
})

testthat::test_that("faostat needs the engine intake for its split", {
  data <- .nmf_polity_data()
  data$livestock_intake <- NULL
  testthat::expect_error(
    whep::build_n_inputs(resolution = "polity", data = data),
    class = "whep_manure_faostat_input"
  )
})

testthat::test_that("faostat on a grid needs the livestock surfaces", {
  data <- .nmf_grid_data()
  data$livestock_spatial <- NULL
  testthat::expect_error(
    whep::build_n_inputs(resolution = "grid", data = data),
    class = "whep_manure_faostat_input"
  )
})

testthat::test_that("a renamed pasture element aborts rather than vanishing", {
  data <- .nmf_polity_data()
  data$manure$Element[
    data$manure$Element == "Manure left on pasture (N content)"
  ] <- "Manure on pasture (N)"
  testthat::expect_error(
    .nmf_quiet_engine(whep::build_n_inputs(resolution = "polity", data = data)),
    class = "whep_absent_label"
  )
})

testthat::test_that("a leaf set that misses All Animals aborts", {
  data <- .nmf_polity_data()
  data$manure <- dplyr::filter(data$manure, .data$Item != "Sheep")
  testthat::expect_error(
    .nmf_quiet_engine(whep::build_n_inputs(resolution = "polity", data = data)),
    class = "whep_manure_faostat_leaves"
  )
})

testthat::test_that("a non-kg unit aborts", {
  data <- .nmf_polity_data()
  data$manure$Unit <- "t"
  testthat::expect_error(
    whep::build_n_inputs(resolution = "polity", data = data),
    "not in"
  )
})

testthat::test_that("a broken manure pin is ignored under the default", {
  # The engine source never reads `manure`, so under the default a pin that
  # would abort the faostat source changes nothing.
  broken <- tibble::tibble(Element = "nothing useful")
  data <- .nmf_polity_data()
  data$manure_method <- NULL
  default <- .nmf_quiet(
    whep::build_n_inputs(resolution = "polity", data = data)
  )
  data$manure <- broken
  testthat::expect_identical(
    .nmf_quiet(whep::build_n_inputs(resolution = "polity", data = data)),
    default
  )
  data$manure_method <- "faostat"
  testthat::expect_error(
    whep::build_n_inputs(resolution = "polity", data = data),
    class = "whep_absent_label"
  )
})

testthat::test_that("the driver builds the faostat stages only when chosen", {
  testthat::expect_identical(
    whep:::.ni_manure_stages("livestock_intake"),
    character()
  )
  testthat::expect_identical(
    whep:::.ni_manure_stages("faostat"),
    c("manure", "livestock_spatial")
  )
})

testthat::test_that("the manure stream is requested by its source's inputs", {
  testthat::expect_true(
    "manure" %in%
      whep:::.ni_requested_streams(list(livestock_intake = 1))
  )
  testthat::expect_false(
    "manure" %in%
      whep:::.ni_requested_streams(
        list(livestock_intake = 1, manure_method = "faostat")
      )
  )
  testthat::expect_true(
    "manure" %in%
      whep:::.ni_requested_streams(
        list(livestock_intake = 1, manure = 1, manure_method = "faostat")
      )
  )
})

testthat::test_that("every FAOSTAT pasture leaf in the pin maps or is named", {
  # The leaves the 2010 pin carries (measured): each either maps to a species
  # group of livestock_mapping.csv or is one of the two the mapping lacks.
  leaves <- c(
    946L,
    960L,
    961L,
    976L,
    1016L,
    1049L,
    1051L,
    1052L,
    1053L,
    1068L,
    1079L,
    1096L,
    1107L,
    1110L,
    1126L,
    1177L
  )
  mapping <- whep:::.ni_livestock_mapping(list())
  testthat::expect_setequal(
    setdiff(leaves, mapping$item_code),
    c(1051L, 1177L)
  )
  testthat::expect_length(
    intersect(whep:::.ni_faostat_aggregate_items(), mapping$item_code),
    0L
  )
})

testthat::test_that("applied manure with no crop share is dropped and recorded", {
  pin <- dplyr::bind_rows(
    .nmf_manure_pin(),
    .nmf_row(
      1755L,
      "All Animals",
      "Manure applied to soils (N content)",
      8000,
      area = 203L
    )
  )
  supported <- tibble::tibble(year = 2010L, area_code = 10L)
  out <- whep:::.n_drop_uncelled_manure(pin, supported, "drop")
  testthat::expect_equal(out$removed$area_code, 203L)
  testthat::expect_equal(out$removed$manure_applied_n_t, 8)
  testthat::expect_equal(out$removed$share_of_global, 8 / 28)
  # Only Spain's applied row goes; every pasture row stays.
  testthat::expect_equal(nrow(pin) - nrow(out$manure), 1L)
  testthat::expect_false(any(
    out$manure$`Area Code` == 203L &
      out$manure$Element == "Manure applied to soils (N content)"
  ))
  testthat::expect_error(
    whep:::.n_drop_uncelled_manure(pin, supported, "abort"),
    class = "whep_uncelled_manure"
  )
})

testthat::test_that("nothing is dropped when every polity is supported", {
  supported <- tibble::tibble(year = 2010L, area_code = 10L)
  out <- whep:::.n_drop_uncelled_manure(.nmf_manure_pin(), supported, "abort")
  testthat::expect_identical(out$manure, .nmf_manure_pin())
  testthat::expect_equal(nrow(out$removed), 0L)
})

testthat::test_that("FAOSTAT regional rollups never reach the pasture term", {
  data <- .nmf_polity_data()
  data$manure <- dplyr::bind_rows(
    data$manure,
    .nmf_row(
      976L,
      "Sheep",
      "Manure left on pasture (N content)",
      50000,
      area = 420L
    ),
    .nmf_row(
      1755L,
      "All Animals",
      "Manure left on pasture (N content)",
      50000,
      area = 420L
    )
  )
  out <- .nmf_quiet(whep::build_n_inputs(resolution = "polity", data = data))
  testthat::expect_equal(
    sum(out$n_input_t[out$fert_type == "excreta"]),
    4.5,
    tolerance = 1e-10
  )
})
