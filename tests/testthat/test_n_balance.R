# Tests for R/n_balance.R (Module C, Task C7): build_nitrogen_balance().

# Helper fixtures --------------------------------------------------------------

.nb_bnf_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~crop_npp_n_t,
    ~product_n_t,
    ~weed_npp_n_t,
    ~land_use,
    ~legumes_seeded,
    ~seeded_cover_crop_share,
    ~area_ha,
    0.25,
    50.25,
    10L,
    2010L,
    "176",
    10,
    5,
    4,
    "Cropland",
    0,
    0,
    40
  )
}

.nb_npp_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~item_cbs_code,
    ~area_ha,
    ~product_dm_t,
    ~residue_dm_t,
    ~root_dm_t,
    0.25,
    50.25,
    10L,
    2010L,
    "15",
    2511L,
    40,
    87.9,
    135.75,
    30,
  )
}

.nb_livestock_intake <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_quality,
    ~intake_dm_t,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    2513L,
    "high_quality",
    200,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    NA,
    "grass",
    600
  )
}

.nb_gridded <- function() {
  list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      "barley",
      6,
      200
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      50
    )
  )
}

.nb_nhx <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    2000000000
  )
}

.nb_noy <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    1000000000
  )
}

.nb_urban_population <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~urban_pop,
    0.25,
    50.25,
    2010L,
    30898536
  )
}

.nb_cropland_ha <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~cropland_ha,
    0.25,
    50.25,
    10L,
    2010L,
    1000
  )
}

.nb_cell_polity <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    ~cell_area_ha,
    0.25,
    50.25,
    10L,
    1,
    3000
  )
}

.nb_ag_land_support <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~land_use, ~area_ha,
    0.25, 50.25, 10L, 2511L, 2010L, "cropland", 1000,
    0.25, 50.25, 10L, 3000L, 2010L, "grassland", 2000
  )
}

.nb_carbon_balance <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~land_use,
    ~year,
    ~area_ha,
    ~son_change_kgn_ha,
    0.25,
    50.25,
    10L,
    "Cropland",
    2010L,
    50,
    -0.4 # negative son_change_kgn_ha means SOM sequestration is positive
  )
}

.nb_primary_prod <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~unit,
    ~value,
    2010L,
    10L,
    2511L,
    "ha",
    700
  )
}

.nb_fertilizer <- function() {
  tibble::tribble(
    ~Year,
    ~`Area Code`,
    ~Element,
    ~Item,
    ~Value,
    2010L,
    10L,
    "Agricultural Use",
    "Nutrient nitrogen N (total)",
    100
  )
}

.nb_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    1
  )
}

.nb_type_cropland <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~luh2_type,
    ~type_ha,
    0.25,
    50.25,
    2010L,
    "c3ann",
    1000
  )
}

.nb_residue_destiny_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~item_prod_code,
    ~residue_dm_t,
    ~region_krausmann,
    ~region_un_sub,
    0.25,
    50.25,
    10L,
    2511L,
    2010L,
    "15",
    135.75,
    "Western Europe",
    "Western Europe"
  )
}

.nb_full_data <- function() {
  list(
    bnf_input = .nb_bnf_input(),
    npp_n_input = .nb_npp_input(),
    livestock_intake = .nb_livestock_intake(),
    gridded = .nb_gridded(),
    nhx = .nb_nhx(),
    noy = .nb_noy(),
    urban_population = .nb_urban_population(),
    # Named, because the default basis is the total population: the pinned
    # balances below were measured on the urban basis.
    human_n_population_basis = "urban",
    # The fixture's one human-N cell carries ~39 kt N on 1,000 ha, far above
    # its 170 t room, with no other cropland in its polity: the room cap
    # (#1336) would leave every tonne where it is and warn on every build.
    # These tests pin the balance, not the cap, which test_n_human.R pins.
    human_n_method_local_residual = "uncapped",
    cropland_ha = .nb_cropland_ha(),
    cell_polity = .nb_cell_polity(),
    ag_land_support = .nb_ag_land_support(),
    carbon_balance = .nb_carbon_balance(),
    primary_prod = .nb_primary_prod(),
    fertilizer = .nb_fertilizer(),
    crop_patterns = .nb_crop_patterns(),
    type_cropland = .nb_type_cropland(),
    residue_destiny_input = .nb_residue_destiny_input()
  )
}

# Joins loss/leaching drivers for every item_cbs_code present in the
# assembled n_inputs, so the join in build_nitrogen_balance() never drops a
# row for a driver mismatch (the tests use method = "ipcc"/"ipcc2019"/
# "ipcc_fracleach", the simplest methods, to keep driver requirements small).
# Built once per run (helper_n_balance.R): it is the same list every time.
.nb_data_with_drivers <- function() {
  memo_n_fixture("nb_data_with_drivers", .nb_build_data_with_drivers)
}

.nb_build_data_with_drivers <- function() {
  data <- .nb_full_data()
  n_inputs <- whep::build_n_inputs(data = data)
  item_codes <- unique(n_inputs$item_cbs_code)
  fert_types <- c(
    "Excreta",
    "Liquid",
    "Solid",
    "SOM",
    "Synthetic",
    "Human",
    "Recycling"
  )
  data$n_balance_drivers <- tidyr::expand_grid(
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      area_code = 10L,
      item_cbs_code = item_codes,
      year = 2010L
    ),
    fert_type = fert_types
  ) |>
    # `climate` and `irrig_type` feed the ATL/MED options; `moisture_regime`
    # feeds the IPCC 2019 wet/dry default. The fixture sets each driver
    # independently, and "wet" keeps the wet/dry leaching defined without the
    # rainfed/irrigated split most tests here switch off.
    dplyr::mutate(
      climate = "MED",
      irrig_type = "Rainfed",
      moisture_regime = "wet"
    )
  # Leaching runs on every balance row, including the output-only SOM
  # sequestration row that carries no crop (NA item_cbs_code).
  data$n_balance_leaching_drivers <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = union(item_codes, NA_integer_),
    year = 2010L,
    moisture_regime = "wet",
    climate = "MED",
    irrig_cat = "Rainfed",
    land_use = "Cropland",
    cn_input = NA_real_,
    tillage = "Not_specified",
    som_share = 0.03
  )
  data$drainage_mm <- 400
  data
}

# With no `data`, the default balance is built once per run and shared: many
# tests read one property each off that same balance. Passing `data` always
# builds afresh, so a test that changes an input really runs the builder.
.nb_run <- function(data = NULL, resolution = "grid") {
  if (is.null(data)) {
    return(memo_n_fixture(
      paste0("nb_run_", resolution),
      function() .nb_build(.nb_data_with_drivers(), resolution)
    ))
  }
  .nb_build(data, resolution)
}

.nb_build <- function(data, resolution) {
  whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach",
      regime = "none"
    ),
    resolution = resolution,
    data = data
  )
}

# build_n_inputs() on the unchanged driver fixture, built once per run.
.nb_inputs_with_drivers <- function() {
  memo_n_fixture(
    "nb_inputs_with_drivers",
    function() whep::build_n_inputs(data = .nb_data_with_drivers())
  )
}

# Tests --------------------------------------------------------------------

testthat::test_that("calculate_npp_carbon_nitrogen runs once per build_nitrogen_balance call", {
  # build_n_inputs()'s "recycling" term and this function's own prod_n_t
  # term both need calculate_npp_carbon_nitrogen()'s result; a prior
  # version recomputed it twice despite an .n_balance_npp() cache helper
  # because the cache was never populated before the first call site ran.
  # Prepare the fixture data BEFORE mocking: .nb_data_with_drivers() makes
  # its own build_n_inputs() call to derive driver rows, which is a
  # legitimate, separate call outside build_nitrogen_balance() and must
  # not be counted here.
  data <- .nb_data_with_drivers()
  call_count <- 0
  real_fn <- whep::calculate_npp_carbon_nitrogen
  testthat::local_mocked_bindings(
    calculate_npp_carbon_nitrogen = function(...) {
      call_count <<- call_count + 1
      real_fn(...)
    },
    .package = "whep"
  )
  .nb_run(data = data)
  testthat::expect_equal(call_count, 1L)
})

testthat::test_that("balance closes: n_balance_t equals input minus output", {
  out <- .nb_run()
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
})

testthat::test_that("surplus_t is never negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("output-only production keys are preserved", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 1
  )
  npp <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2513L,
    year = 2010L,
    product_n_t = 2
  )

  out <- whep:::.nb_add_prod_n(inputs, list(.npp_cache = npp), key)

  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2513L))
  testthat::expect_equal(out$prod_n_t[out$item_cbs_code == 2513L], 2)
  testthat::expect_equal(out$n_input_full_t[out$item_cbs_code == 2513L], 0)
})

testthat::test_that("output-only residue-destiny keys are preserved", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2807L,
    year = 2010L,
    n_input_full_t = 1
  )
  data <- list(residue_destiny_input = .nb_residue_destiny_input())

  out <- whep:::.nb_add_residue_destiny(inputs, data, key)

  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2807L))
  testthat::expect_equal(out$n_input_full_t[out$item_cbs_code == 2511L], 0)
  testthat::expect_false(anyNA(
    out$used_residue_n_t[out$item_cbs_code == 2511L]
  ))
})

testthat::test_that("bedding residue N leaves the field without leaking", {
  # whep#1005: bedding is carved out of the burn share, so the nitrogen it
  # carries stops being counted as burnt. It still LEAVES the field, so it has
  # to arrive as its own output term -- otherwise the removed nitrogen simply
  # vanishes from n_output_full_t and the surplus is overstated by it.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 1
  )
  bare <- whep:::.nb_add_residue_destiny(
    inputs,
    list(
      residue_destiny_input = .nb_residue_destiny_input(),
      residue_bedding = "fraction"
    ),
    key
  )
  bedded <- whep:::.nb_add_residue_destiny(
    inputs,
    list(
      residue_destiny_input = .nb_residue_destiny_input(),
      residue_bedding = "fraction",
      residue_bedding_fraction = 0.4
    ),
    key
  )
  removed <- function(x) {
    x$used_residue_n_t + x$bedding_residue_n_t + x$burnt_residue_n_t
  }
  testthat::expect_equal(bare$bedding_residue_n_t, 0)
  testthat::expect_gt(bedded$bedding_residue_n_t, 0)
  testthat::expect_lt(bedded$burnt_residue_n_t, bare$burnt_residue_n_t)
  testthat::expect_equal(removed(bedded), removed(bare))
})

testthat::test_that("bedded straw N comes back as a manure input", {
  # whep#1445: the bedding the balance books as leaving the field must return
  # in the housed manure it is mixed into. IPCC 2019 Eq. 10.34 puts bedding N
  # outside the storage loss term, so all of it reaches the land.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  manure <- c("manure_solid", "manure_liquid", "excreta")
  run <- function(fraction) {
    data <- .nb_full_data()
    data$residue_bedding <- "fraction"
    data$residue_bedding_fraction <- fraction
    inputs <- suppressWarnings(whep::build_n_inputs(data = data))
    removed <- inputs |>
      dplyr::distinct(dplyr::across(dplyr::all_of(key))) |>
      whep:::.nb_add_residue_destiny(data, key)
    list(
      manure_n = sum(inputs$n_input_t[inputs$fert_type %in% manure]),
      bedding_n = sum(removed$bedding_residue_n_t)
    )
  }
  bare <- run(0)
  bedded <- run(0.4)

  testthat::expect_gt(bedded$bedding_n, 0)
  testthat::expect_equal(
    bedded$manure_n - bare$manure_n,
    bedded$bedding_n,
    tolerance = 1e-9
  )
})

testthat::test_that("a balance without bedding passes no bedding supply", {
  # A zero bedding share is a declared choice, not an absent input: the manure
  # engine then runs exactly as before bedding existed.
  data <- .nb_full_data()
  data$residue_bedding <- "fraction"
  testthat::expect_null(whep:::.ni_manure_bedding(data))
  data$residue_destiny_input <- NULL
  data$residue_bedding_fraction <- 0.4
  testthat::expect_null(whep:::.ni_manure_bedding(data))
})

testthat::test_that("the bedding supply is national, keyed on the territory", {
  # The straw is booked where it grew, the manure where the animals are; the
  # national supply is spread over the housed streams by add_manure_bedding().
  data <- .nb_full_data()
  data$residue_bedding <- "fraction"
  data$residue_bedding_fraction <- 0.4
  supply <- whep:::.ni_manure_bedding(data)

  testthat::expect_named(
    supply,
    c("year", "territory", "bedding_dm_t", "bedding_c_t", "bedding_n_t")
  )
  testthat::expect_identical(supply$territory, "10")
})

testthat::test_that("recycling and the residue removals read one destiny argument set", {
  # The residue left on the field (recycled) and the residue that leaves it
  # (used, bedding, burnt) are the two halves of one destiny split, so both
  # paths must call it with the same method, recovery variant and bedding
  # fraction.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  residue <- dplyr::mutate(
    .nb_npp_input(),
    region_krausmann = "Western Europe",
    region_un_sub = "Western Europe"
  )
  data <- list(
    npp_n_input = residue,
    residue_destiny_input = residue,
    residue_destiny_recovery = "wirsenius",
    residue_bedding = "fraction",
    residue_bedding_fraction = 0.3
  )
  calls <- list()
  real <- whep::calculate_residue_destinies
  testthat::local_mocked_bindings(
    calculate_residue_destinies = function(x, ...) {
      calls[[length(calls) + 1L]] <<- list(...)
      real(x, ...)
    },
    .package = "whep"
  )
  npp <- whep:::.n_balance_npp(data)
  row <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 1
  )
  out <- whep:::.nb_add_residue_destiny(row, data, key)
  testthat::expect_length(calls, 2L)
  testthat::expect_identical(calls[[1]], calls[[2]])
  testthat::expect_identical(calls[[1]]$recovery, "wirsenius")
  testthat::expect_identical(calls[[1]]$bedding_fraction, 0.3)
  testthat::expect_identical(calls[[1]]$method, "recovery_regional")
  # What stays plus what leaves is the whole residue nitrogen.
  testthat::expect_equal(
    npp$residue_soil_n_t +
      out$used_residue_n_t +
      out$bedding_residue_n_t +
      out$burnt_residue_n_t,
    npp$residue_n_t
  )
  testthat::expect_gt(out$bedding_residue_n_t, 0)
})

testthat::test_that("the manure bedding reads the balance's one destiny split", {
  # Recycling, the residue removals and the bedding that comes back as manure
  # are read from one destiny split with one argument set, the recovery
  # variant left at the package default. What stays plus what leaves is the
  # gross residue N, and the bedded N that leaves the field is the bedded N
  # the manure input brings back.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  residue <- dplyr::mutate(
    .nb_npp_input(),
    region_krausmann = "Western Europe",
    region_un_sub = "Western Europe"
  )
  data <- list(
    npp_n_input = residue,
    residue_destiny_input = residue,
    residue_bedding = "fraction",
    residue_bedding_fraction = 0.3
  )
  calls <- list()
  real <- whep::calculate_residue_destinies
  default <- eval(formals(real)$recovery)[[1]]
  testthat::local_mocked_bindings(
    calculate_residue_destinies = function(x, ...) {
      calls[[length(calls) + 1L]] <<- list(...)
      real(x, ...)
    },
    .package = "whep"
  )
  npp <- whep:::.n_balance_npp(data)
  row <- dplyr::distinct(residue, dplyr::across(dplyr::all_of(key)))
  out <- whep:::.nb_add_residue_destiny(row, data, key)
  supply <- whep:::.ni_manure_bedding(data)

  testthat::expect_length(calls, 3L)
  testthat::expect_identical(calls[[1]], calls[[2]])
  testthat::expect_identical(calls[[1]], calls[[3]])
  testthat::expect_identical(calls[[1]]$recovery, default)
  testthat::expect_gt(out$bedding_residue_n_t, 0)
  testthat::expect_equal(
    npp$residue_soil_n_t +
      out$used_residue_n_t +
      out$bedding_residue_n_t +
      out$burnt_residue_n_t,
    npp$residue_n_t,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    sum(supply$bedding_n_t),
    sum(out$bedding_residue_n_t),
    tolerance = 1e-12
  )
})

testthat::test_that("bedded straw N is priced as applied manure, not as excreta", {
  # add_manure_bedding() places the straw on the housed, litter-using streams
  # only, so it lands as "manure_solid" and IPCC 2019 Table 11.1 prices it with
  # EF1 for other N inputs, never with the EF3PRP of grazing deposition. The
  # excreta rows and their species mix must come out untouched. The drivers
  # are set dry, where EF1 (0.005) differs from both EF3PRP factors.
  key <- whep:::.nb_key("grid")
  run <- function(fraction) {
    data <- .nb_data_with_drivers()
    data$residue_bedding <- "fraction"
    data$residue_bedding_fraction <- fraction
    data$n_balance_drivers$moisture_regime <- "dry"
    inputs <- suppressWarnings(whep::build_n_inputs(data = data))
    loss <- whep:::.nb_loss_rows(inputs, key, data) |>
      whep::calculate_soil_n2o(method = "ipcc2019")
    list(inputs = inputs, loss = loss)
  }
  type_n <- function(x, type, col = "n_input_t") {
    sum(x[[col]][x$fert_type == type])
  }
  excreta <- function(x) {
    x |>
      dplyr::filter(.data$fert_type %in% c("excreta", "Excreta")) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  }
  bare <- run(0)
  bedded <- run(0.4)
  bedding_n <- type_n(bedded$inputs, "manure_solid") -
    type_n(bare$inputs, "manure_solid")
  manure <- c("manure_solid", "manure_liquid", "excreta")

  testthat::expect_gt(bedding_n, 0)
  testthat::expect_equal(
    sum(bedded$inputs$n_input_t[bedded$inputs$fert_type %in% manure]) -
      sum(bare$inputs$n_input_t[bare$inputs$fert_type %in% manure]),
    bedding_n,
    tolerance = 1e-12
  )
  testthat::expect_gt(nrow(excreta(bare$inputs)), 0L)
  testthat::expect_identical(excreta(bedded$inputs), excreta(bare$inputs))
  testthat::expect_identical(excreta(bedded$loss), excreta(bare$loss))
  solid <- dplyr::filter(bedded$loss, .data$fert_type == "Solid")
  testthat::expect_true(all(solid$method_soil_n2o == "ipcc2019_wet_dry"))
  testthat::expect_equal(
    type_n(bedded$loss, "Solid", "n2o_direct_n_t") -
      type_n(bare$loss, "Solid", "n2o_direct_n_t"),
    bedding_n * whep:::.ipcc2019_soil_ef("EF1", "all_dry"),
    tolerance = 1e-12
  )
})

testthat::test_that("bedding residue N is an output of the full balance", {
  aggregated <- whep:::.nb_output_aggregates(tibble::tibble(
    prod_n_t = 10,
    used_residue_n_t = 2,
    bedding_residue_n_t = 3,
    burnt_residue_n_t = 1,
    grazed_weeds_n_t = 0,
    som_sequestration_n_t = 0,
    nh3_n_t = 0
  ))
  testthat::expect_equal(aggregated$n_output_full_t, 16)
})

testthat::test_that("duplicate loss-driver keys abort instead of duplicating emissions", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  n_inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    fert_type = "synthetic",
    n_input_t = 10
  )
  drivers <- tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(50.25, 50.25),
    area_code = c(10L, 10L),
    item_cbs_code = c(2511L, 2511L),
    year = c(2010L, 2010L),
    fert_type = c("Synthetic", "Synthetic"),
    climate = c("MED", "MED")
  )

  testthat::expect_error(
    whep:::.nb_loss_rows(n_inputs, key, list(n_balance_drivers = drivers))
  )
})

testthat::test_that("a balance with no loss-relevant inputs gets zero losses", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  balance <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L
  )
  n_inputs <- dplyr::mutate(balance, fert_type = "bnf", n_input_t = 10)
  methods <- list(
    nh3 = "manner",
    n2o = "ipcc2019",
    leaching = "meisinger_drainage"
  )

  out <- whep:::.nb_losses(balance, n_inputs, key, methods, list())

  testthat::expect_equal(out$nh3_n_t, 0)
  testthat::expect_equal(out$n2o_direct_n_t, 0)
  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 0)
})

testthat::test_that("loss methods are validated even when no loss rows exist", {
  testthat::expect_error(whep:::.nb_methods(list(nh3 = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(n2o = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(leaching = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(nh_3 = "ipcc")))
})

testthat::test_that("an unknown methods name aborts with its own name (#621)", {
  # The bare `expect_error()` above is why this survived: cli's own formatter
  # error is still an error. The plural marker sat ahead of both
  # interpolations, so cli deferred it and then refused the message for
  # carrying more than one candidate quantity -- "Multiple quantities for
  # pluralization", a simpleError, on every unknown name. Assert the class and
  # the content, not just that something was thrown. Two unknown names, so the
  # plural branch is the one exercised.
  cnd <- testthat::expect_error(
    whep:::.nb_methods(list(nh_3 = "ipcc", n20 = "ipcc2019")),
    class = "rlang_error"
  )
  testthat::expect_match(conditionMessage(cnd), "nh_3")
  testthat::expect_match(conditionMessage(cnd), "n20")
  testthat::expect_match(conditionMessage(cnd), "names")
  # One unknown name takes the singular branch and must still name it.
  cnd1 <- testthat::expect_error(
    whep:::.nb_methods(list(nh_3 = "ipcc")),
    class = "rlang_error"
  )
  testthat::expect_match(conditionMessage(cnd1), "name:")
  testthat::expect_match(conditionMessage(cnd1), "nh_3")
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nue_std", "nue_som", "nue_useful", "surplus_share", "method_nh3")
  )
})

testthat::test_that("example fixture carries a positive area_ha keyed to the crop", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(out, "area_ha")
  # area_ha is explicit physical agricultural support, keyed to the crop.
  testthat::expect_true(all(out$area_ha > 0))
  testthat::expect_true(rlang::has_name(out, "item_cbs_code"))
})

testthat::test_that("build_nitrogen_balance emits per-crop area_ha on the grid key", {
  # Explicit agricultural support supplies the physical crop area (1000 ha
  # for item_cbs 2511). It must survive on the grid row so boundary pressure
  # is not computed from harvested-area proxies.
  out <- .nb_run()
  pointblank::expect_col_exists(out, "area_ha")
  crop <- dplyr::filter(out, !is.na(item_cbs_code), item_cbs_code == 2511L)
  testthat::expect_equal(nrow(crop), 1L)
  testthat::expect_equal(crop$area_ha, 1000)
  testthat::expect_true(all(crop$area_ha > 0))
})

testthat::test_that("resolution = \"polity\" sums area_ha per key over cells", {
  grid <- .nb_run(resolution = "grid")
  polity <- .nb_run(resolution = "polity")
  pointblank::expect_col_exists(polity, "area_ha")
  # Single grid cell, so physical crop area is conserved across resolutions.
  testthat::expect_equal(
    sum(polity$area_ha),
    sum(grid$area_ha),
    tolerance = 1e-6
  )
  crop_polity <- dplyr::filter(
    polity,
    !is.na(item_cbs_code),
    item_cbs_code == 2511L
  )
  testthat::expect_equal(crop_polity$area_ha, 1000)
})

testthat::test_that("example fixture closes and has non-negative surplus", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("the N-limitation SOM cap engages and recomputes every downstream value", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")

  # Directly test the private two-pass sequence on a constructed scenario:
  # n_balance_t < 0 (deficit) and som_sequestration_n_t > 0 before the cap.
  before_cap <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 50,
    n_input_full_nosom_t = 45,
    n_input_std_t = 45,
    n_input_som_t = 48,
    n_input_for_n2o_t = 40,
    prod_n_t = 20,
    used_residue_n_t = 5,
    bedding_residue_n_t = 0,
    burnt_residue_n_t = 2,
    grazed_weeds_n_t = 3,
    som_sequestration_n_t = 10, # would push n_output_full_t above 50+
    nh3_n_t = 30
  )
  pass1 <- whep:::.nb_indicators_pass1(before_cap)
  # n_output_full_t sums to 70 (20 + 5 + 2 + 3 + 30 + 10) against an input
  # of 50, so n_balance_t is negative.
  testthat::expect_lt(pass1$n_balance_t, 0)
  testthat::expect_gt(pass1$som_sequestration_n_t, 0)

  capped <- whep:::.nb_cap_som(pass1)
  # Capped som is pmax(0, 10 + n_balance_t), i.e. 0 here, strictly smaller
  # than the uncapped value of 10.
  testthat::expect_lt(capped$som_sequestration_n_t, pass1$som_sequestration_n_t)
  testthat::expect_equal(capped$som_sequestration_n_t, 0)

  # Recompute every downstream output/balance value from the capped SOM (the
  # SAME .nb_output_aggregates()/.nb_balance() helpers the real pipeline
  # uses), confirming nothing downstream is left stale.
  recomputed <- capped |>
    whep:::.nb_output_aggregates() |>
    whep:::.nb_balance()
  expected_output_full <- 20 + 5 + 2 + 3 + 30 + 0
  testthat::expect_equal(recomputed$n_output_full_t, expected_output_full)
  testthat::expect_equal(
    recomputed$n_balance_t,
    50 - expected_output_full,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    recomputed$surplus_t,
    pmax(0, 50 - expected_output_full)
  )
  # The stale pre-cap n_output_full_t (70) must not survive anywhere.
  testthat::expect_false(isTRUE(all.equal(recomputed$n_output_full_t, 70)))
})

testthat::test_that("resolution = \"polity\" re-aggregates resolution = \"grid\"", {
  grid <- .nb_run(resolution = "grid")
  polity <- .nb_run(resolution = "polity")

  testthat::expect_true(nrow(polity) > 0)
  # Mass-conserving input/output totals must match exactly between
  # resolutions (both were built from the same single grid cell).
  testthat::expect_equal(
    sum(polity$n_input_full_t),
    sum(grid$n_input_full_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$prod_n_t),
    sum(grid$prod_n_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$n_output_full_t),
    sum(grid$n_output_full_t),
    tolerance = 1e-6
  )
})

testthat::test_that("NUE denominators are not accidentally collapsed", {
  x <- tibble::tibble(
    n_input_std_t = 100,
    n_input_full_t = 90, # deliberately != n_input_std_t and n_input_som_t
    n_input_som_t = 80,
    prod_n_t = 40,
    n_output_residues_t = 50,
    n_output_som_t = 45,
    n_output_useful_t = 60,
    n_output_full_t = 70
  )
  out <- whep:::.nb_nue(x)

  testthat::expect_equal(out$nue_std, 40 / 100)
  testthat::expect_equal(out$nue_residues, 50 / 100)
  # nue_som and nue_useful divide by n_input_full_t (90), NOT n_input_std_t
  # (100) or n_input_som_t (80).
  testthat::expect_equal(out$nue_som, 45 / 90)
  testthat::expect_equal(out$nue_useful, 60 / 90)
  # nue_full divides by n_input_som_t (80), a THIRD distinct denominator.
  testthat::expect_equal(out$nue_full, 70 / 80)
  testthat::expect_false(isTRUE(all.equal(out$nue_som, 45 / 80)))
  testthat::expect_false(isTRUE(all.equal(out$nue_full, 70 / 90)))
})

testthat::test_that("zero-input balance rows have undefined, not infinite, ratios", {
  balance <- tibble::tibble(
    n_input_full_t = 0,
    n_output_full_t = 2
  ) |>
    whep:::.nb_balance()
  nue <- tibble::tibble(
    n_input_std_t = 0,
    n_input_full_t = 0,
    n_input_som_t = 0,
    prod_n_t = 2,
    n_output_residues_t = 2,
    n_output_som_t = 2,
    n_output_useful_t = 2,
    n_output_full_t = 2
  ) |>
    whep:::.nb_nue()

  testthat::expect_true(is.na(balance$surplus_share))
  testthat::expect_true(all(is.na(dplyr::select(
    nue,
    dplyr::starts_with("nue_")
  ))))
})

testthat::test_that("method_nh3/method_soil_n2o/method_leaching are stamped", {
  out <- .nb_run()
  testthat::expect_true(all(out$method_nh3 == "ipcc"))
  # Each soil-N2O stamp names the factor its loss rows took; a row with no
  # loss row says so.
  stamps <- unlist(strsplit(out$method_soil_n2o, "|", fixed = TRUE))
  testthat::expect_true(all(
    stamps %in%
      c("ipcc2019_no_loss_row", "ipcc2019_wet_dry", "ipcc2019_ef3prp_blend")
  ))
  testthat::expect_true(all(out$method_leaching == "ipcc_fracleach_wet"))
})

testthat::test_that("a balance row with no loss row is stamped as such", {
  # A row with no N2O-relevant input forms no loss row, so it emits nothing
  # and no factor was applied to it; its stamp must not read as a priced
  # row's, which under several methods is the bare method name.
  out <- .nb_run()
  no_loss <- out$method_soil_n2o == "ipcc2019_no_loss_row"
  testthat::expect_true(any(no_loss))
  testthat::expect_true(all(out$n_input_for_n2o_t[no_loss] == 0))
  testthat::expect_true(all(out$n2o_direct_n_t[no_loss] == 0))
  testthat::expect_true(all(out$n2o_indirect_nh3_n_t[no_loss] == 0))
  testthat::expect_true(all(
    out$method_n2o_indirect_nh3[no_loss] == "ipcc2019_no_loss_row"
  ))
  testthat::expect_false(any(out$method_soil_n2o == "ipcc2019"))
})

testthat::test_that("resolution argument is validated", {
  testthat::expect_error(
    whep::build_nitrogen_balance(resolution = "province", data = list()),
    "resolution"
  )
})

testthat::test_that("gwp argument is validated and total_gwp_co2e_kg is non-negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$total_gwp_co2e_kg >= 0))
  testthat::expect_error(
    whep::build_nitrogen_balance(gwp = "ar99", data = list())
  )
})

testthat::test_that("total_gwp_co2e_kg matches the 44/28 x GWP x 1000 formula", {
  # .nb_gwp() sums the three N2O-N terms (kg... actually tonnes N), converts
  # N2O-N -> N2O via the 44/28 molecular-mass ratio (whep:::.soil_n2o_factors()
  # $n_to_n2o), applies the N2O GWP factor from whep:::.ghg_gwp_factors(gwp),
  # then scales tonnes -> kilograms (x1000). Pulling both factors from their
  # actual source (rather than hardcoding guessed numbers) means this test
  # fails if either constant, or the x1000 scaling, silently drifts.
  n_to_n2o <- whep:::.soil_n2o_factors()$n_to_n2o
  gwp_n2o_ar6 <- whep:::.ghg_gwp_factors("ar6")[["n2o"]]
  gwp_n2o_ar5 <- whep:::.ghg_gwp_factors("ar5")[["n2o"]]

  x <- tibble::tibble(
    n2o_direct_n_t = 2,
    n2o_indirect_nh3_n_t = 0.5,
    n2o_indirect_no3_n_t = 0.3
  )
  total_n2o_n_t <- 2 + 0.5 + 0.3

  out_ar6 <- whep:::.nb_gwp(x, gwp = "ar6")
  expected_ar6 <- total_n2o_n_t * n_to_n2o * gwp_n2o_ar6 * 1000
  testthat::expect_equal(
    out_ar6$total_gwp_co2e_kg,
    expected_ar6,
    tolerance = 1e-9
  )

  out_ar5 <- whep:::.nb_gwp(x, gwp = "ar5")
  expected_ar5 <- total_n2o_n_t * n_to_n2o * gwp_n2o_ar5 * 1000
  testthat::expect_equal(
    out_ar5$total_gwp_co2e_kg,
    expected_ar5,
    tolerance = 1e-9
  )
  testthat::expect_false(isTRUE(all.equal(expected_ar6, expected_ar5)))
})

testthat::test_that("SOM sequestration is kept when no NA-item input row exists", {
  # A cell in net carbon GAIN (son_change_kgn_ha < 0) emits no
  # som_mineralization input row; if it also lacks deposition/human N, x has
  # no NA-item row. A left join would drop the sequestration output entirely;
  # the full-join merge must keep it (as its own NA-item row).
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L, # a real crop row only; NO NA-item row present
    year = 2010L,
    prod_n_t = 40,
    used_residue_n_t = 5,
    burnt_residue_n_t = 2,
    grazed_weeds_n_t = 0,
    n_input_full_t = 100
  )
  cb <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    land_use = "Cropland",
    year = 2010L,
    area_ha = 1000,
    son_change_kgn_ha = -0.4 # negative => sequestration = 0.4 * 1000 / 1000
  )
  out <- whep:::.nb_add_som_sequestration(x, list(carbon_balance = cb), key)
  testthat::expect_equal(sum(out$som_sequestration_n_t), 0.4)
  # The sequestration lands on its own NA-item row, not the crop row.
  na_row <- out[is.na(out$item_cbs_code), ]
  testthat::expect_equal(nrow(na_row), 1L)
  testthat::expect_equal(na_row$som_sequestration_n_t, 0.4)
  # Input aggregates carried onto the output-only row default to 0, not NA.
  testthat::expect_false(anyNA(out$n_input_full_t))
  testthat::expect_false(anyNA(out$prod_n_t))
})

testthat::test_that("grazed weeds key the grass sentinel (3000L), joining grass rows", {
  # build_n_inputs()'s manure engine keys grass rows to item_cbs_code 3000L;
  # grazed weeds must use the SAME sentinel so they attach to the grass row
  # rather than an NA code that never matches it.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 3000L, # grass row from the manure engine; NO NA-item row
    year = 2010L,
    prod_n_t = 0,
    used_residue_n_t = 0,
    burnt_residue_n_t = 0,
    n_input_full_t = 20
  )
  intake <- tibble::tibble(
    year = 2010L,
    territory = "10",
    sub_territory = "0.25_50.25",
    feed_quality = "grass",
    intake_dm_t = 600
  )
  weed_coef <- whep::whep_coef_table("weed_coefs")$residue_n_kgdm_weed
  out <- whep:::.nb_add_grazed_weeds(x, list(livestock_intake = intake), key)
  # 600 * weed_coef of grazed-weeds N, attached to the single grass row
  # (no phantom NA row created).
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$item_cbs_code, 3000L)
  testthat::expect_equal(out$grazed_weeds_n_t, 600 * weed_coef)
})

testthat::test_that("duplicate n_balance_leaching_drivers keys abort the join", {
  # A caller-supplied leaching-drivers table with a duplicate balance key
  # would fan x out and misalign the balance-key-aligned drainage_mm vector;
  # the many-to-one join must abort at the source instead.
  key <- c("area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    area_code = c(10L, 10L),
    item_cbs_code = c(2511L, 2513L),
    year = 2010L
  )
  drivers_dup <- tibble::tibble(
    area_code = 10L,
    item_cbs_code = c(2511L, 2511L, 2513L), # 2511 duplicated
    year = 2010L,
    climate = "MED"
  )
  testthat::expect_error(
    whep:::.nb_leaching_join(x, drivers_dup, key),
    "match at most 1 row"
  )
  # A unique table joins without error (one row per balance key).
  drivers_ok <- tibble::tibble(
    area_code = 10L,
    item_cbs_code = c(2511L, 2513L),
    year = 2010L,
    climate = "MED"
  )
  testthat::expect_equal(nrow(whep:::.nb_leaching_join(x, drivers_ok, key)), 2L)
})

# ---- C3b: the deposition scope reaches the published balance ------------

# whep#532: nitrogen that reached agricultural land but no single crop is a
# real mass, and a balance that spread it over cropland, over all agricultural
# land, or dropped it, is three different numbers with no way to tell them
# apart unless the choice travels with the rows.
testthat::test_that("the balance names the unattributed policy it used", {
  out <- .nb_run()

  testthat::expect_true(rlang::has_name(out, "method_unattributed"))
  testthat::expect_setequal(
    stats::na.omit(out$method_unattributed),
    "cropland_area"
  )
})

testthat::test_that("C3b: the balance names the deposition scope it used", {
  # DA-14 makes deposition scope a choice, and the two choices differ by
  # about 1.4% of the deposition term on real input. A balance that does not
  # carry the stamp is indistinguishable from one built under the other
  # scope, which is exactly how two conventions coexist unnoticed -- the
  # defect EA4 documents for the crosswalks.
  out <- .nb_run()

  testthat::expect_true(rlang::has_name(out, "method_deposition_scope"))
  testthat::expect_setequal(
    stats::na.omit(out$method_deposition_scope),
    "territory"
  )
})

testthat::test_that("C3b: the land scope reaches the balance and moves it", {
  data <- .nb_data_with_drivers()
  data$cell_polity <- dplyr::mutate(
    .nb_cell_polity(),
    polity_area_ha = 2000,
    land_area_ha = 1200,
    inland_water_ha = 500,
    ice_area_ha = 300
  )
  territory <- .nb_run(data)
  data$deposition_scope <- "land"
  land <- .nb_run(data)

  testthat::expect_setequal(
    stats::na.omit(land$method_deposition_scope),
    "land"
  )
  # The stamp is not decorative: the terrestrial scope really does charge
  # less nitrogen, so a balance carrying "land" is a different number from
  # one carrying "territory".
  testthat::expect_true(
    sum(land$n_input_full_t) < sum(territory$n_input_full_t)
  )
  # And decomposing the support without asking for the land scope moves
  # nothing at all, which is what DA-14's "keep whole" decision means.
  testthat::expect_equal(
    sum(territory$n_input_full_t),
    sum(.nb_run()$n_input_full_t)
  )
})

testthat::test_that("the balance names the deposition product it used", {
  # whep#1105, the other half of the deposition provenance. The scope stamp
  # says which territory the term was credited with; this one says which
  # FIELD it came from. HaNi reproduces only a fraction of Europe's observed
  # decline (whep#1097/#1121), so a corrected field is exactly what a caller
  # is expected to inject -- and a balance built on one has to be tellable
  # apart from a balance built on raw HaNi.
  out <- .nb_run()
  testthat::expect_true(rlang::has_name(out, "method_deposition"))
  # The fixture's nhx/noy carry no tag, so build_n_deposition() records
  # "supplied" rather than relabelling them "hani".
  testthat::expect_setequal(stats::na.omit(out$method_deposition), "supplied")

  data <- .nb_data_with_drivers()
  data$nhx <- dplyr::mutate(.nb_nhx(), method_deposition = "hani_emep")
  data$noy <- dplyr::mutate(.nb_noy(), method_deposition = "hani_emep")
  corrected <- .nb_run(data)

  testthat::expect_setequal(
    stats::na.omit(corrected$method_deposition),
    "hani_emep"
  )
  # Renaming the field moves no number: this is provenance, not a method.
  testthat::expect_equal(
    sum(corrected$n_input_full_t),
    sum(out$n_input_full_t)
  )
})

# polity_validity threading (whep#727) ----------------------------------------

testthat::test_that("build_nitrogen_balance forwards polity_validity down", {
  # Prepared before mocking, so the shared fixture is never built under it.
  data <- .nb_data_with_drivers()
  seen <- new.env(parent = emptyenv())
  real_n_inputs <- whep::build_n_inputs
  testthat::local_mocked_bindings(
    build_n_inputs = function(...) {
      args <- list(...)
      seen$n_inputs <- args$polity_validity
      do.call(real_n_inputs, args)
    }
  )
  out <- whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach",
      regime = "none"
    ),
    polity_validity = "flag",
    data = data
  )
  testthat::expect_equal(seen$n_inputs, "flag")
  pointblank::expect_col_exists(out, "reporting_polity_out_of_span")
  testthat::expect_type(out$reporting_polity_out_of_span, "logical")
})

testthat::test_that("the balance keeps every row by default", {
  out <- .nb_run()
  testthat::expect_false(
    "reporting_polity_out_of_span" %in% names(out)
  )
})

testthat::test_that("the balance example honours polity_validity", {
  out <- whep::build_nitrogen_balance(example = TRUE, polity_validity = "flag")
  pointblank::expect_col_exists(out, "reporting_polity_out_of_span")
})

testthat::test_that("build_nitrogen_balance validates polity_validity", {
  testthat::expect_error(
    whep::build_nitrogen_balance(
      polity_validity = "discard",
      data = list()
    ),
    "polity_validity"
  )
})

testthat::test_that("the fert_type bridge covers the whole input vocabulary", {
  # Invariant, not a transcription of the mapping: every fert_type the wide
  # balance schema guarantees must have a Title-case counterpart, or
  # .nb_loss_rows() would join NA against data$n_balance_drivers and lose the
  # driver columns. Pinned here because whep#850 rewrote the bridge off the
  # deprecated dplyr::case_match().
  vocabulary <- names(whep:::.nb_ensure_fert_cols(tibble::tibble()))
  bridged <- whep:::.nb_loss_fert_type(vocabulary)

  testthat::expect_false(anyNA(bridged))
  testthat::expect_equal(length(unique(bridged)), length(vocabulary))
  # Anything outside the vocabulary falls through to NA rather than to a wrong
  # label: an unmatched value must stay visible.
  testthat::expect_true(is.na(whep:::.nb_loss_fert_type("bogus")))
  testthat::expect_true(is.na(whep:::.nb_loss_fert_type(NA_character_)))
  testthat::expect_equal(whep:::.nb_loss_fert_type(character()), character())
})

testthat::test_that("a manufactured crop-less row carries no pressure", {
  # .nb_merge_output_term()'s full join CREATES a balance row when an output
  # term has no input row to attach to, and .nb_add_som_sequestration() books
  # its term at item_cbs_code = NA. Those rows are real, and one hop
  # downstream build_n_boundary_exceedance() filters them out -- a join that
  # manufactures rows a later filter removes. This pins the reason that round
  # trip costs nothing: every numeric column on such a row is the join's zero
  # fill except the sequestration itself, and .nb_cap_som() caps that to
  # pmax(0, inputs - other outputs) = 0. If any of those three mechanisms
  # changes, the exceedance filter starts deleting real nitrogen (#1173).
  carbon <- .nb_carbon_balance()
  booked <- sum(pmax(0, -carbon$son_change_kgn_ha) * carbon$area_ha / 1000)
  # Not vacuous: the fixture really does book sequestration at the NA item.
  testthat::expect_gt(booked, 0)

  out <- .nb_run()
  crop_less <- dplyr::filter(out, is.na(.data$item_cbs_code))
  testthat::expect_gt(nrow(crop_less), 0)
  testthat::expect_equal(sum(crop_less$som_sequestration_n_t), 0)
  testthat::expect_equal(sum(crop_less$n_input_full_t), 0)
  testthat::expect_equal(sum(crop_less$n_balance_t), 0)

  surplus <- whep::calculate_n_surplus(out)
  crop_less_surplus <- dplyr::filter(surplus, is.na(.data$item_cbs_code))
  testthat::expect_equal(sum(crop_less_surplus$surplus_n_t), 0)
  testthat::expect_equal(sum(crop_less_surplus$n_input_std_t), 0)
})

testthat::test_that("a grid balance refuses an incomplete input key", {
  # The other half of the same guarantee: crop-less rows cannot arrive from
  # the INPUT side of a gridded balance, so the manufactured rows above are
  # the only ones there are.
  data <- .nb_data_with_drivers()
  inputs <- .nb_inputs_with_drivers()
  data$n_inputs <- dplyr::bind_rows(
    inputs,
    dplyr::mutate(
      inputs[1, ],
      item_cbs_code = NA_integer_,
      n_input_t = 1000
    )
  )
  testthat::expect_error(.nb_run(data = data), "complete grid and item keys")
})

# ---- whep#1034: removals that go absent must not become zero ---------------
#
# Each output term below is merged onto the balance by a full join and
# zero-filled, and the balance is then input minus output. So a term that
# silently lands as zero leaves n_balance_t == n_input_full_t -
# n_output_full_t holding exactly: the identity cannot see it, and the surplus
# absorbs the whole lost removal. These tests assert the identity HOLDS on the
# zero-filled row and that the guard fires anyway.

.nb_zero_removal_row <- function() {
  tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 3000L,
    year = 2010L,
    n_input_full_t = 20,
    prod_n_t = 0,
    used_residue_n_t = 0,
    bedding_residue_n_t = 0,
    burnt_residue_n_t = 0,
    grazed_weeds_n_t = 0,
    nh3_n_t = 0,
    som_sequestration_n_t = 0
  )
}

# The balance key alone, before any output term is merged onto it.
.nb_bare_key_row <- function(item_cbs_code = 3000L) {
  dplyr::mutate(
    dplyr::select(.nb_zero_removal_row(), lon:n_input_full_t),
    item_cbs_code = item_cbs_code
  )
}

.nb_balance_closes <- function(row) {
  out <- whep:::.nb_indicators_pass1(row)
  isTRUE(all.equal(out$n_balance_t, out$n_input_full_t - out$n_output_full_t))
}

.nb_grass_intake <- function(feed_quality = "grass", intake_dm_t = 600) {
  tibble::tibble(
    year = 2010L,
    territory = "10",
    sub_territory = "0.25_50.25",
    feed_quality = feed_quality,
    intake_dm_t = intake_dm_t
  )
}

testthat::test_that("a moved feed_quality label cannot pass as no grazing", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  expect_supplied_guard(
    identity = .nb_balance_closes(.nb_zero_removal_row()),
    guard = whep:::.nb_add_grazed_weeds(
      .nb_bare_key_row(),
      list(livestock_intake = .nb_grass_intake(feed_quality = "Grass")),
      key
    ),
    class = "whep_absent_label"
  )
})

testthat::test_that("grass rows that carry no dry matter are refused", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  expect_supplied_guard(
    identity = .nb_balance_closes(.nb_zero_removal_row()),
    guard = whep:::.nb_add_grazed_weeds(
      .nb_bare_key_row(),
      list(livestock_intake = .nb_grass_intake(intake_dm_t = NA_real_)),
      key
    )
  )
})

testthat::test_that("residue destinies no N coefficient joins are refused", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  destiny <- dplyr::mutate(
    .nb_residue_destiny_input(),
    item_prod_code = "0015"
  )
  expect_supplied_guard(
    identity = .nb_balance_closes(.nb_zero_removal_row()) &&
      sum(destiny$residue_dm_t) > 0,
    guard = whep:::.nb_add_residue_destiny(
      .nb_bare_key_row(2511L),
      list(residue_destiny_input = destiny),
      key
    )
  )
})

# ---- Rainfed/irrigated split -----------------------------------------------

.nb_run_regime <- function(regime, data = .nb_data_with_drivers()) {
  whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach",
      regime = regime
    ),
    data = data
  )
}

# Shares for every grid key of the unsplit balance, so the split is offline.
.nb_regime_share_fixture <- function(unsplit, area = 0.3, yield = 0.6) {
  unsplit |>
    dplyr::distinct(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_cbs_code,
      .data$year
    ) |>
    dplyr::mutate(
      irrigated_area_share = area,
      irrigated_yield_share = yield
    )
}

# The unsplit balance, the share fixture built on it, and the yield split it
# feeds are the same in every regime test, so each is built once per run.
# regime = "none" on the default data is exactly the call .nb_run() makes.
.nb_regime_unsplit <- function() {
  .nb_run()
}

.nb_regime_split_data <- function() {
  memo_n_fixture("nb_regime_split_data", function() {
    data <- .nb_data_with_drivers()
    data$regime_shares <- .nb_regime_share_fixture(.nb_regime_unsplit())
    data
  })
}

.nb_regime_yield_split <- function() {
  memo_n_fixture(
    "nb_regime_yield_split",
    function() .nb_run_regime("yield_split", data = .nb_regime_split_data())
  )
}

testthat::test_that("the regime split conserves the unsplit balance", {
  unsplit <- .nb_regime_unsplit()
  split <- .nb_regime_yield_split()
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  additive <- c(
    "n_input_full_t",
    "n_input_std_t",
    "prod_n_t",
    "used_residue_n_t",
    "area_ha",
    "grazed_weeds_n_t",
    "som_sequestration_n_t",
    "n_output_full_t",
    "n_balance_t",
    "nh3_n_t",
    "n2o_direct_n_t"
  )
  summed <- split |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(additive), sum),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  base <- unsplit |>
    dplyr::select(dplyr::all_of(c(key, additive))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key)))
  testthat::expect_equal(nrow(split), 2L * nrow(unsplit))
  for (col in additive) {
    testthat::expect_equal(summed[[col]], base[[col]], tolerance = 1e-9)
  }
  pointblank::expect_col_vals_in_set(
    split,
    "water_regime",
    c("rainfed", "irrigated")
  )
  testthat::expect_true(all(split$method_regime == "yield_split"))
  testthat::expect_true(all(split$method_regime_split == "regime_shares"))
})

testthat::test_that("the regime split weights production by the yield share", {
  unsplit <- .nb_regime_unsplit()
  split <- .nb_regime_yield_split()
  irrigated <- dplyr::filter(split, .data$water_regime == "irrigated")
  base <- unsplit |>
    dplyr::select("lon", "lat", "item_cbs_code", "prod_n_t", "area_ha") |>
    dplyr::rename(prod_base = "prod_n_t", area_base = "area_ha")
  joined <- dplyr::inner_join(
    irrigated,
    base,
    by = c("lon", "lat", "item_cbs_code")
  )
  testthat::expect_equal(joined$prod_n_t, 0.6 * joined$prod_base)
  testthat::expect_equal(joined$area_ha, 0.3 * joined$area_base)
})

testthat::test_that("area_split replaces the yield share by the area share", {
  unsplit <- .nb_regime_unsplit()
  split <- .nb_run_regime("area_split", data = .nb_regime_split_data())
  irrigated <- dplyr::filter(split, .data$water_regime == "irrigated")
  total <- sum(unsplit$prod_n_t)
  testthat::expect_equal(sum(irrigated$prod_n_t), 0.3 * total)
  testthat::expect_true(all(split$method_regime == "area_split"))
})

testthat::test_that("the regime split aborts when no shares can be built", {
  # A regime layer that covers none of the balance's cells: the real share
  # path runs and builds nothing. The layer is supplied, so the default
  # readers (the gridded crop areas and the country grid, both pins) are
  # never reached; the pin reader is stubbed to abort under its own class
  # so that a regression reaching one fails here instead of going online.
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      cli::cli_abort("Pin {.val {file_alias}} read.", class = "t_pin_read")
    },
    .download_pin_paths = function(file_alias, ...) {
      cli::cli_abort("Pin {.val {file_alias}} read.", class = "t_pin_read")
    },
    .package = "whep"
  )
  data <- .nb_data_with_drivers()
  data$regime_areas <- tibble::tibble(
    lon = 100.25,
    lat = -10.25,
    area_code = 10L,
    item_prod_code = 56L,
    year = 2010L,
    rainfed_ha = 50,
    irrigated_ha = 50
  )
  testthat::expect_error(
    .nb_run_regime("yield_split", data = data),
    class = "whep_regime_shares_missing"
  )
})

testthat::test_that("build_nitrogen_balance validates methods$regime", {
  testthat::expect_error(.nb_run_regime("tiled"), "regime")
})
testthat::test_that("the balance names the human-N population basis it used", {
  out <- .nb_run()
  testthat::expect_setequal(
    stats::na.omit(out$method_human_population),
    "urban_population"
  )
  testthat::expect_setequal(
    stats::na.omit(out$method_human_kgn_cap),
    "kg_n_per_urban_inhabitant"
  )

  data <- .nb_data_with_drivers()
  data$urban_population <- NULL
  data$total_population <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    year = 2010L,
    population = 30898536
  )
  data$human_n_population_basis <- "total"
  total <- .nb_run(data)
  testthat::expect_setequal(
    stats::na.omit(total$method_human_population),
    "total_population"
  )
  testthat::expect_setequal(
    stats::na.omit(total$method_human_kgn_cap),
    "kg_n_per_total_inhabitant"
  )
  # The default basis is the total population.
  data$human_n_population_basis <- NULL
  testthat::expect_identical(.nb_run(data), total)
})

testthat::test_that("inputs and drivers keyed by the former urban names balance the same", {
  # A table built before the rename says "urban" / "Urban" / method_urban_*.
  # Unrecognised, "urban" would get a pivot column outside every sum and be
  # skipped by the loss filter, so its nitrogen would leave the balance
  # silently; it is translated instead, with a deprecation warning.
  data <- .nb_data_with_drivers()
  current <- .nb_run()
  inputs <- .nb_inputs_with_drivers()
  legacy <- data
  legacy$n_inputs <- inputs |>
    dplyr::mutate(
      fert_type = dplyr::if_else(
        .data$fert_type == "human",
        "urban",
        .data$fert_type
      )
    ) |>
    dplyr::rename(
      method_urban_population = "method_human_population",
      method_urban_kgn_cap = "method_human_kgn_cap"
    )
  legacy$n_balance_drivers <- legacy$n_balance_drivers |>
    dplyr::mutate(
      fert_type = dplyr::if_else(
        .data$fert_type == "Human",
        "Urban",
        .data$fert_type
      )
    )
  testthat::expect_gt(sum(inputs$fert_type == "human"), 0L)
  suppressWarnings(testthat::expect_warning(
    out <- .nb_run(legacy),
    class = "whep_urban_fert_type_deprecated"
  ))
  testthat::expect_equal(out, current)
})

testthat::test_that("the default manure source reproduces the pre-option balance", {
  # Written by the code before the manure-source option existed (whep main at
  # 5421973b), relabelled for the human-N rename; see test_n_balance_inputs.R.
  # That code recycled the gross residue N; the recycling default is now the
  # soil-returned residue, so the gross basis is asked for to keep this
  # test's subject the manure source. Held at the as-shipped EF3 it was
  # written under (whep#601), and at the legacy residue feed share, which it
  # predates (whep#1398).
  golden <- readRDS(testthat::test_path(
    "fixtures",
    "n_inputs_default_golden.rds"
  ))
  # The golden predates the wet/dry methods: its N2O columns are the ATL/MED
  # option on this fixture's MED climate, and its FracLEACH is the 0.24 that
  # today's wet rows take. Built with those options, every number must still
  # match; only the stamps name the options more precisely now. It also
  # predates the sourced bedding share (whep#1005), so it is reproduced under
  # the rule it was written with: no bedding.
  data <- .nb_data_with_drivers()
  data$recycling_residue_basis <- "total_residue"
  data$methods <- list(losses = list(ef3_source = "as_shipped"))
  data$residue_feed_share <- "legacy"
  data$residue_bedding <- "fraction"
  out <- suppressMessages(whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019_atl_med",
      n2o_indirect = "ipcc2019_atl_med",
      leaching = "ipcc_fracleach",
      regime = "none"
    ),
    data = data
  ))
  # Rows with no N2O-relevant input carry the method and the reason.
  atl_med <- c("ipcc2019_atl_med", "ipcc2019_atl_med_no_loss_row")
  testthat::expect_true(all(out$method_soil_n2o %in% atl_med))
  testthat::expect_true(all(out$method_n2o_indirect_nh3 %in% atl_med))
  testthat::expect_true(all(out$method_leaching == "ipcc_fracleach_wet"))
  # Added after the golden: the retained-in-profile term (zero on wet rows),
  # the drivers' provenance and the class the leaching drivers carry.
  testthat::expect_true(all(out$retained_profile_n_t == 0))
  testthat::expect_true(all(out$method_drivers == "supplied"))
  testthat::expect_true(all(out$moisture_regime == "wet"))
  testthat::expect_identical(
    out |>
      dplyr::select(
        -"method_manure",
        -"method_n2o_indirect_nh3",
        -"retained_profile_n_t",
        -"method_drivers",
        -"moisture_regime"
      ) |>
      dplyr::mutate(
        method_soil_n2o = "ipcc2019",
        method_leaching = "ipcc_fracleach"
      ),
    golden$balance_grid
  )
  # Keys with no manure row carry no manure stamp.
  testthat::expect_identical(
    unique(stats::na.omit(out$method_manure)),
    "livestock_intake"
  )
})

testthat::test_that("the residue feed share moves only the residue split", {
  # whep#1398: switching the feed share moves recovered residue N between the
  # fed and the burned destinies. Everything else in the balance, the
  # surplus included, is identical.
  data <- .nb_data_with_drivers()
  data$residue_feed_share <- "legacy"
  legacy <- suppressMessages(.nb_build(data, "grid"))
  current <- suppressMessages(.nb_run())
  split <- c(
    "used_residue_n_t",
    "burnt_residue_n_t",
    "n_output_residues_t",
    "n_output_useful_t",
    "nue_residues",
    "nue_useful"
  )
  testthat::expect_identical(
    dplyr::select(current, -dplyr::all_of(split)),
    dplyr::select(legacy, -dplyr::all_of(split))
  )
  testthat::expect_false(identical(
    current$used_residue_n_t,
    legacy$used_residue_n_t
  ))
  testthat::expect_equal(
    current$used_residue_n_t + current$burnt_residue_n_t,
    legacy$used_residue_n_t + legacy$burnt_residue_n_t
  )
})

# whep#1078: the default ammonia method (MANNER) needs per-row driver columns
# that no reader in the package produces (only windspeed_ms has one,
# read_lpjml_wind()). Without them the balance used to assemble every input --
# the NPP chain, build_n_inputs() -- and only then abort inside calculate_nh3().
# The stub below stands for that expensive work: reaching it means the refusal
# came too late.
.nb_stub_assembly <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    .n_balance_npp = function(...) {
      cli::cli_abort("assembly reached", class = "nb_test_assembly_reached")
    },
    .package = "whep",
    .env = env
  )
}

testthat::test_that("the default bedding share returns its N as manure", {
  # whep#1005: the Wirsenius bedding share carves bedding out of the burnt
  # residue, so the residue leaving the field only changes term. Since
  # whep#1445 the bedded straw comes back in the housed manure, so the N input
  # rises by exactly the bedded N and nothing else enters.
  data <- .nb_data_with_drivers()
  data$residue_bedding <- "fraction"
  bare <- suppressMessages(.nb_run(data))
  bedded <- suppressMessages(.nb_run())
  testthat::expect_gt(sum(bedded$bedding_residue_n_t), 0)
  testthat::expect_equal(sum(bare$bedding_residue_n_t), 0)
  testthat::expect_equal(
    bedded$bedding_residue_n_t + bedded$burnt_residue_n_t,
    bare$burnt_residue_n_t
  )
  testthat::expect_equal(bedded$used_residue_n_t, bare$used_residue_n_t)
  testthat::expect_equal(
    sum(bedded$n_input_full_t) - sum(bare$n_input_full_t),
    sum(bedded$bedding_residue_n_t),
    tolerance = 1e-9
  )
})

testthat::test_that("default MANNER ammonia without drivers is refused before any assembly", {
  .nb_stub_assembly()
  testthat::expect_error(
    whep::build_nitrogen_balance(data = list()),
    class = "whep_error_nh3_drivers_absent"
  )
  testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = list(nh3 = "manner_default"),
      data = list()
    ),
    class = "whep_error_nh3_drivers_absent"
  )
})

testthat::test_that("the refusal names the IPCC alternative and the wind reader", {
  .nb_stub_assembly()
  err <- rlang::catch_cnd(
    whep::build_nitrogen_balance(data = list()),
    classes = "whep_error_nh3_drivers_absent"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "ipcc", fixed = TRUE)
  testthat::expect_match(msg, "read_lpjml_wind", fixed = TRUE)
})

testthat::test_that("MANNER drivers lacking windspeed_ms are refused before any assembly", {
  .nb_stub_assembly()
  drivers <- tibble::tribble(
    ~fert_type, ~manner_fertiliser, ~rainfall_mm, ~irrigated, ~technique,
    ~system, ~temp_c, ~incorporation_delay_h, ~species,
    "Liquid", "cattle_slurry", 40, FALSE, "Broadcast",
    "Arable", 15, 24, "Cattle"
  )
  err <- rlang::catch_cnd(
    whep::build_nitrogen_balance(data = list(n_balance_drivers = drivers)),
    classes = c("whep_error_nh3_missing_driver", "nb_test_assembly_reached")
  )
  testthat::expect_s3_class(err, "whep_error_nh3_missing_driver")
  testthat::expect_match(conditionMessage(err), "windspeed_ms", fixed = TRUE)
})

testthat::test_that("the entry check lets complete drivers and the IPCC method through", {
  .nb_stub_assembly()
  # Recycling/SOM never reach calculate_nh3() (.nb_losses() zeroes them), so an
  # NA manner_fertiliser there must not demand the organic driver set.
  drivers <- tibble::tribble(
    ~fert_type, ~manner_fertiliser, ~soil_ph, ~rate_kg_ha, ~rainfall_mm,
    ~irrigated, ~temp_c, ~temp_c_annual_mean,
    "Synthetic", "Urea", 6, 50, 0, FALSE, 8.6, 8.6,
    "Recycling", NA, NA, NA, NA, NA, NA, NA
  )
  # The default Meisinger leaching reads its own driver table, which the
  # caller supplies too: build_n_loss_drivers() has no Meisinger drivers yet.
  testthat::expect_error(
    whep::build_nitrogen_balance(
      data = list(
        n_balance_drivers = drivers,
        n_balance_leaching_drivers = tibble::tibble(fert_type = "Synthetic")
      )
    ),
    class = "nb_test_assembly_reached"
  )
  testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = list(nh3 = "ipcc", leaching = "ipcc_fracleach"),
      data = list()
    ),
    class = "nb_test_assembly_reached"
  )
})

# ---- IPCC 2019 wet/dry loss methods ---------------------------------------

.nb_run_methods <- function(data = .nb_data_with_drivers(), ...) {
  whep::build_nitrogen_balance(
    methods = utils::modifyList(
      list(nh3 = "ipcc", leaching = "ipcc_fracleach", regime = "none"),
      list(...)
    ),
    data = data
  )
}

testthat::test_that("the indirect N2O method is a validated methods entry", {
  m <- whep:::.nb_methods(list())
  testthat::expect_identical(m$n2o, "ipcc2019")
  testthat::expect_identical(m$n2o_indirect, "ipcc2019")
  testthat::expect_identical(
    whep:::.nb_methods(list(n2o_indirect = "ipcc2019_atl_med"))$n2o_indirect,
    "ipcc2019_atl_med"
  )
  testthat::expect_identical(
    whep:::.nb_methods(list(n2o = "ipcc2019_atl_med"))$n2o,
    "ipcc2019_atl_med"
  )
  testthat::expect_error(
    whep:::.nb_methods(list(n2o_indirect = "aguilera")),
    class = "rlang_error"
  )
})

testthat::test_that("the loss rows carry the N-weighted excreta share", {
  # 10 t at a CPP share of 0.2 and 30 t at 0.6 on one key: 0.5 by N.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  n_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~fert_type, ~n_input_t,
    ~excreta_cpp_share, ~method_excreta_species,
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 10, 0.2,
    "engine_livestock_category",
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 30, 0.6,
    "engine_livestock_category",
    0.25, 50.25, 10L, 3000L, 2010L, "synthetic", 5, NA, NA
  )
  rows <- whep:::.nb_loss_rows(n_inputs, key, list())
  excreta <- dplyr::filter(rows, .data$fert_type == "Excreta")
  testthat::expect_equal(excreta$n_input_t, 40)
  testthat::expect_equal(excreta$excreta_cpp_share, 0.5, tolerance = 1e-12)
  testthat::expect_identical(
    excreta$method_excreta_species,
    "engine_livestock_category"
  )
  testthat::expect_true(is.na(rows$excreta_cpp_share[
    rows$fert_type != "Excreta"
  ]))
  # One excreta row of unknown mix leaves the whole key without a share.
  n_inputs$excreta_cpp_share[2] <- NA
  n_inputs$method_excreta_species[2] <- NA
  unknown <- whep:::.nb_loss_rows(n_inputs, key, list())
  testthat::expect_true(is.na(
    unknown$excreta_cpp_share[unknown$fert_type == "Excreta"]
  ))
})

testthat::test_that("fallback-labelled excreta is pooled apart from grazing excreta", {
  # Collected manure labelled excreta only as a fallback is priced as
  # applied manure, so it must not pool with (and erase the mix of) the
  # grazing excreta of the same key.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  n_inputs <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~fert_type, ~n_input_t,
    ~excreta_cpp_share, ~method_excreta_species,
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 10, 0.2,
    "engine_livestock_category",
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 30, NA,
    "fallback_label_no_species",
    0.25, 50.25, 10L, 3000L, 2010L, "excreta", 5, NA,
    "fallback_label_no_species"
  )
  rows <- whep:::.nb_loss_rows(n_inputs, key, list())
  testthat::expect_identical(nrow(rows), 2L)
  grazing <- rows[
    rows$method_excreta_species %in% "engine_livestock_category",
  ]
  fallback <- rows[
    rows$method_excreta_species %in% "fallback_label_no_species",
  ]
  testthat::expect_equal(grazing$n_input_t, 10)
  testthat::expect_equal(grazing$excreta_cpp_share, 0.2)
  testthat::expect_equal(fallback$n_input_t, 35)
  testthat::expect_true(is.na(fallback$excreta_cpp_share))
  # Priced in the cascade: the grazing part at the wet EF3PRP blend, the
  # fallback part at wet EF1 for other N inputs (0.006).
  priced <- rows |>
    dplyr::mutate(moisture_regime = "wet") |>
    whep::calculate_soil_n2o(method = "ipcc2019")
  testthat::expect_equal(
    sum(priced$n2o_direct_n_t),
    10 * (0.2 * 0.006 + 0.8 * 0.003) + 35 * 0.006,
    tolerance = 1e-12
  )
})

testthat::test_that("the default balance prices its loss rows with the wet IPCC factors", {
  out <- .nb_run_methods()
  inputs <- .nb_inputs_with_drivers()
  n_of <- function(type) sum(inputs$n_input_t[inputs$fert_type == type])
  synthetic <- n_of("synthetic")
  others <- sum(purrr::map_dbl(
    c(
      "manure_liquid",
      "manure_solid",
      "som_mineralization",
      "human",
      "recycling"
    ),
    n_of
  ))
  excreta <- inputs[inputs$fert_type == "excreta", ]
  # Table 11.1: wet EF1 0.016 synthetic, 0.006 other; the grass excreta take
  # the EF3PRP blend of their CPP share (0.006 wet CPP, 0.003 SO).
  excreta_ef <- excreta$excreta_cpp_share *
    0.006 +
    (1 - excreta$excreta_cpp_share) * 0.003
  expected <- 0.016 *
    synthetic +
    0.006 * others +
    sum(excreta$n_input_t * excreta_ef)
  testthat::expect_gt(nrow(excreta), 0L)
  testthat::expect_equal(sum(out$n2o_direct_n_t), expected, tolerance = 1e-12)
  # Table 11.3: wet EF4 0.014 on every tonne volatilised.
  testthat::expect_equal(
    sum(out$n2o_indirect_nh3_n_t),
    0.014 * sum(out$nh3_n_t),
    tolerance = 1e-12
  )
  stamps <- unlist(strsplit(stats::na.omit(out$method_soil_n2o), "|", TRUE))
  testthat::expect_true(all(
    stamps %in%
      c("ipcc2019_no_loss_row", "ipcc2019_wet_dry", "ipcc2019_ef3prp_blend")
  ))
  grass <- out[out$item_cbs_code %in% 3000L, ]
  testthat::expect_identical(grass$method_soil_n2o, "ipcc2019_ef3prp_blend")
  pointblank::expect_col_vals_in_set(
    out,
    "method_n2o_indirect_nh3",
    c("ipcc2019_no_loss_row", "ipcc2019_wet_dry")
  )
})

testthat::test_that("the ATL/MED options reproduce the former balance", {
  # Golden totals of this fixture at commit bb604f8b, before the wet/dry
  # method took the `ipcc2019` name (MED, so EF1 0.005 and the Rainfed ef on
  # NH3). FracLEACH 0.24 on every row then equals today's wet rows. The
  # golden also predates the sourced bedding share (whep#1005), so it is
  # reproduced under the rule it was written with: no bedding, and it
  # recycled the gross residue N.
  data <- .nb_data_with_drivers()
  data$methods <- list(losses = list(ef3_source = "as_shipped"))
  data$residue_feed_share <- "legacy"
  data$residue_bedding <- "fraction"
  data$recycling_residue_basis <- "total_residue"
  out <- .nb_run_methods(
    data = data,
    n2o = "ipcc2019_atl_med",
    n2o_indirect = "ipcc2019_atl_med"
  )
  testthat::expect_equal(
    sum(out$n2o_direct_n_t),
    195.67972956176624,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    sum(out$n2o_indirect_nh3_n_t),
    22.162198946713094,
    tolerance = 1e-12
  )
  testthat::expect_equal(sum(out$no3_n_t), 8139.995079404959, tolerance = 1e-12)
  # Rows with no N2O-relevant input carry the method and the reason.
  atl_med <- c("ipcc2019_atl_med", "ipcc2019_atl_med_no_loss_row")
  testthat::expect_true(all(out$method_soil_n2o %in% atl_med))
  testthat::expect_true(all(out$method_n2o_indirect_nh3 %in% atl_med))
})

testthat::test_that("the balance refuses loss drivers without a moisture regime", {
  data <- .nb_data_with_drivers()
  data$n_balance_drivers$moisture_regime <- NULL
  testthat::expect_error(
    .nb_run_methods(data),
    class = "whep_missing_moisture_regime"
  )
})

testthat::test_that("a driver table keyed on the former excreta label is refused", {
  # build_n_inputs()'s excreta now reaches the losses as "Excreta"; drivers
  # still keyed on "Excreta_other" leave those rows without a moisture regime.
  data <- .nb_data_with_drivers()
  data$n_balance_drivers <- dplyr::mutate(
    data$n_balance_drivers,
    fert_type = dplyr::if_else(
      .data$fert_type == "Excreta",
      "Excreta_other",
      .data$fert_type
    )
  )
  err <- testthat::expect_error(
    .nb_run_methods(data),
    class = "whep_moisture_regime_na"
  )
  testthat::expect_match(cli::ansi_strip(conditionMessage(err)), "1 row")
})

testthat::test_that("dry irrigated parts leach and dry rainfed parts do not", {
  data <- .nb_regime_split_data()
  data$n_balance_leaching_drivers$moisture_regime <- "dry"
  out <- .nb_run_methods(data, regime = "yield_split")
  irrigated <- out$water_regime == "irrigated"
  testthat::expect_equal(
    out$no3_n_t[irrigated],
    0.24 * out$surplus_t[irrigated],
    tolerance = 1e-12
  )
  testthat::expect_true(all(out$no3_n_t[!irrigated] == 0))
  testthat::expect_true(all(
    out$method_leaching[irrigated] == "ipcc_fracleach_dry_irrigated"
  ))
  testthat::expect_true(all(
    out$method_leaching[!irrigated] == "ipcc_fracleach_dry_rainfed_retained"
  ))
  testthat::expect_gt(sum(out$surplus_t[irrigated]), 0)
})

testthat::test_that("an unsplit dry balance cannot choose a leaching fraction", {
  data <- .nb_data_with_drivers()
  data$n_balance_leaching_drivers$moisture_regime <- "dry"
  testthat::expect_error(
    .nb_run_methods(data),
    class = "whep_missing_water_regime"
  )
})

testthat::test_that("aguilera blends the grass excreta modifier in the balance", {
  out <- .nb_run_methods(
    n2o = "aguilera",
    n2o_indirect = "ipcc2019_atl_med"
  )
  grass <- out[out$item_cbs_code %in% 3000L, ]
  testthat::expect_identical(grass$method_soil_n2o, "aguilera_excreta_blend")
  excreta <- .nb_inputs_with_drivers() |>
    dplyr::filter(.data$fert_type == "excreta")
  # MED / Rainfed: ef 0.0027; mf 0.20 (cattle/monogastric), 0.30 (other).
  mf <- excreta$excreta_cpp_share *
    0.20 +
    (1 - excreta$excreta_cpp_share) * 0.30
  testthat::expect_equal(
    grass$n2o_direct_n_t,
    sum(excreta$n_input_t * 0.0027 * mf),
    tolerance = 1e-12
  )
})

testthat::test_that("the balance example carries the method columns a build returns", {
  example <- whep::build_nitrogen_balance(example = TRUE)
  built <- .nb_run_methods()
  method_cols <- grep("^method_", names(example), value = TRUE)
  testthat::expect_true(all(method_cols %in% names(built)))
  testthat::expect_true("method_n2o_indirect_nh3" %in% method_cols)
  testthat::expect_identical(example$method_soil_n2o, "ipcc2019_wet_dry")
})

# ---- WHEP-built loss drivers ------------------------------------------------

.nb_ipcc_methods <- function(...) {
  utils::modifyList(
    list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach",
      regime = "yield_split"
    ),
    list(...)
  )
}

# The split fixture without its driver tables, and the layers the builder
# reads in their place: the fixture cell's own wet/dry classes for its crop
# (2511 wet irrigated, dry rainfed) and a dry 30-year climate for the cell,
# which classes every row without a crop of its own.
.nb_built_driver_data <- function() {
  data <- .nb_regime_split_data()
  # The shares and the classes describe one land use: only 2511 holds
  # irrigated land, so no other item carries irrigated N (an irrigated row
  # with N and no class of its own is refused).
  data$regime_shares <- dplyr::mutate(
    data$regime_shares,
    irrigated_area_share = dplyr::if_else(
      .data$item_cbs_code %in% 2511L,
      .data$irrigated_area_share,
      0
    ),
    irrigated_yield_share = dplyr::if_else(
      .data$item_cbs_code %in% 2511L,
      .data$irrigated_yield_share,
      0
    )
  )
  data$n_balance_drivers <- NULL
  data$n_balance_leaching_drivers <- NULL
  data$drainage_mm <- NULL
  classes <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~water_regime, ~year,
    ~moisture_regime, ~method_moisture_regime,
    0.25, 50.25, 10L, 2511L, "irrigated", 2010L, "wet",
    "ipcc2019_wet_dry;trailing_30;precip_irrig",
    0.25, 50.25, 10L, 2511L, "rainfed", 2010L, "dry",
    "ipcc2019_wet_dry;trailing_30;precip_irrig"
  ) |>
    dplyr::mutate(climate_fill_km = NA_real_)
  climate <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    year = 1981:2010,
    mean_annual_temp_c = 10,
    annual_precip_mm = 500,
    annual_pet_mm = 900,
    annual_frost_days = 60
  )
  data$n_loss_driver_inputs <- list(
    moisture_regime = classes,
    climate = climate
  )
  data
}

.nb_built_balance <- function() {
  memo_n_fixture("nb_built_balance", function() {
    suppressMessages(whep::build_nitrogen_balance(
      methods = .nb_ipcc_methods(),
      data = .nb_built_driver_data()
    ))
  })
}

testthat::test_that("a grid balance without drivers builds them and says so", {
  testthat::skip_if_not_installed("pointblank")
  out <- .nb_built_balance()
  out |>
    pointblank::expect_col_vals_in_set("method_drivers", "whep_built") |>
    pointblank::expect_col_vals_in_set("moisture_regime", c("wet", "dry")) |>
    pointblank::expect_col_vals_not_null("method_moisture_regime")
  crop <- out[out$item_cbs_code %in% 2511L, ]
  testthat::expect_identical(
    crop$moisture_regime[crop$water_regime == "irrigated"],
    "wet"
  )
  no_item <- out[is.na(out$item_cbs_code), ]
  testthat::expect_gt(nrow(no_item), 0L)
  testthat::expect_true(all(stringr::str_detect(
    no_item$method_moisture_regime,
    "cell_rainfed_class=no_crop_item"
  )))
  # The soil N2O of the irrigated (wet) crop part is priced on wet factors.
  testthat::expect_true(all(
    crop$method_leaching[crop$water_regime == "irrigated"] ==
      "ipcc_fracleach_wet"
  ))
})

testthat::test_that("built drivers give the balance the supplied drivers' result", {
  # The tables the build makes, supplied back, give the same balance.
  data <- .nb_built_driver_data()
  real <- whep::build_n_loss_drivers
  tables <- NULL
  built <- testthat::with_mocked_bindings(
    suppressMessages(whep::build_nitrogen_balance(
      methods = .nb_ipcc_methods(),
      data = data
    )),
    build_n_loss_drivers = function(...) {
      tables <<- real(...)
      tables
    },
    .package = "whep"
  )
  testthat::expect_named(
    tables,
    c("n_balance_drivers", "n_balance_leaching_drivers")
  )
  out <- suppressMessages(whep::build_nitrogen_balance(
    methods = .nb_ipcc_methods(),
    data = c(data, tables)
  ))
  testthat::expect_true(all(out$method_drivers == "supplied"))
  testthat::expect_true(all(built$method_drivers == "whep_built"))
  testthat::expect_identical(
    dplyr::select(out, -"method_drivers"),
    dplyr::select(built, -"method_drivers")
  )
})

testthat::test_that("one supplied driver table is kept and the other built", {
  data <- .nb_built_driver_data()
  data$n_balance_drivers <- .nb_data_with_drivers()$n_balance_drivers
  out <- suppressMessages(whep::build_nitrogen_balance(
    methods = .nb_ipcc_methods(),
    data = data
  ))
  testthat::expect_true(all(out$method_drivers == "supplied|whep_built"))
})

testthat::test_that("dry rainfed surplus is retained, and the partition holds", {
  out <- .nb_built_balance()
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t + out$retained_profile_n_t,
    out$surplus_t,
    tolerance = 1e-9
  )
  dry_rainfed <- out$moisture_regime == "dry" & out$water_regime == "rainfed"
  testthat::expect_true(any(dry_rainfed & out$surplus_t > 0))
  testthat::expect_true(all(out$denitrification_n_t[dry_rainfed] == 0))
  testthat::expect_equal(
    out$retained_profile_n_t[dry_rainfed],
    out$surplus_t[dry_rainfed],
    tolerance = 1e-12
  )
  testthat::expect_true(all(
    out$method_leaching[dry_rainfed] == "ipcc_fracleach_dry_rainfed_retained"
  ))
  testthat::expect_true(all(out$retained_profile_n_t[!dry_rainfed] == 0))
})

testthat::test_that("a polity balance without drivers is refused before assembly", {
  .nb_stub_assembly()
  err <- testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = .nb_ipcc_methods(),
      resolution = "polity",
      data = list()
    ),
    class = "whep_n_loss_drivers_polity"
  )
  testthat::expect_match(
    cli::ansi_strip(conditionMessage(err)),
    "n_balance_leaching_drivers",
    fixed = TRUE
  )
})

testthat::test_that("an unsplit grid balance cannot build its drivers", {
  .nb_stub_assembly()
  testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = .nb_ipcc_methods(regime = "none"),
      data = list()
    ),
    class = "whep_n_loss_drivers_regime"
  )
})

testthat::test_that("Meisinger and MANNER name the drivers the builder lacks", {
  .nb_stub_assembly()
  err <- testthat::expect_error(
    whep::build_nitrogen_balance(
      methods = .nb_ipcc_methods(leaching = "meisinger_drainage"),
      data = list()
    ),
    class = "whep_n_loss_drivers_unsupported"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  for (col in c("drainage_mm", "som_share", "irrig_cat", "tillage")) {
    testthat::expect_match(msg, col, fixed = TRUE)
  }
  err <- testthat::expect_error(
    whep::build_nitrogen_balance(data = list()),
    class = "whep_error_nh3_drivers_absent"
  )
  testthat::expect_match(
    cli::ansi_strip(conditionMessage(err)),
    "build_n_loss_drivers",
    fixed = TRUE
  )
})

# Runs last: every fixture the tests above shared must still be exactly what
# a fresh build produces, so no test's edits leaked into another's input.
testthat::test_that("the shared balance fixtures were never mutated", {
  expect_n_fixtures_untouched("nb_")
})

test_that("the balance's residue recovery default is the package's", {
  default <- eval(formals(whep::calculate_residue_destinies)$recovery)[[1]]
  args <- whep:::.nb_residue_destiny_args(list())
  expect_identical(args$recovery, default)
})
