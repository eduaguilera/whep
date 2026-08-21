# Helper fixtures --------------------------------------------------------------

# BNF: minimal calculate_bnf() input (one crop, one cell-year).
.nbi_bnf_input <- function() {
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

# NPP N: minimal calculate_npp_carbon_nitrogen() input (one crop, one
# cell-year), item_prod_code 15 (wheat) matches whep::items_prod_full so
# calculate_bnf()/npp joins resolve.
.nbi_npp_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~item_cbs_code,
    ~product_dm_t,
    ~residue_dm_t,
    ~root_dm_t,
    0.25,
    50.25,
    10L,
    2010L,
    "15",
    2511L,
    87.9,
    135.75,
    30,
  )
}

# Livestock intake: the redistribute_feed() realised-intake contract expected
# by build_livestock_nutrient_flows(); territory is area_code cast to
# character (the redistribute_feed() convention), sub_territory a "lon_lat"
# cell id.
.nbi_livestock_intake <- function() {
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

.nbi_gridded <- function() {
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
      # The canonical crop key: as.character(item_prod_code). 44 is Barley,
      # whose item_cbs_code is 2513 -- the same answer the deprecated
      # name key "barley" gives, so this fixture change moves no number.
      "44",
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

.nbi_nhx <- function() {
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

.nbi_noy <- function() {
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

.nbi_urban_population <- function() {
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

.nbi_cropland_ha <- function() {
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

.nbi_cell_polity <- function() {
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

.nbi_carbon_balance <- function() {
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
    12, # mineralization: positive input
    0.25,
    50.25,
    10L,
    "Grassland",
    2010L,
    50,
    -5 # immobilization: must be clamped out
  )
}

.nbi_primary_prod <- function() {
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
    700,
    2010L,
    10L,
    2807L,
    "ha",
    300
  )
}

.nbi_fertilizer <- function() {
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

.nbi_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    0.6, # wheat
    0.25,
    50.25,
    27L,
    0.2 # rice
  )
}

.nbi_type_cropland <- function() {
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

.nbi_ag_land_support <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~land_use,
    ~area_ha,
    0.25, 50.25, 10L, 2511L, 2010L, "cropland", 700,
    0.25, 50.25, 10L, 2807L, 2010L, "cropland", 300,
    0.25, 50.25, 10L, 3000L, 2010L, "grassland", 500
  )
}

# Gridded grassland hectares, bypassing the LUH2 read when the support table is
# derived natively rather than injected.
.nbi_grassland_ha <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~area_ha,
    0.25,
    50.25,
    10L,
    2010L,
    500
  )
}

.nbi_full_data <- function() {
  list(
    bnf_input = .nbi_bnf_input(),
    npp_n_input = .nbi_npp_input(),
    livestock_intake = .nbi_livestock_intake(),
    gridded = .nbi_gridded(),
    nhx = .nbi_nhx(),
    noy = .nbi_noy(),
    urban_population = .nbi_urban_population(),
    cropland_ha = .nbi_cropland_ha(),
    cell_polity = .nbi_cell_polity(),
    carbon_balance = .nbi_carbon_balance(),
    primary_prod = .nbi_primary_prod(),
    fertilizer = .nbi_fertilizer(),
    crop_patterns = .nbi_crop_patterns(),
    type_cropland = .nbi_type_cropland(),
    ag_land_support = .nbi_ag_land_support()
  )
}

# Tests ------------------------------------------------------------------------

testthat::test_that("all seven implemented fert_type values are present", {
  out <- whep::build_n_inputs(data = .nbi_full_data())

  expected <- c(
    "bnf",
    "recycling",
    "manure_solid",
    "manure_liquid",
    "excreta",
    "deposition",
    "urban",
    "som_mineralization",
    "synthetic"
  )
  testthat::expect_true(all(expected %in% out$fert_type))
})

testthat::test_that("schema is complete at grid resolution", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "fert_type",
      "n_input_t"
    )
  )
})

testthat::test_that("synthetic gridded output re-aggregates to the polity total", {
  out <- whep::build_n_inputs(
    data = .nbi_full_data(),
    resolution = "grid"
  )
  synthetic <- out[out$fert_type == "synthetic", ]
  testthat::expect_true(nrow(synthetic) > 0)
  testthat::expect_equal(sum(synthetic$n_input_t), 100, tolerance = 1e-6)
})

testthat::test_that("SOM term clamps negative son_change_kgn_ha out", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  som <- out[out$fert_type == "som_mineralization", ]
  testthat::expect_true(nrow(som) > 0)
  testthat::expect_true(all(som$n_input_t > 0))
  # The Grassland row (rate -5, immobilization) must not appear at all.
  expected_t <- 12 * 50 * 1 / 1000
  testthat::expect_equal(sum(som$n_input_t), expected_t, tolerance = 1e-6)
})

testthat::test_that("SOM mineralization is allocated across cropland crops", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  som <- out[out$fert_type == "som_mineralization", ]
  testthat::expect_setequal(som$item_cbs_code, c(2511L, 2807L))
  testthat::expect_equal(sum(som$n_input_t), 0.6)
})

testthat::test_that("deposition and urban use agricultural item support", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  dep <- out[out$fert_type == "deposition", ]
  urb <- out[out$fert_type == "urban", ]
  testthat::expect_setequal(dep$item_cbs_code, c(2511L, 2807L, 3000L))
  testthat::expect_setequal(urb$item_cbs_code, c(2511L, 2807L))
  testthat::expect_false(anyNA(dep$item_cbs_code))
  testthat::expect_false(anyNA(urb$item_cbs_code))
})

testthat::test_that("deposition excludes forest and natural land mass", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  dep <- dplyr::filter(out, .data$fert_type == "deposition")
  # 3e9 g N over a 3000 ha cell = 1000 kg N/ha. Agricultural support is
  # 1000 ha cropland + 500 ha grassland; the other 1500 ha is not charged.
  testthat::expect_equal(sum(dep$n_input_t), 1500)
  testthat::expect_equal(
    dep$n_input_t[dep$item_cbs_code == 3000L],
    500
  )
})

testthat::test_that("agricultural support is derived when not supplied", {
  data <- .nbi_full_data()
  data$ag_land_support <- NULL
  data$grassland_ha <- .nbi_grassland_ha()
  out <- whep::build_n_inputs(data = data)
  dep <- out[out$fert_type == "deposition", ]
  # The 1000 physical cropland ha are split by the crop pattern (0.6 wheat /
  # 0.2 rice -> 0.75 / 0.25), not by the raw harvest fractions, plus 500 ha of
  # grassland: the same 1500 charged hectares the injected support gives.
  testthat::expect_setequal(dep$item_cbs_code, c(2511L, 2807L, 3000L))
  testthat::expect_equal(sum(dep$n_input_t), 1500)
  testthat::expect_equal(dep$n_input_t[dep$item_cbs_code == 2511L], 750)
  testthat::expect_equal(dep$n_input_t[dep$item_cbs_code == 2807L], 250)
  testthat::expect_equal(dep$n_input_t[dep$item_cbs_code == 3000L], 500)
})

testthat::test_that("non-item inputs abort when no support can be derived", {
  # Dropping data$type_cropland does not remove the input, it makes the reader
  # fall back to WHEP_TYPE_CROPLAND_PATH. Without pinning the variable the test
  # reads whatever gridded cropland the machine happens to have configured and
  # asserts "no land surface exists" while one is being loaded -- so it checks
  # the branch it is named after only where the data is absent, which is never
  # the machines that have it.
  withr::local_envvar(WHEP_TYPE_CROPLAND_PATH = NA)
  data <- .nbi_full_data()
  data$ag_land_support <- NULL
  data$type_cropland <- NULL
  testthat::expect_error(
    whep::build_n_inputs(data = data),
    "WHEP_TYPE_CROPLAND_PATH|ag_land_support|land support"
  )
})

testthat::test_that("the manure engine resolution is never overwritten", {
  testthat::expect_equal(
    whep:::.ni_manure_resolution(list(), "grid"),
    "subnational"
  )
  testthat::expect_equal(
    whep:::.ni_manure_resolution(list(), "polity"),
    "national"
  )
  testthat::expect_equal(
    whep:::.ni_manure_resolution(list(resolution = "national"), "grid"),
    "national"
  )
})

testthat::test_that("urban ISO3 area codes resolve instead of becoming NA", {
  data <- .nbi_full_data()
  data$cell_polity$area_code <- "ESP"
  data$cropland_ha$area_code <- "ESP"

  # The iso3c form is a deprecated bridge (#463), so resolving it warns; what
  # this test is about is that it still resolves rather than becoming NA.
  testthat::expect_warning(out <- whep:::.n_inputs_urban(data), "deprecated")

  testthat::expect_equal(out$area_code, 203L)
  testthat::expect_false(anyNA(out$area_code))
})

testthat::test_that("polity resolution is the cell-summed aggregate of grid", {
  grid <- whep::build_n_inputs(data = .nbi_full_data(), resolution = "grid")
  polity <- whep::build_n_inputs(data = .nbi_full_data(), resolution = "polity")

  expected <- grid |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t),
      .by = c("area_code", "item_cbs_code", "year", "fert_type")
    ) |>
    dplyr::arrange(
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      .data$fert_type
    )
  got <- polity |>
    dplyr::arrange(
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      .data$fert_type
    )

  testthat::expect_equal(
    sum(got$n_input_t),
    sum(expected$n_input_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(nrow(got), nrow(expected))
})

testthat::test_that("manure_type maps to manure_solid/manure_liquid/excreta", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  manure <- out[
    out$fert_type %in% c("manure_solid", "manure_liquid", "excreta"),
  ]
  testthat::expect_true(nrow(manure) > 0)
  # area_code resolved from the manure engine's territory string.
  testthat::expect_true(all(manure$area_code == 10L))
})

testthat::test_that("manure territory resolves an iso3c code, not just a stringified area_code", {
  # The real pipeline (redistribute_feed()) casts area_code to character; an
  # iso3c territory is a deprecated bridge for fixtures written before the
  # manure chain's @examples used area codes (#463). It must still resolve
  # rather than silently NA out, but it now warns, and the warning has to reach
  # the exported boundary rather than dying inside the helper -- that is what
  # tells a caller their fixture is on the lossy vocabulary.
  intake <- .nbi_livestock_intake()
  intake$territory <- "ESP"
  gridded <- .nbi_gridded()
  gridded$crops$territory <- "ESP"
  gridded$grass$territory <- "ESP"
  data <- .nbi_full_data()
  data$livestock_intake <- intake
  data$gridded <- gridded

  testthat::expect_warning(
    out <- whep::build_n_inputs(data = data),
    "deprecated"
  )
  manure <- out[
    out$fert_type %in% c("manure_solid", "manure_liquid", "excreta"),
  ]
  testthat::expect_true(nrow(manure) > 0)
  # ESP's whep::regions_full "code" is 203.
  testthat::expect_true(all(manure$area_code == 203L))
})

testthat::test_that("an unresolvable manure territory aborts rather than propagating NA", {
  intake <- .nbi_livestock_intake()
  intake$territory <- "NOT_A_REAL_CODE"
  gridded <- .nbi_gridded()
  gridded$crops$territory <- "NOT_A_REAL_CODE"
  gridded$grass$territory <- "NOT_A_REAL_CODE"
  data <- .nbi_full_data()
  data$livestock_intake <- intake
  data$gridded <- gridded

  testthat::expect_error(whep::build_n_inputs(data = data))
})

testthat::test_that("years filters the assembled output", {
  data <- .nbi_full_data()
  out <- whep::build_n_inputs(years = 2010L, data = data)
  testthat::expect_true(all(out$year == 2010L))
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_n_inputs(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "fert_type",
      "n_input_t"
    )
  )
  expected_present <- c(
    "bnf",
    "synthetic",
    "deposition",
    "urban",
    "som_mineralization"
  )
  testthat::expect_true(all(expected_present %in% out$fert_type))
})

testthat::test_that("resolution argument is validated", {
  testthat::expect_error(
    whep::build_n_inputs(resolution = "province", data = .nbi_full_data()),
    "resolution"
  )
})

testthat::test_that("recycling stamps the total-residue basis when no soil col", {
  # .nbi_npp_input() supplies residue_dm_t but not residue_soil_dm_t, so the
  # recycling term must fall back to gross residue N and say so.
  out <- whep::build_n_inputs(data = .nbi_full_data())
  rec <- out[out$fert_type == "recycling", ]
  testthat::expect_true(nrow(rec) > 0)
  testthat::expect_true(all(rec$method_recycling_n == "total_residue"))
})

testthat::test_that("recycling stamps the soil-returned basis when supplied", {
  data <- .nbi_full_data()
  data$npp_n_input <- dplyr::mutate(
    data$npp_n_input,
    residue_soil_dm_t = .data$residue_dm_t * 0.4
  )
  out <- whep::build_n_inputs(data = data)
  rec <- out[out$fert_type == "recycling", ]
  testthat::expect_true(nrow(rec) > 0)
  testthat::expect_true(all(rec$method_recycling_n == "residue_soil_returned"))
})

testthat::test_that("recycling basis switch changes n_input_t and is stamped", {
  total_basis <- whep::build_n_inputs(data = .nbi_full_data())
  soil_data <- .nbi_full_data()
  soil_data$npp_n_input <- dplyr::mutate(
    soil_data$npp_n_input,
    residue_soil_dm_t = .data$residue_dm_t * 0.4
  )
  soil_basis <- whep::build_n_inputs(data = soil_data)

  total_n <- total_basis$n_input_t[total_basis$fert_type == "recycling"]
  soil_n <- soil_basis$n_input_t[soil_basis$fert_type == "recycling"]
  # The soil-returned basis must be strictly smaller (residue removed for
  # feed/fuel is excluded), and the two calls must carry different stamps.
  testthat::expect_lt(soil_n, total_n)
  testthat::expect_false(
    identical(
      unique(total_basis$method_recycling_n[
        total_basis$fert_type == "recycling"
      ]),
      unique(soil_basis$method_recycling_n[
        soil_basis$fert_type == "recycling"
      ])
    )
  )
})

testthat::test_that("method_recycling_n is NA for non-recycling fert_types", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  non_rec <- out[out$fert_type != "recycling", ]
  testthat::expect_true(all(is.na(non_rec$method_recycling_n)))
})

testthat::test_that("an unmapped Cropland crop name aborts rather than NA", {
  gridded <- .nbi_gridded()
  gridded$crops$crop <- "mixed cereals not a real crop"
  data <- .nbi_full_data()
  data$gridded <- gridded

  testthat::expect_error(
    whep::build_n_inputs(data = data),
    "item_cbs_code"
  )
})

testthat::test_that("a code-keyed Cropland crop resolves without NA", {
  # The default fixture crop is "44" (Barley's item_prod_code); its manure rows
  # must carry a real item_cbs_code, never NA_integer_.
  out <- whep::build_n_inputs(data = .nbi_full_data())
  cropland_manure <- out[
    out$fert_type %in%
      c("manure_solid", "manure_liquid") &
      out$item_cbs_code != 3000L,
  ]
  testthat::expect_true(nrow(cropland_manure) > 0)
  testthat::expect_true(all(!is.na(cropland_manure$item_cbs_code)))
})

testthat::test_that("the crops layer the carbon path builds resolves (#788)", {
  # THE regression this file did not have: every nitrogen test injected its own
  # gridded fixture, so none ever fed the nitrogen path the layer the carbon
  # path actually produces. .sci_manure_crop_layer() keys `crop` by
  # as.character(item_prod_code); the nitrogen side used to resolve it by NAME
  # only and aborted on all of it (9298 rows / 1.383e9 ha for real 2010 data).
  production <- tibble::tribble(
    ~area_code, ~year, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
    ~unit, ~value,
    10L, 2010L, "15", 2511L, NA_integer_, "ha", 100,
    10L, 2010L, "44", 2513L, NA_integer_, "ha", 50,
    10L, 2010L, "56", 2514L, NA_integer_, "ha", 25,
    # Grass and livestock rows the layer must exclude.
    10L, 2010L, "3000", 3000L, NA_integer_, "ha", 400,
    10L, 2010L, "867", 2731L, 866L, "ha", 10
  )
  layer <- whep:::.sci_manure_crop_layer(production)
  testthat::expect_setequal(layer$crop, c("15", "44", "56"))

  # Resolves with no abort and no deprecation warning, and to the crosswalk's
  # own item_cbs_code -- an invariant, not a hand-picked expectation.
  resolved <- testthat::expect_no_warning(
    whep:::.ni_crop_to_item_cbs(layer$crop)
  )
  expected <- whep:::.ni_item_cbs_from_prod(layer$crop)
  testthat::expect_equal(resolved, expected)
  testthat::expect_false(anyNA(resolved))
})

testthat::test_that("the code key survives the full nitrogen assembly (#788)", {
  # End to end through build_n_inputs(), with the crops layer keyed the way
  # .sci_manure_crop_layer() keys it, and no deprecation warning.
  gridded <- .nbi_gridded()
  gridded$crops$crop <- "15"
  data <- .nbi_full_data()
  data$gridded <- gridded

  out <- testthat::expect_no_warning(whep::build_n_inputs(data = data))
  cropland_manure <- out[
    out$fert_type %in%
      c("manure_solid", "manure_liquid") &
      !is.na(out$item_cbs_code) &
      out$item_cbs_code != 3000L,
  ]
  testthat::expect_true(nrow(cropland_manure) > 0)
  # 15 is Wheat -> item_cbs_code 2511.
  testthat::expect_setequal(cropland_manure$item_cbs_code, 2511L)
})

testthat::test_that("a name-keyed crop still resolves but warns as deprecated", {
  gridded <- .nbi_gridded()
  gridded$crops$crop <- "barley"
  data <- .nbi_full_data()
  data$gridded <- gridded

  testthat::expect_warning(
    out <- whep::build_n_inputs(data = data),
    "deprecated"
  )
  cropland_manure <- out[
    out$fert_type %in%
      c("manure_solid", "manure_liquid") &
      !is.na(out$item_cbs_code) &
      out$item_cbs_code != 3000L,
  ]
  testthat::expect_true(nrow(cropland_manure) > 0)
  # The name bridge must land on the same item_cbs_code the code key gives.
  testthat::expect_setequal(cropland_manure$item_cbs_code, 2513L)
})

testthat::test_that("trying codes before names can never steal a match", {
  # The resolution order is only safe while no item_prod NAME equals a
  # DIFFERENT item_prod_code. `Fallow` is the crosswalk's one name/code
  # collision and both its rows are the same item, so the order is unambiguous;
  # this guard fails if a future crosswalk edit breaks that.
  pairs <- whep::items_prod_full |>
    dplyr::transmute(
      code = as.character(.data$item_prod_code),
      name_lower = stringr::str_to_lower(.data$item_prod)
    ) |>
    dplyr::filter(!is.na(.data$code), !is.na(.data$name_lower))
  clashing <- pairs |>
    dplyr::filter(.data$name_lower %in% pairs$code) |>
    dplyr::filter(.data$name_lower != stringr::str_to_lower(.data$code))
  testthat::expect_equal(nrow(clashing), 0L)
})

testthat::test_that("item_prod_code is a unique key onto item_cbs_code", {
  # The reason the code key is canonical: it is 1:1, and the name key is not.
  by_code <- whep::items_prod_full |>
    dplyr::transmute(
      code = as.character(.data$item_prod_code),
      item_cbs_code = whep:::.as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(!is.na(.data$code), !is.na(.data$item_cbs_code)) |>
    dplyr::distinct()
  testthat::expect_false(any(duplicated(by_code$code)))
})

testthat::test_that("unattributed Cropland manure is not mislabeled as grass", {
  applied <- tibble::tibble(
    year = 2010L,
    territory = "10",
    sub_territory = "0.25_50.25",
    land_use = "Cropland",
    crop = NA_character_,
    manure_type = "Solid",
    applied_n = 5
  )

  out <- whep:::.manure_to_n_inputs(applied)

  testthat::expect_equal(out$fert_type, "manure_solid")
  testthat::expect_true(is.na(out$item_cbs_code))
})

testthat::test_that("unattributed Cropland manure stays on cropland support", {
  inputs <- dplyr::bind_rows(
    whep:::.ni_empty(),
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      area_code = 10L,
      item_cbs_code = NA_integer_,
      year = 2010L,
      fert_type = "manure_solid",
      n_input_t = 5,
      method_recycling_n = NA_character_,
      method_synthetic = NA_character_
    )
  )

  out <- whep:::.ni_allocate_unattributed(
    inputs,
    list(ag_land_support = .nbi_ag_land_support())
  )

  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2807L))
  testthat::expect_false(3000L %in% out$item_cbs_code)
  testthat::expect_equal(sum(out$n_input_t), 5)
  testthat::expect_equal(
    out$n_input_t[match(c(2511L, 2807L), out$item_cbs_code)],
    c(3.5, 1.5)
  )
})

testthat::test_that("transported manure is retained as an unattributed agricultural input", {
  applied <- tibble::tibble(
    year = 2010L,
    territory = "10",
    sub_territory = "0.25_50.25",
    land_use = "transported",
    crop = NA_character_,
    manure_type = "Liquid",
    applied_n = 7
  )

  out <- whep:::.manure_to_n_inputs(applied)

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$fert_type, "manure_liquid")
  testthat::expect_equal(out$n_input_t, 7)
  testthat::expect_true(is.na(out$item_cbs_code))
})

# Synthetic fertiliser: Coello rate-weighted crop split (Task 1.4) ------------

.nis_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2010L, 10L, 2511L, "ha", 100, # wheat
    2010L, 10L, 2514L, "ha", 100 # maize
  )
}
.nis_fertilizer <- function() {
  tibble::tribble(
    ~Element, ~Item, ~Year, ~`Area Code`, ~Value,
    "Agricultural Use", "Nutrient nitrogen N (total)", 2010L, 10L, 1000
  )
}
.nis_coello_rates <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~kg_n_ha,
    2010L, 10L, 2511L, 150,
    2010L, 10L, 2514L, 50
  )
}

testthat::test_that("build_n_inputs conserves FAOSTAT synthetic total", {
  res <- whep::build_n_inputs(
    resolution = "polity",
    data = list(
      primary_prod = .nis_primary_prod(),
      fertilizer = .nis_fertilizer(),
      coello_rates = .nis_coello_rates(),
      synthetic_method = "coello"
    )
  ) |>
    dplyr::filter(.data$fert_type == "synthetic")
  testthat::expect_equal(sum(res$n_input_t), 1000) # national total held
  # wheat takes 0.75 of the 1000 t national total
  testthat::expect_equal(
    res$n_input_t[res$item_cbs_code == 2511L],
    750
  )
  testthat::expect_equal(
    res$n_input_t[res$item_cbs_code == 2514L],
    250
  )
  testthat::expect_true(all(res$method_synthetic == "coello"))
})

testthat::test_that("build_n_inputs area_share method conserves too", {
  res <- whep::build_n_inputs(
    resolution = "polity",
    data = list(
      primary_prod = .nis_primary_prod(),
      fertilizer = .nis_fertilizer(),
      coello_rates = .nis_coello_rates(),
      synthetic_method = "area_share"
    )
  ) |>
    dplyr::filter(.data$fert_type == "synthetic")
  testthat::expect_equal(sum(res$n_input_t), 1000)
  testthat::expect_equal(
    res$n_input_t[res$item_cbs_code == 2511L],
    500
  ) # equal area -> equal split
  testthat::expect_true(all(res$method_synthetic == "area_share"))
})

testthat::test_that("all-zero Coello rates fall back to conserving area shares", {
  zero_rates <- dplyr::mutate(.nis_coello_rates(), kg_n_ha = 0)
  res <- whep::build_n_inputs(
    resolution = "polity",
    data = list(
      primary_prod = .nis_primary_prod(),
      fertilizer = .nis_fertilizer(),
      coello_rates = zero_rates,
      synthetic_method = "coello"
    )
  ) |>
    dplyr::filter(.data$fert_type == "synthetic")
  testthat::expect_equal(sum(res$n_input_t), 1000)
  testthat::expect_equal(res$n_input_t, c(500, 500))
  testthat::expect_true(all(res$method_synthetic == "area_share"))
  testthat::expect_false(anyNA(res$n_input_t))
})

testthat::test_that("build_n_inputs re-keys FAOSTAT synthetic N to polities", {
  # Regression for issue 464. The country_totals built inside
  # .n_inputs_synthetic must speak the same vocabulary as crop_shares, which
  # descends from get_primary_production and is keyed on
  # polity_area_code. FAOSTAT reports Sudan as 276 and South Sudan as 277 after
  # the 2011 split, both bucketed to 206; the World rollup 5000 has no polity at
  # all. Before the crosswalk was applied, none of the three joined and this
  # build emitted zero synthetic N for Sudan. A fixture is used because whep
  # ships no fertiliser table -- faostat-fertilizer-nutrients is a remote pin.
  res <- whep::build_n_inputs(
    resolution = "polity",
    data = list(
      primary_prod = tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        2015L, 206L, 2511L, "ha", 100,
        2015L, 206L, 2514L, "ha", 100
      ),
      fertilizer = tibble::tribble(
        ~Element, ~Item, ~Year, ~`Area Code`, ~Value,
        "Agricultural Use", "Nutrient nitrogen N (total)", 2015L, 276L, 700,
        "Agricultural Use", "Nutrient nitrogen N (total)", 2015L, 277L, 300,
        "Agricultural Use", "Nutrient nitrogen N (total)", 2015L, 5000L, 1e6
      ),
      synthetic_method = "area_share"
    )
  ) |>
    dplyr::filter(.data$fert_type == "synthetic")

  testthat::expect_setequal(res$area_code, 206L)
  # 700 + 300 summed under the shared bucket, split equally by equal area; the
  # World rollup contributes nothing.
  testthat::expect_equal(sum(res$n_input_t), 1000)
  testthat::expect_equal(res$n_input_t, c(500, 500))
})

testthat::test_that("a duplicate-ISO3 territory no longer shifts later ones", {
  # Regression: the previous inline lookup joined on regions_full$code, where
  # ETH and SDN each carry a second historical row. The join grew the result and
  # shifted every LATER territory onto the wrong country -- c("ESP","ETH","DEU")
  # returned 203, 238, 62, silently turning Germany into Ethiopia PDR.
  #
  # The ISO3 form is now a deprecated bridge that warns, and these assertions
  # are about the resolved codes rather than the warning, so silence it here;
  # the warning itself is asserted in the test below.
  territory_codes <- function(x) {
    suppressWarnings(whep:::.manure_territory_to_area_code(x))
  }
  testthat::expect_equal(
    territory_codes(c("ESP", "ETH", "DEU")),
    c(203L, 238L, 79L)
  )
  testthat::expect_equal(territory_codes(c("SDN", "DEU")), c(206L, 79L))
  # Stringified area codes still pass straight through, mixed with ISO3.
  testthat::expect_equal(territory_codes(c("203", "ETH")), c(203L, 238L))
})

testthat::test_that("the ISO3 territory bridge warns and area codes do not", {
  # #463: territory carried two undocumented vocabularies. The pipeline emits
  # only stringified area codes (feed_intake_redistribute.R:805), while the
  # manure chain's own @examples used ISO3 literals, and the two disagree about
  # what the resolved code means: an ISO3 resolves through polity_area_code, a
  # FABIO aggregation bucket, which for 62 of the 257 ISO3 codes in
  # whep::regions_full is not that territory's own code (measured). 61 land on
  # 999, Rest of World; "SSD" lands on 206, Sudan (former), where the numeric
  # form "277" keeps South Sudan. So the bridge must be audible.
  testthat::expect_warning(
    testthat::expect_equal(
      whep:::.manure_territory_to_area_code(c("ESP", "203")),
      c(203L, 203L)
    ),
    "deprecated"
  )
  # The warning names the offending value, so a caller can find the fixture.
  testthat::expect_warning(
    whep:::.manure_territory_to_area_code("SSD"),
    "SSD"
  )
  # The measured collapse itself: the two vocabularies for South Sudan resolve
  # to different area codes, hence different polities downstream (206 is
  # SDN-2011-2025, 277 is SSD-2011-2025).
  testthat::expect_equal(
    suppressWarnings(whep:::.manure_territory_to_area_code("SSD")),
    206L
  )
  testthat::expect_equal(whep:::.manure_territory_to_area_code("277"), 277L)
  # The pipeline's own vocabulary stays silent: no warning for anyone who
  # follows the (now area-code) documented examples.
  testthat::expect_silent(
    whep:::.manure_territory_to_area_code(c("203", "79", NA))
  )
})

testthat::test_that("an unresolvable territory still aborts", {
  testthat::expect_error(
    whep:::.manure_territory_to_area_code(c("203", "NOTACODE")),
    "NOTACODE"
  )
})

testthat::test_that("the manure chain's examples use the pipeline vocabulary", {
  # #463: every @examples block in the manure chain identified a territory with
  # an ISO literal -- "ESP" in the allocation, transport and driver examples,
  # "ES" in the excretion and management ones -- while the pipeline itself
  # passes as.character(area_code). "ES" is not resolvable at all: pasting the
  # documented split_manure_management() fixture into build_n_inputs() aborted
  # with "Could not resolve territory to an area_code", and "ESP" resolved only
  # through the lossy iso3c bridge. The defect lives in the documentation, so
  # this scans the roxygen @examples blocks themselves; asserting on one
  # function's output would let the next ISO literal back in.
  #
  # R/ holds compiled objects rather than sources in an installed package, so
  # the scan skips wherever the five files are not readable and is load-bearing
  # under devtools::test() and in a source checkout.
  files <- c(
    "excretion.R",
    "manure_management.R",
    "manure_allocation.R",
    "manure_transport.R",
    "build_livestock_nutrient_flows.R"
  )
  roots <- c(testthat::test_path("..", "..", "R"), "../../00_pkg_src/whep/R")
  found <- lapply(roots, function(r) file.path(r, files))
  found <- Filter(function(p) all(file.exists(p)), found)
  testthat::skip_if(
    length(found) == 0L,
    "the manure chain's R sources are not available next to the tests"
  )
  paths <- found[[1]]

  # Every #' line below an @examples tag, up to the end of that roxygen block.
  example_lines <- function(path) {
    lines <- readLines(path, warn = FALSE)
    rox <- grepl("^#'", lines)
    starts <- which(rox & grepl("@examples", lines))
    unlist(lapply(starts, function(i) {
      j <- i + 1L
      while (j <= length(lines) && rox[[j]]) {
        j <- j + 1L
      }
      if (j > i + 1L) lines[seq(i + 1L, j - 1L)] else character()
    }))
  }
  lits <- unlist(lapply(paths, function(p) {
    ex <- example_lines(p)
    gsub('"', "", unlist(regmatches(ex, gregexpr('"[^"]*"', ex))))
  }))
  testthat::expect_gt(length(lits), 0L)

  iso_like <- unique(lits[grepl("^[A-Za-z]{2,3}$", lits)])
  testthat::expect_equal(
    iso_like,
    character(),
    info = paste0(
      "ISO-shaped literals in the manure chain's examples: ",
      paste(iso_like, collapse = ", ")
    )
  )
  # In these blocks the quoted all-digit literals ARE the territory values:
  # years and item codes are unquoted integers and cell ids carry a "_". Each
  # must resolve through the chain's terminal step, and resolve silently, since
  # a warning here would mean the examples still teach the bridge.
  codes <- unique(lits[grepl("^[0-9]+$", lits)])
  testthat::expect_gt(length(codes), 0L)
  testthat::expect_silent(whep:::.manure_territory_to_area_code(codes))
  testthat::expect_equal(
    whep:::.manure_territory_to_area_code(codes),
    as.integer(codes)
  )
})

# ---- C3b: the ledger states which territory category it consumes -------
#
# build_n_deposition() now emits one row per polycell per territory
# category, so .n_inputs_deposition() has to say which it takes. DA-14
# leaves the substantive question open -- whether the cropland ledger should
# be credited with only the terrestrial share of a cell's deposited mass --
# and these blocks assert that it is NOT answered here: `deposition_kgn_ha`
# is a whole-cell rate carried identically on every category row, so
# filtering to one category leaves every ledger value untouched.
#
# What the filter does buy is that the ledger cannot silently consume all
# three, which would charge each cell's agricultural land three times.

# The same one cell and one polity, with its 2,000 ha of territory
# decomposed 1,200 land / 500 inland water / 300 ice. Its 3,000 ha
# cell_area_ha and polity_frac = 1 are unchanged.
.nbi_decomposed_cell_polity <- function() {
  dplyr::mutate(
    .nbi_cell_polity(),
    polity_area_ha = 2000,
    land_area_ha = 1200,
    inland_water_ha = 500,
    ice_area_ha = 300
  )
}

# The control: the same territory, undecomposed. It splits by the same
# `polity_area_ha` key, so the ONLY difference from the fixture above is the
# presence of the category columns.
.nbi_undecomposed_cell_polity <- function() {
  dplyr::mutate(.nbi_cell_polity(), polity_area_ha = 2000)
}

.nbi_deposition_rows <- function(cell_polity) {
  data <- .nbi_full_data()
  data$cell_polity <- cell_polity
  whep::build_n_inputs(data = data) |>
    dplyr::filter(.data$fert_type == "deposition") |>
    dplyr::arrange(.data$item_cbs_code)
}

# A build_n_deposition() slice as .ni_deposition_in_scope() receives it: one
# cell, one polity, one whole-cell rate, its 1,000 t of mass split 600 land /
# 300 inland water / 100 ice, so the land scope is 0.6 of the territory scope.
.nbi_scope_rows <- function(categories, method = "land_water_ice") {
  tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    year = 2010L,
    area_category = categories,
    deposition_kgn_ha = 1000,
    deposition_n_t = c(600, 300, 100)[seq_along(categories)],
    method_area_split = method
  )
}

testthat::test_that("C3b: the default scope moves no ledger value", {
  decomposed <- .nbi_deposition_rows(.nbi_decomposed_cell_polity())
  undecomposed <- .nbi_deposition_rows(.nbi_undecomposed_cell_polity())

  # Bit-identical, not merely close. DA-14 was decided on 2026-08-06 in
  # favour of the WHOLE territory: nitrogen deposited on a lake or a glacier
  # still drives indirect N2O and still reaches the eutrophication pathway,
  # so the impact terms have to account for it. The scope factor is therefore
  # exactly 1, and no published number moves on this commit -- asserted here
  # rather than argued in a commit message.
  testthat::expect_identical(decomposed, undecomposed)
  # And it is the pre-C3b number: 1000 kg N/ha over 1,000 ha of cropland and
  # 500 ha of grassland.
  testthat::expect_equal(sum(decomposed$n_input_t), 1500)
  testthat::expect_setequal(decomposed$item_cbs_code, c(2511L, 2807L, 3000L))
  # One row per (cell, polity, item), not three. Consuming all three
  # categories would charge the same hectares once per category: 4,500 t
  # rather than 1,500, every hectare of it plausible-looking.
  testthat::expect_identical(nrow(decomposed), 3L)
  testthat::expect_false(any(duplicated(decomposed$item_cbs_code)))
})

testthat::test_that("C3b: the land scope is selectable and takes the land share", {
  # The alternative DA-14 declined. The fixture's polity holds 2,000 ha of
  # territory of which 1,200 is land, so the terrestrial scope charges 60% of
  # what the territory scope does. The same construction on real 2014 HaNi
  # input measured 60.7385 Tg against 61.6285 Tg (AM-30), a 1.444% fall.
  data <- .nbi_full_data()
  data$cell_polity <- .nbi_decomposed_cell_polity()
  data$deposition_scope <- "land"
  land <- whep::build_n_inputs(data = data) |>
    dplyr::filter(.data$fert_type == "deposition")
  territory <- .nbi_deposition_rows(.nbi_decomposed_cell_polity())

  testthat::expect_equal(sum(land$n_input_t), 900)
  testthat::expect_equal(sum(land$n_input_t) / sum(territory$n_input_t), 0.6)
  testthat::expect_identical(nrow(land), 3L)
})

testthat::test_that("C3b: the scope is recorded, and only on deposition rows", {
  data <- .nbi_full_data()
  data$cell_polity <- .nbi_decomposed_cell_polity()
  out <- whep::build_n_inputs(data = data)
  land <- whep::build_n_inputs(
    data = c(data, list(deposition_scope = "land"))
  )

  # Without a recorded scope, a territory-scope table and a land-scope table
  # are indistinguishable after the fact -- which is exactly how two
  # incompatible conventions coexist unnoticed.
  dep <- out$fert_type == "deposition"
  testthat::expect_true(all(out$method_deposition_scope[dep] == "territory"))
  testthat::expect_true(all(is.na(out$method_deposition_scope[!dep])))
  testthat::expect_true(all(
    land$method_deposition_scope[land$fert_type == "deposition"] == "land"
  ))
  # It survives the polity aggregation, where a method column that is not a
  # grouping key would collapse rows of different scopes into one.
  polity <- whep::build_n_inputs(data = data, resolution = "polity")
  testthat::expect_true(rlang::has_name(polity, "method_deposition_scope"))
  testthat::expect_true(all(
    polity$method_deposition_scope[polity$fert_type == "deposition"] ==
      "territory"
  ))
})

testthat::test_that("C3b: a scope the support cannot serve aborts", {
  # No silent fallback, exactly as for `split =`. The interim crosswalk
  # carries no category columns, so its deposition table has one undecomposed
  # "territory" row per polycell. Serving that under a "land" label would
  # overstate the terrestrial term by the whole water and ice share.
  data <- .nbi_full_data()
  data$deposition_scope <- "land"

  testthat::expect_error(
    whep::build_n_inputs(data = data),
    "needs a decomposed territory"
  )
  testthat::expect_error(
    whep::build_n_inputs(data = data),
    "land_area_ha"
  )
  # And an unrecognised scope is refused rather than silently defaulting.
  data$deposition_scope <- "terrestrial"
  testthat::expect_error(
    whep::build_n_inputs(data = data),
    "deposition_scope.*must be"
  )
})

testthat::test_that("C3b: a scope filter matching nothing aborts", {
  # THE failure this guard exists for. Under .ni_empty() semantics a
  # deposition term that filters down to zero rows is indistinguishable from
  # one whose inputs were absent, so a mislabelled category would delete the
  # whole term from the ledger without a word.
  mislabelled <- .nbi_scope_rows(c("terrestrial", "inland_water", "ice"))

  testthat::expect_error(
    whep:::.ni_deposition_in_scope(mislabelled, "land"),
    "No deposition row falls inside scope"
  )
  testthat::expect_error(
    whep:::.ni_deposition_in_scope(mislabelled, "land"),
    "kept 0 of 3 rows"
  )
})

testthat::test_that("C3b: the scope filter keeps rows under both methods", {
  # The positive control for the block above: a filter that aborted on
  # everything would also pass an abort test, so both decompositions and both
  # scopes must be shown to survive it, with the right scope fraction.
  decomposed <- .nbi_scope_rows(c("land", "inland_water", "ice"))
  undecomposed <- .nbi_scope_rows("territory", method = "none")

  territory <- whep:::.ni_deposition_in_scope(decomposed, "territory")
  land <- whep:::.ni_deposition_in_scope(decomposed, "land")
  testthat::expect_identical(nrow(territory), 1L)
  testthat::expect_identical(territory$scope_frac, 1)
  testthat::expect_equal(land$scope_frac, 0.6)
  testthat::expect_identical(
    whep:::.ni_deposition_in_scope(undecomposed, "territory")$scope_frac,
    1
  )
  # An empty input stays empty rather than aborting: no deposition input at
  # all is a legitimate state, and it is not what the guard is looking for.
  testthat::expect_identical(
    nrow(whep:::.ni_deposition_in_scope(decomposed[0, ], "land")),
    0L
  )
})

testthat::test_that("C3b: a cell whose polities disagree on the rate aborts", {
  # AM-5 risk 1, guarded where the rate is CONSUMED as well as where it is
  # produced. Two rates in one cell mean rate x area recovers the cell's whole
  # mass once per polity, so the ledger would multiply deposition by the
  # number of polities sharing a border, behind entirely plausible rates.
  split_rate <- dplyr::mutate(
    .nbi_scope_rows(c("land", "inland_water", "ice")),
    deposition_kgn_ha = c(1000, 1000, 1200)
  )

  testthat::expect_error(
    whep:::.ni_deposition_in_scope(split_rate, "territory"),
    "one whole-cell rate per polycell"
  )
  # A cell that received nothing has 0/0 categories; that is not a defect and
  # must not become NaN in the ledger.
  empty_cell <- dplyr::mutate(
    .nbi_scope_rows(c("land", "inland_water", "ice")),
    deposition_kgn_ha = 0,
    deposition_n_t = 0
  )
  out <- whep:::.ni_deposition_in_scope(empty_cell, "land")
  testthat::expect_identical(out$scope_frac, 0)
  testthat::expect_false(anyNA(out$scope_frac))
})

# polity_validity threading (whep#727) ----------------------------------------

# Records the `polity_validity` each mocked builder was handed, then delegates
# to the real function so the assembly still runs end to end. The original
# closure is captured BEFORE the mock is installed, so calling it here does not
# re-enter the mock.
.nbi_validity_recorder <- function(seen, name, fn) {
  # Forced HERE, not at the first call: `fn` is passed as `whep::the_builder`,
  # and a promise forced after local_mocked_bindings() has run would resolve to
  # the mock itself and recurse until the C stack overflows.
  force(fn)
  force(name)
  function(...) {
    args <- list(...)
    seen[[name]] <- args$polity_validity
    do.call(fn, args)
  }
}

testthat::test_that("polity_validity reaches all four gridded builders", {
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    build_ag_land_support = .nbi_validity_recorder(
      seen,
      "ag_land_support",
      whep::build_ag_land_support
    ),
    build_n_deposition = .nbi_validity_recorder(
      seen,
      "deposition",
      whep::build_n_deposition
    ),
    build_urban_n = .nbi_validity_recorder(
      seen,
      "urban",
      whep::build_urban_n
    ),
    spatialize_country_n_to_crops = .nbi_validity_recorder(
      seen,
      "spatialize",
      whep::spatialize_country_n_to_crops
    )
  )
  data <- .nbi_full_data()
  # Derived rather than injected, so build_ag_land_support() is really called.
  data$ag_land_support <- NULL
  data$grassland_ha <- .nbi_grassland_ha()
  out <- whep::build_n_inputs(data = data, polity_validity = "flag")

  testthat::expect_equal(seen$ag_land_support, "flag")
  testthat::expect_equal(seen$deposition, "flag")
  testthat::expect_equal(seen$urban, "flag")
  testthat::expect_equal(seen$spatialize, "flag")
  testthat::expect_true(nrow(out) > 0L)
})

testthat::test_that("the polity_crop synthetic path forwards it too", {
  # The grid branch of the synthetic term is covered above; the polity_crop
  # branch is a second call site and gets its own check.
  seen <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    spatialize_country_n_to_crops = .nbi_validity_recorder(
      seen,
      "spatialize",
      whep::spatialize_country_n_to_crops
    )
  )
  data <- .nbi_full_data()
  data$cell_polity <- NULL
  data$urban_population <- NULL
  whep::build_n_inputs(
    resolution = "polity",
    data = data,
    polity_validity = "drop"
  )
  testthat::expect_equal(seen$spatialize, "drop")
})

testthat::test_that("polity_validity = flag adds the per-row flag column", {
  out <- whep::build_n_inputs(data = .nbi_full_data(), polity_validity = "flag")
  pointblank::expect_col_exists(out, "reporting_polity_out_of_span")
  testthat::expect_type(out$reporting_polity_out_of_span, "logical")
  testthat::expect_false(anyNA(out$reporting_polity_out_of_span))
  kept <- whep::build_n_inputs(data = .nbi_full_data())
  testthat::expect_false(
    "reporting_polity_out_of_span" %in% names(kept)
  )
})

testthat::test_that("the example fixture honours polity_validity", {
  out <- whep::build_n_inputs(example = TRUE, polity_validity = "flag")
  pointblank::expect_col_exists(out, "reporting_polity_out_of_span")
})

testthat::test_that("polity_validity is validated", {
  testthat::expect_error(
    whep::build_n_inputs(
      polity_validity = "discard",
      data = .nbi_full_data()
    ),
    "polity_validity"
  )
})
