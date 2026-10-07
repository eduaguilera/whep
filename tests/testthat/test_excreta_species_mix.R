# Fixtures ---------------------------------------------------------------------

# One polity (area_code 10): dairy cattle and sheep on grass, pigs housed. The
# cell id is NA for the national tests and a "lon_lat" id for the grid ones.
.esm_intake <- function(sub_territory = NA_character_) {
  tibble::tribble(
    ~year, ~territory, ~livestock_category, ~item_cbs_code, ~feed_quality,
    ~intake_dm_t,
    2010L, "10", "Cattle_milk", 2513L, "high_quality", 200,
    2010L, "10", "Cattle_milk", NA, "grass", 600,
    2010L, "10", "Sheep", NA, "grass", 300,
    2010L, "10", "Pigs", 2513L, "high_quality", 300
  ) |>
    dplyr::mutate(sub_territory = sub_territory, .after = "territory")
}

.esm_gridded <- function(sub_territory = NA_character_) {
  list(
    crops = tibble::tribble(
      ~year, ~territory, ~crop, ~manure_n_receptivity, ~crop_n_cap,
      2010L, "10", "44", 6, 500
    ) |>
      dplyr::mutate(sub_territory = sub_territory, .after = "territory")
  )
}

# The manure engine's per-category management rows, the same chain
# build_livestock_nutrient_flows() runs before allocation.
.esm_losses <- function(sub_territory = NA_character_) {
  .esm_intake(sub_territory) |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management() |>
    whep::apply_management_losses()
}

.esm_flows <- function(sub_territory = NA_character_, resolution = "national") {
  .esm_quiet(whep::build_livestock_nutrient_flows(
    .esm_intake(sub_territory),
    resolution = resolution,
    gridded = .esm_gridded(sub_territory)
  ))
}

# The engine's "Unknown or uninitialised column: `method_bedding_mms`"
# warning, raised on every call without bedding (pre-existing).
.esm_quiet <- function(expr) {
  withCallingHandlers(
    expr,
    warning = \(w) {
      if (grepl("method_bedding_mms", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# The CPP share of grazing N computed by hand from the management rows:
# cattle, poultry and pigs over all grazing N.
.esm_expected_share <- function(losses) {
  grazing <- dplyr::filter(losses, .data$stream == "grazing")
  cpp <- grazing$livestock_category %in%
    c("Cattle_milk", "Cattle_meat", "Pigs", "Poultry")
  sum(grazing$applied_n[cpp]) / sum(grazing$applied_n)
}

# The species classes ----------------------------------------------------------

testthat::test_that("the class table pins the IPCC 2019 CPP / SO split", {
  testthat::skip_if_not_installed("pointblank")
  classes <- whep:::.excreta_species_classes()
  pointblank::expect_col_vals_in_set(classes, "ef3prp_class", c("CPP", "SO"))
  pointblank::expect_col_vals_in_set(
    classes,
    "vocabulary",
    c("livestock_category", "species_group")
  )
  pointblank::expect_col_vals_not_null(classes, "source")
  testthat::expect_true(all(stringr::str_detect(classes$source, "Table 11.1")))
  testthat::expect_false(any(duplicated(classes[, c("vocabulary", "species")])))
  cpp <- classes |>
    dplyr::filter(.data$ef3prp_class == "CPP") |>
    dplyr::pull("species")
  testthat::expect_setequal(
    cpp,
    c(
      "Cattle_milk",
      "Cattle_meat",
      "Pigs",
      "Poultry",
      "cattle_dairy",
      "cattle_non_dairy",
      "buffalo",
      "pigs",
      "chickens_layers",
      "chickens_broilers",
      "poultry"
    )
  )
})

testthat::test_that("every engine livestock category has a class", {
  bridge <- whep:::.species_taxonomy_bridge()
  classes <- whep:::.excreta_species_classes() |>
    dplyr::filter(.data$vocabulary == "livestock_category")
  testthat::expect_setequal(classes$species, bridge$livestock_category)
})

testthat::test_that("every FAOSTAT species group has a class", {
  mapping <- system.file(
    "extdata",
    "livestock_mapping.csv",
    package = "whep"
  ) |>
    utils::read.csv(stringsAsFactors = FALSE)
  classes <- whep:::.excreta_species_classes() |>
    dplyr::filter(.data$vocabulary == "species_group")
  testthat::expect_setequal(classes$species, unique(mapping$species_group))
})

testthat::test_that("rabbits are other animals, not the pigs they proxy", {
  # The engine gives rabbits the swine species_gen as an energy and MMS proxy;
  # keyed on livestock_category they stay SO.
  bridge <- whep:::.species_taxonomy_bridge()
  testthat::expect_equal(
    bridge$species_gen[bridge$livestock_category == "Rabbits"],
    "Swine"
  )
  testthat::expect_false(
    whep:::.excreta_is_cpp("Rabbits", "livestock_category")
  )
  testthat::expect_true(whep:::.excreta_is_cpp("Pigs", "livestock_category"))
})

testthat::test_that("an unmapped species aborts", {
  testthat::expect_error(
    whep:::.excreta_is_cpp(c("Sheep", "Reindeer"), "livestock_category"),
    class = "whep_excreta_species_unmapped"
  )
  testthat::expect_error(
    whep:::.excreta_is_cpp("llamas", "species_group"),
    class = "whep_excreta_species_unmapped"
  )
})

testthat::test_that("a missing species is an absent input", {
  testthat::expect_error(
    whep:::.excreta_is_cpp(c("Sheep", NA), "livestock_category"),
    class = "whep_excreta_species_absent"
  )
})

# The manure engine ------------------------------------------------------------

testthat::test_that("engine excreta rows carry the grazing CPP share", {
  testthat::skip_if_not_installed("pointblank")
  res <- .esm_flows()
  excreta <- dplyr::filter(res$applied, .data$manure_type == "Excreta")
  testthat::expect_gt(nrow(excreta), 0L)
  pointblank::expect_col_vals_between(excreta, "excreta_cpp_share", 0, 1)
  pointblank::expect_col_vals_in_set(
    excreta,
    "method_excreta_species",
    "engine_livestock_category"
  )
  share <- .esm_expected_share(.esm_losses())
  # Both herds graze, so the mix is a real blend, not a corner.
  testthat::expect_gt(share, 0)
  testthat::expect_lt(share, 1)
  testthat::expect_equal(excreta$excreta_cpp_share, share, tolerance = 1e-12)
})

testthat::test_that("solid and liquid rows carry no species mix", {
  testthat::skip_if_not_installed("pointblank")
  res <- .esm_flows()
  other <- dplyr::filter(res$applied, .data$manure_type != "Excreta")
  testthat::expect_gt(nrow(other), 0L)
  pointblank::expect_col_exists(
    other,
    c("excreta_cpp_share", "method_excreta_species")
  )
  testthat::expect_true(all(is.na(other$excreta_cpp_share)))
  testthat::expect_true(all(is.na(other$method_excreta_species)))
})

testthat::test_that("attaching the mix adds no row and moves no nitrogen", {
  losses <- .esm_losses()
  alloc <- whep::allocate_manure_to_land(losses, .esm_gridded())
  out <- whep:::.attach_excreta_species(alloc, losses)
  testthat::expect_equal(nrow(out), nrow(alloc))
  testthat::expect_identical(out$applied_n, alloc$applied_n)
  # Excreta N is exactly the grazing N of the management rows.
  testthat::expect_equal(
    sum(out$applied_n[out$manure_type == "Excreta"]),
    sum(losses$applied_n[losses$stream == "grazing"]),
    tolerance = 1e-12
  )
})

testthat::test_that("the subnational engine carries the share per cell", {
  intake <- dplyr::bind_rows(
    .esm_intake("0.25_50.25"),
    .esm_intake("0.75_50.25") |>
      dplyr::filter(.data$livestock_category == "Sheep")
  )
  gridded <- list(
    crops = dplyr::bind_rows(
      .esm_gridded("0.25_50.25")$crops,
      .esm_gridded("0.75_50.25")$crops
    )
  )
  res <- .esm_quiet(whep::build_livestock_nutrient_flows(
    intake,
    resolution = "subnational",
    gridded = gridded
  ))
  excreta <- res$applied |>
    dplyr::filter(.data$source_stream == "grazing") |>
    dplyr::arrange(.data$sub_territory)
  losses <- intake |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management() |>
    whep::apply_management_losses()
  expected <- c(
    .esm_expected_share(dplyr::filter(
      losses,
      .data$sub_territory == "0.25_50.25"
    )),
    .esm_expected_share(dplyr::filter(
      losses,
      .data$sub_territory == "0.75_50.25"
    ))
  )
  testthat::expect_equal(excreta$excreta_cpp_share, expected, tolerance = 1e-12)
  # The sheep-only cell is wholly SO.
  testthat::expect_equal(excreta$excreta_cpp_share[2], 0)
})

testthat::test_that("absent species are refused although excreta N balances", {
  losses <- dplyr::select(.esm_losses(), -"livestock_category")
  alloc <- whep::allocate_manure_to_land(losses, .esm_gridded())
  expect_supplied_guard(
    identity = isTRUE(all.equal(
      sum(alloc$applied_n[alloc$manure_type == "Excreta"]),
      sum(losses$applied_n[losses$stream == "grazing"])
    )),
    guard = whep:::.attach_excreta_species(alloc, losses)
  )
})

testthat::test_that("an unmapped engine category aborts", {
  losses <- dplyr::mutate(
    .esm_losses(),
    livestock_category = dplyr::if_else(
      .data$livestock_category == "Sheep",
      "Reindeer",
      .data$livestock_category
    )
  )
  alloc <- whep::allocate_manure_to_land(losses, .esm_gridded())
  testthat::expect_error(
    whep:::.attach_excreta_species(alloc, losses),
    class = "whep_excreta_species_unmapped"
  )
})

testthat::test_that("a grazing row with no cell mix is refused", {
  losses <- .esm_losses()
  alloc <- whep::allocate_manure_to_land(losses, .esm_gridded())
  # The management rows of another polity: the join finds no mix.
  elsewhere <- dplyr::mutate(losses, territory = "11")
  testthat::expect_error(
    whep:::.attach_excreta_species(alloc, elsewhere),
    class = "whep_excreta_species_absent"
  )
})

testthat::test_that("a fallback Excreta label is stamped, not guessed", {
  losses <- .esm_losses()
  alloc <- whep::allocate_manure_to_land(losses, .esm_gridded())
  # The shape .apply_type_split() leaves when it finds no manure-type share
  # for transported manure: an "Excreta" row that is not grazing deposition.
  fallback <- alloc |>
    dplyr::filter(.data$source_stream == "grazing") |>
    dplyr::mutate(source_stream = "transported", land_use = "Cropland")
  out <- whep:::.attach_excreta_species(
    dplyr::bind_rows(alloc, fallback),
    losses
  )
  stamped <- dplyr::filter(out, .data$source_stream == "transported")
  testthat::expect_equal(
    stamped$method_excreta_species,
    "fallback_label_no_species"
  )
  testthat::expect_true(is.na(stamped$excreta_cpp_share))
})

# The nitrogen-input table -----------------------------------------------------

.esm_applied <- function() {
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~manure_type,
    ~applied_n, ~excreta_cpp_share, ~method_excreta_species,
    2010L, "10", "0.25_50.25", "Grassland", NA, "Excreta",
    3, 1, "engine_livestock_category",
    2010L, "10", "0.25_50.25", "Grassland", NA, "Excreta",
    1, 0, "engine_livestock_category",
    2010L, "10", "0.25_50.25", "Cropland", "44", "Solid",
    5, NA, NA
  )
}

testthat::test_that("folded excreta rows take the N-weighted share", {
  out <- whep:::.manure_to_n_inputs(.esm_applied())
  excreta <- dplyr::filter(out, .data$fert_type == "excreta")
  testthat::expect_equal(nrow(excreta), 1L)
  testthat::expect_equal(excreta$n_input_t, 4)
  testthat::expect_equal(excreta$excreta_cpp_share, 0.75)
  testthat::expect_equal(
    excreta$method_excreta_species,
    "engine_livestock_category"
  )
  solid <- dplyr::filter(out, .data$fert_type == "manure_solid")
  testthat::expect_true(is.na(solid$excreta_cpp_share))
  testthat::expect_true(is.na(solid$method_excreta_species))
})

testthat::test_that("one fallback tonne in a row leaves its share undefined", {
  applied <- dplyr::bind_rows(
    .esm_applied(),
    tibble::tibble(
      year = 2010L,
      territory = "10",
      sub_territory = "0.25_50.25",
      land_use = "Grassland",
      crop = NA_character_,
      manure_type = "Excreta",
      applied_n = 0.5,
      excreta_cpp_share = NA_real_,
      method_excreta_species = "fallback_label_no_species"
    )
  )
  out <- whep:::.manure_to_n_inputs(applied)
  excreta <- dplyr::filter(out, .data$fert_type == "excreta")
  testthat::expect_equal(nrow(excreta), 1L)
  testthat::expect_equal(excreta$n_input_t, 4.5)
  testthat::expect_true(is.na(excreta$excreta_cpp_share))
  testthat::expect_equal(
    excreta$method_excreta_species,
    "fallback_label_no_species"
  )
})

testthat::test_that("excreta rows with no species stamp are refused", {
  applied <- dplyr::select(
    .esm_applied(),
    -"excreta_cpp_share",
    -"method_excreta_species"
  )
  expect_supplied_guard(
    identity = sum(applied$applied_n[applied$manure_type == "Excreta"]) == 4,
    guard = whep:::.manure_to_n_inputs(applied)
  )
})

testthat::test_that("a table with no excreta needs no species columns", {
  testthat::skip_if_not_installed("pointblank")
  applied <- .esm_applied() |>
    dplyr::filter(.data$manure_type == "Solid") |>
    dplyr::select(-"excreta_cpp_share", -"method_excreta_species")
  out <- whep:::.manure_to_n_inputs(applied)
  pointblank::expect_col_exists(
    out,
    c("excreta_cpp_share", "method_excreta_species")
  )
  testthat::expect_true(all(is.na(out$excreta_cpp_share)))
})

testthat::test_that("the weighted share and stamp follow the rows with N", {
  testthat::expect_equal(
    whep:::.excreta_weighted_share(c(1, 0, NA), c(3, 1, 0)),
    0.75
  )
  testthat::expect_true(is.na(whep:::.excreta_weighted_share(
    c(1, NA),
    c(3, 1)
  )))
  testthat::expect_true(is.na(whep:::.excreta_weighted_share(c(1, 0), c(0, 0))))
  testthat::expect_equal(
    whep:::.excreta_method_stamp(c("a", "a"), c(NA, NA), c(0, 0)),
    "no_excreta_n"
  )
  testthat::expect_true(is.na(
    whep:::.excreta_method_stamp(c(NA, NA), c(NA, NA), c(1, 1))
  ))
})

testthat::test_that("build_n_inputs carries the engine mix to excreta rows", {
  data <- list(
    livestock_intake = .esm_intake(),
    gridded = .esm_gridded(),
    resolution = "national"
  )
  out <- .esm_quiet(whep::build_n_inputs(resolution = "polity", data = data))
  excreta <- dplyr::filter(out, .data$fert_type == "excreta")
  testthat::expect_equal(nrow(excreta), 1L)
  testthat::expect_equal(
    excreta$excreta_cpp_share,
    .esm_expected_share(.esm_losses()),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    excreta$method_excreta_species,
    "engine_livestock_category"
  )
  testthat::expect_true(all(is.na(
    out$excreta_cpp_share[out$fert_type != "excreta"]
  )))
})

# The FAOSTAT option -----------------------------------------------------------

.esm_faostat_species <- function() {
  tibble::tribble(
    ~year, ~area_code, ~species_group, ~pasture_n_t,
    2010L, 10L, "cattle_dairy", 3,
    2010L, 10L, "sheep_goats", 1,
    2010L, 10L, "chickens_layers", 0.5
  )
}

testthat::test_that("the FAOSTAT polity path keeps the species mix", {
  out <- whep:::.ni_pasture_polity(.esm_faostat_species())
  testthat::expect_equal(out$n_input_t, 4.5)
  testthat::expect_equal(out$excreta_cpp_share, 3.5 / 4.5)
  testthat::expect_equal(out$method_excreta_species, "faostat_species_group")
})

testthat::test_that("a FAOSTAT row with no pasture N is stamped", {
  species <- dplyr::mutate(.esm_faostat_species(), pasture_n_t = 0)
  out <- whep:::.ni_pasture_polity(species)
  testthat::expect_true(is.na(out$excreta_cpp_share))
  testthat::expect_equal(out$method_excreta_species, "no_excreta_n")
})

testthat::test_that("gridded FAOSTAT rows without species are refused", {
  gridded <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~pasture_n_t,
    0.25, 50.25, 10L, 2010L, 4.5
  )
  expect_supplied_guard(
    identity = sum(gridded$pasture_n_t) == 4.5,
    guard = whep:::.ni_faostat_cpp_n(gridded, "pasture_n_t")
  )
})

testthat::test_that("an unmapped FAOSTAT species group aborts", {
  species <- dplyr::mutate(
    .esm_faostat_species(),
    species_group = dplyr::if_else(
      .data$species_group == "sheep_goats",
      "reindeer",
      .data$species_group
    )
  )
  testthat::expect_error(
    whep:::.ni_pasture_polity(species),
    class = "whep_excreta_species_unmapped"
  )
})
