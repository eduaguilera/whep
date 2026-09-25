# The nitrogen balance's livestock inputs at the grain its resolution needs
# (whep#1300). One polity with dairy cattle and one crop, on two cells.

.gm_area <- function() {
  region <- whep:::.feed_region_lookup(whep::polity_area_crosswalk)
  bouwman_regions <- unique(whep::conv_bouwman$region_bouwman)
  region$area_code[region$region_bouwman %in% bouwman_regions][1]
}

.gm_production <- function(area) {
  tibble::tribble(
    ~year  , ~area_code , ~item_cbs_code , ~live_anim_code , ~item_prod_code , ~unit   , ~value ,
    2010L  , area       ,           960L , NA_character_   , "960"           , "heads" ,    1e5 ,
    2010L  , area       ,          2511L , NA_character_   , "15"            , "ha"    ,  2e4
  )
}

.gm_cbs <- function(area) {
  tibble::tribble(
    ~year , ~area_code , ~item_cbs_code , ~feed ,
    2010L , area       ,          2591L ,   5e4
  )
}

# The one cell support: cell A is wholly the polity's, cell B is a border
# cell it owns 0.6 of.
.gm_support <- function(area) {
  tibble::tribble(
    ~lon  , ~lat  , ~area_code , ~cell_area_frac ,
    10.25 , 50.25 , area       ,             1.0 ,
    10.75 , 50.25 , area       ,             0.6
  )
}

.gm_crop_patterns <- function() {
  tibble::tribble(
    ~lon  , ~lat  , ~item_prod_code , ~crop_area_ha ,
    10.25 , 50.25 , "15"            ,         30000 ,
    10.75 , 50.25 , "15"            ,         10000
  )
}

.gm_cells <- function() {
  c(whep:::.cell_id(10.25, 50.25), whep:::.cell_id(10.75, 50.25))
}

# What .local_spatial_inputs() returns, built from the support it is handed,
# so a test can see which support the heads were placed on.
.gm_spatial <- function(area, seen) {
  function(years, paths, country_grid = NULL) {
    seen$country_grid <- country_grid
    cells <- .gm_cells()
    list(
      cell_shares = tibble::tibble(
        year = 2010L,
        territory = as.character(area),
        livestock_category = "Cattle_milk",
        sub_territory = cells,
        cell_share = c(0.7, 0.3)
      ),
      grass_avail = tibble::tibble(
        year = 2010L,
        territory = as.character(area),
        sub_territory = cells,
        grass_avail_dm_t = c(1e5, 1e12)
      )
    )
  }
}

.gm_land_support <- function(area) {
  tibble::tribble(
    ~lon  , ~lat  , ~area_code , ~item_cbs_code , ~year , ~land_use   , ~area_ha ,
    10.25 , 50.25 , area       ,          2511L , 2010L , "cropland"  ,    30000 ,
    10.25 , 50.25 , area       ,          3000L , 2010L , "grassland" ,    50000 ,
    10.75 , 50.25 , area       ,          2511L , 2010L , "cropland"  ,    10000 ,
    10.75 , 50.25 , area       ,          3000L , 2010L , "grassland" ,    50000
  )
}

.gm_n_inputs <- function(intake, crops, area) {
  build_n_inputs(
    resolution = "grid",
    data = list(
      livestock_intake = intake,
      gridded = list(crops = crops),
      methods = list(allocation = list(cap_method = "fixed_ceiling")),
      ag_land_support = .gm_land_support(area)
    )
  )
}

test_that("grid intake is the local grain, placed on the support passed in", {
  area <- .gm_area()
  support <- .gm_support(area)
  seen <- new.env()
  testthat::local_mocked_bindings(
    .local_spatial_inputs = .gm_spatial(area, seen)
  )

  intake <- whep:::.n_livestock_intake(
    "grid",
    .gm_production(area),
    .gm_cbs(area),
    country_grid = support
  )

  expect_identical(seen$country_grid, support)
  expect_setequal(unique(intake$sub_territory), .gm_cells())
  expect_false(anyNA(intake$sub_territory))
})

test_that("grid intake runs the engine with its own border allowance", {
  area <- .gm_area()
  testthat::local_mocked_bindings(
    .local_spatial_inputs = .gm_spatial(area, new.env())
  )
  allowance <- NULL
  engine <- whep:::.run_redistribute_local
  testthat::local_mocked_bindings(
    .run_redistribute_local = function(
      production,
      cbs,
      demand_tier,
      spatial,
      data,
      distribute_surplus
    ) {
      allowance <<- spatial$grass_border_allowance
      expect_false(distribute_surplus)
      expect_identical(demand_tier, "ipcc")
      engine(production, cbs, demand_tier, spatial, data, distribute_surplus)
    }
  )

  whep:::.n_livestock_intake(
    "grid",
    .gm_production(area),
    .gm_cbs(area),
    country_grid = .gm_support(area)
  )

  expect_identical(
    allowance,
    whep:::.local_intake_defaults()$grass_border_allowance
  )
  expect_identical(allowance, 0.1)
})

test_that("grid crop layer spreads harvested area onto the support's cells", {
  area <- .gm_area()
  crops <- whep:::.n_manure_crop_layer(
    "grid",
    .gm_production(area),
    .gm_support(area),
    crop_patterns = .gm_crop_patterns()
  )

  expect_setequal(crops$sub_territory, .gm_cells())
  expect_equal(sum(crops$crop_area_ha), 2e4)
  # Weights are the cell's crop-pattern area times the polity's share:
  # 30000 x 1.0 against 10000 x 0.6.
  expect_equal(
    crops$crop_area_ha[crops$sub_territory == .gm_cells()[1]],
    2e4 * 30000 / (30000 + 6000)
  )
  expect_equal(crops$manure_n_receptivity, crops$crop_area_ha)
})

test_that("grid crop layer reports harvested area with no cell", {
  area <- .gm_area()
  production <- .gm_production(area) |>
    tibble::add_row(
      year = 2010L,
      area_code = area,
      item_cbs_code = 2514L,
      live_anim_code = NA_character_,
      item_prod_code = "56",
      unit = "ha",
      value = 5e3
    )

  expect_message(
    crops <- whep:::.n_manure_crop_layer(
      "grid",
      production,
      .gm_support(area),
      crop_patterns = .gm_crop_patterns()
    ),
    class = "whep_manure_crop_area_unplaced"
  )
  expect_equal(sum(crops$crop_area_ha), 2e4)
})

test_that("grid resolution needs a cell support", {
  area <- .gm_area()
  expect_error(
    whep:::.n_livestock_intake("grid", .gm_production(area), .gm_cbs(area)),
    "cell support"
  )
  expect_error(
    whep:::.n_manure_crop_layer("grid", .gm_production(area)),
    "cell support"
  )
})

test_that("grid n_inputs carries every manure term on cells", {
  area <- .gm_area()
  support <- .gm_support(area)
  testthat::local_mocked_bindings(
    .local_spatial_inputs = .gm_spatial(area, new.env())
  )
  intake <- whep:::.n_livestock_intake(
    "grid",
    .gm_production(area),
    .gm_cbs(area),
    country_grid = support
  )
  crops <- whep:::.n_manure_crop_layer(
    "grid",
    .gm_production(area),
    support,
    crop_patterns = .gm_crop_patterns()
  )

  out <- suppressMessages(.gm_n_inputs(intake, crops, area))
  manure <- dplyr::filter(
    out,
    .data$fert_type %in% c("excreta", "manure_solid", "manure_liquid")
  )

  expect_setequal(
    unique(manure$fert_type),
    c("excreta", "manure_solid", "manure_liquid")
  )
  expect_false(anyNA(manure$lon) || anyNA(manure$lat))
  expect_setequal(
    unique(whep:::.cell_id(manure$lon, manure$lat)),
    .gm_cells()
  )
  # Collected manure met the crop layer on its cells, so it lands on the crop
  # under the fixed ceiling rather than as crop-less nitrogen spread by area.
  collected <- dplyr::filter(manure, .data$fert_type != "excreta")
  expect_true(all(collected$item_cbs_code == 2511L))

  # And the N is the manure engine's own applied N, all of it.
  flows <- build_livestock_nutrient_flows(
    intake,
    resolution = "subnational",
    methods = list(allocation = list(cap_method = "fixed_ceiling")),
    gridded = list(crops = crops)
  )
  applied <- flows$applied |>
    dplyr::filter(.data$land_use %in% c("Cropland", "Grassland", "transported"))
  expect_equal(sum(manure$n_input_t), sum(applied$applied_n))
  expect_gt(sum(manure$n_input_t), 0)
})

test_that("national intake still aborts at grid, which is why grid is local", {
  area <- .gm_area()
  intake <- whep:::.n_livestock_intake(
    "polity",
    .gm_production(area),
    .gm_cbs(area)
  )
  crops <- whep:::.n_manure_crop_layer("polity", .gm_production(area))

  expect_true(all(is.na(intake$sub_territory)))
  expect_error(
    suppressMessages(.gm_n_inputs(intake, crops, area)),
    "missing spatial keys"
  )
})

test_that("polity resolution keeps the national grain unchanged", {
  area <- .gm_area()
  production <- .gm_production(area)
  cbs <- .gm_cbs(area)

  expect_identical(
    whep:::.n_livestock_intake("polity", production, cbs),
    whep:::.run_redistribute_national(
      production = production,
      cbs = cbs,
      demand_tier = "ipcc",
      options = list(distribute_surplus = FALSE)
    )
  )
  expect_identical(
    whep:::.n_manure_crop_layer("polity", production),
    whep:::.sci_manure_crop_layer(production)
  )
})
