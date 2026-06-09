testthat::test_that("get_land_fp_production example returns expected structure", {
  result <- get_land_fp_production(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  pointblank::expect_col_exists(
    result,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "impact",
      "element",
      "origin",
      "group",
      "impact_u",
      "extension_scope"
    )
  )
  testthat::expect_type(result$year, "integer")
  testthat::expect_type(result$area_code, "integer")
  testthat::expect_type(result$item_cbs_code, "integer")
  pointblank::expect_col_vals_in_set(
    result,
    impact,
    set = "Land"
  )
  pointblank::expect_col_vals_in_set(
    result,
    origin,
    set = "Production"
  )
  pointblank::expect_col_vals_in_set(
    result,
    extension_scope,
    set = "occupation"
  )
})


testthat::test_that("get_land_fp_production filters and cleans land_fp", {
  local_mocked_bindings(
    whep_read_file = function(...) {
      tibble::tribble(
        ~year, ~area, ~item_code, ~Impact, ~element, ~Origin, ~group, ~impact_u,
        2020, "Spain", 2511, "Land", "Cropland", "Production", "Primary crops", 10,
        2020, "Spain", 2571, "Land", "Production", "Production", "Crop products", 20,
        2020, "Spain", 2905, "Land", "Production", "Production", "Crop residues", 30,
        2020, "Spain", 2960, "Land", "Production", "Production", "Draught", 40,
        2020, "Spain", 2731, "Land", "Production", "Production", "Livestock products", 20,
        2020, "Spain", 2511, "Water", "Blue", "Production", "Crops", 2,
        2020, "Spain", 2511, "Land", "Cropland", "Import", "Crops", 3
      )
    },
    add_area_code = function(data, name_column, code_column) {
      data[[code_column]] <- dplyr::if_else(
        data[[name_column]] == "Spain",
        203L,
        NA_integer_
      )
      data
    }
  )

  result <- get_land_fp_production()

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_false(any(result$group %in% c(
    "Crop products",
    "Crop residues",
    "Draught",
    "Livestock products"
  )))
  pointblank::expect_col_vals_in_set(
    result,
    impact,
    set = "Land"
  )
  pointblank::expect_col_vals_in_set(
    result,
    origin,
    set = "Production"
  )
  testthat::expect_type(result$year, "integer")
  testthat::expect_type(result$area_code, "integer")
  testthat::expect_type(result$item_cbs_code, "integer")
  testthat::expect_equal(result$area_code[[1]], 203L)
  testthat::expect_equal(result$extension_scope[[1]], "occupation")
})

testthat::test_that("get_land_fp_production can use active grazing metric", {
  local_mocked_bindings(
    whep_read_file = function(...) {
      tibble::tribble(
        ~year, ~area, ~item_code, ~Impact, ~element, ~Origin, ~group, ~impact_u,
        2020, "Spain", 2511, "Land", "Cropland", "Production", "Crops", 10,
        2020, "Spain", 3000, "Land", "Production", "Production", "Grass", 100,
        2020, "Spain", 3002, "Land", "Production", "Production", "Grass", 50
      )
    },
    get_feed_intake = function(...) {
      tibble::tribble(
        ~year, ~area_code, ~live_anim_code, ~item_cbs_code, ~feed_type,
        ~supply, ~intake, ~intake_dry_matter, ~loss, ~loss_share,
        2020, 203, 1053, 3000, "grass", 80, 70, 60, 10, 0.125,
        2020, 203, 1053, 3002, "grass", 300, 250, 200, 50, 0.167
      )
    }
  )

  result <- get_land_fp_production(
    grassland_metric = "active_grazing",
    usable_grass_yield_dm_t_ha = 2
  )

  grass <- result |>
    dplyr::filter(.data$item_cbs_code %in% c(3000L, 3002L)) |>
    dplyr::arrange(.data$item_cbs_code)

  testthat::expect_equal(result$extension_scope, rep("active_grazing", 3))
  testthat::expect_equal(
    grass$impact_u,
    c(30, 50),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    grass$impact_u_occupation,
    c(100, 50),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    grass$active_fraction,
    c(0.3, 1),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    result$impact_u[result$item_cbs_code == 2511L],
    10
  )
})

testthat::test_that("get_land_fp_production can return both grassland scopes", {
  local_mocked_bindings(
    whep_read_file = function(...) {
      tibble::tribble(
        ~year, ~area, ~item_code, ~Impact, ~element, ~Origin, ~group, ~impact_u,
        2020, "Spain", 2511, "Land", "Cropland", "Production", "Crops", 10,
        2020, "Spain", 3000, "Land", "Production", "Production", "Grass", 100
      )
    },
    get_feed_intake = function(...) {
      tibble::tribble(
        ~year, ~area_code, ~live_anim_code, ~item_cbs_code, ~feed_type,
        ~supply, ~intake, ~intake_dry_matter, ~loss, ~loss_share,
        2020, 203, 1053, 3000, "grass", 80, 70, 60, 10, 0.125
      )
    }
  )

  result <- get_land_fp_production(
    grassland_metric = "both",
    usable_grass_yield_dm_t_ha = 2
  )

  testthat::expect_equal(nrow(result), 4)
  testthat::expect_equal(
    sort(unique(result$extension_scope)),
    c("active_grazing", "occupation")
  )
  testthat::expect_equal(
    result |>
      dplyr::filter(
        .data$extension_scope == "active_grazing",
        .data$item_cbs_code == 3000L
      ) |>
      dplyr::pull(.data$impact_u),
    30
  )
})

testthat::test_that("get_land_fp_production rejects invalid active grazing yield", {
  testthat::expect_error(
    get_land_fp_production(
      example = TRUE,
      grassland_metric = "active_grazing",
      usable_grass_yield_dm_t_ha = 0
    ),
    "usable_grass_yield_dm_t_ha"
  )
})

testthat::test_that("get_land_fp_production resolves regions_full area aliases", {
  local_mocked_bindings(
    whep_read_file = function(...) {
      tibble::tribble(
        ~year, ~area, ~item_code, ~Impact, ~element, ~Origin, ~group, ~impact_u,
        2020, "Iran", 2511, "Land", "Cropland", "Production", "Crops", 10,
        2020, "Bolivia", 2511, "Land", "Cropland", "Production", "Crops", 20,
        2020, "DR Congo", 2511, "Land", "Cropland", "Production", "Crops", 30,
        2020, "Syria", 2511, "Land", "Cropland", "Production", "Crops", 40,
        2020, "China, Taiwan", 2511, "Land", "Cropland", "Production", "Crops", 50,
        2020, "Ethiopia", 2511, "Land", "Cropland", "Production", "Crops", 60,
        2020, "Sudan", 2511, "Land", "Cropland", "Production", "Crops", 70,
        2020, "Asia Other", 2511, "Land", "Cropland", "Production", "Crops", 80
      )
    }
  )

  result <- get_land_fp_production()

  testthat::expect_equal(
    result$area_code,
    c(102L, 19L, 250L, 212L, 214L, 238L, 276L, 902L)
  )
  testthat::expect_false(anyNA(result$area_code))
})
