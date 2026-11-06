.gwlp_test_inputs <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat, ~year, ~band_name,
    0.25, 0.25, 2010L, "rainfed grassland",
    0.25, 0.25, 2010L, "irrigated grassland",
    0.25, 50.25, 2010L, "rainfed grassland",
    0.25, 50.25, 2010L, "irrigated grassland"
  )
  list(
    green = dplyr::mutate(cells, value = c(100, NA, 200, 300)),
    blue = dplyr::mutate(cells, value = c(5, NA, 10, 20)),
    stand_frac = dplyr::mutate(cells, value = c(0.25, 0, 0.5, 0.1)),
    vintage = tibble::tribble(
      ~input, ~units, ~lpjml_version, ~lpjml_git_hash, ~lpjml_run,
      "green", "mm/yr", "synthetic", "fixture", "scenario_fixture",
      "blue", "mm/yr", "synthetic", "fixture", "scenario_fixture",
      "stand_frac", "", "synthetic", "fixture", "scenario_fixture"
    ) |>
      dplyr::mutate(
        spatial_basis = c("stand", "stand", "cell_fraction"),
        source_basis = "Explicit synthetic stand-depth fixture"
      )
  )
}

testthat::test_that("native ET preserves stand and component accounting", {
  out <- whep:::.gwlp_assemble(.gwlp_test_inputs(), 2010L)
  testthat::expect_equal(nrow(out), 8L)
  testthat::expect_setequal(out$component, c("green", "blue"))
  positive <- dplyr::filter(out, grass_area_ha > 0)
  testthat::expect_equal(
    positive$grass_water_m3,
    positive$water_mm * positive$grass_area_ha * 10
  )
  testthat::expect_equal(
    out$grass_area_ha,
    out$stand_frac * out$cell_area_ha
  )
  absent <- dplyr::filter(out, stand_frac == 0)
  testthat::expect_true(all(is.na(absent$water_mm)))
  testthat::expect_true(all(absent$grass_water_m3 == 0))
  testthat::expect_true(all(absent$water_input_status == "no_grass_stand"))
  testthat::expect_true(all(out$water_scenario == "scenario_fixture"))
  testthat::expect_false("area_code" %in% names(out))
  testthat::expect_false("quantity_basis" %in% names(out))
})

testthat::test_that("named bands and row order cannot move grazing water", {
  input <- .gwlp_test_inputs()
  expected <- whep:::.gwlp_assemble(input, 2010L)
  input$green <- dplyr::mutate(input$green[4:1, ], band = 1:4)
  input$blue <- dplyr::mutate(input$blue[c(2, 4, 1, 3), ], band = 4:1)
  actual <- whep:::.gwlp_assemble(input, 2010L)
  testthat::expect_identical(actual, expected)
})

testthat::test_that("absent input rows cannot be repaired with zeros", {
  input <- .gwlp_test_inputs()
  input$blue <- input$blue[-2, ]
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_coverage"
  )
  input <- .gwlp_test_inputs()
  input$green$value[1] <- NA_real_
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_coverage"
  )
  input <- .gwlp_test_inputs()
  input$stand_frac$value[1] <- NA_real_
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("run identity and physical units are required per cube", {
  input <- .gwlp_test_inputs()
  input$vintage$lpjml_git_hash[2] <- "another_run"
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_vintage"
  )
  input <- .gwlp_test_inputs()
  input$vintage <- input$vintage[-2, ]
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_vintage"
  )
  input <- .gwlp_test_inputs()
  input$vintage$units[1] <- "m3"
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("invalid grids, fractions and duplicates fail", {
  input <- .gwlp_test_inputs()
  input$blue <- dplyr::bind_rows(input$blue, input$blue[1, ])
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
  input <- .gwlp_test_inputs()
  input$stand_frac$value[3:4] <- c(0.8, 0.8)
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
  input <- .gwlp_test_inputs()
  input$blue$lon[1] <- 0.1
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("requested years and both grassland bands need coverage", {
  input <- .gwlp_test_inputs()
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2009:2010),
    class = "whep_grazing_water_coverage"
  )
  input$green <- dplyr::filter(input$green, band_name == "rainfed grassland")
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_coverage"
  )
})

testthat::test_that("depth units alone cannot establish the spatial basis", {
  input <- .gwlp_test_inputs()
  input$vintage$spatial_basis <- NULL
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
  input <- .gwlp_test_inputs()
  input$vintage$spatial_basis[1] <- "cell"
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
  input <- .gwlp_test_inputs()
  input$vintage$source_basis <- ""
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010L),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("shared cell-year omissions do not pass coverage checks", {
  input <- .gwlp_test_inputs()
  input[c("green", "blue", "stand_frac")] <- purrr::map(
    input[c("green", "blue", "stand_frac")],
    function(x) dplyr::bind_rows(x, dplyr::mutate(x[1:2, ], year = 2011L))
  )
  testthat::expect_error(
    whep:::.gwlp_assemble(input, 2010:2011),
    class = "whep_grazing_water_coverage"
  )
})

.gwlp_test_file <- function(spec, run_dir, swap = FALSE) {
  bands <- c("irrigated grassland", "rainfed grassland")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", c(0.25, 0.75))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", c(0.25, 0.75))
  pft <- ncdf4::ncdim_def("pft", "", 1:2)
  len <- ncdf4::ncdim_def("len", "", 1:19)
  time <- ncdf4::ncdim_def("time", "days since 1750-1-1 0:0:0", (0:2) * 365)
  dims <- if (swap) list(lat, lon, pft, time) else list(lon, lat, pft, time)
  value <- ncdf4::ncvar_def(
    spec$var,
    spec$units,
    dims,
    missval = -9999
  )
  name <- ncdf4::ncvar_def("NamePFT", "", list(len, pft), prec = "char")
  nc <- ncdf4::nc_create(file.path(run_dir, spec$file), list(value, name))
  on.exit(ncdf4::nc_close(nc))
  ncdf4::ncvar_put(nc, name, bands)
  values <- rep(c(spec$zero, NA, NA, NA, spec$positive, NA, NA, NA), 3L)
  ncdf4::ncvar_put(nc, value, array(values, dim = c(2, 2, 2, 3)))
  ncdf4::ncatt_put(nc, 0, "source", "Synthetic LPJmL fixture")
  ncdf4::ncatt_put(nc, 0, "GIT_hash", "fixture")
  ncdf4::ncatt_put(nc, 0, "history", "Mon Aug 31 14:13:10 2026: fixture.json")
  ncdf4::ncatt_put(nc, "time", "calendar", "noleap")
}

.gwlp_test_run <- function() {
  path <- withr::local_tempdir(.local_envir = parent.frame())
  specs <- tibble::tribble(
    ~file, ~var, ~units, ~zero, ~positive,
    "cft_consump_water_g.nc", "consump_water_g", "mm/yr", NA_real_, 100,
    "cft_consump_water_b.nc", "consump_water_b", "mm/yr", NA_real_, 5,
    "cftfrac.nc", "CFTfrac", "", 0, 0.25
  )
  purrr::walk(seq_len(nrow(specs)), function(i) {
    .gwlp_test_file(specs[i, ], path)
  })
  config <- file.path(path, "fixture.json")
  jsonlite::write_json(list(grid_scaled = FALSE), config, auto_unbox = TRUE)
  list(run_dir = path, config = config)
}

testthat::test_that("injected cubes need no run or implicit external read", {
  testthat::local_mocked_bindings(
    .gwlp_read = function(...) testthat::fail("injected inputs read a run"),
    .package = "whep"
  )
  out <- whep::build_grazing_water_lpjml(data = .gwlp_test_inputs())
  testthat::expect_identical(
    out,
    whep:::.gwlp_assemble(.gwlp_test_inputs(), 2010L)
  )
})

testthat::test_that("run reads require bounded years and an area basis", {
  testthat::expect_error(
    whep::build_grazing_water_lpjml(),
    class = "whep_grazing_water_input"
  )
  testthat::expect_error(
    whep::build_grazing_water_lpjml(years = 1750L),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("native files preserve names, years and provenance", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  out <- whep::build_grazing_water_lpjml(
    run$run_dir,
    years = c(1750L, 1752L),
    config = run$config
  )
  testthat::expect_setequal(out$year, c(1750L, 1752L))
  testthat::expect_equal(nrow(out), 8L)
  testthat::expect_true(all(out$lon == 0.25))
  zero <- dplyr::filter(out, stand_frac == 0)
  testthat::expect_equal(nrow(zero), 4L)
  testthat::expect_true(all(is.na(zero$water_mm)))
  testthat::expect_true(all(zero$grass_water_m3 == 0))
  testthat::expect_true(all(out$lpjml_git_hash == "fixture"))
  testthat::expect_true(all(out$water_input_basis == "stand"))
  testthat::expect_true(all(grepl(
    unname(tools::sha256sum(run$config)),
    out$source_water_basis,
    fixed = TRUE
  )))
})

testthat::test_that("mixed runs and grid-scaled depths fail", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  nc <- ncdf4::nc_open(
    file.path(run$run_dir, "cft_consump_water_b.nc"),
    write = TRUE
  )
  ncdf4::ncatt_put(nc, 0, "GIT_hash", "different")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_vintage"
  )
  jsonlite::write_json(list(grid_scaled = TRUE), run$config, auto_unbox = TRUE)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("time indices cannot relabel a discontinuous source", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  nc <- ncdf4::nc_open(
    file.path(run$run_dir, "cft_consump_water_b.nc"),
    write = TRUE
  )
  ncdf4::ncvar_put(nc, "time", c(0, 365, 1095))
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1752L, run$config),
    class = "whep_grazing_water_coverage"
  )
})

testthat::test_that("non-native time units and calendars cannot be assumed", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  nc <- ncdf4::nc_open(
    file.path(run$run_dir, "cft_consump_water_g.nc"),
    write = TRUE
  )
  ncdf4::ncatt_put(nc, "time", "units", "hours since 1750-1-1 0:0:0")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_coverage"
  )
  nc <- ncdf4::nc_open(
    file.path(run$run_dir, "cft_consump_water_g.nc"),
    write = TRUE
  )
  ncdf4::ncatt_put(nc, "time", "units", "days since 1750-1-1 0:0:0")
  ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_coverage"
  )
})

testthat::test_that("each component retains its own spatial-basis evidence", {
  input <- .gwlp_test_inputs()
  input$vintage$source_basis <- c(
    "Green evidence",
    "Blue evidence",
    "Area evidence"
  )
  out <- whep::build_grazing_water_lpjml(data = input)
  testthat::expect_true(all(
    out$source_water_basis[out$component == "blue"] == "Blue evidence"
  ))
  testthat::expect_true(all(
    out$source_water_basis[out$component == "green"] == "Green evidence"
  ))
  testthat::expect_identical(
    out$source_fraction_basis,
    rep("Area evidence", 8L)
  )
})

testthat::test_that("native provenance and area support are required", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  path <- file.path(run$run_dir, "cft_consump_water_b.nc")
  nc <- ncdf4::nc_open(path, write = TRUE)
  ncdf4::ncatt_put(nc, 0, "history", " ")
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_vintage"
  )
  nc <- ncdf4::nc_open(path, write = TRUE)
  ncdf4::ncatt_put(nc, 0, "history", "Mon Aug 31 14:13:10 2026: fixture.json")
  ncdf4::ncvar_put(
    nc,
    "consump_water_b",
    1,
    start = c(2, 1, 1, 1),
    count = c(1, 1, 1, 1)
  )
  ncdf4::nc_close(nc)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_coverage"
  )
})

testthat::test_that("invalid depths and missing columns do not become zeros", {
  input <- .gwlp_test_inputs()
  input$green$value[1] <- -1
  testthat::expect_error(
    whep::build_grazing_water_lpjml(data = input),
    class = "whep_grazing_water_input"
  )
  input$green$value[1] <- Inf
  testthat::expect_error(
    whep::build_grazing_water_lpjml(data = input),
    class = "whep_grazing_water_input"
  )
  input$green$value <- NULL
  testthat::expect_error(
    whep::build_grazing_water_lpjml(data = input),
    class = "whep_grazing_water_input"
  )
})

testthat::test_that("native dimensions cannot silently transpose grass cells", {
  testthat::skip_if_not_installed("ncdf4")
  run <- .gwlp_test_run()
  spec <- tibble::tibble(
    file = "cft_consump_water_g.nc",
    var = "consump_water_g",
    units = "mm/yr",
    zero = NA_real_,
    positive = 100
  )
  .gwlp_test_file(spec, run$run_dir, swap = TRUE)
  testthat::expect_error(
    whep::build_grazing_water_lpjml(run$run_dir, 1750L, run$config),
    class = "whep_grazing_water_input"
  )
})
