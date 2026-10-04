# Tests of the SJOS-N production driver (R/sjos_n_run.R, run by
# inst/scripts/run_sjos_nitrogen.R). Every run writes into a temporary
# directory: a fixture balance root with its manifest, and an output root, both
# under withr::local_tempdir(); each test that writes asserts the files landed
# there.
#
# The boundary inputs are the grassland-split fixture of
# helper_nbx_grassland_split.R (cells A-G, countries 1 and 2), turned into a
# balance whose harvest-removal surplus is the fixture's surplus; the
# nourishment inputs are the package's coherent SJOS-N fixture
# (.sjos_n_example_data(), countries 1 and 2), moved to the run's year.

.sjr_test_years <- c(2015L, 2016L)

.sjr_sha <- function() list(sha = strrep("a", 40), clean = TRUE)

.sjr_context <- function(whep = .sjr_sha()) {
  list(readers = .sjr_gs_readers(), whep = whep)
}

# A balance with the given harvest-removal surplus per row: inputs are the
# positive part of the surplus plus 1 t, and the product removal makes up the
# difference, so calculate_n_surplus() returns `surplus_n_t` exactly.
.sjr_balance <- function(rows) {
  rows |>
    dplyr::mutate(
      n_input_std_t = pmax(.data$surplus_n_t, 0) + 1,
      prod_n_t = .data$n_input_std_t - .data$surplus_n_t,
      used_residue_n_t = 0,
      grazed_weeds_n_t = 0,
      burnt_residue_n_t = 0,
      n_balance_t = .data$surplus_n_t
    ) |>
    dplyr::select(-"surplus_n_t")
}

.sjr_gs_balance <- function(year) {
  .gs_surplus(year) |>
    dplyr::select(-"n_input_std_t") |>
    .sjr_balance()
}

# The nourishment-side inputs of the package fixture, moved to `year`.
.sjr_nourishment_data <- function(year) {
  base <- whep:::.sjos_n_example_data()
  keys <- c(
    "cbs_food",
    "population",
    "population_age",
    "habitual_cv",
    "biomass_coefs",
    "items_full",
    "ag_land"
  )
  purrr::map(base[keys], \(x) {
    if (rlang::has_name(x, "year")) {
      dplyr::mutate(x, year = as.integer(.env$year))
    } else {
      x
    }
  })
}

# The binding table of the critical surface `critical` (lon, value): the
# deposited "mi" is the surface itself and groundwater is the lowest of the
# three threshold-specific surpluses everywhere.
.sjr_binding <- function(critical, land_use) {
  layer <- \(value, threshold, var = "critical_n_surplus") {
    tibble::tibble(
      lon = critical$lon,
      lat = critical$lat,
      value = value,
      critical_var = var,
      critical_threshold = threshold,
      critical_land_use = land_use
    )
  }
  mi <- critical$value
  values <- list(de = mi + 10, gw = mi, sw = mi + 5, mi = mi)
  whep::build_critical_n_binding(
    purrr::imap(values, layer),
    purrr::map(
      c(de = "de", gw = "gw", sw = "sw"),
      \(threshold) layer(rep(-5, length(mi)), threshold, "exceedance")
    ),
    land_use = land_use
  )
}

# Readers for the grassland-split scenario. `extra(year)` adds the
# nourishment inputs and the four grassland-split elements for that year.
.sjr_gs_readers <- function(cbs_food = NULL) {
  list(
    critical = \(options) {
      .gs_critical(options$land_use) |>
        dplyr::mutate(
          value = .data$critical_kgn_ha,
          critical_var = "critical_n_surplus"
        ) |>
        dplyr::select(-"critical_kgn_ha")
    },
    binding = \(options) {
      .sjr_binding(
        .sjr_gs_readers()$critical(options),
        options$land_use
      )
    },
    cbs_food = cbs_food %||% \(year) .sjr_nourishment_data(year)$cbs_food,
    extra = \(year) {
      c(
        .sjr_nourishment_data(year)[setdiff(
          names(.sjr_nourishment_data(year)),
          "cbs_food"
        )],
        list(grassland = .gs_grassland(classes = .gs_classes(year)))
      )
    }
  )
}

# A balance root holding one grid partition per year and the manifest that
# lists them, with a driver report per year carrying `messages`.
.sjr_march <- function(dir, balances, reports = list()) {
  parts <- purrr::imap(balances, \(balance, year) {
    path <- file.path(
      dir,
      "whep_n_balance_grid",
      sprintf("year=%s", year),
      "part.parquet"
    )
    dir.create(dirname(path), recursive = TRUE)
    arrow::write_parquet(balance, path)
    list(year = as.integer(year), resolution = "grid", rows = nrow(balance))
  })
  manifest <- list(
    schema_version = 9L,
    whep_commit = strrep("b", 40),
    partitions = c(
      unname(parts),
      list(list(year = 2015L, resolution = "polity", rows = 3L))
    ),
    driver_report = reports
  )
  jsonlite::write_json(
    manifest,
    file.path(dir, "whep_n_balance_run_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  dir
}

.sjr_report <- function(messages, resolution = "grid") {
  conditions <- purrr::map(messages, \(m) list(class = "warning", message = m))
  if (resolution == "grid") {
    list(
      resolution = "grid",
      stages = list(
        list(input = "cell_polity", conditions = list()),
        list(input = "n_inputs", conditions = conditions)
      )
    )
  } else {
    list(
      resolution = resolution,
      stages = list(list(input = "n_inputs", conditions = list())),
      second_resolution_conditions = list(grid = conditions)
    )
  }
}

.sjr_uniform_message <- function(crops, n_t) {
  paste0(
    "! ",
    crops,
    " polity-crop totals (",
    n_t,
    " t N) had no crop-pattern",
    " grid cells;\n  reallocating uniformly across the polity's cropland",
    " cells.\nℹ Affected item_cbs_codes: 2511 and 2513."
  )
}

.sjr_gs_march <- function(dir, years = .sjr_test_years, reports = NULL) {
  balances <- purrr::set_names(
    purrr::map(years, .sjr_gs_balance),
    as.character(years)
  )
  reports <- reports %||%
    list("2015" = .sjr_report(.sjr_uniform_message(2, "12.5")))
  .sjr_march(dir, balances, reports)
}

.sjr_quiet <- function(expr) suppressMessages(suppressWarnings(expr))

.sjr_read <- function(root, product, year) {
  tibble::as_tibble(arrow::read_parquet(
    file.path(root, product, sprintf("year=%d", year), "part.parquet")
  ))
}

.sjr_in_tempdir <- function(paths, dir) {
  normalized <- normalizePath(paths, winslash = "/")
  all(startsWith(normalized, normalizePath(dir, winslash = "/")))
}

# One primary-option run over both years, built once for the read-only tests.
.sjr_primary_run <- function() {
  memo_fixture("sjr_primary", \() {
    dir <- tempfile("sjr_primary_")
    dir.create(dir)
    march <- .sjr_gs_march(file.path(dir, "march"))
    out <- file.path(dir, "out")
    manifest <- .sjr_quiet(whep:::.sjr_run(
      years = .sjr_test_years,
      march_root = march,
      out_root = out,
      context = .sjr_context()
    ))
    list(dir = dir, march = march, out = out, manifest = manifest)
  })
}

testthat::test_that("the year loop writes every product for every year", {
  run <- .sjr_primary_run()
  products <- whep:::.sjr_products()
  expected <- file.path(
    run$out,
    rep(products, each = 2L),
    sprintf("year=%d", .sjr_test_years),
    "part.parquet"
  )
  testthat::expect_true(all(file.exists(expected)))
  # Everything the run wrote is under the temporary directory.
  written <- list.files(run$dir, recursive = TRUE, full.names = TRUE)
  testthat::expect_true(.sjr_in_tempdir(written, run$dir))
  testthat::expect_true(.sjr_in_tempdir(run$manifest, run$dir))
  for (product in products) {
    for (year in .sjr_test_years) {
      part <- .sjr_read(run$out, product, year)
      testthat::expect_gt(nrow(part), 0L)
      testthat::expect_equal(unique(part$year), year)
    }
  }
  manifest <- jsonlite::read_json(run$manifest)
  testthat::expect_equal(unlist(manifest$years), .sjr_test_years)
  testthat::expect_length(manifest$partitions, 2L * length(products))
  rows <- purrr::map_int(manifest$partitions, "rows")
  read_rows <- purrr::map_int(manifest$partitions, \(p) {
    nrow(arrow::read_parquet(file.path(run$out, p$path)))
  })
  testthat::expect_equal(rows, read_rows)
  binding <- arrow::read_parquet(
    file.path(run$out, "whep_sjos_n_critical_binding", "part.parquet")
  )
  testthat::expect_equal(nrow(binding), 5L)
  testthat::expect_true(all(binding$critical_land_use == "all"))
})

testthat::test_that("the default years run from 1961 to the last grid year", {
  march <- list(grid = tibble::tibble(year = 1961:1963, rows = 1L))
  testthat::expect_equal(whep:::.sjr_years(NULL, march), 1961:1963)
  testthat::expect_equal(whep:::.sjr_years(c(1963L, 1962L), march), 1962:1963)
  testthat::expect_error(
    whep:::.sjr_years(1960:1961, march),
    class = "whep_sjr_missing_years"
  )
  gap <- list(grid = tibble::tibble(year = c(1961L, 1963L), rows = 1L))
  testthat::expect_error(
    whep:::.sjr_years(NULL, gap),
    class = "whep_sjr_missing_years"
  )
})

testthat::test_that("the written tables are the build_sjos_nitrogen() tables", {
  run <- .sjr_primary_run()
  year <- 2016L
  readers <- .sjr_gs_readers()
  options <- whep:::.sjr_options()
  direct <- .sjr_quiet(whep::build_sjos_nitrogen(
    data = c(
      list(
        balance = .sjr_gs_balance(year),
        critical = readers$critical(options),
        critical_binding = readers$binding(options),
        cbs_food = readers$cbs_food(year)
      ),
      readers$extra(year)
    ),
    boundary_land_use = "all",
    negative_critical = "clamp",
    country_table = TRUE,
    include = character()
  ))
  same <- \(written, built) {
    testthat::expect_equal(
      as.data.frame(written[names(built)]),
      as.data.frame(built),
      ignore_attr = TRUE
    )
  }
  same(.sjr_read(run$out, "whep_sjos_n", year), direct$boundary_surplus$country)
  same(
    .sjr_read(run$out, "whep_sjos_n_grid", year),
    direct$boundary_surplus$grid
  )
  country <- .sjr_read(run$out, "whep_sjos_n_country", year)
  same(
    dplyr::mutate(country, sjos_class = as.character(.data$sjos_class)),
    dplyr::mutate(
      direct$country_table$country,
      sjos_class = as.character(.data$sjos_class)
    )
  )
  # The columns downstream consumers read are present.
  testthat::expect_true(all(
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "exceedance_n_t",
      "critical_reference_year"
    ) %in%
      names(.sjr_read(run$out, "whep_sjos_n", year))
  ))
  testthat::expect_true(all(
    c(
      "input_std_n_t",
      "positive_surplus_n_t",
      "ag_area_ha",
      "beyond_share",
      "boundary_side",
      "signed_denominator_nonpositive",
      "ratio_outside_unit"
    ) %in%
      names(country)
  ))
  grid <- .sjr_read(run$out, "whep_sjos_n_grid", year)
  # The binding threshold names the impact behind a cell's critical surplus,
  # so the extensive-grassland component, compared with IMAGE's 2010 budget,
  # carries none.
  compared <- dplyr::filter(grid, .data$coverage_state == "valid")
  # Cells outside the binding table (C and F, absent from the deposited
  # surface) have no label.
  in_binding <- compared$lon %in% .gs_critical()$lon
  managed <- compared$boundary_component == "managed" & in_binding
  testthat::expect_true(any(managed) && any(!managed))
  testthat::expect_true(all(
    compared$binding_threshold[managed] == "groundwater"
  ))
  testthat::expect_true(all(is.na(compared$binding_threshold[!managed])))
})

testthat::test_that("the options are stamped on the tables and the manifest", {
  run <- .sjr_primary_run()
  for (product in c("whep_sjos_n", "whep_sjos_n_grid", "whep_sjos_n_country")) {
    part <- .sjr_read(run$out, product, 2015L)
    testthat::expect_equal(unique(part$negative_critical), "clamp")
    testthat::expect_equal(unique(part$land_use), "all")
    testthat::expect_equal(unique(part$grassland_split), "image_density")
  }
  for (product in c(
    "whep_sjos_n_country",
    "whep_sjos_n_class",
    "whep_sjos_n_nourishment",
    "whep_sjos_n_diag"
  )) {
    part <- .sjr_read(run$out, product, 2015L)
    testthat::expect_equal(unique(part$nourishment_thresholds), "composed")
  }
  manifest <- jsonlite::read_json(run$manifest)
  testthat::expect_equal(manifest$options$land_use, "all")
  testthat::expect_equal(manifest$options$negative_critical, "clamp")
  testthat::expect_equal(manifest$options$grassland_split, "image_density")
  testthat::expect_equal(manifest$options$nourishment_thresholds, "composed")
  testthat::expect_equal(manifest$options$critical_threshold, "mi")
  testthat::expect_equal(manifest$options$surplus_method, "harvest_removal")
  testthat::expect_equal(manifest$options$beyond_share_cut, 0.5)
  testthat::expect_equal(manifest$options$boundary_mode, "surplus")
  testthat::expect_match(manifest$boundary_modes$pathway, "#359")
  testthat::expect_match(
    manifest$declared_departures$negative_critical,
    "departure from Schulte-Uebbing"
  )
  testthat::expect_true(manifest$arm$primary)
  testthat::expect_equal(
    manifest$resolved_defaults$loss_wedge_method,
    "gustavsson_half_min"
  )
})

testthat::test_that("the manifest names the WHEP commit and the balance hash", {
  run <- .sjr_primary_run()
  manifest <- jsonlite::read_json(run$manifest)
  testthat::expect_equal(manifest$whep_sha, strrep("a", 40))
  testthat::expect_true(manifest$whep_tree_clean)
  march_manifest <- file.path(run$march, "whep_n_balance_run_manifest.json")
  testthat::expect_equal(
    manifest$input_march_manifest_hash,
    unname(tools::sha256sum(march_manifest))
  )
  testthat::expect_equal(
    manifest$input_march_manifest$whep_commit,
    strrep("b", 40)
  )
  testthat::expect_equal(manifest$schema_version, 1L)
  testthat::expect_equal(
    sort(purrr::map_chr(manifest$products, "dir")),
    sort(unname(whep:::.sjr_products()))
  )
  # Each partition record's md5 is the file's.
  for (p in manifest$partitions) {
    testthat::expect_equal(
      p$md5,
      unname(tools::md5sum(file.path(run$out, p$path)))
    )
  }
  testthat::expect_equal(
    manifest$diagnostics[["2015"]]$reconciliation_status,
    "pass"
  )
})

testthat::test_that("the country table carries the chain's own population", {
  run <- .sjr_primary_run()
  country <- .sjr_read(run$out, "whep_sjos_n_country", 2015L)
  nourishment <- .sjr_read(run$out, "whep_sjos_n_nourishment", 2015L)
  joined <- dplyr::left_join(
    dplyr::select(country, "area_code", "population"),
    dplyr::select(nourishment, "area_code", expected = "population"),
    by = "area_code"
  )
  testthat::expect_equal(joined$population, joined$expected)
  testthat::expect_equal(
    sort(country$population),
    sort(.sjr_nourishment_data(2015L)$population$population)
  )
  testthat::expect_true(all(country$method_population == "supplied"))
  manifest <- jsonlite::read_json(run$manifest)
  testthat::expect_equal(manifest$population$population_source, "pin")
  testthat::expect_equal(
    manifest$input_march_manifest$allocation$recorded,
    "not recorded in the balance manifest"
  )
})

testthat::test_that("the allocation record is copied and a grant refused", {
  manifest <- list(
    whep_commit = "x",
    input_overrides = list(WHEP_POLYCELL_SUBNATIONAL_PATH = "<unset>"),
    driver_report = list(
      "2010" = list(spatialize = list(granted_containers = list()))
    )
  )
  allocation <- whep:::.sjr_allocation(manifest)
  testthat::expect_equal(allocation$expected, "national (no subnational grant)")
  testthat::expect_named(
    allocation$recorded,
    c(
      "input_overrides.WHEP_POLYCELL_SUBNATIONAL_PATH",
      "driver_report.2010.spatialize.granted_containers"
    )
  )
  testthat::expect_equal(
    whep:::.sjr_allocation(list(whep_commit = "x"))$recorded,
    "not recorded in the balance manifest"
  )
  manifest$driver_report[["2010"]]$spatialize$granted_containers <- list(76L)
  testthat::expect_error(
    whep:::.sjr_allocation(manifest),
    class = "whep_sjr_subnational_granted"
  )
  balance <- tibble::tibble(year = 2010L, level_polity_code = c(NA, NA))
  testthat::expect_identical(
    whep:::.sjr_check_national(balance, 2010L),
    balance
  )
  balance$level_polity_code[[2]] <- 7L
  testthat::expect_error(
    whep:::.sjr_check_national(balance, 2010L),
    class = "whep_sjr_subnational_granted"
  )
})

testthat::test_that("the uniform-spread share is reported per year (#533)", {
  run <- .sjr_primary_run()
  diag <- dplyr::bind_rows(
    .sjr_read(run$out, "whep_sjos_n_diag", 2015L),
    .sjr_read(run$out, "whep_sjos_n_diag", 2016L)
  )
  input <- purrr::map_dbl(
    .sjr_test_years,
    \(y) sum(.sjr_gs_balance(y)$n_input_std_t)
  )
  testthat::expect_equal(
    diag$uniform_spread_status,
    c("recorded", "not_recorded")
  )
  testthat::expect_equal(diag$uniform_spread_n_t, c(12.5, NA))
  testthat::expect_equal(diag$uniform_spread_polity_crops, c(2L, NA))
  testthat::expect_equal(diag$grid_input_std_n_t, input)
  testthat::expect_equal(
    diag$uniform_spread_share_of_input,
    c(12.5 / input[[1]], NA)
  )
  testthat::expect_true(all(c("valid_input_fraction") %in% names(diag)))
})

testthat::test_that("uniform-spread warnings are summed, missing or unreadable", {
  messages <- c(
    .sjr_uniform_message(2, "10.25"),
    "some other warning",
    .sjr_uniform_message(1, "1.5e+3")
  )
  report <- list("2010" = .sjr_report(messages))
  out <- whep:::.sjr_uniform_spread(report, 2010L, 2000)
  testthat::expect_equal(out$uniform_spread_n_t, 1510.25)
  testthat::expect_equal(out$uniform_spread_polity_crops, 3L)
  testthat::expect_equal(out$uniform_spread_warnings, 2L)
  testthat::expect_equal(out$uniform_spread_share_of_input, 1510.25 / 2000)

  # A grid built as the balance run's second resolution.
  second <- list("2010" = .sjr_report(messages, resolution = "polity"))
  testthat::expect_equal(
    whep:::.sjr_uniform_spread(second, 2010L, 2000)$uniform_spread_n_t,
    1510.25
  )

  # No warning in a recorded year is zero; no report is not zero.
  none <- list("2010" = .sjr_report("some other warning"))
  testthat::expect_equal(
    whep:::.sjr_uniform_spread(none, 2010L, 2000)$uniform_spread_n_t,
    0
  )
  missing <- whep:::.sjr_uniform_spread(none, 2011L, 2000)
  testthat::expect_equal(missing$uniform_spread_status, "not_recorded")
  testthat::expect_true(is.na(missing$uniform_spread_n_t))

  garbled <- list(
    "2010" = .sjr_report(
      "! many polity-crop totals (lots t N) had no crop-pattern grid cells; x"
    )
  )
  testthat::expect_error(
    whep:::.sjr_uniform_spread(garbled, 2010L, 2000),
    class = "whep_sjr_uniform_unparsed"
  )
})

# A cell netting to zero surplus (country 1 +5 t, country 2 -5 t) against a
# -2 t allowance overshoots by 2 t under "keep", and the crop shares of a zero
# total are undefined, so the 2 t sit on a residual record.
.sjr_residual_out <- function() {
  balance <- tibble::tribble(
    ~lon, ~area_code, ~item_cbs_code, ~surplus_n_t,
    0.25, 1L,         2511L,          5,
    0.25, 2L,         2511L,          -5,
    0.75, 1L,         2511L,          4
  ) |>
    dplyr::mutate(lat = 0.25, year = 2015L, area_ha = 100) |>
    .sjr_balance()
  critical <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 0.25,
    value = c(-20, 100),
    source_area_ha = 100,
    image_region = 11L,
    critical_var = "critical_n_surplus",
    critical_land_use = "all",
    critical_threshold = "mi",
    critical_year = 2010L
  )
  .sjr_quiet(whep::build_sjos_nitrogen(
    data = c(
      list(balance = balance, critical = critical),
      .sjr_nourishment_data(2015L)
    ),
    boundary_land_use = "all",
    grassland_split = "none",
    negative_critical = "keep",
    country_table = TRUE,
    include = character()
  ))
}

testthat::test_that("reconciliation counts the unallocated residual", {
  out <- .sjr_residual_out()
  rec <- whep:::.sjr_reconcile(out, 2015L)
  testthat::expect_equal(rec$reconciliation_status, "pass")
  testthat::expect_equal(rec$reconciliation_unallocated_n_t, 2)
  testthat::expect_equal(rec$reconciliation_cell_exceedance_n_t, 2)
  testthat::expect_equal(rec$reconciliation_crop_exceedance_n_t, 0)
})

testthat::test_that("a reconciliation breach aborts", {
  out <- .sjr_residual_out()
  breaches <- list(
    # The residual dropped: the country sums no longer reach the cell total.
    \(x) {
      x$boundary_surplus$grid$unallocated_positive_overshoot_n_t <- 0
      x
    },
    \(x) {
      x$country_table$country$exceedance_n_t[[1]] <-
        x$country_table$country$exceedance_n_t[[1]] + 1
      x
    },
    \(x) {
      x$boundary_surplus$country$exceedance_n_t[[1]] <- 1e-3
      x
    },
    \(x) {
      x$sjos_class$exceedance_n_t[[1]] <- 5
      x
    },
    \(x) {
      x$boundary_surplus$grid$exceedance_n_t[[1]] <- NA_real_
      x
    }
  )
  for (breach in breaches) {
    testthat::expect_error(
      whep:::.sjr_reconcile(breach(out), 2015L),
      class = "whep_sjr_unreconciled"
    )
  }
})

testthat::test_that("a breach in the run writes nothing for that year", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  out <- file.path(dir, "out")
  real <- whep::build_sjos_nitrogen
  testthat::local_mocked_bindings(
    build_sjos_nitrogen = function(...) {
      x <- real(...)
      x$country_table$country$exceedance_n_t <-
        x$country_table$country$exceedance_n_t + 1
      x
    }
  )
  testthat::expect_error(
    .sjr_quiet(whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = out,
      context = .sjr_context()
    )),
    class = "whep_sjr_unreconciled"
  )
  parts <- Sys.glob(file.path(out, "whep_sjos_n*", "year=*", "part.parquet"))
  testthat::expect_length(parts, 0L)
  manifest <- jsonlite::read_json(file.path(
    out,
    "whep_sjos_n_run_manifest.json"
  ))
  testthat::expect_length(manifest$partitions, 0L)
  testthat::expect_true(.sjr_in_tempdir(
    list.files(dir, recursive = TRUE, full.names = TRUE),
    dir
  ))
})

testthat::test_that("another option set writes under its own arm", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  out <- file.path(dir, "out")
  options <- whep:::.sjr_options(
    land_use = "all",
    grassland_split = "none",
    nourishment_thresholds = "flat"
  )
  manifest <- .sjr_quiet(whep:::.sjr_run(
    years = 2015L,
    march_root = march,
    out_root = out,
    options = options,
    context = .sjr_context()
  ))
  arm <- file.path(out, "whep_sjos_n_arms", whep:::.sjr_arm_id(options))
  testthat::expect_equal(
    normalizePath(manifest, winslash = "/"),
    normalizePath(
      file.path(arm, "whep_sjos_n_run_manifest.json"),
      winslash = "/"
    )
  )
  testthat::expect_false(file.exists(file.path(out, "whep_sjos_n")))
  country <- .sjr_read(arm, "whep_sjos_n_country", 2015L)
  testthat::expect_equal(unique(country$grassland_split), "none")
  testthat::expect_equal(unique(country$nourishment_thresholds), "flat")
  testthat::expect_false(jsonlite::read_json(manifest)$arm$primary)
  testthat::expect_true(.sjr_in_tempdir(manifest, dir))
})

testthat::test_that("a finished year is skipped unless forced", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  out <- file.path(dir, "out")
  run <- \(...) {
    .sjr_quiet(whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = out,
      context = .sjr_context(),
      ...
    ))
  }
  run()
  calls <- 0L
  real <- whep::build_sjos_nitrogen
  testthat::local_mocked_bindings(
    build_sjos_nitrogen = function(...) {
      calls <<- calls + 1L
      real(...)
    }
  )
  run()
  testthat::expect_equal(calls, 0L)
  run(force = TRUE)
  testthat::expect_equal(calls, 1L)
})

testthat::test_that("outputs of another run or with no manifest are refused", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  out <- file.path(dir, "out")
  .sjr_quiet(whep:::.sjr_run(
    years = 2015L,
    march_root = march,
    out_root = out,
    context = .sjr_context()
  ))
  testthat::expect_error(
    whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = out,
      context = .sjr_context(list(sha = strrep("c", 40), clean = TRUE))
    ),
    class = "whep_sjr_incompatible_output"
  )
  unlink(file.path(out, "whep_sjos_n_run_manifest.json"))
  testthat::expect_error(
    whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = out,
      context = .sjr_context()
    ),
    class = "whep_sjr_unmanaged_output"
  )
})

testthat::test_that("a balance partition that is not the listed one aborts", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  path <- file.path(march, "whep_n_balance_grid", "year=2015", "part.parquet")
  arrow::write_parquet(.sjr_gs_balance(2015L)[-1, ], path)
  testthat::expect_error(
    whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = file.path(dir, "out"),
      context = .sjr_context()
    ),
    class = "whep_sjr_balance_mismatch"
  )
  testthat::expect_error(
    whep:::.sjr_read_march(file.path(dir, "nowhere")),
    class = "whep_sjr_no_march"
  )
})

testthat::test_that("pathway mode is refused and arguments are checked", {
  testthat::expect_error(
    whep:::.sjr_options(boundary_mode = "pathway"),
    class = "whep_sjr_pathway_unsupported"
  )
  testthat::expect_error(
    whep:::.sjr_parse_args("--boundary-mode=pathway"),
    class = "whep_sjr_pathway_unsupported"
  )
  testthat::expect_error(
    whep:::.sjr_parse_args("--land-uses=all"),
    class = "whep_sjr_bad_argument"
  )
  testthat::expect_error(
    whep:::.sjr_parse_args("--years=2010-2012"),
    class = "whep_sjr_bad_argument"
  )
  testthat::expect_error(whep:::.sjr_options(land_use = "igl"))
  testthat::expect_error(whep:::.sjr_options(beyond_share_cut = 1))
  args <- whep:::.sjr_parse_args(c(
    "--years=1961:1963,1970",
    "--land-use=ara",
    "--nourishment=flat",
    "--negative-critical=keep",
    "--beyond-share-cut=0.4",
    "--march-root=m",
    "--force"
  ))
  testthat::expect_equal(args$years, c(1961:1963, 1970L))
  testthat::expect_equal(args$options$land_use, "ara")
  testthat::expect_equal(args$options$nourishment_thresholds, "flat")
  testthat::expect_equal(args$options$negative_critical, "keep")
  testthat::expect_equal(args$options$beyond_share_cut, 0.4)
  testthat::expect_equal(args$march_root, "m")
  testthat::expect_true(args$force)
  default <- whep:::.sjr_parse_args(character())
  testthat::expect_identical(default$options, whep:::.sjr_options())
  testthat::expect_true(whep:::.sjr_is_primary(default$options))
  testthat::expect_false(whep:::.sjr_is_primary(args$options))
})

testthat::test_that("food comes from the tonnes rows of the balances only", {
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~food,
    2015L, 1L, 2511L, "tonnes", 10,
    2015L, 1L, 866L, "heads", 0,
    2014L, 1L, 2511L, "tonnes", 99
  )
  food <- whep:::.sjr_cbs_food(2015L, cbs)
  testthat::expect_equal(food$food_t, 10)
  testthat::expect_equal(food$item_cbs_code, 2511L)
  cbs$food[[2]] <- 3
  testthat::expect_error(
    whep:::.sjr_cbs_food(2015L, cbs),
    class = "whep_sjr_food_units"
  )
})

testthat::test_that("the shared driver fixtures were never mutated", {
  .sjr_primary_run()
  expect_memo_fixtures_untouched("sjr_")
})
