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

# Captured conditions as the balance run records them, `class` and `message`.
.sjr_conditions <- function(messages, class = "warning") {
  purrr::map2(
    messages,
    rep_len(class, length(messages)),
    \(m, cl) list(class = cl, message = m)
  )
}

# One stage record of the balance run's driver report: its counts and the
# conditions it captured. `warnings` defaults to the warning-class conditions
# captured, i.e. a complete capture.
.sjr_stage <- function(input, conditions = list(), warnings = NULL) {
  classes <- purrr::map_chr(conditions, "class")
  list(
    input = input,
    status = "ok",
    seconds = 1.5,
    rows = 10L,
    detail = NA_character_,
    warnings = warnings %||% sum(classes == "warning"),
    messages = sum(classes == "message"),
    conditions = conditions
  )
}

# The per-year record the balance run writes: `resolution`, `stages` and
# `second_resolution_conditions` (layout 8 of its manifest). A grid-primary
# run carries the warnings in its own stages; a polity-primary run carries the
# grid's conditions, uncounted, under `second_resolution_conditions$grid`.
.sjr_report <- function(messages, resolution = "grid", year = NULL) {
  conditions <- .sjr_conditions(messages)
  none <- stats::setNames(list(), character())
  record <- if (resolution == "grid") {
    list(
      resolution = "grid",
      stages = list(
        .sjr_stage("cell_polity"),
        .sjr_stage("n_inputs", conditions)
      ),
      second_resolution_conditions = none
    )
  } else {
    list(
      resolution = resolution,
      stages = list(.sjr_stage("n_inputs")),
      second_resolution_conditions = list(grid = conditions)
    )
  }
  c(if (!is.null(year)) list(year = as.integer(year)), record)
}

# A driver report as it reads back from the balance manifest's JSON.
.sjr_json <- function(x) {
  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(x, path, auto_unbox = TRUE, digits = NA, null = "null")
  jsonlite::read_json(path, simplifyVector = FALSE)
}

# The uniform-spread warning exactly as .nbd_capture_conditions() captures it
# from the balance (.n_warn_unmatched()) at console width `width`.
.sjr_captured_uniform <- function(crops, n_t, width = 80L) {
  withr::local_options(cli.width = width, width = width)
  unmatched <- tibble::tibble(
    n_t = rep(n_t / crops, crops),
    item_cbs_code = 2500L + seq_len(crops)
  )
  captured <- whep:::.nbd_capture_conditions(
    whep:::.n_warn_unmatched(unmatched)
  )
  captured$conditions$message
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
  testthat::expect_equal(manifest$schema_version, 2L)
  # Each balance partition read is recorded by the SHA-256 of its bytes.
  for (year in .sjr_test_years) {
    input <- manifest$input_balance[[as.character(year)]]
    path <- file.path(run$march, input$path)
    testthat::expect_equal(input$sha256, unname(tools::sha256sum(path)))
    testthat::expect_equal(input$rows, nrow(arrow::read_parquet(path)))
  }
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
  testthat::expect_equal(
    manifest$population$population_source,
    "pin_wpp_fbs_fallback"
  )
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
})

testthat::test_that("a subnational record is refused at any depth", {
  # The per-year allocation record of the balance run, national.
  national <- list(
    grid = "national",
    level = 0L,
    granted_containers = list(),
    subnational_calls = 0L,
    guarded = list("run_spatialize"),
    basis = "national"
  )
  manifest <- \(allocation) {
    .sjr_json(list(
      whep_commit = "x",
      driver_report = list("2015" = list(allocation = allocation))
    ))
  }
  passes <- list(
    national = national,
    granted_null = list(granted_containers = NULL),
    granted_false = list(subnational_granted = FALSE),
    granted_none = list(granted_containers = "None"),
    level_zero_text = list(level = "0")
  )
  for (allocation in passes) {
    testthat::expect_no_error(whep:::.sjr_allocation(manifest(allocation)))
  }
  testthat::expect_named(
    whep:::.sjr_allocation(manifest(national))$recorded,
    "driver_report.2015.allocation"
  )
  refused <- list(
    # A grant nested under a key that already matched.
    nested_grant = list(subnational = list(granted = list(392L))),
    grant_key = list(subnational_grant = list("JPN")),
    level_one = utils::modifyList(national, list(level = 1L)),
    level_deep = list(grid = list(depth = list(admin_level = 2L))),
    level_text = list(level = "subnational"),
    granted_true = list(subnational_granted = TRUE)
  )
  for (allocation in refused) {
    testthat::expect_error(
      whep:::.sjr_allocation(manifest(allocation)),
      class = "whep_sjr_subnational_granted"
    )
  }
  # A level outside any allocation record is not an allocation level.
  testthat::expect_no_error(whep:::.sjr_allocation(list(
    driver_report = list("2015" = list(log = list(level = 3L)))
  )))
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
  testthat::expect_equal(
    diag$uniform_spread_note,
    c(NA, "no driver report for the year")
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

testthat::test_that("uniform-spread warnings are summed when fully captured", {
  messages <- c(
    .sjr_uniform_message(2, "10.25"),
    "some other warning",
    .sjr_uniform_message(1, "1.5e+3")
  )
  report <- .sjr_json(list("2010" = .sjr_report(messages, year = 2010L)))
  out <- whep:::.sjr_uniform_spread(report, 2010L, 2000)
  testthat::expect_equal(out$uniform_spread_status, "recorded")
  testthat::expect_equal(out$uniform_spread_n_t, 1510.25)
  testthat::expect_equal(out$uniform_spread_polity_crops, 3L)
  testthat::expect_equal(out$uniform_spread_warnings, 2L)
  testthat::expect_equal(out$uniform_spread_share_of_input, 1510.25 / 2000)

  # The same warning in two stages is two spreads.
  twice <- .sjr_report(character())
  uniform <- .sjr_conditions(.sjr_uniform_message(3, "4"))
  twice$stages <- list(
    .sjr_stage("n_inputs", uniform),
    .sjr_stage("balance", uniform)
  )
  testthat::expect_equal(
    whep:::.sjr_uniform_spread(
      list("2010" = twice),
      2010L,
      100
    )$uniform_spread_n_t,
    8
  )

  # No uniform warning in a completely captured year is zero.
  none <- list("2010" = .sjr_report("some other warning"))
  zero <- whep:::.sjr_uniform_spread(none, 2010L, 2000)
  testthat::expect_equal(zero$uniform_spread_status, "recorded")
  testthat::expect_equal(zero$uniform_spread_n_t, 0)
})

testthat::test_that("the uniform spread is read at any console width", {
  # .n_warn_unmatched() wraps at the console width, so the break can fall
  # anywhere in the text; 250 crops and 1234567.891 t wrap differently at each
  # of these widths.
  for (width in c(30L, 40L, 60L, 80L, 200L)) {
    message <- .sjr_captured_uniform(250L, 1234567.891, width)
    report <- .sjr_json(list("2000" = .sjr_report(message)))
    out <- whep:::.sjr_uniform_spread(report, 2000L, 1e8)
    testthat::expect_equal(out$uniform_spread_n_t, 1234567.891)
    testthat::expect_equal(out$uniform_spread_polity_crops, 250L)
  }
  # Very small and very large masses, and one polity-crop.
  for (case in list(c(1, 12.5), c(3, 1e-5), c(1000, 98765432.1), c(12, 4e-4))) {
    message <- .sjr_captured_uniform(case[[1]], case[[2]])
    out <- whep:::.sjr_uniform_spread(
      list("2000" = .sjr_report(message)),
      2000L,
      1e8
    )
    testthat::expect_equal(out$uniform_spread_n_t, round(case[[2]], 3))
    testthat::expect_equal(out$uniform_spread_polity_crops, case[[1]])
  }
  # A message the balance run truncated at 2000 characters keeps its numbers.
  long <- paste0(.sjr_uniform_message(3, "3"), strrep(" padding", 400))
  truncated <- paste0(substr(long, 1L, 2000L), " [truncated]")
  testthat::expect_equal(
    whep:::.sjr_uniform_spread(
      list("2000" = .sjr_report(truncated)),
      2000L,
      100
    )$uniform_spread_n_t,
    3
  )
})

testthat::test_that("other crop-pattern conditions are not counted", {
  # Captured in the same 2010 balance run as the uniform-spread warning: the
  # cropland support's and the pin fetch's mention the crop pattern but are
  # not uniform spreads, and the soil carbon's is carbon, not nitrogen.
  others <- c(
    paste(
      "! 8251 cell-years (12118155.7 ha) have cropland but no crop-pattern\n ",
      "composition; they carry no cropland support."
    ),
    "ℹ Fetching files for spatialize-crop-patterns...",
    paste(
      "! 3 polity-crop carbon components (12.5 Mg C) had no crop-pattern",
      "cells; reallocating uniformly across the polity's cropland cells."
    )
  )
  report <- .sjr_report(c(others, .sjr_uniform_message(650, "9331451.681")))
  report$stages[[2]]$conditions[[2]]$class <- "message"
  report$stages[[2]]$warnings <- 3L
  out <- whep:::.sjr_uniform_spread(list("2010" = report), 2010L, 1e8)
  testthat::expect_equal(out$uniform_spread_status, "recorded")
  testthat::expect_equal(out$uniform_spread_n_t, 9331451.681)
  testthat::expect_equal(out$uniform_spread_warnings, 1L)
})

testthat::test_that("an incomplete capture is not recorded, never zero", {
  expect_not_recorded <- \(report, note) {
    out <- whep:::.sjr_uniform_spread(report, 2010L, 2000)
    testthat::expect_equal(out$uniform_spread_status, "not_recorded")
    testthat::expect_match(out$uniform_spread_note, note)
    testthat::expect_true(is.na(out$uniform_spread_n_t))
    testthat::expect_true(is.na(out$uniform_spread_share_of_input))
  }
  # No report for the year.
  expect_not_recorded(list("2011" = .sjr_report(character())), "no driver")
  expect_not_recorded(list(), "no driver")
  # Warnings counted but not captured: zero captured is not zero spread.
  counted <- .sjr_report("some other warning")
  counted$stages[[2]]$warnings <- 5L
  expect_not_recorded(list("2010" = counted), "not captured in stage n_inputs")
  # ... and a partial capture is not the total either.
  partial <- .sjr_report(.sjr_uniform_message(2, "12.5"))
  partial$stages[[2]]$warnings <- 2L
  expect_not_recorded(list("2010" = partial), "not captured")
  # A stage without a count.
  uncounted <- .sjr_report(character())
  uncounted$stages[[1]]$warnings <- NULL
  expect_not_recorded(list("2010" = uncounted), "cell_polity")
  # No stages.
  empty <- .sjr_report(character())
  empty$stages <- list()
  expect_not_recorded(.sjr_json(list("2010" = empty)), "no stages")
  # A grid built as the balance run's second resolution: its conditions carry
  # no count, so even a readable spread is not a complete total.
  second <- .sjr_json(list(
    "2010" = .sjr_report(.sjr_uniform_message(2, "12.5"), "polity")
  ))
  expect_not_recorded(second, "second resolution")
  no_grid <- .sjr_report(character(), "polity")
  no_grid$second_resolution_conditions <- stats::setNames(list(), character())
  expect_not_recorded(list("2010" = no_grid), "no grid conditions")
})

testthat::test_that("an unreadable uniform-spread condition aborts", {
  abort <- \(report) {
    testthat::expect_error(
      whep:::.sjr_uniform_spread(list("2010" = report), 2010L, 2000),
      class = "whep_sjr_uniform_unparsed"
    )
  }
  abort(.sjr_report(
    "! many polity-crop totals (lots t N) had no crop-pattern grid cells; x"
  ))
  abort(.sjr_report("! 2 polity-crop totals (12.5 t N) were spread somehow."))
  # Even when the capture is incomplete.
  incomplete <- .sjr_report("! 2 polity-crop totals were dropped")
  incomplete$stages[[2]]$warnings <- 4L
  abort(incomplete)
  # A uniform spread reported other than as a warning.
  message_class <- .sjr_report(.sjr_uniform_message(2, "12.5"))
  message_class$stages[[2]]$conditions[[1]]$class <- "message"
  message_class$stages[[2]]$warnings <- 0L
  abort(message_class)
})

testthat::test_that("a driver report in another layout aborts", {
  abort <- \(report) {
    testthat::expect_error(
      whep:::.sjr_uniform_spread(report, 2010L, 2000),
      class = "whep_sjr_report_layout"
    )
  }
  stages <- .sjr_report(.sjr_uniform_message(2, "12.5"))$stages
  # A bare list of stage records, without the per-year wrapper.
  abort(.sjr_json(list("2010" = stages)))
  # The wrapper without its stages, or without the second-resolution record.
  wrapper <- .sjr_report(.sjr_uniform_message(2, "12.5"))
  abort(list("2010" = wrapper[c("resolution", "second_resolution_conditions")]))
  abort(list("2010" = wrapper[c("resolution", "stages")]))
  # Stages keyed by name rather than listed, or a stage that is not a record.
  keyed <- wrapper
  keyed$stages <- list(n_inputs = wrapper$stages[[2]])
  abort(list("2010" = keyed))
  scalar <- wrapper
  scalar$stages <- list("n_inputs")
  abort(list("2010" = scalar))
  # A record that says it is another year.
  abort(list("2010" = .sjr_report(character(), year = 2011L)))
  # A driver report that is not keyed by year at all.
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  manifest_path <- file.path(march, "whep_n_balance_run_manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  manifest$driver_report <- unname(manifest$driver_report)
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  testthat::expect_error(
    whep:::.sjr_read_march(march),
    class = "whep_sjr_report_layout"
  )
})

testthat::test_that("the real layout-8 driver report reads as recorded", {
  # The stage counts of the 2010 smoke run of the balance (layout 8): every
  # stage's `warnings` equals its captured warning-class conditions, messages
  # are captured too, and the uniform spread sits in n_inputs.
  counts <- tibble::tribble(
    ~input, ~warnings, ~messages,
    "primary_prod", 12L, 98L,
    "ag_land_support", 1L, 0L,
    "npp_n_input", 0L, 2L,
    "carbon_balance", 300L, 904L,
    "n_inputs", 3L, 2L
  )
  stages <- purrr::pmap(counts, \(input, warnings, messages) {
    conditions <- c(
      .sjr_conditions(rep("a warning", warnings)),
      .sjr_conditions(rep("a message", messages), "message")
    )
    if (input == "n_inputs") {
      conditions[[1]]$message <- .sjr_uniform_message(650, "9331451.681")
    }
    .sjr_stage(input, conditions)
  })
  report <- list(
    year = 2010L,
    resolution = "grid",
    human_n_population_basis = "total",
    stages = stages,
    second_resolution_conditions = list(
      polity = .sjr_conditions(.sjr_uniform_message(650, "9331451.681"))
    )
  )
  out <- whep:::.sjr_uniform_spread(
    .sjr_json(list("2010" = report)),
    2010L,
    1e8
  )
  testthat::expect_equal(out$uniform_spread_status, "recorded")
  testthat::expect_equal(out$uniform_spread_n_t, 9331451.681)
  testthat::expect_equal(out$uniform_spread_polity_crops, 650L)
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

testthat::test_that("arm ids carry the cut at fixed precision", {
  arm <- \(cut) whep:::.sjr_arm_id(whep:::.sjr_options(beyond_share_cut = cut))
  testthat::expect_match(arm(0.3), "_cut-0\\.300000$")
  testthat::expect_match(arm(0), "_cut-0\\.000000$")
  testthat::expect_match(arm(0.123456), "_cut-0\\.123456$")
  # Distinct cuts that one label would share are refused, so no two accepted
  # cuts can write to the same arm.
  for (cut in c(0.30000000001, 0.1234567, 1 / 3)) {
    testthat::expect_error(
      whep:::.sjr_options(beyond_share_cut = cut),
      class = "whep_sjr_arm_collision"
    )
  }
  testthat::expect_error(
    whep:::.sjr_parse_args("--beyond-share-cut=0.30000000001"),
    class = "whep_sjr_arm_collision"
  )
  cuts <- c(0, 0.05, 0.1, 0.25, 0.3, 0.333333, 0.4, 0.6, 0.75, 0.999999)
  testthat::expect_false(anyDuplicated(purrr::map_chr(cuts, arm)) > 0L)
})

# A WHEP state as .sjr_whep_state() returns it for an uncommitted tree.
.sjr_dirty <- function() {
  list(
    sha = strrep("a", 40),
    clean = FALSE,
    dirty_paths = "R/sjos_n_run.R",
    diff_sha256 = strrep("d", 64)
  )
}

testthat::test_that("a dirty tree is refused unless asked for", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  run <- \(out, whep, allow_dirty = FALSE) {
    .sjr_quiet(whep:::.sjr_run(
      years = 2015L,
      march_root = march,
      out_root = out,
      allow_dirty = allow_dirty,
      context = .sjr_context(whep)
    ))
  }
  testthat::expect_error(
    run(file.path(dir, "dev"), .sjr_dirty()),
    class = "whep_sjr_dirty_tree"
  )
  # Asked for, it still never writes into the balance root ...
  testthat::expect_error(
    run(march, .sjr_dirty(), allow_dirty = TRUE),
    class = "whep_sjr_dirty_root"
  )
  # ... nor beside a clean run's outputs, at the root or in an arm under it.
  clean <- file.path(dir, "clean")
  run(clean, .sjr_sha())
  testthat::expect_error(
    run(clean, .sjr_dirty(), allow_dirty = TRUE),
    class = "whep_sjr_dirty_root"
  )
  .sjr_quiet(whep:::.sjr_run(
    years = 2015L,
    march_root = march,
    out_root = file.path(dir, "arm_only"),
    options = whep:::.sjr_options(negative_critical = "keep"),
    context = .sjr_context()
  ))
  testthat::expect_error(
    run(file.path(dir, "arm_only"), .sjr_dirty(), allow_dirty = TRUE),
    class = "whep_sjr_dirty_root"
  )
  testthat::expect_false(dir.exists(file.path(dir, "dev")))
  testthat::expect_false(file.exists(file.path(
    march,
    "whep_sjos_n_run_manifest.json"
  )))

  # Into a root of its own, the run records the tree's state per year.
  dev <- file.path(dir, "dev")
  manifest <- run(dev, .sjr_dirty(), allow_dirty = TRUE)
  testthat::expect_true(.sjr_in_tempdir(manifest, dir))
  record <- jsonlite::read_json(manifest)
  testthat::expect_false(record$whep_tree_clean)
  testthat::expect_false(record$diagnostics[["2015"]]$whep_tree_clean)
  testthat::expect_equal(
    record$diagnostics[["2015"]]$whep_dirty_diff_sha256,
    strrep("d", 64)
  )
  diag <- .sjr_read(dev, "whep_sjos_n_diag", 2015L)
  testthat::expect_false(diag$whep_tree_clean)
  # A clean run refuses the development root in turn.
  testthat::expect_error(
    run(dev, .sjr_sha()),
    class = "whep_sjr_dirty_root"
  )
  clean_diag <- .sjr_read(clean, "whep_sjos_n_diag", 2015L)
  testthat::expect_true(clean_diag$whep_tree_clean)
  testthat::expect_true(is.na(clean_diag$whep_dirty_diff_sha256))
  testthat::expect_true(whep:::.sjr_parse_args("--allow-dirty")$allow_dirty)
  testthat::expect_false(whep:::.sjr_parse_args(character())$allow_dirty)
})

testthat::test_that("the tree state counts tracked code changes only", {
  testthat::skip_if(Sys.which("git") == "", "git is not available")
  repo <- withr::local_tempdir()
  git <- \(...) {
    system2("git", c("-C", shQuote(repo), ...), stdout = TRUE, stderr = TRUE)
  }
  git("init", "-q")
  git("config", "user.email", "t@example.org")
  git("config", "user.name", "t")
  dir.create(file.path(repo, "R"))
  dir.create(file.path(repo, "inst", "scripts"), recursive = TRUE)
  writeLines("x <- 1", file.path(repo, "R", "a.R"))
  writeLines("y <- 1", file.path(repo, "inst", "scripts", "s.R"))
  writeLines("notes", file.path(repo, "NEWS.md"))
  git("add", "-A")
  git("commit", "-q", "-m", "init")
  state <- whep:::.sjr_whep_state(repo)
  testthat::expect_match(state$sha, "^[0-9a-f]{40}$")
  testthat::expect_true(state$clean)
  testthat::expect_true(is.na(state$diff_sha256))
  # Outside R/ and inst/scripts/, or untracked: still the commit's code.
  writeLines("changed", file.path(repo, "NEWS.md"))
  writeLines("z <- 1", file.path(repo, "R", "untracked.R"))
  testthat::expect_true(whep:::.sjr_whep_state(repo)$clean)
  # A tracked R file changed.
  writeLines("x <- 2", file.path(repo, "R", "a.R"))
  dirty <- whep:::.sjr_whep_state(repo)
  testthat::expect_false(dirty$clean)
  testthat::expect_equal(dirty$dirty_paths, "R/a.R")
  testthat::expect_match(dirty$diff_sha256, "^[0-9a-f]{64}$")
  # A tracked script changed.
  git("checkout", "--", "R/a.R")
  writeLines("y <- 2", file.path(repo, "inst", "scripts", "s.R"))
  testthat::expect_equal(
    whep:::.sjr_whep_state(repo)$dirty_paths,
    "inst/scripts/s.R"
  )
  testthat::expect_error(
    whep:::.sjr_whep_state(file.path(repo, "nowhere")),
    class = "whep_sjr_no_sha"
  )
})

testthat::test_that("a balance partition changed since it was read is refused", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"))
  out <- file.path(dir, "out")
  run <- \(years) {
    .sjr_quiet(whep:::.sjr_run(
      years = years,
      march_root = march,
      out_root = out,
      context = .sjr_context()
    ))
  }
  run(2015L)
  gc()
  # Same rows, so the balance manifest still vouches for it, but other values.
  path <- file.path(march, "whep_n_balance_grid", "year=2015", "part.parquet")
  changed <- .sjr_gs_balance(2015L) |>
    dplyr::mutate(prod_n_t = .data$prod_n_t * 2)
  arrow::write_parquet(changed, path)
  # Adding another year to the root is refused, not only re-running 2015.
  testthat::expect_error(run(2016L), class = "whep_sjr_incompatible_output")
  # A recorded partition that is gone is refused too.
  unlink(path)
  testthat::expect_error(
    whep:::.sjr_check_inputs(
      list("2015" = list(path = "whep_n_balance_grid/year=2015/part.parquet")),
      march,
      "manifest"
    ),
    class = "whep_sjr_incompatible_output"
  )
  testthat::expect_false(dir.exists(file.path(out, "whep_sjos_n", "year=2016")))
})

testthat::test_that("the band table carries the chain's band and headcounts", {
  run <- .sjr_primary_run()
  year <- 2016L
  band <- .sjr_read(run$out, "whep_sjos_n_band", year)
  testthat::expect_named(
    band,
    c(
      "year",
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "floor_g_cap_day",
      "ceiling_g_cap_day",
      "prevalence_protein_deficit",
      "prevalence_protein_excess",
      "people_under",
      "people_over",
      "population",
      "method_quality",
      "method_shortfall",
      "method_ceiling",
      "method_population",
      "nourishment_thresholds"
    )
  )
  # The values are build_nourishment_band()'s as build_sjos_nitrogen()
  # composed it, not recomputed.
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
    include = "band"
  ))$nourishment_band
  shared <- intersect(names(band), names(direct))
  testthat::expect_equal(
    as.data.frame(band[shared]),
    as.data.frame(direct[shared]),
    ignore_attr = TRUE
  )
  testthat::expect_equal(nrow(band), 2L)
  testthat::expect_equal(unique(band$nourishment_thresholds), "composed")
  # Population is the country table's, and the headcounts are its shares.
  country <- .sjr_read(run$out, "whep_sjos_n_country", year)
  joined <- dplyr::inner_join(
    dplyr::select(band, "area_code", "population"),
    dplyr::select(country, "area_code", expected = "population"),
    by = "area_code"
  )
  testthat::expect_equal(nrow(joined), nrow(country))
  testthat::expect_identical(joined$population, joined$expected)
  testthat::expect_equal(
    band$people_under,
    band$prevalence_protein_deficit * band$population
  )
  testthat::expect_equal(
    band$people_over,
    band$prevalence_protein_excess * band$population
  )
  testthat::expect_true(all(
    band$people_under + band$people_over <= band$population
  ))
  testthat::expect_true(all(band$people_under > 0 & band$people_over > 0))
  diag <- .sjr_read(run$out, "whep_sjos_n_diag", year)
  testthat::expect_equal(diag$band_reconciliation_status, "pass")
  testthat::expect_equal(diag$band_rows_without_headcount, 0L)
  testthat::expect_lt(diag$band_max_headcount_excess, 0)
  manifest <- jsonlite::read_json(run$manifest)
  testthat::expect_true(
    "whep_sjos_n_band" %in% purrr::map_chr(manifest$products, "dir")
  )
})

testthat::test_that("a band that does not reconcile aborts", {
  band <- tibble::tibble(
    year = 2015L,
    area_code = c(1L, 2L),
    prevalence_protein_deficit = c(0.1, 0.2),
    prevalence_protein_excess = c(0.3, 0.4),
    people_under = c(10, 40),
    people_over = c(30, 80),
    population = c(100, 200)
  )
  country <- dplyr::select(band, "year", "area_code", "population")
  check <- whep:::.sjr_reconcile_band(band, country, 2015L)
  testthat::expect_equal(check$band_reconciliation_status, "pass")
  testthat::expect_equal(check$band_max_headcount_excess, -60)
  breaches <- list(
    # The country table divides by another population.
    list(band = band, country = dplyr::mutate(country, population = 101)),
    # A headcount that is not its prevalence times the population.
    list(
      band = dplyr::mutate(band, people_under = c(11, 40)),
      country = country
    ),
    list(
      band = dplyr::mutate(band, people_over = c(30, 81)),
      country = country
    ),
    # Both tails together above the population.
    list(
      band = dplyr::mutate(
        band,
        prevalence_protein_excess = c(0.95, 0.4),
        people_over = c(95, 80)
      ),
      country = country
    ),
    # A headcount next to a missing population.
    list(
      band = dplyr::mutate(band, population = c(NA, 200)),
      country = dplyr::mutate(country, population = c(NA, 200))
    )
  )
  for (case in breaches) {
    testthat::expect_error(
      whep:::.sjr_reconcile_band(case$band, case$country, 2015L),
      class = "whep_sjr_unreconciled"
    )
  }
  # A band row without headcounts is counted, not failed.
  gap <- dplyr::mutate(
    band,
    people_under = c(NA, 40),
    prevalence_protein_deficit = c(NA, 0.2)
  )
  testthat::expect_equal(
    whep:::.sjr_reconcile_band(gap, country, 2015L)$band_rows_without_headcount,
    1L
  )
  testthat::expect_equal(
    whep:::.sjr_reconcile_band(NULL, country, 2015L)$band_reconciliation_status,
    "not_applicable"
  )
})

testthat::test_that("a band breach in the run writes nothing for that year", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  out <- file.path(dir, "out")
  real <- whep::build_sjos_nitrogen
  testthat::local_mocked_bindings(
    build_sjos_nitrogen = function(...) {
      x <- real(...)
      x$nourishment_band$people_over <- x$nourishment_band$people_over * 2
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
})

testthat::test_that("the flat pair writes no band table", {
  dir <- withr::local_tempdir()
  march <- .sjr_gs_march(file.path(dir, "march"), years = 2015L)
  options <- whep:::.sjr_options(nourishment_thresholds = "flat")
  manifest <- .sjr_quiet(whep:::.sjr_run(
    years = 2015L,
    march_root = march,
    out_root = file.path(dir, "out"),
    options = options,
    context = .sjr_context()
  ))
  arm <- dirname(manifest)
  testthat::expect_true(.sjr_in_tempdir(manifest, dir))
  testthat::expect_false(dir.exists(file.path(arm, "whep_sjos_n_band")))
  record <- jsonlite::read_json(manifest)
  testthat::expect_false(
    "whep_sjos_n_band" %in% purrr::map_chr(record$products, "dir")
  )
  testthat::expect_length(record$partitions, 6L)
  testthat::expect_equal(
    .sjr_read(arm, "whep_sjos_n_diag", 2015L)$band_reconciliation_status,
    "not_applicable"
  )
  # Resuming the flat arm skips the finished year without a band table.
  calls <- 0L
  real <- whep::build_sjos_nitrogen
  testthat::local_mocked_bindings(
    build_sjos_nitrogen = function(...) {
      calls <<- calls + 1L
      real(...)
    }
  )
  .sjr_quiet(whep:::.sjr_run(
    years = 2015L,
    march_root = march,
    out_root = file.path(dir, "out"),
    options = options,
    context = .sjr_context()
  ))
  testthat::expect_equal(calls, 0L)
})

# The balance root as the balance run itself writes it (R/nbd_march.R), not as
# a fixture hand-builds its JSON (whep#1411): each year's stages are captured
# by .nbd_capture_conditions() and recorded by .nbd_stage_row(), so the
# warning counts the driver checks are the handler's own.
.sjr_written_march <- function(dir, stages_by_year) {
  identity <- .nbd_march_test_identity()
  purrr::iwalk(stages_by_year, \(stages, year) {
    whep:::.nbd_write_march_year(
      dir,
      as.integer(year),
      .sjr_gs_balance(as.integer(year)),
      .nbd_march_test_report(stages),
      identity
    )
  })
  dir
}

testthat::test_that("a balance root the balance run wrote reads back (#1411)", {
  dir <- withr::local_tempdir()
  march <- .sjr_written_march(
    file.path(dir, "march"),
    list(
      "2015" = list(
        cell_polity = rlang::quo(1),
        n_inputs = rlang::quo({
          message("Reallocated 3 rows (120 t N).")
          .nbd_warn_uniform(2, 12.5)
          .nbd_warn_uniform(3, 7.5)
          2
        })
      ),
      # A lossy capture: one warning counted but never recorded.
      "2016" = list(
        n_inputs = rlang::quo({
          .nbd_warn_uniform(2, 12.5)
          .nbd_unreadable_warning()
          2
        })
      )
    )
  )
  read <- whep:::.sjr_read_march(march)
  testthat::expect_equal(read$grid$year, .sjr_test_years)
  testthat::expect_equal(
    read$grid$rows,
    purrr::map_int(.sjr_test_years, \(y) nrow(.sjr_gs_balance(y)))
  )
  testthat::expect_equal(read$whep_commit, strrep("c", 40))
  out <- file.path(dir, "out")
  .sjr_quiet(whep:::.sjr_run(
    years = .sjr_test_years,
    march_root = march,
    out_root = out,
    context = .sjr_context()
  ))
  diag <- dplyr::bind_rows(
    .sjr_read(out, "whep_sjos_n_diag", 2015L),
    .sjr_read(out, "whep_sjos_n_diag", 2016L)
  )
  testthat::expect_equal(
    diag$uniform_spread_status,
    c("recorded", "not_recorded")
  )
  testthat::expect_equal(diag$uniform_spread_n_t, c(20, NA))
  testthat::expect_equal(diag$uniform_spread_polity_crops, c(5L, NA))
  testthat::expect_match(
    diag$uniform_spread_note[[2]],
    "not captured in stage n_inputs"
  )
})

testthat::test_that("the shared driver fixtures were never mutated", {
  .sjr_primary_run()
  expect_memo_fixtures_untouched("sjr_")
})
