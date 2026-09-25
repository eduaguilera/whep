# build_carbon_balance() builds its span one block of years at a time
# (whep#1287). The contract under test: whatever `block_years` is, the result
# -- every column of every row, in order -- and the warnings and messages are
# identical to building the span in one pass. Offline throughout; the fixture
# is in helper_carbon_blocks.R.

# One build, recording its conditions. Progress output is off under testthat.
.cbb_build <- function(block_years, ...) {
  .cbb_record(whep::build_carbon_balance(..., block_years = block_years))
}

testthat::test_that("the fixture reaches every term a block boundary can break", {
  one <- .cbb_build(Inf, data = .cbb_data())
  out <- one$value
  # A crop group and a natural row vanish at a block boundary (2009 is the
  # first year of the second block at block_years = 2) and are carried at
  # zero area; the group reappears in 2011.
  zero <- out[out$area_ha == 0, ]
  testthat::expect_setequal(
    paste(zero$land_use, zero$year),
    c(
      "cropland_irrigated_herbaceous 2009",
      "cropland_irrigated_herbaceous 2010",
      "cropland_irrigated_herbaceous 2013",
      "natural 2009",
      "natural 2010"
    )
  )
  testthat::expect_true(any(
    out$land_use == "cropland_irrigated_herbaceous" &
      out$year == 2011L &
      out$area_ha > 0
  ))
  # Cells come in string-key order, which is not numeric order.
  testthat::expect_identical(
    unique(out$lon),
    c(-0.25, -10.25, 100.25, 30.25, 5.25)
  )
  testthat::expect_true(any(is.na(out$input_cn)))
  testthat::expect_true(any(!is.na(out$input_cn)))
  # Every report the class table and the tail raise fires.
  said <- purrr::map_chr(one$conditions, 2L)
  for (pattern in c(
    "grouped carbon-input rows? in",
    "march on a\\s+zero carbon input",
    "no\\s+modelled land at all",
    "Dropped 2 cell-polity compartments",
    "did not exist in that row's\\s+year"
  )) {
    testthat::expect_true(any(grepl(pattern, said)), info = pattern)
  }
})

testthat::test_that("a blocked build is identical to one pass", {
  one <- .cbb_build(Inf, data = .cbb_data())
  for (size in c(1, 2, 3, 4, 7, 100)) {
    testthat::expect_identical(
      .cbb_build(size, data = .cbb_data()),
      one,
      info = paste("block_years =", size)
    )
  }
})

# The build as it ran before it was blocked: every private step once, over
# the whole span, in the order `build_carbon_balance()` called them. The
# block tests below compare blocked builds with `block_years = Inf`, which
# shares the blocked assembly; this pins that assembly to the unblocked
# pipeline itself, so a wrong interleaving cannot hide on both sides.
.cbb_unblocked <- function(
  data,
  resolution = "grid",
  init = "own_equilibrium",
  polity_validity = "keep"
) {
  groups <- whep:::.ci_group_config(list())
  d <- whep:::.cb_resolve_inputs(
    data,
    NULL,
    groups,
    list(basis = "renormalised", grazing = "whep")
  )
  d$class_water <- "cell"
  classes <- whep:::.cb_class_table(d, "hsoc")
  coverage <- whep:::.cb_take_land_coverage(classes)
  classes <- whep:::.cb_attach_equilibrium(classes, "hsoc")
  opening <- whep:::.cb_initialise(classes, "hsoc", d, init)
  whep:::.cb_march(classes, opening) |>
    whep:::.cb_attach_input_cn(classes) |>
    whep:::.cb_derive_son("justes_2009") |>
    dplyr::mutate(
      method_soc = "hsoc",
      method_soc_init = init,
      method_class_water = "cell",
      method_area_basis = "renormalised",
      method_grazing = "whep",
      method_crop_groups = if (is.null(groups$method)) "none" else groups$method
    ) |>
    whep:::.cb_finalise(resolution, coverage) |>
    whep:::.resolve_polity_validity(polity_validity)
}

testthat::test_that("a blocked build is the unblocked pipeline", {
  quiet <- \(x) suppressWarnings(suppressMessages(x))
  cases <- list(
    list(
      resolution = "grid",
      init = "own_equilibrium",
      polity_validity = "keep"
    ),
    list(resolution = "grid", init = "cell_average", polity_validity = "flag"),
    list(
      resolution = "polity",
      init = "own_equilibrium",
      polity_validity = "drop"
    )
  )
  for (case in cases) {
    reference <- quiet(do.call(.cbb_unblocked, c(list(.cbb_data()), case)))
    for (size in c(Inf, 2)) {
      testthat::expect_identical(
        quiet(do.call(
          whep::build_carbon_balance,
          c(list(data = .cbb_data(), block_years = size), case)
        )),
        reference,
        info = paste(case$resolution, case$init, "at block_years =", size)
      )
    }
  }
})

testthat::test_that("blocking is identical across openings, models and grain", {
  precomputed <- .cbb_data()
  precomputed$climate <- precomputed$climate |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code, .data$year) |>
    dplyr::mutate(
      climate_modifier = 0.6 + 0.05 * (.data$year - 2007) + .data$lat / 400
    )
  precomputed$clay <- dplyr::distinct(
    .cbb_climate(),
    .data$lon,
    .data$lat,
    .data$clay_pct
  )
  cases <- list(
    cell_average_flag = list(
      data = .cbb_data(),
      init = "cell_average",
      polity_validity = "flag"
    ),
    polity = list(data = .cbb_data(), resolution = "polity"),
    polity_cell_average_drop = list(
      data = .cbb_data(),
      resolution = "polity",
      init = "cell_average",
      polity_validity = "drop"
    ),
    # The two cover layers and the equilibrium-climate normal, which has no
    # year and is read only at the opening, with irrigation by regime.
    covers_normal_regime = list(
      data = .cbb_data(extras = TRUE),
      class_water = "regime"
    ),
    # A model whose modifier takes the per-group path, not the vectorised one.
    icbm_polity = list(
      data = .cbb_data(),
      model = "icbm",
      resolution = "polity"
    ),
    rothc_drop = list(
      data = .cbb_data(),
      model = "rothc",
      polity_validity = "drop"
    ),
    precomputed_modifier = list(data = precomputed)
  )
  for (name in names(cases)) {
    one <- do.call(.cbb_build, c(list(block_years = Inf), cases[[name]]))
    for (size in c(2, 3)) {
      testthat::expect_identical(
        do.call(.cbb_build, c(list(block_years = size), cases[[name]])),
        one,
        info = paste(name, "at block_years =", size)
      )
    }
  }
})

testthat::test_that("each block reads its own years; the carbon inputs span", {
  land_use <- .cbb_land_use()
  climate <- .cbb_climate()
  c_inputs <- .cbb_c_inputs()
  calls <- list()
  testthat::local_mocked_bindings(
    .cb_read_land_use = function(years = NULL) {
      calls$land_use <<- c(calls$land_use, list(years))
      land_use[land_use$year %in% years, ]
    },
    .cb_read_climate = function(years = NULL) {
      calls$climate <<- c(calls$climate, list(years))
      climate[climate$year %in% years, ]
    },
    .cb_read_c_inputs = function(
      data = list(),
      years = NULL,
      crop_groups = list(),
      methods = list()
    ) {
      calls$c_inputs <<- c(calls$c_inputs, list(years))
      c_inputs[c_inputs$year %in% years, ]
    },
    .package = "whep"
  )
  span <- 2007:2013
  one <- .cbb_build(Inf, years = span, method_grazing = "lpjml")
  testthat::expect_identical(calls$climate, list(span))
  testthat::expect_identical(calls$land_use, list(span))
  calls <- list()
  blocked <- .cbb_build(3, years = span, method_grazing = "lpjml")
  testthat::expect_identical(blocked, one)
  blocks <- list(2007:2009, 2010:2012, 2013L)
  testthat::expect_identical(calls$climate, blocks)
  testthat::expect_identical(calls$land_use, blocks)
  # Read once over the whole span: a year-scoped read changes its rows
  # (whep#833, whep#834).
  testthat::expect_identical(calls$c_inputs, list(span))
})

# Multi-year monthly inputs for `.socd_build()`, varying by year so a
# misaligned year shows. Days-per-month, and so the PET total, differ in the
# leap years 2000 and 2004.
.cbb_socd_inputs <- function(years = 2000:2004) {
  cells <- tibble::tribble(
    ~lon, ~lat,
    9.25, 47.75,
    -3.25, 40.25,
    20.25, -10.25
  )
  months <- tidyr::expand_grid(cells, year = years, month = 1:12)
  shape <- 1 + 0.2 * sin(months$lon + 0.7 * (months$year - 2000) + months$month)
  swc <- tidyr::expand_grid(cells, year = years, month = 1:12, layer = 1:2) |>
    dplyr::mutate(
      value = dplyr::if_else(.data$layer == 1L, 0.45, 0.40) -
        0.01 * (.data$year - 2000) +
        0.005 * .data$month
    )
  list(
    temp = dplyr::mutate(
      months,
      value = 5 + 10 * (.data$month / 12) + 0.5 * (.data$year - 2000)
    ),
    pet = dplyr::mutate(months, value = (1 + 2 * .data$month / 12) * shape),
    prec = dplyr::mutate(months, value = 60 * shape),
    irrig = dplyr::mutate(months, value = 5 + .data$year %% 2),
    swc = swc,
    clay = dplyr::mutate(cells, clay_pct = c(22, 30, 15)),
    cell_polity = dplyr::mutate(cells, area_code = c(11L, 203L, 21L)),
    soil_hydraulic = dplyr::mutate(
      cells,
      t_field = 0.29,
      t_wilt = 0.14,
      porosity = 0.43
    )
  )
}

testthat::test_that("the climate drivers read per block are the span read's rows", {
  # `.cb_read_climate()` is one `.socd_build()` call. Reading a block of years
  # must return exactly the block's rows of a whole-span read, in the same
  # order: that is what lets the balance read its climate per block.
  data <- .cbb_socd_inputs()
  read <- function(years) {
    whep:::.socd_build(NULL, years, "keep", "abort", data)
  }
  span <- read(2000:2004)
  testthat::expect_setequal(unique(span$year), 2000:2004)
  testthat::expect_identical(nrow(span), 3L * 5L * 12L)
  for (size in 1:3) {
    starts <- seq(1L, 5L, by = size)
    for (start in starts) {
      block <- (2000:2004)[start:min(start + size - 1L, 5L)]
      testthat::expect_identical(
        read(block),
        vctrs::vec_slice(span, span$year %in% block),
        info = paste(range(block), collapse = "-")
      )
    }
  }
})

# The fixture with `drop` removed from the land use, so a cell's class rows
# go missing for those years.
.cbb_without <- function(drop) {
  d <- .cbb_data()
  d$land_use <- d$land_use[!drop(d$land_use), ]
  d
}

.cbb_expect_lattice_refused <- function(d) {
  for (size in c(Inf, 3, 2, 1)) {
    testthat::expect_error(
      suppressWarnings(suppressMessages(
        whep::build_carbon_balance(data = d, block_years = size)
      )),
      class = "whep_incomplete_lattice",
      info = paste("block_years =", size)
    )
  }
}

testthat::test_that("a cell or year missing across a block boundary is refused", {
  # Absent from the whole middle block (2010-2012 at block_years = 3): no
  # single block sees a hole, only the carried cells do.
  .cbb_expect_lattice_refused(.cbb_without(
    \(x) x$lon == 100.25 & x$year %in% 2010:2012
  ))
  # Appearing only from the second block on.
  .cbb_expect_lattice_refused(.cbb_without(
    \(x) x$lon == 100.25 & x$year <= 2009L
  ))
  # A year missing for every cell, between two blocks.
  .cbb_expect_lattice_refused(.cbb_without(\(x) x$year == 2010L))
})

testthat::test_that("a block that reaches back into marched years is refused", {
  classes <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    area_code = 1L,
    year = 2001L
  )
  cells <- classes[c("lon", "lat", "area_code")]
  testthat::expect_error(
    whep:::.cb_check_march_years(classes, list(cells = cells, year = 2001L)),
    class = "whep_block_overlap"
  )
  testthat::expect_no_error(
    whep:::.cb_check_march_years(classes, list(cells = cells, year = 2000L))
  )
})

testthat::test_that("the reports of finished blocks survive a later abort", {
  # The climate gap is reported by every block; the lattice fails in the
  # second. A single pass reports the gap before it fails, and so must this.
  d <- .cbb_without(\(x) x$lon == 100.25 & x$year %in% 2010:2012)
  said <- character()
  testthat::expect_error(
    withCallingHandlers(
      whep::build_carbon_balance(data = d, block_years = 3),
      warning = function(w) {
        said <<- c(said, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    ),
    class = "whep_incomplete_lattice"
  )
  testthat::expect_true(any(grepl("Dropped 2 cell-polity compartments", said)))
})

testthat::test_that("a span with nothing to march says so", {
  d <- .cbb_data()
  d$climate <- dplyr::mutate(d$climate, area_code = 999L)
  for (size in c(Inf, 2)) {
    testthat::expect_error(
      suppressWarnings(suppressMessages(
        whep::build_carbon_balance(data = d, block_years = size)
      )),
      class = "whep_empty_carbon_march"
    )
  }
})

testthat::test_that("block_years must be a positive whole number or Inf", {
  for (bad in list(0, -1, 1.5, NA_real_, "3", c(2, 3), NULL)) {
    testthat::expect_error(
      whep::build_carbon_balance(example = TRUE, block_years = bad),
      class = "whep_bad_block_years",
      info = format(bad)
    )
  }
  for (good in list(1L, 2, Inf)) {
    testthat::expect_identical(whep:::.cb_check_block_years(good), good)
  }
})

testthat::test_that("the block plan covers the span once, in order", {
  plan <- whep:::.cb_block_plan(list(), c(2003L, 2001:2007), 3)
  testthat::expect_identical(
    purrr::map(plan, "years"),
    list(2001:2003, 2004:2006, 2007L)
  )
  testthat::expect_identical(
    purrr::map(plan, "read"),
    purrr::map(plan, "years")
  )
  testthat::expect_identical(purrr::map_int(plan, "index"), 1:3)
  # One block keeps the single pass's own call: nothing sliced, readers scoped
  # to `years` exactly as given.
  one <- whep:::.cb_block_plan(list(), c(2003L, 2001:2002), 3)
  testthat::expect_identical(
    one,
    list(list(index = 1L, years = NULL, read = c(2003L, 2001:2002)))
  )
  # A supplied land use sets the span, even when the readers are unscoped.
  d <- .cbb_data()
  testthat::expect_identical(
    purrr::map(whep:::.cb_block_plan(d, NULL, 4), "years"),
    list(2007:2010, 2011:2013)
  )
  # A supplied land use running outside `years`, and no span at all, are one
  # block.
  testthat::expect_length(whep:::.cb_block_plan(d, 2007:2009, 2), 1L)
  testthat::expect_length(whep:::.cb_block_plan(list(), NULL, 2), 1L)
})

testthat::test_that("blocks with different columns are not bound", {
  store <- new.env()
  store$parts <- list(tibble::tibble(a = 1), tibble::tibble(b = 2))
  testthat::expect_error(whep:::.cb_bind_parts(store), "different columns")
  store$parts <- list(
    tibble::tibble(a = 1:2, b = "x"),
    tibble::tibble(a = 3L, b = "y")
  )
  testthat::expect_identical(
    whep:::.cb_bind_parts(store),
    list(a = 1:3, b = c("x", "x", "y"))
  )
})

testthat::test_that("reports that cannot be merged are raised as they came", {
  lost <- tibble::tibble(area_code = 68L, year = 2007L)
  item <- list(kind = "cb_lost_polities", summary = lost)
  # Twice in one block: two sources, raised separately.
  log <- whep:::.cb_report_log()
  whep:::.cb_log_reports(log, "classes", list(item, item), list(index = 1L))
  said <- .cbb_record(whep:::.cb_emit_reports(log))$conditions
  testthat::expect_length(said, 2L)
  # Once in each of two blocks: one combined warning.
  log <- whep:::.cb_report_log()
  whep:::.cb_log_reports(log, "classes", list(item), list(index = 1L))
  whep:::.cb_log_reports(log, "classes", list(item), list(index = 2L))
  said <- .cbb_record(whep:::.cb_emit_reports(log))$conditions
  testthat::expect_length(said, 1L)
  # A kind the driver does not know is re-raised, never dropped.
  captured <- whep:::.cb_capture(cli::cli_warn(
    "an unknown report",
    class = "whep_report",
    report_kind = "not_a_kind",
    report_summary = NULL
  ))
  log <- whep:::.cb_report_log()
  whep:::.cb_log_reports(log, "tail", captured$reports, list(index = 1L))
  said <- .cbb_record(whep:::.cb_emit_reports(log))$conditions
  testthat::expect_identical(said, list(c("warning", "an unknown report")))
})
