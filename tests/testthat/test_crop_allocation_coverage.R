.cc_allocation <- function() {
  a <- .ca_allocation()
  a$weights <- dplyr::filter(a$weights, .data$item_prod_code == 15L) |>
    dplyr::mutate(item_prod_code = 536L)
  a
}

.cc_primary <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    ~source, ~fao_flag,
    2010L, 10L,        536L,            2625L,          "ha", 100,
    "observed", "A",
    2010L, 10L,        550L,            2625L,          "ha", 100,
    "imputed", "M"
  )
}

.cc_spread <- function(policy = NULL, primary = .cc_primary()) {
  whep::spatialize_country_n_to_crops(
    tibble::tibble(year = 2010L, area_code = 10L, n_t = 100),
    tibble::tibble(
      year = 2010L,
      area_code = 10L,
      item_cbs_code = 2625L,
      area_share = 1
    ),
    tibble::tibble(
      lon = c(0.25, 0.75),
      lat = 50.25,
      area_code = 10L,
      polity_frac = 1,
      cell_area_ha = 1000
    ),
    resolution = "grid",
    data = list(
      crop_allocation = .cc_allocation(),
      primary_prod = primary,
      component_policy = policy
    )
  )
}

testthat::test_that("partial CBS coverage cannot hide a missing component", {
  err <- tryCatch(.cc_spread(), error = identity)
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_equal(err$missing_components$item_prod_code, 550L)
  testthat::expect_equal(err$missing_components$fao_flag, "M")
  testthat::expect_equal(err$missing_components$source, "imputed")
})

testthat::test_that("legacy pooling requires explicit choice and diagnostics", {
  testthat::expect_warning(
    out <- .cc_spread("legacy_pool"),
    class = "whep_crop_component_coverage"
  )
  testthat::expect_equal(sum(out$n_t), 100)
  testthat::expect_true(all(out$method_crop_components == "legacy_pool"))
  audit <- attr(out, "crop_component_coverage")
  testthat::expect_equal(
    dplyr::filter(audit, .data$coverage_status == "missing_component") |>
      dplyr::pull("item_prod_code"),
    550L
  )
})

testthat::test_that("complete component coverage keeps numerical placement", {
  out <- .cc_spread(
    primary = dplyr::filter(.cc_primary(), item_prod_code == 536L)
  )
  testthat::expect_equal(out$n_t, c(75, 25))
  testthat::expect_true(all(
    out$method_crop_components == "supplied_targets_covered"
  ))
})

testthat::test_that("unknown coverage is not declared complete", {
  out <- .cc_spread(primary = NULL)
  testthat::expect_true(all(
    out$method_crop_components == "targets_unavailable"
  ))
})

testthat::test_that("component policy is validated and threaded by N callers", {
  testthat::expect_error(.cc_spread("invent_crop"), class = "rlang_error")
  data <- list(primary_prod = .cc_primary(), component_policy = "legacy_pool")
  routed <- whep:::.ni_allocation_data(data)
  testthat::expect_equal(routed$primary_prod, data$primary_prod)
  testthat::expect_equal(routed$component_policy, "legacy_pool")
})


testthat::test_that("coverage only inspects requested territory and year", {
  primary <- dplyr::filter(.cc_primary(), .data$item_prod_code == 536L)
  outside <- dplyr::mutate(.cc_primary(), area_code = 20L, value = NA_real_)
  out <- .cc_spread(primary = dplyr::bind_rows(primary, outside))
  testthat::expect_equal(sum(out$n_t), 100)
  testthat::expect_equal(nrow(attr(out, "crop_component_coverage")), 1L)
})

testthat::test_that("a missing target stays missing in its diagnostic", {
  primary <- dplyr::mutate(
    .cc_primary(),
    value = dplyr::if_else(
      .data$item_prod_code == 550L,
      NA_real_,
      .data$value
    )
  )
  err <- tryCatch(.cc_spread(primary = primary), error = identity)
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_true(is.na(err$missing_components$target_area_ha))
  testthat::expect_equal(
    err$missing_components$coverage_status,
    "missing_target"
  )
})

testthat::test_that("N caller adapters enforce component coverage", {
  totals <- tibble::tibble(year = 2010L, area_code = 10L, n_t = 100)
  shares <- tibble::tibble(
    year = 2010L,
    area_code = 10L,
    item_cbs_code = 2625L,
    area_share = 1
  )
  data <- list(
    primary_prod = .cc_primary(),
    crop_allocation = .cc_allocation(),
    cell_polity = tibble::tibble(
      lon = c(0.25, 0.75),
      lat = 50.25,
      area_code = 10L,
      polity_frac = 1,
      cell_area_ha = 1000
    )
  )
  testthat::expect_error(
    whep:::.ni_synthetic_grid(totals, shares, data),
    class = "whep_crop_component_coverage"
  )
  testthat::expect_error(
    whep:::.ni_spread_country_to_crops(totals, shares, data),
    class = "whep_crop_component_coverage"
  )
  data$component_policy <- "legacy_pool"
  testthat::expect_warning(
    out <- whep:::.ni_synthetic_grid(totals, shares, data),
    class = "whep_crop_component_coverage"
  )
  testthat::expect_equal(unique(out$method_crop_components), "legacy_pool")
})

testthat::test_that("coverage never borrows same-bucket component support", {
  a <- .cc_allocation()
  first <- dplyr::mutate(
    a$weights,
    area_code = 999L,
    reporting_polity_code = "A"
  )
  second <- dplyr::mutate(
    first,
    reporting_polity_code = "B",
    item_prod_code = 550L
  )
  a$weights <- dplyr::bind_rows(first, second)
  primary <- dplyr::mutate(
    .cc_primary(),
    area_code = 999L,
    reporting_polity_code = "A"
  )
  primary <- dplyr::bind_rows(
    primary,
    dplyr::mutate(primary[2, ], reporting_polity_code = "B")
  )
  mapping <- tibble::tibble(
    item_prod_code = c(536L, 550L),
    item_cbs_code = 2625L
  )
  audit <- whep:::.crop_component_audit(a, primary, mapping)
  missing <- dplyr::filter(audit, .data$coverage_status == "missing_component")
  testthat::expect_equal(missing$reporting_polity_code, "A")
  testthat::expect_equal(missing$item_prod_code, 550L)
})


testthat::test_that("a missing production code cannot vanish from CBS coverage", {
  primary <- .cc_primary()
  primary$item_prod_code[2] <- NA_integer_
  err <- tryCatch(.cc_spread(primary = primary), error = identity)
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_true(is.na(err$missing_components$item_prod_code))
  testthat::expect_equal(
    err$missing_components$coverage_status,
    "missing_mapping"
  )
  testthat::expect_equal(err$missing_components$fao_flag, "M")
})

testthat::test_that("conflicting component mapping is reported", {
  primary <- .cc_primary()
  primary$item_prod_code[2] <- 15L
  mapping <- tibble::tibble(
    item_prod_code = c(536L, 15L),
    item_cbs_code = c(2625L, 2511L)
  )
  audit <- whep:::.crop_component_audit(.cc_allocation(), primary, mapping)
  err <- tryCatch(
    whep:::.crop_component_check(audit, "error"),
    error = identity
  )
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_equal(
    err$missing_components$coverage_status,
    "conflicting_mapping"
  )
})

testthat::test_that("unknown CBS mapping remains an inventory uncertainty", {
  primary <- .cc_primary()
  primary$item_prod_code[2] <- NA_integer_
  primary$item_cbs_code[2] <- NA_integer_
  err <- tryCatch(.cc_spread(primary = primary), error = identity)
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_equal(
    err$missing_components$coverage_status,
    "missing_mapping"
  )
  testthat::expect_true(is.na(err$missing_components$item_cbs_code))
})

testthat::test_that("negative harvested areas are not silently removed", {
  primary <- .cc_primary()
  primary$value[2] <- -1
  err <- tryCatch(.cc_spread(primary = primary), error = identity)
  testthat::expect_s3_class(err, "whep_crop_component_coverage")
  testthat::expect_equal(err$missing_components$target_area_ha, -1)
  testthat::expect_equal(
    err$missing_components$coverage_status,
    "invalid_target"
  )
})

.cc_scope_inputs <- function() {
  list(
    primary = tibble::tibble(
      year = 2010L,
      area_code = 10L,
      reporting_polity_code = "A",
      item_prod_code = 536L,
      item_cbs_code = 2625L,
      unit = "ha",
      value = 100
    ),
    weights = tibble::tibble(
      year = 2010L,
      area_code = 10L,
      item_prod_code = 536L,
      area_weight = 1
    ),
    groups = tibble::tibble(
      year = 2010L,
      area_code = 10L,
      item_cbs_code = 2625L
    ),
    mapping = tibble::tibble(item_prod_code = 536L, item_cbs_code = 2625L)
  )
}

.cc_scope_audit <- function(x) {
  whep:::.crop_component_audit(
    list(weights = x$weights),
    x$primary,
    x$mapping,
    x$groups
  )
}

testthat::test_that("unrequested missing identities do not block coverage", {
  x <- .cc_scope_inputs()
  x$primary <- dplyr::bind_rows(
    x$primary,
    dplyr::mutate(
      x$primary,
      area_code = 20L,
      reporting_polity_code = NA_character_
    )
  )
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(audit$reporting_polity_code, "A")
  testthat::expect_equal(audit$coverage_status, "placed")
})

testthat::test_that("unrequested legacy support is excluded before resolution", {
  x <- .cc_scope_inputs()
  x$weights <- dplyr::bind_rows(
    x$weights,
    dplyr::mutate(x$weights, area_code = 20L)
  )
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(audit$reporting_polity_code, "A")
  testthat::expect_equal(audit$coverage_status, "placed")
})

testthat::test_that("requested missing identities still reject coverage", {
  x <- .cc_scope_inputs()
  x$primary$reporting_polity_code <- NA_character_
  testthat::expect_error(
    .cc_scope_audit(x),
    class = "whep_crop_allocation_identity"
  )
})

testthat::test_that("native coverage does not match numeric annotations", {
  x <- .cc_scope_inputs()
  x$groups$reporting_polity_code <- "A"
  x$groups$area_code <- 30L
  x$weights$reporting_polity_code <- "A"
  x$weights$area_code <- 20L
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(audit$reporting_polity_code, "A")
  testthat::expect_equal(audit$coverage_status, "placed")
})

testthat::test_that("an empty scoped inventory does not claim coverage", {
  x <- .cc_scope_inputs()
  x$primary$area_code <- 20L
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(nrow(audit), 0L)
  testthat::expect_equal(
    whep:::.crop_component_check(audit, "error"),
    "targets_unavailable"
  )
})

testthat::test_that("source-code adapters normalize candidate key types", {
  x <- .cc_scope_inputs()
  x$primary$area_code <- as.character(x$primary$area_code)
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(audit$reporting_polity_code, "A")
  testthat::expect_equal(audit$coverage_status, "placed")
  testthat::expect_equal(audit$area_code, "10")

  x <- .cc_scope_inputs()
  x$weights$area_code <- as.character(x$weights$area_code)
  audit <- .cc_scope_audit(x)
  testthat::expect_equal(audit$reporting_polity_code, "A")
  testthat::expect_equal(audit$coverage_status, "placed")
  testthat::expect_equal(audit$area_code, 10L)
})


.cy_spread <- function(
  allocation = .cc_allocation(),
  primary = .cc_primary()[1, ],
  year = 2010L
) {
  whep::spatialize_country_n_to_crops(
    tibble::tibble(year = year, area_code = 10L, n_t = 100),
    tibble::tibble(
      year = year,
      area_code = 10L,
      item_cbs_code = 2625L,
      area_share = 1
    ),
    tibble::tibble(
      lon = c(0.25, 0.75),
      lat = 50.25,
      area_code = 10L,
      polity_frac = 1,
      cell_area_ha = 1000
    ),
    resolution = "grid",
    data = list(crop_allocation = allocation, primary_prod = primary)
  )
}

testthat::test_that("public allocation rejects malformed annual keys", {
  invalid <- list(2010.5, NA_real_, Inf, -Inf, 1e20, "2010", "2010.5")
  purrr::walk(invalid, \(yr) {
    a <- .cc_allocation()
    a$weights$year <- yr
    testthat::expect_error(.cy_spread(a), class = "whep_crop_allocation_year")
    a <- .cc_allocation()
    a$fallback$year <- yr
    testthat::expect_error(.cy_spread(a), class = "whep_crop_allocation_year")
    testthat::expect_error(
      .cy_spread(year = yr),
      class = "whep_crop_allocation_year"
    )
  })
})

testthat::test_that("invalid source years cannot hide missing components", {
  invalid <- list(2010.5, NA_real_, Inf, -Inf, 1e20, "2010", "2010.5")
  purrr::walk(invalid, \(yr) {
    primary <- .cc_primary()
    primary$year[2] <- yr
    testthat::expect_error(
      .cy_spread(primary = primary),
      class = "whep_crop_allocation_year"
    )
  })
})

testthat::test_that("whole numeric annual keys retain public placement", {
  expected <- .cy_spread()
  a <- .cc_allocation()
  a$weights$year <- as.numeric(a$weights$year)
  a$fallback$year <- as.numeric(a$fallback$year)
  primary <- .cc_primary()[1, ]
  primary$year <- as.numeric(primary$year)
  testthat::expect_equal(.cy_spread(a, primary, 2010), expected)
  testthat::expect_equal(expected$n_t, c(75, 25))
})


.cy_unexpected_inputs <- function(...) {
  cli::cli_abort("Input resolution must not run.", class = "unexpected_input")
}

testthat::test_that("build N validates requested years before resolving data", {
  testthat::local_mocked_bindings(
    .ni_resolve_allocation = .cy_unexpected_inputs,
    .package = "whep"
  )
  testthat::expect_error(
    whep::build_n_inputs(years = 2010.5),
    class = "whep_crop_allocation_year"
  )
})

testthat::test_that("mixed geography cannot resolve an invalid year", {
  x <- .cc_scope_inputs()
  x$primary$year <- 2010.5
  testthat::expect_error(
    whep:::.crop_alloc_resolve(x$weights, x$primary),
    class = "whep_crop_allocation_year"
  )
  testthat::expect_error(
    whep:::.crop_allocation(
      2010.5,
      NULL,
      list(crop_allocation = .cc_allocation())
    ),
    class = "whep_crop_allocation_year"
  )
})
