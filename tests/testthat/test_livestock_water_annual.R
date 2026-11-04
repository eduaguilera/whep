.annual_water_inventory <- function() {
  polity <- tibble::tibble(year = 2000L, area_code = 203L) |>
    whep::add_polity_code() |>
    dplyr::pull(.data$polity_code)
  tibble::tibble(
    exposure_id = c("adult", "young"),
    year = 2000L,
    reporting_polity_code = polity,
    item_cbs_code = 960L,
    cohort = c("lactating", "young"),
    production_system = "fixture",
    animal_days = c(100, 40),
    period_start = as.Date("2000-01-01"),
    period_end = as.Date("2000-12-31"),
    inventory_source = "Illustrative independent annual cohort inventory",
    inventory_scope = "Illustrative whole herd; both cohorts supplied"
  )
}

.annual_water_ledger <- function() {
  .annual_water_inventory() |>
    dplyr::select(-inventory_source, -inventory_scope) |>
    dplyr::mutate(
      quantity_basis = "net_consumptive_blue_water",
      consumption_m3 = c(3, 1),
      coverage_direct_water = "complete",
      consumption_source = "Illustrative net water balance",
      consumption_basis = "measured",
      water_origin = "surface_or_groundwater",
      consumption_boundary = "net_freshwater_consumption",
      boundary_source = "Illustrative matched withdrawals and returns",
      exposure_source = "Illustrative cohort observations"
    )
}

test_that("annual volumes retain inventory and are not multiplied by heads", {
  x <- .annual_water_ledger()
  out <- whep::as_annual_livestock_water(x, .annual_water_inventory())
  expect_equal(out$impact_u, c(3, 1))
  expect_equal(out$animal_days, c(100, 40))
  expect_equal(out$reporting_polity_code, x$reporting_polity_code)
  expect_equal(out$quantity_basis, x$quantity_basis)
  expect_true(all(out$coverage_status == "matched"))
  expect_equal(attr(out, "exposure_inventory"), .annual_water_inventory())
})

test_that("an entirely missing cohort is visible despite balanced volumes", {
  x <- .annual_water_ledger()[1, ]
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_coverage"
  )
  expect_warning(
    out <- whep::as_annual_livestock_water(
      x,
      .annual_water_inventory(),
      coverage = "partial"
    ),
    class = "whep_annual_water_partial"
  )
  expect_equal(nrow(out), 2L)
  expect_equal(out$coverage_status, c("matched", "missing_exposure"))
  expect_true(is.na(out$impact_u[2]))
  expect_equal(sum(out$impact_u, na.rm = TRUE), 3)
})

test_that("requirements and intake are retained but never summed as consumption", {
  x <- .annual_water_ledger() |>
    dplyr::mutate(quantity_basis = c("requirement", "free_drinking_intake"))
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_coverage"
  )
  expect_warning(
    out <- whep::as_annual_livestock_water(
      x,
      .annual_water_inventory(),
      coverage = "partial"
    ),
    class = "whep_annual_water_partial"
  )
  expect_equal(out$quantity_basis, x$quantity_basis)
  expect_true(all(is.na(out$impact_u)))
  expect_true(all(out$coverage_status == "incompatible_quantity"))
})

test_that("identity, scope and exposure cannot be silently changed", {
  x <- .annual_water_ledger()
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::bind_rows(x, x[1, ]),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_identity"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, animal_days = animal_days + 1),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_identity"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, exposure_id = c("extra", "young")),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_identity"
  )
  bad <- dplyr::mutate(
    .annual_water_inventory(),
    reporting_polity_code = "not-a-polity"
  )
  expect_error(
    whep::as_annual_livestock_water(x, bad),
    class = "whep_annual_water_polity"
  )
})

test_that("unknown components, origin and consumption remain gaps", {
  x <- .annual_water_ledger() |>
    dplyr::mutate(coverage_direct_water = "service_unknown")
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_coverage"
  )
  x <- .annual_water_ledger() |>
    dplyr::mutate(water_origin = "unknown")
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_coverage"
  )
  x <- .annual_water_ledger() |>
    dplyr::mutate(consumption_m3 = NA_real_)
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_coverage"
  )
})

test_that("real Cardot study is not silently annualized", {
  # Cardot et al. 2008 Tables 1, 3 and 4, doi:10.3168/jds.2007-0204.
  # Actual reported cow-days and rounded means; not raw individual records.
  study <- tibble::tibble(
    animal_days = 1837,
    item_cbs_code = 960L,
    cohort = "lactating",
    production_system = "free_stall_tmr",
    dry_matter_intake_kg_day = 20.6,
    milk_kg_day = 26.5,
    diet_dry_matter_percent = 48.1,
    minimum_temperature_c = 0.1,
    rainfall_mm_day = 1.3,
    assessment_period = "2003–2005 winter trials"
  ) |>
    whep::estimate_dairy_drinking()
  expect_equal(study$drinking_m3, 153.933252)
  expect_error(
    whep::as_annual_livestock_water(
      study,
      .annual_water_inventory()
    ),
    class = "whep_annual_water_scope"
  )
  x <- .annual_water_ledger() |>
    dplyr::mutate(period_start = as.Date("1999-12-01"))
  expect_error(
    whep::as_annual_livestock_water(x, .annual_water_inventory()),
    class = "whep_annual_water_scope"
  )
})

test_that("unknown withdrawal conversion remains visible under partial coverage", {
  x <- .annual_water_ledger() |>
    dplyr::mutate(quantity_basis = "withdrawal", consumption_m3 = NA_real_)
  expect_warning(
    out <- whep::as_annual_livestock_water(
      x,
      .annual_water_inventory(),
      coverage = "partial"
    ),
    class = "whep_annual_water_partial"
  )
  expect_true(all(out$coverage_status == "missing_consumption"))
  expect_true(all(is.na(out$impact_u)))
})

test_that("withdrawal conversion must be explicit and arithmetically consistent", {
  x <- .annual_water_ledger() |>
    dplyr::mutate(
      quantity_basis = "withdrawal",
      drinking_m3 = c(5, 1),
      service_m3 = c(1, 1),
      drinking_fraction = 0.5,
      service_fraction = 0.5
    )
  out <- whep::as_annual_livestock_water(x, .annual_water_inventory())
  expect_equal(out$impact_u, c(3, 1))
  expect_equal(out$drinking_fraction, x$drinking_fraction)
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, drinking_fraction = 2),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_input"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, consumption_m3 = 777),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_input"
  )
})

test_that("annual output collisions and invalid periods cannot erase context", {
  x <- .annual_water_ledger()
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, impact_u = 777),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_input"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, year = 2000.5),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_scope"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, period_end = as.Date("1999-12-31")),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_scope"
  )
})

test_that("known zero consumption and empty scopes are preserved", {
  x <- .annual_water_ledger() |>
    dplyr::mutate(consumption_m3 = 0)
  out <- whep::as_annual_livestock_water(x, .annual_water_inventory())
  expect_equal(out$impact_u, c(0, 0))
  expect_true(all(out$coverage_status == "matched"))
  out <- whep::as_annual_livestock_water(x[0, ], .annual_water_inventory()[0, ])
  expect_equal(nrow(out), 0L)
  expect_type(out$impact_u, "double")
})

test_that("canonical final reporting years remain distinct from future years", {
  inventory <- .annual_water_inventory()[rep(1, 3), ] |>
    dplyr::mutate(
      exposure_id = c("ussr", "blx", "sudan"),
      year = c(1991L, 1999L, 2011L),
      reporting_polity_code = c(
        "F228-1945-1991",
        "BLX-1850-1999",
        "SUD-1956-2011"
      ),
      period_start = as.Date(paste0(year, "-01-01")),
      period_end = as.Date(paste0(year, "-12-31"))
    )
  context <- .annual_water_ledger()[rep(1, 3), ] |>
    dplyr::select(-dplyr::any_of(names(inventory)))
  ledger <- dplyr::bind_cols(
    dplyr::select(inventory, -inventory_source, -inventory_scope),
    context
  )
  out <- whep::as_annual_livestock_water(ledger, inventory)
  expect_equal(out$reporting_polity_code, inventory$reporting_polity_code)
  expect_true(all(out$polity_year_basis == "declared_reporting_year"))
  inventory <- dplyr::mutate(
    inventory,
    year = year + 1L,
    period_start = as.Date(paste0(year, "-01-01")),
    period_end = as.Date(paste0(year, "-12-31"))
  )
  expect_error(
    whep::as_annual_livestock_water(ledger, inventory),
    class = "whep_annual_water_polity"
  )
})

test_that("invalid quantities and missing boundary sources cannot be covered", {
  x <- .annual_water_ledger()
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, consumption_m3 = -1),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_input"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, consumption_m3 = Inf),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_input"
  )
  expect_error(
    whep::as_annual_livestock_water(
      dplyr::mutate(x, boundary_source = ""),
      .annual_water_inventory()
    ),
    class = "whep_annual_water_coverage"
  )
  inventory <- dplyr::mutate(.annual_water_inventory(), animal_days = 0)
  x <- dplyr::mutate(x, animal_days = 0, consumption_m3 = NA_real_)
  expect_error(
    whep::as_annual_livestock_water(x, inventory),
    class = "whep_annual_water_coverage"
  )
})

test_that("eligible impact is typed separately from the original ledger basis", {
  out <- whep::as_annual_livestock_water(
    .annual_water_ledger(),
    .annual_water_inventory()
  )
  expect_equal(out$impact_quantity_basis, rep("net_consumptive_blue_water", 2))
})
