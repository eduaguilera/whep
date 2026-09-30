# Fixtures. Each scenario is a handful of crop rows on the canonical 0.5-degree
# grid (0.25 N row) and a critical surface, run through the real
# build_n_boundary_exceedance(resolution = "grid"), so the country table is
# checked against the grid it is meant to summarise. Expected values are
# derived by hand in each test, in t N. A critical rate of r kg N/ha on 100 ha
# is an allowance of r / 10 t N.

.nbc_lon <- function(cell) {
  c(A = 0.25, B = 0.75, C = 1.25, D = 1.75)[cell]
}

# rows: cell, area_code, item_cbs_code, surplus_n_t, n_input_std_t.
.nbc_surplus <- function(rows) {
  rows |>
    dplyr::mutate(
      lon = unname(.nbc_lon(.data$cell)),
      lat = 0.25,
      year = 2015L,
      area_ha = 100
    ) |>
    dplyr::select(-"cell")
}

# rates: cell, rate (kg N/ha) on a 100 ha source area.
.nbc_critical <- function(rates) {
  rates |>
    dplyr::mutate(
      lon = unname(.nbc_lon(.data$cell)),
      lat = 0.25,
      value = .data$rate,
      source_area_ha = 100,
      image_region = 11L,
      critical_var = "critical_n_surplus",
      critical_land_use = "all",
      critical_threshold = "mi",
      critical_year = 2010L
    ) |>
    dplyr::select(-"cell", -"rate")
}

.nbc_grid <- function(
  surplus,
  critical,
  negative_critical = "keep",
  resolution = "grid"
) {
  whep::build_n_boundary_exceedance(
    surplus = surplus,
    critical = critical,
    land_use = "all",
    resolution = resolution,
    metric = "surplus",
    actual_year = 2015L,
    critical_reference_year = 2010L,
    grassland_split = "none",
    negative_critical = negative_critical
  )
}

.nbc_ag_land <- function(codes = 1:3) {
  tibble::tibble(area_code = as.integer(codes), year = 2015L, area_ha = 1000)
}

# One run: rows and rates in, the country/diagnostics list out.
.nbc_run <- function(
  rows,
  rates,
  negative_critical = "keep",
  ag_land = .nbc_ag_land(),
  ...
) {
  surplus <- .nbc_surplus(rows)
  grid <- .nbc_grid(surplus, .nbc_critical(rates), negative_critical)
  whep::build_n_boundary_country(grid, surplus, ag_land, ...)
}

.nbc_row <- function(x, code) {
  dplyr::filter(x$country, .data$area_code == code)
}

testthat::test_that("a shared border cell is not counted once per polity", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          12,           20,
    "A",   2L,         2511L,          8,            10
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 100)
  out <- .nbc_run(rows, rates)
  one <- .nbc_row(out, 1L)
  two <- .nbc_row(out, 2L)

  # The cell surplus is 20 t and its allowance 10 t, so the overshoot is 10 t,
  # shared 12:8 between the polities.
  testthat::expect_equal(one$exceedance_n_t, 6)
  testthat::expect_equal(two$exceedance_n_t, 4)
  testthat::expect_equal(one$positive_surplus_n_t, 12)
  testthat::expect_equal(two$positive_surplus_n_t, 8)
  testthat::expect_equal(one$exceeding_surplus_n_t, 12)
  testthat::expect_equal(two$exceeding_surplus_n_t, 8)
  testthat::expect_equal(c(one$beyond_share, two$beyond_share), c(1, 1))
  # The exceedance over the positive surplus: 6 / 12 and 4 / 8.
  testthat::expect_equal(
    c(
      one$exceedance_share_of_positive_surplus,
      two$exceedance_share_of_positive_surplus
    ),
    c(0.5, 0.5)
  )
  testthat::expect_equal(one$input_std_n_t, 20)
  testthat::expect_equal(two$input_std_n_t, 10)
  testthat::expect_equal(one$excess_share_of_inputs, 6 / 20)
  testthat::expect_equal(two$excess_share_of_inputs, 4 / 10)

  # The world sum is the cell's positive surplus once, from the cell result.
  cell <- .nbc_grid(
    .nbc_surplus(rows),
    .nbc_critical(rates),
    resolution = "cell"
  )
  testthat::expect_equal(cell$cell_actual_n_t, 20)
  testthat::expect_equal(sum(out$country$positive_surplus_n_t), 20)
  testthat::expect_false(
    isTRUE(all.equal(sum(out$country$positive_surplus_n_t), 2 * 20))
  )
  testthat::expect_equal(out$diagnostics$positive_surplus_n_t, 20)
})

testthat::test_that("deficit cells never offset excess in another cell", {
  # Country 1: cell A +10 t against a 4 t allowance (6 t over), cell B -8 t.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          10,           15,
    "B",   1L,         2511L,          -8,           1
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 40, "B", 50)
  out <- .nbc_run(rows, rates)$country

  # The positive surplus is 10 t, not the net of 2 t.
  testthat::expect_equal(out$positive_surplus_n_t, 10)
  testthat::expect_equal(out$exceeding_surplus_n_t, 10)
  testthat::expect_equal(out$beyond_share, 1)
  testthat::expect_equal(out$boundary_side, "Exceedance")
  testthat::expect_equal(out$exceedance_n_t, 6)
  testthat::expect_equal(out$input_std_n_t, 16)
  testthat::expect_equal(out$excess_share_of_inputs, 6 / 16)

  # Loosen A's allowance to 20 t: nothing exceeds, the positive surplus stays.
  loose <- .nbc_run(rows, tibble::tribble(~cell, ~rate, "A", 200, "B", 50))
  testthat::expect_equal(loose$country$positive_surplus_n_t, 10)
  testthat::expect_equal(loose$country$exceeding_surplus_n_t, 0)
  testthat::expect_equal(loose$country$beyond_share, 0)
  testthat::expect_equal(loose$country$boundary_side, "Within_boundary")
})

testthat::test_that("the beyond-share cut is inclusive and an argument", {
  # Two +10 t cells, one over its 4 t allowance and one under its 20 t one:
  # exactly half of the positive surplus lies in an exceeding cell.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          10,           12,
    "B",   1L,         2511L,          10,           12
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 40, "B", 200)
  half <- .nbc_run(rows, rates)$country
  testthat::expect_equal(half$beyond_share, 0.5)
  testthat::expect_equal(half$beyond_share_cut, 0.5)
  testthat::expect_equal(half$boundary_side, "Within_boundary")

  low <- .nbc_run(rows, rates, beyond_share_cut = 0.4)$country
  testthat::expect_equal(low$beyond_share_cut, 0.4)
  testthat::expect_equal(low$boundary_side, "Exceedance")
  testthat::expect_equal(low$beyond_share, 0.5)
})

testthat::test_that("negative critical surplus: keep versus clamp", {
  # A +1 t surplus with a -2 t allowance (critical -20 kg N/ha on 100 ha),
  # and 2 t of inputs. Kept, the overshoot is 1 - (-2) = 3 t, more than the
  # inputs; clamped it is 1 t.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          1,            2
  )
  rates <- tibble::tribble(~cell, ~rate, "A", -20)
  keep <- .nbc_run(rows, rates, "keep")
  clamp <- .nbc_run(rows, rates, "clamp")

  testthat::expect_equal(keep$country$exceedance_n_t, 3)
  testthat::expect_equal(keep$country$excess_share_of_inputs, 1.5)
  testthat::expect_true(keep$country$ratio_outside_unit)
  testthat::expect_equal(keep$diagnostics$excess_share_of_inputs, 1.5)
  testthat::expect_equal(keep$diagnostics$n_ratio_outside_unit, 1L)
  testthat::expect_true(keep$diagnostics$world_ratio_outside_unit)
  testthat::expect_equal(keep$country$negative_critical, "keep")
  # 3 t of exceedance over 1 t of positive surplus.
  testthat::expect_equal(keep$country$exceedance_share_of_positive_surplus, 3)
  testthat::expect_equal(
    keep$diagnostics$exceedance_share_of_positive_surplus,
    3
  )

  testthat::expect_equal(clamp$country$exceedance_n_t, 1)
  testthat::expect_equal(clamp$country$excess_share_of_inputs, 0.5)
  testthat::expect_false(clamp$country$ratio_outside_unit)
  testthat::expect_equal(clamp$diagnostics$excess_share_of_inputs, 0.5)
  testthat::expect_false(clamp$diagnostics$world_ratio_outside_unit)
  testthat::expect_equal(clamp$country$exceedance_share_of_positive_surplus, 1)
  testthat::expect_equal(clamp$country$negative_critical, "clamp")

  # The country's own positive surplus is the same either way.
  for (out in list(keep, clamp)) {
    testthat::expect_equal(out$country$positive_surplus_n_t, 1)
    testthat::expect_equal(out$country$exceeding_surplus_n_t, 1)
    testthat::expect_equal(out$country$beyond_share, 1)
  }
})

testthat::test_that("an exceeding cell with no positive surplus is not counted", {
  # Cell A nets to zero surplus (country 1 +5 t, country 2 -5 t) against a
  # -2 t allowance, so kept it overshoots by 2 t. That overshoot is exceedance
  # (left on a residual row: the crop shares of a zero total are undefined),
  # but the cell is not part of the positive surplus, so country 1's +5 t there
  # must not enter the numerator of its share; counted, the share would be
  # 5 / 4 above one.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          5,            6,
    "A",   2L,         2511L,          -5,           1,
    "B",   1L,         2511L,          4,            5
  )
  rates <- tibble::tribble(~cell, ~rate, "A", -20, "B", 100)
  out <- .nbc_run(rows, rates, "keep")
  one <- .nbc_row(out, 1L)
  two <- .nbc_row(out, 2L)

  testthat::expect_equal(out$diagnostics$unallocated_exceedance_n_t, 2)
  testthat::expect_equal(out$diagnostics$overshoot_without_surplus_n_t, 2)
  testthat::expect_equal(c(one$exceedance_n_t, two$exceedance_n_t), c(0, 0))
  # Cell B (4 t against 10 t) is the only positive cell and is within.
  testthat::expect_equal(one$positive_surplus_n_t, 4)
  testthat::expect_equal(one$exceeding_surplus_n_t, 0)
  testthat::expect_equal(one$beyond_share, 0)
  testthat::expect_false(one$ratio_outside_unit)
  testthat::expect_equal(two$positive_surplus_n_t, 0)
  testthat::expect_true(is.na(two$beyond_share))
})

testthat::test_that("a country-year without positive surplus is undefined", {
  # Country 3 has one deficit cell. Country 2 has a negative own-crop surplus
  # in a cell that is positive in total (country 1 carries it): its positive
  # surplus is -3 t, also undefined. Both are flagged, listed and counted.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          8,            9,
    "A",   2L,         2511L,          -3,           1,
    "B",   3L,         2511L,          -4,           1
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 20, "B", 50)
  nourishment <- tibble::tribble(
    ~year, ~area_code, ~nourish,
    2015L, 1L,         "Over",
    2015L, 2L,         "Under",
    2015L, 3L,         "Adequate"
  )
  out <- .nbc_run(rows, rates, nourishment = nourishment)
  three <- .nbc_row(out, 3L)
  two <- .nbc_row(out, 2L)
  one <- .nbc_row(out, 1L)

  testthat::expect_equal(three$positive_surplus_n_t, 0)
  testthat::expect_true(is.na(three$beyond_share))
  testthat::expect_true(is.na(three$exceedance_share_of_positive_surplus))
  testthat::expect_true(is.na(three$boundary_side))
  testthat::expect_true(three$signed_denominator_nonpositive)
  testthat::expect_true(is.na(three$sjos_class))
  # Its inputs are positive, so the headline share is defined (0 / 1).
  testthat::expect_equal(three$excess_share_of_inputs, 0)

  testthat::expect_equal(two$positive_surplus_n_t, -3)
  testthat::expect_true(is.na(two$beyond_share))
  testthat::expect_true(is.na(two$exceedance_share_of_positive_surplus))
  testthat::expect_true(two$signed_denominator_nonpositive)
  # Cell A: 5 t surplus, 2 t allowance, 3 t over, shared -3/5 and 8/5.
  testthat::expect_equal(two$exceedance_n_t, -1.8)
  testthat::expect_true(two$ratio_outside_unit)
  testthat::expect_equal(one$exceedance_n_t, 4.8)
  testthat::expect_equal(one$beyond_share, 1)
  testthat::expect_equal(one$boundary_side, "Exceedance")
  testthat::expect_equal(as.character(one$sjos_class), "Exceedance Over")

  d <- out$diagnostics
  testthat::expect_equal(d$n_undefined_beyond_share, 2L)
  testthat::expect_equal(d$n_undefined_exceedance_share, 2L)
  testthat::expect_equal(d$n_signed_denominator_nonpositive, 2L)
  testthat::expect_equal(d$n_unclassified, 2L)
  testthat::expect_equal(d$n_countries, 3L)
})

testthat::test_that("a signed shared-cell share puts the exceedance ratio outside [0, 1]", {
  # Cell A is shared: country 1 has -3 t and country 2 +8 t, a 5 t unit against
  # a 2 t allowance, so 3 t over, shared -3/5 and 8/5 (-1.8 t and +4.8 t of
  # exceedance). Cell B is country 1 alone: 4 t against 1 t, 3 t over. Country 1
  # therefore has 1.2 t of exceedance over a positive surplus of -3 + 4 = 1 t,
  # a ratio above one, while both of its other ratios stay inside [0, 1]: every
  # unit it is in exceeds, so beyond_share is 1, and its inputs are 6 t.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          -3,           1,
    "A",   2L,         2511L,          8,            9,
    "B",   1L,         2511L,          4,            5
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 20, "B", 10)
  for (negative_critical in c("keep", "clamp")) {
    out <- .nbc_run(rows, rates, negative_critical)
    one <- .nbc_row(out, 1L)
    two <- .nbc_row(out, 2L)

    testthat::expect_equal(one$exceedance_n_t, 1.2)
    testthat::expect_equal(one$positive_surplus_n_t, 1)
    testthat::expect_equal(one$exceedance_share_of_positive_surplus, 1.2)
    testthat::expect_equal(one$beyond_share, 1)
    testthat::expect_equal(one$excess_share_of_inputs, 1.2 / 6)
    # Only the exceedance ratio is outside [0, 1], and it alone sets the flag.
    testthat::expect_true(one$ratio_outside_unit)

    testthat::expect_equal(two$exceedance_n_t, 4.8)
    testthat::expect_equal(two$exceedance_share_of_positive_surplus, 4.8 / 8)
    testthat::expect_false(two$ratio_outside_unit)

    d <- out$diagnostics
    testthat::expect_equal(d$n_ratio_outside_unit, 1L)
    testthat::expect_equal(d$n_undefined_exceedance_share, 0L)
    # World: 6 t of exceedance over 9 t of positive surplus (the two cells'
    # 5 t and 4 t), inside [0, 1] although one country is not.
    testthat::expect_equal(d$positive_surplus_n_t, 9)
    testthat::expect_equal(d$exceedance_share_of_positive_surplus, 6 / 9)
    testthat::expect_false(d$world_ratio_outside_unit)
  }
})

testthat::test_that("a negative unit beneath a negative allowance is left outside the surpluses", {
  # Cell A: -1 t against a -2 t allowance, kept, so a 1 t overshoot on a unit
  # with a negative surplus; its single crop row has a defined share of one, so
  # the country's exceedance carries the 1 t. Cell B: +4 t well within its
  # allowance. Membership follows the unit's state, not the attributed
  # exceedance: A is outside the positive surplus (4 t, not 3 t) and outside
  # the exceeding surplus (0 t), and its overshoot is reported.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          -1,           1,
    "B",   1L,         2511L,          4,            5
  )
  rates <- tibble::tribble(~cell, ~rate, "A", -20, "B", 100)
  out <- .nbc_run(rows, rates, "keep")
  one <- .nbc_row(out, 1L)
  d <- out$diagnostics

  testthat::expect_equal(one$exceedance_n_t, 1)
  testthat::expect_equal(one$positive_surplus_n_t, 4)
  testthat::expect_equal(one$exceeding_surplus_n_t, 0)
  testthat::expect_equal(one$beyond_share, 0)
  testthat::expect_equal(one$exceedance_share_of_positive_surplus, 1 / 4)
  testthat::expect_equal(one$boundary_side, "Within_boundary")
  testthat::expect_false(one$ratio_outside_unit)
  testthat::expect_equal(d$overshoot_without_surplus_n_t, 1)
  testthat::expect_equal(d$unallocated_exceedance_n_t, 0)
  testthat::expect_equal(d$n_undefined_attribution_rows, 0L)
  testthat::expect_equal(d$exceedance_gap_n_t, 0, tolerance = 1e-12)

  # Clamped, the allowance is zero and the deficit unit does not overshoot.
  clamp <- .nbc_run(rows, rates, "clamp")
  testthat::expect_equal(clamp$country$exceedance_n_t, 0)
  testthat::expect_equal(clamp$diagnostics$overshoot_without_surplus_n_t, 0)
})

testthat::test_that("the world flag reports a share of inputs above one", {
  # A surplus above the row's inputs, as a balance that includes soil organic
  # matter mineralisation can give: 5 t of surplus on 2 t of inputs with a zero
  # allowance is 5 t of exceedance even after the clamp, 2.5 times the inputs.
  # The exceedance over the positive surplus is exactly one.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          5,            2
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 0)
  out <- .nbc_run(rows, rates, "clamp")

  testthat::expect_equal(out$country$excess_share_of_inputs, 2.5)
  testthat::expect_equal(out$country$exceedance_share_of_positive_surplus, 1)
  testthat::expect_true(out$country$ratio_outside_unit)
  testthat::expect_equal(out$diagnostics$excess_share_of_inputs, 2.5)
  testthat::expect_true(out$diagnostics$world_ratio_outside_unit)
})

testthat::test_that("the residual reconciles country sums to cell exceedance", {
  # Cell A nets to zero surplus with a -2 t allowance: 2 t of overshoot that
  # no crop share can carry. Cell B is an ordinary exceeding cell.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          2,            3,
    "A",   1L,         2513L,          -2,           1,
    "B",   1L,         2511L,          6,            7,
    "B",   2L,         2511L,          2,            3
  )
  rates <- tibble::tribble(~cell, ~rate, "A", -20, "B", 50)
  surplus <- .nbc_surplus(rows)
  critical <- .nbc_critical(rates)
  grid <- .nbc_grid(surplus, critical)
  out <- whep::build_n_boundary_country(grid, surplus, .nbc_ag_land())
  d <- out$diagnostics

  # Cell B: 8 t against 5 t, 3 t over, shared 6:2.
  testthat::expect_equal(out$country$exceedance_n_t, c(2.25, 0.75))
  testthat::expect_equal(d$unallocated_exceedance_n_t, 2)
  # The independent path: the cell result, never attributed to crops.
  cell <- .nbc_grid(surplus, critical, resolution = "cell")
  total <- sum(cell$cell_positive_overshoot_n_t, na.rm = TRUE)
  testthat::expect_equal(total, 5)
  testthat::expect_equal(d$cell_exceedance_n_t, total)
  testthat::expect_equal(
    sum(out$country$exceedance_n_t) + d$unallocated_exceedance_n_t,
    total
  )
  testthat::expect_equal(d$exceedance_gap_n_t, 0, tolerance = 1e-12)

  # A grid with its residual rows removed no longer reconciles: refused.
  crops <- dplyr::filter(
    grid,
    .data$attribution_record_type == "crop_allocation"
  )
  testthat::expect_error(
    whep::build_n_boundary_country(crops, surplus, .nbc_ag_land()),
    class = "whep_nbc_unreconciled"
  )
})

testthat::test_that("the world excess share stays in [0, 1] under the clamp", {
  # Random grids with the balance's own sign structure: inputs are
  # non-negative and the surplus is the inputs minus a non-negative removal,
  # so it never exceeds them. The allowance is random and often negative.
  # Cell overshoot <= max(surplus, 0) <= inputs holds once clamped; kept, a
  # negative allowance lifts the overshoot above the surplus (the explicit
  # fixture above pushes it past the inputs).
  shares <- purrr::map(1:40, \(seed) {
    withr::with_seed(seed, {
      n <- 24L
      cells <- sample(-179.75 + 0.5 * (0:59), n)
      rows <- tibble::tibble(
        lon = rep(cells, each = 2L),
        lat = 0.25,
        area_code = sample(1:5, 2L * n, replace = TRUE),
        item_cbs_code = rep(c(2511L, 2513L), times = n),
        year = 2015L,
        area_ha = 100,
        n_input_std_t = stats::runif(2L * n, 0, 100)
      ) |>
        dplyr::mutate(
          surplus_n_t = .data$n_input_std_t * stats::runif(2L * n, -0.5, 1)
        )
      critical <- tibble::tibble(
        lon = cells,
        lat = 0.25,
        value = stats::runif(n, -400, 300),
        source_area_ha = 100,
        image_region = 11L,
        critical_var = "critical_n_surplus",
        critical_land_use = "all",
        critical_threshold = "mi",
        critical_year = 2010L
      )
      ag <- .nbc_ag_land(1:5)
      run <- \(nc) {
        grid <- .nbc_grid(rows, critical, nc)
        whep::build_n_boundary_country(grid, rows, ag)$diagnostics
      }
      list(clamp = run("clamp"), keep = run("keep"))
    })
  })
  clamp <- purrr::map_dbl(shares, \(s) s$clamp$excess_share_of_inputs)
  keep <- purrr::map_dbl(shares, \(s) s$keep$excess_share_of_inputs)
  testthat::expect_true(all(clamp >= 0 & clamp <= 1))
  # The world exceedance over the positive surplus is bounded the same way
  # (a clamped unit overshoot is at most its positive surplus), and the
  # clamped runs are not flagged.
  positive <- purrr::map_dbl(
    shares,
    \(s) s$clamp$exceedance_share_of_positive_surplus
  )
  testthat::expect_true(all(positive >= 0 & positive <= 1))
  testthat::expect_false(any(purrr::map_lgl(
    shares,
    \(s) s$clamp$world_ratio_outside_unit
  )))
  # The property is not vacuous: the shares are well above zero, and kept
  # negative allowances raise the share in every one of these grids.
  testthat::expect_true(all(clamp > 0.05))
  testthat::expect_true(all(keep > clamp))
  gaps <- purrr::map_dbl(shares, \(s) abs(s$clamp$exceedance_gap_n_t))
  testthat::expect_true(all(gaps < 1e-8))
})

testthat::test_that("inputs outside valid cells fall outside both terms", {
  # Cell C has no critical value: its 6 t of inputs are neither in the
  # exceedance nor in the headline denominator, and the valid fraction says so.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          3,            4,
    "C",   1L,         2511L,          5,            6
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 100)
  out <- .nbc_run(rows, rates)
  testthat::expect_equal(out$country$input_std_n_t, 4)
  testthat::expect_equal(out$country$positive_surplus_n_t, 3)
  testthat::expect_equal(out$diagnostics$input_std_n_t, 4)
  testthat::expect_equal(out$diagnostics$all_input_n_t, 10)
  testthat::expect_equal(out$diagnostics$valid_input_fraction, 0.4)
})

testthat::test_that("the intensive/extensive split compares only valid rows", {
  surplus <- .gs_surplus()
  grid <- suppressMessages(.gs_run("grid", grassland = .gs_grassland()))
  cell <- suppressMessages(.gs_run("cell", grassland = .gs_grassland()))
  ag <- tibble::tibble(area_code = 1:2, year = 2015L, area_ha = 1000)
  out <- whep::build_n_boundary_country(grid, surplus, ag)

  # The headline denominator is the input of the rows the exceedance sums
  # over, formed here from the grid keys independently of the function.
  crops <- dplyr::filter(
    grid,
    .data$attribution_record_type == "crop_allocation"
  )
  keyed <- dplyr::inner_join(
    dplyr::distinct(
      crops,
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_cbs_code,
      .data$year
    ),
    surplus,
    by = c("lon", "lat", "area_code", "item_cbs_code", "year")
  )
  expected <- dplyr::summarise(
    keyed,
    input = sum(.data$n_input_std_t),
    .by = "area_code"
  )
  testthat::expect_equal(
    dplyr::arrange(out$country, .data$area_code)$input_std_n_t,
    dplyr::arrange(expected, .data$area_code)$input
  )
  # Some fixture pressure sits in components the comparison leaves out, so
  # the valid fraction is below one.
  testthat::expect_lt(out$diagnostics$valid_input_fraction, 1)
  testthat::expect_equal(
    out$diagnostics$all_input_n_t,
    sum(surplus$n_input_std_t)
  )

  # Positive component surplus, once, from the cell result's own component
  # columns (never attributed to crops); the exceedance reconciles to the
  # summed cell overshoot including the rowless components.
  valid <- dplyr::filter(cell, .data$coverage_state == "valid")
  compared <- c("valid", "empty")
  use_managed <- valid$managed_coverage_state %in% compared
  use_extensive <- valid$extensive_coverage_state %in% compared
  positive <- sum(pmax(valid$managed_actual_n_t[use_managed], 0)) +
    sum(pmax(valid$extensive_actual_n_t[use_extensive], 0))
  testthat::expect_equal(out$diagnostics$positive_surplus_n_t, positive)
  exceeding <- sum(
    valid$managed_actual_n_t[
      use_managed &
        valid$managed_actual_n_t > 0 &
        valid$managed_positive_overshoot_n_t > 0
    ]
  ) +
    sum(
      valid$extensive_actual_n_t[
        use_extensive &
          valid$extensive_actual_n_t > 0 &
          valid$extensive_positive_overshoot_n_t > 0
      ]
    )
  testthat::expect_equal(sum(out$country$exceeding_surplus_n_t), exceeding)
  testthat::expect_equal(
    out$diagnostics$cell_exceedance_n_t,
    sum(valid$cell_positive_overshoot_n_t)
  )
  testthat::expect_equal(
    out$diagnostics$exceedance_gap_n_t,
    0,
    tolerance = 1e-12
  )
  testthat::expect_equal(unique(out$country$grassland_split), "image_density")
})

testthat::test_that("split components never net: managed beside extensive", {
  # Cell B (country 1): cropland is managed (+15 t against a 5 t allowance,
  # 10 t over) and its extensive grassland has a 6 t allowance. The extensive
  # surplus x takes four values; the managed excess is counted in every one.
  b <- dplyr::filter(.gs_surplus(), .data$lon == 0.75)
  ag <- tibble::tibble(area_code = 1L, year = 2015L, area_ha = 1000)
  run <- \(x) {
    surplus <- dplyr::mutate(
      b,
      surplus_n_t = dplyr::if_else(.data$item_cbs_code == 3000L, x, 15),
      n_input_std_t = pmax(.data$surplus_n_t, 0) + 1
    )
    grid <- suppressMessages(whep::build_n_boundary_exceedance(
      surplus = surplus,
      critical = .gs_critical(),
      land_use = "all",
      resolution = "grid",
      actual_year = 2015L,
      critical_reference_year = 2010L,
      grassland = .gs_grassland()
    ))
    list(
      country = whep::build_n_boundary_country(grid, surplus, ag),
      grid = grid
    )
  }
  # x = -8: the cell nets to +7 t, but the country's positive surplus is the
  # managed 15 t, not the 7 t a cell-level rule would take.
  net <- run(-8)
  testthat::expect_equal(unique(net$grid$cell_actual_n_t), 7)
  testthat::expect_equal(net$country$country$positive_surplus_n_t, 15)
  testthat::expect_equal(net$country$country$exceeding_surplus_n_t, 15)
  testthat::expect_equal(net$country$country$exceedance_n_t, 10)
  testthat::expect_equal(net$country$country$beyond_share, 1)

  # x = -30: the cell nets to -15 t and a cell-level rule would see no
  # positive surplus at all; the managed excess still counts.
  deficit <- run(-30)
  testthat::expect_equal(unique(deficit$grid$cell_actual_n_t), -15)
  testthat::expect_equal(deficit$country$country$positive_surplus_n_t, 15)
  testthat::expect_equal(deficit$country$country$exceeding_surplus_n_t, 15)
  testthat::expect_equal(deficit$country$country$exceedance_n_t, 10)
  testthat::expect_equal(deficit$country$diagnostics$exceedance_gap_n_t, 0)

  # x = +2: extensive headroom of 4 t; a positive surplus that does not exceed.
  under <- run(2)$country$country
  testthat::expect_equal(under$positive_surplus_n_t, 17)
  testthat::expect_equal(under$exceeding_surplus_n_t, 15)
  testthat::expect_equal(under$beyond_share, 15 / 17)

  # x = +9: 3 t over the extensive allowance, so both components exceed.
  over <- run(9)$country$country
  testthat::expect_equal(over$positive_surplus_n_t, 24)
  testthat::expect_equal(over$exceeding_surplus_n_t, 24)
  testthat::expect_equal(over$exceedance_n_t, 13)
})

testthat::test_that("undefined attribution keeps a unit's rows in the shares", {
  # Cell A: country 1 +5 t and country 2 -5 t sum to zero, so the crop shares
  # are undefined and the overshoot sits on a residual record; kept negative
  # allowance (-2 t) here. Cell B: countries 1 and 2 share a +6 t surplus
  # against a 2 t allowance. Membership follows the unit's state, not the
  # attributed exceedance, so cell B's rows count and cell A's do not (zero
  # surplus), and the diagnostics count the residual unit and the two rows.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          5,            6,
    "A",   2L,         2511L,          -5,           1,
    "B",   1L,         2511L,          4,            5,
    "B",   2L,         2511L,          2,            3
  )
  rates <- tibble::tribble(~cell, ~rate, "A", -20, "B", 20)
  keep <- .nbc_run(rows, rates, "keep")
  clamp <- .nbc_run(rows, rates, "clamp")

  d <- keep$diagnostics
  testthat::expect_equal(d$n_undefined_attribution_rows, 2L)
  testthat::expect_equal(d$n_unallocated_units, 1L)
  testthat::expect_equal(d$unallocated_exceedance_n_t, 2)
  # Kept, cell A overshoots by 2 t with a zero surplus: outside both shares.
  testthat::expect_equal(d$overshoot_without_surplus_n_t, 2)
  testthat::expect_equal(keep$country$positive_surplus_n_t, c(4, 2))
  testthat::expect_equal(keep$country$exceeding_surplus_n_t, c(4, 2))
  testthat::expect_equal(keep$country$beyond_share, c(1, 1))

  # Clamped, cell A has a zero allowance and a zero surplus: no overshoot, so
  # every exceeding unit has a positive surplus and nothing is left outside.
  testthat::expect_equal(clamp$diagnostics$overshoot_without_surplus_n_t, 0)
  testthat::expect_equal(clamp$diagnostics$n_unallocated_units, 0L)
  testthat::expect_equal(clamp$diagnostics$n_undefined_attribution_rows, 2L)
  testthat::expect_equal(clamp$country$exceeding_surplus_n_t, c(4, 2))
})

testthat::test_that("an ill-conditioned unit still counts in the shares", {
  # Country 1 +1 t and country 2 -(1 - 1e-12) t: the unit surplus is 1e-12 t,
  # positive but far below the rounding-safe fraction of the 2 t moved, so no
  # crop share is defined and the 1e-12 t overshoot (zero allowance) goes to a
  # residual record. Country 1's rows are still in a positive, exceeding unit,
  # so its share is one although its attributed exceedance is zero.
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          1,            2,
    "A",   2L,         2511L,          -1 + 1e-12,   1
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 0)
  out <- .nbc_run(rows, rates, "clamp")
  one <- .nbc_row(out, 1L)
  testthat::expect_equal(one$exceedance_n_t, 0)
  testthat::expect_equal(one$positive_surplus_n_t, 1)
  testthat::expect_equal(one$exceeding_surplus_n_t, 1)
  testthat::expect_equal(one$beyond_share, 1)
  testthat::expect_equal(one$boundary_side, "Exceedance")
  testthat::expect_equal(out$diagnostics$n_undefined_attribution_rows, 2L)
  testthat::expect_equal(out$diagnostics$n_unallocated_units, 1L)
  testthat::expect_equal(
    out$diagnostics$unallocated_exceedance_n_t,
    1e-12,
    tolerance = 1e-3
  )
})

testthat::test_that("the agricultural area is summed per country-year", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          3,            4,
    "B",   2L,         2511L,          3,            4
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 100, "B", 100)
  ag <- tibble::tribble(
    ~area_code, ~year, ~area_ha,
    1L,         2015L, 40,
    1L,         2015L, 60,
    1L,         2016L, 999
  )
  out <- .nbc_run(rows, rates, ag_land = ag)
  testthat::expect_equal(.nbc_row(out, 1L)$ag_area_ha, 100)
  # A country-year with no land row is NA, not zero.
  testthat::expect_true(is.na(.nbc_row(out, 2L)$ag_area_ha))
  testthat::expect_equal(out$diagnostics$n_missing_ag_area, 1L)
  testthat::expect_error(
    .nbc_run(rows, rates, ag_land = dplyr::mutate(ag, area_code = 9L)),
    class = "whep_nbc_no_ag_land"
  )
  testthat::expect_error(
    .nbc_run(rows, rates, ag_land = dplyr::mutate(ag, area_ha = 0)),
    class = "whep_absent_input"
  )
})

testthat::test_that("the classification uses the levels of sjos_levels", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          10,           12,
    "B",   2L,         2511L,          10,           12
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 40, "B", 200)
  nourishment <- tibble::tribble(
    ~year, ~area_code, ~nourish,
    2015L, 1L,         "Adequate",
    2015L, 2L,         "Over"
  )
  out <- .nbc_run(rows, rates, nourishment = nourishment)$country
  testthat::expect_s3_class(out$sjos_class, "factor")
  testthat::expect_equal(levels(out$sjos_class), whep::sjos_levels$level)
  testthat::expect_equal(
    as.character(out$sjos_class),
    c("Exceedance Adequate", "Within_boundary Over")
  )
  # The labels are the ones classify_sjos_n() gives its per-crop side.
  crop <- whep::classify_sjos_n(
    whep::build_n_boundary_exceedance(
      .nbc_surplus(rows),
      .nbc_critical(rates),
      land_use = "all",
      resolution = "country",
      actual_year = 2015L,
      critical_reference_year = 2010L,
      grassland_split = "none"
    ),
    nourishment
  )
  testthat::expect_equal(
    dplyr::arrange(crop, .data$area_code)$boundary_side,
    out$boundary_side
  )
  testthat::expect_false(rlang::has_name(
    .nbc_run(rows, rates)$country,
    "sjos_class"
  ))
})

testthat::test_that("the country table refuses inputs it cannot read faithfully", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",   1L,         2511L,          3,            4
  )
  rates <- tibble::tribble(~cell, ~rate, "A", 100)
  surplus <- .nbc_surplus(rows)
  critical <- .nbc_critical(rates)
  grid <- .nbc_grid(surplus, critical)
  ag <- .nbc_ag_land()

  # A cell result has no crop rows.
  testthat::expect_error(
    whep::build_n_boundary_country(
      .nbc_grid(surplus, critical, resolution = "cell"),
      surplus,
      ag
    ),
    "Missing columns"
  )
  # A surplus that is not the one the grid came from.
  testthat::expect_error(
    whep::build_n_boundary_country(
      grid,
      dplyr::mutate(surplus, area_code = 7L),
      ag
    ),
    class = "whep_nbc_missing_input"
  )
  testthat::expect_error(
    whep::build_n_boundary_country(
      grid,
      dplyr::bind_rows(surplus, surplus),
      ag
    ),
    class = "whep_nbc_duplicate_surplus"
  )
  # An input-mode grid, and a grid of mixed clamp settings.
  testthat::expect_error(
    whep::build_n_boundary_country(
      dplyr::mutate(grid, metric = "input"),
      surplus,
      ag
    ),
    class = "whep_nbc_metric"
  )
  mixed <- dplyr::bind_rows(
    grid,
    dplyr::mutate(grid, negative_critical = "clamp")
  )
  testthat::expect_error(
    whep::build_n_boundary_country(mixed, surplus, ag),
    class = "whep_nbc_mixed_runs"
  )
  testthat::expect_error(
    whep::build_n_boundary_country(grid, surplus, ag, beyond_share_cut = 1),
    "beyond_share_cut"
  )
  testthat::expect_error(
    whep::build_n_boundary_country(
      grid,
      surplus,
      ag,
      nourishment = tibble::tibble(
        year = 2015L,
        area_code = 1L,
        nourish = c("Under", "Over")
      )
    ),
    class = "whep_nbc_duplicate_nourishment"
  )
})

testthat::test_that("the country table example runs and is coherent", {
  out <- whep::build_n_boundary_country(example = TRUE)
  testthat::expect_named(out, c("country", "diagnostics"))
  testthat::expect_equal(
    out$country$boundary_side,
    c("Exceedance", "Within_boundary")
  )
  testthat::expect_equal(out$country$exceedance_n_t, c(6, 4))
  testthat::expect_equal(out$diagnostics$exceedance_gap_n_t, 0)
})

# build_nitrogen_balance() splits each grid row into a rainfed and an
# irrigated part (whep#1233), so calculate_n_surplus() returns two rows per
# cell, crop and year. build_n_boundary_exceedance() sums them back to one
# before comparing a cell with its critical surplus; the country table must
# read the surplus the same way, or it would refuse the default balance.
.nbc_split_regimes <- function(surplus, irrigated_share = 0.3) {
  additive <- c("surplus_n_t", "n_input_std_t", "area_ha")
  dplyr::bind_rows(
    surplus |>
      dplyr::mutate(
        water_regime = "rainfed",
        dplyr::across(dplyr::all_of(additive), \(v) v * (1 - irrigated_share))
      ),
    surplus |>
      dplyr::mutate(
        water_regime = "irrigated",
        dplyr::across(dplyr::all_of(additive), \(v) v * irrigated_share)
      )
  )
}

testthat::test_that("a rainfed/irrigated split surplus gives the unsplit table", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",            1L,          2511L,           10,             20,
    "B",            1L,          2511L,           -8,              5,
    "C",            2L,          2513L,            6,             12
  )
  critical <- .nbc_critical(tibble::tribble(
    ~cell, ~rate,
    "A",      20,
    "B",      20,
    "C",      20
  ))
  unsplit <- .nbc_surplus(rows)
  split <- .nbc_split_regimes(unsplit)
  expected <- whep::build_n_boundary_country(
    .nbc_grid(unsplit, critical),
    unsplit,
    .nbc_ag_land()
  )
  out <- whep::build_n_boundary_country(
    .nbc_grid(split, critical),
    split,
    .nbc_ag_land()
  )
  testthat::expect_equal(out$country, expected$country)
  testthat::expect_equal(out$diagnostics, expected$diagnostics)
  testthat::expect_equal(sum(out$country$input_std_n_t), 37)
})

testthat::test_that("a duplicate within one water regime is still refused", {
  rows <- tibble::tribble(
    ~cell, ~area_code, ~item_cbs_code, ~surplus_n_t, ~n_input_std_t,
    "A",            1L,          2511L,           10,             20
  )
  critical <- .nbc_critical(tibble::tribble(~cell, ~rate, "A", 20))
  split <- .nbc_split_regimes(.nbc_surplus(rows))
  doubled <- dplyr::bind_rows(
    split,
    dplyr::filter(split, .data$water_regime == "rainfed")
  )
  testthat::expect_error(
    whep::build_n_boundary_country(
      .nbc_grid(split, critical),
      doubled,
      .nbc_ag_land(1L)
    ),
    class = "whep_nbc_duplicate_surplus"
  )
})
