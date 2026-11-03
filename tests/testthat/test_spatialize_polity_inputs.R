.polity_input_support <- function() {
  tibble::tribble(
    ~lon, ~lat, ~polity_code, ~start_year, ~end_year, ~cell_area_ha, ~land_area_ha,
    0.25, 0.25, "A", 1900L, 1950L, 100, 25,
    0.25, 0.25, "B", 1900L, 1950L, 100, 75,
    1.25, 0.25, "A", 1900L, 1950L, 100, 100,
    0.25, 0.25, "C", 1950L, 2000L, 100, 100,
    1.25, 0.25, "C", 1950L, 2000L, 100, 100
  )
}

.polity_input_cells <- function() {
  tibble::tribble(
    ~year, ~lon, ~lat, ~luh2_type, ~crop_ha, ~irrig_ha,
    1949L, 0.25, 0.25, "c3ann", 80, 40,
    1949L, 1.25, 0.25, "c3ann", 20, 0,
    1949L, 0.25, 0.25, "c4ann", 0, 0,
    1949L, 1.25, 0.25, "c4ann", 0, 0
  )
}

.polity_input_mirca <- function() {
  tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~irrig_ha, ~rainfed_ha,
    0.25, 0.25, 15L, 40, 60,
    1.25, 0.25, 15L, 0, 10
  )
}

testthat::test_that("LUH2 totals weight complete support before selection", {
  cells <- .polity_input_cells()
  support <- .polity_input_support()
  all <- whep:::.polity_luh2_totals(cells, support, c("c3ann", "c4ann"))
  one <- whep:::.polity_luh2_totals(cells, support, c("c3ann", "c4ann"), "A")
  testthat::expect_equal(one, dplyr::filter(all, .data$polity_code == "A"))
  testthat::expect_equal(one$crop_ha, c(40, 0))
  testthat::expect_equal(one$irrig_ha, c(10, 0))
  testthat::expect_equal(sum(all$crop_ha), sum(cells$crop_ha))
  testthat::expect_equal(sum(all$irrig_ha), sum(cells$irrig_ha))
  testthat::expect_false("area_code" %in% names(all))
})

testthat::test_that("MIRCA ratios use weighted sums and patterns stay physical", {
  patterns <- .polity_input_mirca()
  out <- whep:::.polity_mirca_totals(patterns, .polity_input_support(), 1949L)
  testthat::expect_identical(out$patterns, patterns)
  a <- dplyr::filter(out$totals, .data$polity_code == "A")
  testthat::expect_equal(a$irrig_ha, 10)
  testthat::expect_equal(a$rainfed_ha, 25)
  testthat::expect_equal(a$irrig_frac, 10 / 35)
  testthat::expect_equal(sum(out$totals$irrig_ha), 40)
  one <- whep:::.polity_mirca_totals(
    patterns,
    .polity_input_support(),
    1949L,
    "A"
  )
  testthat::expect_equal(one$totals, a)
  zero <- dplyr::mutate(patterns, irrig_ha = 0, rainfed_ha = 0)
  testthat::expect_true(all(is.na(
    whep:::.polity_mirca_totals(
      zero,
      .polity_input_support(),
      1949L
    )$totals$irrig_frac
  )))
})

testthat::test_that("physical preparations respect half-open support periods", {
  out <- whep:::.polity_mirca_totals(
    .polity_input_mirca(),
    .polity_input_support(),
    c(1949L, 1950L)
  )$totals
  testthat::expect_setequal(out$polity_code[out$year == 1950], "C")
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      .polity_input_mirca(),
      .polity_input_support(),
      2000L
    ),
    class = "whep_polity_input_support"
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      .polity_input_mirca(),
      .polity_input_support(),
      1949.5
    ),
    class = "whep_polity_input_year"
  )
})

testthat::test_that("missing classes differ from explicitly supplied zero", {
  cells <- .polity_input_cells()
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      dplyr::filter(cells, .data$luh2_type != "c4ann"),
      .polity_input_support(),
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_class"
  )
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      cells[-4, ],
      .polity_input_support(),
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_class"
  )
})

testthat::test_that("invalid measurements and duplicate physical rows fail", {
  cells <- .polity_input_cells()
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      dplyr::bind_rows(cells, cells[1, ]),
      .polity_input_support(),
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_duplicate"
  )
  for (bad in c(-1, NA_real_, Inf)) {
    invalid <- cells
    invalid$crop_ha[1] <- bad
    testthat::expect_error(
      whep:::.polity_luh2_totals(
        invalid,
        .polity_input_support(),
        c("c3ann", "c4ann")
      ),
      class = "whep_polity_input_measure"
    )
  }
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      dplyr::bind_rows(.polity_input_mirca(), .polity_input_mirca()),
      .polity_input_support(),
      1949L
    ),
    class = "whep_polity_input_duplicate"
  )
})

testthat::test_that("missing spatial support and missing physical cells fail", {
  patterns <- .polity_input_mirca()
  patterns$lon[1] <- 10.25
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      patterns,
      .polity_input_support(),
      1949L
    ),
    class = "whep_polity_input_support"
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      .polity_input_mirca()[1, ],
      .polity_input_support(),
      1949L
    ),
    class = "whep_polity_input_support"
  )
})

testthat::test_that("physical preparation rejects malformed input schemas", {
  cells <- .polity_input_cells()
  support <- .polity_input_support()
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      cells[0, ],
      support,
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_measure"
  )
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      dplyr::mutate(cells, irrig_ha = crop_ha + 1),
      support,
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_measure"
  )
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      dplyr::mutate(cells, polity_code = "A"),
      support,
      c("c3ann", "c4ann")
    ),
    class = "whep_polity_input_schema"
  )
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      cells,
      support,
      c("c3ann", "c4ann"),
      "UNKNOWN"
    ),
    class = "whep_polity_input_support"
  )
  testthat::expect_error(
    whep:::.polity_luh2_totals(
      cells,
      support,
      c("c3ann", NA_character_)
    ),
    class = "whep_polity_input_class"
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      .polity_input_mirca(),
      support,
      NA_real_
    ),
    class = "whep_polity_input_year"
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      .polity_input_mirca(),
      support,
      numeric()
    ),
    class = "whep_polity_input_year"
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      dplyr::mutate(.polity_input_mirca(), rainfed_ha = -1),
      support,
      1949L
    ),
    class = "whep_polity_input_measure"
  )
  missing_crop <- dplyr::bind_rows(
    .polity_input_mirca(),
    dplyr::mutate(.polity_input_mirca()[1, ], item_prod_code = 56L)
  )
  testthat::expect_error(
    whep:::.polity_mirca_totals(
      missing_crop,
      support,
      1949L
    ),
    class = "whep_polity_input_class"
  )
})
