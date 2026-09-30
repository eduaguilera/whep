# One country grazes cattle (dairy) and sheep, producing milk, bovine
# meat and mutton. Cattle eat 75% of the grass, sheep 25%.
.grazing_inputs <- function() {
  list(
    grass_land = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      value = 100
    ),
    grazer_intake = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = c(960L, 976L),
      value = c(75, 25)
    ),
    livestock_production = tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = c(960L, 960L, 976L, 976L),
      item_cbs_code = c(2848L, 2731L, 2732L, 2748L),
      value = c(90, 10, 5, 1)
    )
  )
}

testthat::test_that("forward allocation conserves all grazing land", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production
  )

  out |>
    pointblank::expect_col_exists(
      c("year", "area_code", "item_cbs_code", "value", "method_allocation")
    )
  testthat::expect_equal(sum(out$value), 100)
  testthat::expect_true(all(out$method_allocation == "all"))
})

testthat::test_that("intake share drives the split across animals", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production
  )

  # Cattle ate 75% -> 75 ha across milk (90 t) and bovine meat (10 t):
  # milk 67.5, bovine meat 7.5. Sheep ate 25% -> 25 ha across mutton (5 t)
  # and hides (1 t): mutton 20.83, hides 4.17.
  by_item <- out |> dplyr::arrange(item_cbs_code)
  testthat::expect_equal(
    by_item$value,
    c(7.5, 20.833333, 4.166667, 67.5),
    tolerance = 1e-5
  )
})

testthat::test_that("meat_milk keeps all land on meat and dairy items", {
  inp <- .grazing_inputs()
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    inp$livestock_production,
    products = "meat_milk"
  )

  # Hides (2748) are excluded; sheep's 25 ha now all lands on mutton.
  testthat::expect_false(2748L %in% out$item_cbs_code)
  testthat::expect_equal(sum(out$value), 100)
  mutton <- out |> dplyr::filter(item_cbs_code == 2732L)
  testthat::expect_equal(mutton$value, 25)
})

testthat::test_that("animals that did not graze receive no land", {
  inp <- .grazing_inputs()
  # Add a granivore (broilers, 1053) producing poultry meat but absent
  # from grass intake: it must not receive grazing land.
  prod <- dplyr::bind_rows(
    inp$livestock_production,
    tibble::tibble(
      year = 2010L,
      area_code = 1L,
      live_anim_code = 1053L,
      item_cbs_code = 2734L,
      value = 500
    )
  )
  out <- whep::allocate_grazing_to_products(
    inp$grass_land,
    inp$grazer_intake,
    prod
  )

  testthat::expect_false(2734L %in% out$item_cbs_code)
  testthat::expect_equal(sum(out$value), 100)
})

testthat::test_that("intake without eligible output is surfaced not dropped", {
  grass_land <- tibble::tibble(year = 2010L, area_code = 1L, value = 100)
  # Cattle graze 75% of the land and produce milk; sheep graze the other
  # 25% but produce no eligible output at all. The second-stage join must
  # not silently drop the sheep's grazing land.
  grazer_intake <- tibble::tibble(
    year = 2010L,
    area_code = 1L,
    live_anim_code = c(960L, 976L),
    value = c(75, 25)
  )
  production <- tibble::tibble(
    year = 2010L,
    area_code = 1L,
    live_anim_code = 960L,
    item_cbs_code = 2848L,
    value = 90
  )
  testthat::expect_warning(
    out <- whep::allocate_grazing_to_products(
      grass_land,
      grazer_intake,
      production
    ),
    "25% of grazing land"
  )
  # The 25 ha is surfaced by the warning, never attributed to cattle milk
  # and never left as an NA output item.
  testthat::expect_false(anyNA(out$item_cbs_code))
  testthat::expect_equal(sum(out$value), 75)
})

testthat::test_that("full output coverage stays silent", {
  inp <- .grazing_inputs()
  testthat::expect_no_warning(
    whep::allocate_grazing_to_products(
      inp$grass_land,
      inp$grazer_intake,
      inp$livestock_production
    )
  )
})

testthat::test_that("forward footprint routes grazing land to meat consumers", {
  grass_land <- tibble::tibble(year = 2010L, area_code = 10L, value = 200)
  grazer_intake <- tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    value = 100
  )
  production <- tibble::tibble(
    year = 2010L,
    area_code = c(10L, 41L),
    live_anim_code = c(961L, 961L),
    item_cbs_code = c(2731L, 2731L),
    value = c(100, 0)
  )
  # Country 10 exports 40% of its bovine meat to country 41.
  trade <- tibble::tibble(
    from_code = 10L,
    to_code = 41L,
    item_cbs_code = 2731L,
    value = 40
  )

  fp <- whep::build_grazing_feed_footprint(
    year = 2010L,
    data = list(
      grass_land = grass_land,
      grazer_intake = grazer_intake,
      livestock_production = production,
      trade = trade
    )
  )

  fp |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  by_area <- fp |> dplyr::arrange(area_code)
  # 60% of 200 ha stays with producer 10, 40% follows the meat to 41.
  testthat::expect_equal(by_area$value, c(120, 80))
  testthat::expect_equal(sum(fp$value), 200)
  testthat::expect_true(all(fp$method == "grazing_feed_allocation"))
})

testthat::test_that("missing columns are reported", {
  testthat::expect_error(
    whep::allocate_grazing_to_products(
      tibble::tibble(area_code = 1L, value = 1),
      tibble::tibble(
        year = 1L,
        area_code = 1L,
        live_anim_code = 1L,
        value = 1
      ),
      tibble::tibble(
        year = 1L,
        area_code = 1L,
        live_anim_code = 1L,
        item_cbs_code = 1L,
        value = 1
      )
    ),
    "grass_land"
  )
})

testthat::test_that("grazing land with no grazer intake is surfaced", {
  grass_land <- tibble::tibble(
    year = 2010L,
    area_code = c(10L, 99L),
    value = c(100, 50)
  )
  # Area 99 has grassland but no grazer intake -> 1/3 unattributable.
  grazer_intake <- tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    value = 80
  )
  testthat::expect_warning(
    whep:::.warn_unattributed_land(grass_land, grazer_intake),
    "33.3% of grazing land"
  )
  # Full coverage must stay silent.
  testthat::expect_no_warning(
    whep:::.warn_unattributed_land(
      dplyr::filter(grass_land, area_code == 10L),
      grazer_intake
    )
  )
})

testthat::test_that("example output is a tidy tibble", {
  ex <- whep::build_grazing_feed_footprint(example = TRUE)
  ex |>
    pointblank::expect_col_exists(
      c("area_code", "item_cbs_code", "value", "method")
    )
  testthat::expect_true(all(ex$method == "grazing_feed_allocation"))
  testthat::expect_true(all(ex$value > 0))
})

# ---- whep#1034: a moved feed-type or unit label ------------------------------

.grazing_raw_intake <- function(grass = "grass") {
  tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    feed_type = c(grass, "residues", "crops"),
    intake_dry_matter = c(60, 40, 25)
  )
}

.grazing_raw_production <- function(unit = "tonnes") {
  tibble::tibble(
    year = 2010L,
    area_code = 10L,
    live_anim_code = 961L,
    item_cbs_code = 2731L,
    unit = c(unit, "heads"),
    value = c(100, 50)
  )
}

# Grass land and trade are supplied; intake and production go through the
# package's own raw-input builders, reading the mocked tables.
.grazing_footprint_from <- function(intake, production) {
  testthat::with_mocked_bindings(
    whep::build_grazing_feed_footprint(
      year = 2010L,
      data = list(
        grass_land = tibble::tibble(year = 2010L, area_code = 10L, value = 200),
        trade = tibble::tibble(
          from_code = 10L,
          to_code = 41L,
          item_cbs_code = 2731L,
          value = 40
        )
      )
    ),
    get_feed_intake = function(...) intake,
    get_primary_production = function(...) production
  )
}

testthat::test_that("the raw-input builders price a keyed input", {
  fp <- .grazing_footprint_from(
    .grazing_raw_intake(),
    .grazing_raw_production()
  )
  testthat::expect_equal(sum(fp$value), 200)
})

testthat::test_that("a moved grass label cannot ship as a grazing footprint", {
  intake <- .grazing_raw_intake(grass = "Grass")
  unguarded <- testthat::with_mocked_bindings(
    .grazing_footprint_from(intake, .grazing_raw_production()),
    check_labels_supplied = function(data, ...) invisible(data)
  )
  # Unguarded, the residues alone carry the whole 200 ha: the land is conserved
  # and the footprint looks complete while grass never entered it.
  expect_supplied_guard(
    identity = isTRUE(all.equal(sum(unguarded$value), 200)) &&
      all(unguarded$method == "grazing_feed_allocation"),
    guard = .grazing_footprint_from(intake, .grazing_raw_production()),
    class = "whep_absent_label"
  )
})

testthat::test_that("a moved output unit cannot ship as an empty footprint", {
  production <- .grazing_raw_production(unit = "t")
  unguarded <- testthat::with_mocked_bindings(
    suppressWarnings(
      .grazing_footprint_from(.grazing_raw_intake(), production)
    ),
    check_labels_supplied = function(data, ...) invisible(data)
  )
  expect_supplied_guard(
    identity = sum(unguarded$value) == 0 && all(unguarded$value >= 0),
    guard = .grazing_footprint_from(.grazing_raw_intake(), production),
    class = "whep_absent_label"
  )
})
