.absent_fixture <- function(water = c(0, 0), ice = c(0, 0)) {
  tibble::tibble(
    land_area_ha = c(700, 300),
    inland_water_ha = water,
    ice_area_ha = ice
  ) |>
    dplyr::mutate(
      polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha
    )
}

.absent_layers <- c(land = "land_area_ha", water = "inland_water_ha")

testthat::test_that("a supplied input passes", {
  supplied <- .absent_fixture(water = c(20, 5), ice = c(0, 1))
  testthat::expect_identical(
    whep::check_inputs_supplied(
      supplied,
      c("land_area_ha", "inland_water_ha", "ice_area_ha")
    ),
    supplied
  )
})

testthat::test_that("the identity alone cannot see a zero-filled input", {
  support <- .absent_fixture()
  expect_supplied_guard(
    identity = isTRUE(all.equal(
      support$polity_area_ha,
      support$land_area_ha + support$inland_water_ha + support$ice_area_ha
    )),
    guard = whep::check_inputs_supplied(support, .absent_layers)
  )
})

testthat::test_that("the absent keys reach the condition", {
  support <- .absent_fixture()
  condition <- tryCatch(
    whep::check_inputs_supplied(
      support,
      c(land = "land_area_ha", water = "inland_water_ha", ice = "ice_area_ha")
    ),
    whep_absent_input = function(e) e
  )
  testthat::expect_identical(condition$absent, c("water", "ice"))
  testthat::expect_match(conditionMessage(condition), "water.*ice")
})

testthat::test_that("an all-missing column is absent, not merely missing", {
  support <- .absent_fixture(water = c(NA_real_, NA_real_))
  testthat::expect_error(
    whep::check_inputs_supplied(support, .absent_layers),
    class = "whep_absent_input"
  )
})

testthat::test_that("a column that is not there at all is absent", {
  support <- .absent_fixture(water = c(20, 5)) |>
    dplyr::select(-"inland_water_ha")
  condition <- tryCatch(
    whep::check_inputs_supplied(support, .absent_layers),
    whep_absent_input = function(e) e
  )
  testthat::expect_identical(condition$absent, "water")
})

testthat::test_that("the check is scale-free, not a magnitude floor", {
  tiny <- .absent_fixture(water = c(1e-9, 0))
  testthat::expect_no_error(
    whep::check_inputs_supplied(tiny, .absent_layers)
  )
})

testthat::test_that("zero rows is a filter that matched nothing", {
  empty <- .absent_fixture()[0, ]
  testthat::expect_no_error(
    whep::check_inputs_supplied(empty, .absent_layers)
  )
  testthat::expect_error(
    whep::check_inputs_supplied(empty, c(ice = "no_such_column")),
    class = "whep_absent_input"
  )
})

testthat::test_that("warn returns the data and keeps the caller running", {
  support <- .absent_fixture()
  testthat::expect_warning(
    out <- whep::check_inputs_supplied(
      support,
      .absent_layers,
      action = "warn"
    ),
    class = "whep_absent_input"
  )
  testthat::expect_identical(out, support)
})

testthat::test_that("caller details reach the message", {
  support <- .absent_fixture()
  testthat::expect_error(
    whep::check_inputs_supplied(
      support,
      .absent_layers,
      details = c(i = "Source: the polycell_support pin.")
    ),
    "polycell_support pin"
  )
})

testthat::test_that("the stamp overrides a legitimately zero column", {
  support <- .absent_fixture() |>
    whep::stamp_inputs_supplied(c("water", "land"))
  testthat::expect_no_error(
    whep::check_inputs_supplied(
      support,
      .absent_layers,
      stamp = "inputs_supplied"
    )
  )
})

testthat::test_that("the stamp refuses a non-zero column it does not name", {
  support <- .absent_fixture(water = c(20, 5)) |>
    whep::stamp_inputs_supplied("land")
  condition <- tryCatch(
    whep::check_inputs_supplied(
      support,
      .absent_layers,
      stamp = "inputs_supplied"
    ),
    whep_absent_input = function(e) e
  )
  testthat::expect_identical(condition$absent, "water")
})

testthat::test_that("an unstamped table falls back to its columns", {
  support <- .absent_fixture()
  testthat::expect_error(
    whep::check_inputs_supplied(
      support,
      .absent_layers,
      stamp = "inputs_supplied"
    ),
    class = "whep_absent_input"
  )
})

testthat::test_that("stamp_inputs_supplied sorts, dedupes and names none", {
  data <- tibble::tibble(x = 1:2)
  testthat::expect_identical(
    whep::stamp_inputs_supplied(
      data,
      c("water", "ice", "water")
    )$inputs_supplied,
    c("ice,water", "ice,water")
  )
  testthat::expect_identical(
    unique(whep::stamp_inputs_supplied(data, character())$inputs_supplied),
    "none"
  )
  testthat::expect_error(
    whep::stamp_inputs_supplied(data, 1),
    "must be a character vector"
  )
})

testthat::test_that("required must be a non-empty character vector", {
  testthat::expect_error(
    whep::check_inputs_supplied(tibble::tibble(x = 1), character()),
    "non-empty character vector"
  )
})

testthat::test_that("a character input is vacuous when every label is empty", {
  labelled <- tibble::tibble(source = c("", ""))
  testthat::expect_error(
    whep::check_inputs_supplied(labelled, "source"),
    class = "whep_absent_input"
  )
  testthat::expect_no_error(
    whep::check_inputs_supplied(
      tibble::tibble(source = c("", "FAOSTAT")),
      "source"
    )
  )
})

testthat::test_that("an all-FALSE flag is an observation, not an absence", {
  flags <- tibble::tibble(irrigated = c(FALSE, FALSE))
  testthat::expect_no_error(whep::check_inputs_supplied(flags, "irrigated"))
})

testthat::test_that("a present label passes and returns the data", {
  landuse <- tibble::tibble(Element = c("Area", "Yield"), Value = c(1, 2))
  testthat::expect_identical(
    whep::check_labels_supplied(landuse, "Element", "Area"),
    landuse
  )
})

testthat::test_that("a renamed label is refused and the new one is shown", {
  landuse <- tibble::tibble(Element = "Area under cultivation", Value = 1)
  condition <- tryCatch(
    whep::check_labels_supplied(landuse, "Element", c("Area", "Yield")),
    whep_absent_label = function(e) e
  )
  testthat::expect_identical(condition$absent, c("Area", "Yield"))
  testthat::expect_identical(condition$observed, "Area under cultivation")
  testthat::expect_match(conditionMessage(condition), "Area under cultivation")
})

testthat::test_that("a numeric code is compared as a label", {
  landuse <- tibble::tibble(`Item Code` = c(6620, 6621))
  testthat::expect_error(
    whep::check_labels_supplied(landuse, "Item Code", 6655),
    class = "whep_absent_label"
  )
  testthat::expect_no_error(
    whep::check_labels_supplied(landuse, "Item Code", 6620)
  )
})

testthat::test_that("a column that is not there is the whole vocabulary", {
  condition <- tryCatch(
    whep::check_labels_supplied(tibble::tibble(x = 1), "Element", "Area"),
    whep_absent_label = function(e) e
  )
  testthat::expect_identical(condition$observed, character())
  testthat::expect_match(conditionMessage(condition), "not a column")
})

testthat::test_that("the observed vocabulary is truncated, and says so", {
  wide <- tibble::tibble(Element = paste0("e", 1:9))
  testthat::expect_error(
    whep::check_labels_supplied(wide, "Element", "Area"),
    "3 other"
  )
})

testthat::test_that("a table with no rows carries no vocabulary", {
  empty <- tibble::tibble(Element = character(), Value = numeric())
  testthat::expect_no_error(
    whep::check_labels_supplied(empty, "Element", "Area")
  )
  # A column that is not there is still absent, rows or no rows.
  testthat::expect_error(
    whep::check_labels_supplied(
      tibble::tibble(Value = numeric()),
      "Element",
      "Area"
    ),
    class = "whep_absent_label"
  )
})

testthat::test_that("a label check can warn instead of aborting", {
  landuse <- tibble::tibble(Element = "Yield")
  testthat::expect_warning(
    out <- whep::check_labels_supplied(
      landuse,
      "Element",
      "Area",
      action = "warn"
    ),
    class = "whep_absent_label"
  )
  testthat::expect_identical(out, landuse)
})

testthat::test_that("both checks share one condition class", {
  vocabulary <- tibble::tibble(Element = "Yield")
  testthat::expect_error(
    whep::check_labels_supplied(vocabulary, "Element", "Area"),
    class = "whep_absent_input"
  )
})

testthat::test_that("labels must be a non-missing, non-empty vector", {
  landuse <- tibble::tibble(Element = "Area")
  testthat::expect_error(
    whep::check_labels_supplied(landuse, "Element", character()),
    "non-missing, non-empty"
  )
  testthat::expect_error(
    whep::check_labels_supplied(landuse, "Element", NA),
    "non-missing, non-empty"
  )
})
