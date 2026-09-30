# Offline tests for the FAOSTAT livestock-emissions helpers in
# inst/scripts/prepare_spatialize_all.R. They live at script scope, so the
# script is sourced once (see helper_prepare_spatialize.R) and the helpers are
# exercised against hand-built tibbles -- no pin, no network.
#
# What they pin down is whep#1016: the registered
# `faostat-emissions-livestock` pin no longer carries the three emission
# Elements, every filter returned 0 rows, and the missing values were turned
# into literal zeros. The shipped `spatialize-livestock-country-data` pin
# (20260625T101231Z) is 208,244 rows in which all three emission columns
# are exactly 0, with no NA and no non-zero value anywhere -- in place of
# roughly 108 Tg CH4/yr of enteric methane alone. A missing input must
# abort, never become a zero.

.source_prepare_spatialize()

# One well-formed pin row per element, for two species that map to distinct
# groups. Values are arbitrary; only their propagation is under test.
.emi_fixture <- function() {
  tibble::tribble(
    ~Source, ~`Area Code`, ~Year, ~Item, ~Element, ~Value,
    "FAO TIER 1", 1L, 2020L, "Cattle, dairy",
    "Enteric fermentation (Emissions CH4)", 100,
    "FAO TIER 1", 1L, 2020L, "Cattle, dairy",
    "Manure management (Emissions CH4)", 10,
    "FAO TIER 1", 1L, 2020L, "Cattle, dairy",
    "Manure management (Emissions N2O)", 1,
    "FAO TIER 1", 1L, 2020L, "Asses",
    "Enteric fermentation (Emissions CH4)", 5,
    "FAO TIER 1", 1L, 2020L, "Asses",
    "Manure management (Emissions CH4)", 2,
    "FAO TIER 1", 1L, 2020L, "Asses",
    "Manure management (Emissions N2O)", 3
  )
}


test_that("an element with no rows aborts instead of becoming zero", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # The whep#1016 shape exactly: the pin reads fine, but the enteric rows are
  # gone. The old code let the filter return 0 rows and zero-filled the join.
  emi <- .emi_fixture() |>
    dplyr::filter(Element != "Enteric fermentation (Emissions CH4)")
  expect_error(
    .summarise_livestock_emissions(emi, "20260526T151303Z-bac9d"),
    "Enteric fermentation"
  )
  # The message must name the pin version, so the reader knows which vintage
  # to replace.
  expect_error(
    .summarise_livestock_emissions(emi, "20260526T151303Z-bac9d"),
    "20260526T151303Z-bac9d"
  )
})


test_that("an element whose items are all unmapped aborts", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- .emi_fixture() |>
    dplyr::mutate(
      Item = dplyr::if_else(
        Element == "Manure management (Emissions N2O)",
        "Llamas",
        Item
      )
    )
  expect_error(
    .summarise_livestock_emissions(emi, "v1"),
    "Manure management \\(Emissions N2O\\)"
  )
})


test_that("only FAO TIER 1 rows are kept", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # Measured on pin vintage 20260325T113403Z-23bf8: 41 areas report under
  # both sources in 2020 (179 across 1961-2050), and keeping both raises
  # 2020 enteric CH4 from 107,993 kt to 130,793 kt, a 1.21x inflation. A
  # duplicated UNFCCC row must not reach the sum.
  emi <- dplyr::bind_rows(
    .emi_fixture(),
    dplyr::mutate(.emi_fixture(), Source = "UNFCCC")
  )
  out <- .summarise_livestock_emissions(emi, "v1")
  dairy <- dplyr::filter(out, species_group == "cattle_dairy")
  expect_equal(dairy$enteric_ch4_kt, 100)
})


test_that("a pin with no Source column aborts", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- dplyr::select(.emi_fixture(), -Source)
  expect_error(.summarise_livestock_emissions(emi, "v1"), "Source")
})


test_that("the equine aggregate is not summed with its own parts", {
  .need_spatialize_helper(".livestock_emi_species_map")
  map <- .livestock_emi_species_map()
  # "Mules and Asses" is the FAOSTAT aggregate of "Asses" and
  # "Mules and hinnies". Measured on pin vintage 20260325T113403Z-23bf8,
  # areas with `Area Code` < 5000 under FAO TIER 1: 133 areas report the
  # aggregate, 83 of them alongside both parts, 133 alongside at least one
  # part, and 0 alongside neither -- so mapping the aggregate as well would
  # double-count equines in every area that reports it, and dropping it
  # loses no area.
  expect_false("Mules and Asses" %in% map$emi_item)
  expect_true(all(c("Asses", "Mules and hinnies") %in% map$emi_item))
})


test_that("the aggregate row is dropped from a summarised total", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- dplyr::bind_rows(
    .emi_fixture(),
    dplyr::mutate(
      dplyr::filter(
        .emi_fixture(),
        Element == "Enteric fermentation (Emissions CH4)",
        Item == "Asses"
      ),
      Item = "Mules and Asses"
    )
  )
  out <- .summarise_livestock_emissions(emi, "v1")
  equines <- dplyr::filter(out, species_group == "equines")
  expect_equal(equines$enteric_ch4_kt, 5)
})


test_that("a stock row with no emissions row stays NA, not zero", {
  .need_spatialize_helper(".join_livestock_emissions")
  emissions <- .summarise_livestock_emissions(.emi_fixture(), "v1")
  stocks <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2020L, 1L, "cattle_dairy", 10,
    1900L, 1L, "cattle_dairy", 8
  )
  expect_warning(
    .join_livestock_emissions(stocks, emissions),
    "no FAOSTAT emissions row"
  )
  out <- suppressWarnings(.join_livestock_emissions(stocks, emissions))
  expect_equal(out$enteric_ch4_kt[out$year == 2020L], 100)
  expect_true(is.na(out$enteric_ch4_kt[out$year == 1900L]))
})


test_that("a join that matches nothing aborts", {
  .need_spatialize_helper(".join_livestock_emissions")
  emissions <- .summarise_livestock_emissions(.emi_fixture(), "v1")
  stocks <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2020L, 999L, "cattle_dairy", 10
  )
  expect_error(
    .join_livestock_emissions(stocks, emissions),
    "matched none"
  )
})


# The registered vintage 20260526T151303Z-bac9d as it actually is, read from
# the local pins cache: 2,555,034 rows, 14 columns, NO `Source` column, and
# ten Elements none of which is one of the three this build needs. The column
# set and the Element list below are that pin's, verbatim.
.registered_pin_fixture <- function() {
  elements <- c(
    "Amount excreted in manure (N content)",
    "Manure left on pasture (N content)",
    "Manure left on pasture that volatilises (N content)",
    "Manure left on pasture that leaches (N content)",
    "Manure management (manure treated, N content)",
    "Losses from manure treated (N content)",
    "Manure applied to soils (N content)",
    "Manure applied to soils that volatilises (N content)",
    "Manure applied to soils that leaches (N content)",
    "Stocks"
  )
  tibble::tibble(
    `Area Code` = 1L,
    `Area Code (M49)` = "'004",
    Area = "Afghanistan",
    `Item Code` = 1107L,
    `Item Code (CPC)` = "'02121",
    Item = "Asses",
    `Element Code` = 5111L,
    Element = elements,
    `Year Code` = 2020L,
    Year = 2020L,
    Unit = "kg",
    Value = 1,
    Flag = "E",
    Note = NA_character_
  )
}

# The rendered abort text, or a sentinel when the call did not abort at all,
# so a test that expects a diagnosis fails on the message rather than on a
# type error.
.abort_message <- function(expr) {
  tryCatch(
    {
      force(expr)
      "<no error was raised>"
    },
    error = conditionMessage
  )
}


test_that("the registered pin's shape aborts on the Element, not on Source", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # whep#1016 is "the three emission Elements are gone". The registered pin
  # also happens to have no `Source` column, so checking columns first
  # diagnosed a UNFCCC duplication that vintage cannot even have, named no
  # Element and named no pin version.
  emi <- .registered_pin_fixture()
  expect_false("Source" %in% names(emi))
  msg <- .abort_message(
    .summarise_livestock_emissions(emi, "20260526T151303Z-bac9d")
  )
  expect_match(msg, "Enteric fermentation (Emissions CH4)", fixed = TRUE)
  expect_match(msg, "20260526T151303Z-bac9d", fixed = TRUE)
  expect_false(grepl("UNFCCC", msg, fixed = TRUE))
})


test_that("a pin missing Source but carrying the Elements says so", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # The other condition, and it needs its own diagnosis: the Elements are
  # here, so the build can proceed as soon as the duplication can be removed.
  emi <- dplyr::select(.emi_fixture(), -Source)
  msg <- .abort_message(
    .summarise_livestock_emissions(emi, "20260526T151303Z-bac9d")
  )
  expect_match(msg, "Source", fixed = TRUE)
  expect_match(msg, "20260526T151303Z-bac9d", fixed = TRUE)
  expect_false(grepl("Enteric fermentation", msg, fixed = TRUE))
})


test_that("a pin missing a structural column names that column", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- dplyr::select(.emi_fixture(), -Value)
  expect_error(.summarise_livestock_emissions(emi, "v1"), "Value")
})


test_that("an element whose Values are all NA aborts, never sums to zero", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # `sum(NA, na.rm = TRUE)` is 0, so the rows survive every filter, the
  # row-count guard sees a non-empty result and a literal 0 kt ships. That is
  # the whep#1016 artifact reached by a second route.
  emi <- .emi_fixture() |>
    dplyr::mutate(
      Value = dplyr::if_else(
        Element == "Enteric fermentation (Emissions CH4)",
        NA_real_,
        Value
      )
    )
  msg <- .abort_message(
    suppressWarnings(
      .summarise_livestock_emissions(emi, "20260526T151303Z-bac9d")
    )
  )
  expect_match(msg, "Enteric fermentation (Emissions CH4)", fixed = TRUE)
  expect_match(msg, "20260526T151303Z-bac9d", fixed = TRUE)
})


test_that("a group with no observed Value is NA, not zero", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # One species keeps a real enteric value so the element is not empty; the
  # other has rows but no observation at all and must not be booked at 0.
  emi <- .emi_fixture() |>
    dplyr::mutate(
      Value = dplyr::if_else(
        Element == "Enteric fermentation (Emissions CH4)" & Item == "Asses",
        NA_real_,
        Value
      )
    )
  out <- suppressWarnings(.summarise_livestock_emissions(emi, "v1"))
  equines <- dplyr::filter(out, species_group == "equines")
  expect_true(is.na(equines$enteric_ch4_kt))
  expect_equal(equines$manure_ch4_kt, 2)
})


test_that("a partly observed group warns instead of silently dropping", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- dplyr::bind_rows(
    .emi_fixture(),
    tibble::tibble(
      Source = "FAO TIER 1",
      `Area Code` = 1L,
      Year = 2020L,
      Item = "Mules and hinnies",
      Element = "Enteric fermentation (Emissions CH4)",
      Value = NA_real_
    )
  )
  expect_warning(
    .summarise_livestock_emissions(emi, "v1"),
    "missing Value"
  )
})


test_that("only poultry gets a structural enteric zero", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # FAOSTAT publishes no enteric fermentation for any poultry item, and none
  # for the item "Camels and Llamas" either -- but a camel ferments and a
  # chicken does not, so only one of those absences is a structural zero.
  emi <- dplyr::bind_rows(
    .emi_fixture(),
    tibble::tribble(
      ~Source, ~`Area Code`, ~Year, ~Item, ~Element, ~Value,
      "FAO TIER 1", 1L, 2020L, "Ducks",
      "Manure management (Emissions CH4)", 7,
      "FAO TIER 1", 1L, 2020L, "Ducks",
      "Manure management (Emissions N2O)", 8,
      "FAO TIER 1", 1L, 2020L, "Camels and Llamas",
      "Manure management (Emissions CH4)", 9,
      "FAO TIER 1", 1L, 2020L, "Camels and Llamas",
      "Manure management (Emissions N2O)", 11
    )
  )
  out <- .summarise_livestock_emissions(emi, "v1")
  poultry <- dplyr::filter(out, species_group == "poultry")
  camels <- dplyr::filter(out, species_group == "camels")
  expect_equal(poultry$enteric_ch4_kt, 0)
  expect_true(is.na(camels$enteric_ch4_kt))
})


test_that("a row matching only some emission columns still counts as joined", {
  .need_spatialize_helper(".join_livestock_emissions")
  # With camel enteric left NA, "matched" can no longer be read off one
  # column: the row IS in the emissions table.
  emissions <- tibble::tribble(
    ~year, ~area_code, ~species_group,
    ~enteric_ch4_kt, ~manure_ch4_kt, ~manure_n2o_kt,
    2020L, 1L, "camels", NA_real_, 9, 11
  )
  stocks <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2020L, 1L, "camels", 10
  )
  out <- .join_livestock_emissions(stocks, emissions)
  expect_equal(out$manure_ch4_kt, 9)
  expect_true(is.na(out$enteric_ch4_kt))
})


test_that("an element present only under UNFCCC aborts after the filter", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  # The Element check passes -- the rows are in the pin -- and the Source
  # filter then removes them all. Naming that as "no rows at all" would send
  # the reader to replace a pin that already carries the element.
  emi <- .emi_fixture() |>
    dplyr::mutate(
      Source = dplyr::if_else(
        Element == "Manure management (Emissions N2O)",
        "UNFCCC",
        Source
      )
    )
  msg <- .abort_message(.summarise_livestock_emissions(emi, "v1"))
  expect_match(msg, "Manure management (Emissions N2O)", fixed = TRUE)
  expect_match(msg, "FAO TIER 1", fixed = TRUE)
})


test_that("a pin with no FAO TIER 1 row at all names its sources", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- dplyr::mutate(.emi_fixture(), Source = "UNFCCC")
  msg <- .abort_message(.summarise_livestock_emissions(emi, "v1"))
  expect_match(msg, "FAO TIER 1", fixed = TRUE)
  expect_match(msg, "UNFCCC", fixed = TRUE)
})


test_that("an element whose items are all unmapped names the filters", {
  .need_spatialize_helper(".summarise_livestock_emissions")
  emi <- .emi_fixture() |>
    dplyr::mutate(
      Item = dplyr::if_else(
        Element == "Manure management (Emissions N2O)",
        "Llamas",
        Item
      )
    )
  msg <- .abort_message(.summarise_livestock_emissions(emi, "v1"))
  expect_match(msg, "item-to-species map", fixed = TRUE)
})
