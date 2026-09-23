testthat::test_that(".use_crop_process_cbs_item keeps crop process codes", {
  crop_residues <- tibble::tribble(
    ~item_cbs_code_crop, ~value,
    328, 100,
    248, 200,
    254, 300,
    310, 350,
    772, 360,
    776, 370,
    999, 400
  )

  result <- .use_crop_process_cbs_item(crop_residues)

  expected_codes <- result |>
    dplyr::pull(item_cbs_code_crop)
  testthat::expect_equal(
    expected_codes,
    c(328, 248, 254, 310, 772, 776, 999)
  )
  testthat::expect_equal(result$value, c(100, 200, 300, 350, 360, 370, 400))
})

testthat::test_that(".use_crop_process_cbs_item leaves non-mapped codes unchanged", {
  crop_residues <- tibble::tibble(
    item_cbs_code_crop = c(1000, 2000, 3000),
    value = c(10, 20, 30)
  )

  result <- .use_crop_process_cbs_item(crop_residues)

  testthat::expect_equal(
    result$item_cbs_code_crop,
    c(1000, 2000, 3000)
  )
})

testthat::test_that(".warn_residues_no_area names rows that resolved to no area", {
  # `get_primary_residues()` is the only builder that resolves its areas by NAME,
  # through `add_area_code()`, and it left `area_code` as NA wherever no name
  # matched. Those rows travelled to the output with NA polity columns and on into
  # `build_supply_use()` in complete silence (whep#684).
  #
  # `.residue_area_from_polity()` now recovers the 14 short-form labels that were
  # nearly all of that gap; this diagnostic stays for whatever neither route
  # resolves, so the label used here is one no route holds.
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    1961L, "Nowhereland", NA_integer_,
    1962L, "Nowhereland", NA_integer_,
    1961L, "Spain", 203L
  )

  testthat::expect_warning(
    result <- .warn_residues_no_area(dt),
    "crop-residue"
  )
  # Reports rather than drops: whether an unattributable residue row should be
  # removed is a modelling question, so the frame comes back untouched.
  testthat::expect_identical(result, dt)
})

testthat::test_that(".warn_residues_no_area stays quiet when every row resolved", {
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    1961L, "Spain", 203L
  )

  testthat::expect_no_warning(.warn_residues_no_area(dt))
})

# get_primary_production / get_primary_residues --------------------------------

testthat::test_that("get_primary_production(example = TRUE) needs no remote", {
  out <- whep::get_primary_production(example = TRUE)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(
    all(
      c("year", "area_code", "item_prod_code", "unit", "value") %in%
        names(out)
    )
  )
})

# The `crop_residues` pin, in the mixed-case schema the builder lowercases.
# "Nowhereland" is deliberately a label NEITHER route holds -- no canonical area
# name and no polity alias -- which is how the unresolved-area branch is reached.
# "Tanzania" is the opposite case and the one the pin really carries: no
# canonical area name, but a polity alias, so it exercises the recovery route.
residues_pin_fixture <- function() {
  tibble::tribble(
    ~Area,         ~Product_residue, ~Item_cbs,             ~Prod_ygpit_Mg,
    "Spain",       "Residue",        "Straw",               100,
    "Spain",       "Residue",        "Straw",               50,
    "Spain",       "Product",        "Straw",               999,
    "Spain",       "Residue",        "Other crop residues", 0,
    "Tanzania",    "Residue",        "Straw",               11,
    "Nowhereland", "Residue",        "Straw",               7
  ) |>
    dplyr::mutate(
      Year = 2000L,
      Item_cbs_crop = "Wheat and products",
      Name_biomass = "Wheat"
    )
}

testthat::test_that("get_primary_residues aggregates residues on codes", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residues_pin_fixture()
  })

  testthat::expect_warning(
    out <- suppressMessages(whep::get_primary_residues()),
    "resolved to no area"
  )

  spain <- out |> dplyr::filter(area_code == 203L)
  # The two Spanish straw rows are summed, the "Product" row (999) is not a
  # residue and the zero-tonne residue row is dropped.
  testthat::expect_equal(nrow(spain), 1)
  testthat::expect_equal(spain$value, 150)
  testthat::expect_equal(spain$item_cbs_code_crop, 2511)
  testthat::expect_equal(spain$item_cbs_code_residue, 2105)
  testthat::expect_false(any(out$value == 999))
})

testthat::test_that("get_primary_residues ignores NA rows within a group", {
  # whep#167: `prod_ygpit_mg` carries real NAs in the source pin (2,898 of
  # 475,688 rows measured on the current pin). Without `na.rm = TRUE`, one NA
  # sibling turns the whole `(year, area_code, item_cbs_code_crop,
  # item_cbs_code_residue)` sum into NA, and `filter(value > 0)` then silently
  # drops the group -- erasing the real, non-NA rows summed into it, not just
  # the missing one.
  local_mocked_bindings(whep_read_file = function(name, ...) {
    tibble::tribble(
      ~Area,   ~Product_residue, ~Item_cbs, ~Prod_ygpit_Mg,
      "Spain", "Residue",        "Straw",   100,
      "Spain", "Residue",        "Straw",   NA_real_
    ) |>
      dplyr::mutate(
        Year = 2000L,
        Item_cbs_crop = "Wheat and products",
        Name_biomass = "Wheat"
      )
  })

  out <- whep::get_primary_residues()

  spain <- out |> dplyr::filter(area_code == 203L)
  # The valid 100-tonne row must survive; only the NA sibling is ignored.
  testthat::expect_equal(nrow(spain), 1)
  testthat::expect_equal(spain$value, 100)
})

testthat::test_that("get_primary_residues converts each crop's residue to DM", {
  # whep#1215: the pin's residue tonnes are fresh matter. `value` stays fresh,
  # like every CBS quantity, and `value_dm` converts each pin row with the
  # residue dry-matter content of its own crop.
  local_mocked_bindings(whep_read_file = function(name, ...) {
    tibble::tribble(
      ~Area,   ~Product_residue, ~Item_cbs,             ~Prod_ygpit_Mg,
      ~Item_cbs_crop,           ~Name_biomass,
      "Spain", "Residue",        "Straw",               100,
      "Wheat and products",     "Wheat",
      "Spain", "Residue",        "Straw",               NA_real_,
      "Wheat and products",     "Wheat",
      "Spain", "Residue",        "Other crop residues", 200,
      "Tomatoes and products",  "Tomato",
      "Spain", "Residue",        "Other crop residues", 50,
      "Tomatoes and products",  "Lettuce"
    ) |>
      dplyr::mutate(Year = 2000L)
  })
  kgdm <- function(nm) {
    coefs <- whep::biomass_coefs
    coefs$Residue_kgDM_kgFM[coefs$Name_biomass == nm]
  }

  out <- whep::get_primary_residues()

  straw <- out[out$item_cbs_code_residue == 2105, ]
  testthat::expect_equal(straw$value, 100)
  testthat::expect_equal(straw$value_dm, 100 * kgdm("Wheat"))
  other <- out[out$item_cbs_code_residue == 2106, ]
  testthat::expect_equal(other$value, 250)
  testthat::expect_equal(
    other$value_dm,
    200 * kgdm("Tomato") + 50 * kgdm("Lettuce")
  )
  # Fresh tomato haulm is mostly water: nowhere near 0.9 kg DM per kg.
  testthat::expect_lt(other$value_dm / other$value, 0.25)
})

testthat::test_that("get_primary_residues keeps a missing DM content visible", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    tibble::tribble(
      ~Area,   ~Product_residue, ~Item_cbs, ~Prod_ygpit_Mg, ~Name_biomass,
      "Spain", "Residue",        "Straw",   100,            "Wheat",
      "Spain", "Residue",        "Straw",   40,             "Mushrooms"
    ) |>
      dplyr::mutate(Year = 2000L, Item_cbs_crop = "Wheat and products")
  })

  testthat::expect_warning(
    out <- whep::get_primary_residues(),
    "no residue dry-matter content"
  )
  # The fresh mass is kept whole; the dry matter is NA, not a silent partial
  # sum that would read as a real, smaller number.
  testthat::expect_equal(out$value, 140)
  testthat::expect_true(is.na(out$value_dm))
})

testthat::test_that("get_primary_residues aborts without name_biomass", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    dplyr::select(residues_pin_fixture(), -"Name_biomass")
  })
  testthat::expect_error(
    suppressMessages(suppressWarnings(whep::get_primary_residues())),
    "name_biomass"
  )
})

testthat::test_that("get_primary_residues keeps unresolved areas visible", {
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residues_pin_fixture()
  })

  out <- suppressMessages(suppressWarnings(whep::get_primary_residues()))

  # The row whose area label did not resolve is reported, not dropped, so the
  # gap stays visible downstream instead of silently shrinking the totals.
  unresolved <- out |> dplyr::filter(is.na(area_code))
  testthat::expect_equal(unresolved$value, 7)
  testthat::expect_true(is.na(unresolved$reporting_polity_code))
  testthat::expect_equal(sum(out$value), 168)
})

testthat::test_that("get_primary_residues resolves a short-form area label", {
  # whep#1175, whep#684. The pin spells 14 of its 185 labels short, "Tanzania"
  # where the crosswalk holds "United Republic of Tanzania", and `add_area_code`
  # matches the canonical name exactly, so those 44,985 rows carried no area code
  # at all: 16.65 Gt, 5.08 percent of the pin's residue dry matter. Downstream
  # that is not a gap but a wrong answer, because the row reaches no polity and
  # no region label, its recovery rate is read as zero, and the whole residue is
  # booked to soil.
  local_mocked_bindings(whep_read_file = function(name, ...) {
    residues_pin_fixture()
  })

  out <- suppressMessages(suppressWarnings(whep::get_primary_residues()))

  tanzania <- out |> dplyr::filter(area_code == 215L)
  testthat::expect_equal(nrow(tanzania), 1)
  testthat::expect_equal(tanzania$value, 11)
  testthat::expect_equal(tanzania$reporting_polity_code, "TZA-1964-2025")
})

testthat::test_that(".residue_area_from_polity keeps the label's own year", {
  # A label's referent moves, so the polity route is asked per (label, year).
  # "Tanzania" is TZA-1964-2025 from 1964 on and the pre-union TZA-1961-1964
  # before it, which no FAOSTAT area reports: booking 1961 to the United
  # Republic would be a wrong answer dressed as a fix, so it stays NA.
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    1961L, "Tanzania", NA_integer_,
    2000L, "Tanzania", NA_integer_,
    2000L, "Nowhereland", NA_integer_,
    2000L, "Spain", 203L
  )

  out <- suppressMessages(.residue_area_from_polity(dt))

  testthat::expect_equal(out$area_code, c(NA_integer_, 215L, NA_integer_, 203L))
  testthat::expect_identical(names(out), names(dt))
})

testthat::test_that(".residue_area_from_polity leaves a resolved table alone", {
  dt <- tibble::tribble(
    ~year, ~area, ~area_code,
    2000L, "Spain", 203L
  )

  testthat::expect_identical(.residue_area_from_polity(dt), dt)
})

testthat::test_that(".unique_polity_area refuses a polity many areas share", {
  # The Rest-of-World bucket ROW-1850-2025 is carried by 15 area codes, so it
  # names no single area and must not resolve to whichever one came first.
  map <- .unique_polity_area()

  testthat::expect_false("ROW-1850-2025" %in% map$polity_code)
  testthat::expect_equal(
    map$area_code_from_polity[map$polity_code == "TZA-1964-2025"],
    215L
  )
  testthat::expect_equal(anyDuplicated(map$polity_code), 0L)
})

testthat::test_that("get_primary_residues(example = TRUE) needs no remote", {
  out <- whep::get_primary_residues(example = TRUE)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(
    all(c("year", "area_code", "item_cbs_code_crop", "value") %in% names(out))
  )
})
