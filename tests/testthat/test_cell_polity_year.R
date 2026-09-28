# The year-aware cell support (`build_cell_polity(year = )`, whep#1196).
#
# The fixture is a polycell support on made-up cells but with REAL polity
# codes, so the recorded mapping (inst/extdata/polity_cell_support_map.csv) and
# the shipped polity_area_crosswalk are exercised, not stand-ins for them. Every
# cell has an area of 100 ha, so a territory sum above 100 is an overlap.

.cpy_row <- function(lon, lat, polity, start, end, land, terr = land) {
  tibble::tibble(
    lon = lon,
    lat = lat,
    polity_code = polity,
    area_code = NA_integer_,
    start_year = as.integer(start),
    end_year = as.integer(end),
    cell_area_ha = 100,
    polity_area_ha = terr,
    land_area_ha = land
  )
}

.cpy_support <- function() {
  dplyr::bind_rows(
    # A: the USSR alone.
    .cpy_row(30.25, 55.25, "F228-1945-1991", 1945, 1991, 100),
    # B: the USSR and the Estonian SSR label the same land twice.
    .cpy_row(24.25, 57.25, "F228-1945-1991", 1945, 1991, 60),
    .cpy_row(24.25, 57.25, "EST-1940-1991", 1940, 1991, 60),
    # C: the Estonian SSR alone, then Estonia.
    .cpy_row(24.75, 57.25, "EST-1940-1991", 1940, 1991, 50),
    .cpy_row(24.75, 57.25, "EST-1991-2025", 1991, 2025, 50),
    # D: Belgium and Luxembourg share a cell without overlapping.
    .cpy_row(4.25, 50.75, "BEL-1831-2025", 1831, 2025, 70),
    .cpy_row(4.25, 50.75, "LUX-1839-2025", 1839, 2025, 30),
    # E: Germany over West Germany, the same 90 ha twice.
    .cpy_row(10.25, 51.25, "DEU-1949-1990", 1949, 1990, 90),
    .cpy_row(10.25, 51.25, "F78-1949-1990", 1949, 1990, 90),
    # F: Russia, from 1991.
    .cpy_row(37.25, 55.75, "RUS-1991-2014", 1991, 2014, 100),
    # G: two claimants that both report.
    .cpy_row(78.25, 34.25, "IND-1949-2025", 1949, 2025, 80),
    .cpy_row(78.25, 34.25, "CHN-1950-2025", 1950, 2025, 80),
    # H: Spain and the Canary Islands (no area code), not overlapping.
    .cpy_row(-15.25, 28.25, "ESP-1800-2025", 1800, 2025, 60),
    .cpy_row(-15.25, 28.25, "ICN-1800-2025", 1800, 2025, 40),
    # North and South Vietnam, then Viet Nam.
    .cpy_row(105.75, 21.25, "DRV-1954-1975", 1954, 1975, 100),
    .cpy_row(106.75, 10.75, "RVN-1954-1975", 1954, 1975, 100),
    .cpy_row(105.75, 21.25, "VNM-1975-2025", 1975, 2025, 100),
    .cpy_row(106.75, 10.75, "VNM-1975-2025", 1975, 2025, 100),
    # Yemen: the Kingdom and Aden, then modern Yemen on both cells.
    .cpy_row(44.25, 15.25, "MKY-1918-1962", 1918, 1962, 100),
    .cpy_row(45.25, 13.25, "ADE-1839-1963", 1839, 1963, 100),
    .cpy_row(44.25, 15.25, "YEM-1990-2025", 1990, 2025, 100),
    .cpy_row(45.25, 13.25, "YEM-1990-2025", 1990, 2025, 100),
    # A border cell of modern Yemen whose polycell the support splits at 1993,
    # shared with Saudi Arabia.
    .cpy_row(46.25, 16.25, "YEM-1990-2025", 1990, 1993, 60),
    .cpy_row(46.25, 16.25, "YEM-1990-2025", 1993, 2025, 60),
    .cpy_row(46.25, 16.25, "SAU-1924-2025", 1924, 2025, 40)
  )
}

.cpy_reporting <- function() {
  c(228L, 15L, 79L, 185L, 237L, 249L, 100L, 41L, 203L, 255L, 256L, 63L, 194L)
}

.cpy_build <- function(year, reporting = .cpy_reporting(), key = "grid") {
  suppressMessages(
    whep:::.cell_polity_year_support(.cpy_support(), year, reporting, key)
  )
}

.cpy_cell <- function(out, lon, lat) {
  dplyr::filter(out, .data$lon == !!lon, .data$lat == !!lat)
}

testthat::test_that("every mapping row names a real polity and a live code", {
  map <- whep:::.cell_polity_support_map()
  testthat::expect_true(all(map$polity_code %in% whep::polities$polity_code))
  testthat::expect_true(all(map$rule %in% whep:::.cpy_rules()))
  testthat::expect_true(all(map$start_year < map$end_year))
  testthat::expect_false(anyDuplicated(map$polity_code) > 0)
})

testthat::test_that("a union's cells carry the union in its years", {
  out <- .cpy_build(1970L)
  testthat::expect_equal(.cpy_cell(out, 30.25, 55.25)$area_code, 228L)
  # The Estonian SSR folds into the USSR where it is alone ...
  cell_c <- .cpy_cell(out, 24.75, 57.25)
  testthat::expect_equal(cell_c$area_code, 228L)
  testthat::expect_equal(cell_c$polity_rule, "contained_fold")
  testthat::expect_equal(cell_c$polity_frac, 1)
  # ... and is removed where it duplicates the USSR, which keeps the cell.
  cell_b <- .cpy_cell(out, 24.25, 57.25)
  testthat::expect_equal(cell_b$area_code, 228L)
  testthat::expect_equal(cell_b$polity_frac, 1)
  removed <- attr(out, "deduplicated")
  testthat::expect_equal(
    removed$removed_reason[removed$polity_code == "EST-1940-1991"],
    "duplicates_container"
  )
})

testthat::test_that("successors carry their own codes once they report", {
  out <- .cpy_build(1995L)
  testthat::expect_equal(.cpy_cell(out, 37.25, 55.75)$area_code, 185L)
  testthat::expect_equal(.cpy_cell(out, 24.75, 57.25)$area_code, 63L)
  testthat::expect_equal(nrow(.cpy_cell(out, 30.25, 55.25)), 0L)
})

testthat::test_that("the USSR's successors fold into it in 1991", {
  out <- .cpy_build(1991L)
  testthat::expect_equal(.cpy_cell(out, 37.25, 55.75)$area_code, 228L)
  testthat::expect_equal(.cpy_cell(out, 24.75, 57.25)$area_code, 228L)
})

testthat::test_that("an aggregate takes the union of its members' cells", {
  in_union <- .cpy_build(1990L)
  cell_d <- .cpy_cell(in_union, 4.25, 50.75)
  testthat::expect_equal(cell_d$area_code, 15L)
  testthat::expect_equal(cell_d$polity_frac, 1)
  testthat::expect_equal(cell_d$polity_rule, "aggregate_member")

  after <- .cpy_build(2005L)
  cell_d <- .cpy_cell(after, 4.25, 50.75)
  testthat::expect_equal(sort(cell_d$area_code), c(255L, 256L))
  testthat::expect_equal(
    cell_d$polity_frac[cell_d$area_code == 255L],
    0.7
  )

  vietnam <- .cpy_build(1970L) |> dplyr::filter(.data$area_code == 237L)
  testthat::expect_equal(nrow(vietnam), 2L)
  testthat::expect_equal(unique(vietnam$polity_rule), "aggregate_member")
})

testthat::test_that("Yemen is its predecessors in 1961 and modern Yemen after", {
  in_1961 <- .cpy_build(1961L) |> dplyr::filter(.data$area_code == 249L)
  testthat::expect_equal(nrow(in_1961), 2L)
  testthat::expect_equal(unique(in_1961$polity_rule), "aggregate_member")

  for (yr in c(1962L, 1970L)) {
    later <- .cpy_build(yr) |>
      dplyr::filter(.data$area_code == 249L) |>
      dplyr::arrange(.data$lon)
    testthat::expect_equal(nrow(later), 3L)
    testthat::expect_equal(unique(later$polity_rule), "constant_territory")
    # The split polycell is taken once: 60 of the cell's 100 ha, not 120.
    testthat::expect_equal(later$polity_frac, c(1, 1, 0.6))
  }
  # In 1962 Aden (no area code) overlaps the modern cells and is removed.
  removed <- attr(.cpy_build(1962L), "deduplicated")
  testthat::expect_equal(
    removed$removed_reason[removed$polity_code == "ADE-1839-1963"],
    "no_area_code"
  )
})

testthat::test_that("an overlapping cell drops a polity with no area code", {
  out <- .cpy_build(1970L)
  cell_e <- .cpy_cell(out, 10.25, 51.25)
  testthat::expect_equal(cell_e$area_code, 79L)
  testthat::expect_equal(cell_e$polity_frac, 1)
  removed <- attr(out, "deduplicated")
  testthat::expect_equal(
    removed$removed_reason[removed$polity_code == "F78-1949-1990"],
    "no_area_code"
  )
})

testthat::test_that("an overlapping cell drops a polity with no national data", {
  out <- .cpy_build(1970L, reporting = setdiff(.cpy_reporting(), 41L))
  cell_g <- .cpy_cell(out, 78.25, 34.25)
  testthat::expect_equal(cell_g$area_code, 100L)
  testthat::expect_equal(cell_g$polity_frac, 1)
})

testthat::test_that("two reporting claimants keep their halves and are listed", {
  out <- .cpy_build(1970L)
  cell_g <- .cpy_cell(out, 78.25, 34.25)
  testthat::expect_equal(sort(cell_g$area_code), c(41L, 100L))
  testthat::expect_equal(cell_g$polity_frac, c(0.5, 0.5))
  kept <- attr(out, "overlap_kept")
  testthat::expect_setequal(kept$area_code, c(41L, 100L))
})

testthat::test_that("an unkeyed polity keeps its land in a clean cell", {
  cell_h <- .cpy_cell(.cpy_build(1970L), -15.25, 28.25)
  testthat::expect_equal(cell_h$area_code, 203L)
  testthat::expect_equal(cell_h$polity_frac, 0.6)
})

testthat::test_that("shares never exceed the cell and equal cell_area_frac", {
  for (yr in c(1961L, 1970L, 1990L, 1991L, 2005L)) {
    out <- .cpy_build(yr)
    per_cell <- dplyr::summarise(
      out,
      total = sum(.data$polity_frac),
      .by = c("lon", "lat")
    )
    testthat::expect_true(all(per_cell$total <= 1 + 1e-12))
    testthat::expect_equal(out$polity_frac, out$cell_area_frac)
    pointblank::expect_col_vals_not_null(out, "area_code")
  }
})

testthat::test_that("the union's fertiliser lands on its cells, conserved", {
  crop_patterns <- .cpy_support() |>
    dplyr::distinct(.data$lon, .data$lat) |>
    dplyr::mutate(item_prod_code = 15L, harvest_fraction = 1)
  cropland <- function(yr) {
    dplyr::distinct(.cpy_support(), .data$lon, .data$lat) |>
      dplyr::mutate(
        year = yr,
        luh2_type = "c3ann",
        type_ha = 10,
        type_irrig_ha = 0
      )
  }
  spread <- function(yr, code) {
    whep::spatialize_country_n_to_crops(
      country_totals = tibble::tibble(year = yr, area_code = code, n_t = 1000),
      crop_shares = tibble::tibble(
        year = yr,
        area_code = code,
        item_cbs_code = 2511L,
        area_share = 1
      ),
      cell_polity = .cpy_build(yr),
      resolution = "grid",
      data = list(crop_patterns = crop_patterns, type_cropland = cropland(yr))
    )
  }
  union <- spread(1970L, 228L)
  testthat::expect_equal(sum(union$n_t), 1000)
  testthat::expect_setequal(union$lon, c(30.25, 24.25, 24.75))

  successor <- spread(1995L, 185L)
  testthat::expect_equal(sum(successor$n_t), 1000)
  testthat::expect_equal(unique(successor$lon), 37.25)

  # The removal record: 228 has a cell in 1970, and would have none on the
  # year-invariant crosswalk, whose Russia is 185.
  fert <- tibble::tibble(
    Element = "Agricultural Use",
    Item = "Nutrient nitrogen N (total)",
    Year = 1970L,
    `Area Code` = 228L,
    Value = 1000
  )
  supported <- dplyr::distinct(.cpy_build(1970L), year = 1970L, .data$area_code)
  testthat::expect_equal(
    nrow(whep:::.n_uncelled_fertilizer(fert, supported)),
    0L
  )
  invariant <- tibble::tibble(year = 1970L, area_code = 185L)
  removed <- whep:::.n_uncelled_fertilizer(fert, invariant)
  testthat::expect_equal(removed$area_code, 228L)
  testthat::expect_equal(removed$synthetic_n_t, 1000)
})

testthat::test_that("build_cell_polity reads the polycell support with year", {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) .cpy_support(),
    whep_read_file = function(...) {
      testthat::fail("the year-aware path must not read the crosswalk pin")
    },
    .package = "whep"
  )
  out <- suppressMessages(whep::build_cell_polity(
    area_key = "polity_area",
    year = 1970L,
    reporting_areas = .cpy_reporting()
  ))
  testthat::expect_true(228L %in% out$area_code)
  pointblank::expect_col_vals_in_set(out, "method_cell_polity", "year_aware")
  pointblank::expect_col_exists(out, c("grid_area_code", "cell_area_frac"))
})

testthat::test_that("the year-aware path refuses missing or malformed input", {
  testthat::expect_error(
    whep::build_cell_polity(year = 1970L),
    class = "whep_cell_polity_reporting_areas"
  )
  testthat::expect_error(
    whep::build_cell_polity(year = c(1970L, 1971L), reporting_areas = 1L),
    class = "whep_cell_polity_year"
  )
  testthat::expect_error(
    whep::build_cell_polity(
      polity_fraction_path = "x.parquet",
      year = 1970L,
      reporting_areas = 1L
    ),
    "year-invariant"
  )
})

testthat::test_that("without year the crosswalk path is unchanged", {
  withr::local_envvar(WHEP_POLITY_FRACTION_PATH = "")
  payload <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~polity_frac,
    0.25, 50.25,       114L,          1.0
  )
  testthat::local_mocked_bindings(
    whep_read_file = function(...) payload,
    read_polycell_support = function(...) {
      testthat::fail("the year-invariant path must not read the polycell pin")
    },
    .package = "whep"
  )
  out <- whep::build_cell_polity()
  testthat::expect_equal(
    out,
    dplyr::mutate(payload, cell_area_ha = whep:::.cell_area_ha_lat(.data$lat))
  )
})

testthat::test_that("a malformed mapping row is refused", {
  bad <- tibble::tibble(
    polity_code = "X",
    area_code = 1L,
    start_year = 1990L,
    end_year = 1990L,
    rule = "guess"
  )
  testthat::expect_error(whep:::.cpy_check_map(bad), "invalid row")
})
