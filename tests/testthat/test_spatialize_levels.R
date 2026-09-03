# Tests for R/spatialize_levels.R, plus the T12 edits to
# R/spatialize_compartments.R and R/run_spatialize.R (whep#1000).
#
# New functions are exercised through `whep:::` so the file passes whether or
# not the exports have been rolled into NAMESPACE yet; the two exported ones
# (`read_level_country_grid()`, `build_allocation_layer()`,
# `admin_coverage_prototype()`) are also reached through `whep::` where that
# is the point of the test.
#
# NOTHING here reads the network or a `WHEP_*` path: every support and edge
# table is injected, and the one pin read is stubbed with
# `local_mocked_bindings()` (the pattern at test_run_spatialize.R:291).

# --- Fixtures ---------------------------------------------------------------

# Copied verbatim from `two_country_fixture()` in test_spatialize.R, which is a
# file-local helper and therefore not visible here. Its allocation is the
# level-0 golden this file pins.
.lv_two_country_fixture <- function() {
  list(
    country_areas = tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2000L,         1L,             15L,               1000,
      2000L,         2L,             15L,                500
    ),
    crop_patterns = tibble::tribble(
      ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 50.25,             15L,               0.8,
      0.75, 50.25,             15L,               0.2,
      1.25, 50.25,             15L,               0.5,
      1.75, 50.25,             15L,               0.5
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,           600,
      0.75, 50.25, 2000L,           400,
      1.25, 50.25, 2000L,           300,
      1.75, 50.25, 2000L,           200
    ),
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1,
      0.75, 50.25,         1L,               1,
      1.25, 50.25,         2L,               1,
      1.75, 50.25,         2L,               1
    )
  )
}

# A polycell support in the `build_polycell_support()` grain holding one
# container's two units, in the REPLACEMENT shape (the units stand instead of
# their container) plus a second country's cell so the cells are shared.
.lv_support <- function() {
  tibble::tribble(
    ~polycell_id,     ~cell_id, ~lon,  ~lat,  ~polity_code,
    "AICHI@1",              1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
    "AICHI@2",              2L, 137.75, 35.25, "JPN-AICHI-1871-2025",
    "GIFU@2",               2L, 137.75, 35.25, "JPN-GIFU-1871-2025",
    "OTHER@2",              2L, 137.75, 35.25, "USA-1959-2025"
  ) |>
    dplyr::mutate(
      area_code = NA_integer_,
      start_year = 1952L,
      end_year = 2025L,
      cell_area_ha = c(3000, 4000, 4000, 4000),
      land_area_ha = c(3000, 1000, 2000, 1000)
    )
}

.lv_containment <- function() {
  tibble::tribble(
    ~member_code,          ~container_code, ~start_year, ~end_year,
    "JPN-AICHI-1871-2025", "JPN-1952-2025",       1952L,     2025L,
    "JPN-GIFU-1871-2025",  "JPN-1952-2025",       1952L,     2025L
  ) |>
    dplyr::mutate(basis = "prefecture inside JPN-1952-2025")
}

# The T37 fixture's country A at level 1 and country B at level 0, with the
# matching level-0 grid the cross-depth assertion compares against.
.lv_layer_inputs <- function() {
  deep <- .level1_country_grid()
  grid0 <- dplyr::bind_rows(
    .level1_level0_shares(),
    deep |>
      dplyr::filter(area_code == 901L) |>
      dplyr::select(lon, lat, area_code, cell_area_frac)
  )
  list(
    grid0 = grid0,
    grid_deep = deep,
    granted = tibble::tibble(area_code = 900L, level = 1L)
  )
}

# --- read_level_country_grid: level 0 ---------------------------------------

testthat::test_that("level 0 delegates to the unchanged polycell path", {
  sentinel <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 1L,
    cell_area_ha = 100,
    land_area_ha = 90,
    cell_area_frac = 1
  )
  testthat::local_mocked_bindings(
    .read_polycell_country_grid = function() sentinel,
    .package = "whep"
  )
  testthat::expect_identical(whep:::read_level_country_grid(), sentinel)
  testthat::expect_identical(
    whep:::read_level_country_grid(level = 0L),
    sentinel
  )
})

testthat::test_that("level 0 refuses arguments it would not read", {
  testthat::expect_error(
    whep:::read_level_country_grid(level = 0L, support = .lv_support()),
    "not read at"
  )
  testthat::expect_error(
    whep:::read_level_country_grid(level = 0L, reference_year = 2000L),
    "not read at"
  )
})

testthat::test_that(".check_grid_level refuses a non-depth", {
  fn <- whep:::.check_grid_level
  testthat::expect_identical(fn(NULL), 0L)
  testthat::expect_identical(fn(2), 2L)
  testthat::expect_error(fn(-1L), "non-negative whole number")
  testthat::expect_error(fn(1.5), "non-negative whole number")
  testthat::expect_error(fn(c(1L, 2L)), "non-negative whole number")
})

# --- read_level_country_grid: level 1 ---------------------------------------

testthat::test_that("level 1 keys cells on the unit and the container code", {
  grid <- whep:::read_level_country_grid(
    level = 1L,
    support = .lv_support(),
    containment = .lv_containment()
  )
  testthat::expect_setequal(
    names(grid),
    c(
      "lon",
      "lat",
      "area_code",
      "level_polity_code",
      "level",
      "cell_area_frac",
      "polycell_id",
      "start_year",
      "end_year",
      "cell_area_ha",
      "land_area_ha"
    )
  )
  # Japan's reporting code, resolved from the CONTAINER polity through the
  # package's own lookup and never from a name.
  testthat::expect_identical(unique(grid$area_code), 110L)
  testthat::expect_identical(unique(grid$level), 1L)
  testthat::expect_setequal(
    grid$level_polity_code,
    c("JPN-AICHI-1871-2025", "JPN-AICHI-1871-2025", "JPN-GIFU-1871-2025")
  )
  # `land_area_ha / cell_land_ha` on the land basis: cell 2 holds
  # 1000 + 2000 + 1000 = 4000 ha of land.
  aichi_2 <- dplyr::filter(
    grid,
    lon == 137.75,
    level_polity_code == "JPN-AICHI-1871-2025"
  )
  testthat::expect_equal(aichi_2$cell_area_frac, 0.25, tolerance = 1e-12)
  gifu <- dplyr::filter(grid, level_polity_code == "JPN-GIFU-1871-2025")
  testthat::expect_equal(gifu$cell_area_frac, 0.5, tolerance = 1e-12)
  # The cell Aichi holds alone is its whole cell.
  aichi_1 <- dplyr::filter(grid, lon == 137.25)
  testthat::expect_equal(aichi_1$cell_area_frac, 1, tolerance = 1e-12)
})

testthat::test_that("level 1 reads the support pin when none is passed", {
  testthat::local_mocked_bindings(
    read_polycell_support = function(...) .lv_support(),
    .package = "whep"
  )
  grid <- whep:::read_level_country_grid(
    level = 1L,
    containment = .lv_containment()
  )
  testthat::expect_equal(nrow(grid), 3L)
})

testthat::test_that("reference_year snapshots the grid on edge validity", {
  support <- dplyr::mutate(
    .lv_support(),
    start_year = c(1952L, 1952L, 1990L, 1952L)
  )
  containment <- dplyr::mutate(
    .lv_containment(),
    start_year = c(1952L, 1990L)
  )
  grid <- whep:::read_level_country_grid(
    level = 1L,
    support = support,
    containment = containment,
    reference_year = 1980L
  )
  testthat::expect_setequal(
    grid$level_polity_code,
    c("JPN-AICHI-1871-2025", "JPN-AICHI-1871-2025")
  )
})

testthat::test_that("an aggregate container is refused, not folded to 999", {
  containment <- tibble::tibble(
    member_code = "JPN-AICHI-1871-2025",
    container_code = "ROW-1850-2025",
    start_year = 1952L,
    end_year = 2025L,
    basis = "fixture"
  )
  testthat::expect_warning(
    testthat::expect_error(
      whep:::read_level_country_grid(
        level = 1L,
        support = .lv_support(),
        containment = containment
      ),
      class = "whep_level_no_edges"
    ),
    "aggregate container"
  )
})

testthat::test_that("a support with no rows for the depth aborts", {
  support <- dplyr::mutate(.lv_support(), polity_code = "USA-1959-2025")
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment()
    ),
    class = "whep_level_support_empty"
  )
})

testthat::test_that("a container kept beside its own units is refused", {
  # The T07 hazard: both claim the same ground and the container's national
  # total still reconciles, so nothing downstream can see it.
  support <- dplyr::bind_rows(
    .lv_support(),
    tibble::tibble(
      polycell_id = "JPN@1",
      cell_id = 1L,
      lon = 137.25,
      lat = 35.25,
      polity_code = "JPN-1952-2025",
      area_code = 110L,
      start_year = 1952L,
      end_year = 2025L,
      cell_area_ha = 3000,
      land_area_ha = 3000
    )
  )
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment()
    ),
    class = "whep_level_support_double_claim"
  )
})

testthat::test_that("a within-container share is composed, not read raw", {
  support <- .lv_support() |>
    dplyr::mutate(
      container_frac = c(1, 0.5, 0.5, NA),
      land_area_ha = c(3000, 1000, 2000, 1000)
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        polycell_id = "JPN@2",
        cell_id = 2L,
        lon = 137.75,
        lat = 35.25,
        polity_code = "JPN-1952-2025",
        area_code = 110L,
        start_year = 1952L,
        end_year = 2025L,
        cell_area_ha = 4000,
        land_area_ha = 3000,
        container_frac = NA_real_
      ),
      tibble::tibble(
        polycell_id = "JPN@1",
        cell_id = 1L,
        lon = 137.25,
        lat = 35.25,
        polity_code = "JPN-1952-2025",
        area_code = 110L,
        start_year = 1952L,
        end_year = 2025L,
        cell_area_ha = 3000,
        land_area_ha = 3000,
        container_frac = NA_real_
      )
    )
  grid <- NULL
  testthat::expect_message(
    grid <- whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment()
    ),
    "composing"
  )
  # Cell 2 land is 1000 + 2000 + 1000 + 3000 = 7000 (the container's own row is
  # part of the denominator in the nested shape), Japan's own share is
  # 3000 / 7000, and each unit takes half of it.
  gifu <- dplyr::filter(grid, level_polity_code == "JPN-GIFU-1871-2025")
  testthat::expect_equal(
    gifu$cell_area_frac,
    0.5 * (3000 / 7000),
    tolerance = 1e-12
  )
})

# --- build_allocation_layer -------------------------------------------------

testthat::test_that("the layer takes granted depth and level 0 elsewhere", {
  inputs <- .lv_layer_inputs()
  layer <- whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  )
  # Country A arrives only as units; country B only at level 0.
  a_rows <- dplyr::filter(layer, area_code == 900L)
  testthat::expect_true(all(!is.na(a_rows$level_polity_code)))
  testthat::expect_equal(nrow(a_rows), 6L)
  b_rows <- dplyr::filter(layer, area_code == 901L)
  testthat::expect_true(all(is.na(b_rows$level_polity_code)))
  testthat::expect_equal(nrow(b_rows), 2L)
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
})

testthat::test_that("assertion (a) holds on the named fixture cells", {
  inputs <- .lv_layer_inputs()
  layer <- whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  )
  totals <- layer |>
    dplyr::summarise(total = sum(cell_area_frac), .by = c(lon, lat))
  testthat::expect_equal(
    totals$total,
    rep(1, nrow(totals)),
    tolerance = 1e-12
  )
  # The A1/A2/B border cell: three compartments in one physical cell.
  border <- dplyr::filter(layer, lon == 10.75, lat == 40.25)
  testthat::expect_equal(nrow(border), 3L)
  testthat::expect_equal(
    sort(border$cell_area_frac),
    c(0.2, 0.3, 0.5),
    tolerance = 1e-12
  )
  # The A1/A2 straddling cell is the same physical cell seen from A: the two
  # units sum to A's level-0 share of 0.8, NOT to 1.
  straddle <- dplyr::filter(border, area_code == 900L)
  testthat::expect_equal(sum(straddle$cell_area_frac), 0.8, tolerance = 1e-12)
})

testthat::test_that("assertion (a) aborts on a cell that does not partition", {
  inputs <- .lv_layer_inputs()
  broken <- dplyr::mutate(
    inputs$grid_deep,
    cell_area_frac = dplyr::if_else(
      level_polity_code %in% "A-A2-1975-2100" & lon == 10.75,
      0.9,
      cell_area_frac
    )
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, broken, inputs$granted),
    class = "whep_alloc_layer_not_partition"
  )
})

testthat::test_that("assertion (a) aborts on a share that moves in time", {
  inputs <- .lv_layer_inputs()
  split <- dplyr::bind_rows(
    inputs$grid_deep,
    inputs$grid_deep |>
      dplyr::filter(level_polity_code %in% "A-A1-1900-2100", lon == 10.25) |>
      dplyr::mutate(
        start_year = 1800L,
        end_year = 1900L,
        cell_area_frac = 0.4
      )
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, split, inputs$granted),
    class = "whep_alloc_layer_varying_share"
  )
})

testthat::test_that("a compartment's repeated intervals are counted once", {
  inputs <- .lv_layer_inputs()
  repeated <- dplyr::bind_rows(
    inputs$grid_deep,
    inputs$grid_deep |>
      dplyr::filter(level_polity_code %in% "A-A1-1900-2100", lon == 10.25) |>
      dplyr::mutate(start_year = 1800L, end_year = 1900L)
  )
  layer <- whep:::build_allocation_layer(
    inputs$grid0,
    repeated,
    inputs$granted
  )
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
})

testthat::test_that("assertion (b) reports ragged cells and never fills", {
  inputs <- .lv_layer_inputs()
  # A cell where the level-0 grid gives A more than its units cover: the
  # straddling cell's A2 share is halved and B's is raised so the layer still
  # partitions, which is exactly the case (a) cannot see.
  deep <- dplyr::mutate(
    inputs$grid_deep,
    cell_area_frac = dplyr::case_when(
      level_polity_code %in% "A-A2-1975-2100" & lon == 10.75 ~ 0.25,
      area_code == 901L & lon == 10.75 ~ 0.45,
      .default = cell_area_frac
    )
  )
  grid0 <- dplyr::mutate(
    inputs$grid0,
    cell_area_frac = dplyr::if_else(
      area_code == 901L & lon == 10.75,
      0.45,
      cell_area_frac
    )
  )
  layer <- NULL
  testthat::expect_warning(
    layer <- whep:::build_allocation_layer(grid0, deep, inputs$granted),
    "ragged"
  )
  ragged <- attr(layer, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_identical(ragged$reason, "unit_share_mismatch")
  testthat::expect_equal(ragged$difference, -0.25, tolerance = 1e-12)
  # Never back-filled: no container-keyed row for A appears in the layer.
  testthat::expect_equal(
    sum(layer$area_code == 900L & is.na(layer$level_polity_code)),
    0L
  )
})

testthat::test_that("a granted container with no deep rows aborts", {
  inputs <- .lv_layer_inputs()
  testthat::expect_error(
    whep:::build_allocation_layer(
      inputs$grid0,
      inputs$grid_deep,
      tibble::tibble(area_code = c(900L, 902L), level = c(1L, 1L))
    ),
    class = "whep_alloc_layer_no_supply"
  )
})

testthat::test_that("no grant returns grid0 with an empty diagnostic", {
  inputs <- .lv_layer_inputs()
  layer <- whep:::build_allocation_layer(inputs$grid0, inputs$grid_deep, NULL)
  testthat::expect_equal(nrow(layer), nrow(inputs$grid0))
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
})

testthat::test_that("granted is validated", {
  inputs <- .lv_layer_inputs()
  testthat::expect_error(
    whep:::build_allocation_layer(
      inputs$grid0,
      inputs$grid_deep,
      tibble::tibble(area_code = 900L, level = 0L)
    ),
    "depth of at least 1"
  )
  testthat::expect_error(
    whep:::build_allocation_layer(
      inputs$grid0,
      inputs$grid_deep,
      tibble::tibble(area_code = c(900L, 900L), level = c(1L, 1L))
    ),
    "twice"
  )
})

# --- Compartment columns ----------------------------------------------------

testthat::test_that(".compartment_id_cols learns level_polity_code last", {
  fn <- whep:::.compartment_id_cols
  deep <- .level1_country_grid()
  testthat::expect_identical(
    fn(deep),
    c("polycell_id", "area_code", "level_polity_code")
  )
  # A level-0 grid is untouched: the column is simply not there.
  level0 <- dplyr::select(deep, lon, lat, area_code, cell_area_frac)
  testthat::expect_identical(fn(level0), "area_code")
  full <- dplyr::mutate(deep, cell_id = 1L)
  testthat::expect_identical(
    fn(full),
    c("polycell_id", "cell_id", "area_code", "level_polity_code")
  )
})

testthat::test_that("a level-1 grid survives .normalize_country_grid", {
  fn <- whep:::.normalize_country_grid
  deep <- dplyr::mutate(
    .level1_country_grid(),
    area_code = as.numeric(area_code)
  )
  out <- fn(deep)
  testthat::expect_type(out$area_code, "integer")
  testthat::expect_type(out$level_polity_code, "character")
  testthat::expect_type(out$level, "integer")
  testthat::expect_identical(out$level_polity_code, deep$level_polity_code)
  testthat::expect_identical(out$level, .level1_country_grid()$level)
  testthat::expect_equal(out$cell_area_frac, deep$cell_area_frac)
})

testthat::test_that(".normalize_country_grid still refuses a shareless grid", {
  fn <- whep:::.normalize_country_grid
  deep <- dplyr::select(.level1_country_grid(), -cell_area_frac)
  testthat::expect_error(fn(deep), "carries no polity share")
})

testthat::test_that(".normalize_country_grid type-checks the depth columns", {
  fn <- whep:::.normalize_country_grid
  bad_code <- dplyr::mutate(
    .level1_country_grid(),
    level_polity_code = as.factor(dplyr::coalesce(level_polity_code, "x"))
  )
  testthat::expect_error(fn(bad_code), "must be")
  bad_level <- dplyr::mutate(.level1_country_grid(), level = 0.5)
  testthat::expect_error(fn(bad_level), "containment depth")
})

# --- Level-0 equality: the golden the engine must reproduce -----------------
#
# Pinned from the engine BEFORE the `.compartment_id_cols()` change, on
# 2026-09-02, by running `build_gridded_landuse()` on each fixture at
# worktree HEAD a9edaf6d. Keys and row count are compared exactly; values at
# `tolerance = 1e-12`, because the sums follow input order and `identical()`
# is the wrong test.

.lv_golden_two_country <- function() {
  tibble::tribble(
    ~year, ~area_code,  ~lon,  ~lat, ~item_prod_code,         ~rainfed_ha,
    2000L,         2L, 1.25, 50.25,             15L, 300.00000000000000,
    2000L,         2L, 1.75, 50.25,             15L, 200.00000000000000,
    2000L,         1L, 0.25, 50.25,             15L, 773.24210837910118,
    2000L,         1L, 0.75, 50.25,             15L, 226.75789162089893
  ) |>
    dplyr::mutate(irrigated_ha = 0)
}

.lv_golden_country_b <- function() {
  tibble::tribble(
    ~year,   ~lon,  ~lat, ~item_prod_code,         ~rainfed_ha,
    1974L, 10.75, 40.25,             15L,  63.157894736842103,
    1974L, 12.25, 40.25,             15L, 236.842105263157890,
    1974L, 10.75, 40.25,             44L,  29.032258064516128,
    1974L, 12.25, 40.25,             44L, 120.967741935483858,
    1975L, 10.75, 40.25,             15L,  65.263157894736835,
    1975L, 12.25, 40.25,             15L, 244.736842105263150,
    1975L, 10.75, 40.25,             44L,  30.000000000000000,
    1975L, 12.25, 40.25,             44L, 124.999999999999986,
    1976L, 10.75, 40.25,             15L,  67.368421052631575,
    1976L, 12.25, 40.25,             15L, 252.631578947368439,
    1976L, 10.75, 40.25,             44L,  30.967741935483872,
    1976L, 12.25, 40.25,             44L, 129.032258064516128
  ) |>
    dplyr::mutate(area_code = 901L, irrigated_ha = 0)
}

.lv_sort_output <- function(result) {
  result |>
    dplyr::select(
      dplyr::any_of(c(
        "year",
        "area_code",
        "level_polity_code",
        "lon",
        "lat",
        "item_prod_code",
        "rainfed_ha",
        "irrigated_ha"
      ))
    ) |>
    dplyr::arrange(year, area_code, lon, lat, item_prod_code)
}

testthat::test_that("level 0 output is unchanged on the two-country fixture", {
  fix <- .lv_two_country_fixture()
  got <- NULL
  testthat::expect_warning(
    got <- whep::build_gridded_landuse(
      fix$country_areas,
      fix$crop_patterns,
      fix$gridded_cropland,
      fix$country_grid,
      config = list(years = 2000L)
    ),
    "capacity"
  )
  got <- .lv_sort_output(got)
  want <- .lv_sort_output(.lv_golden_two_country())
  testthat::expect_equal(nrow(got), nrow(want))
  testthat::expect_identical(
    got[c("year", "area_code", "lon", "lat", "item_prod_code")],
    want[c("year", "area_code", "lon", "lat", "item_prod_code")]
  )
  testthat::expect_equal(
    got$rainfed_ha,
    want$rainfed_ha,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    got$irrigated_ha,
    want$irrigated_ha,
    tolerance = 1e-12
  )
  testthat::expect_false("level_polity_code" %in% names(got))
})

testthat::test_that("level 0 output is unchanged on T37's country B", {
  grid <- dplyr::filter(.level1_country_grid(), area_code == 901L)
  plain <- dplyr::select(grid, lon, lat, area_code, cell_area_frac)
  areas <- dplyr::filter(.level1_country_areas(), area_code == 901L)
  # One year first, so the "cells in no polity compartment" branch is asserted
  # (B's two cells are 2 of the fixture's 6); then all three years with the
  # same warning suppressed, since it fires once per year and testthat's
  # `expect_warning()` consumes only the first.
  testthat::expect_warning(
    whep::build_gridded_landuse(
      areas,
      .level1_crop_patterns(),
      .level1_gridded_cropland(),
      plain,
      config = list(years = 1974L)
    ),
    "no polity compartment"
  )
  got <- suppressWarnings(
    whep::build_gridded_landuse(
      areas,
      .level1_crop_patterns(),
      .level1_gridded_cropland(),
      plain,
      config = list(years = c(1974L, 1975L, 1976L))
    )
  ) |>
    .lv_sort_output()
  want <- .lv_sort_output(.lv_golden_country_b())
  testthat::expect_equal(nrow(got), nrow(want))
  testthat::expect_identical(
    got[c("year", "area_code", "lon", "lat", "item_prod_code")],
    want[c("year", "area_code", "lon", "lat", "item_prod_code")]
  )
  testthat::expect_equal(got$rainfed_ha, want$rainfed_ha, tolerance = 1e-12)
  testthat::expect_equal(
    got$irrigated_ha,
    want$irrigated_ha,
    tolerance = 1e-12
  )
})

testthat::test_that("carrying the depth columns moves no level-0 value", {
  # The same country B rows, this time WITH `level_polity_code` (all `NA`) and
  # `level`. `.compartment_id_cols()` now carries the identity column through,
  # so the output gains an all-`NA` column and nothing else.
  grid <- dplyr::filter(.level1_country_grid(), area_code == 901L)
  areas <- dplyr::filter(.level1_country_areas(), area_code == 901L)
  got <- suppressWarnings(
    whep::build_gridded_landuse(
      areas,
      .level1_crop_patterns(),
      .level1_gridded_cropland(),
      grid,
      config = list(years = c(1974L, 1975L, 1976L))
    )
  ) |>
    .lv_sort_output()
  want <- .lv_sort_output(.lv_golden_country_b())
  testthat::expect_true(all(is.na(got$level_polity_code)))
  testthat::expect_equal(nrow(got), nrow(want))
  testthat::expect_equal(got$rainfed_ha, want$rainfed_ha, tolerance = 1e-12)
})

# --- Output grain -----------------------------------------------------------

testthat::test_that("a level-0 result is returned identically", {
  fold <- whep:::.level_fold_output
  result <- .lv_golden_two_country()
  testthat::expect_identical(fold(result, 0L), result)
  testthat::expect_identical(fold(result, 1L), result)
})

testthat::test_that("granted-depth rows fold onto the container by default", {
  fold <- whep:::.level_fold_output
  result <- tibble::tribble(
    ~year, ~area_code,   ~level_polity_code, ~polycell_id,  ~lon,  ~lat,
    1975L,       900L, "A-A1-1900-2100",   "A1@381260", 10.75, 40.25,
    1975L,       900L, "A-A2-1975-2100",   "A2@381260", 10.75, 40.25,
    1975L,       901L, NA_character_,     "901@384260", 12.25, 40.25
  ) |>
    dplyr::mutate(
      level = c(1L, 1L, 0L),
      item_prod_code = 15L,
      rainfed_ha = c(10, 20, 30),
      irrigated_ha = c(1, 2, 3)
    )
  folded <- NULL
  testthat::expect_message(folded <- fold(result, 0L), "folded")
  testthat::expect_equal(nrow(folded), 2L)
  testthat::expect_false("level_polity_code" %in% names(folded))
  testthat::expect_false("polycell_id" %in% names(folded))
  testthat::expect_equal(
    dplyr::filter(folded, area_code == 900L)$rainfed_ha,
    30
  )
  testthat::expect_equal(
    dplyr::filter(folded, area_code == 900L)$irrigated_ha,
    3
  )
  # And the plan's key is unique after the fold.
  key <- folded[c("lon", "lat", "area_code", "item_prod_code", "year")]
  testthat::expect_equal(sum(duplicated(key)), 0L)
  # The schema equals a level-0 run's, ORDER included: the value columns are
  # already last in the engine's output, so summarising leaves the rest where
  # they were.
  testthat::expect_identical(
    names(folded),
    setdiff(names(result), c("level_polity_code", "level", "polycell_id"))
  )
  # `output_level = 1` keeps the unit grain untouched.
  testthat::expect_identical(fold(result, 1L), result)
})

# --- run_spatialize wiring --------------------------------------------------

.lv_write_livestock_fixture <- function(dir) {
  nanoparquet::write_parquet(
    tibble::tribble(
      ~year, ~area_code, ~species_group, ~heads, ~enteric_ch4_kt,
      2000L,         1L, "cattle_dairy",  10000,             1.0
    ),
    file.path(dir, "livestock_country_data.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
      0.25, 50.25, 2000L,          500,           100,
      0.75, 50.25, 2000L,          400,            80
    ),
    file.path(dir, "gridded_pasture.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,          300,
      0.75, 50.25, 2000L,          200
    ),
    file.path(dir, "gridded_cropland.parquet")
  )
  nanoparquet::write_parquet(
    tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1,
      0.75, 50.25,         1L,               1
    ),
    file.path(dir, "country_grid.parquet")
  )
}

testthat::test_that("the four depth overrides are recognised and recorded", {
  tmp_in <- withr::local_tempdir()
  .lv_write_livestock_fixture(tmp_in)
  tmp_out <- withr::local_tempdir()

  result <- whep::run_spatialize(
    preset = "whep",
    years = 2000L,
    components = "livestock",
    overrides = list(
      country_grid = "centroid",
      level = 0L,
      output_level = 0L,
      constraint_exclude = list(USA = 1961:1963),
      livestock_proxy = "luh2"
    ),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )

  testthat::expect_identical(result$config$level, 0L)
  testthat::expect_identical(result$config$output_level, 0L)
  testthat::expect_identical(result$config$livestock_proxy, "luh2")
  testthat::expect_identical(
    result$config$constraint_exclude,
    list(USA = 1961:1963)
  )
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_true("constraint_exclude" %in% names(meta$config))
  testthat::expect_equal(meta$config$level, 0L)
  testthat::expect_equal(meta$config$output_level, 0L)
  testthat::expect_equal(meta$config$livestock_proxy, "luh2")
  testthat::expect_equal(meta$config$constraint_exclude$USA, 1961:1963)
  # No depth requested, so no coverage report is written at all.
  testthat::expect_false(file.exists(file.path(tmp_out, "admin_coverage.csv")))
})

testthat::test_that("an empty hold-out is still a recorded key", {
  tmp_in <- withr::local_tempdir()
  .lv_write_livestock_fixture(tmp_in)
  tmp_out <- withr::local_tempdir()
  whep::run_spatialize(
    preset = "whep",
    years = 2000L,
    components = "livestock",
    overrides = list(country_grid = "centroid"),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_true("constraint_exclude" %in% names(meta$config))
  testthat::expect_null(meta$config$constraint_exclude)
})

testthat::test_that("both presets carry the new keys", {
  presets <- whep:::.spatialize_presets()
  for (nm in names(presets)) {
    testthat::expect_true("level" %in% names(presets[[nm]]))
    testthat::expect_true("output_level" %in% names(presets[[nm]]))
    testthat::expect_true("constraint_exclude" %in% names(presets[[nm]]))
    testthat::expect_true("livestock_proxy" %in% names(presets[[nm]]))
    testthat::expect_identical(presets[[nm]]$level, 0L)
    testthat::expect_identical(presets[[nm]]$livestock_proxy, "luh2")
    testthat::expect_null(presets[[nm]]$constraint_exclude)
  }
  testthat::expect_true(
    all(
      c("level", "output_level", "constraint_exclude", "livestock_proxy") %in%
        whep:::.known_override_keys()
    )
  )
})

testthat::test_that("livestock_proxy reaches the livestock engine", {
  tmp_in <- withr::local_tempdir()
  .lv_write_livestock_fixture(tmp_in)
  tmp_out <- withr::local_tempdir()
  testthat::expect_error(
    whep::run_spatialize(
      preset = "whep",
      years = 2000L,
      components = "livestock",
      overrides = list(country_grid = "centroid", livestock_proxy = "glw3"),
      paths = list(input_dir = tmp_in, out_dir = tmp_out)
    ),
    "glw"
  )
})

testthat::test_that("the depth config is cross-checked before anything runs", {
  fn <- whep:::.validate_level_config
  testthat::expect_error(
    fn(list(level = 0L, output_level = 1L), "landuse"),
    "finer than"
  )
  testthat::expect_error(
    fn(
      list(level = 1L, output_level = 0L, livestock_proxy = "glw4"),
      "landuse"
    ),
    "livestock_proxy"
  )
  testthat::expect_error(
    fn(
      list(
        level = 0L,
        output_level = 0L,
        livestock_proxy = "luh2",
        constraint_exclude = list(1961:1963)
      ),
      "landuse"
    ),
    "fully named list"
  )
  ok <- fn(
    list(level = 0L, output_level = 0L, livestock_proxy = "luh2"),
    "landuse"
  )
  testthat::expect_identical(ok$level, 0L)
})

testthat::test_that("a depth run warns that livestock keeps the grid grain", {
  fn <- whep:::.validate_level_config
  testthat::expect_warning(
    fn(
      list(level = 1L, output_level = 0L, livestock_proxy = "luh2"),
      c("landuse", "livestock")
    ),
    "T15b"
  )
})

testthat::test_that("only the polycell crosswalk answers a depth request", {
  fn <- whep:::.load_country_grid
  testthat::expect_error(
    fn(NULL, "centroid", 1L),
    "carries no containment depth"
  )
  testthat::expect_error(
    fn(NULL, "fraction", 2L),
    "carries no containment depth"
  )
})

testthat::test_that(".load_country_grid forwards the depth to the reader", {
  seen <- NULL
  testthat::local_mocked_bindings(
    read_level_country_grid = function(level = 0L, ...) {
      seen <<- level
      tibble::tibble(lon = 0.25, lat = 50.25, area_code = 1L)
    },
    .package = "whep"
  )
  fn <- whep:::.load_country_grid
  fn(NULL, "polycell", 3L)
  testthat::expect_identical(seen, 3L)
  fn(NULL, "polycell", NULL)
  testthat::expect_identical(seen, 0L)
})

# --- admin_coverage.csv -----------------------------------------------------

testthat::test_that("admin_coverage.csv is written with a header and no rows", {
  tmp <- withr::local_tempdir()
  path <- whep:::.write_admin_coverage(tmp)
  testthat::expect_true(file.exists(path))
  back <- utils::read.csv(path)
  testthat::expect_equal(nrow(back), 0L)
  testthat::expect_identical(
    names(back),
    names(whep:::admin_coverage_prototype())
  )
})

testthat::test_that("a coverage table missing a column is refused", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(
    whep:::.write_admin_coverage(tmp, tibble::tibble(area_code = 1L)),
    "missing"
  )
})

testthat::test_that("run_spatialize writes the coverage report at depth", {
  # The whole run is driven off a level-1 grid supplied through the polycell
  # reader, so the depth path is exercised end to end without a pin.
  deep <- dplyr::filter(.level1_country_grid(), area_code == 901L) |>
    dplyr::mutate(level = 1L, level_polity_code = "B-B1-1850-2100")
  testthat::local_mocked_bindings(
    read_level_country_grid = function(level = 0L, ...) deep,
    .package = "whep"
  )
  tmp_in <- withr::local_tempdir()
  nanoparquet::write_parquet(
    dplyr::filter(.level1_country_areas(), area_code == 901L),
    file.path(tmp_in, "country_areas.parquet")
  )
  nanoparquet::write_parquet(
    .level1_crop_patterns(),
    file.path(tmp_in, "crop_patterns.parquet")
  )
  nanoparquet::write_parquet(
    .level1_gridded_cropland(),
    file.path(tmp_in, "gridded_cropland.parquet")
  )
  tmp_out <- withr::local_tempdir()

  result <- NULL
  testthat::expect_warning(
    result <- whep::run_spatialize(
      preset = "whep",
      years = 1975L,
      components = "landuse",
      overrides = list(
        level = 1L,
        use_type_constraint = FALSE,
        aggregate_to_cft = FALSE
      ),
      paths = list(input_dir = tmp_in, out_dir = tmp_out)
    ),
    "no polity compartment"
  )

  testthat::expect_true(file.exists(file.path(tmp_out, "admin_coverage.csv")))
  testthat::expect_identical(result$config$level, 1L)
  out <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse_crops.parquet")
  )
  # `output_level` defaults to 0, so the written crop output is folded back
  # onto the container and keeps `main`'s schema.
  testthat::expect_false("level_polity_code" %in% names(out))
  key <- out[c("lon", "lat", "area_code", "item_prod_code", "year")]
  testthat::expect_equal(sum(duplicated(key)), 0L)
})

testthat::test_that("output_level = 1 keeps the unit grain in the file", {
  deep <- dplyr::filter(.level1_country_grid(), area_code == 901L) |>
    dplyr::mutate(level = 1L, level_polity_code = "B-B1-1850-2100")
  testthat::local_mocked_bindings(
    read_level_country_grid = function(level = 0L, ...) deep,
    .package = "whep"
  )
  tmp_in <- withr::local_tempdir()
  nanoparquet::write_parquet(
    dplyr::filter(.level1_country_areas(), area_code == 901L),
    file.path(tmp_in, "country_areas.parquet")
  )
  nanoparquet::write_parquet(
    .level1_crop_patterns(),
    file.path(tmp_in, "crop_patterns.parquet")
  )
  nanoparquet::write_parquet(
    .level1_gridded_cropland(),
    file.path(tmp_in, "gridded_cropland.parquet")
  )
  tmp_out <- withr::local_tempdir()
  testthat::expect_warning(
    whep::run_spatialize(
      preset = "whep",
      years = 1975L,
      components = "landuse",
      overrides = list(
        level = 1L,
        output_level = 1L,
        use_type_constraint = FALSE,
        aggregate_to_cft = FALSE
      ),
      paths = list(input_dir = tmp_in, out_dir = tmp_out)
    ),
    "no polity compartment"
  )
  out <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse_crops.parquet")
  )
  testthat::expect_true("level_polity_code" %in% names(out))
  testthat::expect_true(all(out$level_polity_code == "B-B1-1850-2100"))
})

# --- Verification surface (ii): the real-data comparison --------------------

testthat::test_that(".compare_spatialize_outputs detects a moved value", {
  fn <- whep:::.compare_spatialize_outputs
  a <- .lv_golden_two_country()
  same <- fn(a, dplyr::slice(a, c(3L, 1L, 4L, 2L)))
  testthat::expect_true(same$identical)
  testthat::expect_equal(same$max_abs_diff, 0)
  moved <- dplyr::mutate(a, rainfed_ha = rainfed_ha + 1e-6)
  differs <- fn(a, moved, tolerance = 1e-9)
  testthat::expect_false(differs$identical)
  # The compared values are ROUNDED to the stated tolerance before hashing,
  # so the reported difference carries that rounding: it is 1e-6 to within
  # the rounding grain, not to machine precision.
  testthat::expect_equal(differs$max_abs_diff, 1e-6, tolerance = 1e-3)
  # Below the stated tolerance the two hash the same, by design.
  tiny <- dplyr::mutate(a, rainfed_ha = rainfed_ha + 1e-12)
  testthat::expect_true(fn(a, tiny, tolerance = 1e-9)$identical)
})

testthat::test_that(".spatialize_peak_mb returns a positive R-heap figure", {
  peak <- whep:::.spatialize_peak_mb()
  testthat::expect_true(is.numeric(peak) && length(peak) == 1L && peak > 0)
})

testthat::test_that("a level-0 run reproduces a main-built reference", {
  # VERIFICATION SURFACE (ii). Local parquet only -- never `whep_read_file()`,
  # never a pin, never the network.
  #
  #   WHEP_SPATIALIZE_OUT_DIR : a level-0 run of this branch
  #   WHEP_SPATIALIZE_REF_DIR : the same run built on `main`
  #
  # Both must hold `gridded_landuse_crops.parquet`. Contents are compared
  # sorted, at `tolerance` (default 1e-9 ha), and rows plus the R-heap peak are
  # reported so the memory floor can be quoted in the PR body.
  testthat::skip_on_cran()
  out_dir <- Sys.getenv("WHEP_SPATIALIZE_OUT_DIR", "")
  ref_dir <- Sys.getenv("WHEP_SPATIALIZE_REF_DIR", "")
  testthat::skip_if(
    !nzchar(out_dir) || !nzchar(ref_dir),
    "Set WHEP_SPATIALIZE_OUT_DIR and WHEP_SPATIALIZE_REF_DIR to compare runs."
  )
  new_path <- file.path(out_dir, "gridded_landuse_crops.parquet")
  ref_path <- file.path(ref_dir, "gridded_landuse_crops.parquet")
  testthat::skip_if(
    !file.exists(new_path) || !file.exists(ref_path),
    "Both directories must hold gridded_landuse_crops.parquet."
  )
  new <- nanoparquet::read_parquet(new_path)
  reference <- nanoparquet::read_parquet(ref_path)
  cmp <- whep:::.compare_spatialize_outputs(new, reference, tolerance = 1e-9)
  cli::cli_inform(c(
    "i" = "rows new {cmp$rows_new} / reference {cmp$rows_reference}; \\
           max |diff| {cmp$max_abs_diff}; \\
           R-heap peak {whep:::.spatialize_peak_mb()} MB."
  ))
  testthat::expect_equal(cmp$rows_new, cmp$rows_reference)
  testthat::expect_true(cmp$identical)
})
