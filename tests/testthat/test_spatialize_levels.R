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

# Every condition a call raised, so a test about one warning is not silenced
# by another warning arriving first. `testthat::expect_warning()` matches the
# FIRST warning only, and several of these paths legitimately raise more than
# one.
.pr_conditions <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(value = value, warnings = warnings)
}

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

# `.lv_layer_inputs()` with one cell only partly claimed, which is the shape
# the LIVE level-0 grid has: read off the `20260825T102349Z-1a0eb` polycell
# support pin, 37 of its 66,709 cells sum below 1, the worst claimed to
# 0.01156708 at (20.75, 42.75), and none sums above 1. (The counts were 40 of
# 68,546 on `20260827T190201Z-f82a2`, which books inland water and ice as
# land -- whep#1010.) Country B's own cell (12.25, 40.25) keeps 0.6 of its
# land and the remaining 0.4 is claimed by no reporting polity. `land_area_ha`
# is carried so the shortfall can be reported in hectares: 600 ha claimed out
# of 1000.
#
# BOTH sides carry it, at one cell land of 1000 ha throughout. A layer with the
# column on one side only is a different case with its own test: the missing
# rows read as 0 ha claimed while their share still counts, so every hectare
# comes out under-stated, and the fixture that measures the shortfall must not
# be that layer.
.lv_short_layer_inputs <- function() {
  inputs <- .lv_layer_inputs()
  inputs$grid0 <- inputs$grid0 |>
    dplyr::mutate(
      cell_area_frac = dplyr::if_else(
        area_code == 901L & lon == 12.25,
        0.6,
        cell_area_frac
      ),
      land_area_ha = cell_area_frac * 1000
    )
  inputs$grid_deep <- dplyr::mutate(
    inputs$grid_deep,
    land_area_ha = cell_area_frac * 1000
  )
  inputs
}

# --- read_level_country_grid: level 0 ---------------------------------------

testthat::test_that("the 2015 snapshot delegates to the unchanged path", {
  # T39 leaves the 2015 snapshot as the default and adds the year-aware read
  # beside it; the snapshot is asserted here both as the default and by name.
  # The delegation itself is what must not move: this is the bit-identity
  # claim for every run made before whep#1000 T39.
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
  testthat::expect_identical(
    whep:::read_level_country_grid(grid_vintage = "snapshot_2015"),
    sentinel
  )
  testthat::expect_identical(
    whep:::read_level_country_grid(
      level = 0L,
      grid_vintage = "snapshot_2015"
    ),
    sentinel
  )
})

testthat::test_that("the 2015 snapshot refuses arguments it would not read", {
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 0L,
      support = .lv_support(),
      grid_vintage = "snapshot_2015"
    ),
    "not read at"
  )
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 0L,
      reference_year = 2000L,
      grid_vintage = "snapshot_2015"
    ),
    "not read at"
  )
})

# --- read_level_country_grid: grid_vintage (whep#1000 T39) -------------------

# A support in the `build_polycell_support()` grain carrying NO `polity_code`,
# so `.carbon_rekey_area_code()` is a no-op and the fixture needs neither the
# polity vocabulary nor a pin. Four cells, chosen so the two vintages have a
# stated place to differ and a stated place to agree:
#
#   (0.25, 50.25) area 1 until 1900, area 2 after   -> a SUCCESSION
#   (0.75, 50.25) areas 1 and 2 beside an UNKEYABLE claim -> the denominator
#   (1.25, 50.25) area 3, which ends in 1900        -> absent from the snapshot
#   (1.75, 50.25) area 4 twice, in two epochs       -> the fold must not merge
#
# THE UNKEYABLE ROW IS LOAD-BEARING, and the fixture is wrong without it. A
# support every row of which the reporting vocabulary can express makes the two
# candidate denominators -- the cell's whole land, and the land of the rows that
# survive the drop -- numerically identical, so it cannot tell a share taken
# before the drop from one taken after it, and a partition check on such a
# fixture comes out at exactly 1 either way. The live `polycell_support` pin
# carries a row with no `area_code` in 17,655 cells, where the two denominators
# differ and the survivors absorb the dropped land. Here 500 of the cell's
# 1,500 ha are unkeyable, so the kept shares must sum to 2/3 and never to 1.
.lv_epoch_support <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~area_code, ~start_year, ~end_year, ~land_area_ha,
    0.25, 50.25,          1L,       1800L,     1900L,           800,
    0.25, 50.25,          2L,       1900L,     2100L,           800,
    0.75, 50.25,          1L,       1800L,     2100L,           600,
    0.75, 50.25,          2L,       1800L,     2100L,           400,
    0.75, 50.25, NA_integer_,       1800L,     2100L,           500,
    1.25, 50.25,          3L,       1800L,     1900L,           900,
    1.75, 50.25,          4L,       1800L,     1900L,           100,
    1.75, 50.25,          4L,       1900L,     2100L,           200
  ) |>
    dplyr::mutate(cell_area_ha = 1000)
}

# The year-aware read of that fixture. It warns, every time, about the
# unkeyable row -- asserted on its own below -- so the warning is silenced here
# rather than repeated in each test that is about something else.
.lv_epoch_grid <- function(support = .lv_epoch_support(), ...) {
  withCallingHandlers(
    whep:::read_level_country_grid(
      level = 0L,
      support = support,
      grid_vintage = "year_aware",
      ...
    ),
    warning = function(w) {
      if (grepl("carry\\s+no", conditionMessage(w))) {
        rlang::cnd_muffle(w)
      }
    }
  )
}

testthat::test_that("grid_vintage is validated against a closed vocabulary", {
  testthat::expect_error(
    whep:::read_level_country_grid(grid_vintage = "2015"),
    class = "rlang_error"
  )
  # The default is the 2015 snapshot, held there against T31(j)'s stated
  # preference on the T39 measurement: a year-aware grid has no cell for the
  # Soviet successor states in 1961, and dropping them loses 17.2% of the
  # world's harvested area. Pinned so a flip is a deliberate edit here.
  testthat::expect_identical(
    whep:::.check_grid_vintage(NULL),
    "snapshot_2015"
  )
  testthat::expect_identical(
    whep:::.grid_vintages(),
    c("snapshot_2015", "year_aware")
  )
})

testthat::test_that("the year-aware read reports what it cannot key", {
  # The drop is the reason the denominator has to be taken before it, so the
  # run must be able to see how much land left the ledger.
  testthat::expect_warning(
    whep:::read_level_country_grid(
      level = 0L,
      support = .lv_epoch_support(),
      grid_vintage = "year_aware"
    ),
    "carry\\s+no"
  )
})

testthat::test_that("the year-aware level-0 grid carries its epochs", {
  grid <- .lv_epoch_grid()
  testthat::expect_true(whep:::.country_grid_is_dynamic(grid))
  testthat::expect_setequal(
    names(grid),
    c(
      "lon",
      "lat",
      "area_code",
      "cell_area_ha",
      "land_area_ha",
      "cell_area_frac",
      "start_year",
      "end_year"
    )
  )
  testthat::expect_false(rlang::has_name(grid, "level_polity_code"))
})

testthat::test_that("a granted depth does not read grid_vintage", {
  # Year-aware by construction, so the argument has nothing to select. It must
  # not ABORT under the shipped default either, which is the snapshot: that
  # would make every depth run fail on a key it never reads.
  testthat::expect_message(
    grid <- whep:::read_level_country_grid(
      level = 1L,
      support = .lv_support(),
      containment = .lv_containment(),
      grid_vintage = "snapshot_2015"
    ),
    "not read at"
  )
  testthat::expect_identical(unique(grid$level), 1L)

  # Silent when nothing was supplied: the notice exists for a caller who set
  # the key, not as a line every depth run prints about a key it never read.
  testthat::expect_no_message(
    whep:::read_level_country_grid(
      level = 1L,
      support = .lv_support(),
      containment = .lv_containment()
    ),
    message = "not read at"
  )
})

testthat::test_that("year-aware level 0 refuses a containment edge", {
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 0L,
      support = .lv_epoch_support(),
      containment = .lv_containment(),
      grid_vintage = "year_aware"
    ),
    "not read at"
  )
})

testthat::test_that("the two vintages differ in exactly the moved cells", {
  # THE MACHINE CRITERION (whep#1000 T39). The snapshot side is the very
  # function the level-0 loader calls, handed the same support, so this
  # compares the two READS rather than two re-implementations. At 2015 they
  # must agree ROW FOR ROW, `cell_area_frac` included: the same rows are
  # valid, so the same denominator has to be taken over them. It is the check
  # that fails if the year-aware share is taken over the surviving land
  # instead of the cell's whole land -- 0.6 / 0.4 against the snapshot's
  # 0.4 / 0.2667 at (0.75, 50.25).
  support <- .lv_epoch_support()
  snapshot <- suppressWarnings(
    tibble::as_tibble(whep:::.carbon_cell_support(support))
  )
  aware <- .lv_epoch_grid(support)

  at_2015 <- whep:::.filter_country_grid_year(aware, 2015L) |>
    dplyr::select(dplyr::all_of(names(snapshot))) |>
    dplyr::arrange(lon, lat, area_code)
  testthat::expect_equal(
    at_2015,
    dplyr::arrange(snapshot, lon, lat, area_code)
  )

  at_1850 <- whep:::.filter_country_grid_year(aware, 1850L)
  # (0.75, 50.25) is held by the same two areas in the same proportions in
  # both vintages and at both years: the "agree everywhere else" half. Their
  # shares are 600/1500 and 400/1500 -- the unkeyable 500 ha stay in the
  # denominator and are attributed to nobody.
  agreed <- dplyr::filter(at_1850, lon == 0.75)
  testthat::expect_identical(agreed$area_code, c(1L, 2L))
  testthat::expect_equal(agreed$cell_area_frac, c(0.4, 400 / 1500))
  # The two moved cells, and only those two.
  moved <- dplyr::anti_join(
    dplyr::select(at_1850, lon, lat, area_code),
    dplyr::select(snapshot, lon, lat, area_code),
    by = c("lon", "lat", "area_code")
  )
  testthat::expect_equal(
    moved,
    tibble::tibble(
      lon = c(0.25, 1.25),
      lat = c(50.25, 50.25),
      area_code = c(1L, 3L)
    )
  )
  # A cell whose only polity has ended is ABSENT year-aware, not folded onto
  # its modern holder: 1.25 exists at 1850 and not at 2015.
  testthat::expect_false(1.25 %in% at_2015$lon)
  testthat::expect_true(1.25 %in% at_1850$lon)
})

testthat::test_that("year-aware shares are taken over the cell's whole land", {
  # NOT "the shares sum to 1". That assertion is satisfied BY the defect it is
  # meant to catch: renormalising the survivors over their own land forces
  # every cell to exactly 1, so an all-ones result is the bug's signature and
  # not evidence of anything. What has to hold is the property
  # `.carbon_support_to_area_code()` states in its own comment -- the
  # denominator is the cell's WHOLE measured land, taken before any row is
  # dropped -- so a hectare the reporting vocabulary cannot express is
  # attributed to NOBODY rather than absorbed by the neighbours it shares the
  # cell with.
  support <- .lv_epoch_support()
  aware <- .lv_epoch_grid(support)
  purrr::walk(c(1850L, 1899L, 1900L, 2015L), function(yr) {
    slice <- whep:::.filter_country_grid_year(aware, yr)
    whole <- whep:::.filter_country_grid_year(support, yr) |>
      dplyr::summarise(whole_land = sum(land_area_ha), .by = c(lon, lat))
    got <- slice |>
      dplyr::summarise(
        total = sum(cell_area_frac),
        kept_land = sum(land_area_ha),
        .by = c(lon, lat)
      ) |>
      dplyr::left_join(whole, by = c("lon", "lat"))
    testthat::expect_equal(
      got$total,
      got$kept_land / got$whole_land,
      tolerance = 1e-12
    )
    # And the cell that has an unkeyable claim is BELOW 1 by exactly it.
    shared <- dplyr::filter(got, lon == 0.75)
    testthat::expect_equal(shared$total, 1000 / 1500, tolerance = 1e-12)
  })
})

testthat::test_that("the epoch fold does not merge a code with itself", {
  # Area 4 holds cell (1.75, 50.25) in two successive epochs with different
  # land. Folding on `(lon, lat, area_code)` alone -- the snapshot's key --
  # would sum 100 and 200 into one 300-hectare row that exists in neither
  # epoch, and the share would still come out at 1.
  aware <- .lv_epoch_grid()
  cell <- dplyr::filter(aware, lon == 1.75)
  testthat::expect_identical(nrow(cell), 2L)
  testthat::expect_equal(sort(cell$land_area_ha), c(100, 200))
  testthat::expect_equal(cell$cell_area_frac, c(1, 1))
})

testthat::test_that("polities sharing a code in one epoch are folded once", {
  support <- .lv_epoch_support()
  support$area_code[support$lon == 0.75 & !is.na(support$area_code)] <- 1L
  testthat::expect_warning(
    grid <- .lv_epoch_grid(support),
    "fold more than one"
  )
  cell <- dplyr::filter(grid, lon == 0.75)
  testthat::expect_identical(nrow(cell), 1L)
  # The fold sums the two KEYABLE claims and no more; the unkeyable 500 ha are
  # neither folded in nor renormalised away.
  testthat::expect_equal(cell$land_area_ha, 1000)
  testthat::expect_equal(cell$cell_area_frac, 1000 / 1500)
})

testthat::test_that("reference_year snapshots the year-aware grid", {
  grid <- .lv_epoch_grid(reference_year = 1850L)
  testthat::expect_setequal(grid$area_code, c(1L, 2L, 3L, 4L))
  testthat::expect_false(any(grid$start_year >= 1900L))
})

testthat::test_that("year-aware level 0 refuses overlapping epochs in a cell", {
  # The denominator is looked up per interval-START year, so it is only
  # well defined where the intervals inside one cell either coincide or are
  # disjoint. Where they overlap without coinciding, two rows valid in the
  # same year get denominators taken over different row sets: area 1, whose
  # interval starts alone, takes 600/600 = 1 while area 2 takes 400/1000, so
  # the cell is 1.4 times claimed from 1850 on. Refused, because the shipped
  # support has no such cell and a caller's table is checked nowhere else --
  # the snapshot path never faces the question, it reads one year.
  ok <- dplyr::filter(.lv_epoch_support(), lon == 0.75, !is.na(area_code))
  overlapping <- ok
  overlapping$start_year[overlapping$area_code == 2L] <- 1850L
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 0L,
      support = overlapping,
      grid_vintage = "year_aware"
    ),
    class = "whep_level0_overlapping_epochs"
  )
  # The same two rows on identical intervals are the ordinary case.
  testthat::expect_no_error(
    whep:::read_level_country_grid(
      level = 0L,
      support = ok,
      grid_vintage = "year_aware"
    )
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
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  ))
  layer <- out$value
  # Country A arrives only as units; country B only at level 0.
  a_rows <- dplyr::filter(layer, area_code == 900L)
  testthat::expect_true(all(!is.na(a_rows$level_polity_code)))
  testthat::expect_equal(nrow(a_rows), 6L)
  b_rows <- dplyr::filter(layer, area_code == 901L)
  testthat::expect_true(all(is.na(b_rows$level_polity_code)))
  testthat::expect_equal(nrow(b_rows), 2L)
  # A's units do reproduce its level-0 share -- but only from 1975, because
  # A2's containment edge starts there. In 1900-1975 the layer holds 0.3 of
  # the straddling cell for A where level 0 gives it 0.8, and that is ragged
  # coverage in those years however it sums over the whole layer. Summing the
  # cell across its epochs (or reading it at its fullest one alone) counted
  # A2's share in years A2 did not exist, and reported nothing.
  ragged <- attr(layer, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_equal(ragged$lon, 10.75)
  testthat::expect_identical(ragged$start_year, 1900L)
  testthat::expect_identical(ragged$end_year, 1975L)
  testthat::expect_identical(ragged$reason, "unit_share_mismatch")
  testthat::expect_equal(ragged$unit_share, 0.3, tolerance = 1e-12)
  testthat::expect_equal(ragged$level0_share, 0.8, tolerance = 1e-12)
})

testthat::test_that("assertion (a) holds on the named fixture cells", {
  inputs <- .lv_layer_inputs()
  layer <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  ))$value
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

testthat::test_that("a cell no polity fully claims is reported, not refused", {
  inputs <- .lv_short_layer_inputs()
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  ))
  layer <- out$value
  testthat::expect_true(
    any(grepl("no reporting polity claims", out$warnings))
  )
  unclaimed <- attr(layer, "unclaimed_land")
  short <- dplyr::filter(unclaimed, lon == 12.25)
  testthat::expect_equal(nrow(short), 1L)
  testthat::expect_equal(short$lat, 40.25)
  testthat::expect_equal(short$claimed_share, 0.6, tolerance = 1e-12)
  testthat::expect_equal(short$unclaimed_share, 0.4, tolerance = 1e-12)
  testthat::expect_equal(short$unclaimed_ha, 400, tolerance = 1e-9)
  # B's cell carries no validity interval, so the layer describes it in one
  # epoch and both bounds are reported as absent rather than as the sentinel
  # the sweep places its events on.
  testthat::expect_true(is.na(short$start_year))
  testthat::expect_true(is.na(short$end_year))
  # The short cell keeps its own row: nothing is dropped, and the shortfall is
  # never back-filled onto whoever else is in the cell.
  kept <- dplyr::filter(layer, lon == 12.25, lat == 40.25)
  testthat::expect_equal(nrow(kept), 1L)
  testthat::expect_equal(kept$cell_area_frac, 0.6, tolerance = 1e-12)
})

testthat::test_that("a cell short in a later epoch is reported at it", {
  # The straddling cell (10.75, 40.25) holds B's 0.2 always, A1's 0.3 from
  # 1900 and A2's 0.5 from 1975: whole from 1975, and short before and after.
  # Every one of those epochs is a row, and the hectares are that epoch's own.
  inputs <- .lv_short_layer_inputs()
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  ))
  # The unbounded epoch first, then by year: `arrange()` puts `NA` last.
  straddle <- attr(out$value, "unclaimed_land") |>
    dplyr::filter(lon == 10.75) |>
    dplyr::arrange(!is.na(start_year), start_year)
  testthat::expect_equal(nrow(straddle), 3L)
  testthat::expect_identical(straddle$start_year, c(NA, 1900L, 2100L))
  testthat::expect_identical(straddle$end_year, c(1900L, 1975L, NA))
  testthat::expect_equal(
    straddle$claimed_share,
    c(0.2, 0.5, 0.2),
    tolerance = 1e-12
  )
  # 1000 ha of cell land throughout, so the shortfall is that epoch's share of
  # it and not the fullest epoch's.
  testthat::expect_equal(
    straddle$unclaimed_ha,
    c(800, 500, 800),
    tolerance = 1e-9
  )
  # The epoch 1975-2100 is whole and is NOT a row.
  testthat::expect_false(any(straddle$start_year %in% 1975L))
})

testthat::test_that("the shortfall has no hectares without land on the layer", {
  inputs <- .lv_short_layer_inputs()
  grid0 <- dplyr::select(inputs$grid0, -"land_area_ha")
  deep <- dplyr::select(inputs$grid_deep, -"land_area_ha")
  out <- .pr_conditions(
    whep:::build_allocation_layer(grid0, deep, inputs$granted)
  )
  testthat::expect_true(
    any(grepl("no land column on the layer", out$warnings))
  )
  unclaimed <- attr(out$value, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 4L)
  # Not measurable is reported as such, never as a shortfall of 0 ha.
  testthat::expect_true(all(is.na(unclaimed$unclaimed_ha)))
})

testthat::test_that("a half-filled land column is not silently under-read", {
  # `land_area_ha` on one side only: the rows without it read as 0 ha claimed
  # while their share still counts, so the shortfall comes out scaled by less
  # land than is actually claimed -- 200 ha where 500 is unclaimed, in the
  # 1900-1975 epoch of the straddling cell. The column being PRESENT is not
  # the same statement as its being FILLED, so the gap is said out loud.
  inputs <- .lv_short_layer_inputs()
  deep <- dplyr::select(inputs$grid_deep, -"land_area_ha")
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    deep,
    inputs$granted
  ))
  testthat::expect_true(any(grepl("carry no", out$warnings)))
  testthat::expect_true(any(grepl("under-state", out$warnings)))
  partial <- attr(out$value, "unclaimed_land") |>
    dplyr::filter(lon == 10.75, start_year %in% 1900L)
  testthat::expect_equal(partial$unclaimed_ha, 200, tolerance = 1e-9)
})

testthat::test_that("an epoch nobody claims has no hectares, and says so", {
  # The cell's land is recovered from the claimed land and the claimed share,
  # so an epoch claimed by NOBODY is 0/0: the layer never measures that cell's
  # land there. It came back as `NaN`, which `is.na()` accepts, so the warning
  # took the whole layer for one carrying no land column at all and told the
  # reader to go and fix the wrong thing.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~land_area_ha,
    20.25, 60.25,       901L,             0.0,             0,
    21.25, 60.25,       900L,             1.0,          3000
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1850-2100",     1L,             1.0
  ) |>
    dplyr::mutate(land_area_ha = 3000)
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  unclaimed <- attr(out$value, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_equal(unclaimed$claimed_share, 0)
  testthat::expect_true(is.na(unclaimed$unclaimed_ha))
  testthat::expect_false(is.nan(unclaimed$unclaimed_ha))
  # The layer DOES carry land, and the warning must not say otherwise.
  testthat::expect_true(rlang::has_name(out$value, "land_area_ha"))
  testthat::expect_false(any(grepl("no land column", out$warnings)))
  testthat::expect_true(any(grepl("claimed by nobody", out$warnings)))
})

testthat::test_that("the unclaimed headline is an area, not hectare-epochs", {
  # A cell short in more than one epoch contributes its land ONCE PER EPOCH to
  # a sum over the report's rows, so that sum is hectare-epochs and not an
  # area: on the shipped support it reads 6182.76 Mha where the land involved
  # is 2951.11, sitting in the same sentence as a per-year figure a reader
  # compares against global land. This cell is 10 Mha, 4 of which nobody
  # claims, in each of two epochs: the headline is 4 Mha, not 8.
  grid0 <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    0.25, 0.25,       900L,             0.6,       1850L,     1950L,
    0.25, 0.25,       900L,             0.6,       1950L,     2050L
  ) |>
    dplyr::mutate(land_area_ha = 6e6)
  deep <- grid0 |>
    dplyr::mutate(
      level_polity_code = paste0("A-A1-", start_year),
      level = 1L
    )
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  testthat::expect_equal(nrow(attr(out$value, "unclaimed_land")), 2L)
  said <- gsub("\\s+", " ", paste(out$warnings, collapse = " "))
  testthat::expect_match(said, "4 Mha")
  testthat::expect_match(said, "each cell counted once")
  testthat::expect_false(grepl("8 Mha", said))
})

testthat::test_that("a cell claimed twice aborts even beside a short cell", {
  inputs <- .lv_short_layer_inputs()
  over <- dplyr::mutate(
    inputs$grid_deep,
    cell_area_frac = dplyr::if_else(
      level_polity_code %in% "A-A2-1975-2100" & lon == 10.75,
      0.9,
      cell_area_frac
    )
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, over, inputs$granted),
    class = "whep_alloc_layer_not_partition"
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, over, inputs$granted),
    "claimed twice"
  )
})

testthat::test_that("the unclaimed report is empty when every cell is whole", {
  inputs <- .lv_layer_inputs()
  cols <- c(
    "lon",
    "lat",
    "start_year",
    "end_year",
    "claimed_share",
    "unclaimed_share",
    "unclaimed_ha"
  )
  # Whole in EVERY epoch, not merely in its fullest one: the container's two
  # units cover it before and after the succession that splits the cell.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             1.0,       1850L,     1950L,
    21.25, 60.25,       900L,             1.0,       1950L,     2100L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1850-2100",     1L,             0.6,
    21.25, 60.25,       900L,   "A-A2-1850-2100",     1L,             0.4,
    21.25, 60.25,       900L,   "A-A1-1850-2100",     1L,             0.6,
    21.25, 60.25,       900L,   "A-A2-1850-2100",     1L,             0.4
  ) |>
    dplyr::mutate(
      start_year = rep(c(1850L, 1950L), each = 2L),
      end_year = rep(c(1950L, 2100L), each = 2L)
    )
  layer <- whep:::build_allocation_layer(
    grid0,
    deep,
    tibble::tibble(area_code = 900L, level = 1L)
  )
  testthat::expect_equal(nrow(attr(layer, "unclaimed_land")), 0L)
  testthat::expect_named(attr(layer, "unclaimed_land"), cols)
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
  # The no-grant path returns `grid0` unexamined, but still carries both
  # diagnostics so a caller never has to test for their presence.
  none <- whep:::build_allocation_layer(inputs$grid0, inputs$grid_deep, NULL)
  testthat::expect_equal(nrow(attr(none, "unclaimed_land")), 0L)
  testthat::expect_named(attr(none, "unclaimed_land"), cols)
})

testthat::test_that("assertion (a) aborts on two shares of one epoch", {
  # One compartment, one cell, one validity interval, two different shares:
  # a contradiction in the layer as given, which cannot be summed and is
  # refused rather than resolved by picking one.
  inputs <- .lv_layer_inputs()
  split <- dplyr::bind_rows(
    inputs$grid_deep,
    inputs$grid_deep |>
      dplyr::filter(level_polity_code %in% "A-A1-1900-2100", lon == 10.25) |>
      dplyr::mutate(cell_area_frac = 0.4)
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, split, inputs$granted),
    class = "whep_alloc_layer_varying_share"
  )
})

testthat::test_that("a share the sweep cannot read is refused, not skipped", {
  # AN NA SHARE DOES NOT FAIL ASSERTION (a); IT DISABLES IT. `cumsum()` carries
  # the NA into every later segment of the cell, and both bounds of the sweep
  # -- `claimed_share > 1 + tol` and `< 1 - tol` -- drop an NA row, so one
  # missing share silently exempts its whole cell from the check the layer
  # exists to pass. The control below is the same cell with the NA row taken
  # out: it is claimed 1.85 times over and aborts, which is exactly what the
  # NA row hid.
  grid0 <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    0.25, 0.25,       900L,             1.0,       1850L,     2000L
  )
  twice <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 0.25,       900L,     "A-A1-1900",        1L,             1.0,
    0.25, 0.25,       900L,     "A-A2-1900",        1L,            0.85
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2000L)
  granted <- tibble::tibble(area_code = 900L, level = 1L)
  testthat::expect_error(
    whep:::build_allocation_layer(grid0, twice, granted),
    class = "whep_alloc_layer_not_partition"
  )
  # The same layer with one earlier row whose share is NA. Nothing about the
  # 1.85 changes; the NA merely arrives before it in the sweep.
  masked <- dplyr::bind_rows(
    tibble::tibble(
      lon = 0.25,
      lat = 0.25,
      area_code = 900L,
      level_polity_code = "A-A0-1850",
      level = 1L,
      cell_area_frac = NA_real_,
      start_year = 1850L,
      end_year = 1900L
    ),
    twice
  )
  testthat::expect_error(
    whep:::build_allocation_layer(grid0, masked, granted),
    class = "whep_alloc_layer_share_not_finite"
  )
  testthat::expect_error(
    whep:::build_allocation_layer(grid0, masked, granted),
    "is not finite"
  )
  # `Inf` and `NaN` are refused by the same check, and by name: `Inf` used to
  # reach the partition abort, which reported it as a cell claimed twice, and
  # `NaN` used to behave like the NA.
  for (value in c(Inf, NaN)) {
    bad <- dplyr::mutate(
      masked,
      cell_area_frac = dplyr::coalesce(cell_area_frac, value)
    )
    testthat::expect_error(
      whep:::build_allocation_layer(grid0, bad, granted),
      class = "whep_alloc_layer_share_not_finite"
    )
  }
})

testthat::test_that("a negative share cannot cancel an over-claim", {
  # `.level_check_finite_share()` tested only `is.finite()`, so a negative
  # share passed it silently. Paired with a genuine over-claim in the SAME
  # cell and epoch it summed back to exactly 1: country 900 claims 1.4 of
  # the cell (140%) while 901 claims -0.4 beside it, and 1.4 + (-0.4) = 1
  # read as a perfect partition -- 0 unclaimed rows, 0 ragged rows, no
  # abort -- although one compartment claims more ground than the cell has.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~land_area_ha,
    21.25, 60.25,       900L,             1.4,          4200,
    21.25, 60.25,       901L,            -0.4,         -1200
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1900-2100",     1L,             1.4
  ) |>
    dplyr::mutate(land_area_ha = 4200)
  granted <- tibble::tibble(area_code = 900L, level = 1L)
  # Same class as the NA/Inf/NaN guard above: a negative share disables
  # assertion (a) exactly as effectively, just by cancelling rather than by
  # propagating.
  testthat::expect_error(
    whep:::build_allocation_layer(grid0, deep, granted),
    class = "whep_alloc_layer_share_not_finite"
  )
  testthat::expect_error(
    whep:::build_allocation_layer(grid0, deep, granted),
    "is negative"
  )
})

testthat::test_that("a share moving between epochs is read per epoch", {
  # It used to be refused outright, because the check summed a cell over the
  # whole layer and could not tell a share that MOVES from a cell claimed
  # twice. The check is now per epoch, so a share that differs between two
  # disjoint intervals of one compartment is simply read at each of them --
  # which is what the year-aware level-0 read produces on real data, in
  # 24,227 compartments of the `20260825T102349Z-1a0eb` polycell support.
  # (24,369 was the count on `20260827T190201Z-f82a2`, the pin that books
  # inland water and ice as land -- whep#1010.)
  inputs <- .lv_layer_inputs()
  moved <- dplyr::bind_rows(
    inputs$grid_deep,
    inputs$grid_deep |>
      dplyr::filter(level_polity_code %in% "A-A1-1900-2100", lon == 10.25) |>
      dplyr::mutate(
        start_year = 1800L,
        end_year = 1900L,
        cell_area_frac = 0.4
      )
  )
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    moved,
    inputs$granted
  ))
  # Cell (10.25, 40.25) is A1's alone: 0.4 of it in 1800-1900 and all of it
  # from 1900. The first epoch is short, at exactly the share it holds there,
  # and the second is whole -- neither reading is taken from the other.
  moved_cell <- attr(out$value, "unclaimed_land") |>
    dplyr::filter(lon == 10.25, lat == 40.25)
  testthat::expect_equal(nrow(moved_cell), 1L)
  testthat::expect_identical(moved_cell$start_year, 1800L)
  testthat::expect_identical(moved_cell$end_year, 1900L)
  testthat::expect_equal(moved_cell$claimed_share, 0.4, tolerance = 1e-12)
  # Assertion (b) sees the same epoch from the container's side: A holds 0.4
  # of a cell level 0 gives it whole, alongside the fixture's own 1900-1975
  # row at (10.75, 40.25).
  ragged <- attr(out$value, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 2L)
  testthat::expect_equal(sort(ragged$lon), c(10.25, 10.75))

  # The epoch where the compartment claims MORE than the cell holds is still
  # refused, which is the half assertion (a) exists for.
  over <- dplyr::mutate(
    moved,
    cell_area_frac = dplyr::if_else(
      level_polity_code %in% "A-A1-1900-2100" & start_year == 1800L,
      1.4,
      cell_area_frac
    )
  )
  testthat::expect_error(
    whep:::build_allocation_layer(inputs$grid0, over, inputs$granted),
    class = "whep_alloc_layer_not_partition"
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
  out <- .pr_conditions(whep:::build_allocation_layer(
    inputs$grid0,
    repeated,
    inputs$granted
  ))
  # Carrying the SAME share over a second, earlier interval adds a whole epoch
  # rather than a second claim: cell (10.25, 40.25) is A1's alone in both, so
  # neither is short and neither is ragged. Only the fixture's own 1900-1975
  # row survives, at the other cell.
  ragged <- attr(out$value, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_equal(ragged$lon, 10.75)
  unclaimed <- attr(out$value, "unclaimed_land")
  testthat::expect_equal(nrow(dplyr::filter(unclaimed, lon == 10.25)), 0L)
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
  out <- .pr_conditions(
    whep:::build_allocation_layer(grid0, deep, inputs$granted)
  )
  layer <- out$value
  testthat::expect_true(any(grepl("ragged", out$warnings)))
  ragged <- attr(layer, "ragged_coverage") |> dplyr::arrange(start_year)
  # Two epochs of the straddling cell, and the halved A2 share is the second:
  # 1900-1975 is A1's 0.3 against A's 0.8, 1975-2100 is 0.3 + 0.25 against the
  # same 0.8. Both are read against the level-0 share of THAT epoch.
  testthat::expect_equal(nrow(ragged), 2L)
  testthat::expect_true(all(ragged$reason == "unit_share_mismatch"))
  testthat::expect_identical(ragged$start_year, c(1900L, 1975L))
  testthat::expect_equal(
    ragged$difference,
    c(-0.5, -0.25),
    tolerance = 1e-12
  )
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
      constraint_exclude = list("840" = 1961:1963),
      livestock_proxy = "luh2"
    ),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )

  testthat::expect_identical(result$config$level, 0L)
  testthat::expect_identical(result$config$output_level, 0L)
  testthat::expect_identical(result$config$livestock_proxy, "luh2")
  # Keyed by `area_code`, which is the key `resolve_admin_shares()` reads
  # and the only key `.check_constraint_exclude()` now accepts.
  testthat::expect_identical(
    result$config$constraint_exclude,
    list("840" = 1961:1963)
  )
  meta <- yaml::read_yaml(file.path(tmp_out, "run_metadata.yaml"))
  testthat::expect_true("constraint_exclude" %in% names(meta$config))
  testthat::expect_equal(meta$config$level, 0L)
  testthat::expect_equal(meta$config$output_level, 0L)
  testthat::expect_equal(meta$config$livestock_proxy, "luh2")
  testthat::expect_equal(meta$config$constraint_exclude[["840"]], 1961:1963)
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
      list(
        level = 1L,
        output_level = 0L,
        granted_containers = 900L,
        livestock_proxy = "glw4"
      ),
      "landuse"
    ),
    "livestock_proxy"
  )
  # A depth with nothing granted, and a grant with no depth, are both refused
  # here rather than hours into a run.
  testthat::expect_error(
    fn(list(level = 1L, output_level = 0L), "landuse"),
    "grants a depth to no container"
  )
  testthat::expect_error(
    fn(
      list(level = 0L, output_level = 0L, granted_containers = 900L),
      "landuse"
    ),
    "level = 0"
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
      list(
        level = 1L,
        output_level = 0L,
        granted_containers = 900L,
        livestock_proxy = "luh2"
      ),
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
  # A depth run reads the grid TWICE -- level 0 for the ungranted countries,
  # the granted depth scoped to `containers` -- and combines them with
  # `build_allocation_layer()`. Both calls are captured, because which one
  # carries the scope and which the vintage is the whole difference between a
  # constrained run and a level-0 one wearing a depth's metadata.
  seen <- list()
  testthat::local_mocked_bindings(
    read_level_country_grid = function(
      level = 0L,
      containers = NULL,
      grid_vintage = NULL,
      double_claim = NULL,
      ...
    ) {
      seen[[length(seen) + 1L]] <<- list(
        level = level,
        containers = containers,
        grid_vintage = grid_vintage,
        double_claim = double_claim
      )
      tibble::tibble(
        lon = 0.25,
        lat = 50.25,
        area_code = 900L,
        level_polity_code = "A-A1-1900-2100",
        level = as.integer(level),
        cell_area_frac = 1
      )
    },
    .package = "whep"
  )
  fn <- whep:::.load_country_grid
  suppressMessages(fn(NULL, "polycell", 3L, "snapshot_2015", 900L, "measured"))
  testthat::expect_identical(seen[[1]]$level, 0L)
  testthat::expect_identical(seen[[1]]$grid_vintage, "year_aware")
  testthat::expect_null(seen[[1]]$containers)
  testthat::expect_identical(seen[[2]]$level, 3L)
  testthat::expect_identical(seen[[2]]$containers, 900L)
  testthat::expect_identical(seen[[2]]$double_claim, "measured")

  seen <- list()
  suppressMessages(fn(NULL, "polycell", NULL))
  testthat::expect_length(seen, 1L)
  testthat::expect_identical(seen[[1]]$level, 0L)
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

# The container-B fixture at depth: one unit over B's two cells, with the
# admin evidence a depth run now REQUIRES. Before whep#1000 T40 a `level = 1L`
# run read none of this -- it allocated the national totals on the level-0
# pattern and wrote an empty coverage header -- so these two tests exercise
# the wiring as much as the output grain.
#
# `source` names a registered family because `.admin_code_systems_for()` is a
# closed world: an undeclared label has no code system and is refused, which
# is the behaviour a fixture must not opt out of.
.lv_depth_b_grid <- function() {
  dplyr::filter(.level1_country_grid(), area_code == 901L) |>
    dplyr::mutate(level = 1L, level_polity_code = "B-B1-1850-2100")
}

.lv_depth_b_shares <- function() {
  tibble::tibble(
    area_code = 901L,
    level_polity_code = NA_character_,
    level = 1L,
    item_prod_code = c(15L, 44L),
    indicator_used = "area_harvested",
    year = 1975L,
    value = c(310, 155),
    share = NA_real_,
    source = "admin-stats-japan",
    tier = 2L,
    grain = "admin1",
    concept_break = FALSE,
    nuts_version = NA_character_,
    source_native_id = "B-B1",
    source_native_name = "B1",
    source_id = "admin-stats-japan",
    source_version = NA_character_,
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = "observed",
    value_flag = NA_character_
  )
}

.lv_local_depth_mocks <- function(grid, env = parent.frame()) {
  testthat::local_mocked_bindings(
    read_level_country_grid = function(level = 0L, ...) grid,
    read_admin_shares = function(...) {
      list(
        shares = .lv_depth_b_shares(),
        excluded = tibble::tibble(),
        not_shipped = character()
      )
    },
    resolve_admin_units = function(x, code_system, ...) {
      list(
        rows = dplyr::mutate(
          x,
          alias_source = as.character(code_system),
          level_polity_code = "B-B1-1850-2100"
        ),
        diagnostics = tibble::tibble()
      )
    },
    .package = "whep",
    .env = env
  )
}

.lv_write_depth_inputs <- function(dir) {
  nanoparquet::write_parquet(
    dplyr::filter(.level1_country_areas(), area_code == 901L),
    file.path(dir, "country_areas.parquet")
  )
  nanoparquet::write_parquet(
    .level1_crop_patterns(),
    file.path(dir, "crop_patterns.parquet")
  )
  nanoparquet::write_parquet(
    .level1_gridded_cropland(),
    file.path(dir, "gridded_cropland.parquet")
  )
}

testthat::test_that("run_spatialize writes the coverage report at depth", {
  # The coverage file used to be written with its header and NO rows whatever
  # a run did, because nothing resolved a constraint. It now carries the
  # resolver's own report, one row per container-item-year.
  .lv_local_depth_mocks(.lv_depth_b_grid())
  tmp_in <- withr::local_tempdir()
  .lv_write_depth_inputs(tmp_in)
  tmp_out <- withr::local_tempdir()

  result <- suppressWarnings(suppressMessages(whep::run_spatialize(
    preset = "whep",
    years = 1975L,
    components = "landuse",
    overrides = list(
      level = 1L,
      granted_containers = 901L,
      use_type_constraint = FALSE,
      aggregate_to_cft = FALSE
    ),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )))

  coverage <- utils::read.csv(file.path(tmp_out, "admin_coverage.csv"))
  testthat::expect_identical(
    names(coverage),
    names(whep:::admin_coverage_prototype())
  )
  testthat::expect_equal(nrow(coverage), 2L)
  testthat::expect_identical(unique(coverage$area_code), 901L)
  testthat::expect_identical(result$config$level, 1L)

  out <- nanoparquet::read_parquet(
    file.path(tmp_out, "gridded_landuse_crops.parquet")
  )
  # `output_level` defaults to 0, so the written crop output is folded back
  # onto the container and keeps `main`'s schema.
  testthat::expect_false("level_polity_code" %in% names(out))
  key <- out[c("lon", "lat", "area_code", "item_prod_code", "year")]
  testthat::expect_equal(sum(duplicated(key)), 0L)
  # The one unit reporting is the container's whole evidence, so the national
  # totals arrive whole at it and the run still conserves them.
  testthat::expect_equal(
    sum(out$rainfed_ha + out$irrigated_ha),
    310 + 155
  )
})

testthat::test_that("output_level = 1 keeps the unit grain in the file", {
  .lv_local_depth_mocks(.lv_depth_b_grid())
  tmp_in <- withr::local_tempdir()
  .lv_write_depth_inputs(tmp_in)
  tmp_out <- withr::local_tempdir()

  suppressWarnings(suppressMessages(whep::run_spatialize(
    preset = "whep",
    years = 1975L,
    components = "landuse",
    overrides = list(
      level = 1L,
      output_level = 1L,
      granted_containers = 901L,
      use_type_constraint = FALSE,
      aggregate_to_cft = FALSE
    ),
    paths = list(input_dir = tmp_in, out_dir = tmp_out)
  )))
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

# --- T13: two-level crop allocation -----------------------------------------
#
# THE MACHINE CRITERION is the first test below. Everything else in this
# block exists because it is a way for the allocation to be wrong while
# every national total still reconciles.

# Two units of container 900, ONE CELL EACH, one crop with a positive
# `harvest_fraction` in BOTH cells. Cell A carries 100 ha of cropland and
# cell B 1000, so unit A's 150 ha target does not fit and unit B's 100 does,
# several times over.
.t13_layer <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25,  50.25,       1L, "A-A1-1900-2100",       1L,               1,
    0.75,  50.25,       1L, "A-A2-1900-2100",       1L,               1
  )
}

.t13_patterns <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~item_prod_code, ~harvest_fraction,
    0.25,  50.25,             15L,               0.5,
    0.75,  50.25,             15L,               0.5
  )
}

.t13_cropland <- function(cell_a = 100, cell_b = 1000) {
  tibble::tribble(
    ~lon,   ~lat,  ~year, ~cropland_ha,
    0.25,  50.25, 2000L,        cell_a,
    0.75,  50.25, 2000L,        cell_b
  )
}

.t13_national <- function(ha = 250) {
  tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = ha
  )
}

.t13_shares <- function(a = 150, b = 100) {
  tibble::tibble(
    area_code = 1L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    level = 1L,
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    year = 2000L,
    value = c(a, b),
    treatment = "observed"
  )
}

.t13_allocated <- function(out) {
  out$allocation |>
    dplyr::summarise(
      ha = sum(rainfed_ha + irrigated_ha),
      .by = c("level_polity_code", "item_prod_code")
    ) |>
    dplyr::arrange(item_prod_code, level_polity_code)
}

testthat::test_that("a unit's target binds and its breach stays its own", {
  # THE MACHINE CRITERION (plan T13). Unit A's target is 150 ha into a cell
  # that holds 100; unit B's is 100 into a cell that holds 1000. Pass iff
  # A = 150 and B = 100, and the breach table reports 50 ha for A and
  # nothing for B.
  #
  # `mc_factor = "national"` because the criterion states the capacities as
  # given: at the unit basis the ceiling is the unit's OWN implied intensity
  # (T31(g)), which by construction equals its allocation, so "capacity 100"
  # is the national-factor reading of this fixture. The unit basis is
  # exercised in its own test below, and both are reported here.
  out <- NULL
  # The soft ceiling gave way and said so, with the magnitude: that warning
  # is the breach table's own summary and is asserted, not swallowed.
  testthat::expect_warning(
    out <- allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(),
      .t13_layer(),
      .t13_shares(),
      config = list(mc_factor = "national")
    ),
    "50 ha over"
  )
  got <- .t13_allocated(out)
  testthat::expect_equal(got$ha, c(150, 100), tolerance = 1e-6)

  breach <- dplyr::filter(out$breach, in_force)
  testthat::expect_equal(breach$level_polity_code, "A-A1-1900-2100")
  testthat::expect_equal(breach$over_ha, 50, tolerance = 1e-6)
  testthat::expect_equal(
    nrow(dplyr::filter(breach, level_polity_code == "A-A2-1900-2100")),
    0L
  )
  # The reported share bound: it was not rescaled away to fit (T31(c)).
  testthat::expect_equal(
    dplyr::filter(out$targets, level_polity_code == "A-A1-1900-2100")$target_ha,
    150
  )
})

testthat::test_that("the same fixture re-keyed to the container swaps them", {
  # THE GROUPING ERROR THIS TASK EXISTS TO PREVENT, pinned so it cannot come
  # back. Same pre-capacity allocation, same capacities; only
  # `.crop_group`'s grain differs. On the unit grain each unit keeps its own
  # target. On the container grain the two units are pooled, unit A's excess
  # is pushed across the border into unit B, and the container total is
  # still 250 -- so no conservation check can see it.
  #
  # The engine's ceiling is soft: the logit pass scales A back towards its
  # 100 ha ceiling, the final rescale re-inflates both to the pooled 250,
  # and the two settle at the values pinned below. The plan's "A ~ 100,
  # B ~ 150" states the SIZE and DIRECTION of the defect, not a limit this
  # loop converges to; where it converges is pinned below, and the
  # iteration sweep at the end of this test shows that is a fixed point
  # rather than wherever the cap happened to stop.
  work <- function() {
    data.table::data.table(
      area_code = 1L,
      level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
      lon = c(0.25, 0.75),
      lat = 50.25,
      item_prod_code = 15L,
      rainfed_ha = c(150, 100),
      irrigated_ha = 0,
      rf_capacity = c(100, 1000),
      ir_capacity = 0
    )
  }
  run <- function(cols) {
    out <- whep:::.redistribute_country_dt(
      work(),
      list(alloc_cols = cols, max_iterations = 1000L)
    )
    stats::setNames(out$rainfed_ha, out$level_polity_code)
  }
  unit <- run(c("area_code", "level_polity_code", "item_prod_code"))
  container <- run(c("area_code", "item_prod_code"))

  testthat::expect_equal(unname(unit), c(150, 100), tolerance = 1e-9)
  testthat::expect_equal(
    unname(container),
    c(111.18211682577976, 138.81788317422024),
    tolerance = 1e-9
  )
  # Stated as the property too, so the pin above is not the only thing
  # holding it: A loses mass to B, and the pooled total hides it.
  testthat::expect_lt(container[["A-A1-1900-2100"]], 120)
  testthat::expect_gt(container[["A-A2-1900-2100"]], 130)
  testthat::expect_equal(sum(container), sum(unit), tolerance = 1e-9)

  # And the pinned pair is the process's FIXED POINT, not a value the
  # iteration cap happened to stop at: 10 passes and 100,000 give the same
  # number to the last bit.
  runs <- purrr::map(
    c(10L, 100L, 100000L),
    \(it) {
      out <- whep:::.redistribute_country_dt(
        work(),
        list(
          alloc_cols = c("area_code", "item_prod_code"),
          max_iterations = it
        )
      )
      stats::setNames(out$rainfed_ha, out$level_polity_code)
    }
  )
  purrr::walk(
    runs,
    \(r) testthat::expect_equal(r, container, tolerance = 1e-12)
  )
})

testthat::test_that("a constrained and an unconstrained item share one pass", {
  # Decision T31(f): every item of a constrained country is allocated on
  # unit keys in ONE pass, the item without statistics taking
  # pattern-implied unit shares, under one shared capacity ceiling.
  patterns <- dplyr::bind_rows(
    .t13_patterns(),
    tibble::tribble(
      ~lon,   ~lat, ~item_prod_code, ~harvest_fraction,
      0.25,  50.25,             44L,              0.25,
      0.75,  50.25,             44L,              0.75
    )
  )
  national <- dplyr::bind_rows(
    .t13_national(),
    tibble::tibble(
      year = 2000L,
      area_code = 1L,
      item_prod_code = 44L,
      harvested_area_ha = 200
    )
  )
  out <- allocate_level_crops(
    national,
    patterns,
    .t13_cropland(cell_a = 100, cell_b = 1000),
    .t13_layer(),
    .t13_shares()
  )
  got <- .t13_allocated(out)

  # Item 15 keeps its reported shares; item 44 splits by the pattern-implied
  # weights, 0.25 * 100 against 0.75 * 1000, and its national total holds.
  testthat::expect_equal(got$ha[got$item_prod_code == 15L], c(150, 100))
  implied <- 200 * c(25, 750) / 775
  testthat::expect_equal(
    got$ha[got$item_prod_code == 44L],
    implied,
    tolerance = 1e-6
  )
  testthat::expect_equal(sum(got$ha[got$item_prod_code == 44L]), 200)
  testthat::expect_setequal(
    out$targets$method_crop_alloc,
    c("admin_area_shares", "pattern_implied")
  )
  # ONE ceiling, and it sees both items: unit A1 is asked for 150 + 6.45 in
  # a 100 ha cell, and the national-basis breach is attributed pro rata.
  national_breach <- dplyr::filter(
    out$breach,
    mc_basis == "national",
    level_polity_code == "A-A1-1900-2100"
  )
  testthat::expect_setequal(national_breach$item_prod_code, c(15L, 44L))
  over <- sum(national_breach$over_ha)
  testthat::expect_equal(over, 150 + implied[[1L]] - 100, tolerance = 1e-6)
  testthat::expect_equal(
    national_breach$over_ha[national_breach$item_prod_code == 15L] / over,
    150 / (150 + implied[[1L]]),
    tolerance = 1e-6
  )
  # The factor in force is the unit's own intensity, under which the unit's
  # aggregate ceiling equals its allocation, so the same cell is not over.
  testthat::expect_true(all(out$breach$mc_basis[out$breach$in_force] == "unit"))
  testthat::expect_equal(nrow(dplyr::filter(out$breach, in_force)), 0L)
})

testthat::test_that("the irrigation split composes, and clips at 0", {
  # Unit targets take the irrigated split from the engine's own
  # `ir_potential` per unit; here all the irrigated cropland is in unit B's
  # cell, so B is handed the whole national irrigated area -- more than its
  # own 100 ha area target. The rainfed remainder is floored at 0 and the
  # clipped hectares are reported rather than moved to the unit with room.
  cropland <- .t13_cropland(cell_a = 100, cell_b = 1000) |>
    dplyr::mutate(irrigated_ha = c(0, 50))
  national <- dplyr::mutate(.t13_national(), irrigated_area_ha = 200)
  out <- NULL
  testthat::expect_warning(
    testthat::expect_warning(
      out <- allocate_level_crops(
        national,
        .t13_patterns(),
        cropland,
        .t13_layer(),
        .t13_shares(),
        config = list(mc_factor = "national")
      ),
      "clipped"
    ),
    "capacity"
  )
  targets <- dplyr::arrange(out$targets, level_polity_code)
  testthat::expect_equal(targets$irrigated_target_ha, c(0, 100))
  testthat::expect_equal(targets$rainfed_target_ha, c(150, 0))
  testthat::expect_equal(targets$irrigation_clipped_ha, c(0, 100))
  testthat::expect_true(all(targets$rainfed_target_ha >= 0))

  # The regime split survives into the grid: unit B's hectares are irrigated
  # and unit A's are not.
  by_unit <- out$allocation |>
    dplyr::summarise(
      rainfed = sum(rainfed_ha),
      irrigated = sum(irrigated_ha),
      .by = "level_polity_code"
    ) |>
    dplyr::arrange(level_polity_code)
  testthat::expect_equal(by_unit$rainfed, c(150, 0), tolerance = 1e-6)
  testthat::expect_equal(by_unit$irrigated, c(0, 100), tolerance = 1e-6)

  # The second irrigation boundary: unit B's irrigated target is twice its
  # irrigated capacity, and that breach is reported at the national factor.
  ir_breach <- dplyr::filter(out$breach, mc_basis == "national")
  testthat::expect_equal(
    ir_breach$ir_over_ha[ir_breach$level_polity_code == "A-A2-1900-2100"],
    50,
    tolerance = 1e-6
  )
})

testthat::test_that("partial coverage raises a residual on the silent units", {
  # Decision T31(a). Unit A reports 150 of a 250 ha national total and unit
  # B does not report at all, so the residual is 100 ha and it is supported
  # by unit B's cells ONLY -- it never touches unit A.
  shares <- .t13_shares()[1L, ]
  out <- allocate_level_crops(
    .t13_national(),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    shares
  )
  targets <- dplyr::arrange(out$targets, level_polity_code)
  testthat::expect_equal(targets$target_ha, c(150, 100))
  testthat::expect_equal(
    targets$method_crop_alloc,
    c("admin_area_shares", "admin_residual")
  )
  testthat::expect_equal(out$coverage$coverage, 0.5)
  testthat::expect_equal(out$coverage$residual_target_ha, 100)
  testthat::expect_equal(out$coverage$basis, "residual")

  # A reported value binds in absolute hectares while the admin sum is below
  # the national total: raising the national total moves the residual, not
  # the reporting unit.
  bigger <- allocate_level_crops(
    .t13_national(400),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    shares
  )
  moved <- dplyr::arrange(bigger$targets, level_polity_code)
  testthat::expect_equal(moved$target_ha, c(150, 250))
})

testthat::test_that("a complete unit set missing the total is refused", {
  # Decision T31(d): BOTH tolerances must break. 250 reported against 3000
  # national is 92% and 2750 ha, so it is refused; the same shape at 250
  # against 260 is 4% and 10 ha and is not; and 250 against 1000 breaks the
  # relative tolerance alone (75%, but only 750 ha) and is not either.
  testthat::expect_error(
    allocate_level_crops(
      .t13_national(3000),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 1000),
      .t13_layer(),
      .t13_shares()
    ),
    class = "whep_alloc_admin_discrepancy"
  )
  testthat::expect_no_error(
    allocate_level_crops(
      .t13_national(1000),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 1000),
      .t13_layer(),
      .t13_shares()
    )
  )
  ok <- allocate_level_crops(
    .t13_national(260),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    .t13_shares()
  )
  # Rescaled proportionally onto the national total, and the leftover is a
  # diagnostic rather than a residual unit.
  testthat::expect_equal(
    dplyr::arrange(ok$targets, level_polity_code)$target_ha,
    260 * c(150, 100) / 250
  )
  testthat::expect_equal(ok$coverage$discrepancy_ha, 10)
  testthat::expect_equal(ok$coverage$residual_target_ha, 0)
})

testthat::test_that("a unit with no pattern for a crop is filled uniformly", {
  # Decision T31(b). Unit A has no `crop_patterns` row for item 15 at all,
  # which at province grain is the common case; without the extension its
  # 150 ha target has no engine row and is dropped. With it, the target is
  # spread uniformly over unit A's own cropland.
  patterns <- .t13_patterns()[2L, ]
  out <- allocate_level_crops(
    .t13_national(),
    patterns,
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    .t13_shares()
  )
  got <- .t13_allocated(out)
  testthat::expect_equal(got$ha, c(150, 100), tolerance = 1e-6)
  testthat::expect_equal(
    sum(out$conservation$difference_ha),
    0,
    tolerance = 1e-6
  )
})

testthat::test_that("straddle, breach and conservation tables are emitted", {
  # A three-cell layer: unit A and unit B share the middle cell, and unit B
  # also shares a cell with another country.
  layer <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25,  50.25,       1L, "A-A1-1900-2100",       1L,             1.0,
    0.75,  50.25,       1L, "A-A1-1900-2100",       1L,             0.5,
    0.75,  50.25,       1L, "A-A2-1900-2100",       1L,             0.5,
    1.25,  50.25,       1L, "A-A2-1900-2100",       1L,             0.4,
    1.25,  50.25,       2L, NA_character_,          0L,             0.6
  )
  patterns <- tibble::tribble(
    ~lon,   ~lat, ~item_prod_code, ~harvest_fraction,
    0.25,  50.25,             15L,               0.5,
    0.75,  50.25,             15L,               0.5,
    1.25,  50.25,             15L,               0.5
  )
  cropland <- tibble::tribble(
    ~lon,   ~lat,  ~year, ~cropland_ha,
    0.25,  50.25, 2000L,          1000,
    0.75,  50.25, 2000L,          1000,
    1.25,  50.25, 2000L,          1000
  )
  national <- dplyr::bind_rows(
    .t13_national(),
    tibble::tibble(
      year = 2000L,
      area_code = 2L,
      item_prod_code = 15L,
      harvested_area_ha = 60
    )
  )
  out <- allocate_level_crops(
    national,
    patterns,
    cropland,
    layer,
    .t13_shares()
  )
  straddle <- dplyr::filter(out$straddle, area_code == 1L)
  testthat::expect_equal(straddle$n_cells, c(2L, 2L))
  testthat::expect_true(all(straddle$cell_limited))
  # Unit A1 holds one whole cell and half of the shared one; a third of its
  # 150 ha lands in the shared cell (500 of 1500 ha of its cropland).
  testthat::expect_equal(
    straddle$straddle_sibling[straddle$level_polity_code == "A-A1-1900-2100"],
    1 / 3,
    tolerance = 1e-6
  )
  testthat::expect_equal(
    straddle$straddle_foreign[straddle$level_polity_code == "A-A1-1900-2100"],
    0
  )
  # Unit A2 is the one that meets another country, in the third cell.
  testthat::expect_gt(
    straddle$straddle_foreign[straddle$level_polity_code == "A-A2-1900-2100"],
    0
  )
  # Conservation on the constrained path, at both grains.
  testthat::expect_true(all(abs(out$conservation$difference_ha) < 1e-9))
  testthat::expect_setequal(out$conservation$grain, c("container", "unit"))
})

testthat::test_that("a level-0 country inside a granted layer is unchanged", {
  # Decision T31(f)'s stated consequence: unconstrained COUNTRIES are
  # bit-identical, and only unconstrained items inside a constrained country
  # move. Container 901 is at level 0 in the same layer and must land
  # exactly where `build_gridded_landuse()` puts it on its own.
  layer <- dplyr::bind_rows(
    .t13_layer(),
    tibble::tibble(
      lon = 1.25,
      lat = 50.25,
      area_code = 2L,
      level_polity_code = NA_character_,
      level = 0L,
      cell_area_frac = 1
    )
  )
  patterns <- dplyr::bind_rows(
    .t13_patterns(),
    tibble::tibble(
      lon = 1.25,
      lat = 50.25,
      item_prod_code = 15L,
      harvest_fraction = 0.5
    )
  )
  cropland <- dplyr::bind_rows(
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    tibble::tibble(lon = 1.25, lat = 50.25, year = 2000L, cropland_ha = 700)
  )
  national <- dplyr::bind_rows(
    .t13_national(),
    tibble::tibble(
      year = 2000L,
      area_code = 2L,
      item_prod_code = 15L,
      harvested_area_ha = 60
    )
  )
  out <- allocate_level_crops(
    national,
    patterns,
    cropland,
    layer,
    .t13_shares()
  )
  alone <- suppressWarnings(
    whep::build_gridded_landuse(
      dplyr::filter(national, area_code == 2L),
      patterns,
      cropland,
      dplyr::tibble(
        lon = 1.25,
        lat = 50.25,
        area_code = 2L,
        cell_area_frac = 1
      )
    )
  )
  got <- dplyr::filter(out$allocation, area_code == 2L)
  testthat::expect_identical(got$rainfed_ha, alone$rainfed_ha)
  testthat::expect_identical(got$irrigated_ha, alone$irrigated_ha)
  testthat::expect_equal(
    dplyr::filter(out$targets, area_code == 2L)$method_crop_alloc,
    "pattern_national"
  )
})

testthat::test_that("a back-cast share sets the shape and records the regime", {
  # `backcast_admin_shares()` produces rows with a `share` and no `value`:
  # the shares already sum to 1 over the units they cover, so they are used
  # as they stand and the regime says where they came from.
  shares <- .t13_shares() |>
    dplyr::mutate(
      value = NA_real_,
      share = c(0.7, 0.3),
      treatment = "backcast_t0_geometry"
    )
  out <- allocate_level_crops(
    .t13_national(),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    shares
  )
  targets <- dplyr::arrange(out$targets, level_polity_code)
  testthat::expect_equal(targets$target_ha, c(175, 75))
  testthat::expect_equal(
    unique(targets$method_crop_alloc),
    "admin_backcast_luh2"
  )
  testthat::expect_equal(out$coverage$basis, "share_normalised")
  # The longest non-observed run per (container, item) is reported, so a
  # 60-year bridge is visible rather than merely legal (decision T31(e)).
  testthat::expect_equal(out$bridges$longest_run, 1L)
  testthat::expect_equal(out$bridges$treatment, "backcast_t0_geometry")
})

testthat::test_that("a longer bridge reports its longest run", {
  bridged <- tibble::tibble(
    area_code = 1L,
    item_prod_code = 15L,
    year = c(1990:1994, 1996L),
    treatment = "luh2_bridged"
  )
  got <- whep:::.alloc_bridge_report(bridged)
  testthat::expect_equal(got$n_years, 6L)
  testthat::expect_equal(got$longest_run, 5L)
})

testthat::test_that("a production share never binds an allocation", {
  # Decision T31(i): the anchor is the first observed AREA year, and a
  # production row stays a reconciliation diagnostic.
  shares <- dplyr::mutate(.t13_shares(), indicator_used = "production")
  out <- NULL
  testthat::expect_warning(
    out <- allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 1000),
      .t13_layer(),
      shares
    ),
    "area indicator"
  )
  testthat::expect_equal(
    unique(out$targets$method_crop_alloc),
    "pattern_implied"
  )
  testthat::expect_equal(out$coverage$basis, "pattern")
})

testthat::test_that("an admin row the layer cannot place is reported", {
  shares <- dplyr::bind_rows(
    .t13_shares(),
    dplyr::mutate(.t13_shares()[1L, ], level_polity_code = "A-A9-1900-2100")
  )
  testthat::expect_warning(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 1000),
      .t13_layer(),
      shares
    ),
    "the allocation layer does not carry"
  )
})

testthat::test_that("the unit weights are rebuilt for every year", {
  # The per-unit weights are computed year by year, from that year's cropland
  # and that year's slice of the layer, so a country whose cropland moves
  # between years must move with it. One fixture, two years, opposite
  # cropland: the shares invert, and both years conserve.
  cropland <- tibble::tribble(
    ~lon,   ~lat,  ~year, ~cropland_ha,
    0.25,  50.25, 2000L,          900,
    0.75,  50.25, 2000L,          100,
    0.25,  50.25, 2001L,          100,
    0.75,  50.25, 2001L,          900
  )
  national <- tibble::tibble(
    year = c(2000L, 2001L),
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = c(200, 400)
  )
  out <- allocate_level_crops(
    national,
    .t13_patterns(),
    cropland,
    .t13_layer(),
    admin_shares = NULL
  )
  targets <- dplyr::arrange(out$targets, year, level_polity_code)
  testthat::expect_equal(targets$share, c(0.9, 0.1, 0.1, 0.9))
  testthat::expect_equal(targets$target_ha, c(180, 20, 40, 360))
  testthat::expect_equal(
    unique(targets$method_crop_alloc),
    "pattern_implied"
  )
  testthat::expect_true(all(abs(out$conservation$difference_ha) < 1e-9))
  testthat::expect_equal(nrow(out$coverage), 2L)
  testthat::expect_equal(out$coverage$basis, c("pattern", "pattern"))
})

testthat::test_that("a layer with nothing granted is refused", {
  testthat::expect_error(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(),
      dplyr::select(.t13_layer(), -"level_polity_code"),
      .t13_shares()
    ),
    class = "whep_alloc_no_granted_depth"
  )
})

testthat::test_that("two rows for one unit-year are refused", {
  shares <- dplyr::bind_rows(.t13_shares(), .t13_shares()[1L, ])
  testthat::expect_error(
    build_level_crop_targets(
      .t13_national(),
      tibble::tibble(
        year = 2000L,
        area_code = 1L,
        level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
        item_prod_code = 15L,
        weight_rainfed = c(50, 500),
        weight_irrigated = 0,
        cropland_rainfed_ha = c(100, 1000),
        cropland_irrigated_ha = 0,
        n_cells = 1L
      ),
      shares
    ),
    class = "whep_alloc_duplicate_shares"
  )
})

# --- T13 repair: the four failures an adversarial verifier found -------------

.t13_weights <- function() {
  tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    item_prod_code = 15L,
    weight_rainfed = c(50, 500),
    weight_irrigated = 0,
    cropland_rainfed_ha = c(100, 1000),
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
}

testthat::test_that("all-zero targets return the documented empty result", {
  # The outcome `build_level_crop_targets()` documents for a group whose
  # reported values are all zero -- every share 0, the hectares counted in
  # `coverage$dropped_ha` -- was unreachable through the driver: with no
  # positive target the engine saw a national table with no YEAR in it, and
  # its polity step aborted on a missing `area_code`. The empty allocation is
  # now returned WITH the coverage that explains it.
  #
  # It is also LOUD: the whole national total became no unit's target, so the
  # dropped-hectare warning and the container's conservation shortfall both
  # fire. Asserted here rather than muffled, because "the result is empty"
  # and "250 ha left the country" are the same event.
  raised <- .pr_conditions(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 1000),
      .t13_layer(),
      .t13_shares(a = 0, b = 0)
    )
  )
  out <- raised$value
  testthat::expect_true(any(grepl("250 ha dropped", raised$warnings)))
  testthat::expect_true(
    any(grepl("0 ha allocated against 250 ha targeted", raised$warnings))
  )
  testthat::expect_equal(nrow(out$allocation), 0L)
  testthat::expect_equal(out$coverage$basis, "admin_sum")
  testthat::expect_equal(out$coverage$allocated_share, 0)
  testthat::expect_equal(out$coverage$dropped_ha, 250)
  testthat::expect_equal(out$targets$target_ha, c(0, 0))
  testthat::expect_equal(nrow(out$breach), 0L)

  # The empty allocation is the ENGINE's schema, not a hand-written list of
  # names that can drift from it: compared column for column, and class for
  # class, against a run of the same driver that does place something.
  placed <- allocate_level_crops(
    .t13_national(),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    .t13_shares()
  )
  testthat::expect_gt(nrow(placed$allocation), 0L)
  testthat::expect_identical(
    names(out$allocation),
    names(placed$allocation)
  )
  testthat::expect_identical(
    vapply(out$allocation, \(x) class(x)[[1L]], character(1)),
    vapply(placed$allocation, \(x) class(x)[[1L]], character(1))
  )
})

testthat::test_that("a declared share binds where a value would have", {
  # ONE PREDICATE FOR "REPORTED". `coverage` counted a unit carrying a share
  # and no value as reporting while the split treated it as a non-reporter,
  # so its declared shape was silently replaced by the frozen pattern
  # weights. The contrast is against the same fixture with no shares at all:
  # unit A's 100 ha binds in both, and only the split of the 150 ha residual
  # differs.
  layer <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25,  50.25,       1L, "U1",                     1L,               1,
    0.75,  50.25,       1L, "U2",                     1L,               1,
    1.25,  50.25,       1L, "U3",                     1L,               1
  )
  patterns <- tibble::tribble(
    ~lon,   ~lat, ~item_prod_code, ~harvest_fraction,
    0.25,  50.25,             15L,               0.5,
    0.75,  50.25,             15L,               0.2,
    1.25,  50.25,             15L,               0.8
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75, 1.25),
    lat = 50.25,
    year = 2000L,
    cropland_ha = 5e4
  )
  share_row <- function(codes, value = NA_real_, share = NA_real_, treat) {
    tibble::tibble(
      area_code = 1L,
      level_polity_code = codes,
      level = 1L,
      item_prod_code = 15L,
      indicator_used = "area_harvested",
      year = 2000L,
      value = value,
      share = share,
      treatment = treat
    )
  }
  run <- function(shares) {
    allocate_level_crops(.t13_national(), patterns, cropland, layer, shares)
  }

  # Only U1 reports: the residual takes the pattern weights, 0.2 : 0.8.
  silent <- run(share_row(
    c("U1", "U2", "U3"),
    value = c(100, NA, NA),
    treat = "observed"
  ))
  testthat::expect_equal(silent$targets$target_ha, c(100, 30, 120))
  testthat::expect_equal(silent$coverage$n_units_reporting, 1L)
  testthat::expect_equal(silent$coverage$weight_basis, "pattern")

  # U2 and U3 declare 0.9 : 0.1 instead. They are reporters, the residual is
  # split by what they declared, and the pattern plays no part.
  declared <- run(dplyr::bind_rows(
    share_row("U1", value = 100, treat = "observed"),
    share_row(
      c("U2", "U3"),
      share = c(0.9, 0.1),
      treat = "backcast_t0_geometry"
    )
  ))
  testthat::expect_equal(declared$targets$target_ha, c(100, 135, 15))
  testthat::expect_equal(declared$coverage$n_units_reporting, 3L)
  testthat::expect_equal(declared$coverage$coverage, 1)
  testthat::expect_equal(declared$coverage$weight_basis, "declared")
  testthat::expect_equal(
    declared$targets$method_crop_alloc,
    c("admin_area_shares", "admin_backcast_luh2", "admin_backcast_luh2")
  )
  # Not vacuous: the two runs disagree, and the total is still conserved.
  testthat::expect_false(
    isTRUE(all.equal(declared$targets$target_ha, silent$targets$target_ha))
  )
  testthat::expect_equal(sum(declared$targets$target_ha), 250)

  # A residual set only PARTLY covered by declared shares keeps the pattern:
  # a declared fraction and a hectare of potential are not the same quantity,
  # so they are never added together.
  partial <- run(dplyr::bind_rows(
    share_row("U1", value = 100, treat = "observed"),
    share_row("U2", share = 0.9, treat = "backcast_t0_geometry"),
    share_row("U3", treat = "observed")
  ))
  testthat::expect_equal(partial$coverage$weight_basis, "pattern")
  testthat::expect_equal(partial$targets$target_ha, c(100, 30, 120))
})

testthat::test_that("a negative reported area is refused, not dropped", {
  # A negative value made a negative unit target, which the engine's
  # positivity filter dropped; the units that remained then divided the whole
  # national total between them, so the container allocated MORE than it was
  # given -- 600 ha against 500 -- with every surviving row looking ordinary.
  # Fail closed instead, naming the unit, item and year.
  negative_value <- function() {
    allocate_level_crops(
      .t13_national(500),
      .t13_patterns(),
      .t13_cropland(cell_a = 5e4, cell_b = 5e4),
      .t13_layer(),
      .t13_shares(a = -100, b = 600)
    )
  }
  testthat::expect_error(
    negative_value(),
    class = "whep_alloc_negative_value"
  )
  testthat::expect_error(negative_value(), "A-A1-1900-2100")

  # A negative declared SHARE is the same defect through the other column.
  negative_share <- .t13_shares() |>
    dplyr::mutate(value = NA_real_, share = c(-0.2, 1.2))
  testthat::expect_error(
    allocate_level_crops(
      .t13_national(500),
      .t13_patterns(),
      .t13_cropland(cell_a = 5e4, cell_b = 5e4),
      .t13_layer(),
      negative_share
    ),
    class = "whep_alloc_negative_value"
  )

  # And the target check is not made redundant by the value check: the
  # national total is a separate input, and a negative one reaches a negative
  # target with every reported value non-negative.
  testthat::expect_error(
    build_level_crop_targets(
      .t13_national(-500),
      .t13_weights(),
      .t13_shares()
    ),
    class = "whep_alloc_negative_target"
  )
})

testthat::test_that("the conservation warning states the direction it found", {
  # It said "allocate less than their target" whatever the sign, so an
  # over-allocation -- exactly what a dropped negative target produces --
  # would have been reported in words denying it.
  over <- tibble::tibble(
    grain = "container",
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    allocated_ha = 600,
    target_ha = 500,
    difference_ha = 100,
    difference_frac = 0.2
  )
  msg <- testthat::expect_warning(
    whep:::.alloc_warn_conservation(over, 1e-6),
    "600 ha allocated against 500 ha targeted"
  )
  testthat::expect_false(grepl("less than", conditionMessage(msg)))
  under <- dplyr::mutate(
    over,
    allocated_ha = 400,
    difference_ha = -100,
    difference_frac = -0.2
  )
  testthat::expect_warning(
    whep:::.alloc_warn_conservation(under, 1e-6),
    "400 ha allocated against 500 ha targeted"
  )
})

# --- T13 re-verification: the labelling and ordering defects ----------------

.t13_three_weights <- function() {
  tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("U1", "U2", "U3"),
    item_prod_code = 15L,
    weight_rainfed = c(0.5, 0.2, 0.8),
    weight_irrigated = 0,
    cropland_rainfed_ha = 5e4,
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
}

testthat::test_that("a residual split on the pattern is not called a share", {
  # `method_crop_alloc` says what actually placed the hectares. A unit that
  # declares a SHARE and no absolute area is a reporter, but its declared
  # share sets its slice of the residual only where EVERY residual candidate
  # declares one. Here only U2 does, so the frozen PATTERN weights split the
  # residual and U2 receives 0.6 x 0.2 = 30 ha, not the 0.9 it declared. The
  # hectares were always right; the label said "admin_area_shares", crediting
  # a declared share the split never used. U2 and U3 were allocated by the
  # same weights on the same residual, so they carry the same name.
  share_row <- function(codes, value = NA_real_, share = NA_real_, ...) {
    tibble::tibble(
      year = 2000L,
      area_code = 1L,
      level_polity_code = codes,
      item_prod_code = 15L,
      value = value,
      share = share,
      ...
    )
  }
  built <- build_level_crop_targets(
    .t13_national(),
    .t13_three_weights(),
    dplyr::bind_rows(
      share_row("U1", value = 100),
      share_row("U2", share = 0.9)
    )
  )
  testthat::expect_equal(built$coverage$basis, "residual")
  testthat::expect_equal(built$coverage$weight_basis, "pattern")
  testthat::expect_equal(built$targets$target_ha, c(100, 30, 120))
  testthat::expect_equal(
    built$targets$method_crop_alloc,
    c("admin_area_shares", "admin_residual", "admin_residual")
  )

  # Same row through the OTHER label that would credit an unused share: a
  # backcast share the pattern displaced is not an "admin_backcast_luh2"
  # allocation either.
  backcast <- build_level_crop_targets(
    .t13_national(),
    .t13_three_weights(),
    dplyr::bind_rows(
      share_row("U1", value = 100, treatment = "observed"),
      share_row("U2", share = 0.9, treatment = "backcast_t0_geometry")
    )
  )
  testthat::expect_equal(backcast$targets$target_ha, c(100, 30, 120))
  testthat::expect_equal(
    backcast$targets$method_crop_alloc,
    c("admin_area_shares", "admin_residual", "admin_residual")
  )

  # Not vacuous: where every candidate DOES declare, the declared share is
  # what places the hectares and the label says so.
  declared <- build_level_crop_targets(
    .t13_national(),
    .t13_three_weights(),
    dplyr::bind_rows(
      share_row("U1", value = 100),
      share_row(c("U2", "U3"), share = c(0.9, 0.1))
    )
  )
  testthat::expect_equal(declared$coverage$weight_basis, "declared")
  testthat::expect_equal(declared$targets$target_ha, c(100, 135, 15))
  testthat::expect_equal(
    unique(declared$targets$method_crop_alloc),
    "admin_area_shares"
  )
})

testthat::test_that("a negative national total is refused as one", {
  # It aborted through `.check_irrigation_within_area()`, which runs first on
  # the same table and finds `0 > -250`: the refusal said the row had "more
  # irrigated than harvested area", describing an input the caller never
  # gave. It fails closed either way, so no wrong number ever escaped -- the
  # message was the defect. The sign check now speaks first.
  err <- testthat::expect_error(
    allocate_level_crops(
      .t13_national(-250),
      .t13_patterns(),
      .t13_cropland(cell_a = 5e4, cell_b = 5e4),
      .t13_layer()
    ),
    class = "whep_alloc_negative_national"
  )
  testthat::expect_match(conditionMessage(err), "-250")
  testthat::expect_false(grepl("irrigated", conditionMessage(err)))

  # The irrigation gate still fires on the input it actually describes.
  testthat::expect_error(
    allocate_level_crops(
      dplyr::mutate(.t13_national(100), irrigated_area_ha = 150),
      .t13_patterns(),
      .t13_cropland(cell_a = 5e4, cell_b = 5e4),
      .t13_layer()
    ),
    class = "whep_spatialize_irrigation_over_area"
  )

  # And the derived-target check is not made vacuous by the input check: the
  # exported half is reached without the driver's gate in front of it.
  testthat::expect_error(
    build_level_crop_targets(
      .t13_national(-500),
      .t13_weights(),
      .t13_shares()
    ),
    class = "whep_alloc_negative_target"
  )
})

# --- Pre-PR review: the confirmed defects -----------------------------------
#
# One block per finding, each written to FAIL on the code as reviewed. The
# fixtures are the reviewers' own inputs wherever they gave one, so a reader
# can put the finding and the test side by side.

# One granted container and one level-0 container, which is the only shape in
# which the two meanings of a missing `level_polity_code` meet.
.pr_mixed_weights <- function() {
  tibble::tibble(
    year = 2000L,
    area_code = c(1L, 2L),
    level_polity_code = c("A-A1-1900-2100", NA_character_),
    item_prod_code = 15L,
    weight_rainfed = 100,
    weight_irrigated = 0,
    cropland_rainfed_ha = 1000,
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
}

.pr_mixed_national <- function() {
  tibble::tibble(
    year = 2000L,
    area_code = c(1L, 2L),
    item_prod_code = 15L,
    harvested_area_ha = c(250, 5e5)
  )
}

# What `resolve_admin_units()` leaves behind on every source that carries no
# alias row: the unit is named in the statistics and resolves to no polity, so
# `level_polity_code` is `NA` -- the same value the allocation layer uses to
# say "this container IS its own unit, at level 0".
.pr_unresolved_share <- function(value = 495000) {
  tibble::tibble(
    year = 2000L,
    area_code = 2L,
    level_polity_code = NA_character_,
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    value = value
  )
}

testthat::test_that("an unresolved admin row binds no container", {
  # BLOCKING. `level_polity_code == NA` means "level 0, the container is its
  # own unit" in the allocation layer and "this unit resolved to no polity" in
  # the admin-share contract, and the target join matches `NA` to `NA`. One
  # unresolved province then arrives as the country's complete subnational
  # evidence: with 495,000 ha against a 500,000 ha national total the level-0
  # row comes out `admin_area_shares`, and with 5,000 ha the whole run aborts
  # on a discrepancy that no province ever reported.
  built <- .pr_conditions(
    build_level_crop_targets(
      .pr_mixed_national(),
      .pr_mixed_weights(),
      .pr_unresolved_share()
    )
  )
  testthat::expect_true(any(grepl("name no unit", built$warnings)))

  container <- dplyr::filter(built$value$targets, area_code == 2L)
  testthat::expect_identical(
    container$method_crop_alloc,
    "pattern_national"
  )
  testthat::expect_equal(container$target_ha, 5e5)

  coverage <- dplyr::filter(built$value$coverage, area_code == 2L)
  testthat::expect_identical(coverage$basis, "pattern")
  testthat::expect_equal(coverage$n_units_reporting, 0L)
  testthat::expect_equal(coverage$admin_sum, 0)

  # The same row at a value the discrepancy gate would refuse: it must not
  # reach the gate at all, because it reports about no unit of this container.
  refused <- .pr_conditions(
    build_level_crop_targets(
      .pr_mixed_national(),
      .pr_mixed_weights(),
      .pr_unresolved_share(5000)
    )
  )
  testthat::expect_identical(
    dplyr::filter(refused$value$targets, area_code == 2L)$method_crop_alloc,
    "pattern_national"
  )

  # Not vacuous: a row naming a unit the layer DOES carry still binds.
  bound <- build_level_crop_targets(
    .pr_mixed_national(),
    .pr_mixed_weights(),
    dplyr::mutate(
      .pr_unresolved_share(200),
      area_code = 1L,
      level_polity_code = "A-A1-1900-2100"
    )
  )
  testthat::expect_identical(
    dplyr::filter(bound$targets, area_code == 1L)$method_crop_alloc,
    "admin_area_shares"
  )
})

testthat::test_that("the container conserves against its national total", {
  # `.alloc_conservation()` measured the container against the SUM OF UNIT
  # TARGETS, so hectares the unit split never turned into a target left the
  # country with the table reporting a difference of exactly 0. Here A-A1
  # reports 100 of a 250 ha national total and A-A2, the residual candidate,
  # sits in a cell with no cropland: its share is 0, 150 ha become no unit's
  # target, and the container row used to read 100 against 100.
  out <- .pr_conditions(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 1000, cell_b = 0),
      .t13_layer(),
      .t13_shares(a = 100)[1L, ]
    )
  )
  container <- dplyr::filter(out$value$conservation, grain == "container")
  testthat::expect_equal(container$target_ha, 250)
  testthat::expect_equal(container$allocated_ha, 100)
  testthat::expect_equal(container$difference_ha, -150)
  testthat::expect_equal(container$difference_frac, -0.6)
  testthat::expect_true(
    any(grepl("100 ha allocated against 250 ha targeted", out$warnings))
  )
  # The mass is the same one `coverage` already recorded; the two now agree.
  testthat::expect_equal(out$value$coverage$dropped_ha, 150)
})

testthat::test_that("a national total no unit can carry is warned about", {
  # `build_gridded_landuse()` warns when a national total has no allocatable
  # cell; the two-level path dropped the same hectares in silence, because
  # `.alloc_warn_unweighted()` only anti-joins on the group key and a weights
  # row of zeros matches it. Here neither of the container's cells carries
  # cropland, so every unit weight is zero and the whole 250 ha is dropped.
  out <- .pr_conditions(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 0, cell_b = 0),
      .t13_layer()
    )
  )
  testthat::expect_true(
    any(grepl("250 ha dropped", out$warnings))
  )
  testthat::expect_equal(out$value$coverage$dropped_ha, 250)
  testthat::expect_equal(nrow(out$value$allocation), 0L)

  # And it stays quiet where every hectare is placed.
  placed <- .pr_conditions(
    allocate_level_crops(
      .t13_national(),
      .t13_patterns(),
      .t13_cropland(cell_a = 1e5, cell_b = 1e5),
      .t13_layer()
    )
  )
  testthat::expect_false(any(grepl("dropped", placed$warnings)))
})

# Two successive polities of one cell, plus a granted container in a cell of
# its own. The succession's two rows each hold their cell whole, in intervals
# that never overlap, so nothing is claimed twice at any year.
.pr_epoch_grid0 <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    20.25, 60.25,       228L,             1.0,       1922L,     1991L,
    20.25, 60.25,       185L,             1.0,       1991L,     2025L,
    21.25, 60.25,       900L,             1.0,       1850L,     2100L
  )
}

.pr_epoch_deep <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L, "A-A1-1900-2100",       1L,             0.6,
    21.25, 60.25,       900L, "A-A2-1900-2100",       1L,             0.4
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2100L)
}

testthat::test_that("a succession is not a cell claimed twice", {
  # The partition holds per (cell, EPOCH). Summed over the layer as passed, a
  # cell whose polity has a successor sums to 2 and the layer aborted --
  # which is every USSR, Yugoslavia, Sudan and Czechoslovakia cell of the
  # year-aware level-0 read, so that vintage could not be combined with any
  # granted depth.
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      .pr_epoch_grid0(),
      .pr_epoch_deep(),
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  layer <- out$value
  unclaimed <- attr(layer, "unclaimed_land")
  testthat::expect_equal(nrow(dplyr::filter(unclaimed, lon == 20.25)), 0L)
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
  testthat::expect_equal(nrow(layer), 4L)
  # THE GRANTED CELL IS SHORT, AT THE OTHER END. Level 0 gives 900 the cell
  # (21.25, 60.25) from 1850 and its depth only begins in 1900, so nothing
  # claims that cell over 1850-1900. The fixture has carried the shape since
  # it was written and no report named it: the sweep's own events begin in
  # 1900, so a window drawn from the layer alone began there too. It is the
  # mirror of a depth that ends early, and the widened window reads both.
  early <- dplyr::filter(unclaimed, lon == 21.25)
  testthat::expect_equal(nrow(early), 1L)
  testthat::expect_identical(early$start_year, 1850L)
  testthat::expect_identical(early$end_year, 1900L)
  testthat::expect_equal(early$claimed_share, 0)

  # Still one-sided and still loud where the two rows DO coexist.
  overlapping <- dplyr::mutate(
    .pr_epoch_grid0(),
    end_year = dplyr::if_else(area_code == 228L, 2025L, end_year)
  )
  testthat::expect_error(
    whep:::build_allocation_layer(
      overlapping,
      .pr_epoch_deep(),
      tibble::tibble(area_code = 900L, level = 1L)
    ),
    class = "whep_alloc_layer_not_partition"
  )
})

testthat::test_that("a cell short in one epoch is reported at that epoch", {
  # The unclaimed report is one row per (cell, EPOCH), so a cell short in one
  # of its epochs and whole in another is reported once, for the epoch it is
  # short in. Reading each cell at its fullest epoch instead reported this as
  # nothing at all -- and, by the same line, said nothing about a cell that
  # goes short LATER, which is the direction below.
  partial <- dplyr::mutate(
    .pr_epoch_grid0(),
    cell_area_frac = dplyr::if_else(area_code == 228L, 0.5, cell_area_frac)
  )
  early <- .pr_conditions(
    whep:::build_allocation_layer(
      partial,
      .pr_epoch_deep(),
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  # The succession cell only: the granted cell (21.25, 60.25) carries its own
  # row here, for the 1850-1900 years its depth begins after, which the test
  # above pins.
  unclaimed <- attr(early$value, "unclaimed_land") |>
    dplyr::filter(lon == 20.25)
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_identical(unclaimed$start_year, 1922L)
  testthat::expect_identical(unclaimed$end_year, 1991L)
  testthat::expect_equal(unclaimed$claimed_share, 0.5, tolerance = 1e-12)

  # THE SAME CELL, SHORT IN ITS LATER EPOCH. Judged at its fullest epoch this
  # was silent, where the pre-sweep rule aborted on it outright: 1991-2025 is
  # half claimed and half of that cell's land is claimed by nobody in every
  # one of those years.
  late <- .pr_conditions(
    whep:::build_allocation_layer(
      dplyr::mutate(
        .pr_epoch_grid0(),
        cell_area_frac = dplyr::if_else(area_code == 185L, 0.5, cell_area_frac)
      ),
      .pr_epoch_deep(),
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  late_rows <- attr(late$value, "unclaimed_land") |>
    dplyr::filter(lon == 20.25)
  testthat::expect_equal(nrow(late_rows), 1L)
  testthat::expect_identical(late_rows$start_year, 1991L)
  testthat::expect_identical(late_rows$end_year, 2025L)
  testthat::expect_equal(late_rows$claimed_share, 0.5, tolerance = 1e-12)

  # A cell short in EVERY epoch is reported once per epoch, not once.
  short <- dplyr::mutate(
    .pr_epoch_grid0(),
    cell_area_frac = dplyr::if_else(lon == 20.25, 0.5, cell_area_frac)
  )
  reported <- .pr_conditions(
    whep:::build_allocation_layer(
      short,
      .pr_epoch_deep(),
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  every <- attr(reported$value, "unclaimed_land") |>
    dplyr::filter(lon == 20.25)
  testthat::expect_equal(nrow(every), 2L)
  testthat::expect_identical(sort(every$start_year), c(1922L, 1991L))
  testthat::expect_equal(
    every$claimed_share,
    c(0.5, 0.5),
    tolerance = 1e-12
  )
})

# The level-0 grid the four "ending depth" encodings below are read against:
# country 900 holds one cell whole from 1850 to 2100, on 3000 ha of land.
.pr_ending_grid0 <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             1.0,       1850L,     2100L
  ) |>
    dplyr::mutate(land_area_ha = 3000)
}

# The granted depth of `.pr_ending_grid0()`: level 0 reproduced whole until
# 1950, and `last_share` of the cell from then. `NULL` drops the last epoch's
# row ALTOGETHER, which is what a real deep grid produces -- a unit simply has
# no rows outside its own validity -- and is the encoding that was silent.
.pr_ending_deep <- function(last_share) {
  first <- tibble::tibble(
    lon = 21.25,
    lat = 60.25,
    area_code = 900L,
    level_polity_code = "A-A1-1850-1950",
    level = 1L,
    cell_area_frac = 1.0,
    start_year = 1850L,
    end_year = 1950L,
    land_area_ha = 3000
  )
  if (is.null(last_share)) {
    return(first)
  }
  dplyr::bind_rows(
    first,
    dplyr::mutate(
      first,
      level_polity_code = "A-A2-1950-2100",
      cell_area_frac = last_share,
      start_year = 1950L,
      end_year = 2100L,
      land_area_ha = last_share * 3000
    )
  )
}

.pr_ending_out <- function(deep, grid0 = .pr_ending_grid0(), codes = 900L) {
  .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = codes, level = 1L)
    )
  )
}

testthat::test_that("a granted depth ending early is seen however it ends", {
  # THE DEFECT THE PEAK-BASED READ CONVERTED INTO SILENCE, in the four ways a
  # depth can stop covering its container. Country 900 holds its cell whole;
  # its depth covers it whole to 1950 and then falls short. Summing the cell
  # over the layer gave 1.5 in the first case, which aborted, while reading it
  # at its fullest epoch gave 1 and reported nothing.
  #
  # The last three encodings all say the same thing -- from 1950 the layer
  # does not claim this cell -- and only the first two were ever reported,
  # because a window drawn from the layer's own rows ENDS where the silence
  # starts. The window is now the union of the layer's span and the level-0
  # span of the granted containers in the cell, so the years after a depth
  # stops are read whether the depth shrank there or simply has no row.
  shrink <- .pr_ending_out(.pr_ending_deep(0.5))
  unclaimed <- attr(shrink$value, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_identical(unclaimed$start_year, 1950L)
  testthat::expect_identical(unclaimed$end_year, 2100L)
  testthat::expect_equal(unclaimed$unclaimed_ha, 1500, tolerance = 1e-9)
  ragged <- attr(shrink$value, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_identical(ragged$start_year, 1950L)
  testthat::expect_identical(ragged$reason, "unit_share_mismatch")
  testthat::expect_equal(ragged$difference, -0.5, tolerance = 1e-12)
  # The 1850-1950 epoch is whole and correct, and neither report names it.
  testthat::expect_false(any(unclaimed$start_year %in% 1850L))
  testthat::expect_false(any(ragged$start_year %in% 1850L))
  # Exactly the shortfall warning and the raggedness warning -- the ending
  # tests ran every one of these fixtures before but never looked at
  # `$warnings`, which is how an uncaught `max()` warning inside the total
  # (see below) sat unnoticed for a whole verification round.
  testthat::expect_length(shrink$warnings, 2L)

  # A share of 0.001 is the same fact at a smaller magnitude, and the cell's
  # land is still measured there, so the shortfall has hectares: 2997 of 3000.
  tiny <- .pr_ending_out(.pr_ending_deep(0.001))
  tiny_rows <- attr(tiny$value, "unclaimed_land")
  testthat::expect_equal(nrow(tiny_rows), 1L)
  testthat::expect_identical(tiny_rows$start_year, 1950L)
  testthat::expect_equal(tiny_rows$unclaimed_ha, 2997, tolerance = 1e-9)
  testthat::expect_equal(nrow(attr(tiny$value, "ragged_coverage")), 1L)
  testthat::expect_length(tiny$warnings, 2L)

  # A share of 0 keeps the row and claims nothing with it. The cell's land is
  # still measured, though not by this row: `base` -- country 900's own
  # `grid0` row, unaffected by the depth's own zero-share row -- prices the
  # shortfall at the whole 3000 ha, never 0 (which would deny it) and never
  # NA (which would deny it just as much, by saying it was never measured).
  zero <- .pr_ending_out(.pr_ending_deep(0))
  zero_rows <- attr(zero$value, "unclaimed_land")
  testthat::expect_equal(nrow(zero_rows), 1L)
  testthat::expect_identical(zero_rows$start_year, 1950L)
  testthat::expect_equal(zero_rows$claimed_share, 0)
  testthat::expect_equal(zero_rows$unclaimed_ha, 3000, tolerance = 1e-9)
  testthat::expect_equal(nrow(attr(zero$value, "ragged_coverage")), 1L)
  testthat::expect_length(zero$warnings, 2L)

  # THE ENCODING A REAL DEEP GRID PRODUCES: no row at all from 1950. The layer
  # holds nothing for the cell after 1949 and level 0 gives it to 900 until
  # 2100, so 1950-2100 is unclaimed land and is named as such.
  gone <- .pr_ending_out(.pr_ending_deep(NULL))
  gone_rows <- attr(gone$value, "unclaimed_land")
  testthat::expect_equal(nrow(gone_rows), 1L)
  testthat::expect_identical(gone_rows$start_year, 1950L)
  testthat::expect_identical(gone_rows$end_year, 2100L)
  testthat::expect_equal(gone_rows$claimed_share, 0)
  testthat::expect_equal(gone_rows$unclaimed_share, 1)
  # `base` prices this the same way as the explicit zero-share row above: the
  # two encodings say the same fact -- from 1950 nobody claims this cell --
  # and now report the same magnitude rather than one a number and the other
  # NA.
  testthat::expect_equal(gone_rows$unclaimed_ha, 3000, tolerance = 1e-9)
  # Assertion (b) is silent here, and that is deliberate rather than the same
  # defect twice: it compares the units against level 0 over the container's
  # DEPTH span, because a level-0 grid with no time dimension claims every
  # year and would report every granted cell as ragged before its units began
  # and after they ended (T31(j)). The land is the unclaimed report's
  # question, and it answers it above.
  testthat::expect_equal(nrow(attr(gone$value, "ragged_coverage")), 0L)
  # Only the shortfall warning: with every unclaimed row now carrying a real
  # hectare figure, `.level_unclaimed_total()` never has to fall back to
  # reporting nothing measured, and the `max()`-on-empty-frame warning that
  # used to ride along uncaught here is gone.
  testthat::expect_length(gone$warnings, 1L)

  # TWO GRANTED CONTAINERS ENDING TOGETHER, which is how a multi-country grant
  # produces the same silence: neither holds the cell after 1950, so no row of
  # the layer survives there and the cell's last claim is the depth's end.
  pair_grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             0.5,       1850L,     2100L,
    21.25, 60.25,       901L,             0.5,       1850L,     2100L
  ) |>
    dplyr::mutate(land_area_ha = 1500)
  pair_deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1850-1950",     1L,             0.5,
    21.25, 60.25,       901L,   "B-B1-1850-1950",     1L,             0.5
  ) |>
    dplyr::mutate(
      start_year = 1850L,
      end_year = 1950L,
      land_area_ha = 1500
    )
  pair <- .pr_ending_out(pair_deep, pair_grid0, codes = c(900L, 901L))
  pair_rows <- attr(pair$value, "unclaimed_land")
  testthat::expect_equal(nrow(pair_rows), 1L)
  testthat::expect_identical(pair_rows$start_year, 1950L)
  testthat::expect_identical(pair_rows$end_year, 2100L)
  testthat::expect_equal(pair_rows$claimed_share, 0)
  # 1850-1950 is claimed whole by the two depths together and is not a row.
  testthat::expect_false(any(pair_rows$start_year %in% 1850L))
  # `base` here carries BOTH granted containers' own rows for the shared
  # cell, and sums them: 900's 1500 ha plus 901's 1500 ha is the whole
  # 3000 ha cell that neither claims after 1950.
  testthat::expect_equal(pair_rows$unclaimed_ha, 3000, tolerance = 1e-9)
  testthat::expect_equal(nrow(attr(pair$value, "ragged_coverage")), 0L)
  testthat::expect_length(pair$warnings, 1L)
})

testthat::test_that("a bound level 0 never gave is not one to fall short of", {
  # The window may only be widened to a bound level 0 actually STATES. The
  # 2015 snapshot carries no time dimension at all, so its rows are given the
  # open interval; reading that silence as a claim on every year would report
  # a leading and a trailing unclaimed epoch for every granted cell of that
  # vintage, which is the T31(j) vintage question and not a shortfall.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac,
    21.25, 60.25,       900L,             1.0
  ) |>
    dplyr::mutate(land_area_ha = 3000)
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1900-2000",     1L,             1.0
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2000L, land_area_ha = 3000)
  out <- .pr_ending_out(deep, grid0)
  testthat::expect_equal(nrow(attr(out$value, "unclaimed_land")), 0L)
  testthat::expect_equal(nrow(attr(out$value, "ragged_coverage")), 0L)
  # And the run is SILENT: with every bound open there is nothing to take a
  # bound over, and reducing the empty frame anyway warns ("no non-missing
  # arguments to min") while returning `Inf`, which would then be read as a
  # year the layer falls short of.
  testthat::expect_length(out$warnings, 0L)
})

testthat::test_that("the unclaimed total never warns on nothing measured", {
  # `.level_unclaimed_total()`'s own empty-frame case, the twin of
  # `.level_cell_bound()`'s above: every unclaimed row can carry `NA`
  # hectares -- `base` gives none of them a measurement -- and
  # `dplyr::summarise()` then type-probes `max()` on the post-filter empty
  # frame, which warns ("no non-missing arguments to max; returning -Inf")
  # and would print "0 Mha of land" where nothing was measured at all. This
  # is exactly what `.pr_ending_grid0()` + `.pr_ending_deep(NULL)` produced
  # before `base` supplied a land figure (see the "ending early" test
  # above); it is tested directly here so the guard stands even where a
  # caller passes no `base` at all.
  all_na <- tibble::tibble(
    lon = c(21.25, 22.25),
    lat = c(60.25, 61.25),
    unclaimed_ha = NA_real_
  )
  out <- .pr_conditions(
    whep:::.level_unclaimed_total(all_na, has_land = TRUE)
  )
  testthat::expect_length(out$warnings, 0L)
  testthat::expect_false(grepl("Mha of land", out$value, fixed = TRUE))
  testthat::expect_match(out$value, "2 rows claimed by nobody", fixed = TRUE)
})

testthat::test_that("an epoch nobody claims is priced from base, not left NA", {
  # The trailing epoch's state is what is left after every claim in the cell
  # has been closed: nobody claims it, but `base` -- `grid0`'s own row for
  # the granted container (900), on its own 1850-2100 interval -- still
  # measures land there. 900 holds 0.1 of the 3000 ha cell, so the shortfall
  # is 300 ha, not NA: NA would deny a shortfall `grid0` itself measures.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             0.1,       1850L,     2100L,
    21.25, 60.25,       901L,             0.2,       1850L,     1950L,
    21.25, 60.25,       902L,             0.7,       1850L,     1950L
  ) |>
    dplyr::mutate(land_area_ha = cell_area_frac * 3000)
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1850-1950",     1L,             0.1
  ) |>
    dplyr::mutate(start_year = 1850L, end_year = 1950L, land_area_ha = 300)
  out <- .pr_ending_out(deep, grid0)
  tail_row <- attr(out$value, "unclaimed_land") |>
    dplyr::filter(start_year == 1950L)
  testthat::expect_equal(nrow(tail_row), 1L)
  testthat::expect_identical(tail_row$end_year, 2100L)
  testthat::expect_equal(tail_row$claimed_share, 0, tolerance = 1e-12)
  testthat::expect_equal(tail_row$unclaimed_ha, 300, tolerance = 1e-9)
})

testthat::test_that("a cancelled residue does not fabricate hectares", {
  # `cumsum()` does not return exactly 0 from a sum of doubles that cancels:
  # 0.1 + 0.2 + 0.7 - 0.1 - 0.2 - 0.7 leaves 2.2e-16. Dividing the land
  # residue by the share residue is a ratio of two rounding errors, which is
  # either 0 -- denying the shortfall -- or a plausible hectare figure that is
  # pure noise, and `base` cannot rescue it either: called directly, with no
  # `base` at all, `.level_unclaimed_ha()` must still not fabricate one.
  #
  # AND THE RESIDUE THE SHIPPED SUPPORT ACTUALLY PRODUCES. `cumsum()`
  # accumulates in long double, so a cell whose claims cancel exactly comes
  # back as exactly 0 -- until the terms span enough orders of magnitude to
  # lose bits, which they do on real data: cell (16.75, 2.75) of the
  # `20260825T102349Z-1a0eb` support is claimed by nobody over 1912-1919
  # between shares of 2.7e-05 and 0.99997, and the sweep leaves 1.4e-19 of
  # share and -2.8e-14 ha behind. Their ratio is an area of the wrong sign
  # and six figures, and `> 0` published it.
  residue <- tibble::tibble(
    lon = 16.75,
    lat = 2.75,
    start_year = 1912L,
    end_year = 1919L,
    claimed_share = 1.355253e-19,
    claimed_ha = -2.842171e-14,
    unclaimed_share = 1
  )
  fabricated <- residue$claimed_ha *
    residue$unclaimed_share /
    residue$claimed_share
  testthat::expect_true(is.finite(fabricated))
  testthat::expect_gt(abs(fabricated), 1e5)
  testthat::expect_lt(fabricated, 0)
  read <- whep:::.level_unclaimed_ha(residue, has_land = TRUE)
  testthat::expect_true(is.na(read$unclaimed_ha))
  testthat::expect_identical(read$start_year, 1912L)
})

testthat::test_that("units reproducing level 0 every year are not ragged", {
  # Both sides of assertion (b) on ONE grain. The container holds its cell
  # whole in two epochs and its two units split it 0.6/0.4 in both, so it
  # reproduces its level-0 share in every year. Reading the units at one
  # epoch while summing `grid0` across all of them made the level-0 side 2
  # against a unit side of 1, and reported a correct layer as ragged.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             1.0,       1850L,     1950L,
    21.25, 60.25,       900L,             1.0,       1950L,     2100L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1850-2100",     1L,             0.6,
    21.25, 60.25,       900L,   "A-A2-1850-2100",     1L,             0.4,
    21.25, 60.25,       900L,   "A-A1-1850-2100",     1L,             0.6,
    21.25, 60.25,       900L,   "A-A2-1850-2100",     1L,             0.4
  ) |>
    dplyr::mutate(
      start_year = rep(c(1850L, 1950L), each = 2L),
      end_year = rep(c(1950L, 2100L), each = 2L)
    )
  layer <- whep:::build_allocation_layer(
    grid0,
    deep,
    tibble::tibble(area_code = 900L, level = 1L)
  )
  testthat::expect_equal(nrow(attr(layer, "ragged_coverage")), 0L)
})

testthat::test_that("a cell's peak epoch is not the container's", {
  # The epoch a CELL is fullest in need not be one the container is even in.
  # Cell (20.25, 60.25) is half country 500's to 1950 and half country 900's
  # from then; 900's two units are exactly its 0.5. Choosing one moment per
  # cell put 900's reading in 500's epoch, where 900 has no row at all, and
  # reported a correct container as having no units. Swapping the two epochs
  # changed the verdict, which is how a per-cell moment fails.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    20.25, 60.25,       500L,             0.5,       1900L,     1950L,
    20.25, 60.25,       900L,             0.5,       1950L,     2000L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    20.25, 60.25,       900L,   "A-A1-1950-2000",     1L,             0.3,
    20.25, 60.25,       900L,   "A-A2-1950-2000",     1L,             0.2
  ) |>
    dplyr::mutate(start_year = 1950L, end_year = 2000L)
  granted <- tibble::tibble(area_code = 900L, level = 1L)
  out <- .pr_conditions(whep:::build_allocation_layer(grid0, deep, granted))
  testthat::expect_equal(nrow(attr(out$value, "ragged_coverage")), 0L)
  # And with the two epochs exchanged, which used to give a different answer.
  swapped <- dplyr::mutate(
    grid0,
    start_year = c(1950L, 1900L),
    end_year = c(2000L, 1950L)
  )
  deep_swapped <- dplyr::mutate(deep, start_year = 1900L, end_year = 1950L)
  swap <- .pr_conditions(
    whep:::build_allocation_layer(swapped, deep_swapped, granted)
  )
  testthat::expect_equal(nrow(attr(swap$value, "ragged_coverage")), 0L)
})

testthat::test_that("a cell a granted depth never reaches is still ragged", {
  # The window assertion (b) is read over must not become a way to disappear:
  # a container with NO unit row in a cell it holds at level 0 is reported
  # over its level-0 span, as having no units, which is what it has.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    20.25, 60.25,       900L,             1.0,       1900L,     2000L,
    21.25, 60.25,       900L,             1.0,       1900L,     2000L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1900-2000",     1L,             1.0
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2000L)
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  ragged <- attr(out$value, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_equal(ragged$lon, 20.25)
  testthat::expect_identical(ragged$reason, "no_unit_rows")
  testthat::expect_identical(ragged$n_units, 0L)
  testthat::expect_identical(ragged$start_year, 1900L)
  testthat::expect_identical(ragged$end_year, 2000L)
  # The unclaimed report answers for the same cell in its own terms. The
  # layer holds NO row there at all, so the sweep has no event of its own to
  # place and the cell is read over the level-0 span alone -- the limit of
  # the same widening a depth that stops early gets, and the reason a cell
  # the depth never reaches cannot fall out of the report entirely.
  never <- attr(out$value, "unclaimed_land") |>
    dplyr::filter(lon == 20.25)
  testthat::expect_equal(nrow(never), 1L)
  testthat::expect_identical(never$start_year, 1900L)
  testthat::expect_identical(never$end_year, 2000L)
  testthat::expect_equal(never$claimed_share, 0)
})

testthat::test_that("one cell's two granted containers are swept apart", {
  # The sweep behind assertion (b) runs per (cell, CONTAINER). With the cell
  # in the key alone, one granted container's unit events are carried into
  # the running totals of the next container in the same cell: 901's rows at
  # (0, 0) inherit 900's two unit starts, its depth window is then read as
  # already closed, and the `no_unit_rows` row -- the guarantee that a
  # container the depth never reaches in a cell is still reported -- silently
  # disappears. The shape needs TWO GRANTED CONTAINERS SHARING ONE CELL,
  # which is the multi-country grant this feature exists to enable.
  grid0 <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    0.25, 0.25,       900L,             0.6,       1900L,     2000L,
    0.25, 0.25,       901L,             0.4,       1900L,     2000L
  )
  deep <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25, 0.25,       900L,      "A-A1-1900",       1L,             0.3,
    0.25, 0.25,       900L,      "A-A2-1900",       1L,             0.3,
    1.25, 0.25,       901L,      "B-B1-1900",       1L,             1.0
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2000L)
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = c(900L, 901L), level = 1L)
    )
  )
  ragged <- attr(out$value, "ragged_coverage") |>
    dplyr::arrange(lon)
  testthat::expect_equal(nrow(ragged), 2L)
  testthat::expect_identical(
    ragged$reason,
    c("no_unit_rows", "unit_outside_level0")
  )
  testthat::expect_identical(ragged$area_code, c(901L, 901L))
  testthat::expect_equal(ragged$lon, c(0.25, 1.25))
  testthat::expect_equal(ragged$difference, c(-0.4, 1.0), tolerance = 1e-12)
  # 900's own two units reproduce its level-0 share in the shared cell, so it
  # is not reported: the split is between containers, not a blanket report.
  testthat::expect_false(any(ragged$area_code == 900L))
})

testthat::test_that("a gap in both sides at once is not ragged", {
  # A container can LEAVE a cell and come back: country 900 holds this one
  # whole to 1848 and a fifth of it from 1867, and holds nothing there in
  # between. Its depth reproduces both, so in the gap the layer and level 0
  # agree that the container is not in the cell -- there is nothing to report,
  # and "no unit rows" is false of a container level 0 does not put there
  # either. On the `20260825T102349Z-1a0eb` support this shape is 2,632
  # (cell, container, epoch) rows, the first of them Mexico's own
  # (-99.25, 27.75) over 1848-1867; a diagnostic reporting rows whose two
  # sides agree is the failure that was rejected, in the other direction.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    20.25, 60.25,       900L,             1.0,       1800L,     1848L,
    20.25, 60.25,       900L,             0.2,       1867L,     2000L,
    20.25, 60.25,       901L,             0.8,       1867L,     2000L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    20.25, 60.25,       900L,   "A-A1-1800-1848",     1L,             1.0,
    20.25, 60.25,       900L,   "A-A1-1867-2000",     1L,             0.2
  ) |>
    dplyr::mutate(
      start_year = c(1800L, 1867L),
      end_year = c(1848L, 2000L)
    )
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  testthat::expect_equal(nrow(attr(out$value, "ragged_coverage")), 0L)
  # The gap IS unclaimed land, and that is the report whose question it is:
  # nobody holds the cell between 1848 and 1867.
  unclaimed <- attr(out$value, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_identical(unclaimed$start_year, 1848L)
  testthat::expect_identical(unclaimed$end_year, 1867L)
  testthat::expect_equal(unclaimed$claimed_share, 0)
})

testthat::test_that("a unit in a cell level 0 does not give it is ragged", {
  # The third reason, which nothing reached: a granted container holding a
  # unit in a cell its own level-0 grid gives it no share of. The layer still
  # partitions -- country 901 holds the other half -- so assertion (a) cannot
  # see it, and the difference is the unit's whole share.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    20.25, 60.25,       900L,             1.0,       1900L,     2000L,
    21.25, 60.25,       901L,             0.5,       1900L,     2000L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    20.25, 60.25,       900L,   "A-A1-1900-2000",     1L,             1.0,
    21.25, 60.25,       900L,   "A-A2-1900-2000",     1L,             0.5
  ) |>
    dplyr::mutate(start_year = 1900L, end_year = 2000L)
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  ragged <- attr(out$value, "ragged_coverage")
  testthat::expect_equal(nrow(ragged), 1L)
  testthat::expect_equal(ragged$lon, 21.25)
  testthat::expect_identical(ragged$reason, "unit_outside_level0")
  testthat::expect_equal(ragged$unit_share, 0.5, tolerance = 1e-12)
  testthat::expect_equal(ragged$level0_share, 0)
  testthat::expect_equal(ragged$difference, 0.5, tolerance = 1e-12)
})

testthat::test_that("a container row beside its units is ragged anywhere", {
  # The other half of assertion (b), and the one that must not be windowed
  # away: a container-keyed row surviving for a granted country is refused
  # wherever it sits, including in an epoch its units do not cover.
  grid0 <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~cell_area_frac, ~start_year, ~end_year,
    21.25, 60.25,       900L,             1.0,       1850L,     2100L
  )
  deep <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    21.25, 60.25,       900L,   "A-A1-1950-2100",     1L,             1.0,
    21.25, 60.25,       900L, NA_character_,          1L,             1.0
  ) |>
    dplyr::mutate(
      start_year = c(1950L, 1850L),
      end_year = c(2100L, 1950L)
    )
  out <- .pr_conditions(
    whep:::build_allocation_layer(
      grid0,
      deep,
      tibble::tibble(area_code = 900L, level = 1L)
    )
  )
  ragged <- attr(out$value, "ragged_coverage")
  testthat::expect_true(any(ragged$reason == "container_row_present"))
  present <- dplyr::filter(ragged, reason == "container_row_present")
  testthat::expect_identical(present$start_year, 1850L)
  testthat::expect_identical(present$n_container_rows, 1L)
})

testthat::test_that("the containment edge cuts the support's interval", {
  # `.level_support_units()` intersects the support row's validity with the
  # edge's: `pmax(start_year, start_year_edge)`. With `pmin` there, a
  # prefecture would carry cells for the years BEFORE its container held it,
  # and no fixture had an edge whose start differed from its support row's.
  edges <- dplyr::mutate(
    .lv_containment(),
    start_year = dplyr::if_else(
      member_code == "JPN-AICHI-1871-2025",
      1980L,
      start_year
    )
  )
  grid <- whep:::read_level_country_grid(
    level = 1L,
    support = .lv_support(),
    containment = edges
  )
  aichi <- dplyr::filter(grid, level_polity_code == "JPN-AICHI-1871-2025")
  testthat::expect_equal(nrow(aichi), 2L)
  testthat::expect_true(all(aichi$start_year == 1980L))

  # The consequence, at the year the intersection exists to exclude.
  early <- whep:::.filter_country_grid_year(grid, 1960L)
  testthat::expect_identical(
    unique(early$level_polity_code),
    "JPN-GIFU-1871-2025"
  )
})

testthat::test_that("an empty intersection yields no compartment", {
  # `start_year < end_year`, not `<=`: the interval convention is half-open,
  # so an edge starting exactly where the support row ends covers no year at
  # all and must not put an allocatable share on the grid.
  edges <- dplyr::mutate(
    .lv_containment(),
    start_year = dplyr::if_else(
      member_code == "JPN-AICHI-1871-2025",
      2025L,
      start_year
    ),
    end_year = dplyr::if_else(
      member_code == "JPN-AICHI-1871-2025",
      2030L,
      end_year
    )
  )
  grid <- whep:::read_level_country_grid(
    level = 1L,
    support = .lv_support(),
    containment = edges
  )
  testthat::expect_identical(
    unique(grid$level_polity_code),
    "JPN-GIFU-1871-2025"
  )
  testthat::expect_true(all(grid$start_year < grid$end_year))
})

testthat::test_that("a share outside [0, 1] is refused at every bound", {
  # The sole bound on a share that multiplies every downstream hectare. Only
  # the over-claim has a second net (`build_allocation_layer()`); `NA` and a
  # negative share pass that one silently, so all three bounds are pinned
  # here, through the public reader on a nested support. The bound is on the
  # COMPOSED cell share, so the over-claim case declares 2.5 of a container
  # holding half its cell.
  nested <- function(frac) {
    .lv_support() |>
      dplyr::mutate(
        container_frac = c(frac, 0.5, 0.5, NA),
        land_area_ha = c(3000, 1000, 2000, 1000)
      ) |>
      dplyr::bind_rows(
        tibble::tibble(
          polycell_id = c("JPN@1", "JPN@2"),
          cell_id = c(1L, 2L),
          lon = c(137.25, 137.75),
          lat = 35.25,
          polity_code = "JPN-1952-2025",
          area_code = 110L,
          start_year = 1952L,
          end_year = 2025L,
          cell_area_ha = c(3000, 4000),
          land_area_ha = 3000,
          container_frac = NA_real_
        )
      )
  }
  read <- function(frac) {
    whep:::read_level_country_grid(
      level = 1L,
      support = nested(frac),
      containment = .lv_containment()
    )
  }
  testthat::expect_error(
    read(2.5),
    class = "whep_level_share_out_of_range"
  )
  testthat::expect_error(
    read(NA_real_),
    class = "whep_level_share_out_of_range"
  )
  testthat::expect_error(
    read(-0.4),
    class = "whep_level_share_out_of_range"
  )
  # Not vacuous: the same support with a share inside the bound reads.
  testthat::expect_s3_class(read(1), "tbl_df")
})

testthat::test_that("a duplicated container key is refused by the fold", {
  # `.level_check_output_key()` is what keeps the default output grain's
  # promise that `(lon, lat, area_code, item_prod_code, year)` is unique. A
  # column varying within a container's units -- `method_crop_alloc` here,
  # `grid_area_code` under `area_key = "polity_area"` -- survives the fold
  # into the group key and splits the row.
  result <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    lon = 0.25,
    lat = 0.25,
    item_prod_code = 15L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    method_crop_alloc = c("admin_area_shares", "admin_residual"),
    rainfed_ha = c(10, 20),
    irrigated_ha = c(1, 2)
  )
  err <- testthat::expect_error(
    whep:::.level_fold_output(result, 0L),
    "duplicated key row"
  )
  testthat::expect_match(conditionMessage(err), "item_prod_code")

  # A column constant within the container folds without complaint.
  ok <- whep:::.level_fold_output(
    dplyr::mutate(result, method_crop_alloc = "admin_area_shares"),
    0L
  )
  testthat::expect_equal(nrow(ok), 1L)
  testthat::expect_equal(ok$rainfed_ha, 30)
})

testthat::test_that("T31(d) refuses only past BOTH tolerances", {
  # Decision T31(d) is an AND, and only the absolute half was pinned:
  # dropping the relative half from the gate failed no test. A group 50,000
  # ha short of its national total is 50 times the absolute tolerance and
  # half the relative one, so it must NOT be refused.
  national <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 1e7
  )
  weights <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("U1", "U2"),
    item_prod_code = 15L,
    weight_rainfed = c(1, 1),
    weight_irrigated = 0,
    cropland_rainfed_ha = 1e6,
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
  shares <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("U1", "U2"),
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    value = c(5e6, 4.95e6)
  )
  built <- build_level_crop_targets(national, weights, shares)
  testthat::expect_identical(built$coverage$basis, "admin_sum")
  testthat::expect_equal(built$coverage$discrepancy_ha, 5e4)
  testthat::expect_equal(sum(built$targets$target_ha), 1e7)

  # The mirror half, and the tolerances are arguments rather than constants:
  # tightening the relative one turns the same group into a refusal.
  testthat::expect_error(
    build_level_crop_targets(
      national,
      weights,
      shares,
      tolerance_relative = 0.001
    ),
    class = "whep_alloc_admin_discrepancy"
  )
  testthat::expect_error(
    build_level_crop_targets(
      national,
      weights,
      shares,
      tolerance_relative = 0.001,
      tolerance_absolute = 1e6
    ),
    NA
  )
})

testthat::test_that("partial coverage raises a residual, not a refusal", {
  # The other half of T31(d): the gate applies only where coverage is
  # COMPLETE. Widening it to the residual basis failed no test, because every
  # partial-coverage fixture in the suite sits below the 1,000 ha absolute
  # tolerance. Here one unit of two reports 20,000 ha of a 100,000 ha
  # national total -- 80% and 80,000 ha, past both tolerances -- and that
  # difference is the residual unit's target, not a discrepancy.
  national <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    item_prod_code = 15L,
    harvested_area_ha = 1e5
  )
  weights <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("U1", "U2"),
    item_prod_code = 15L,
    weight_rainfed = 1,
    weight_irrigated = 0,
    cropland_rainfed_ha = 1e4,
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
  shares <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "U1",
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    value = 2e4
  )
  built <- build_level_crop_targets(national, weights, shares)
  testthat::expect_identical(built$coverage$basis, "residual")
  testthat::expect_equal(built$coverage$discrepancy_ha, 8e4)
  testthat::expect_equal(built$coverage$residual_target_ha, 8e4)
  testthat::expect_equal(built$targets$target_ha, c(2e4, 8e4))
  testthat::expect_identical(
    built$targets$method_crop_alloc,
    c("admin_area_shares", "admin_residual")
  )
})

testthat::test_that("a unit short while its sibling absorbs it warns", {
  # The failure the unit-grain conservation rows exist to see, and the one
  # the function's own comment names: the container still reconciles because
  # a sibling took the hectares, so only the unit rows can show it.
  allocation <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    item_prod_code = 15L,
    rainfed_ha = c(0, 200),
    irrigated_ha = 0
  )
  targets <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    item_prod_code = 15L,
    target_ha = c(100, 100),
    national_total_ha = 200
  )
  out <- .pr_conditions(whep:::.alloc_conservation(allocation, targets, 1e-6))
  testthat::expect_true(any(grepl("A-A1-1900-2100", out$warnings)))
  container <- dplyr::filter(out$value, grain == "container")
  testthat::expect_equal(container$difference_ha, 0)
  units <- dplyr::filter(out$value, grain == "unit")
  testthat::expect_equal(sort(units$difference_ha), c(-100, 100))
})

testthat::test_that("two national totals in one group are refused", {
  # The container's conservation target IS `national_total_ha`, taken once per
  # (year, container, item). That it is constant inside the group was stated
  # in a comment and then trusted: `dplyr::first()` took whichever row sorted
  # first, so a targets table carrying 200 and 9999 set the target the whole
  # check is measured against to 200 in silence. The point of the container
  # grain is that its target is not circular; the one input it does rest on
  # is now checked.
  targets <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("A-A1-1900-2100", "A-A2-1900-2100"),
    item_prod_code = 15L,
    target_ha = c(100, 100),
    national_total_ha = c(200, 9999)
  )
  testthat::expect_error(
    whep:::.alloc_national_totals(targets),
    class = "whep_alloc_national_total_varies"
  )
  testthat::expect_error(
    whep:::.alloc_national_totals(targets),
    "more than one"
  )
  # One total per group is the ordinary case and still passes straight
  # through, at the group's own value rather than at the sum of the units.
  ok <- whep:::.alloc_national_totals(
    dplyr::mutate(targets, national_total_ha = 250)
  )
  testthat::expect_equal(ok$target_ha, 250)
  testthat::expect_named(
    ok,
    c("year", "area_code", "item_prod_code", "target_ha")
  )
})

testthat::test_that("hectares allocated against no target are warned", {
  # `difference_frac` is `NA` where the target is 0, and coalescing that to 0
  # excused the breach at both grains: 500 ha placed for a group that asked
  # for none passed a RELATIVE test that had nothing to divide by. The
  # absolute half decides it instead.
  allocation <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "A-A1-1900-2100",
    item_prod_code = 15L,
    rainfed_ha = 500,
    irrigated_ha = 0
  )
  targets <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = "A-A1-1900-2100",
    item_prod_code = 15L,
    target_ha = 0,
    national_total_ha = 0
  )
  out <- .pr_conditions(whep:::.alloc_conservation(allocation, targets, 1e-6))
  testthat::expect_true(any(grepl("no target at all", out$warnings)))
  container <- dplyr::filter(out$value, grain == "container")
  testthat::expect_equal(container$difference_ha, 500)
  testthat::expect_true(is.na(container$difference_frac))
  # A group on its target with nothing to place stays quiet.
  quiet <- .pr_conditions(whep:::.alloc_conservation(
    dplyr::mutate(allocation, rainfed_ha = 0),
    targets,
    1e-6
  ))
  testthat::expect_length(quiet$warnings, 0L)
})

testthat::test_that("straddling a foreign cell is not straddling a sibling", {
  # `n_units_here` counted the distinct units of a CELL rather than of a
  # container, so a unit sharing its cell with another country scored as
  # sibling straddle with no sibling anywhere in the layer -- the exact
  # distinction the two columns carry.
  layer <- tibble::tribble(
    ~lon,   ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
    0.25,  50.25,       1L, "A-A1-1900-2100",       1L,             0.5,
    0.25,  50.25,       2L, "B-B1-1900-2100",       1L,             0.5,
    0.75,  50.25,       1L, "A-A1-1900-2100",       1L,             1.0
  )
  national <- tibble::tibble(
    year = 2000L,
    area_code = c(1L, 2L),
    item_prod_code = 15L,
    harvested_area_ha = 100
  )
  cropland <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = 50.25,
    year = 2000L,
    cropland_ha = 1e5
  )
  out <- allocate_level_crops(
    national,
    .t13_patterns(),
    cropland,
    layer
  )
  straddle <- dplyr::arrange(out$straddle, area_code)
  testthat::expect_equal(straddle$straddle_sibling, c(0, 0))
  testthat::expect_gt(straddle$straddle_foreign[[1L]], 0)
  testthat::expect_equal(straddle$straddle_foreign[[2L]], 1)

  # Not vacuous: a genuine sibling pair on the same geometry still scores.
  sibling <- dplyr::mutate(
    layer,
    area_code = 1L,
    level_polity_code = dplyr::if_else(
      level_polity_code == "B-B1-1900-2100",
      "A-A2-1900-2100",
      level_polity_code
    )
  )
  paired <- allocate_level_crops(
    dplyr::filter(national, area_code == 1L),
    .t13_patterns(),
    cropland,
    sibling
  )
  testthat::expect_true(all(paired$straddle$straddle_sibling > 0))
  testthat::expect_equal(paired$straddle$straddle_foreign, c(0, 0))
})

testthat::test_that("a regime that placed nothing is not named for one", {
  # `method_crop_alloc` says what placed the hectares. A group whose weights
  # and cropland are all zero placed none, and calling that "pattern_implied"
  # -- or "pattern_national" on a level-0 container -- claims a regime that
  # never ran. The vocabulary already reserves "unallocated" for a unit whose
  # target is 0.
  weights <- tibble::tibble(
    year = 2000L,
    area_code = 1L,
    level_polity_code = c("U1", "U2"),
    item_prod_code = 15L,
    weight_rainfed = 0,
    weight_irrigated = 0,
    cropland_rainfed_ha = 0,
    cropland_irrigated_ha = 0,
    n_cells = 1L
  )
  built <- build_level_crop_targets(.t13_national(1000), weights)
  testthat::expect_identical(built$coverage$weight_basis, "none")
  testthat::expect_equal(built$targets$target_ha, c(0, 0))
  testthat::expect_identical(
    unique(built$targets$method_crop_alloc),
    "unallocated"
  )

  # The same on a level-0 container, where the misnomer was
  # "pattern_national".
  level0 <- build_level_crop_targets(
    .t13_national(1000),
    dplyr::mutate(weights[1L, ], level_polity_code = NA_character_)
  )
  testthat::expect_identical(level0$targets$method_crop_alloc, "unallocated")

  # Not vacuous: a positive weight still reads as the pattern regime.
  placed <- build_level_crop_targets(
    .t13_national(1000),
    dplyr::mutate(weights, weight_rainfed = c(1, 3))
  )
  testthat::expect_identical(
    unique(placed$targets$method_crop_alloc),
    "pattern_implied"
  )
})

testthat::test_that("the coverage prototype is the resolver's own schema", {
  # The exported prototype named `source`/`tier`/`grain` and typed
  # `not_shipped` as character, where the resolver that fills it emits
  # `resolved_*` and a logical. Writing a real coverage table through
  # `.write_admin_coverage()` therefore aborted on three missing columns, and
  # a reader parsing the file against the export would have found TRUE/FALSE
  # where a source label was promised.
  proto <- whep::admin_coverage_prototype()
  testthat::expect_identical(
    names(proto),
    names(whep:::.admin_coverage_prototype())
  )
  testthat::expect_type(proto$not_shipped, "logical")

  coverage <- whep:::resolve_admin_shares(.level1_admin_shares())$coverage
  tmp <- withr::local_tempdir()
  path <- whep:::.write_admin_coverage(tmp, coverage)
  back <- utils::read.csv(path)
  testthat::expect_identical(names(back), names(proto))
  testthat::expect_equal(nrow(back), nrow(coverage))
})

# --- Container scope for a depth read (whep#1000 T40) ------------------------

# Two containers in one support, each with its own unit, and ONLY the second
# container is also carried in the cell beside its unit. Read unscoped, the
# double-claim gate refuses the whole read for a clash in a container a
# Japanese run has no business with; scoped to Japan it must not.
#
# The shape is the shipped edge table's, minus the size: the real
# `polity_containment` places Alaska inside `USA-1867-1959`, Singapore inside
# `MYS-1963-1965` and seven more containers at depth 1 beside the 46 Japanese
# prefectures, and the deployed polycell support carries every one of those
# containers' own rows, so `read_level_country_grid(1L)` refuses a Japanese
# run over 3,965 container-cell rows in ten containers.
.lv_two_container_support <- function() {
  tibble::tribble(
    ~polycell_id,  ~cell_id, ~lon,   ~lat,   ~polity_code,
    "AICHI@1",           1L, 137.25, 35.25,  "JPN-AICHI-1871-2025",
    "ALK@9",             9L, -150.25, 65.25, "ALK-1867-1959",
    "USA@9",             9L, -150.25, 65.25, "USA-1867-1959"
  ) |>
    dplyr::mutate(
      area_code = c(110L, 231L, 231L),
      start_year = c(1952L, 1867L, 1867L),
      end_year = c(2025L, 1959L, 1959L),
      cell_area_ha = c(3000, 4000, 4000),
      land_area_ha = c(3000, 3000, 3000)
    )
}

testthat::test_that("an ungranted container's nesting does not abort a run", {
  # FAILS BEFORE T40: `read_level_country_grid()` read every admitted edge, so
  # Alaska sitting inside the USA in the support refused a Japanese read.
  support <- .lv_two_container_support()
  testthat::expect_error(
    whep:::read_level_country_grid(level = 1L, support = support),
    class = "whep_level_support_double_claim"
  )
  grid <- suppressMessages(whep:::read_level_country_grid(
    level = 1L,
    support = support,
    containers = 110L
  ))
  testthat::expect_identical(unique(grid$area_code), 110L)
  testthat::expect_identical(
    unique(grid$level_polity_code),
    "JPN-AICHI-1871-2025"
  )
})

testthat::test_that("scoping is not suppressing: a granted clash aborts", {
  # The other half of the same fixture. Scoped to the container that DOES
  # carry its own row beside its unit, the gate must fire exactly as before:
  # `containers` chooses whose clashes are read, never whether they matter.
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = .lv_two_container_support(),
      containers = 231L
    ),
    class = "whep_level_support_double_claim"
  )
})

testthat::test_that("a container with no edge at this depth is refused", {
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = .lv_two_container_support(),
      containers = c(110L, 724L)
    ),
    class = "whep_level_container_not_admitted"
  )
})

testthat::test_that("containers is refused at level 0", {
  testthat::expect_error(
    whep:::read_level_country_grid(level = 0L, containers = 110L),
    class = "rlang_error"
  )
  testthat::expect_error(
    whep:::read_level_country_grid(level = 1L, containers = "110"),
    class = "rlang_error"
  )
  testthat::expect_null(whep:::.level_check_containers(NULL))
  testthat::expect_identical(
    whep:::.level_check_containers(c(110, 110L, 4)),
    c(4L, 110L)
  )
})

# --- What the double-claim gate says, and which rule it judges by -----------

testthat::test_that("the double-claim abort counts rows and cells apart", {
  # It said "{nrow(clash)} cells" while `nrow(clash)` counts container-cell
  # ROWS, one per (cell, container, epoch): on the deployed support that read
  # "3965 cells" over 2,748 distinct ones. Here one cell carries the same
  # unit's container in two successive epochs, so two rows describe one cell.
  support <- tibble::tribble(
    ~polycell_id,  ~cell_id, ~lon,   ~lat,  ~polity_code,          ~start_year,
    "AICHI@1",           1L, 137.25, 35.25, "JPN-AICHI-1871-2025",       1871L,
    "JPN45@1",           1L, 137.25, 35.25, "JPN-1945-1952",             1945L,
    "JPN52@1",           1L, 137.25, 35.25, "JPN-1952-2025",             1952L
  ) |>
    dplyr::mutate(
      area_code = 110L,
      end_year = c(2025L, 1952L, 2025L),
      # 1,000 + 2,000 ha of a 2,500 ha cell in each epoch: 500 ha of ground
      # the cell does not have, so both epochs are refuted as well as
      # co-present.
      cell_area_ha = 2500,
      land_area_ha = c(1000, 2000, 2000)
    )
  err <- testthat::expect_error(
    suppressWarnings(whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containers = 110L
    )),
    class = "whep_level_support_double_claim"
  )
  msg <- conditionMessage(err)
  testthat::expect_match(msg, "2 container-cell rows over 1 distinct cell")
  testthat::expect_match(msg, "Measured over-claim: 2 of 2")
})

testthat::test_that("the measured rule passes a clash the cell cannot refute", {
  # SGP/MYS's shape: the container and its unit share a cell but their ground
  # is disjoint, so the cell's own area refutes nothing (1,000 + 500 of a
  # 4,000 ha cell). On the deployed support that pair over-claims 0 Mha over
  # its 1 shared cell, and RYU inside JPN-1895-1945 over-claims 0 Mha over 8.
  support <- tibble::tribble(
    ~polycell_id,  ~cell_id, ~lon,   ~lat,  ~polity_code,
    "AICHI@1",           1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
    "JPN@1",             1L, 137.25, 35.25, "JPN-1952-2025"
  ) |>
    dplyr::mutate(
      area_code = 110L,
      start_year = 1952L,
      end_year = 2025L,
      cell_area_ha = 4000,
      land_area_ha = c(1000, 500)
    )
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment()
    ),
    class = "whep_level_support_double_claim"
  )
  testthat::expect_warning(
    suppressMessages(whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment(),
      double_claim = "measured"
    )),
    "does not refute"
  )
  grid <- suppressWarnings(suppressMessages(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment(),
      double_claim = "measured"
    )
  ))
  # PASSING THE CLASH DOES NOT REMOVE THE CONTAINER'S LAND from the cell's
  # denominator, and must not: where the ground really is disjoint, the
  # container genuinely holds its half of the cell, so the unit's share of the
  # cell is 1000 of the 1500 ha the two of them measure between them.
  testthat::expect_equal(grid$cell_area_frac, 1000 / 1500)
})

testthat::test_that("the measured rule still refuses a proven over-claim", {
  # ALK/USA's shape: the unit and its container each claim 3,000 ha of a
  # 4,000 ha cell, so the cell holds 2,000 ha more territory than it has. On
  # the deployed support Alaska over-claims 150 Mha over 1,156 of its 1,372
  # shared cells, which is why the rule keeps refusing it.
  support <- tibble::tribble(
    ~polycell_id,  ~cell_id, ~lon,   ~lat,  ~polity_code,
    "AICHI@1",           1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
    "JPN@1",             1L, 137.25, 35.25, "JPN-1952-2025"
  ) |>
    dplyr::mutate(
      area_code = 110L,
      start_year = 1952L,
      end_year = 2025L,
      cell_area_ha = 4000,
      land_area_ha = 3000
    )
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment(),
      double_claim = "measured"
    ),
    class = "whep_level_support_double_claim"
  )
})

testthat::test_that("the measured rule needs a cell area to measure against", {
  # A support with no `cell_area_ha` gives the rule nothing to refute a claim
  # with; guessing would turn the weaker rule into no rule at all.
  support <- dplyr::select(
    dplyr::bind_rows(
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
    ),
    -"cell_area_ha"
  )
  testthat::expect_error(
    whep:::read_level_country_grid(
      level = 1L,
      support = support,
      containment = .lv_containment(),
      double_claim = "measured"
    ),
    "cell_area_ha"
  )
})

testthat::test_that("an unknown double_claim rule is refused", {
  testthat::expect_error(
    whep:::read_level_country_grid(level = 1L, double_claim = "loose"),
    class = "rlang_error"
  )
})
