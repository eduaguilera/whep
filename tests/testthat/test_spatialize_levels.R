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

# `.lv_layer_inputs()` with one cell only partly claimed, which is the shape
# the LIVE level-0 grid has: read off the polycell support pin, 40 of its
# 68,546 cells sum below 1, the worst claimed to 0.01156708 at (20.75, 42.75),
# and none sums above 1. Country B's own cell (12.25, 40.25) keeps 0.6 of its
# land and the remaining 0.4 is claimed by no reporting polity. `land_area_ha`
# is carried so the shortfall can be reported in hectares: 600 ha claimed out
# of 1000.
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

testthat::test_that("a cell no polity fully claims is reported, not refused", {
  inputs <- .lv_short_layer_inputs()
  layer <- NULL
  testthat::expect_warning(
    layer <- whep:::build_allocation_layer(
      inputs$grid0,
      inputs$grid_deep,
      inputs$granted
    ),
    "no reporting polity claims"
  )
  unclaimed <- attr(layer, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_equal(unclaimed$lon, 12.25)
  testthat::expect_equal(unclaimed$lat, 40.25)
  testthat::expect_equal(unclaimed$claimed_share, 0.6, tolerance = 1e-12)
  testthat::expect_equal(unclaimed$unclaimed_share, 0.4, tolerance = 1e-12)
  testthat::expect_equal(unclaimed$unclaimed_ha, 400, tolerance = 1e-9)
  # The short cell keeps its own row: nothing is dropped, and the shortfall is
  # never back-filled onto whoever else is in the cell.
  short <- dplyr::filter(layer, lon == 12.25, lat == 40.25)
  testthat::expect_equal(nrow(short), 1L)
  testthat::expect_equal(short$cell_area_frac, 0.6, tolerance = 1e-12)
})

testthat::test_that("the shortfall has no hectares without land on the layer", {
  inputs <- .lv_short_layer_inputs()
  grid0 <- dplyr::select(inputs$grid0, -"land_area_ha")
  layer <- NULL
  testthat::expect_warning(
    layer <- whep:::build_allocation_layer(
      grid0,
      inputs$grid_deep,
      inputs$granted
    ),
    "no land column on the layer"
  )
  unclaimed <- attr(layer, "unclaimed_land")
  testthat::expect_equal(nrow(unclaimed), 1L)
  testthat::expect_equal(unclaimed$unclaimed_share, 0.4, tolerance = 1e-12)
  # Not measurable is reported as such, never as a shortfall of 0 ha.
  testthat::expect_true(is.na(unclaimed$unclaimed_ha))
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
  cols <- c("lon", "lat", "claimed_share", "unclaimed_share", "unclaimed_ha")
  layer <- whep:::build_allocation_layer(
    inputs$grid0,
    inputs$grid_deep,
    inputs$granted
  )
  testthat::expect_equal(nrow(attr(layer, "unclaimed_land")), 0L)
  testthat::expect_named(attr(layer, "unclaimed_land"), cols)
  # The no-grant path returns `grid0` unexamined, but still carries both
  # diagnostics so a caller never has to test for their presence.
  none <- whep:::build_allocation_layer(inputs$grid0, inputs$grid_deep, NULL)
  testthat::expect_equal(nrow(attr(none, "unclaimed_land")), 0L)
  testthat::expect_named(attr(none, "unclaimed_land"), cols)
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
  out <- allocate_level_crops(
    .t13_national(),
    .t13_patterns(),
    .t13_cropland(cell_a = 1000, cell_b = 1000),
    .t13_layer(),
    .t13_shares(a = 0, b = 0)
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
