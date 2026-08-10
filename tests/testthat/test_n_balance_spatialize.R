# Helper fixtures --------------------------------------------------------------

.nbs_country_totals <- function() {
  tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L, 100
  )
}

.nbs_crop_shares <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L, 2511L, 0.7, # wheat, item_prod_code 15
    2010L, 10L, 2807L, 0.3 # rice, item_prod_code 27
  )
}

.nbs_cell_polity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
    0.25, 50.25, 10L, 1, 3000,
    0.75, 50.25, 10L, 1, 3000
  )
}

# A polycell-support-shaped feed for the same grid: one border cell shared
# between polities 10 and 20, one interior cell. Land is deliberately BELOW the
# whole-cell area in both cells (inland water and ice are territory, not land)
# and the two cells hold different absolute land, so that
#   a share of the cell's LAND      (10 takes 1800/2400 = 0.75 and 2000/2000 = 1)
#   a share of the WHOLE cell       (1800/3000 = 0.6 and 2000/3000 = 0.667)
#   an absolute area                (1800 and 2000)
# are three different weights and no test can pass on two of them at once.
.nbs_support <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_area_ha, ~cell_area_ha,
    0.25, 50.25, 10L, 1800, 3000,
    0.25, 50.25, 20L, 600, 3000,
    0.75, 50.25, 10L, 2000, 3000
  )
}

# The same support with `polity_frac` alongside `land_area_ha`, so an explicit
# `split` has both keys available and cannot be satisfied by accident. The two
# keys disagree on the border cell: the subcell count says 0.5/0.5, the
# measured land says 0.75/0.25.
.nbs_support_both <- function() {
  dplyr::mutate(.nbs_support(), polity_frac = c(0.5, 0.5, 1))
}

.nbs_crop_patterns <- function() {
  tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.6, # wheat
    0.75, 50.25, 15L, 0.2,
    0.25, 50.25, 27L, 0.1, # rice
    0.75, 50.25, 27L, 0.3
  )
}

.nbs_type_cropland <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~luh2_type, ~type_ha, ~type_irrig_ha,
    0.25, 50.25, 2010L, "c3ann", 1000, 100,
    0.75, 50.25, 2010L, "c3ann", 1000, 50,
    0.25, 50.25, 2010L, "c4ann", 200, 0
  )
}

.nbs_grid_data <- function() {
  list(
    crop_patterns = .nbs_crop_patterns(),
    type_cropland = .nbs_type_cropland()
  )
}

# build_cell_polity --------------------------------------------------------

testthat::test_that("build_cell_polity aborts with no path or env var", {
  withr::local_envvar(WHEP_POLITY_FRACTION_PATH = "")
  testthat::expect_error(
    whep::build_cell_polity(),
    "WHEP_POLITY_FRACTION_PATH"
  )
})

testthat::test_that("build_cell_polity adds cell_area_ha from latitude", {
  path <- withr::local_tempfile(fileext = ".parquet")
  raw <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~polity_frac,
    0.25, 50.25, 10L, 1,
    0.75, 50.25, 10L, 0.5
  )
  nanoparquet::write_parquet(raw, path)

  result <- whep::build_cell_polity(polity_fraction_path = path)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha")
  )
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(result$cell_area_ha > 0))
})

# The grid is rasterized from regions.csv, so it carries reporting-area codes
# that are folded away in polity_area_crosswalk: 212 Syria and 299 Palestine
# both live in bucket 999, and 277 South Sudan in bucket 206 alongside 276
# Sudan. On the deployed cell_polity_fraction parquet 12 such codes cover 819
# cells, and every one of them is invisible to the polity-keyed national
# tables every gridded consumer joins against.
.nbs_off_bucket_grid <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~area_code, ~polity_frac,
    0.25, 50.25,       212L,          1.0,
    0.75, 50.25,       299L,          0.4,
    0.75, 50.25,        68L,          0.6,
    1.25, 50.25,       276L,          0.7,
    1.25, 50.25,       277L,          0.3
  )
}

.nbs_write_grid <- function(raw, envir = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".parquet", .local_envir = envir)
  nanoparquet::write_parquet(raw, path)
  path
}

testthat::test_that("build_cell_polity warns on off-bucket grid codes", {
  path <- .nbs_write_grid(.nbs_off_bucket_grid())

  testthat::expect_warning(
    result <- whep::build_cell_polity(polity_fraction_path = path),
    "cannot join"
  )
  # The default reproduces the parquet's own codes bit-for-bit.
  testthat::expect_equal(result$area_code, c(212L, 299L, 68L, 276L, 277L))
})

testthat::test_that("build_cell_polity keeps on-bucket grids silent", {
  path <- .nbs_write_grid(
    dplyr::filter(.nbs_off_bucket_grid(), area_code == 68L)
  )

  testthat::expect_no_warning(
    whep::build_cell_polity(polity_fraction_path = path)
  )
})

testthat::test_that("area_key = polity_area re-keys the grid on buckets", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  path <- .nbs_write_grid(.nbs_off_bucket_grid())

  result <- whep::build_cell_polity(
    polity_fraction_path = path,
    area_key = "polity_area"
  )

  # 212 and 299 resolve to Rest of World, 276 and 277 both to Sudan (former).
  pointblank::expect_col_vals_in_set(
    result,
    columns = "area_code",
    set = c(68L, 206L, 999L)
  )
  # The Sudan cell held two areas of one bucket: they collapse to one row.
  sudan <- dplyr::filter(result, lon == 1.25)
  testthat::expect_equal(nrow(sudan), 1L)
  testthat::expect_equal(sudan$area_code, 206L)
  testthat::expect_equal(sudan$polity_frac, 1)
})

testthat::test_that("area_key = polity_area conserves each cell's fractions", {
  path <- .nbs_write_grid(.nbs_off_bucket_grid())

  result <- whep::build_cell_polity(
    polity_fraction_path = path,
    area_key = "polity_area"
  )

  sums <- result |>
    dplyr::summarise(total = sum(polity_frac), .by = c(lon, lat))
  pointblank::expect_col_vals_equal(sums, columns = "total", value = 1)
  # No cell is lost and no code appears twice in one cell.
  testthat::expect_equal(
    nrow(dplyr::distinct(result, lon, lat)),
    nrow(dplyr::distinct(.nbs_off_bucket_grid(), lon, lat))
  )
  testthat::expect_equal(
    nrow(dplyr::distinct(result, lon, lat, area_code)),
    nrow(result)
  )
})

testthat::test_that("area_key = polity_area emits no off-bucket code", {
  path <- .nbs_write_grid(.nbs_off_bucket_grid())

  result <- whep::build_cell_polity(
    polity_fraction_path = path,
    area_key = "polity_area"
  )

  testthat::expect_equal(whep:::.cell_polity_off_bucket(result), integer(0))
})

testthat::test_that("build_cell_polity rejects an unknown area_key", {
  path <- .nbs_write_grid(.nbs_off_bucket_grid())

  testthat::expect_error(
    whep::build_cell_polity(polity_fraction_path = path, area_key = "polity"),
    class = "rlang_error"
  )
})

# spatialize_country_n_to_crops --------------------------------------------

testthat::test_that("polity_crop resolution splits totals by area share", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = NULL,
    resolution = "polity_crop"
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "n_t")
  )
  testthat::expect_equal(sum(result$n_t), 100)
  wheat <- result$n_t[result$item_cbs_code == 2511L]
  rice <- result$n_t[result$item_cbs_code == 2807L]
  testthat::expect_equal(wheat, 70)
  testthat::expect_equal(rice, 30)
})

testthat::test_that(".n_warn_unmatched warns (not errors) for several codes", {
  # Regression: the "i" bullet interpolated `item_cbs_code{?s}: {codes}` with a
  # NUMERIC `codes` vector, so cli's make_quantity() hit stopifnot(length == 1)
  # and ABORTED whenever >= 2 crops were unmatched (770 of them for a real 2010
  # run). A warning about reallocation must never turn into an error.
  unmatched <- tibble::tibble(
    item_cbs_code = c(2511L, 2807L, 2513L),
    n_t = c(1, 2, 3)
  )

  testthat::expect_warning(
    result <- whep:::.n_warn_unmatched(unmatched),
    "no crop-pattern grid cells"
  )
  # It must not raise a condition of class "error".
  testthat::expect_no_error(
    withCallingHandlers(
      whep:::.n_warn_unmatched(unmatched),
      warning = function(w) invokeRestart("muffleWarning")
    )
  )
})

testthat::test_that("polity_crop aborts rather than dropping a total with no crop shares", {
  shares <- dplyr::mutate(.nbs_crop_shares(), year = 2011L)

  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = shares,
      cell_polity = NULL,
      resolution = "polity_crop"
    ),
    "no crop-area shares"
  )
})

testthat::test_that("grid resolution conserves mass to the country total", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_cell_polity(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "area_code", "year", "item_cbs_code", "n_t")
  )
  testthat::expect_equal(sum(result$n_t), sum(.nbs_country_totals()$n_t))
})

testthat::test_that("grid cells split within a crop by crop-pattern hectares", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_cell_polity(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  wheat <- result[result$item_cbs_code == 2511L, ]
  # wheat crop_pattern_ha: cell1 = 1200*0.6=720, cell2 = 1000*0.2=200
  # cell1 share = 720/920, cell2 share = 200/920, of the 70 t wheat total
  wheat_cell1 <- wheat$n_t[wheat$lon == 0.25]
  wheat_cell2 <- wheat$n_t[wheat$lon == 0.75]
  testthat::expect_equal(wheat_cell1, 70 * 720 / 920)
  testthat::expect_equal(wheat_cell2, 70 * 200 / 920)
  testthat::expect_equal(sum(wheat$n_t), 70)
})

# crop_shares whose second crop (barley, item_cbs_code 2513 -> item_prod_code
# 44) is absent from the crop-pattern raster below.
.nbs_crop_shares_unmatched <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L, 2511L, 0.7, # wheat, item_prod_code 15 (in the raster)
    2010L, 10L, 2513L, 0.3 # barley, item_prod_code 44 (NOT in the raster)
  )
}

# crop_patterns carrying hectares only for wheat (item_prod_code 15); barley
# (44) has no cells, triggering the uniform-cropland fallback.
.nbs_crop_patterns_wheat_only <- function() {
  tibble::tribble(
    ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
    0.25, 50.25, 15L, 0.6,
    0.75, 50.25, 15L, 0.2
  )
}

.nbs_grid_data_unmatched <- function() {
  list(
    crop_patterns = .nbs_crop_patterns_wheat_only(),
    type_cropland = .nbs_type_cropland()
  )
}

testthat::test_that("grid warns when a crop is absent from patterns", {
  testthat::expect_warning(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares_unmatched(),
      cell_polity = .nbs_cell_polity(),
      resolution = "grid",
      data = .nbs_grid_data_unmatched()
    ),
    "2513"
  )
})

testthat::test_that("grid conserves mass when a crop is absent from patterns", {
  result <- suppressWarnings(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares_unmatched(),
      cell_polity = .nbs_cell_polity(),
      resolution = "grid",
      data = .nbs_grid_data_unmatched()
    )
  )

  testthat::expect_equal(sum(result$n_t), sum(.nbs_country_totals()$n_t))
  wheat <- result$n_t[result$item_cbs_code == 2511L]
  barley <- result$n_t[result$item_cbs_code == 2513L]
  testthat::expect_equal(sum(wheat), 70)
  testthat::expect_equal(sum(barley), 30)
})

testthat::test_that("absent crop is spread across cropland cells by area", {
  result <- suppressWarnings(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares_unmatched(),
      cell_polity = .nbs_cell_polity(),
      resolution = "grid",
      data = .nbs_grid_data_unmatched()
    )
  )

  barley <- result[result$item_cbs_code == 2513L, ]
  # cropland type_ha: cell1 = 1000+200 = 1200, cell2 = 1000; total 2200.
  # 30 t barley split 1200/2200 and 1000/2200 across the two cropland cells.
  barley_cell1 <- barley$n_t[barley$lon == 0.25]
  barley_cell2 <- barley$n_t[barley$lon == 0.75]
  testthat::expect_equal(barley_cell1, 30 * 1200 / 2200)
  testthat::expect_equal(barley_cell2, 30 * 1000 / 2200)
})

testthat::test_that("grid aborts rather than dropping N when no fallback cropland exists", {
  zero_cropland <- .nbs_grid_data_unmatched()
  zero_cropland$type_cropland$type_ha <- 0

  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares_unmatched(),
      cell_polity = .nbs_cell_polity(),
      resolution = "grid",
      data = zero_cropland
    ),
    "no positive cropland"
  )
})

testthat::test_that("grid resolution requires cell_polity", {
  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = NULL,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "cell_polity"
  )
})

testthat::test_that("missing required columns abort with a clear message", {
  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = dplyr::select(.nbs_country_totals(), -n_t),
      crop_shares = .nbs_crop_shares(),
      cell_polity = NULL,
      resolution = "polity_crop"
    ),
    "country_totals"
  )
})

# C5: the polycell split key ------------------------------------------------

testthat::test_that("auto takes the measured land when the support carries it", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_support(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  testthat::expect_true(all(result$method_polity_split == "land_area_ha"))
})

testthat::test_that("auto prefers measured land when both keys are present", {
  # Without this the migration can be undone by preference order alone: `auto`
  # would keep choosing the coarser key on exactly the supports that carry the
  # finer one, and every caller would go on producing crosswalk numbers under a
  # migrated support.
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_support_both(),
    resolution = "grid",
    data = .nbs_grid_data()
  )
  wheat <- result$n_t[result$item_cbs_code == 2511L & result$lon == 0.25]

  testthat::expect_true(all(result$method_polity_split == "land_area_ha"))
  testthat::expect_equal(wheat, 70 * 540 / 740)
})

testthat::test_that("auto takes polity_frac when there is no measured land", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_cell_polity(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  testthat::expect_true(all(result$method_polity_split == "polity_frac"))
})

testthat::test_that("an explicit key the support lacks aborts, never downgrades", {
  # The failure this forbids: a caller asking for the geodesic split, being
  # handed subcell-count numbers, and having no way to tell afterwards.
  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = .nbs_cell_polity(),
      resolution = "grid",
      split = "land_area_ha",
      data = .nbs_grid_data()
    ),
    "land_area_ha"
  )
})

testthat::test_that("an explicit polity_frac is not upgraded either", {
  both <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_support_both(),
    resolution = "grid",
    split = "polity_frac",
    data = .nbs_grid_data()
  )
  wheat <- both$n_t[both$item_cbs_code == 2511L & both$lon == 0.25]

  testthat::expect_true(all(both$method_polity_split == "polity_frac"))
  # 0.5 of the border cell and all of the interior one: 720*0.5 vs 200*1.
  testthat::expect_equal(wheat, 70 * 360 / 560)
})

testthat::test_that("the split key is a share of the cell's LAND", {
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = .nbs_support(),
    resolution = "grid",
    data = .nbs_grid_data()
  )

  wheat <- result[result$item_cbs_code == 2511L, ]
  # wheat crop_pattern_ha: cell1 = 1200*0.6 = 720, cell2 = 1000*0.2 = 200.
  # Polity 10 holds 1800/2400 of cell1's land and 2000/2000 of cell2's, so the
  # weights are 540 and 200. Under `land_area_ha / cell_area_ha` they would be
  # 432 and 133.3; under the absolute area, 1,296,000 and 400,000; under a
  # cell-level key, 720 and 200. All four differ.
  testthat::expect_equal(wheat$n_t[wheat$lon == 0.25], 70 * 540 / 740)
  testthat::expect_equal(wheat$n_t[wheat$lon == 0.75], 70 * 200 / 740)
})

testthat::test_that("an absolute area is not a normalised weight", {
  # Two interior cells, so both keys give every cell a fraction of exactly 1
  # and the grid output must be bit-identical to the crosswalk's. It is not if
  # `land_area_ha` itself is used as the weight, because the two cells carry
  # different absolute land: that is the ~11% signal this consumer exists to
  # catch cheaply.
  interior <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_area_ha, ~cell_area_ha,
    0.25, 50.25, 10L, 2400, 3000,
    0.75, 50.25, 10L, 2000, 3000
  )
  args <- list(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    resolution = "grid",
    data = .nbs_grid_data()
  )
  old <- do.call(
    whep::spatialize_country_n_to_crops,
    c(args, list(cell_polity = .nbs_cell_polity()))
  )
  new <- do.call(
    whep::spatialize_country_n_to_crops,
    c(args, list(cell_polity = interior))
  )

  testthat::expect_identical(
    dplyr::select(new, -"method_polity_split"),
    dplyr::select(old, -"method_polity_split")
  )
})

testthat::test_that("the polity total is invariant under the key swap", {
  # The control-case property: the cell weights are renormalised inside each
  # polity-crop-year, so a change of partition redistributes the national total
  # and cannot change it. Measured on a border fixture where the shares DO
  # move, so this is not passing on an unchanged input.
  args <- list(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    resolution = "grid",
    data = .nbs_grid_data()
  )
  old <- do.call(
    whep::spatialize_country_n_to_crops,
    c(args, list(cell_polity = .nbs_support_both(), split = "polity_frac"))
  )
  new <- do.call(
    whep::spatialize_country_n_to_crops,
    c(args, list(cell_polity = .nbs_support_both(), split = "land_area_ha"))
  )
  totals <- function(x) {
    x |>
      dplyr::summarise(n_t = sum(.data$n_t), .by = c("year", "area_code")) |>
      dplyr::arrange(.data$year, .data$area_code)
  }

  testthat::expect_equal(totals(new), totals(old))
  testthat::expect_false(isTRUE(all.equal(new$n_t, old$n_t)))
})

testthat::test_that("a border cell delivers to each polity only its own share", {
  # S-A6, the pig case: two polities share a cell and carry deliberately
  # incompatible national totals. Neither may receive any part of the other's.
  totals <- tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L, 100,
    2010L, 20L, 7
  )
  shares <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L, 2511L, 1,
    2010L, 20L, 2511L, 1
  )
  result <- whep::spatialize_country_n_to_crops(
    country_totals = totals,
    crop_shares = shares,
    cell_polity = .nbs_support(),
    resolution = "grid",
    data = .nbs_grid_data()
  )
  by_polity <- result |>
    dplyr::summarise(n_t = sum(.data$n_t), .by = "area_code") |>
    dplyr::arrange(.data$area_code)

  testthat::expect_equal(by_polity$n_t, c(100, 7))
  # Polity 20 holds only the border cell, so all of its nitrogen lands there.
  testthat::expect_equal(
    sort(unique(result$lon[result$area_code == 20L])),
    0.25
  )
})

testthat::test_that("cell_frac is a partition of each cell's land", {
  frac <- whep:::.n_cell_frac(.nbs_support(), "land_area_ha")
  per_cell <- frac |>
    dplyr::summarise(total = sum(.data$cell_frac), .by = c("lon", "lat"))

  testthat::expect_equal(per_cell$total, rep(1, nrow(per_cell)))
  testthat::expect_equal(frac$cell_frac, c(0.75, 0.25, 1))
})

testthat::test_that("a duplicated area_code in a cell is refused, not folded", {
  # DA-23: `polity_area_crosswalk` folds Sudan and South Sudan onto 206. Folding
  # two polities' shares here would look exactly like a partition afterwards.
  folded <- dplyr::mutate(.nbs_support(), area_code = 10L)

  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = folded,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "duplicated"
  )
})

testthat::test_that("an NA area_code is refused too", {
  unkeyed <- .nbs_support()
  unkeyed$area_code[2] <- NA_integer_

  testthat::expect_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = unkeyed,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "duplicated"
  )
})

testthat::test_that("the transitional key is used as supplied, not repaired", {
  # `polity_frac` is a partition of the cell by construction, so a support whose
  # fractions do not sum to 1 is broken and must lose weight VISIBLY. Silently
  # renormalising it here would repair a broken crosswalk behind shares that
  # then look complete -- and would make the transitional path stop reproducing
  # today's numbers, which DA-13 requires it to do.
  partial <- dplyr::mutate(.nbs_cell_polity(), polity_frac = c(0.5, 1))
  result <- whep::spatialize_country_n_to_crops(
    country_totals = .nbs_country_totals(),
    crop_shares = .nbs_crop_shares(),
    cell_polity = partial,
    resolution = "grid",
    data = .nbs_grid_data()
  )
  wheat <- result$n_t[result$item_cbs_code == 2511L & result$lon == 0.25]

  # 720*0.5 against 200*1. Renormalised per cell both would become 1 and the
  # answer would be 70*720/920.
  testthat::expect_equal(wheat, 70 * 360 / 560)
})

testthat::test_that("the refusal does not reach the transitional key", {
  # DA-13: the crosswalk path must stay exactly what it is today, so the DA-23
  # refusal is a property of the polycell key only.
  duped <- dplyr::bind_rows(.nbs_cell_polity(), .nbs_cell_polity()[1, ])

  testthat::expect_no_error(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = duped,
      resolution = "grid",
      split = "polity_frac",
      data = .nbs_grid_data()
    )
  )
})

testthat::test_that("an NA or negative land area aborts", {
  # The DA-13 shim's `coverage_status == "crosswalk_only"` rows carried NA in
  # every area column; C9 removed them, so the producer no longer emits any.
  # The support still arrives from the caller, and weighting an NA would delete
  # one polity's claim while the rest of the cell still looked like a complete
  # partition, so the refusal is asserted on a hand-made NA rather than retired
  # with the padding that used to produce one.
  padded <- .nbs_support()
  padded$land_area_ha[2] <- NA_real_
  negative <- .nbs_support()
  negative$land_area_ha[2] <- -1

  testthat::expect_error(
    whep:::.n_cell_frac(padded, "land_area_ha"),
    "finite and non-negative"
  )
  testthat::expect_error(
    whep:::.n_cell_frac(negative, "land_area_ha"),
    "finite and non-negative"
  )
})

testthat::test_that("a landless cell is named and dropped, and mass is kept", {
  landless <- .nbs_support()
  landless$land_area_ha[landless$lon == 0.75] <- 0

  testthat::expect_warning(
    result <- whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = landless,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "land_area_ha"
  )

  testthat::expect_equal(sum(result$n_t), 100)
  testthat::expect_false(any(result$lon == 0.75))
})

testthat::test_that("the fallback reallocation carries the split stamp too", {
  result <- suppressWarnings(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares_unmatched(),
      cell_polity = .nbs_support(),
      resolution = "grid",
      data = .nbs_grid_data_unmatched()
    )
  )
  barley <- result[result$item_cbs_code == 2513L, ]

  testthat::expect_true(all(result$method_polity_split == "land_area_ha"))
  # cropland type_ha weighted by the land share: 1200*0.75 = 900 and 1000*1.
  testthat::expect_equal(barley$n_t[barley$lon == 0.25], 30 * 900 / 1900)
  testthat::expect_equal(sum(result$n_t), 100)
})

# .n_crop_rate_shares (Coello rate-weighted, conserving crop shares) ----------

.nrs_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2010L, 10L, 2511L, "ha", 100, # wheat
    2010L, 10L, 2514L, "ha", 100, # maize
    2010L, 10L, 3000L, "ha", 500 # grassland (excluded)
  )
}
.nrs_coello_rates <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~kg_n_ha,
    2010L, 10L, 2511L, 150, # wheat: high rate
    2010L, 10L, 2514L, 50 # maize: low rate
  )
}

testthat::test_that(".n_crop_rate_shares conserves + differentiates", {
  res <- whep:::.n_crop_rate_shares(
    .nrs_primary_prod(),
    .nrs_coello_rates()
  )
  testthat::expect_equal(sum(res$area_share), 1)
  wheat <- res$area_share[res$item_cbs_code == 2511L]
  maize <- res$area_share[res$item_cbs_code == 2514L]
  testthat::expect_gt(wheat, maize) # equal area, higher rate -> higher share
  # wheat weight (rate 150 x 100 ha) over the two-crop weight sum is 0.75
  testthat::expect_equal(wheat, 0.75)
  testthat::expect_true(all(res$method_synthetic == "coello"))
  testthat::expect_false(any(res$item_cbs_code == 3000L)) # grass excluded
})

testthat::test_that(".n_crop_rate_shares falls back to area shares", {
  empty <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~kg_n_ha,
    2010L, 88L, 2511L, 150 # no coverage for area 10
  )
  res <- whep:::.n_crop_rate_shares(.nrs_primary_prod(), empty)
  testthat::expect_equal(sum(res$area_share), 1)
  testthat::expect_equal(res$area_share[res$item_cbs_code == 2511L], 0.5)
  testthat::expect_true(all(res$method_synthetic == "area_share"))
})

# .n_synthetic_crop_shares (method dispatcher) --------------------------------

testthat::test_that(".n_synthetic_crop_shares dispatches on method", {
  coello <- whep:::.n_synthetic_crop_shares(
    .nrs_primary_prod(),
    "coello",
    .nrs_coello_rates()
  )
  testthat::expect_equal(
    coello$area_share[coello$item_cbs_code == 2511L],
    0.75
  )
  testthat::expect_true(all(coello$method_synthetic == "coello"))

  area <- whep:::.n_synthetic_crop_shares(
    .nrs_primary_prod(),
    "area_share"
  )
  testthat::expect_equal(
    area$area_share[area$item_cbs_code == 2511L],
    0.5
  )
  testthat::expect_true(all(area$method_synthetic == "area_share"))
})

testthat::test_that(".n_synthetic_crop_shares rejects bad method", {
  testthat::expect_error(
    whep:::.n_synthetic_crop_shares(.nrs_primary_prod(), "smil"),
    "method"
  )
})

testthat::test_that("a country-year with no positive rate falls back cleanly", {
  primary_prod <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~unit,
    ~value,
    2010L,
    10L,
    2511L,
    "ha",
    100,
    2010L,
    20L,
    2511L,
    "ha",
    50
  )
  # Polity 20's only Coello rate is zero, so it is not "covered" and takes the
  # harvested-area branch. Shares stay finite and the basis is stamped, rather
  # than the country-year emitting a NaN share.
  rates <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~kg_n_ha,
    2010L,
    10L,
    2511L,
    80,
    2010L,
    20L,
    2511L,
    0
  )
  shares <- whep:::.n_synthetic_crop_shares(primary_prod, "coello", rates)
  testthat::expect_true(all(is.finite(shares$area_share)))
  testthat::expect_equal(
    shares$method_synthetic[shares$area_code == 10L],
    "coello"
  )
  testthat::expect_equal(
    shares$method_synthetic[shares$area_code == 20L],
    "area_share"
  )
})

# The abort that reports an ambiguous crosswalk used to lose its own message:
# the plural marker sits ahead of the INTEGER code vector with nothing numeric
# before it, so cli read the quantity off the vector and died on
# "length(object) == 1 is not TRUE" -- hiding which codes were at fault, exactly
# when someone needs them. Same class as #618; see #621.
testthat::test_that("an ambiguous crosswalk names the offending area codes", {
  testthat::local_mocked_bindings(
    .polity_crosswalk = function() {
      tibble::tribble(
        ~area_code, ~polity_area_code,
        41L, 41L,
        41L, 214L,
        96L, 96L,
        96L, 344L
      )
    }
  )

  testthat::expect_error(.cell_polity_bucket_lookup(), "Ambiguous area code")
  testthat::expect_error(.cell_polity_bucket_lookup(), "41")
})

# ---- polity_validity (#675) -------------------------------------------

# The same fixtures re-keyed onto area 277 (South Sudan, SSD-2011-2025),
# whose polity postdates the fixture's 2010 year.
.nbs_out_of_span_totals <- function() {
  dplyr::mutate(.nbs_country_totals(), area_code = 277L)
}

.nbs_out_of_span_shares <- function() {
  dplyr::mutate(.nbs_crop_shares(), area_code = 277L)
}

testthat::test_that("polity_crop resolution names an anachronistic polity", {
  testthat::expect_warning(
    result <- whep::spatialize_country_n_to_crops(
      country_totals = .nbs_out_of_span_totals(),
      crop_shares = .nbs_out_of_span_shares(),
      cell_polity = NULL,
      resolution = "polity_crop"
    ),
    "did not exist in that row's year"
  )

  # "keep" is the default: every row and the whole 100 t survive.
  testthat::expect_equal(sum(result$n_t), 100)
})

testthat::test_that("spatialize_country_n_to_crops honours drop and flag", {
  testthat::expect_warning(
    dropped <- whep::spatialize_country_n_to_crops(
      country_totals = .nbs_out_of_span_totals(),
      crop_shares = .nbs_out_of_span_shares(),
      cell_polity = NULL,
      resolution = "polity_crop",
      polity_validity = "drop"
    )
  )
  testthat::expect_warning(
    flagged <- whep::spatialize_country_n_to_crops(
      country_totals = .nbs_out_of_span_totals(),
      crop_shares = .nbs_out_of_span_shares(),
      cell_polity = NULL,
      resolution = "polity_crop",
      polity_validity = "flag"
    )
  )

  testthat::expect_equal(nrow(dropped), 0L)
  testthat::expect_true(all(flagged$reporting_polity_out_of_span))
})

testthat::test_that("grid resolution reports validity too", {
  cell_polity <- dplyr::mutate(.nbs_cell_polity(), area_code = 277L)

  testthat::expect_warning(
    result <- whep::spatialize_country_n_to_crops(
      country_totals = .nbs_out_of_span_totals(),
      crop_shares = .nbs_out_of_span_shares(),
      cell_polity = cell_polity,
      resolution = "grid",
      data = .nbs_grid_data()
    ),
    "did not exist in that row's year"
  )

  testthat::expect_equal(sum(result$n_t), 100)
})

testthat::test_that("an in-span key reports nothing", {
  testthat::expect_no_warning(
    whep::spatialize_country_n_to_crops(
      country_totals = .nbs_country_totals(),
      crop_shares = .nbs_crop_shares(),
      cell_polity = NULL,
      resolution = "polity_crop"
    )
  )
})
