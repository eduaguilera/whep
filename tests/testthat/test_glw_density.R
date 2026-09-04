# Tests for R/glw_density.R (plan T15a-ii, the GLW3 data mechanism).
#
# `read_glw_density()` is reached through `whep:::` rather than `whep::`:
# roxygen has not run on this branch yet, so it is not in NAMESPACE, and
# the dispatcher's documentation pass exports it.
#
# Nothing here reaches the network or a `WHEP_*` path. The raster leg
# writes its own tiny GeoTIFF into a temporary directory and passes it
# through `glw_dir`; the crosswalk and aggregation legs are plain tibbles
# and need no terra at all. Every test that could otherwise inherit a real
# `WHEP_GLW3_DIR` from the developer's machine unsets it first.

# --- fixtures ---------------------------------------------------------

# A minimal crosswalk exercising both rules at once: one species feeding
# two groups (cattle) and two species feeding one group (sheep + goats).
glw_test_crosswalk <- function() {
  tibble::tribble(
    ~glw_species, ~glw_code, ~species_group,
    "cattle",     "Ct",      "cattle_dairy",
    "cattle",     "Ct",      "cattle_non_dairy",
    "sheep",      "Sh",      "sheep_goats",
    "goats",      "Gt",      "sheep_goats"
  )
}

# Per-cell head counts as `.read_glw_species()` returns them.
glw_test_counts <- function() {
  tibble::tribble(
    ~lon,   ~lat, ~heads, ~glw_species,
    10.25, 40.25,    100, "cattle",
    10.75, 40.25,     40, "cattle",
    10.25, 40.25,      7, "sheep",
    10.25, 40.25,      3, "goats",
    10.75, 40.25,      5, "goats"
  )
}

# A 12 x 12 five-arcmin raster over one degree square, so it aggregates to
# exactly four 0.5-degree cells. Values 1..144 in row-major order from the
# north, which makes each block sum checkable by hand.
glw_write_test_raster <- function(dir, code = "Ct", variant = "Da") {
  raster <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 10,
    xmax = 11,
    ymin = 40,
    ymax = 41,
    crs = "EPSG:4326"
  )
  terra::values(raster) <- seq_len(144)
  path <- file.path(dir, sprintf("5_%s_2010_%s.tif", code, variant))
  terra::writeRaster(raster, path, overwrite = TRUE)
  path
}

# --- the packaged crosswalk -------------------------------------------

test_that("the packaged crosswalk covers the eight GLW3 species", {
  crosswalk <- whep:::.read_glw_crosswalk()

  expect_true(all(
    c("glw_species", "glw_code", "species_group") %in% names(crosswalk)
  ))
  expect_setequal(
    unique(crosswalk$glw_species),
    c(
      "buffaloes",
      "cattle",
      "chickens",
      "ducks",
      "goats",
      "horses",
      "pigs",
      "sheep"
    )
  )
  # One file per species, so exactly one code each.
  expect_equal(
    dplyr::n_distinct(crosswalk$glw_species),
    nrow(dplyr::distinct(crosswalk, glw_species, glw_code))
  )
})

test_that("camels and other are absent from the crosswalk by design", {
  crosswalk <- whep:::.read_glw_crosswalk()

  # GLW3 has no camel and no catch-all layer, so those groups must not
  # appear: build_gridded_livestock(proxy_method = "glw3") aborts naming
  # them rather than giving them another species' geography.
  expect_false(any(c("camels", "other") %in% crosswalk$species_group))
  mapped <- unique(crosswalk$species_group)
  groups <- unique(whep:::.read_livestock_mapping()$species_group)
  expect_setequal(setdiff(groups, mapped), c("camels", "other"))
})

test_that("a crosswalk with two codes for one species is refused", {
  bad <- dplyr::mutate(
    glw_test_crosswalk(),
    glw_code = c("Ct", "XX", "Sh", "Gt")
  )

  expect_error(
    whep:::.check_glw_crosswalk(bad),
    "glw_code"
  )
})

test_that("a duplicated crosswalk row is refused, not deduplicated", {
  bad <- dplyr::bind_rows(
    glw_test_crosswalk(),
    dplyr::slice(glw_test_crosswalk(), 1)
  )

  expect_error(
    whep:::.check_glw_crosswalk(bad),
    "duplicated"
  )
})

test_that("a crosswalk missing a required column aborts", {
  expect_error(
    whep:::.check_glw_crosswalk(
      dplyr::select(glw_test_crosswalk(), -glw_code)
    ),
    "glw_code"
  )
})

# --- the crosswalk rules ----------------------------------------------

test_that("one GLW species feeding two groups gives each the same count", {
  out <- whep:::.apply_glw_crosswalk(
    glw_test_counts(),
    glw_test_crosswalk()
  )

  dairy <- dplyr::filter(out, species_group == "cattle_dairy")
  non_dairy <- dplyr::filter(out, species_group == "cattle_non_dairy")
  expect_equal(dairy$density, c(100, 40))
  expect_equal(non_dairy$density, c(100, 40))
})

test_that("two GLW species feeding one group are summed", {
  out <- whep:::.apply_glw_crosswalk(
    glw_test_counts(),
    glw_test_crosswalk()
  )

  sheep_goats <- out |>
    dplyr::filter(species_group == "sheep_goats") |>
    dplyr::arrange(lon)
  expect_equal(sheep_goats$lon, c(10.25, 10.75))
  # 7 sheep + 3 goats in the first cell, goats only in the second.
  expect_equal(sheep_goats$density, c(10, 5))
})

test_that("the crosswalk output is exactly the engine's contract", {
  out <- whep:::.apply_glw_crosswalk(
    glw_test_counts(),
    glw_test_crosswalk()
  )

  expect_named(out, c("lon", "lat", "species_group", "density"))
  expect_true(all(out$density > 0))
  # The engine's own input check must accept it for the groups it covers.
  expect_silent(
    whep:::.check_livestock_proxy_inputs(
      "glw3",
      out,
      c("cattle_dairy", "cattle_non_dairy", "sheep_goats")
    )
  )
})

test_that("a group with no positive count is dropped", {
  counts <- dplyr::mutate(
    glw_test_counts(),
    heads = dplyr::if_else(glw_species == "cattle", 0, heads)
  )

  out <- whep:::.apply_glw_crosswalk(counts, glw_test_crosswalk())

  expect_false(any(
    c("cattle_dairy", "cattle_non_dairy") %in% out$species_group
  ))
  expect_setequal(out$species_group, "sheep_goats")
})

test_that("counts missing a contract column abort", {
  expect_error(
    whep:::.apply_glw_crosswalk(
      dplyr::select(glw_test_counts(), -heads),
      glw_test_crosswalk()
    ),
    "heads"
  )
})

# --- the aggregation factor -------------------------------------------

test_that("five arc-minutes tiles a 0.5-degree cell six times", {
  expect_identical(whep:::.glw_agg_factor(c(1 / 12, 1 / 12)), 6L)
  expect_identical(whep:::.glw_agg_factor(c(0.25, 0.25)), 2L)
  expect_identical(whep:::.glw_agg_factor(c(0.5, 0.5)), 1L)
})

test_that("a resolution that does not tile the cell aborts", {
  expect_error(whep:::.glw_agg_factor(c(0.3, 0.3)), "tile")
  # Coarser than the target: aggregating would invent detail.
  expect_error(whep:::.glw_agg_factor(c(1, 1)), "tile")
})

test_that("a non-square grid aborts", {
  expect_error(whep:::.glw_agg_factor(c(1 / 12, 1 / 6)), "square")
})

# --- file naming and directory resolution -----------------------------

test_that("the variant selects the published file name", {
  expect_identical(whep:::.glw_file_name("Ct", "DA"), "5_Ct_2010_Da.tif")
  expect_identical(whep:::.glw_file_name("Dk", "AW"), "6_Dk_2010_Aw.tif")
})

test_that("an unset WHEP_GLW3_DIR aborts naming the download script", {
  withr::local_envvar(WHEP_GLW3_DIR = "")

  expect_error(
    whep:::read_glw_density(species = "cattle"),
    "download_glw3"
  )
})

test_that("a directory that does not exist aborts naming it", {
  withr::local_envvar(WHEP_GLW3_DIR = "")
  missing_dir <- file.path(withr::local_tempdir(), "absent")

  expect_error(
    whep:::read_glw_density(species = "cattle", glw_dir = missing_dir),
    "does not exist"
  )
})

test_that("an unknown species aborts listing the known ones", {
  expect_error(
    whep:::.select_glw_species("llamas", glw_test_crosswalk()),
    "llamas"
  )
  expect_error(
    whep:::.select_glw_species("llamas", glw_test_crosswalk()),
    "cattle"
  )
})

test_that("NULL species reads every species in the crosswalk", {
  expect_setequal(
    whep:::.select_glw_species(NULL, glw_test_crosswalk()),
    c("cattle", "goats", "sheep")
  )
})

test_that("a missing raster aborts naming the paths searched", {
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()

  expect_error(
    whep:::read_glw_density(
      species = "cattle",
      glw_dir = dir,
      crosswalk = glw_test_crosswalk()
    ),
    "5_Ct_2010_Da.tif"
  )
})

# --- the raster path --------------------------------------------------

test_that("a 12 x 12 raster block-sums to four 0.5-degree cells", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  glw_write_test_raster(dir)

  out <- whep:::read_glw_density(
    species = "cattle",
    glw_dir = dir,
    crosswalk = glw_test_crosswalk()
  )

  expect_named(out, c("lon", "lat", "species_group", "density"))
  expect_setequal(
    out$species_group,
    c("cattle_dairy", "cattle_non_dairy")
  )
  dairy <- dplyr::filter(out, species_group == "cattle_dairy")
  expect_equal(nrow(dairy), 4L)
  # Cell centres are WHEP's, not the source raster's.
  expect_setequal(dairy$lon, c(10.25, 10.75))
  expect_setequal(dairy$lat, c(40.25, 40.75))
  # The north-west block holds rows 1-6, columns 1-6 of 1..144:
  # sum over r in 0..5 of (72r + 21) = 1206.
  north_west <- dplyr::filter(dairy, lon == 10.25, lat == 40.75)
  expect_equal(north_west$density, 1206)
  # The four blocks partition the raster, so they sum to sum(1:144).
  expect_equal(sum(dairy$density), sum(seq_len(144)))
})

test_that("a raster in a GLW3 subdirectory of glw_dir is found", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  parent <- withr::local_tempdir()
  dir.create(file.path(parent, "GLW3"))
  glw_write_test_raster(file.path(parent, "GLW3"))

  out <- whep:::read_glw_density(
    species = "cattle",
    glw_dir = parent,
    crosswalk = glw_test_crosswalk()
  )

  expect_equal(nrow(out), 8L)
})

test_that("missing pixels are skipped, not turned into a missing cell", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  path <- glw_write_test_raster(dir)
  raster <- terra::rast(path)
  values <- terra::values(raster)
  # Blank one pixel of the north-west block and the whole north-east one.
  values[1L] <- NA
  values[c(7:12, 19:24, 31:36, 43:48, 55:60, 67:72)] <- NA
  terra::values(raster) <- values
  terra::writeRaster(raster, path, overwrite = TRUE)

  out <- whep:::read_glw_density(
    species = "cattle",
    glw_dir = dir,
    crosswalk = glw_test_crosswalk()
  ) |>
    dplyr::filter(species_group == "cattle_dairy")

  # The north-west block loses only the blanked pixel, not the cell.
  north_west <- dplyr::filter(out, lon == 10.25, lat == 40.75)
  expect_equal(north_west$density, 1206 - 1)
  # The all-missing block yields no row at all.
  expect_equal(nrow(dplyr::filter(out, lon == 10.75, lat == 40.75)), 0L)
  expect_equal(nrow(out), 3L)
})

test_that("the AW variant reads its own file, never the DA one", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  glw_write_test_raster(dir, variant = "Da")

  # Only the dasymetric raster is on disk, so the areal-weighted read must
  # abort rather than fall back to it.
  expect_error(
    whep:::read_glw_density(
      species = "cattle",
      variant = "AW",
      glw_dir = dir,
      crosswalk = glw_test_crosswalk()
    ),
    "6_Ct_2010_Aw.tif"
  )
})

test_that("an unknown variant aborts", {
  expect_error(
    whep:::read_glw_density(variant = "Da"),
    class = "rlang_error"
  )
})

# --- the example fixture ----------------------------------------------

test_that("example = TRUE returns the contract without touching disk", {
  withr::local_envvar(WHEP_GLW3_DIR = "")

  out <- whep:::read_glw_density(example = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("lon", "lat", "species_group", "density"))
  expect_gt(nrow(out), 5L)
  expect_true(all(out$density > 0))
  # The replication rule is visible in the fixture itself.
  paired <- out |>
    dplyr::filter(species_group %in% c("cattle_dairy", "cattle_non_dairy")) |>
    dplyr::summarise(n_values = dplyr::n_distinct(density), .by = c(lon, lat))
  expect_true(all(paired$n_values == 1L))
})
