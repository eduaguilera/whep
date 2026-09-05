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
#
# `values` and `extent` are arguments so that a variant of the raster is
# built in memory and written once. Reading a GeoTIFF back, editing its
# values and writing over the path while the `SpatRaster` still holds the
# file open is a Windows/GDAL hazard: the write can fail or truncate, and
# it did so once in eighteen runs of this file.
glw_write_test_raster <- function(
  dir,
  code = "Ct",
  variant = "Da",
  values = seq_len(144),
  extent = c(10, 11, 40, 41)
) {
  raster <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = extent[[1]],
    xmax = extent[[2]],
    ymin = extent[[3]],
    ymax = extent[[4]],
    crs = "EPSG:4326"
  )
  terra::values(raster) <- values
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

test_that("poultry takes the duck layer, never the chicken one", {
  # WHEP's `poultry` group is ducks + geese/guinea fowls + turkeys
  # (inst/extdata/livestock_mapping.csv); chickens have their own two
  # groups. Feeding chickens into `poultry` as well replaced a duck-delta
  # geography with a chicken-dominated one (whep#1000, task T15a-iii).
  crosswalk <- whep:::.read_glw_crosswalk()

  poultry <- dplyr::filter(crosswalk, species_group == "poultry")
  expect_setequal(poultry$glw_species, "ducks")
  chickens <- dplyr::filter(crosswalk, glw_species == "chickens")
  expect_setequal(
    chickens$species_group,
    c("chickens_broilers", "chickens_layers")
  )
})

test_that("a chicken-dominated cell does not capture poultry mass", {
  counts <- tibble::tribble(
    ~lon,   ~lat, ~heads, ~glw_species,
    10.25, 40.25,    500, "ducks",
    10.75, 40.25,  90000, "chickens"
  )

  out <- whep:::.apply_glw_crosswalk(
    counts,
    whep:::.read_glw_crosswalk()
  )

  poultry <- dplyr::filter(out, species_group == "poultry")
  expect_equal(poultry$lon, 10.25)
  expect_equal(poultry$density, 500)
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

# --- alignment to WHEP's grid -----------------------------------------

test_that("a raster offset from WHEP's grid aborts naming the offset", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  # Same 1/12-degree pixels, shifted 0.1 degrees east and north. The
  # resolution check passes; the blocks would aggregate onto centres
  # 10.35/10.85 and 40.35/40.85, off WHEP's 0.5-degree centres, and the
  # engine's inner join on (lon, lat) would then drop the whole species.
  glw_write_test_raster(dir, extent = c(10.1, 11.1, 40.1, 41.1))

  expect_error(
    whep:::read_glw_density(
      species = "cattle",
      glw_dir = dir,
      crosswalk = glw_test_crosswalk()
    ),
    "0\\.1"
  )
})

test_that("the alignment check names every edge that is off the grid", {
  skip_if_not_installed("terra")
  shifted <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 10.1,
    xmax = 11.1,
    ymin = 40,
    ymax = 41,
    crs = "EPSG:4326"
  )

  err <- expect_error(whep:::.check_glw_alignment(shifted))
  message <- conditionMessage(err)
  expect_match(message, "xmin")
  expect_match(message, "xmax")
  # The y edges are on the grid, so they must not be reported.
  expect_false(grepl("ymin", message, fixed = TRUE))
})

test_that("an extent that is not whole WHEP cells aborts", {
  skip_if_not_installed("terra")
  # Nine rows of 1/12 degree: the extent is 0.75 degrees tall, so the
  # bottom block would be half a cell and its truncated sum would travel
  # as a full cell's head count.
  partial <- terra::rast(
    nrows = 9,
    ncols = 12,
    xmin = 10,
    xmax = 11,
    ymin = 40,
    ymax = 40.75,
    crs = "EPSG:4326"
  )

  expect_error(whep:::.check_glw_alignment(partial), "ymax")
})

test_that("a raster on WHEP's grid passes the alignment check", {
  skip_if_not_installed("terra")
  aligned <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = 10,
    xmax = 11,
    ymin = 40,
    ymax = 41,
    crs = "EPSG:4326"
  )

  expect_null(whep:::.check_glw_alignment(aligned))
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
  # `read_glw_density()` demands terra before it looks for any file, so
  # without terra installed this reaches the terra abort and never the
  # path one. Guarded so the test skips there instead of failing.
  skip_if_not_installed("terra")
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

test_that("terra is demanded before any raster path is searched", {
  # This ordering is why the test above has to be guarded: on a machine
  # without terra the terra abort wins and the path is never looked for.
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .require_terra_for_glw = function() {
      cli::cli_abort("terra sentinel")
    }
  )

  expect_error(
    whep:::read_glw_density(
      species = "cattle",
      glw_dir = dir,
      crosswalk = glw_test_crosswalk()
    ),
    "terra sentinel"
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

  expect_named(
    out,
    c("lon", "lat", "species_group", "density", "glw_variant")
  )
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
  values <- seq_len(144)
  # Blank one pixel of the north-west block and the whole north-east one.
  values[1L] <- NA
  values[c(7:12, 19:24, 31:36, 43:48, 55:60, 67:72)] <- NA
  glw_write_test_raster(dir, values = values)

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

# --- which GLW3 product a run allocated on ----------------------------
#
# The `glw_variant` column, the `method_livestock_proxy` label it becomes
# and the `livestock_glw_variant` run override are one mechanism, so they
# are tested together here rather than split across three files. The
# helpers they exercise live in `R/spatialize_livestock.R` and
# `R/run_spatialize.R`.

test_that("the returned table records which product was read", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  glw_write_test_raster(dir, variant = "Da")

  out <- whep:::read_glw_density(
    species = "cattle",
    glw_dir = dir,
    crosswalk = glw_test_crosswalk()
  )

  expect_true("glw_variant" %in% names(out))
  expect_setequal(out$glw_variant, "DA")
})

test_that("the areal-weighted read records itself, not the default", {
  skip_if_not_installed("terra")
  withr::local_envvar(WHEP_GLW3_DIR = "")
  dir <- withr::local_tempdir()
  # `.glw_file_name()` builds `6_Ct_2010_Aw.tif` for the AW variant.
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
  terra::writeRaster(raster, file.path(dir, "6_Ct_2010_Aw.tif"))

  out <- whep:::read_glw_density(
    species = "cattle",
    variant = "AW",
    glw_dir = dir,
    crosswalk = glw_test_crosswalk()
  )

  expect_setequal(out$glw_variant, "AW")
})

test_that("the proxy method column names the product, not just glw3", {
  density <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    species_group = "pigs",
    density = 1,
    glw_variant = "DA"
  )

  expect_identical(
    whep:::.livestock_proxy_method("glw3", "cropland", NULL, density),
    "glw3_da"
  )
  expect_identical(
    whep:::.livestock_proxy_method(
      "glw3",
      "cropland",
      NULL,
      dplyr::mutate(density, glw_variant = "AW")
    ),
    "glw3_aw"
  )
  # A hand-built table records no product, so the label says only what is
  # known about it.
  expect_identical(
    whep:::.livestock_proxy_method(
      "glw3",
      "cropland",
      NULL,
      dplyr::select(density, -glw_variant)
    ),
    "glw3"
  )
  # The LUH2 labels are untouched.
  expect_identical(
    whep:::.livestock_proxy_method("luh2", "pasture", NULL, NULL),
    "luh2_area"
  )
})

test_that("a density table mixing the two products is refused", {
  mixed <- tibble::tibble(
    lon = c(0.25, 0.75),
    lat = c(0.25, 0.25),
    species_group = "pigs",
    density = 1,
    glw_variant = c("DA", "AW")
  )

  expect_error(
    whep:::.check_livestock_proxy_inputs("glw3", mixed, "pigs"),
    "glw_variant"
  )
})

test_that("livestock_glw_variant is a run override defaulting to DA", {
  expect_true(
    "livestock_glw_variant" %in% whep:::.known_override_keys()
  )
  presets <- whep:::.spatialize_presets()
  expect_identical(presets$whep$livestock_glw_variant, "DA")
  expect_identical(presets$lpjml$livestock_glw_variant, "DA")
  # An unset key still resolves to the dasymetric product.
  expect_identical(whep:::.check_glw_run_variant(NULL), "DA")
})

test_that("an unknown livestock_glw_variant aborts naming the key", {
  expect_error(
    whep:::.check_glw_run_variant("Da"),
    "livestock_glw_variant"
  )
})

test_that("a livestock_glw_variant override reaches the resolved config", {
  # `run_spatialize()` writes the resolved config into
  # `run_metadata.yaml`, so a run that used the areal-weighted product
  # says so there as well as in `method_livestock_proxy`.
  config <- whep:::.resolve_spatialize_config(
    "whep",
    list(livestock_glw_variant = "AW")
  ) |>
    whep:::.validate_level_config("livestock")

  expect_identical(config$livestock_glw_variant, "AW")
  expect_error(
    whep:::.resolve_spatialize_config(
      "whep",
      list(livestock_glw_variant = "Aw")
    ) |>
      whep:::.validate_level_config("livestock"),
    "livestock_glw_variant"
  )
})

# --- the example fixture ----------------------------------------------

test_that("example = TRUE returns the contract without touching disk", {
  withr::local_envvar(WHEP_GLW3_DIR = "")

  out <- whep:::read_glw_density(example = TRUE)

  expect_s3_class(out, "tbl_df")
  expect_named(
    out,
    c("lon", "lat", "species_group", "density", "glw_variant")
  )
  expect_gt(nrow(out), 5L)
  expect_true(all(out$density > 0))
  expect_setequal(out$glw_variant, "DA")
})

test_that("the fixture obeys the crosswalk rules it illustrates", {
  # A fixture that breaks the rule it stands for teaches the wrong
  # contract. Every group a single GLW species feeds must carry the same
  # value in the same cells, so the layer/broiler pair and the dairy pair
  # are each cell-for-cell identical (whep#1000, wave-7 review finding 10).
  withr::local_envvar(WHEP_GLW3_DIR = "")

  out <- whep:::read_glw_density(example = TRUE)

  replicated <- list(
    c("cattle_dairy", "cattle_non_dairy"),
    c("chickens_layers", "chickens_broilers")
  )
  for (pair in replicated) {
    cells <- out |>
      dplyr::filter(species_group %in% pair) |>
      dplyr::summarise(
        n_groups = dplyr::n_distinct(species_group),
        n_values = dplyr::n_distinct(density),
        .by = c(lon, lat)
      )
    # Both members of the pair in every cell either of them reaches ...
    expect_true(all(cells$n_groups == 2L))
    # ... carrying the one value their shared GLW layer holds there.
    expect_true(all(cells$n_values == 1L))
  }
})
