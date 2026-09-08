# Fixture mimicking hwsd_data.csv's real columns: one map unit per group of
# rows, each row one USDA texture class' share and pH within that unit.
.hwsd_attr_fixture <- function() {
  tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    # Unit 1: texture 7 dominates (60%), so its pH (6.5) is picked, not the
    # higher-share-weighted average with texture 9's pH (5.0).
    1L, 7L, 60, 6.5,
    1L, 9L, 40, 5.0,
    # Unit 2: dominant texture's pH is missing -> defaults to 7.0.
    2L, 12L, 80, NA_real_,
    2L, 1L, 20, 8.2,
    # Unit 3: single texture class, single row.
    3L, 3L, 100, 4.9
  )
}

# ---- hwsd_data.csv column contract (whep#596) --------------------------

# hwsd_data.csv is derived locally, so an extract written before a column the
# reader needs existed is an ordinary state. It must be named as such, not
# surface as a dplyr missing-column error deep inside a reader.
.write_hwsd_extract <- function(dir, attr = .hwsd_attr_fixture()) {
  readr::write_csv(attr, file.path(dir, "hwsd_data.csv"))
  dir
}

testthat::test_that(".hwsd_missing_columns reports an absent extract", {
  empty <- withr::local_tempdir()

  testthat::expect_equal(
    whep:::.hwsd_missing_columns(empty, whep:::.hwsd_clay_columns()),
    "hwsd_data.csv"
  )
  testthat::expect_equal(
    whep:::.hwsd_missing_columns("", whep:::.hwsd_clay_columns()),
    "hwsd_data.csv"
  )
})

testthat::test_that(".hwsd_missing_columns reports only absent columns", {
  dir <- .write_hwsd_extract(withr::local_tempdir())

  # The fixture carries the pH reader's columns but no t_clay.
  testthat::expect_equal(
    whep:::.hwsd_missing_columns(dir, whep:::.hwsd_ph_columns()),
    character()
  )
  testthat::expect_equal(
    whep:::.hwsd_missing_columns(dir, whep:::.hwsd_clay_columns()),
    "t_clay"
  )
})

testthat::test_that(".read_hwsd_attributes_local names the missing column", {
  dir <- .write_hwsd_extract(withr::local_tempdir())

  testthat::expect_error(
    whep:::.read_hwsd_attributes_local(
      dir,
      required = whep:::.hwsd_clay_columns()
    ),
    "t_clay"
  )
  # ... and points at the script that writes a complete extract.
  testthat::expect_error(
    whep:::.read_hwsd_attributes_local(dir, required = "t_clay"),
    "export_hwsd_attributes"
  )
})

testthat::test_that(".read_hwsd_attributes_local reads a complete extract", {
  dir <- .write_hwsd_extract(withr::local_tempdir())

  out <- whep:::.read_hwsd_attributes_local(
    dir,
    required = whep:::.hwsd_ph_columns()
  )

  pointblank::expect_col_exists(out, whep:::.hwsd_ph_columns())
  testthat::expect_equal(nrow(out), nrow(.hwsd_attr_fixture()))
})

testthat::test_that("read_soil_hydraulic names a missing texture column", {
  testthat::skip_if_not_installed("terra")
  attr <- .hwsd_attr_fixture() |> dplyr::select(-"t_usda_tex")
  dir <- .write_hwsd_extract(withr::local_tempdir(), attr)

  testthat::expect_error(
    whep::read_soil_hydraulic(hwsd_dir = dir),
    "t_usda_tex"
  )
})

# ---- .derive_dominant_soil() -------------------------------------------

testthat::test_that(".derive_dominant_soil picks dominant texture's pH", {
  result <- whep:::.derive_dominant_soil(.hwsd_attr_fixture())

  pointblank::expect_col_exists(result, c("mu_global", "t_ph_h2o"))
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 1L],
    6.5
  )
  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 3L],
    4.9
  )
})

testthat::test_that(".derive_dominant_soil defaults missing pH to 7.0", {
  result <- whep:::.derive_dominant_soil(.hwsd_attr_fixture())

  testthat::expect_equal(
    result$t_ph_h2o[result$mu_global == 2L],
    7.0
  )
})

testthat::test_that(".derive_dominant_soil drops rows with NA texture", {
  attr <- tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    4L, NA_integer_, 100, 6.0,
    4L, 2L, 50, 5.5
  )
  result <- whep:::.derive_dominant_soil(attr)

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$t_ph_h2o, 5.5)
})

# ---- .derive_dominant_texture() / read_soil_hydraulic() ----------------

testthat::test_that(".derive_dominant_texture picks the largest-share class", {
  result <- whep:::.derive_dominant_texture(.hwsd_attr_fixture())

  pointblank::expect_col_exists(result, c("mu_global", "t_usda_tex"))
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 1L], 7L)
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 2L], 12L)
  testthat::expect_equal(result$t_usda_tex[result$mu_global == 3L], 3L)
})

testthat::test_that("read_soil_hydraulic example fixture is schema-complete", {
  out <- whep::read_soil_hydraulic(example = TRUE)

  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "t_field", "t_wilt", "porosity")
  )
  testthat::expect_true(all(out$t_field > out$t_wilt))
  testthat::expect_true(all(out$porosity > out$t_field))
})

testthat::test_that("read_soil_hydraulic maps dominant texture to hydraulics", {
  testthat::skip_if_not_installed("terra")
  dir <- withr::local_tempdir()
  # Map unit 1 is dominantly texture 13 (sand): sand hydraulics are porosity
  # 0.43, field capacity 0.08, wilting point 0.03.
  attr <- tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    1L, 13L, 100, 6.5
  )
  readr::write_csv(attr, file.path(dir, "hwsd_data.csv"))

  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    resolution = 1 / 6
  )
  terra::values(rast) <- 1L
  terra::writeRaster(
    rast,
    file.path(dir, "hwsd.bil"),
    filetype = "EHdr",
    overwrite = TRUE
  )

  result <- whep::read_soil_hydraulic(hwsd_dir = dir)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "t_field", "t_wilt", "porosity")
  )
  testthat::expect_true(all(abs(result$porosity - 0.43) < 1e-9))
  testthat::expect_true(all(abs(result$t_field - 0.08) < 1e-9))
  testthat::expect_true(all(abs(result$t_wilt - 0.03) < 1e-9))
  testthat::expect_true(all(
    result$t_field > result$t_wilt & result$porosity > result$t_field
  ))
})

# ---- .gapfill_soil() ---------------------------------------------------

testthat::test_that(".gapfill_soil fills a missing cell from a neighbour", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    0.25, -0.25
  )
  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_equal(nrow(result), 2L)
  filled <- result[result$lon == 0.25 & result$lat == -0.25, ]
  testthat::expect_equal(filled$soil_ph, 6.0)
})

testthat::test_that(".gapfill_soil skips cells already present", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25
  )
  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  testthat::expect_equal(nrow(result), 1L)
})

testthat::test_that(".gapfill_soil fills a border coordinate only once", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.0
  )
  # The missing coordinate occurs twice because two polities overlap it.
  country_grid <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    -0.25, -0.25, 1L,
    0.25, -0.25, 1L,
    0.25, -0.25, 2L
  )

  result <- whep:::.gapfill_soil(soil_grid, country_grid)

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(
    sum(result$lon == 0.25 & result$lat == -0.25),
    1L
  )
})

testthat::test_that(".gapfill_soil uses the caller's fallback, not pH 7.0", {
  soil_grid <- tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 0.29
  )
  # A cell with no neighbour within the search window must take the caller's
  # domain-neutral fallback (here the loam field capacity, 0.29), never the
  # pH-domain 7.0.
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    89.75, 89.75
  )
  result <- whep:::.gapfill_soil(
    soil_grid,
    country_grid,
    max_search = 1L,
    fallback = 0.29,
    label = "soil hydraulic"
  )
  far <- result[result$lon == 89.75, ]
  testthat::expect_equal(far$soil_ph, 0.29)
})

testthat::test_that(".gapfill_soil_hydraulic falls back to loam, not pH 7.0", {
  # An isolated target cell far from any aggregated hydraulic data must fall
  # back per-property to the central-texture (loam) reference values, never the
  # pH reader's 7.0 (impossible for a volumetric fraction in (0, 1)).
  loam <- whep::soil_hydraulic_by_texture |>
    dplyr::filter(usda_texture_class == "loam")
  grid <- tibble::tribble(
    ~lon, ~lat, ~t_field, ~t_wilt, ~porosity,
    -0.25, -0.25, 0.21, 0.09, 0.40
  )
  country_grid <- tibble::tribble(
    ~lon, ~lat,
    -0.25, -0.25,
    89.75, 89.75
  )
  result <- whep:::.gapfill_soil_hydraulic(grid, country_grid)
  far <- result[result$lon == 89.75, ]
  testthat::expect_equal(far$t_field, loam$field_capacity)
  testthat::expect_equal(far$t_wilt, loam$wilting_point)
  testthat::expect_equal(far$porosity, loam$porosity)
  # Never the impossible pH-domain fallback.
  testthat::expect_true(all(
    far$t_field < 1 & far$t_wilt < 1 & far$porosity < 1
  ))
})

# ---- read_soil_ph() -----------------------------------------------------

testthat::test_that("read_soil_ph example fixture is schema-complete", {
  out <- whep::read_soil_ph(example = TRUE)

  pointblank::expect_col_exists(out, c("lon", "lat", "soil_ph"))
  pointblank::expect_col_vals_between(out, "soil_ph", 0, 14)
})

testthat::test_that("read_soil_ph aggregates HWSD raster + attributes", {
  testthat::skip_if_not_installed("terra")
  dir <- withr::local_tempdir()
  readr::write_csv(.hwsd_attr_fixture(), file.path(dir, "hwsd_data.csv"))

  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    resolution = 1 / 6
  )
  terra::values(rast) <- 1L
  terra::writeRaster(
    rast,
    file.path(dir, "hwsd.bil"),
    filetype = "EHdr",
    overwrite = TRUE
  )

  result <- whep::read_soil_ph(hwsd_dir = dir)

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_true(all(result$soil_ph == 6.5))
})

testthat::test_that(".hwsd_target_extent pads the target grid's bounding box", {
  testthat::skip_if_not_installed("terra")
  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -3,
    xmax = 3,
    ymin = -3,
    ymax = 3,
    resolution = 0.5
  )
  target <- tibble::tibble(lon = c(-0.25, 0.25), lat = c(-0.25, 0.25))

  ext <- whep:::.hwsd_target_extent(rast, target, target_res = 0.5)

  testthat::expect_equal(unname(ext$xmin), -0.5)
  testthat::expect_equal(unname(ext$xmax), 0.5)
  testthat::expect_equal(unname(ext$ymin), -0.5)
  testthat::expect_equal(unname(ext$ymax), 0.5)
})

testthat::test_that(".hwsd_target_extent falls back to the whole raster", {
  testthat::skip_if_not_installed("terra")
  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -3,
    xmax = 3,
    ymin = -3,
    ymax = 3,
    resolution = 0.5
  )

  ext <- whep:::.hwsd_target_extent(rast, NULL, target_res = 0.5)

  testthat::expect_equal(as.vector(ext), as.vector(terra::ext(rast)))
})

# Banding is only safe because each band is a whole number of target rows: an
# aggregated cell's source pixels then lie inside exactly one band, so no
# aggregated value can straddle a boundary. Tile the extent exactly, with no
# gap and no overlap, or the result changes.
testthat::test_that(".hwsd_band_extents tiles the extent in whole target rows", {
  testthat::skip_if_not_installed("terra")
  res <- 0.5
  extent <- terra::ext(-10, 10, -25, 25)

  bands <- whep:::.hwsd_band_extents(extent, target_res = res)
  tops <- vapply(bands, function(b) b$ymax, numeric(1))
  bottoms <- vapply(bands, function(b) b$ymin, numeric(1))

  # every band spans a whole number of target rows
  rows <- (tops - bottoms) / res
  testthat::expect_equal(rows, round(rows))
  # contiguous, no gaps or overlaps, covering the extent exactly
  testthat::expect_equal(max(tops), unname(extent$ymax))
  testthat::expect_equal(min(bottoms), unname(extent$ymin))
  testthat::expect_equal(bottoms[-length(bottoms)], tops[-1])
  # longitude is never split
  testthat::expect_true(all(
    vapply(bands, function(b) b$xmin, numeric(1)) == extent$xmin
  ))
  testthat::expect_true(all(
    vapply(bands, function(b) b$xmax, numeric(1)) == extent$xmax
  ))
})

testthat::test_that(".hwsd_band_extents handles an extent shorter than one band", {
  testthat::skip_if_not_installed("terra")
  extent <- terra::ext(-10, 10, 0, 1)

  bands <- whep:::.hwsd_band_extents(extent, target_res = 0.5)

  testthat::expect_length(bands, 1L)
  testthat::expect_equal(unname(bands[[1]]$ymin), 0)
  testthat::expect_equal(unname(bands[[1]]$ymax), 1)
})

testthat::test_that("read_soil_ph reads real local HWSD data (smoke)", {
  testthat::skip_if_not_installed("terra")
  .skip_unless_hwsd_columns(whep:::.hwsd_ph_columns())

  # Crop to a small Iberian target grid: classifying the full-resolution
  # global HWSD raster whole exhausts memory and crashes the R session.
  target <- tidyr::expand_grid(
    lon = seq(-9.75, 3.75, by = 0.5),
    lat = seq(36.25, 43.75, by = 0.5)
  )

  result <- whep::read_soil_ph(data = list(cell_polity = target))

  pointblank::expect_col_exists(result, c("lon", "lat", "soil_ph"))
  testthat::expect_gt(nrow(result), 0L)
  testthat::expect_true(all(result$soil_ph >= 3.5 & result$soil_ph <= 10))
})

# ---- observed topsoil carbon benchmark ---------------------------------

# Fixture mimicking the carbon columns of hwsd_data.csv. Unit 1 exercises
# share weighting and the measured/reference bulk-density switch; unit 2 is
# an organic soil, where the two densities diverge most; unit 3 has a
# component with no carbon at all.
.hwsd_soc_fixture <- function() {
  tibble::tribble(
    ~mu_global,
    ~share,
    ~t_oc,
    ~t_bulk_density,
    ~t_ref_bulk_density,
    ~t_gravel,
    ~topsoil_depth_cm,
    1L, 75, 1.0, 1.30, 1.40, 10, 30,
    1L, 25, 2.0, 1.20, 1.40, 0, 30,
    2L, 100, 30.0, 0.25, 1.30, 0, 30,
    3L, 50, 1.5, 1.50, 1.50, 0, 30,
    3L, 50, NA, 1.50, 1.50, 0, 30
  )
}

testthat::test_that(".hwsd_soc_columns tracks the bulk-density method", {
  testthat::expect_true(
    "t_bulk_density" %in% whep:::.hwsd_soc_columns("measured")
  )
  # "reference" never reads t_bulk_density, so requiring it would refuse an
  # extract that is perfectly adequate for that method.
  testthat::expect_false(
    "t_bulk_density" %in% whep:::.hwsd_soc_columns("reference")
  )
  testthat::expect_true(
    all(
      c("mu_global", "share", "t_oc", "t_gravel") %in%
        whep:::.hwsd_soc_columns("reference")
    )
  )
})

testthat::test_that(".derive_map_unit_soc computes the documented stock", {
  soc <- whep:::.derive_map_unit_soc(.hwsd_soc_fixture(), "measured")

  # t_oc * bulk * 30 * (1 - gravel), share-weighted within the map unit. The
  # fixture holds two components: three quarters at 1.0 percent carbon, bulk
  # 1.30 and a tenth gravel, one quarter at 2.0 percent, bulk 1.20 and no
  # gravel.
  expected <- 0.75 * (1.0 * 1.30 * 30 * 0.9) + 0.25 * (2.0 * 1.20 * 30)
  testthat::expect_equal(
    soc$soc_obs_mgc_ha[soc$mu_global == 1L],
    expected
  )
})

testthat::test_that("the bulk-density method changes an organic soil most", {
  measured <- whep:::.derive_map_unit_soc(.hwsd_soc_fixture(), "measured")
  reference <- whep:::.derive_map_unit_soc(.hwsd_soc_fixture(), "reference")
  peat <- \(x) x$soc_obs_mgc_ha[x$mu_global == 2L]

  testthat::expect_equal(peat(measured), 30 * 0.25 * 30)
  testthat::expect_equal(peat(reference), 30 * 1.30 * 30)
  # This ratio is the whole reason "measured" is the default: a texture-
  # derived density knows nothing about organic matter.
  testthat::expect_gt(peat(reference) / peat(measured), 5)
})

testthat::test_that(".derive_map_unit_soc drops components with no carbon", {
  soc <- whep:::.derive_map_unit_soc(.hwsd_soc_fixture(), "measured")

  # Unit 3's second component reports no t_oc. It must not be counted as a
  # zero-carbon soil, which would halve the unit's stock.
  testthat::expect_equal(
    soc$soc_obs_mgc_ha[soc$mu_global == 3L],
    1.5 * 1.50 * 30
  )
})

testthat::test_that(".derive_map_unit_soc falls back on reference density", {
  attr <- .hwsd_soc_fixture()
  attr$t_bulk_density[attr$mu_global == 2L] <- NA_real_

  soc <- whep:::.derive_map_unit_soc(attr, "measured")
  testthat::expect_equal(
    soc$soc_obs_mgc_ha[soc$mu_global == 2L],
    30 * 1.30 * 30
  )
})

testthat::test_that("missing gravel is treated as stone-free, not dropped", {
  attr <- .hwsd_soc_fixture()
  attr$t_gravel <- NA_real_

  soc <- whep:::.derive_map_unit_soc(attr, "measured")
  testthat::expect_equal(
    soc$soc_obs_mgc_ha[soc$mu_global == 3L],
    1.5 * 1.50 * 30
  )
})

testthat::test_that("read_hwsd_topsoil_soc names a stale extract", {
  # The pH-era fixture carries none of the carbon columns.
  dir <- .write_hwsd_extract(withr::local_tempdir())

  testthat::expect_error(
    whep:::.read_hwsd_attributes_local(
      dir,
      required = whep:::.hwsd_soc_columns("measured")
    ),
    "t_oc"
  )
})

testthat::test_that("read_hwsd_topsoil_soc rejects an unknown method", {
  testthat::expect_error(
    whep::read_hwsd_topsoil_soc(bulk_density = "guessed"),
    class = "rlang_error"
  )
})

testthat::test_that("read_hwsd_topsoil_soc example is self-contained", {
  out <- whep::read_hwsd_topsoil_soc(example = TRUE)

  testthat::skip_if_not_installed("pointblank")
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "soc_obs_mgc_ha", "method_soc_obs")
  )
  pointblank::expect_col_vals_not_null(out, columns = "soc_obs_mgc_ha")
  # A topsoil carbon stock is a positive density; the fixture must stay a
  # plausible mineral soil so it reads as a real value, not a placeholder.
  pointblank::expect_col_vals_between(
    out,
    columns = "soc_obs_mgc_ha",
    left = 1,
    right = 600
  )
})

# ---- the topsoil depth comes from the artifact, not from a constant ----

testthat::test_that("the SOC reader requires the depth stamp", {
  # Two scripts write hwsd_data.csv: download_hwsd.R from HWSD2 (D1 =
  # 0-20 cm) and export_hwsd_attributes.R from HWSD v1.2 (0-30 cm). Nothing
  # else distinguishes them, so a reader that multiplies by a hardcoded 30
  # would overstate an HWSD2 extract by half (whep#851).
  testthat::expect_true(
    "topsoil_depth_cm" %in% whep:::.hwsd_soc_columns("measured")
  )
  testthat::expect_true(
    "topsoil_depth_cm" %in% whep:::.hwsd_soc_columns("reference")
  )
})

testthat::test_that("the stock scales with the stamped depth", {
  base <- tibble::tribble(
    ~mu_global, ~share, ~t_oc, ~t_bulk_density, ~t_ref_bulk_density,
    ~t_gravel, ~topsoil_depth_cm,
    1L, 100, 1.5, 1.4, 1.4, 0, 30
  )
  shallow <- dplyr::mutate(base, topsoil_depth_cm = 20)

  deep_soc <- whep:::.derive_map_unit_soc(base, "measured")$soc_obs_mgc_ha
  shallow_soc <- whep:::.derive_map_unit_soc(
    shallow,
    "measured"
  )$soc_obs_mgc_ha

  testthat::expect_equal(deep_soc, 1.5 * 1.4 * 30)
  testthat::expect_equal(shallow_soc, 1.5 * 1.4 * 20)
  # Exactly two thirds - the depth is genuinely load-bearing, not decorative.
  testthat::expect_equal(shallow_soc / deep_soc, 2 / 3)
})

testthat::test_that("an extract with no depth stamp is named, not guessed", {
  dir <- withr::local_tempdir()
  attr <- tibble::tribble(
    ~mu_global, ~share, ~t_oc, ~t_bulk_density, ~t_ref_bulk_density, ~t_gravel,
    1L, 100, 1.5, 1.4, 1.4, 0
  )
  readr::write_csv(attr, file.path(dir, "hwsd_data.csv"))

  testthat::expect_error(
    whep:::.read_hwsd_attributes_local(
      dir,
      required = whep:::.hwsd_soc_columns("measured")
    ),
    "topsoil_depth_cm"
  )
})

# -- Derived HWSD grid cache ---------------------------------------------------

# The derived grids are a pure function of the archive and the target grid, but
# were recomputed from an ~11 GB raster on every build: `.socd_soil_hydraulic()`
# alone measured over 35 minutes. Caching is only safe if the key covers
# everything the answer depends on, so that is what these pin.

# A REAL pair of files, because the key fingerprints them by size and mtime.
# The previous version of this helper defaulted to a non-existent `tempfile()`,
# so its "separates every input" test compared two paths that BOTH failed to
# stat and hashed identically -- it would have passed no matter what the key
# contained.
.fake_hwsd <- function(bil_bytes = 10L, hdr_text = "NROWS 4") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  bil <- file.path(dir, "hwsd.bil")
  writeBin(as.raw(seq_len(bil_bytes)), bil)
  writeLines(hdr_text, file.path(dir, "hwsd.hdr"))
  bil
}

.cache_key_args <- function(...) {
  defaults <- list(
    hwsd_path = .fake_hwsd(),
    target_res = 0.5,
    cols = c(t_field = "t_field"),
    target_grid = tibble::tibble(lon = c(0.25, 0.75), lat = c(1.25, 1.75)),
    mu_soils = tibble::tibble(mu_global = 1:2, t_field = c(0.3, 0.4))
  )
  do.call(whep:::.hwsd_cache_key, utils::modifyList(defaults, list(...)))
}

testthat::test_that(".hwsd_cache_key separates every input it depends on", {
  path <- .fake_hwsd()
  base <- .cache_key_args(hwsd_path = path)

  testthat::expect_equal(.cache_key_args(hwsd_path = path), base)
  testthat::expect_false(
    .cache_key_args(hwsd_path = path, target_res = 0.25) == base
  )
  testthat::expect_false(
    .cache_key_args(hwsd_path = path, cols = c(t_wilt = "t_wilt")) == base
  )
  testthat::expect_false(
    .cache_key_args(
      hwsd_path = path,
      target_grid = tibble::tibble(lon = 9.25, lat = 9.75)
    ) ==
      base
  )
  # mu_soils carries the DERIVED attribute values, so a change in how they are
  # derived must invalidate every grid built from them.
  testthat::expect_false(
    .cache_key_args(
      hwsd_path = path,
      mu_soils = tibble::tibble(mu_global = 1:2, t_field = c(0.3, 0.9))
    ) ==
      base
  )
})

testthat::test_that(".hwsd_cache_key separates two different archives", {
  # The dependency that matters most, and the one the old test could not see.
  a <- .fake_hwsd(bil_bytes = 10L)
  b <- .fake_hwsd(bil_bytes = 11L)

  testthat::expect_false(
    .cache_key_args(hwsd_path = a) == .cache_key_args(hwsd_path = b)
  )
})

testthat::test_that(".hwsd_cache_key covers the .hdr sidecar", {
  # terra reads the grid geometry -- rows, columns, corners, byte order --
  # from the header. A corrected header with an untouched .bil changes every
  # aggregated cell, so it has to be part of the fingerprint.
  path <- .fake_hwsd(hdr_text = "NROWS 4")
  before <- .cache_key_args(hwsd_path = path)
  writeLines("NROWS 8", sub("bil$", "hdr", path))

  testthat::expect_false(.cache_key_args(hwsd_path = path) == before)
})

testthat::test_that(".hwsd_cache_key refuses an unstattable archive", {
  # file.info() returns NA size and mtime for a path it cannot stat, and those
  # hash happily -- so every such directory would share one key and be served
  # each other's grids.
  testthat::expect_error(
    .cache_key_args(hwsd_path = file.path(tempfile(), "hwsd.bil")),
    "Cannot fingerprint"
  )
})

testthat::test_that(".hwsd_cache_key changes with the algorithm version", {
  # An algorithm change leaves every user's archive untouched, so without this
  # they would keep being served grids computed by the old code.
  path <- .fake_hwsd()
  before <- .cache_key_args(hwsd_path = path)
  testthat::local_mocked_bindings(
    .hwsd_cache_algo_version = function() "different",
    .package = "whep"
  )

  testthat::expect_false(.cache_key_args(hwsd_path = path) == before)
})

testthat::test_that(".hwsd_cache_write and read round-trip a grid", {
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .hwsd_cache_dir = function() dir,
    .package = "whep"
  )
  grid <- tibble::tibble(lon = c(0.25, 0.75), lat = 1.25, t_field = c(0.3, 0.4))

  testthat::expect_null(whep:::.hwsd_cache_read("nothing-here"))
  whep:::.hwsd_cache_write("abc", grid)

  testthat::expect_equal(whep:::.hwsd_cache_read("abc"), grid)
})

testthat::test_that(".hwsd_cache_read treats a corrupt entry as a miss", {
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .hwsd_cache_dir = function() dir,
    .package = "whep"
  )
  writeLines("not a parquet", file.path(dir, "abc.parquet"))

  # The grid is always recomputable, so a damaged cache must never fail a
  # build -- it must simply miss.
  testthat::expect_null(whep:::.hwsd_cache_read("abc"))
})

testthat::test_that(".hwsd_cache_write warns rather than aborting", {
  testthat::local_mocked_bindings(
    .hwsd_cache_dir = function() file.path(tempfile(), "no", "such", "tree"),
    .package = "whep"
  )
  testthat::local_mocked_bindings(
    write_parquet = function(...) stop("disk full"),
    .package = "nanoparquet"
  )
  grid <- tibble::tibble(lon = 0.25, lat = 1.25, t_field = 0.3)

  testthat::expect_warning(
    whep:::.hwsd_cache_write("abc", grid),
    "Could not cache"
  )
})

testthat::test_that(".hwsd_cache_dir honours the test override", {
  withr::local_envvar(WHEP_HWSD_CACHE_DIR = "")
  testthat::expect_match(whep:::.hwsd_cache_dir(), "hwsd$")

  withr::local_envvar(WHEP_HWSD_CACHE_DIR = "some/where")
  testthat::expect_equal(whep:::.hwsd_cache_dir(), "some/where")
})

testthat::test_that(".hwsd_cache_read treats a zero-row entry as a miss", {
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .hwsd_cache_dir = function() dir,
    .package = "whep"
  )
  empty <- tibble::tibble(
    lon = numeric(0),
    lat = numeric(0),
    t_field = numeric(0)
  )
  nanoparquet::write_parquet(empty, file.path(dir, "abc.parquet"))

  # An empty grid is indistinguishable downstream from soil that happens to be
  # constant everywhere: `.gapfill_soil()` fills a cell with no neighbour from
  # a constant, so zero cells becomes plausible loam covering the world.
  testthat::expect_null(whep:::.hwsd_cache_read("abc"))
})

# ---- texture class counts ------------------------------------------------

# The published artifact carries per-class PIXEL COUNTS, not the hydraulic
# values, because the values are counts multiplied by
# `whep::soil_hydraulic_by_texture` -- a package coefficient table that must
# stay revisable in code rather than frozen inside a data artifact. These pin
# that the two routes agree, so that choice costs nothing.

.two_texture_hwsd <- function(dir) {
  # Two map units with DIFFERENT dominant textures, so a cell mixing them has
  # a mean that no single class produces -- which a counts grid must still
  # reproduce exactly. 13 = sand, 1 = clay.
  attr <- tibble::tribble(
    ~mu_global, ~t_usda_tex, ~share, ~t_ph_h2o,
    1L, 13L, 100, 6.5,
    2L, 1L, 100, 6.5
  )
  readr::write_csv(attr, file.path(dir, "hwsd_data.csv"))
  rast <- terra::rast(
    nrows = 12,
    ncols = 12,
    xmin = -1,
    xmax = 1,
    ymin = -1,
    ymax = 1,
    resolution = 1 / 6
  )
  # A deterministic mix of the two units within every aggregated cell.
  terra::values(rast) <- rep(c(1L, 2L), length.out = terra::ncell(rast))
  terra::writeRaster(
    rast,
    file.path(dir, "hwsd.bil"),
    filetype = "EHdr",
    overwrite = TRUE
  )
  dir
}

testthat::test_that("class counts reproduce the hydraulic means exactly", {
  testthat::skip_if_not_installed("terra")
  dir <- .two_texture_hwsd(withr::local_tempdir())
  attr_tbl <- whep:::.read_hwsd_attributes_local(
    dir,
    required = whep:::.hwsd_texture_columns()
  )

  by_value <- whep:::.aggregate_hwsd_hydraulic(
    dir,
    whep:::.derive_map_unit_hydraulic(attr_tbl),
    target_grid = NULL
  )

  mu_class <- attr_tbl |>
    whep:::.derive_dominant_texture() |>
    dplyr::inner_join(whep::hwsd_texture_usda, by = "t_usda_tex") |>
    dplyr::select("mu_global", "usda_texture_class") |>
    dplyr::distinct()
  by_counts <- whep:::.aggregate_hwsd_classes(dir, mu_class) |>
    whep:::.hydraulic_from_class_counts()

  joined <- dplyr::inner_join(
    by_value,
    by_counts,
    by = c("lon", "lat"),
    suffix = c("_v", "_c")
  )
  testthat::expect_equal(nrow(joined), nrow(by_value))
  for (col in c("t_field", "t_wilt", "porosity")) {
    testthat::expect_equal(
      joined[[paste0(col, "_v")]],
      joined[[paste0(col, "_c")]]
    )
  }
  # The mixture is real, not a single class dressed up as one.
  testthat::expect_gt(
    max(joined$t_field_v) - min(whep::soil_hydraulic_by_texture$field_capacity),
    0
  )
})

testthat::test_that("class counts are pure HWSD, free of the coefficients", {
  testthat::skip_if_not_installed("terra")
  dir <- .two_texture_hwsd(withr::local_tempdir())
  mu_class <- whep:::.read_hwsd_attributes_local(
    dir,
    required = whep:::.hwsd_texture_columns()
  ) |>
    whep:::.derive_dominant_texture() |>
    dplyr::inner_join(whep::hwsd_texture_usda, by = "t_usda_tex") |>
    dplyr::select("mu_global", "usda_texture_class") |>
    dplyr::distinct()

  counts <- whep:::.aggregate_hwsd_classes(dir, mu_class)

  # Every class gets a column, in a fixed order, so a grid means the same
  # thing wherever it was produced.
  testthat::expect_setequal(
    setdiff(names(counts), c("lon", "lat")),
    paste0("n_", whep:::.hwsd_texture_classes())
  )
  # Counts are pixel tallies: integral, non-negative, and summing to the
  # pixels per aggregated cell (a 12x12 raster at 1/6 degree into 0.5-degree
  # cells is 9 pixels each).
  m <- as.matrix(counts[, paste0("n_", whep:::.hwsd_texture_classes())])
  m[is.na(m)] <- 0
  testthat::expect_true(all(m >= 0))
  # A 12x12 raster at 1/6 degree aggregates 3x3 native pixels into each
  # 0.5-degree cell, so 9. It was 4 until `.hwsd_agg_factor()` stopped
  # truncating. The fixture is written to disk and read back deliberately: in
  # memory `0.5 / (1/6)` is exactly 3 and nothing truncates, but the EHdr
  # header stores the decimal `0.166666666666667`, and reading THAT back gives
  # `0.5 / res` = 2.99999999999999422, which `as.integer()` floored to 2.
  # Asserting against an in-memory raster would pass on the buggy code.
  testthat::expect_equal(unique(rowSums(m)), 9)
})

testthat::test_that("a revised coefficient table moves the reconstruction", {
  # This is the whole reason counts are pinned rather than values: a later
  # revision of soil_hydraulic_by_texture must reach users who read the pin,
  # not only those who derive locally.
  counts <- tibble::tibble(lon = 0.25, lat = 0.25)
  for (cl in whep:::.hwsd_texture_classes()) {
    counts[[paste0("n_", cl)]] <- if (cl == "sand") 10 else 0
  }

  base <- whep:::.hydraulic_from_class_counts(counts)

  bumped <- whep::soil_hydraulic_by_texture
  bumped$field_capacity[bumped$usda_texture_class == "sand"] <- 0.5

  testthat::expect_equal(
    whep:::.hydraulic_from_class_counts(counts, coef = bumped)$t_field,
    0.5
  )
  testthat::expect_false(base$t_field == 0.5)
})

# ---- pin versus local resolution ------------------------------------------

# The pinned grid exists so that nobody re-aggregates an 11 GB archive and,
# more importantly, so that everyone reads ONE vintage of it. That is only
# safe if the two routes are indistinguishable, so these pin both the
# resolution ORDER and the agreement.

testthat::test_that("read_soil_hydraulic reads the pin by default", {
  testthat::skip_if_not_installed("terra")
  withr::local_envvar(WHEP_HWSD_DIR = "")
  seen <- NULL
  counts <- tibble::tibble(lon = 0.25, lat = 0.25)
  for (cl in whep:::.hwsd_texture_classes()) {
    counts[[paste0("n_", cl)]] <- if (cl == "sand") 4 else 0
  }
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      seen <<- file_alias
      counts
    },
    .package = "whep"
  )

  out <- whep::read_soil_hydraulic()

  testthat::expect_equal(seen, "hwsd-texture-class-grid")
  testthat::expect_equal(out$t_field, 0.08)
})

testthat::test_that("source = 'local' never reaches the pin", {
  testthat::skip_if_not_installed("terra")
  dir <- .two_texture_hwsd(withr::local_tempdir())
  testthat::local_mocked_bindings(
    whep_read_file = function(...) stop("the pin must not be read here"),
    .package = "whep"
  )

  testthat::expect_no_error(
    whep::read_soil_hydraulic(hwsd_dir = dir, source = "local")
  )
  # An explicit directory alone is enough: naming an archive IS the request.
  testthat::expect_no_error(whep::read_soil_hydraulic(hwsd_dir = dir))
})

testthat::test_that("naming an archive and demanding the pin is refused", {
  testthat::expect_error(
    whep::read_soil_hydraulic(hwsd_dir = "somewhere", source = "pin"),
    "cannot be combined"
  )
})

testthat::test_that("the pin and the local archive agree exactly", {
  testthat::skip_if_not_installed("terra")
  dir <- .two_texture_hwsd(withr::local_tempdir())
  cell_polity <- tibble::tibble(
    lon = c(-0.75, -0.25, 0.25, 0.75),
    lat = c(0.25, 0.25, 0.25, 0.25),
    area_code = 1L
  )

  local <- whep::read_soil_hydraulic(
    hwsd_dir = dir,
    data = list(cell_polity = cell_polity),
    source = "local"
  )

  # Feed the locally derived counts back as the pin: identical inputs must
  # give an identical answer all the way through gap-filling, which is where
  # a route difference would otherwise hide behind a constant fallback.
  pinned_counts <- whep:::.derive_hwsd_texture_counts(dir, NULL)
  testthat::local_mocked_bindings(
    whep_read_file = function(...) pinned_counts,
    .package = "whep"
  )
  pinned <- whep::read_soil_hydraulic(
    data = list(cell_polity = cell_polity),
    source = "pin"
  )

  testthat::expect_equal(pinned, local)
})

testthat::test_that("a pinned grid off the half-degree centres is refused", {
  bad <- tibble::tibble(lon = 0.3, lat = 0.25, clay_pct = 20)

  testthat::expect_error(
    whep:::.check_hwsd_grid(bad, "clay_pct", "test grid"),
    "not on"
  )
})

testthat::test_that(".hwsd_crop_to_target keeps the padded bounding box", {
  grid <- tidyr::expand_grid(
    lon = seq(-1.75, 1.75, by = 0.5),
    lat = seq(-1.75, 1.75, by = 0.5)
  ) |>
    dplyr::mutate(clay_pct = 20)
  target <- tibble::tibble(lon = c(0.25, 0.75), lat = c(0.25, 0.75))

  out <- whep:::.hwsd_crop_to_target(grid, target)

  # The local route crops to the target's bounding box padded by half a cell
  # and returns EVERY HWSD cell inside it, so the pin route must too. Cropping
  # to target-cell membership instead would shrink `.gapfill_soil()`'s
  # neighbour pool, and where that pool empties it substitutes a constant --
  # so the two routes would differ only at the edges and look plausible both
  # ways.
  testthat::expect_setequal(unique(out$lon), c(0.25, 0.75))
  testthat::expect_setequal(unique(out$lat), c(0.25, 0.75))
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_equal(whep:::.hwsd_crop_to_target(grid, NULL), grid)
})

testthat::test_that("the reconstruction keeps the 0.01 quantisation", {
  counts <- tibble::tibble(lon = 0.25, lat = 0.25)
  for (cl in whep:::.hwsd_texture_classes()) {
    counts[[paste0("n_", cl)]] <- if (cl %in% c("sand", "clay")) 3 else 0
  }

  out <- whep:::.hydraulic_from_class_counts(counts)

  # The previous route aggregated the values and rounded to 2 dp, so the
  # quantisation is preserved rather than silently improved: dropping it would
  # move every cell by up to 0.005, where keeping it moves only the ~0.05% of
  # cells whose unrounded value sits exactly on a .xx5 rounding boundary.
  for (col in c("t_field", "t_wilt", "porosity")) {
    testthat::expect_equal(out[[col]], round(out[[col]], 2))
  }
})
