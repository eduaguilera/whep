# Writes a tiny 3x2-cell ESRI ASCII grid (6-line header + matrix) at the
# nested archive path read_critical_n() expects for the default
# threshold "mi" and land_use "all", so the real parser is exercised
# without the off-repo Zenodo archive.
.critical_n_write_asc <- function(dir) {
  target <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Output_files",
    "Critical N surpluses"
  )
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "ncols 3",
      "nrows 2",
      "xllcorner 0",
      "yllcorner 0",
      "cellsize 0.5",
      "NODATA_value -9999",
      "10 20 -9999",
      "40 50 60"
    ),
    file.path(target, "nsur_crit_mi_all_ph.asc")
  )
  input <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Input_files"
  )
  dir.create(input, recursive = TRUE, showWarnings = FALSE)
  header <- c(
    "ncols 3",
    "nrows 2",
    "xllcorner 0",
    "yllcorner 0",
    "cellsize 0.5",
    "NODATA_value -9999"
  )
  writeLines(
    c(header, "100 200 300", "400 500 600"),
    file.path(input, "a_crop.asc")
  )
  writeLines(
    c(header, "10 20 30", "40 50 60"),
    file.path(input, "a_gr_int.asc")
  )
  writeLines(
    c(header, "1 2 3", "4 5 6"),
    file.path(input, "image_region28.asc")
  )
  invisible(dir)
}

testthat::test_that("read_critical_n parses an ESRI grid at cell centres", {
  tmp <- withr::local_tempdir()
  .critical_n_write_asc(tmp)
  out <- whep::read_critical_n("critical_n_surplus", dir = tmp)
  testthat::expect_named(
    out,
    c(
      "lon",
      "lat",
      "value",
      "critical_var",
      "critical_threshold",
      "critical_land_use",
      "critical_year",
      "critical_source",
      "cell_id",
      "source_row",
      "source_col",
      "source_area_ha",
      "image_region",
      "critical_source_doi",
      "critical_source_version",
      "archive_md5"
    )
  )
  testthat::expect_s3_class(out, "tbl_df")
  # 6 cells minus the one NODATA cell.
  testthat::expect_equal(nrow(out), 5L)
  # Row 1 (north) has lat 0.75 with the two non-NODATA values.
  north <- out[out$lat == 0.75, ]
  testthat::expect_equal(sort(north$value), c(10, 20))
  # Cell centre of the north-west cell: lon 0.25, lat 0.75.
  first <- out[out$lon == 0.25 & out$lat == 0.75, ]
  testthat::expect_equal(first$value, 10)
  # South row lat 0.25 keeps all three values.
  testthat::expect_equal(sort(out$value[out$lat == 0.25]), c(40, 50, 60))
  # NODATA (-9999) cells are dropped.
  testthat::expect_false(any(out$value == -9999))
})

testthat::test_that("critical layers carry source land, IMAGE, and integer keys", {
  tmp <- withr::local_tempdir()
  .critical_n_write_asc(tmp)
  out <- whep::read_critical_n("critical_n_surplus", dir = tmp)
  pointblank::expect_col_exists(
    out,
    c("cell_id", "source_row", "source_col", "source_area_ha", "image_region")
  )
  first <- dplyr::filter(out, .data$lon == 0.25, .data$lat == 0.75)
  testthat::expect_equal(first$source_area_ha, 110)
  testthat::expect_equal(first$image_region, 1L)
  testthat::expect_equal(first$cell_id, (178L * 720L) + 361L)
  testthat::expect_equal(nrow(dplyr::distinct(out, .data$cell_id)), nrow(out))
})

testthat::test_that("source manifest pins every grid-boundary raster", {
  manifest <- whep:::.critn_manifest()
  pointblank::expect_col_exists(
    manifest,
    c("relative_path", "bytes", "md5", "sha256")
  )
  testthat::expect_equal(nrow(manifest), 27L)
  testthat::expect_equal(length(unique(manifest$relative_path)), 27L)
  testthat::expect_true(all(grepl("^[0-9a-f]{64}$", manifest$sha256)))
  testthat::expect_setequal(
    manifest$relative_path[grepl("^Input_files", manifest$relative_path)],
    c(
      "Input_files/a_crop.asc",
      "Input_files/a_gr_int.asc",
      "Input_files/image_region28.asc"
    )
  )
})

testthat::test_that("selected source rasters fail closed on content drift", {
  tmp <- withr::local_tempdir()
  .critical_n_write_asc(tmp)
  paths <- whep:::.critn_selected_paths(
    "critical_n_surplus",
    "mi",
    "all"
  )
  root <- file.path(tmp, "extracted", whep:::.critn_archive_root())
  files <- file.path(root, paths)
  manifest <- tibble::tibble(
    relative_path = paths,
    bytes = as.numeric(file.info(files)$size),
    md5 = unname(tools::md5sum(files)),
    sha256 = unname(tools::sha256sum(files))
  )
  testthat::local_mocked_bindings(
    .critn_manifest = function() manifest
  )
  testthat::expect_invisible(whep:::.critn_verify_selected(
    tmp,
    "critical_n_surplus",
    "mi",
    "all"
  ))
  write("tamper", files[[1L]], append = TRUE)
  testthat::expect_error(
    whep:::.critn_verify_selected(
      tmp,
      "critical_n_surplus",
      "mi",
      "all"
    ),
    "failed content verification"
  )
})

testthat::test_that("read_critical_n(example = TRUE) returns lon/lat/value", {
  out <- whep::read_critical_n(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "value", "critical_var", "critical_land_use")
  )
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})

testthat::test_that("read_critical_n(data=) bypasses the file read", {
  grid <- tibble::tribble(
    ~lon, ~lat, ~value,
    -0.25, 51.75, 84,
    0.25, 51.75, 12
  )
  out <- whep::read_critical_n(var = "exceedance", data = grid)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "value", "critical_var", "critical_land_use")
  )
  testthat::expect_equal(out$value, c(84, 12))
  testthat::expect_true(all(out$critical_var == "exceedance"))
})

testthat::test_that("read_critical_n rejects an unknown var", {
  testthat::expect_error(
    whep::read_critical_n(var = "not_a_layer", data = tibble::tibble()),
    "arg_match|must be one of|not_a_layer"
  )
})

testthat::test_that("read_critical_n(data=) errors on missing columns", {
  testthat::expect_error(
    whep::read_critical_n(var = "exceedance", data = tibble::tibble(x = 1)),
    "value|lon|lat"
  )
})

testthat::test_that("no configured directory falls through to the cache", {
  withr::local_envvar(WHEP_CRITICAL_N_DIR = "")
  # The cache is consulted instead of aborting, and nothing downloads unless
  # the cache is empty. Mocked so the assertion holds whether or not the
  # developer running the suite happens to have a populated cache.
  reached <- FALSE
  testthat::local_mocked_bindings(
    .critn_cached_dir = function(...) {
      reached <<- TRUE
      "/nonexistent-cache"
    }
  )
  testthat::expect_equal(
    whep:::.resolve_critical_n_dir(),
    "/nonexistent-cache"
  )
  testthat::expect_true(reached)
})

testthat::test_that("a set WHEP_CRITICAL_N_DIR wins over the cache", {
  dir <- withr::local_tempdir()
  withr::local_envvar(WHEP_CRITICAL_N_DIR = dir)
  testthat::local_mocked_bindings(
    .critn_cached_dir = function(...) testthat::fail("cache must not be used")
  )
  testthat::expect_equal(whep:::.resolve_critical_n_dir(), dir)
})

# ---- Archive resolution and on-demand Zenodo fetch --------------------------

testthat::test_that("the archive constants match the published Zenodo record", {
  testthat::expect_match(
    whep:::.critn_archive_url(),
    "zenodo\\.org/api/records/6395016"
  )
  testthat::expect_match(whep:::.critn_archive_md5(), "^[0-9a-f]{32}$")
  testthat::expect_equal(
    whep:::.critn_archive_md5(),
    "d6b4bf88e9b140bd25a147396e371733"
  )
  # The cache-hit marker must be the directory the reader then reads through.
  testthat::expect_match(whep:::.critn_archive_root(), "^Global_critical_N_")
})

testthat::test_that("an explicit dir short-circuits the cache and the fetch", {
  dir <- withr::local_tempdir()
  testthat::expect_equal(whep:::.resolve_critical_n_dir(dir), dir)
})

testthat::test_that("a populated cache is reused without downloading", {
  dir <- withr::local_tempdir()
  dir.create(
    file.path(dir, "extracted", whep:::.critn_archive_root()),
    recursive = TRUE
  )
  boom <- function(...) testthat::fail("must not touch the network")
  testthat::expect_equal(
    whep:::.critn_cached_dir(dir, download = boom, extract = boom),
    dir
  )
})

testthat::test_that("an empty cache downloads then extracts, in that order", {
  dir <- withr::local_tempdir()
  calls <- character()
  fake_download <- function(d) {
    calls <<- c(calls, "download")
    file.path(d, "critical_n_archive.7z")
  }
  fake_extract <- function(archive, exdir) {
    calls <<- c(calls, "extract")
    dir.create(
      file.path(exdir, whep:::.critn_archive_root()),
      recursive = TRUE
    )
  }
  out <- whep:::.critn_cached_dir(
    dir,
    download = fake_download,
    extract = fake_extract
  )
  testthat::expect_equal(calls, c("download", "extract"))
  testthat::expect_equal(out, dir)
})

testthat::test_that("an extraction that yields the wrong layout aborts", {
  dir <- withr::local_tempdir()
  testthat::expect_error(
    whep:::.critn_cached_dir(
      dir,
      download = function(d) "archive.7z",
      extract = function(archive, exdir) dir.create(exdir, recursive = TRUE)
    ),
    "did not unpack as expected"
  )
})

testthat::test_that("a cached archive with the right MD5 is not re-downloaded", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "critical_n_archive.7z")
  writeLines("x", path)
  testthat::expect_false(whep:::.critn_md5_ok(path))
  testthat::local_mocked_bindings(
    .critn_archive_md5 = function() unname(tools::md5sum(path))
  )
  testthat::expect_true(whep:::.critn_md5_ok(path))
  # .critn_download() returns the cached path without hitting the network:
  # download.file() would error on the unreachable URL if it were called.
  testthat::expect_equal(whep:::.critn_download(dir), path)
})

testthat::test_that("7z discovery returns a real path or nothing", {
  bin <- whep:::.critn_7z_binary()
  if (is.null(bin)) {
    testthat::succeed()
  } else {
    testthat::expect_true(file.exists(bin))
  }
})

testthat::test_that("no extractor aborts, naming the verified download", {
  dir <- withr::local_tempdir()
  archive <- file.path(dir, "critical_n_archive.7z")
  writeLines("x", archive)
  testthat::local_mocked_bindings(.critn_7z_binary = function() NULL)
  testthat::local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )
  testthat::expect_error(
    whep:::.critn_extract(archive, file.path(dir, "extracted")),
    "No 7-Zip extractor available"
  )
  testthat::expect_error(
    whep:::.critn_extract(archive, file.path(dir, "extracted")),
    "MD5-verified"
  )
})

# ---- The two 7z extraction back-ends (#451) ---------------------------------

# fixtures/critical_n_mini.7z is a 495-byte stand-in for the 18.4 MB Zenodo
# asset: the same top-level directory, the same "Output_files/Critical N
# surpluses" subdirectory (spaces included, as in the real archive) and a 4x2
# ESRI ASCII grid in place of each 720x360 one, so the extracted layout is what
# .read_critical_n_file() reads through. Rebuild it with
#   7z a -t7z -mx=9 critical_n_mini.7z <root_dir>
# It is a real 7-Zip archive, so both back-ends have to be able to open it.
critn_mini_archive <- function() {
  testthat::test_path("fixtures", "critical_n_mini.7z")
}

critn_mini_files <- function(exdir) {
  root <- file.path(exdir, whep:::.critn_archive_root())
  c(
    file.path(
      root,
      "Output_files",
      "Critical N surpluses",
      "nsur_crit_mi_all_ph.asc"
    ),
    file.path(root, "Input_files", "a_crop.asc"),
    file.path(root, "Input_files", "a_gr_int.asc"),
    file.path(root, "Input_files", "image_region28.asc")
  )
}

testthat::test_that("the libarchive back-end unpacks a real 7z archive", {
  testthat::skip_if_not_installed("archive")
  dir <- withr::local_tempdir()
  exdir <- file.path(dir, "extracted")
  dir.create(exdir, recursive = TRUE)
  whep:::.critn_extract_archive(critn_mini_archive(), exdir)
  testthat::expect_true(all(file.exists(critn_mini_files(exdir))))
})

testthat::test_that("the 7-Zip binary back-end unpacks a real 7z archive", {
  testthat::skip_if(
    is.null(whep:::.critn_7z_binary()),
    "no 7-Zip binary on PATH"
  )
  dir <- withr::local_tempdir()
  exdir <- file.path(dir, "extracted")
  dir.create(exdir, recursive = TRUE)
  testthat::expect_true(whep:::.critn_extract_7z(critn_mini_archive(), exdir))
  testthat::expect_true(all(file.exists(critn_mini_files(exdir))))
})

testthat::test_that("the 7-Zip back-end quotes an output path with spaces", {
  testthat::skip_if(
    is.null(whep:::.critn_7z_binary()),
    "no 7-Zip binary on PATH"
  )
  # rappdirs cache paths can contain spaces (a user name is enough). An
  # unquoted -o splits there and 7-Zip extracts nothing while still exiting 0,
  # so the failure is silent.
  dir <- withr::local_tempdir()
  exdir <- file.path(dir, "a cache dir", "extracted")
  dir.create(exdir, recursive = TRUE)
  testthat::expect_true(whep:::.critn_extract_7z(critn_mini_archive(), exdir))
  testthat::expect_true(all(file.exists(critn_mini_files(exdir))))
})

testthat::test_that("extraction prefers libarchive over the 7-Zip binary", {
  dir <- withr::local_tempdir()
  used <- character()
  testthat::local_mocked_bindings(
    is_installed = function(...) TRUE,
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .critn_extract_archive = function(archive, exdir) {
      used <<- c(used, "archive")
      invisible(exdir)
    },
    .critn_extract_7z = function(...) testthat::fail("binary must not be used")
  )
  whep:::.critn_extract("a.7z", file.path(dir, "extracted"))
  testthat::expect_equal(used, "archive")
})

testthat::test_that("extraction falls back to the 7-Zip binary", {
  dir <- withr::local_tempdir()
  exdir <- file.path(dir, "extracted")
  seen <- list()
  testthat::local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .critn_extract_archive = function(...) {
      testthat::fail("libarchive must not be used")
    },
    .critn_7z_binary = function() "/fake/7z",
    .critn_extract_7z = function(archive, exdir, bin = NULL) {
      seen <<- list(archive = archive, exdir = exdir, bin = bin)
      TRUE
    }
  )
  testthat::expect_equal(whep:::.critn_extract("a.7z", exdir), exdir)
  testthat::expect_equal(seen$archive, "a.7z")
  testthat::expect_equal(seen$exdir, exdir)
  testthat::expect_equal(seen$bin, "/fake/7z")
})

testthat::test_that("a 7-Zip binary that fails aborts, it does not pass", {
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )
  testthat::local_mocked_bindings(
    .critn_7z_binary = function() "/fake/7z",
    .critn_extract_7z = function(...) FALSE
  )
  testthat::expect_error(
    whep:::.critn_extract("a.7z", file.path(dir, "extracted")),
    "No 7-Zip extractor available"
  )
})

testthat::test_that("a checksum mismatch aborts instead of extracting", {
  # A local file:// URL stands in for the Zenodo asset, so download.file() is
  # really called with no network. Windows spells file URLs differently enough
  # that this leg is only run elsewhere.
  testthat::skip_on_os("windows")
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .critn_archive_url = function() {
      paste0("file://", normalizePath(critn_mini_archive()))
    }
  )
  testthat::expect_error(
    whep:::.critn_download(dir),
    "does not match the published MD5"
  )
  # The download did happen; it is the verification that rejected it.
  testthat::expect_true(file.exists(file.path(dir, "critical_n_archive.7z")))
})

testthat::test_that("an empty cache downloads, unpacks and then reads", {
  testthat::skip_on_os("windows")
  testthat::skip_if(
    !rlang::is_installed("archive") && is.null(whep:::.critn_7z_binary()),
    "no 7z extractor available"
  )
  dir <- withr::local_tempdir()
  fixture <- critn_mini_archive()
  testthat::local_mocked_bindings(
    .critn_archive_url = function() paste0("file://", normalizePath(fixture)),
    .critn_archive_md5 = function() unname(tools::md5sum(fixture))
  )
  # Real .critn_download() and real .critn_extract(): the whole first-run path
  # minus the network.
  testthat::expect_equal(whep:::.critn_cached_dir(dir), dir)
  out <- whep::read_critical_n("critical_n_surplus", dir = dir)
  # 4x2 grid with one NODATA cell.
  testthat::expect_equal(nrow(out), 7L)
  testthat::expect_equal(sort(out$value), c(10, 20, 40, 50, 60, 70, 80))
  testthat::expect_true(
    all(out$lon %in% c(-179.75, -179.25, -178.75, -178.25))
  )
  testthat::expect_true(all(out$lat %in% c(-89.25, -89.75)))
  # land_use = "all" sums the arable and intensive-grassland source areas.
  testthat::expect_equal(
    out$source_area_ha,
    out$value * 11,
    tolerance = 1e-9
  )
  testthat::expect_true(all(out$image_region %in% 1:8))
})

# ---- Extensive-grassland budget (SI Supplementary Table 4) ---------------

# Hand-computed fixture, kg N per cell per year and ha per cell.
# Cell 1 (ndep > nh3_tot): dep_corr is max(5000, 3000), so 5000;
#   f_egl is 400 of 1000 ha, 0.4, so dep_ext is 2000;
#   input is 2000 manure + 800 fixation + 2000 deposition, 4800;
#   surplus is 4800 less 3000 uptake, 1800; rates 12 and 4.5 kg/ha.
# Cell 2 (nh3_tot > ndep): dep_corr is max(4000, 6000), so 6000;
#   f_egl is 500 of 2000 ha, 0.25, so dep_ext is 1500;
#   input is 1000 manure + 250 fixation + 1500 deposition, 2750;
#   surplus is 2750 less 1500 uptake, 1250; rates 5.5 and 2.5 kg/ha.
# Cell 3 (no extensive grassland): f_egl 0, dep_ext 0; input 0,
#   surplus 0; rates NA.
# Cell 4 (tie, whole cell extensive, negative surplus): dep_corr 250;
#   f_egl 1, dep_ext 250; input 100 + 200 + 250, 550;
#   surplus 550 less 900, -350; rates 1.1 and -0.7 kg/ha.
.critn_budget_fixture <- function() {
  tibble::tribble(
    ~cell_id, ~a_tot_ha, ~a_gr_ext_ha, ~manure_ext_n_kg, ~fix_ext_n_kg,
    ~uptake_ext_n_kg, ~ndep_n_kg, ~nh3_tot_n_kg,
    # cell  a_tot  a_ext  manure  fix  uptake  ndep  nh3_tot
    1L,     1000,  400,   2000,   800, 3000,   5000, 3000,
    2L,     2000,  500,   1000,   250, 1500,   4000, 6000,
    3L,     1500,  0,     0,      0,   0,      3000, 1000,
    4L,     500,   500,   100,    200, 900,    250,  250
  )
}

testthat::test_that("extensive budget follows SI Table 4 on both branches", {
  out <- whep:::.critical_n_extensive_budget(.critn_budget_fixture())
  testthat::expect_named(
    out,
    c(
      "cell_id",
      "ext_input_n_kg",
      "ext_surplus_n_kg",
      "ext_input_kgn_ha",
      "ext_surplus_kgn_ha"
    )
  )
  testthat::expect_equal(out$cell_id, 1:4)
  testthat::expect_equal(out$ext_input_n_kg, c(4800, 2750, 0, 550))
  testthat::expect_equal(out$ext_surplus_n_kg, c(1800, 1250, 0, -350))
  testthat::expect_equal(out$ext_input_kgn_ha, c(12, 5.5, NA, 1.1))
  testthat::expect_equal(out$ext_surplus_kgn_ha, c(4.5, 2.5, NA, -0.7))
})

testthat::test_that("extensive budget takes NH3 only when it exceeds ndep", {
  base <- .critn_budget_fixture()[1:2, ]
  out <- whep:::.critical_n_extensive_budget(base)
  # Lowering NH3 below ndep in cell 1 changes nothing (ndep branch);
  # raising ndep above NH3 in cell 2 to 8000 doubles its deposition share:
  # dep_ext = 8000 * 0.25 = 2000, input = 1000 + 250 + 2000 = 3250.
  moved <- base |>
    dplyr::mutate(
      nh3_tot_n_kg = c(10, 6000),
      ndep_n_kg = c(5000, 8000)
    ) |>
    whep:::.critical_n_extensive_budget()
  testthat::expect_equal(moved$ext_input_n_kg[1], out$ext_input_n_kg[1])
  testthat::expect_equal(moved$ext_input_n_kg[2], 3250)
})

testthat::test_that("extensive rates are NA where extensive area is missing", {
  layers <- .critn_budget_fixture()[1:2, ] |>
    dplyr::mutate(a_gr_ext_ha = c(NA, 500))
  out <- whep:::.critical_n_extensive_budget(layers)
  testthat::expect_true(is.na(out$ext_input_kgn_ha[1]))
  testthat::expect_true(is.na(out$ext_surplus_kgn_ha[1]))
  testthat::expect_equal(out$ext_input_kgn_ha[2], 5.5)
})

testthat::test_that("a missing flow on extensive grassland aborts by name", {
  layers <- .critn_budget_fixture() |>
    dplyr::mutate(fix_ext_n_kg = c(800, NA, 0, 200))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    class = "whep_critn_budget_missing_flow"
  )
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    "fix_ext_n_kg"
  )
  dep_gap <- .critn_budget_fixture() |>
    dplyr::mutate(nh3_tot_n_kg = c(NA, 6000, 1000, 250))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(dep_gap),
    "nh3_tot_n_kg",
    class = "whep_critn_budget_missing_flow"
  )
})

testthat::test_that("extensive cells with no manure, fixation, uptake abort", {
  layers <- .critn_budget_fixture() |>
    dplyr::mutate(
      manure_ext_n_kg = c(NA, NA, 0, 100),
      fix_ext_n_kg = c(NA, NA, 0, 200),
      uptake_ext_n_kg = c(NA, NA, 0, 900)
    )
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    "2 cells",
    class = "whep_critn_budget_missing_flow"
  )
})

testthat::test_that("a missing flow on a cell without extensive land passes", {
  layers <- .critn_budget_fixture() |>
    dplyr::mutate(fix_ext_n_kg = c(800, 250, NA, 200))
  out <- whep:::.critical_n_extensive_budget(layers)
  testthat::expect_true(is.na(out$ext_input_n_kg[3]))
  testthat::expect_true(is.na(out$ext_input_kgn_ha[3]))
  testthat::expect_equal(out$ext_input_n_kg[c(1, 2, 4)], c(4800, 2750, 550))
})

testthat::test_that("extensive flows on a cell without extensive land abort", {
  layers <- .critn_budget_fixture() |>
    dplyr::mutate(manure_ext_n_kg = c(2000, 1000, 75, 100))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    class = "whep_critn_budget_flow_without_area"
  )
})

testthat::test_that("an extensive area outside its cell aborts", {
  bad_total <- .critn_budget_fixture() |>
    dplyr::mutate(a_tot_ha = c(1000, 2000, 1500, 400))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(bad_total),
    class = "whep_critn_budget_bad_area"
  )
  no_total <- .critn_budget_fixture() |>
    dplyr::mutate(a_tot_ha = c(NA, 2000, 1500, 500))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(no_total),
    class = "whep_critn_budget_bad_area"
  )
})

testthat::test_that("several extensive areas outside their cells abort", {
  # Two bad cells: the cell-id list is a vector, which once crashed cli's
  # pluraliser before the intended condition class was raised.
  two_bad <- .critn_budget_fixture() |>
    dplyr::mutate(a_tot_ha = c(300, NA, 1500, 500))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(two_bad),
    "Cells: 1 and 2",
    class = "whep_critn_budget_bad_area"
  )
  one_bad <- .critn_budget_fixture() |>
    dplyr::mutate(a_tot_ha = c(300, 2000, 1500, 500))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(one_bad),
    "Cell: 1\\.",
    class = "whep_critn_budget_bad_area"
  )
})

testthat::test_that("flows on several cells without extensive land abort", {
  layers <- .critn_budget_fixture() |>
    dplyr::mutate(
      a_gr_ext_ha = c(400, 0, 0, 500),
      manure_ext_n_kg = c(2000, 1000, 75, 100)
    )
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    "Cells: 2 and 3",
    class = "whep_critn_budget_flow_without_area"
  )
  one <- .critn_budget_fixture() |>
    dplyr::mutate(manure_ext_n_kg = c(2000, 1000, 75, 100))
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(one),
    "Cell: 3\\.",
    class = "whep_critn_budget_flow_without_area"
  )
})

testthat::test_that("extensive budget requires its input columns", {
  layers <- dplyr::select(.critn_budget_fixture(), -"uptake_ext_n_kg")
  testthat::expect_error(
    whep:::.critical_n_extensive_budget(layers),
    "uptake_ext_n_kg"
  )
})

testthat::test_that("NH3 total sums the four whole-cell NH3 layers", {
  testthat::expect_equal(
    whep:::.critical_n_nh3_tot(
      graz = c(1, 10, 0),
      spread_fe = c(2, 20, 0),
      spread_man = c(3, 30, 0),
      stor = c(4, 40, 0)
    ),
    c(10, 100, 0)
  )
  # A missing layer value stays missing; it is never read as zero.
  testthat::expect_equal(
    whep:::.critical_n_nh3_tot(
      graz = c(1, NA),
      spread_fe = c(2, 20),
      spread_man = c(3, 30),
      stor = c(4, 40)
    ),
    c(10, NA)
  )
})

# Writes a minimal 1-row x 3-column synthetic archive (one cell per test
# case: intensive, extensive, cropland-only) at the Input_files path
# .critical_n_grassland_layers() reads. Cell centres: col 1 lon -179.75,
# col 2 lon -179.25, col 3 lon -178.75, all lat 89.75 -- on the canonical
# 0.5-degree grid so .nbx_add_cell_key() accepts them, with cell_id 1, 2, 3.
# Every file uses NODATA -1 except nfix_grass_ext.asc, which uses -2, to
# exercise that .read_esri_asc() reads each file's own NODATA from its own
# header rather than a hard-coded constant.
.critn_grassland_write_asc <- function(dir, a_gr_int, a_gr_ext) {
  input <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Input_files"
  )
  dir.create(input, recursive = TRUE, showWarnings = FALSE)
  header <- c(
    paste("ncols", length(a_gr_int)),
    "nrows 1",
    "xllcorner -180",
    "yllcorner 89.5",
    "cellsize 0.5",
    "NODATA_value -1"
  )
  n <- length(a_gr_int)
  keep <- function(values) values[seq_len(n)]
  write_grid <- function(file, values) {
    writeLines(c(header, paste(values, collapse = " ")), file.path(input, file))
  }
  write_grid("a_tot.asc", keep(c(200, 200, 500)))
  write_grid("a_crop.asc", keep(c(-1, -1, 500)))
  write_grid("a_gr_int.asc", a_gr_int)
  write_grid("a_gr_ext.asc", a_gr_ext)
  write_grid("n_man_eff_grass_int.asc", keep(c(300, -1, -1)))
  write_grid("n_man_eff_grass_ext.asc", keep(c(-1, 400, -1)))
  writeLines(
    c(
      paste("ncols", n),
      "nrows 1",
      "xllcorner -180",
      "yllcorner 89.5",
      "cellsize 0.5",
      "NODATA_value -2"
    ),
    file.path(input, "nfix_grass_ext.asc")
  )
  cat(
    paste(keep(c(-2, 100, -2)), collapse = " "),
    file = file.path(input, "nfix_grass_ext.asc"),
    append = TRUE,
    fill = TRUE
  )
  write_grid("n_up_grass_ext.asc", keep(c(-1, 250, -1)))
  write_grid("ndep.asc", keep(c(120, 90, 60)))
  write_grid("nh3_graz.asc", keep(c(10, 15, 3)))
  write_grid("nh3_spread_fe.asc", keep(c(5, 7, 2)))
  write_grid("nh3_spread_man.asc", keep(c(8, 9, 1)))
  write_grid("nh3_stor.asc", keep(c(2, -1, 4)))
  invisible(dir)
}

testthat::test_that(".critical_n_grassland_layers reads and joins layers", {
  tmp <- withr::local_tempdir()
  .critn_grassland_write_asc(
    tmp,
    a_gr_int = c(50, -1, -1),
    a_gr_ext = c(-1, 80, -1)
  )
  root <- file.path(
    tmp,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances"
  )
  out <- whep:::.critical_n_grassland_layers(root)

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_named(
    out,
    c(
      "cell_id",
      "lon",
      "lat",
      "a_tot_ha",
      "a_crop_ha",
      "a_gr_int_ha",
      "a_gr_ext_ha",
      "manure_int_n_kg",
      "manure_ext_n_kg",
      "fix_ext_n_kg",
      "uptake_ext_n_kg",
      "ndep_n_kg",
      "nh3_tot_n_kg",
      "image_class_2010"
    )
  )

  a <- dplyr::filter(out, .data$cell_id == 1L)
  b <- dplyr::filter(out, .data$cell_id == 2L)
  c_ <- dplyr::filter(out, .data$cell_id == 3L)

  # Cell A: intensive grassland.
  testthat::expect_equal(a$lon, -179.75)
  testthat::expect_equal(a$lat, 89.75)
  testthat::expect_equal(a$a_tot_ha, 200)
  testthat::expect_equal(a$a_crop_ha, 0)
  testthat::expect_equal(a$a_gr_int_ha, 50)
  testthat::expect_equal(a$a_gr_ext_ha, 0)
  testthat::expect_equal(a$manure_int_n_kg, 300)
  testthat::expect_true(is.na(a$manure_ext_n_kg))
  testthat::expect_true(is.na(a$fix_ext_n_kg))
  testthat::expect_true(is.na(a$uptake_ext_n_kg))
  testthat::expect_equal(a$ndep_n_kg, 120)
  testthat::expect_equal(a$nh3_tot_n_kg, 25)
  testthat::expect_equal(a$image_class_2010, "intensive")

  # Cell B: extensive grassland. nh3_stor is deliberately NODATA here, so
  # the summed nh3_tot_n_kg must stay NA rather than silently drop the term.
  testthat::expect_equal(b$a_tot_ha, 200)
  testthat::expect_equal(b$a_crop_ha, 0)
  testthat::expect_equal(b$a_gr_int_ha, 0)
  testthat::expect_equal(b$a_gr_ext_ha, 80)
  testthat::expect_true(is.na(b$manure_int_n_kg))
  testthat::expect_equal(b$manure_ext_n_kg, 400)
  testthat::expect_equal(b$fix_ext_n_kg, 100)
  testthat::expect_equal(b$uptake_ext_n_kg, 250)
  testthat::expect_equal(b$ndep_n_kg, 90)
  testthat::expect_true(is.na(b$nh3_tot_n_kg))
  testthat::expect_equal(b$image_class_2010, "extensive")

  # Cell C: cropland only, no grassland of either class.
  testthat::expect_equal(c_$a_tot_ha, 500)
  testthat::expect_equal(c_$a_crop_ha, 500)
  testthat::expect_equal(c_$a_gr_int_ha, 0)
  testthat::expect_equal(c_$a_gr_ext_ha, 0)
  testthat::expect_true(is.na(c_$manure_int_n_kg))
  testthat::expect_true(is.na(c_$manure_ext_n_kg))
  testthat::expect_true(is.na(c_$fix_ext_n_kg))
  testthat::expect_true(is.na(c_$uptake_ext_n_kg))
  testthat::expect_equal(c_$ndep_n_kg, 60)
  testthat::expect_equal(c_$nh3_tot_n_kg, 10)
  testthat::expect_true(is.na(c_$image_class_2010))

  # Areas are never NA: absence of a class is a structural zero.
  testthat::expect_false(anyNA(out$a_crop_ha))
  testthat::expect_false(anyNA(out$a_gr_int_ha))
  testthat::expect_false(anyNA(out$a_gr_ext_ha))
})

testthat::test_that("grassland layers abort on a mixed-class cell", {
  tmp <- withr::local_tempdir()
  .critn_grassland_write_asc(tmp, a_gr_int = 40, a_gr_ext = 30)
  root <- file.path(
    tmp,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances"
  )
  testthat::expect_error(
    whep:::.critical_n_grassland_layers(root),
    class = "whep_critn_mixed_grassland"
  )
})

testthat::test_that("an all-zero extensive uptake layer is refused", {
  layers <- .critn_budget_fixture()
  layers$uptake_ext_n_kg <- 0
  ext <- layers$a_gr_ext_ha > 0
  inputs <- layers$manure_ext_n_kg[ext] + layers$fix_ext_n_kg[ext]
  # surplus = input - uptake still reconciles when uptake is zero-filled.
  expect_supplied_guard(
    identity = isTRUE(all.equal(inputs - layers$uptake_ext_n_kg[ext], inputs)),
    guard = whep:::.critical_n_extensive_budget(layers)
  )
})
