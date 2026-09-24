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

# ---- build_critical_n_binding(): argmin of the threshold surfaces ----------

# One layer per threshold, stamped as read_critical_n() stamps it.
.binding_layers <- function(values, land_use = "ara") {
  purrr::imap(values, \(value, threshold) {
    tibble::tibble(
      lon = c(0.25, 0.75, 1.25, 1.75, 2.25, 2.75, 3.25)[seq_along(value)],
      lat = 0.25,
      value = value,
      critical_var = "critical_n_surplus",
      critical_threshold = threshold,
      critical_land_use = land_use
    ) |>
      # read_critical_n() drops NODATA cells rather than returning NA.
      dplyr::filter(!is.na(value))
  })
}

.binding_values <- function() {
  list(
    de = c(5, 40, 30, 10, 20, 7, NA),
    gw = c(9, 12, 30, 10, 25, 7, 3),
    sw = c(9, 50, -4, 15, 20, 7, 3),
    mi = c(5, 12, -4, 10, 18, 7, 3)
  )
}

testthat::test_that("the binding threshold is the argmin with explicit ties", {
  out <- whep::build_critical_n_binding(
    .binding_layers(.binding_values()),
    land_use = "ara"
  )
  testthat::expect_equal(
    out$binding_threshold,
    c(
      "deposition",
      "groundwater",
      "surface_water",
      "deposition+groundwater",
      "deposition+surface_water",
      "yield_potential_cap",
      NA
    )
  )
  testthat::expect_equal(
    out$binding_critical_kgn_ha,
    c(5, 12, -4, 10, 20, 7, NA)
  )
  # Cell 5: the deposited mi (18) lies below every threshold surface (20).
  testthat::expect_equal(
    out$binding_matches_mi,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, NA)
  )
  testthat::expect_true(all(out$critical_land_use == "ara"))
  # A "+" label only ever names a genuine two-way tie.
  ties <- stringr::str_split(
    stringr::str_subset(out$binding_threshold, "\\+"),
    "\\+"
  )
  testthat::expect_true(all(lengths(ties) == 2L))
  testthat::expect_true(all(
    unlist(ties) %in% c("deposition", "groundwater", "surface_water")
  ))
})

testthat::test_that("all three surfaces equal is the yield-potential cap", {
  values <- list(
    de = c(40, -12, 25),
    gw = c(40, -12, 25),
    sw = c(40, -12, 26)
  )
  out <- whep::build_critical_n_binding(.binding_layers(values), "ara")
  testthat::expect_equal(
    out$binding_threshold,
    c("yield_potential_cap", "yield_potential_cap", "deposition+groundwater")
  )
  testthat::expect_false(any(grepl(
    "deposition+groundwater+surface_water",
    out$binding_threshold,
    fixed = TRUE
  )))
})

testthat::test_that("binding_matches_mi is NA when mi is not supplied", {
  values <- .binding_values()
  values$mi <- NULL
  out <- whep::build_critical_n_binding(.binding_layers(values), "ara")
  testthat::expect_true(all(is.na(out$critical_mi_kgn_ha)))
  testthat::expect_true(all(is.na(out$binding_matches_mi)))
  testthat::expect_equal(out$binding_threshold[[2]], "groundwater")
})

testthat::test_that("binding layers of the wrong kind or scope abort", {
  layers <- .binding_layers(.binding_values())
  testthat::expect_error(
    whep::build_critical_n_binding(layers, land_use = "all"),
    "critical_land_use"
  )
  wrong_var <- layers
  wrong_var$gw$critical_var <- "critical_n_input"
  testthat::expect_error(
    whep::build_critical_n_binding(wrong_var, "ara"),
    "critical_var"
  )
  swapped <- layers
  swapped$sw$critical_threshold <- "gw"
  testthat::expect_error(
    whep::build_critical_n_binding(swapped, "ara"),
    "critical_threshold"
  )
  testthat::expect_error(
    whep::build_critical_n_binding(layers[c("de", "gw")], "ara"),
    "named list"
  )
})

testthat::test_that("build_critical_n_binding reads four layers when absent", {
  calls <- list()
  values <- .binding_values()
  testthat::local_mocked_bindings(
    read_critical_n = function(var, threshold, land_use, dir) {
      calls[[length(calls) + 1L]] <<- c(var, threshold, land_use, dir)
      .binding_layers(values[threshold], land_use)[[1L]]
    }
  )
  out <- whep::build_critical_n_binding(land_use = "ara", dir = "archive")
  testthat::expect_setequal(
    purrr::map_chr(calls, \(x) x[[2]]),
    c("de", "gw", "sw", "mi")
  )
  testthat::expect_true(all(
    purrr::map_chr(calls, \(x) x[[1]]) == "critical_n_surplus"
  ))
  testthat::expect_true(all(purrr::map_chr(calls, \(x) x[[4]]) == "archive"))
  testthat::expect_equal(nrow(out), 7L)
})

testthat::test_that("the binding example is the real function on a fixture", {
  # Rows come back in cell-key order (north first); put them in the fixture's.
  out <- whep::build_critical_n_binding(example = TRUE) |>
    dplyr::arrange(lat, lon)
  testthat::expect_equal(
    out$binding_threshold,
    c(
      "deposition",
      "groundwater",
      "surface_water",
      "yield_potential_cap"
    )
  )
  testthat::expect_equal(out$binding_matches_mi, c(TRUE, TRUE, TRUE, FALSE))
})

testthat::test_that("binding_threshold is a deprecated alias of the exc map", {
  grid <- tibble::tibble(lon = 0.25, lat = 0.25, value = 5)
  testthat::expect_warning(
    old <- whep::read_critical_n(
      "binding_threshold",
      land_use = "ara",
      data = grid
    ),
    class = "whep_critn_var_deprecated"
  )
  new <- whep::read_critical_n(
    "threshold_exceedance",
    land_use = "ara",
    data = grid
  )
  testthat::expect_identical(old, new)
  testthat::expect_equal(new$critical_var, "threshold_exceedance")
  testthat::expect_equal(new$critical_land_use, "ara")
  testthat::expect_true(is.na(new$critical_threshold))
  spec <- whep:::.critical_n_var_spec("threshold_exceedance", "mi", "all")
  testthat::expect_equal(spec$subdir, "Threshold exceedance by impact")
  testthat::expect_equal(spec$file, "threshold_exc_all.asc")
})

# ---- real archive: the binding surface --------------------------------------

testthat::test_that("the real binding surface is complete and flags mi gaps", {
  dir <- .real_critn_dir()
  binding <- whep::build_critical_n_binding(land_use = "all", dir = dir)
  testthat::expect_equal(nrow(binding), 28881L)
  testthat::expect_false(anyNA(binding$binding_threshold))
  testthat::expect_equal(sum(!binding$binding_matches_mi), 1540L)
  testthat::expect_equal(
    sum(binding$binding_threshold == "yield_potential_cap"),
    9431L
  )
  gap <- abs(binding$critical_mi_kgn_ha - binding$binding_critical_kgn_ha)
  testthat::expect_equal(max(gap), 159.52, tolerance = 1e-6)
})
