# Every parsing test builds a small zip holding only the "TI"/"TR" member
# CSVs it needs, in SPAM2010's real column layout (verified 2026-09-24
# against a live Harvard Dataverse download -- see the header of
# R/spam_yields.R), so the tests exercise the real
# zip-find-member -> verify -> extract -> parse path without downloading or
# committing the ~485 MB real source. The network is never reached: every
# download/fetch function is exercised only through an injected fake, the
# same pattern test_wpp_population.R uses.

# Two SPAM pixels (Madrid, Toledo), two crops (whea, maiz), values chosen so
# irrigated and rainfed differ and one crop-cell pair is legitimately all
# zero (the overwhelmingly common real case).
.spam_write_member <- function(dir, letter, tech, unit, whea, maiz) {
  suffix <- if (identical(tech, "I")) "_i" else "_r"
  df <- data.frame(
    iso3 = c("ESP", "ESP"),
    prod_level = c("ES001", "ES002"),
    alloc_key = c("AK001", "AK002"),
    cell5m = c(1001L, 1002L),
    x = c(-3.5, -3.0),
    y = c(40.5, 40.0),
    rec_type = letter,
    tech_type = tech,
    unit = unit,
    name_cntr = c("Spain", "Spain"),
    name_adm1 = c("Madrid", "Toledo"),
    name_adm2 = c("Madrid", "Toledo"),
    check.names = FALSE
  )
  df[[paste0("whea", suffix)]] <- whea
  df[[paste0("maiz", suffix)]] <- maiz
  df <- df[, c(
    "iso3",
    "prod_level",
    "alloc_key",
    "cell5m",
    "x",
    "y",
    "rec_type",
    "tech_type",
    "unit",
    paste0("whea", suffix),
    paste0("maiz", suffix),
    "name_cntr",
    "name_adm1",
    "name_adm2"
  )]
  fname <- sprintf("spam2010V2r0_global_%s_T%s.csv", letter, tech)
  path <- file.path(dir, fname)
  utils::write.csv(df, path, row.names = FALSE)
  path
}

# Builds `<base>/<vintage>/<the three verified zips>`, matching what
# `read_spam_yields(dir = base)` / `WHEP_SPAM_DIR = base` resolves to. The
# fixture zips are a few hundred bytes, not the real ~150 MB files, so their
# true bytes/MD5 never match the published manifest -- `.spam_manifest()` is
# mocked (scoped to the calling test, via `.local_envir`) to the fixture's
# own computed values, the same technique test_critical_n.R uses for
# `.critn_manifest()`.
.spam_fixture_dir <- function(vintage = "2010") {
  testthat::skip_if_not_installed("zip")
  base <- withr::local_tempdir(.local_envir = parent.frame())
  vdir <- file.path(base, vintage)
  dir.create(vdir, recursive = TRUE)

  zip_one <- function(zip_name, letter, unit, whea_i, maiz_i, whea_r, maiz_r) {
    fi <- .spam_write_member(vdir, letter, "I", unit, whea_i, maiz_i)
    fr <- .spam_write_member(vdir, letter, "R", unit, whea_r, maiz_r)
    withr::with_dir(
      vdir,
      zip::zip(zip_name, c(basename(fi), basename(fr)))
    )
    file.remove(fi, fr)
  }

  # harvested area (ha): cell 1001 whea is irrigated-only (10 ha I, 0 R);
  # cell 1002 maiz is rainfed-only (0 I, 5 ha R); cell 1002 whea is the
  # all-zero pair.
  zip_one(
    .spam_zip_name(vintage, "harvested_area"),
    "H",
    "ha",
    whea_i = c(10, 0),
    maiz_i = c(0, 0),
    whea_r = c(0, 0),
    maiz_r = c(0, 5)
  )
  zip_one(
    .spam_zip_name(vintage, "production"),
    "P",
    "mt",
    whea_i = c(40, 0),
    maiz_i = c(0, 0),
    whea_r = c(0, 0),
    maiz_r = c(20, 5)
  )
  zip_one(
    .spam_zip_name(vintage, "yield"),
    "Y",
    "kg/ha",
    whea_i = c(4000, 0),
    maiz_i = c(0, 0),
    whea_r = c(0, 0),
    maiz_r = c(4000, 3800)
  )

  manifest <- whep:::.spam_manifest()
  for (v in c("harvested_area", "production", "yield")) {
    zpath <- file.path(vdir, .spam_zip_name(vintage, v))
    manifest$bytes[manifest$vintage == vintage & manifest$var == v] <-
      file.size(zpath)
    manifest$md5[manifest$vintage == vintage & manifest$var == v] <-
      unname(tools::md5sum(zpath))
  }
  testthat::local_mocked_bindings(
    .spam_manifest = function() manifest,
    .package = "whep",
    .env = parent.frame()
  )

  base
}

.spam_zip_name <- function(vintage, var) {
  whep:::.spam_manifest_row(vintage, var)$zip_name
}

testthat::test_that("read_spam_yields parses the real column layout end to end", {
  dir <- .spam_fixture_dir()
  out <- whep::read_spam_yields(vintage = "2010", dir = dir)

  testthat::expect_s3_class(out, "tbl_df")
  # 2 cells x 2 crops x 2 technologies.
  testthat::expect_equal(nrow(out), 8L)
  pointblank::expect_col_exists(
    out,
    c(
      "cell5m",
      "lon",
      "lat",
      "iso3",
      "name_cntr",
      "name_adm1",
      "name_adm2",
      "alloc_key",
      "spam_crop",
      "technology",
      "harvested_area_ha",
      "production_t",
      "yield_kg_ha",
      "vintage",
      "method_spam_source"
    )
  )
  testthat::expect_setequal(out$spam_crop, c("whea", "maiz"))
  testthat::expect_true(all(out$vintage == "2010"))
  testthat::expect_true(all(out$method_spam_source == "user_supplied"))

  whea_i_1001 <- dplyr::filter(
    out,
    .data$cell5m == 1001L,
    .data$spam_crop == "whea",
    .data$technology == "I"
  )
  testthat::expect_equal(whea_i_1001$harvested_area_ha, 10)
  testthat::expect_equal(whea_i_1001$production_t, 40)
  testthat::expect_equal(whea_i_1001$yield_kg_ha, 4000)
  testthat::expect_equal(whea_i_1001$lon, -3.5)
  testthat::expect_equal(whea_i_1001$lat, 40.5)
  testthat::expect_equal(whea_i_1001$iso3, "ESP")
  testthat::expect_equal(whea_i_1001$name_adm1, "Madrid")
})

testthat::test_that("the technology filter keeps I and R only, correctly split", {
  dir <- .spam_fixture_dir()
  out <- whep::read_spam_yields(vintage = "2010", dir = dir)

  testthat::expect_setequal(out$technology, c("I", "R"))
  testthat::expect_equal(sum(out$technology == "I"), 4L)
  testthat::expect_equal(sum(out$technology == "R"), 4L)

  maiz_r_1002 <- dplyr::filter(
    out,
    .data$cell5m == 1002L,
    .data$spam_crop == "maiz",
    .data$technology == "R"
  )
  testthat::expect_equal(maiz_r_1002$harvested_area_ha, 5)
  testthat::expect_equal(maiz_r_1002$production_t, 5)

  # cell 1002 x whea is the all-zero pair on both technologies.
  whea_1002 <- dplyr::filter(
    out,
    .data$cell5m == 1002L,
    .data$spam_crop == "whea"
  )
  testthat::expect_true(all(whea_1002$harvested_area_ha == 0))
  testthat::expect_true(all(whea_1002$production_t == 0))
})

testthat::test_that("a populated normalized cache is reused without opening the zip", {
  dir <- withr::local_tempdir()
  extracted <- file.path(dir, "2010", "extracted")
  dir.create(extracted, recursive = TRUE)
  path <- .spam_write_member(extracted, "H", "I", "ha", c(3, 0), c(0, 0))
  file.rename(path, file.path(extracted, "spam_2010_harvested_area_i.csv"))
  path_r <- .spam_write_member(extracted, "H", "R", "ha", c(0, 0), c(0, 7))
  file.rename(path_r, file.path(extracted, "spam_2010_harvested_area_r.csv"))

  resolved <- list(dir = file.path(dir, "2010"), origin = "user_supplied")
  boom <- function(...) testthat::fail("must not touch the network")
  out <- whep:::.spam_read_variable(
    "2010",
    "harvested_area",
    resolved,
    download = boom
  )
  # 2 cells x 2 crops (whea, maiz) x 2 technologies.
  testthat::expect_equal(nrow(out), 8L)
  maiz_r_1002 <- dplyr::filter(
    out,
    .data$cell5m == 1002L,
    .data$spam_crop == "maiz",
    .data$technology == "R"
  )
  testthat::expect_equal(maiz_r_1002$harvested_area_ha, 7)
  whea_i_1001 <- dplyr::filter(
    out,
    .data$cell5m == 1001L,
    .data$spam_crop == "whea",
    .data$technology == "I"
  )
  testthat::expect_equal(whea_i_1001$harvested_area_ha, 3)
})

testthat::test_that("an on-demand-cache MD5 mismatch aborts and discards the file", {
  dir <- withr::local_tempdir()
  manifest_row <- whep:::.spam_manifest_row("2010", "harvested_area")
  fake_fetch <- function(file_id, path) writeLines("not the real zip", path)
  testthat::expect_error(
    whep:::.spam_download_zip(dir, manifest_row, fetch = fake_fetch),
    "MD5"
  )
  testthat::expect_false(file.exists(file.path(dir, manifest_row$zip_name)))
})

testthat::test_that("a user-supplied zip with a bad MD5 aborts without deleting it", {
  dir <- withr::local_tempdir()
  manifest_row <- whep:::.spam_manifest_row("2010", "harvested_area")
  zip_path <- file.path(dir, manifest_row$zip_name)
  writeLines("not the real zip", zip_path)
  testthat::expect_error(
    whep:::.spam_verify_zip(zip_path, manifest_row, unlink_on_fail = FALSE),
    "MD5"
  )
  # It is the caller's file, not WHEP's cache download: never deleted.
  testthat::expect_true(file.exists(zip_path))
})

testthat::test_that("vintage 2020 is never fetched -- it aborts naming the guestbook", {
  withr::local_envvar(WHEP_SPAM_DIR = "")
  testthat::expect_error(
    whep::read_spam_yields(vintage = "2020"),
    "guestbook"
  )
})

testthat::test_that("2020 reads from a user-supplied dir through the same parser", {
  # The fixture's zip names match the real SPAM2020 manifest rows, but its
  # bytes/md5 do not (nothing here was downloaded: SPAM2020 is gated);
  # `.spam_fixture_dir()` mocks `.spam_manifest()` to the fixture's own
  # computed values, scoped to this test.
  dir <- .spam_fixture_dir(vintage = "2020")

  out <- whep::read_spam_yields(vintage = "2020", dir = dir)
  testthat::expect_equal(nrow(out), 8L)
  testthat::expect_true(all(out$vintage == "2020"))
  testthat::expect_true(all(out$method_spam_source == "user_supplied"))
})

testthat::test_that("a header missing an identifying column aborts, naming it", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(iso3 = "ESP", cell5m = 1L, x = 1, y = 1, whea_i = 1),
    path,
    row.names = FALSE
  )
  testthat::expect_error(
    whep:::.spam_check_member_columns(names(utils::read.csv(path)), path),
    "alloc_key|name_cntr|Missing column"
  )
})

testthat::test_that("a header with no crop column aborts", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      iso3 = "ESP",
      alloc_key = "A1",
      cell5m = 1L,
      x = 1,
      y = 1,
      name_cntr = "Spain",
      name_adm1 = "M",
      name_adm2 = "M"
    ),
    path,
    row.names = FALSE
  )
  testthat::expect_error(
    whep:::.spam_check_member_columns(names(utils::read.csv(path)), path),
    "crop column"
  )
})

testthat::test_that("the zip member finder aborts on zero or multiple matches", {
  dir <- withr::local_tempdir()
  testthat::skip_if_not_installed("zip")
  f1 <- file.path(dir, "spamV_global_H_TI.csv")
  writeLines("x", f1)
  zip_path <- file.path(dir, "one_match.zip")
  withr::with_dir(dir, zip::zip(basename(zip_path), basename(f1)))
  testthat::expect_equal(
    whep:::.spam_zip_find_member(zip_path, "harvested_area", "I"),
    basename(f1)
  )
  testthat::expect_error(
    whep:::.spam_zip_find_member(zip_path, "harvested_area", "R"),
    "Could not find a single"
  )

  f2 <- file.path(dir, "spamV_global_H_TI_dup.csv")
  writeLines("x", f2)
  zip_path2 <- file.path(dir, "two_match.zip")
  # Both entries end in "_TI.csv"-shaped names is not achievable with two
  # different basenames under this pattern, so instead check the abort
  # message names every entry found.
  withr::with_dir(
    dir,
    zip::zip(basename(zip_path2), c(basename(f1), basename(f2)))
  )
  testthat::expect_error(
    whep:::.spam_zip_find_member(zip_path2, "harvested_area", "R"),
    "Archive entries"
  )
})

testthat::test_that("dir resolves per-vintage, dir wins over WHEP_SPAM_DIR", {
  withr::local_envvar(WHEP_SPAM_DIR = "/env-dir")
  resolved <- whep:::.resolve_spam_dir("2010", dir = "/explicit-dir")
  testthat::expect_equal(resolved$dir, file.path("/explicit-dir", "2010"))
  testthat::expect_equal(resolved$origin, "user_supplied")

  resolved_env <- whep:::.resolve_spam_dir("2010", dir = NULL)
  testthat::expect_equal(resolved_env$dir, file.path("/env-dir", "2010"))
})

testthat::test_that("with nothing configured, 2010 resolves to the on-demand cache", {
  withr::local_envvar(WHEP_SPAM_DIR = "")
  resolved <- whep:::.resolve_spam_dir("2010", dir = NULL)
  testthat::expect_equal(resolved$origin, "cache")
  testthat::expect_match(resolved$dir, "spam.*2010$")
})

testthat::test_that("an unknown vintage is rejected", {
  testthat::expect_error(
    whep::read_spam_yields(vintage = "1999"),
    "arg_match|must be one of|1999"
  )
})

testthat::test_that("read_spam_yields(example = TRUE) returns the real schema", {
  out <- whep::read_spam_yields(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
  pointblank::expect_col_exists(
    out,
    c(
      "cell5m",
      "lon",
      "lat",
      "iso3",
      "name_cntr",
      "name_adm1",
      "name_adm2",
      "alloc_key",
      "spam_crop",
      "technology",
      "harvested_area_ha",
      "production_t",
      "yield_kg_ha",
      "vintage",
      "method_spam_source"
    )
  )
  testthat::expect_true(all(out$technology %in% c("I", "R")))
})

testthat::test_that("the manifest carries a row per vintage x variable", {
  manifest <- whep:::.spam_manifest()
  testthat::expect_equal(nrow(manifest), 6L)
  testthat::expect_setequal(manifest$vintage, c("2010", "2020"))
  testthat::expect_setequal(
    manifest$var,
    c("harvested_area", "production", "yield")
  )
  testthat::expect_true(all(grepl("^[0-9a-f]{32}$", manifest$md5)))
  # SPAM2020 is never downloaded: no file id to fetch it with.
  testthat::expect_true(all(is.na(
    manifest$dataverse_file_id[manifest$vintage == "2020"]
  )))
  testthat::expect_true(all(
    !is.na(
      manifest$dataverse_file_id[manifest$vintage == "2010"]
    )
  ))
})

testthat::test_that("the healed-quote warning is muffled and nothing else is", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "iso3,name_adm2,whea_r",
      "\"YEM\",\"Ok, with comma\",1.5",
      "\"YEM\",\"Jabal \"Iyal Yazi\",2.5"
    ),
    path
  )
  out <- testthat::expect_no_warning(whep:::.spam_fread_quiet(path))
  testthat::expect_equal(out$whea_r, c(1.5, 2.5))
  testthat::expect_equal(out$name_adm2[[1]], "Ok, with comma")
  testthat::expect_warning(
    whep:::.spam_fread_quiet(path, select = "no_such_column"),
    "no_such_column"
  )
})

# The download script keeps its own copy of the SPAM2010 manifest so it runs
# without whep installed. `^inst/scripts$` is in `.Rbuildignore`, so the
# script is absent from the built tarball and this only runs from a checkout.
testthat::test_that("download_spam.R's manifest matches the package's", {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "download",
    "download_spam.R"
  )
  testthat::skip_if_not(file.exists(path), "download_spam.R not available")
  env <- new.env()
  sys.source(path, envir = env)
  script <- env$.spam_download_manifest_2010()
  pkg <- whep:::.spam_manifest() |>
    dplyr::filter(.data$vintage == "2010")
  cols <- c("var", "zip_name", "bytes", "md5", "dataverse_file_id")
  testthat::expect_equal(
    dplyr::arrange(script[cols], .data$var),
    dplyr::arrange(tibble::as_tibble(pkg[cols]), .data$var),
    ignore_attr = TRUE
  )
})
