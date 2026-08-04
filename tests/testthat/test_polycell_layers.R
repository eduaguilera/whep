# Tests for the polycell input layers (plan
# `plans/2026-08-03-polycell-spatial-support.md`, DA-6, DA-9, DA-17).
#
# The real layers are multi-hundred-megabyte local files behind environment
# variables, so what is exercised here is the reader contract: the CLM binary
# format, the s2 repair the ice layer needs, and the path resolution that must
# abort with an instruction rather than a hardcoded machine path.

# A minimal LPJmL CLM version-4 file plus its `.json` sidecar. `values` is
# supplied band-major per cell, which is the layout the real files use.
pcl_write_clm <- function(
  dir,
  name,
  values,
  nbands,
  datatype,
  scalar,
  magic = "LPJGRID",
  offset = 59L
) {
  path <- file.path(dir, name)
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(magic), con)
  writeBin(raw(offset - 7L), con)
  if (datatype == "short") {
    writeBin(as.integer(values / scalar), con, size = 2L, endian = "little")
  } else {
    writeBin(as.double(values / scalar), con, size = 4L, endian = "little")
  }
  jsonlite::write_json(
    list(
      offset = offset,
      ncell = length(values) / nbands,
      nbands = nbands,
      nstep = 1L,
      nyear = 1L,
      scalar = scalar,
      datatype = datatype,
      bigendian = FALSE
    ),
    file.path(dir, paste0(name, ".json")),
    auto_unbox = TRUE
  )
  path
}

testthat::test_that("read_glwd_water reads a CLM grid and water pair", {
  testthat::skip_if_not_installed("jsonlite")

  dir <- withr::local_tempdir()
  # Three cells on the canonical half-degree centres, lon then lat per cell.
  pcl_write_clm(
    dir,
    "grid.clm",
    c(10.25, 45.25, 10.75, 45.25, 10.25, 45.75),
    nbands = 2L,
    datatype = "short",
    scalar = 0.01
  )
  pcl_write_clm(
    dir,
    "water.clm",
    c(0, 0.25, 1),
    nbands = 1L,
    datatype = "float",
    scalar = 1,
    magic = "LPJLAKE"
  )

  water <- whep::read_glwd_water(dir = dir, file = "water.clm")

  testthat::expect_equal(
    water,
    tibble::tibble(
      lon = c(10.25, 10.75, 10.25),
      lat = c(45.25, 45.25, 45.75),
      water_frac = c(0, 0.25, 1)
    )
  )
})

testthat::test_that("a CLM without its json sidecar aborts", {
  testthat::skip_if_not_installed("jsonlite")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "bare.clm")
  con <- file(path, "wb")
  writeBin(charToRaw("LPJGRID"), con)
  writeBin(raw(60L), con)
  close(con)

  testthat::expect_error(
    whep:::.read_clm(path),
    "sidecar"
  )
})

testthat::test_that("a non-CLM file is rejected on its magic string", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "not.clm")
  con <- file(path, "wb")
  writeBin(charToRaw("NOTACLM-and-more"), con)
  close(con)

  testthat::expect_error(whep:::.read_clm(path), "magic string")
})

testthat::test_that("layer readers name the environment variable they need", {
  withr::local_envvar(
    WHEP_LPJML_INPUT_DIR = "",
    WHEP_NATURALEARTH_DIR = "",
    WHEP_LUH2_DIR = ""
  )

  testthat::expect_error(
    whep::read_glwd_water(),
    "WHEP_LPJML_INPUT_DIR"
  )
  testthat::expect_error(
    whep:::.whep_layer_dir(NULL, "WHEP_MISSING_DIR", "a layer"),
    "WHEP_MISSING_DIR"
  )
})

testthat::test_that("s2-invalid polygons are repaired, classified and kept", {
  testthat::skip_if_not_installed("sf")

  # A bow-tie ring is invalid under both engines; a ring carrying a repeated
  # vertex is what `sf::st_intersection()` itself emits on the antimeridian,
  # and s2 then refuses to read its own output back. Both must be classified,
  # not crashed on.
  bowtie <- sf::st_polygon(list(cbind(
    c(0, 1, 0, 1, 0),
    c(0, 1, 1, 0, 0)
  )))
  duplicated_vertex <- sf::st_polygon(list(cbind(
    c(10, 10.5, 10.5, 10.5, 10, 10),
    c(45, 45, 45, 45.5, 45.5, 45)
  )))
  clean <- sf::st_polygon(list(cbind(
    c(20, 20.5, 20.5, 20, 20),
    c(45, 45, 45.5, 45.5, 45)
  )))

  fixed <- whep:::.s2_repair(
    sf::st_sfc(bowtie, duplicated_vertex, clean, crs = 4326)
  )

  testthat::expect_equal(fixed$status[[3L]], "ok")
  testthat::expect_true(all(fixed$status %in% c("ok", "repaired")))
  # Every repaired geometry is now readable by the spherical engine, and the
  # clean one is untouched.
  testthat::expect_equal(
    as.numeric(sf::st_area(fixed$geom[[3L]] |> sf::st_sfc(crs = 4326))),
    as.numeric(sf::st_area(sf::st_sfc(clean, crs = 4326)))
  )
  testthat::expect_true(all(is.finite(as.numeric(sf::st_area(fixed$geom)))))
})

testthat::test_that("read_polycell_support prefers a local parquet", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "support.parquet")
  support <- whep::build_polycell_support(example = TRUE)
  nanoparquet::write_parquet(support, path)
  withr::local_envvar(WHEP_POLYCELL_SUPPORT_PATH = path)

  testthat::expect_equal(
    whep::read_polycell_support()$polycell_id,
    support$polycell_id
  )
  testthat::expect_error(
    whep::read_polycell_support(path = file.path(dir, "absent.parquet")),
    "not found"
  )
})
