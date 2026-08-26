# `inst/extdata/earthstat_mapping.csv` is the crosswalk
# `prepare_crop_patterns()` iterates: one EarthStat harvested-area raster per
# row, aggregated into the `spatialize-crop-patterns` pin. A crop with no row
# gets no cell, so `build_gridded_landuse()` drops its entire world total in
# every country and every year -- silently, because the allocator's existing
# warning reports it as one ordinary `(country, crop)` miss per country.
#
# That is how barley, the fourth-largest crop on Earth, went missing: the
# crosswalk shipped 169 rows against the 172 crop directories the EarthStat
# tree contains (whep#877). These tests are the offline guard, and they need
# neither the EarthStat archive nor the pin -- both crosswalks are in the
# package.

# Read a producer crosswalk from `inst/extdata`, which ships in the built
# tarball (unlike `inst/scripts`, so nothing here has to source the producer).
.read_extdata_csv <- function(file_name) {
  path <- system.file("extdata", file_name, package = "whep")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata", file_name)
  }
  testthat::expect_true(file.exists(path))
  readr::read_csv(path, show_col_types = FALSE)
}

# The crops `cft_mapping.csv` spatializes that EarthStat simply does not
# publish a raster for. Listing them explicitly, and asserting the gap is
# *exactly* this set, is what makes the test load-bearing in both directions:
# a crop that loses its row fails here, and a crop that gains one has to be
# removed from the list.
.cft_codes_no_earthstat <- function() {
  tibble::tribble(
    ~item_prod_code, ~why,
    277L, "Jojoba seeds -- no EarthStat crop",
    305L, "Tallowtree seed -- no EarthStat crop",
    378L, "Cassava leaves -- EarthStat maps the root, not the leaf",
    407L, "Leeks and other alliaceous vegetables -- no EarthStat crop",
    689L, "Chillies and peppers, dry -- no EarthStat crop",
    839L, "Balata, gutta-percha and similar natural gums -- no EarthStat crop"
  )
}

testthat::test_that("every spatialized crop has an EarthStat raster row", {
  earthstat <- .read_extdata_csv("earthstat_mapping.csv")
  cft <- .read_extdata_csv("cft_mapping.csv")

  mapped <- unique(stats::na.omit(earthstat$item_prod_code))
  gap <- sort(setdiff(unique(cft$item_prod_code), mapped))
  known <- sort(.cft_codes_no_earthstat()$item_prod_code)

  testthat::expect_equal(gap, known)
})

testthat::test_that("barley, green corn and hempseed are in the crosswalk", {
  earthstat <- .read_extdata_csv("earthstat_mapping.csv")

  restored <- tibble::tribble(
    ~earthstat_name, ~item_prod_code,
    "barley",        44L,
    "greencorn",     446L,
    "hempseed",      336L
  )

  found <- earthstat |>
    dplyr::filter(earthstat_name %in% restored$earthstat_name) |>
    dplyr::select(earthstat_name, item_prod_code) |>
    dplyr::arrange(earthstat_name) |>
    dplyr::mutate(item_prod_code = as.integer(item_prod_code))

  testthat::expect_equal(
    found,
    dplyr::arrange(restored, earthstat_name)
  )
})

testthat::test_that("crosswalk codes and names agree with items_prod", {
  earthstat <- .read_extdata_csv("earthstat_mapping.csv")
  mapped <- dplyr::filter(earthstat, !is.na(item_prod_code))

  testthat::expect_equal(
    setdiff(mapped$item_prod_code, whep::items_prod$item_prod_code),
    numeric(0)
  )

  joined <- dplyr::inner_join(
    mapped,
    dplyr::select(whep::items_prod, item_prod_code, pkg_name = item_prod_name),
    by = "item_prod_code"
  )
  testthat::expect_equal(joined$item_prod_name, joined$pkg_name)
})

testthat::test_that("each EarthStat raster appears once in the crosswalk", {
  earthstat <- .read_extdata_csv("earthstat_mapping.csv")

  testthat::expect_equal(sum(duplicated(earthstat$earthstat_name)), 0L)
  testthat::expect_false(anyNA(earthstat$earthstat_name))
})
