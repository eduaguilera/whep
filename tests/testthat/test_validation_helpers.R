# Unit tests for the pure conservation-scoring helpers behind the
# inst/scripts validation figures (issues #260, #261).

test_that(".conservation_rel_error scores agreement and leaks correctly", {
  # Exact agreement scores 0.
  testthat::expect_equal(whep:::.conservation_rel_error(100, 100), 0)
  # A true zero-against-zero pair is legitimate agreement, not a failure.
  testthat::expect_equal(whep:::.conservation_rel_error(0, 0), 0)
  # Ordinary relative error.
  testthat::expect_equal(whep:::.conservation_rel_error(110, 100), 10)
})

test_that(".conservation_rel_error flags spurious gridded mass (#261)", {
  # Zero reference but nonzero gridded mass is a real disagreement and
  # must NOT score as a perfect match.
  err <- whep:::.conservation_rel_error(50, 0)
  testthat::expect_true(is.infinite(err))
  testthat::expect_false(err < 0.01)
})

test_that(".conservation_rel_error is vectorised", {
  testthat::expect_equal(
    whep:::.conservation_rel_error(c(50, 100, 0, 110), c(0, 100, 0, 100)),
    c(Inf, 0, 0, 10)
  )
})

test_that(".join_conservation keeps fully-leaked countries (#260)", {
  gridded <- tibble::tribble(
    ~area_code,
    ~grid_heads,
    1L,
    500,
    2L,
    300 # spurious gridded country absent from the reference
  )
  reference <- tibble::tribble(
    ~area_code,
    ~fao_heads,
    1L,
    500,
    3L,
    900 # reference country with zero gridded output (total leak)
  )

  out <- whep:::.join_conservation(
    gridded,
    reference,
    by = "area_code",
    fill = c("grid_heads", "fao_heads")
  )

  # An inner join would have dropped countries 2 and 3.
  testthat::expect_setequal(out$area_code, c(1L, 2L, 3L))

  leaked <- dplyr::filter(out, area_code == 3L)
  testthat::expect_equal(leaked$grid_heads, 0)
  testthat::expect_equal(leaked$fao_heads, 900)

  spurious <- dplyr::filter(out, area_code == 2L)
  testthat::expect_equal(spurious$fao_heads, 0)
})

test_that("join + scoring flags a total leak as a failure, not perfect", {
  gridded <- tibble::tribble(~area_code, ~grid_heads, 1L, 100)
  reference <- tibble::tribble(
    ~area_code,
    ~fao_heads,
    1L,
    100,
    3L,
    900
  )

  scored <- whep:::.join_conservation(
    gridded,
    reference,
    by = "area_code",
    fill = c("grid_heads", "fao_heads")
  ) |>
    dplyr::mutate(
      err = whep:::.conservation_rel_error(grid_heads, fao_heads)
    )

  leaked_err <- dplyr::filter(scored, area_code == 3L)$err
  testthat::expect_equal(leaked_err, 100)
  testthat::expect_false(leaked_err < 0.01)
})

# Fixtures for `.fabio_area_bridge()` (#264). Both stand in for real inputs
# the suite must not read: FABIO's published `regions.csv` (Zenodo record
# 2577067, columns `code`, `iso3c`) and the polity crosswalk. The four cases
# that matter are all here -- a code both sides share, FABIO's Sudan split
# against WHEP's single bucket 206, a WHEP area FABIO keeps inside Rest of
# World, and a FABIO composite region WHEP has no counterpart for.
.fabio_regions_fixture <- function() {
  tibble::tribble(
    ~code, ~name,                 ~iso3c,
    1L,    "Armenia",             "ARM",
    15L,   "Belgium-Luxembourg",  "BLX",
    276L,  "Sudan",               "SDN",
    277L,  "South Sudan",         "SSD",
    999L,  "RoW",                 "ROW"
  )
}

.fabio_crosswalk_fixture <- function() {
  tibble::tribble(
    ~area_code, ~area_iso3c, ~polity_area_code,
    1L,         "ARM",       1L,
    18L,        "BTN",       18L,
    206L,       "SDN",       206L,
    276L,       "SDN",       206L,
    277L,       "SSD",       206L,
    999L,       "ROW",       999L
  )
}

test_that(".fabio_area_bridge folds FABIO's Sudan split onto 206 (#264)", {
  bridge <- whep:::.fabio_area_bridge(
    .fabio_regions_fixture(),
    whep_area_codes = c(1L, 18L, 206L, 999L),
    crosswalk = .fabio_crosswalk_fixture()
  )

  sudan <- dplyr::filter(bridge, side == "fabio", area_code %in% c(276L, 277L))
  testthat::expect_equal(sudan$compare_area_code, c(206L, 206L))
  testthat::expect_equal(sudan$bridge_kind, c("fabio_fold", "fabio_fold"))

  # WHEP's own bucket keeps its code, so the three meet on one key.
  ours <- dplyr::filter(bridge, side == "whep", area_code == 206L)
  testthat::expect_equal(ours$compare_area_code, 206L)
  testthat::expect_equal(ours$bridge_kind, "direct")
})

test_that(".fabio_area_bridge pools a WHEP-only area into Rest of World", {
  bridge <- whep:::.fabio_area_bridge(
    .fabio_regions_fixture(),
    whep_area_codes = c(1L, 18L, 206L, 999L),
    crosswalk = .fabio_crosswalk_fixture()
  )

  # Bhutan reports under its own code in WHEP (#459) but sits inside FABIO's
  # single Rest-of-World row, so it must be compared there, not dropped.
  bhutan <- dplyr::filter(bridge, side == "whep", area_code == 18L)
  testthat::expect_equal(bhutan$compare_area_code, 999L)
  testthat::expect_equal(bhutan$bridge_kind, "pooled_into_row")
})

test_that(".fabio_area_bridge loses no code a raw join drops (#264)", {
  regions <- .fabio_regions_fixture()
  areas <- c(1L, 18L, 206L, 999L)
  bridge <- whep:::.fabio_area_bridge(
    regions,
    whep_area_codes = areas,
    crosswalk = .fabio_crosswalk_fixture()
  )

  # What the script did before: join the two code spaces raw. Both Sudan
  # codes, Belgium-Luxembourg and WHEP's 18 and 206 fall out unannounced.
  testthat::expect_setequal(
    setdiff(regions$code, areas),
    c(15L, 276L, 277L)
  )
  testthat::expect_setequal(setdiff(areas, regions$code), c(18L, 206L))

  # Under the bridge every WHEP area keeps a key, and every key it keeps is
  # one the FABIO side also carries. Nothing leaves without being named.
  ours <- dplyr::filter(bridge, side == "whep")
  theirs <- dplyr::filter(bridge, side == "fabio")
  testthat::expect_setequal(ours$area_code, areas)
  testthat::expect_false(anyNA(ours$compare_area_code))
  testthat::expect_true(all(
    ours$compare_area_code %in% theirs$compare_area_code
  ))
})

test_that(".fabio_area_bridge names a FABIO region WHEP cannot match", {
  bridge <- whep:::.fabio_area_bridge(
    .fabio_regions_fixture(),
    whep_area_codes = c(1L, 18L, 206L, 999L),
    crosswalk = .fabio_crosswalk_fixture()
  )

  unmatched <- dplyr::filter(bridge, bridge_kind == "unmatched")
  testthat::expect_equal(unmatched$side, "fabio")
  testthat::expect_equal(unmatched$area_code, 15L)
})

test_that(".fabio_area_bridge aborts on unusable inputs", {
  regions <- .fabio_regions_fixture()
  cw <- .fabio_crosswalk_fixture()

  testthat::expect_error(
    whep:::.fabio_area_bridge(
      dplyr::select(regions, !"iso3c"),
      whep_area_codes = 1L,
      crosswalk = cw
    ),
    "iso3c"
  )
  testthat::expect_error(
    whep:::.fabio_area_bridge(
      regions,
      whep_area_codes = integer(0),
      crosswalk = cw
    ),
    "at least one area"
  )
  testthat::expect_error(
    whep:::.fabio_area_bridge(
      dplyr::filter(regions, iso3c != "ROW"),
      whep_area_codes = 1L,
      crosswalk = cw
    ),
    "ROW"
  )
})
