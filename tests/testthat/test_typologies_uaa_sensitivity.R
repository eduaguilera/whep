# .calculate_uaa_area ---------------------------------------------------------

test_that(".calculate_uaa_area sums cropland, pasture/shrubland and dehesa only", {
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "Huesca", "Cropland", 100,
    2000, "Huesca", "Pasture_Shrubland", 50,
    2000, "Huesca", "Dehesa", 25,
    2000, "Huesca", "Forest_high", 1000,
    2000, "Huesca", "Forest_low", 1000,
    2000, "Huesca", "Other", 500
  )

  out <- .calculate_uaa_area(npp_ygpit)

  expect_equal(nrow(out), 1)
  expect_equal(out$Area_ha_uaa, 175)
})

test_that(".calculate_uaa_area keeps Year/Province_name groups separate", {
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "Huesca", "Cropland", 100,
    2010, "Huesca", "Cropland", 200,
    2000, "Zaragoza", "Cropland", 300
  )

  out <- .calculate_uaa_area(npp_ygpit)

  expect_equal(nrow(out), 3)
  expect_equal(
    sort(out$Area_ha_uaa[out$Year == 2000]),
    c(100, 300)
  )
})


# .literature_livestock_sources ------------------------------------------------

test_that(".literature_livestock_sources returns Julia's and Josette's published thresholds", {
  out <- .literature_livestock_sources()

  expect_named(out, c("source", "uaa_threshold"))
  expect_equal(nrow(out), 2)
  expect_equal(out$uaa_threshold[out$source == "Julia"], 0.5)
  expect_equal(out$uaa_threshold[out$source == "Josette"], 1)
})


# .compute_uaa_agreement -------------------------------------------------------

test_that(".compute_uaa_agreement reports 100% for an unchanged classification", {
  baseline <- tibble::tribble(
    ~year, ~province_name, ~Typology_base,
    2000, "Huesca", "Specialized cropping systems (intensive)",
    2000, "Zaragoza", "Specialized livestock systems (extensive)"
  )

  out <- .compute_uaa_agreement(baseline, baseline, "Julia", 0.5, "uaa")

  expect_equal(out$agreement_pct, 100)
  expect_equal(out$n_specialized_livestock, 1)
  expect_equal(out$source, "Julia")
  expect_equal(out$uaa_threshold, 0.5)
  expect_equal(out$area_basis, "uaa")
})

test_that(".compute_uaa_agreement counts specialized-livestock province-years in the reclassified result, not the baseline", {
  baseline <- tibble::tribble(
    ~year, ~province_name, ~Typology_base,
    2000, "Huesca", "Specialized cropping systems (intensive)",
    2000, "Zaragoza", "Specialized cropping systems (intensive)"
  )
  reclassified <- baseline |>
    dplyr::mutate(
      Typology_base = c(
        "Specialized livestock systems (intensive)",
        "Specialized cropping systems (intensive)"
      )
    )

  out <- .compute_uaa_agreement(
    baseline,
    reclassified,
    "Josette",
    1,
    "whole_province"
  )

  expect_equal(out$agreement_pct, 50)
  expect_equal(out$n_specialized_livestock, 1)
  expect_equal(out$area_basis, "whole_province")
})


# run_typology_area_sensitivity ------------------------------------------------

test_that("run_typology_area_sensitivity returns one row per source/area-basis combination", {
  baseline <- tibble::tribble(
    ~year, ~province_name, ~production_seminatural, ~production_crops,
    ~animal_ingestion, ~synthetic_share, ~crop_productivity, ~LU_total,
    ~Livestock_density, ~imported_feed_share, ~feed_from_seminatural_share,
    ~local_feed_share, ~Manure_share, ~Typology_base,
    2000, "Huesca", 1, 100, 5, 0.8, 40, 500,
    0.1, 0.1, 0.1, 0.1, 0.1,
    "Specialized cropping systems (intensive)",
    2000, "Zaragoza", 1, 10, 50, 0.1, 40, 6000,
    0.4, 0.8, 0.1, 0.1, 0.1,
    "Specialized livestock systems (extensive)"
  )
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "Huesca", "Cropland", 8000,
    2000, "Huesca", "Forest_high", 2000,
    2000, "Zaragoza", "Cropland", 4000,
    2000, "Zaragoza", "Pasture_Shrubland", 2000,
    2000, "Zaragoza", "Forest_high", 9000
  )

  out <- run_typology_area_sensitivity(
    npp_ygpit = npp_ygpit,
    baseline = baseline
  )

  expect_equal(nrow(out), 4)
  expect_setequal(out$source, c("Julia", "Josette"))
  expect_setequal(out$area_basis, c("whole_province", "uaa"))
  expect_named(
    out,
    c(
      "source",
      "uaa_threshold",
      "area_basis",
      "agreement_pct",
      "n_specialized_livestock"
    )
  )
})

test_that("run_typology_area_sensitivity's whole-province basis under-counts specialized livestock relative to UAA", {
  # Province B: LU_total 6000, whole-province Livestock_density 0.4 (given),
  # but UAA area (Cropland 4000 + Pasture_Shrubland 2000 = 6000, excluding
  # the 9000 ha of Forest_high) gives a UAA density of 1.0. Julia's
  # threshold (0.5) sits between the two: cleared by the UAA density but not
  # by the whole-province one, so this is exactly the case the two bases are
  # expected to disagree on.
  baseline <- tibble::tribble(
    ~year, ~province_name, ~production_seminatural, ~production_crops,
    ~animal_ingestion, ~synthetic_share, ~crop_productivity, ~LU_total,
    ~Livestock_density, ~imported_feed_share, ~feed_from_seminatural_share,
    ~local_feed_share, ~Manure_share, ~Typology_base,
    2000, "Zaragoza", 1, 10, 50, 0.1, 40, 6000,
    0.4, 0.8, 0.1, 0.1, 0.1,
    "Specialized livestock systems (extensive)"
  )
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "Zaragoza", "Cropland", 4000,
    2000, "Zaragoza", "Pasture_Shrubland", 2000,
    2000, "Zaragoza", "Forest_high", 9000
  )

  out <- run_typology_area_sensitivity(
    npp_ygpit = npp_ygpit,
    baseline = baseline
  )

  julia_uaa <- out[out$source == "Julia" & out$area_basis == "uaa", ]
  julia_whole <- out[
    out$source == "Julia" & out$area_basis == "whole_province",
  ]
  # UAA density 1.0 clears Julia's 0.5; whole-province density 0.4 does not.
  expect_equal(julia_uaa$n_specialized_livestock, 1)
  expect_equal(julia_whole$n_specialized_livestock, 0)
})

.uaa_alias_fixture <- function() {
  list(
    baseline = tibble::tribble(
      ~year, ~province_name, ~production_seminatural, ~production_crops,
      ~animal_ingestion, ~synthetic_share, ~crop_productivity, ~LU_total,
      ~Livestock_density, ~imported_feed_share, ~feed_from_seminatural_share,
      ~local_feed_share, ~Manure_share, ~Typology_base,
      2020, "Alicante", 1, 10, 50, 0.1, 40, 6000,
      0.4, 0.8, 0.1, 0.1, 0.1,
      "Specialized livestock systems (extensive)"
    ),
    npp = tibble::tribble(
      ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
      2020, "Alicante", "Cropland", 4000,
      2020, "Alicante", "Pasture_Shrubland", 2000,
      2020, "Alicante", "Forest_high", 9000
    )
  )
}

test_that("area sensitivity is invariant to equivalent province labels", {
  x <- .uaa_alias_fixture()
  expected <- whep::run_typology_area_sensitivity(
    baseline = x$baseline,
    npp_ygpit = x$npp
  )
  alias <- x$npp |>
    dplyr::mutate(Province_name = "Alicante/Alacant (province of Spain)")
  actual <- whep::run_typology_area_sensitivity(
    baseline = x$baseline,
    npp_ygpit = alias
  )
  expect_equal(actual, expected)
  expect_equal(
    actual |>
      dplyr::filter(source == "Julia", area_basis == "uaa") |>
      dplyr::pull(n_specialized_livestock),
    1
  )
  result <- x$baseline |>
    dplyr::mutate(province_name = "Alicante/Alacant (province of Spain)")
  agreement <- .compute_uaa_agreement(
    x$baseline,
    result,
    "Julia",
    0.5,
    "uaa"
  )
  expect_equal(agreement$agreement_pct, 100)
  expect_equal(agreement$n_specialized_livestock, 1)
})

test_that("area sensitivity rejects duplicate dated identities", {
  x <- .uaa_alias_fixture()
  alias <- x$baseline |>
    dplyr::mutate(province_name = "Alicante/Alacant (province of Spain)")
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = dplyr::bind_rows(x$baseline, alias),
      npp_ygpit = x$npp
    ),
    class = "whep_uaa_duplicate_identity"
  )
  duplicate_area <- dplyr::bind_rows(
    x$npp,
    x$npp |>
      dplyr::mutate(Province_name = "Alicante/Alacant (province of Spain)")
  )
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = duplicate_area
    ),
    class = "whep_uaa_duplicate_identity"
  )
  expect_error(
    .compute_uaa_agreement(
      x$baseline,
      dplyr::bind_rows(x$baseline, alias),
      "Julia",
      0.5,
      "uaa"
    ),
    class = "whep_uaa_duplicate_identity"
  )
})

test_that("area sensitivity refuses incomplete or invalid UAA support", {
  x <- .uaa_alias_fixture()
  missing <- x$npp |> dplyr::filter(LandUse == "Forest_high")
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = missing
    ),
    class = "whep_uaa_missing_support"
  )
  purrr::walk(c(NA_real_, Inf, -1, 0), function(value) {
    invalid <- x$npp |> dplyr::mutate(Area_ygpit_ha = value)
    expect_error(
      whep::run_typology_area_sensitivity(
        baseline = x$baseline,
        npp_ygpit = invalid
      ),
      class = "whep_uaa_missing_support"
    )
  })
  wrong_year <- x$npp |> dplyr::mutate(Year = 2019)
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = wrong_year
    ),
    class = "whep_uaa_missing_support"
  )
  expect_error(
    .compute_uaa_agreement(
      x$baseline,
      x$baseline[0, ],
      "Julia",
      0.5,
      "uaa"
    ),
    class = "whep_uaa_missing_support"
  )
})

test_that("area sensitivity exposes unresolved historical identity", {
  x <- .uaa_alias_fixture()
  x$baseline <- x$baseline |>
    dplyr::mutate(year = 1900, province_name = "Tenerife")
  x$npp <- x$npp |>
    dplyr::mutate(Year = 1900, Province_name = "Tenerife")
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = x$npp
    ),
    class = "whep_uaa_unresolved_identity"
  )
})

test_that("UAA panels cannot silently classify empty or unlabelled support", {
  x <- .uaa_alias_fixture()
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline[0, ],
      npp_ygpit = x$npp
    ),
    class = "whep_uaa_missing_support"
  )
  unlabelled <- x$npp |> dplyr::mutate(LandUse = NA_character_)
  expect_error(
    .calculate_uaa_area(unlabelled),
    class = "whep_uaa_missing_support"
  )
  invalid <- x$baseline |> dplyr::mutate(Typology_base = "")
  expect_error(
    .compute_uaa_agreement(invalid, invalid, "Julia", 0.5, "uaa"),
    class = "whep_uaa_missing_support"
  )
})

test_that("UAA classification retains dated coverage and input errors", {
  x <- .uaa_alias_fixture()
  invalid_year <- x$npp |> dplyr::mutate(Year = 2020.5)
  expect_error(
    .calculate_uaa_area(invalid_year),
    class = "whep_grafs_geography"
  )
  expect_error(
    .calculate_uaa_area(dplyr::select(x$npp, -LandUse)),
    class = "whep_uaa_missing_support"
  )
  invalid <- x$baseline |> dplyr::mutate(LU_total = Inf)
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = invalid,
      npp_ygpit = x$npp
    ),
    class = "whep_uaa_missing_support"
  )
  result <- x$baseline |> dplyr::mutate(Typology_base = NA_character_)
  expect_error(
    .compute_uaa_agreement(x$baseline, result, "Julia", 0.5, "uaa"),
    class = "whep_uaa_missing_support"
  )
})

test_that("UAA ingestion does not overwrite contradictory supplied codes", {
  x <- .uaa_alias_fixture()
  invalid <- x$baseline |>
    dplyr::mutate(reporting_polity_code = "ESP-28-1833-2025")
  expect_error(
    whep::run_typology_area_sensitivity(
      baseline = invalid,
      npp_ygpit = x$npp
    ),
    class = "whep_uaa_unresolved_identity"
  )
})

test_that("UAA sensitivity resolves only the requested panel years", {
  x <- .uaa_alias_fixture()
  historical <- x$npp |>
    dplyr::mutate(Year = 1900, Province_name = "Tenerife")
  expect_equal(
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = dplyr::bind_rows(x$npp, historical)
    ),
    whep::run_typology_area_sensitivity(
      baseline = x$baseline,
      npp_ygpit = x$npp
    )
  )
})
