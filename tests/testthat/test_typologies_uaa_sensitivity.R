# .calculate_uaa_area ---------------------------------------------------------

test_that(".calculate_uaa_area sums cropland, pasture/shrubland and dehesa only", {
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "A", "Cropland", 100,
    2000, "A", "Pasture_Shrubland", 50,
    2000, "A", "Dehesa", 25,
    2000, "A", "Forest_high", 1000,
    2000, "A", "Forest_low", 1000,
    2000, "A", "Other", 500
  )

  out <- .calculate_uaa_area(npp_ygpit)

  expect_equal(nrow(out), 1)
  expect_equal(out$Area_ha_uaa, 175)
})

test_that(".calculate_uaa_area keeps Year/Province_name groups separate", {
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "A", "Cropland", 100,
    2010, "A", "Cropland", 200,
    2000, "B", "Cropland", 300
  )

  out <- .calculate_uaa_area(npp_ygpit)

  expect_equal(nrow(out), 3)
  expect_equal(
    out$Area_ha_uaa[out$Year == 2000 & out$Province_name == "A"],
    100
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
    2000, "A", "Specialized cropping systems (intensive)",
    2000, "B", "Specialized livestock systems (extensive)"
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
    2000, "A", "Specialized cropping systems (intensive)",
    2000, "B", "Specialized cropping systems (intensive)"
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
    2000, "A", 1, 100, 5, 0.8, 40, 500,
    0.1, 0.1, 0.1, 0.1, 0.1,
    "Specialized cropping systems (intensive)",
    2000, "B", 1, 10, 50, 0.1, 40, 6000,
    0.4, 0.8, 0.1, 0.1, 0.1,
    "Specialized livestock systems (extensive)"
  )
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "A", "Cropland", 8000,
    2000, "A", "Forest_high", 2000,
    2000, "B", "Cropland", 4000,
    2000, "B", "Pasture_Shrubland", 2000,
    2000, "B", "Forest_high", 9000
  )

  out <- run_typology_area_sensitivity(npp_ygpit = npp_ygpit, baseline = baseline)

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
    2000, "B", 1, 10, 50, 0.1, 40, 6000,
    0.4, 0.8, 0.1, 0.1, 0.1,
    "Specialized livestock systems (extensive)"
  )
  npp_ygpit <- tibble::tribble(
    ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
    2000, "B", "Cropland", 4000,
    2000, "B", "Pasture_Shrubland", 2000,
    2000, "B", "Forest_high", 9000
  )

  out <- run_typology_area_sensitivity(npp_ygpit = npp_ygpit, baseline = baseline)

  julia_uaa <- out[out$source == "Julia" & out$area_basis == "uaa", ]
  julia_whole <- out[
    out$source == "Julia" & out$area_basis == "whole_province",
  ]
  # UAA density 1.0 clears Julia's 0.5; whole-province density 0.4 does not.
  expect_equal(julia_uaa$n_specialized_livestock, 1)
  expect_equal(julia_whole$n_specialized_livestock, 0)
})
