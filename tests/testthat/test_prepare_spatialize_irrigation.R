# Smoke tests for the MIRCA irrigation national-cap helper added in
# inst/scripts/prepare_spatialize_all.R. The helper lives at script scope
# (not package R/) so we source the script once and exercise it offline.

local({
  pkg_root <- testthat::test_path("..", "..")
  script_path <- file.path(
    pkg_root,
    "inst",
    "scripts",
    "prepare_spatialize_all.R"
  )
  if (file.exists(script_path)) {
    sys.source(script_path, envir = topenv())
  }
})


test_that(".cap_national_irrigation caps summed irrigation at the total", {
  skip_if_not(exists(".cap_national_irrigation", mode = "function"))
  # MIRCA crops already absorb the whole 1000-ha national total; a MIRCA-absent
  # crop then received an extra 400 ha from the per-CFT fallback -> 1400 total.
  crop_areas <- tibble::tribble(
    ~year, ~area_code, ~irrigated_area_ha, ~total_irrig_ha,
    2000L, 1L, 1000, 1000,
    2000L, 1L, 400, 1000
  )
  out <- .cap_national_irrigation(crop_areas)
  expect_equal(sum(out$irrigated_area_ha), 1000, tolerance = 1e-9)
  # scaling is proportional: 1000/1400 and 400/1400 of the total
  expect_equal(
    out$irrigated_area_ha,
    c(1000, 400) * 1000 / 1400,
    tolerance = 1e-9
  )
})

test_that(".cap_national_irrigation leaves within-budget countries untouched", {
  skip_if_not(exists(".cap_national_irrigation", mode = "function"))
  crop_areas <- tibble::tribble(
    ~year, ~area_code, ~irrigated_area_ha, ~total_irrig_ha,
    2000L, 2L, 300, 1000,
    2000L, 2L, 200, 1000
  )
  out <- .cap_national_irrigation(crop_areas)
  expect_equal(out$irrigated_area_ha, c(300, 200))
})

test_that(".cap_national_irrigation caps each country-year independently", {
  skip_if_not(exists(".cap_national_irrigation", mode = "function"))
  crop_areas <- tibble::tribble(
    ~year, ~area_code, ~irrigated_area_ha, ~total_irrig_ha,
    2000L, 1L, 1000, 1000, # over budget -> scaled
    2000L, 1L, 1000, 1000,
    2000L, 2L, 100, 1000 # under budget -> untouched
  )
  out <- .cap_national_irrigation(crop_areas)
  by_country <- tapply(out$irrigated_area_ha, out$area_code, sum)
  expect_equal(unname(by_country[["1"]]), 1000, tolerance = 1e-9)
  expect_equal(unname(by_country[["2"]]), 100, tolerance = 1e-9)
})
