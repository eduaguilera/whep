# Smoke tests for the MIRCA irrigation national-cap helper added in
# inst/scripts/prepare_spatialize_all.R. The helper lives at script scope
# (not package R/) so we source the script once and exercise it offline.

.source_prepare_spatialize()


test_that(".cap_national_irrigation caps summed irrigation at the total", {
  .need_spatialize_helper(".cap_national_irrigation")
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
  .need_spatialize_helper(".cap_national_irrigation")
  crop_areas <- tibble::tribble(
    ~year, ~area_code, ~irrigated_area_ha, ~total_irrig_ha,
    2000L, 2L, 300, 1000,
    2000L, 2L, 200, 1000
  )
  out <- .cap_national_irrigation(crop_areas)
  expect_equal(out$irrigated_area_ha, c(300, 200))
})

test_that(".cap_national_irrigation caps each country-year independently", {
  .need_spatialize_helper(".cap_national_irrigation")
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

test_that(".check_mirca_covers_mapping aborts on a code MIRCA never saw", {
  .need_spatialize_helper(".check_mirca_covers_mapping")
  # A MIRCA table built before 248 Coconuts was mapped (whep#1292).
  mirca <- tibble::tribble(
    ~area_code, ~item_prod_code, ~irrig_frac,
    1L, 249L, 0.1,
    1L, 27L, 0.5
  )
  stale <- tibble::tibble(item_prod_code = c(27L, 248L))
  expect_error(.check_mirca_covers_mapping(mirca, stale), "248")
})

test_that(".check_mirca_covers_mapping passes a code absent in some countries", {
  .need_spatialize_helper(".check_mirca_covers_mapping")
  # Country 2 has no row for 27: a coverage gap the fallback is for.
  mirca <- tibble::tribble(
    ~area_code, ~item_prod_code, ~irrig_frac,
    1L, 248L, 0.1,
    1L, 27L, 0.5,
    2L, 248L, 0.2
  )
  current <- tibble::tibble(item_prod_code = c(27L, 248L))
  expect_identical(.check_mirca_covers_mapping(mirca, current), mirca)
})

test_that(".fallback_irrigation uses the type's density over all its crops", {
  .need_spatialize_helper(".fallback_irrigation")
  # Crop 1 is MIRCA-covered (900 ha), crop 2 is absent (100 ha), same type.
  # The type irrigates 500 ha: density 0.5. Sharing it over the absent crop
  # alone (the old base) would hand crop 2 all 500 ha, more than it harvests.
  crop_areas <- tibble::tribble(
    ~year, ~area_code, ~luh2_type, ~harvested_area_ha,
    2000L, 1L, "c3ann", 900,
    2000L, 1L, "c3ann", 100,
    2000L, 1L, "c4ann", 50
  )
  luh2_irrig <- tibble::tribble(
    ~year, ~area_code, ~luh2_type, ~irrig_ha,
    2000L, 1L, "c3ann", 500
  )
  out <- .fallback_irrigation(crop_areas, luh2_irrig)
  expect_equal(out, c(450, 50, 0), tolerance = 1e-9)
  expect_lte(out[2], crop_areas$harvested_area_ha[2])
})
