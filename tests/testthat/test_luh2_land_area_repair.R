test_that(".fix_luh2_crop_collapse interpolates isolated crop collapses", {
  land_areas <- tibble::tribble(
    ~area_code, ~area, ~year, ~Land_Use, ~Area_Mha,
    109L, "Jamaica", 1899L, "c3ann", 0.010,
    109L, "Jamaica", 1900L, "c3ann", 1e-10,
    109L, "Jamaica", 1901L, "c3ann", 0.012,
    109L, "Jamaica", 1899L, "c3per", 0.030,
    109L, "Jamaica", 1900L, "c3per", 1e-10,
    109L, "Jamaica", 1901L, "c3per", 0.034,
    109L, "Jamaica", 1899L, "pastr", 0.080,
    109L, "Jamaica", 1900L, "pastr", 0.081,
    109L, "Jamaica", 1901L, "pastr", 0.082
  )

  out <- .fix_luh2_crop_collapse(land_areas)

  expect_equal(
    out$Area_Mha[out$year == 1900L & out$Land_Use == "c3ann"],
    0.011,
    tolerance = 1e-12
  )
  expect_equal(
    out$Area_Mha[out$year == 1900L & out$Land_Use == "c3per"],
    0.032,
    tolerance = 1e-12
  )
  expect_equal(
    out$Area_Mha[out$year == 1900L & out$Land_Use == "pastr"],
    0.081,
    tolerance = 1e-12
  )
})
