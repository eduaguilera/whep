test_that("soc_rate_modifier_rothc matches a hand-computed value at 15 C", {
  # Single month, temp 15 C, positive water surplus (so TSMD stays 0, b = 1),
  # bare soil (cover = 0, so c = 1). a = 47.91 / (1 + exp(106.06 / 33.27)).
  a <- 47.91 / (1 + exp(106.06 / (15 + 18.27)))
  out <- whep::soc_rate_modifier_rothc(
    temp_c = 15,
    water_minus_pet_mm = 20,
    clay_pct = 20,
    soil_cover = 0
  )
  testthat::expect_equal(out, a)
  testthat::expect_equal(out, 1.898446, tolerance = 1e-5)
})

test_that("soc_rate_modifier_rothc cover and deficit lower the modifier", {
  covered <- whep::soc_rate_modifier_rothc(
    temp_c = 15,
    water_minus_pet_mm = 20,
    clay_pct = 20,
    soil_cover = 1
  )
  bare <- whep::soc_rate_modifier_rothc(
    temp_c = 15,
    water_minus_pet_mm = 20,
    clay_pct = 20,
    soil_cover = 0
  )
  # c = 0.6 when fully covered, 1.0 when bare.
  testthat::expect_equal(covered / bare, 0.6, tolerance = 1e-8)
  # A sustained moisture deficit drives b below 1, lowering abc.
  dry <- whep::soc_rate_modifier_rothc(
    temp_c = rep(15, 6),
    water_minus_pet_mm = rep(-60, 6),
    clay_pct = 20,
    soil_cover = 0
  )
  testthat::expect_lt(dry, bare)
})

test_that("soc_rate_modifier_icbm re_temp equals 1 at 30 C", {
  # Normalization property (reclim 30-degree anchor): with theta in the plateau
  # and temp at 30 C, re = re_wat * re_temp / 0.1056855 reduces to re_temp = 1
  # divided by the normalizer when re_wat = 1.
  out <- whep::soc_rate_modifier_icbm(
    temp_c = 30,
    theta = 0.30,
    t_field = 0.30,
    t_wilt = 0.10,
    porosity = 0.45
  )
  testthat::expect_equal(out, 1 / 0.1056855, tolerance = 1e-6)
})

test_that("soc_rate_modifier_icbm zeroes below Tmin", {
  out <- whep::soc_rate_modifier_icbm(
    temp_c = -5,
    theta = 0.30,
    t_field = 0.30,
    t_wilt = 0.10,
    porosity = 0.45
  )
  testthat::expect_equal(out, 0)
})

test_that("soc_rate_modifier_amg f_t equals 1 at 15 C", {
  # Normalization property: f(T) is normalized so f(T_Ref = 15) = 1. With the
  # zero water-balance moisture term factored out, the modifier at 15 C is
  # f_h(0) = 1 / (1 + 0.03). The temperature factor alone must be exactly 1.
  out <- whep::soc_rate_modifier_amg(temp_c = 15, water_balance_mm = 0)
  f_h0 <- 1 / (1 + 0.03)
  testthat::expect_equal(out / f_h0, 1, tolerance = 1e-6)
})

test_that("soc_rate_modifier_amg zeroes below 0 C", {
  out <- whep::soc_rate_modifier_amg(temp_c = -2, water_balance_mm = 100)
  testthat::expect_equal(out, 0)
})

test_that("soc_rate_modifier_century t_factor equals 1 at 35 C", {
  # Normalization property: the Poisson temperature factor peaks at 1 at the
  # 35-degree optimum. Pick precip = pet so the moisture logistic is constant,
  # then divide it out.
  out <- whep::soc_rate_modifier_century(
    temp_c = 35,
    precip_mm = 50,
    pet_mm = 50
  )
  w <- 1 / (1 + 30 * exp(-8.5))
  testthat::expect_equal(out / w, 1, tolerance = 1e-8)
})

test_that("soc_rate_modifier_century moisture rises with precip", {
  wet <- whep::soc_rate_modifier_century(
    temp_c = 35,
    precip_mm = 100,
    pet_mm = 50
  )
  dry <- whep::soc_rate_modifier_century(
    temp_c = 35,
    precip_mm = 20,
    pet_mm = 50
  )
  testthat::expect_gt(wet, dry)
})
