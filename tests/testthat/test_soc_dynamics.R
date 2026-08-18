# Tests for the soil-organic-carbon dynamics selector (Module B, Task B5):
# calculate_soc_dynamics. Checks the default model, dispatch to a named model,
# the uniform long output schema shared by all five models, the method_soc
# stamp, invalid-model validation, and that the native climate modifier is
# applied only when the model's climate drivers are present.

test_that("the default model is hsoc and is stamped on every row", {
  out <- whep::calculate_soc_dynamics(
    data = list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
  )
  testthat::expect_true(all(out$method_soc == "hsoc"))
  testthat::expect_setequal(unique(out$pool), c("fresh", "humus", "iom"))
})

test_that("model = icbm dispatches and returns soc_total", {
  out <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
  )
  testthat::expect_true(rlang::has_name(out, "soc_total"))
  testthat::expect_true(all(out$method_soc == "icbm"))
  # Two ICBM pools (young, old) over six years, long.
  testthat::expect_equal(nrow(out), 12L)
})

test_that("all five models return exactly the same columns", {
  # Regression test for #350: hsoc (the default) returned long rows with no
  # soc_total while rothc/icbm/amg/century returned wide per-model pool
  # columns, so every caller had to branch on the model. The selector now
  # reshapes whichever model ran to one long schema.
  testthat::skip_if_not_installed("deSolve")
  models <- c("hsoc", "rothc", "icbm", "amg", "century")
  runs <- purrr::map(
    models,
    \(m) {
      whep::calculate_soc_dynamics(
        model = m,
        data = list(
          initial_soc_mgc_ha = 50,
          c_input_mgc_ha_yr = 2,
          years = 5,
          clay_pct = 20
        )
      )
    }
  ) |>
    purrr::set_names(models)
  schema <- c("year", "pool", "stock_mgc_ha", "soc_total", "method_soc")
  purrr::walk(runs, \(out) testthat::expect_named(out, schema))
  # Being long in the pool dimension, the five runs stack into one tidy frame
  # instead of a ragged one with mutually exclusive pool columns.
  stacked <- dplyr::bind_rows(runs)
  testthat::expect_named(stacked, schema)
  testthat::expect_setequal(unique(stacked$method_soc), models)
  testthat::expect_equal(nrow(stacked), sum(purrr::map_int(runs, nrow)))
})

test_that("soc_total is the year's pool sum for every model", {
  testthat::skip_if_not_installed("deSolve")
  models <- c("hsoc", "rothc", "icbm", "amg", "century")
  purrr::walk(models, \(m) {
    out <- whep::calculate_soc_dynamics(
      model = m,
      data = list(
        initial_soc_mgc_ha = 50,
        c_input_mgc_ha_yr = 2,
        years = 5,
        clay_pct = 20
      )
    )
    per_year <- out |>
      dplyr::summarise(
        pool_sum = sum(stock_mgc_ha),
        n_totals = dplyr::n_distinct(soc_total),
        reported = dplyr::first(soc_total),
        .by = year
      )
    # One total per year, repeated on each pool row, equal to the pool sum, so
    # a total-only caller needs distinct(year, soc_total) and no pivot.
    testthat::expect_true(all(per_year$n_totals == 1L))
    testthat::expect_equal(per_year$pool_sum, per_year$reported)
  })
})

test_that("an invalid model aborts mentioning the model argument", {
  testthat::expect_error(
    whep::calculate_soc_dynamics(
      model = "bogus",
      data = list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
    ),
    regexp = "model",
    class = "rlang_error"
  )
})

test_that("climate drivers in data change the trajectory; absence runs at 1", {
  # ICBM drivers: temp_c, theta, t_field, t_wilt, porosity. Supplying them
  # builds a native climate modifier != 1, which moves the old-pool stock away
  # from the modifier-1 run.
  base_data <- list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 20)
  plain <- whep::calculate_soc_dynamics(model = "icbm", data = base_data)
  with_climate <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = c(
      base_data,
      list(
        temp_c = c(5, 15, 25),
        theta = c(0.20, 0.25, 0.30),
        t_field = 0.30,
        t_wilt = 0.10,
        porosity = 0.45
      )
    )
  )
  modifier <- whep::soc_rate_modifier_icbm(
    temp_c = c(5, 15, 25),
    theta = c(0.20, 0.25, 0.30),
    t_field = 0.30,
    t_wilt = 0.10,
    porosity = 0.45
  )
  testthat::expect_gt(abs(modifier - 1), 1e-6)
  testthat::expect_false(
    isTRUE(all.equal(
      utils::tail(plain$soc_total, 1),
      utils::tail(with_climate$soc_total, 1)
    ))
  )
})

test_that("the example fixture returns a tibble stamped icbm", {
  out <- whep::calculate_soc_dynamics(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(all(out$method_soc == "icbm"))
  # The fixture carries the same uniform schema as a real run.
  testthat::expect_named(
    out,
    c("year", "pool", "stock_mgc_ha", "soc_total", "method_soc")
  )
})

test_that("model = rothc dispatches with a non-neutral native modifier", {
  base_data <- list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
  out <- whep::calculate_soc_dynamics(
    model = "rothc",
    data = c(
      base_data,
      list(
        temp_c = c(5, 15, 25),
        water_minus_pet_mm = c(-30, 10, 40),
        clay_pct = 20,
        soil_cover = 0.5
      )
    )
  )
  modifier <- whep::soc_rate_modifier_rothc(
    temp_c = c(5, 15, 25),
    water_minus_pet_mm = c(-30, 10, 40),
    clay_pct = 20,
    soil_cover = 0.5
  )
  testthat::expect_gt(abs(modifier - 1), 1e-6)
  testthat::expect_true(all(out$method_soc == "rothc"))
  testthat::expect_true(all(is.finite(out$soc_total)))
})

test_that("model = amg dispatches with a non-neutral native modifier", {
  base_data <- list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
  out <- whep::calculate_soc_dynamics(
    model = "amg",
    data = c(
      base_data,
      list(temp_c = c(8, 15, 22), water_balance_mm = c(-100, 0, 200))
    )
  )
  modifier <- whep::soc_rate_modifier_amg(
    temp_c = c(8, 15, 22),
    water_balance_mm = c(-100, 0, 200)
  )
  testthat::expect_gt(abs(modifier - 1), 1e-6)
  testthat::expect_true(all(out$method_soc == "amg"))
  testthat::expect_true(all(is.finite(out$soc_total)))
})

test_that("model = century dispatches with a non-neutral native modifier", {
  base_data <- list(
    initial_soc_mgc_ha = 50,
    c_input_mgc_ha_yr = 2,
    years = 5,
    clay_pct = 20
  )
  out <- whep::calculate_soc_dynamics(
    model = "century",
    data = c(
      base_data,
      list(
        temp_c = c(15, 25, 35),
        precip_mm = c(40, 60, 80),
        pet_mm = c(50, 70, 90)
      )
    )
  )
  modifier <- whep::soc_rate_modifier_century(
    temp_c = c(15, 25, 35),
    precip_mm = c(40, 60, 80),
    pet_mm = c(50, 70, 90)
  )
  testthat::expect_gt(abs(modifier - 1), 1e-6)
  testthat::expect_true(all(out$method_soc == "century"))
  testthat::expect_true(all(is.finite(out$soc_total)))
})

test_that("an already-supplied climate_modifier is honoured when raw drivers are absent", {
  # Regression test: a caller (e.g. the carbon-balance equilibrium spin-up)
  # may compute the model-native modifier itself from real climate data and
  # pass only the resulting scalar, with none of the raw driver columns
  # (temp_c, water_minus_pet_mm, ...) present. calculate_soc_dynamics() must
  # honour that value rather than silently forcing it back to a neutral 1.
  base_data <- list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 20)
  neutral <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = base_data
  )
  supplied <- whep::calculate_soc_dynamics(
    model = "icbm",
    data = c(base_data, list(climate_modifier = 0.5))
  )
  testthat::expect_false(
    isTRUE(all.equal(
      utils::tail(neutral$soc_total, 1),
      utils::tail(supplied$soc_total, 1)
    ))
  )
  # A halved modifier halves the decay+input rate; the analytic ICBM old-pool
  # steady state h*i/k_O scales linearly with the modifier's effect on k_O.
  no_mod <- whep:::.soc_dispatch("icbm", base_data, climate_modifier = 1)
  half_mod <- whep:::.soc_dispatch("icbm", base_data, climate_modifier = 0.5)
  testthat::expect_equal(
    utils::tail(supplied$soc_total, 1),
    utils::tail(half_mod$soc_total, 1),
    tolerance = 1e-9
  )
  testthat::expect_equal(
    utils::tail(neutral$soc_total, 1),
    utils::tail(no_mod$soc_total, 1),
    tolerance = 1e-9
  )
})
