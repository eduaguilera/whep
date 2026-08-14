testthat::test_that("n_boundary_params carries the locked boundary constants", {
  p <- whep::n_boundary_params
  pointblank::expect_col_exists(
    p,
    c("parameter", "value", "unit", "description")
  )
  val <- function(nm) p$value[p$parameter == nm]
  testthat::expect_equal(val("boundary_low"), 60)
  testthat::expect_equal(val("boundary_high"), 125)
  testthat::expect_equal(val("boundary_top"), 205)
  testthat::expect_equal(val("per_capita_cap"), 40)
  testthat::expect_equal(val("food_agri_share"), 0.95)
  testthat::expect_equal(val("syn_tot_agri_ratio"), (109 + 33) / (0.85 * 109))
})

testthat::test_that("nourishment_thresholds applies the 1.35 waste factor", {
  n <- whep::nourishment_thresholds
  pointblank::expect_col_exists(
    n,
    c("metric", "bound", "value", "unit", "provenance")
  )
  v <- function(m, b) n$value[n$metric == m & n$bound == b]
  testthat::expect_equal(v("protein_raw", "floor"), 46)
  testthat::expect_equal(v("protein_raw", "ceiling"), 63)
  testthat::expect_equal(v("protein", "floor"), 62.1)
  testthat::expect_equal(v("protein", "ceiling"), 85.05)
  testthat::expect_equal(v("energy", "floor"), 2300)
  testthat::expect_equal(v("energy", "ceiling"), 2900)
  testthat::expect_equal(v("waste_inequality", "factor"), 1.35)
  testthat::expect_equal(v("class", "under"), 1)
  testthat::expect_equal(v("class", "over"), 2)
})

testthat::test_that("the upper bound is named ceiling, not target", {
  # normalize_nourishment() uses it as the top of the Adequate band, above
  # which a country is classified Over. "target" read as something to aim at,
  # which is the opposite (whep#753).
  n <- whep::nourishment_thresholds
  testthat::expect_false("target" %in% n$bound)
  testthat::expect_setequal(
    n$bound[n$metric %in% c("protein_raw", "protein", "energy")],
    c("floor", "ceiling")
  )
})

testthat::test_that("every shipped threshold declares its provenance", {
  # The point of the column: a number with no source must not be able to look
  # like one that has a source. Only the protein floor is cited.
  n <- whep::nourishment_thresholds
  pointblank::expect_col_vals_in_set(
    n,
    columns = "provenance",
    set = c(
      "trs935_table46_55kg_safe_level",
      "derived_raw_times_waste_inequality",
      "inherited_unsourced",
      "definition"
    )
  )
  cited <- n$provenance == "trs935_table46_55kg_safe_level"
  testthat::expect_equal(n$value[cited], 46)
  # The 1.35 factor and everything built only on it stay labelled: whep#753
  # established the author had no source for it.
  unsourced <- n[n$provenance == "inherited_unsourced", ]
  testthat::expect_setequal(unsourced$value, c(63, 2300, 2900, 1.35))
})

testthat::test_that("sjos/nourish level tables are ordered and coloured", {
  s <- whep::sjos_levels
  nl <- whep::nourish_levels
  pointblank::expect_col_exists(s, c("level", "order", "colour"))
  pointblank::expect_col_exists(nl, c("level", "order", "colour"))
  # 2-way boundary axis crossed with the 3-way nourishment axis.
  testthat::expect_equal(
    s$level,
    c(
      "Within_boundary Under",
      "Within_boundary Adequate",
      "Within_boundary Over",
      "Exceedance Under",
      "Exceedance Adequate",
      "Exceedance Over"
    )
  )
  testthat::expect_equal(nl$level, c("Over", "Adequate", "Under"))
  # Colours ported by value from afsetools::load_vectors() (R colour names).
  testthat::expect_equal(
    s$colour[s$level == "Within_boundary Under"],
    "lightseagreen"
  )
  testthat::expect_equal(s$colour[s$level == "Exceedance Over"], "indianred3")
  testthat::expect_equal(nl$colour[nl$level == "Under"], "blue")
  testthat::expect_equal(nl$colour[nl$level == "Over"], "red")
  testthat::expect_true(all(nzchar(s$colour)))
  testthat::expect_true(all(nzchar(nl$colour)))
})
