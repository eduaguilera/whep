# Indicator fixture spanning several typologies, used across the tests below.
.test_typology_indicators <- function() {
  tibble::tribble(
    ~year,
    ~province_name,
    ~production_seminatural,
    ~production_crops,
    ~animal_ingestion,
    ~synthetic_share,
    ~crop_productivity,
    ~Livestock_density,
    ~imported_feed_share,
    ~feed_from_seminatural_share,
    ~local_feed_share,
    ~Manure_share,
    # semi-natural dominates
    2000,
    "Seminat",
    100,
    10,
    5,
    0.1,
    5,
    0.2,
    0.1,
    0.9,
    0.1,
    0.1,
    # specialised cropping, intensive
    2000,
    "CropInt",
    1,
    100,
    5,
    0.8,
    40,
    0.1,
    0.1,
    0.1,
    0.1,
    0.1,
    # specialised cropping, extensive
    2000,
    "CropExt",
    1,
    100,
    5,
    0.2,
    5,
    0.1,
    0.1,
    0.1,
    0.1,
    0.1,
    # specialised livestock, intensive
    2000,
    "LivInt",
    1,
    10,
    50,
    0.1,
    20,
    2.0,
    0.8,
    0.1,
    0.1,
    0.1,
    # connected crop-livestock, intensive
    2000,
    "ConnInt",
    1,
    10,
    50,
    0.1,
    40,
    0.5,
    0.1,
    0.5,
    0.5,
    0.5,
    # connected crop-livestock, extensive
    2000,
    "ConnExt",
    1,
    10,
    50,
    0.1,
    20,
    0.5,
    0.1,
    0.5,
    0.5,
    0.5
  )
}


# .typology_thresholds --------------------------------------------------------

test_that(".typology_thresholds is a complete, numeric, named list", {
  th <- .typology_thresholds()

  expect_type(th, "list")
  expect_true(all(vapply(th, is.numeric, logical(1))))
  expect_true(all(nzchar(names(th))))
  expect_false(anyDuplicated(names(th)) > 0)
  # The extensive livestock band must be a band, not an empty interval.
  expect_lt(th$livestock_density_ext_lo, th$livestock_density_ext_hi)
})


# .classify_typology_base ------------------------------------------------------

test_that(".classify_typology_base assigns the expected typologies", {
  out <- .classify_typology_base(
    .test_typology_indicators(),
    .typology_thresholds()
  )

  labels <- stats::setNames(out$Typology_base, out$province_name)
  expect_equal(labels[["Seminat"]], "Semi-natural agroecosystems")
  expect_equal(labels[["CropInt"]], "Specialized cropping systems (intensive)")
  expect_equal(labels[["CropExt"]], "Specialized cropping systems (extensive)")
  expect_equal(
    labels[["LivInt"]],
    "Specialized livestock systems (intensive)"
  )
  expect_equal(
    labels[["ConnInt"]],
    "Connected crop-livestock systems (intensive)"
  )
  expect_equal(
    labels[["ConnExt"]],
    "Connected crop-livestock systems (extensive)"
  )
})

test_that(".classify_typology_base always labels every row", {
  out <- .classify_typology_base(
    .test_typology_indicators(),
    .typology_thresholds()
  )

  expect_false(anyNA(out$Typology_base))
  expect_equal(nrow(out), nrow(.test_typology_indicators()))
})

test_that(".classify_typology_base reacts to the thresholds it is given", {
  indicators <- .test_typology_indicators()
  th <- .typology_thresholds()

  # ConnInt sits at crop_productivity 40. Pushing the connected-systems
  # productivity cut-off above it must flip intensive to extensive.
  th$crop_productivity_connected <- 50

  out <- .classify_typology_base(indicators, th)
  label <- out$Typology_base[out$province_name == "ConnInt"]

  expect_equal(label, "Connected crop-livestock systems (extensive)")
})

test_that(".classify_typology_base leaves crop-dominated provinces uncovered when synthetic_share is high but productivity is low", {
  # The two specialised-cropping branches are not complementary: intensive
  # needs synthetic_share above the cut-off, extensive needs it at or below.
  # A crop-dominated province with high synthetic share and low productivity
  # therefore matches neither and falls through to the crop-livestock rules,
  # even though it has almost no livestock. Documented here so the behaviour
  # is visible rather than surprising; changing it is a methodological call.
  indicators <- tibble::tribble(
    ~year,
    ~province_name,
    ~production_seminatural,
    ~production_crops,
    ~animal_ingestion,
    ~synthetic_share,
    ~crop_productivity,
    ~Livestock_density,
    ~imported_feed_share,
    ~feed_from_seminatural_share,
    ~local_feed_share,
    ~Manure_share,
    2000,
    "HighSynthLowYield",
    1,
    100,
    1,
    0.8,
    5,
    0.01,
    0.1,
    0.1,
    0.1,
    0.1
  )

  out <- .classify_typology_base(indicators, .typology_thresholds())

  expect_equal(
    out$Typology_base,
    "Disconnected crop-livestock systems (intensive)"
  )
})


# .oat_threshold_grid ---------------------------------------------------------

test_that(".oat_threshold_grid varies one threshold at a time", {
  th <- list(a = 10, b = 0.5)

  grid <- .oat_threshold_grid(th, variation = 0.2)

  expect_equal(nrow(grid), 4)
  expect_setequal(grid$threshold, c("a", "b"))
  expect_setequal(grid$direction, c("low", "high"))

  a_low <- grid$th[grid$threshold == "a" & grid$direction == "low"][[1]]
  a_high <- grid$th[grid$threshold == "a" & grid$direction == "high"][[1]]

  expect_equal(a_low$a, 8)
  expect_equal(a_high$a, 12)
  # b must be untouched while a is perturbed.
  expect_equal(a_low$b, 0.5)
  expect_equal(a_high$b, 0.5)
})

test_that(".oat_threshold_grid covers every threshold in both directions", {
  th <- .typology_thresholds()

  grid <- .oat_threshold_grid(th, variation = 0.1)

  expect_equal(nrow(grid), 2 * length(th))
  expect_setequal(grid$threshold, names(th))
})


# .compute_agreement ----------------------------------------------------------

test_that(".compute_agreement reports 100% for an unchanged classification", {
  baseline <- tibble::tribble(
    ~year, ~province_name, ~Typology_base,
    2000, "A", "X",
    2000, "B", "Y"
  )

  out <- .compute_agreement(baseline, baseline, "thr", "low")

  expect_equal(out$agreement_pct, 100)
  expect_equal(out$threshold, "thr")
  expect_equal(out$direction, "low")
  expect_named(out, c("threshold", "direction", "agreement_pct"))
})

test_that(".compute_agreement counts the share of unchanged province-years", {
  baseline <- tibble::tribble(
    ~year, ~province_name, ~Typology_base,
    2000, "A", "X",
    2000, "B", "Y",
    2000, "C", "Y",
    2000, "D", "Y"
  )
  changed <- baseline |>
    dplyr::mutate(Typology_base = c("X", "Z", "Y", "Y"))

  out <- .compute_agreement(baseline, changed, "thr", "high")

  expect_equal(out$agreement_pct, 75)
})
