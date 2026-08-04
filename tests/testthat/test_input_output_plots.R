# Tests for input_output_plots.R functions
testthat::local_edition(3)


.fixture_nat_destiny <- function() {
  tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Origin, ~Destiny, ~MgN,
    2000, "Spain", "Wheat and products", "Synthetic", "Cropland", 800,
    2000, "Spain", "Wheat and products", "Fixation", "Cropland", 200,
    2000, "Spain", "Wheat and products", "Cropland", "population_food", 300,
    2000, "Spain", "Straw", "Cropland", "livestock_rum", 100,
    2000, "Spain", "Bovine Meat", "Livestock", "population_food", 50,
    2000, "Spain", "Wheat and products", "Cropland", "livestock_rum", 120,
    2000, "Spain", "Bovine Meat", "Outside", "population_food", 30,
    2000, "Spain", "Wheat and products", "Outside", "livestock_mono", 40,
    2000, "Spain", "Wheat and products", "Cropland", "export", 60
  )
}


# .surplus_from_totals ---------------------------------------------------------

test_that(".surplus_from_totals clamps to zero when positive_only is TRUE", {
  inputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "A", 100
  )
  outputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "B", 250
  )

  clamped <- .surplus_from_totals(inputs, outputs, positive_only = TRUE)
  signed <- .surplus_from_totals(inputs, outputs, positive_only = FALSE)

  expect_equal(clamped$MgN, 0)
  expect_equal(signed$MgN, -150)
  expect_equal(unique(clamped$Type), "Surplus")
})


# .system_production -----------------------------------------------------------

test_that(".system_production separates residues from production", {
  out <- .system_production(.fixture_nat_destiny(), "Cropland")

  residues <- out |>
    dplyr::filter(Type == "Residues") |>
    dplyr::pull(MgN)
  production <- out |>
    dplyr::filter(Type == "Production") |>
    dplyr::pull(MgN)

  expect_equal(residues, 100)
  expect_equal(production, 300 + 120 + 60)
})


# .stack_plot_df ---------------------------------------------------------------

test_that(".stack_plot_df negates input types and rescales to Gg", {
  inputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Synthetic_fertilizer", 1000
  )
  outputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Production", 2000
  )
  surplus <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Surplus", 500
  )

  out <- .stack_plot_df(
    inputs,
    outputs,
    surplus,
    negative_types = "Synthetic_fertilizer",
    type_levels = c("Synthetic_fertilizer", "Surplus", "Production")
  )

  synth <- out |>
    dplyr::filter(Type == "Synthetic_fertilizer") |>
    dplyr::pull(MgN)
  prod <- out |>
    dplyr::filter(Type == "Production") |>
    dplyr::pull(MgN)

  expect_equal(synth, -1)
  expect_equal(prod, 2)
  expect_s3_class(out$Type, "factor")
})


# exported plot builders -------------------------------------------------------

test_that("plot_input_output builders return ggplot objects on example data", {
  expect_s3_class(whep::plot_input_output(example = TRUE), "ggplot")
  expect_s3_class(whep::plot_input_output_livestock(example = TRUE), "ggplot")
  expect_s3_class(whep::plot_input_output_system(example = TRUE), "ggplot")
})


# .system_inputs ---------------------------------------------------------------

test_that(".system_inputs recodes Origin to Type for the Cropland system", {
  fixture <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Origin, ~Destiny, ~MgN,
    2000, "Spain", "Wheat", "Synthetic", "Cropland", 800,
    2000, "Spain", "Wheat", "Fixation", "Cropland", 200,
    2000, "Spain", "Manure", "Livestock", "Cropland", 500,
    2000, "Spain", "Waste", "People", "Cropland", 70,
    2000, "Spain", "Wheat", "Cropland", "export", 60
  )

  out <- .system_inputs(fixture, "Cropland")
  pick <- function(t) dplyr::pull(dplyr::filter(out, Type == t), MgN)

  expect_equal(pick("Synthetic_fertilizer"), 800)
  expect_equal(pick("Fixation"), 200)
  expect_equal(pick("Manure"), 500)
  expect_equal(pick("Urban"), 70)
  # the Cropland->export distractor row is excluded (Origin not an input source)
  expect_false("Cropland" %in% out$Origin)
  expect_equal(nrow(out), 4)
})


# .import_use ------------------------------------------------------------------

test_that(".import_use sums Outside imports for the given destinies", {
  fixture <- tibble::tribble(
    ~Year, ~Origin, ~Destiny, ~MgN,
    2000, "Outside", "livestock_mono", 40,
    2000, "Outside", "population_food", 30,
    2000, "Cropland", "livestock_rum", 100
  )

  feed <- .import_use(
    fixture,
    c("livestock_rum", "livestock_mono"),
    "Feed_import"
  )
  food <- .import_use(
    fixture,
    c("population_food", "population_other_uses"),
    "Food_import"
  )

  expect_equal(feed$MgN, 40)
  expect_equal(feed$Type, "Feed_import")
  expect_equal(food$MgN, 30)
  expect_equal(food$Type, "Food_import")
})


# .system_level_inputs ---------------------------------------------------------

test_that(".system_level_inputs binds recoded soil inputs with imports", {
  fixture <- tibble::tribble(
    ~Year, ~Origin, ~Destiny, ~MgN,
    2000, "Synthetic", "Cropland", 800,
    2000, "Fixation", "Cropland", 200,
    2000, "Outside", "livestock_mono", 40,
    2000, "Outside", "population_food", 30
  )

  out <- .system_level_inputs(fixture)
  pick <- function(t) dplyr::pull(dplyr::filter(out, Type == t), MgN)

  expect_setequal(
    out$Type,
    c("Synthetic_fertilizer", "Fixation", "Feed_import", "Food_import")
  )
  expect_equal(pick("Synthetic_fertilizer"), 800)
  expect_equal(pick("Fixation"), 200)
  expect_equal(pick("Feed_import"), 40)
  expect_equal(pick("Food_import"), 30)
})


# .system_level_uses -----------------------------------------------------------

test_that(".system_level_uses splits Feed, Food, Other_uses and Export", {
  fixture <- tibble::tribble(
    ~Year, ~Origin, ~Destiny, ~MgN,
    2000, "Cropland", "livestock_rum", 100,
    2000, "Cropland", "livestock_rum", 120,
    2000, "Cropland", "population_food", 300,
    2000, "Livestock", "population_food", 50,
    2000, "Cropland", "population_other_uses", 25,
    2000, "Cropland", "export", 60,
    2000, "Outside", "livestock_rum", 999
  )

  out <- .system_level_uses(fixture)
  pick <- function(t) dplyr::pull(dplyr::filter(out, Type == t), MgN)

  expect_equal(pick("Feed"), 220)
  expect_equal(pick("Food"), 350)
  expect_equal(pick("Other_uses"), 25)
  expect_equal(pick("Export"), 60)
})


# .load_nat_destiny ------------------------------------------------------------

test_that(".load_nat_destiny renames the snake_case contract to PascalCase", {
  out <- .load_nat_destiny(example = TRUE)

  expect_true(all(
    c(
      "Year",
      "Province_name",
      "Item",
      "Irrig_cat",
      "Box",
      "Origin",
      "Destiny",
      "MgN"
    ) %in%
      names(out)
  ))
  expect_false(any(c("year", "province_name", "mg_n") %in% names(out)))
})


# .stacked_area_plot -----------------------------------------------------------

test_that(".stacked_area_plot returns a labelled stacked-area ggplot", {
  plot_df <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "A", 1,
    2001, "A", 2
  ) |>
    dplyr::mutate(Type = factor(Type))

  g <- .stacked_area_plot(plot_df, "My title", c("A" = "red"))
  geoms <- vapply(g$layers, function(l) class(l$geom)[1], character(1))

  expect_s3_class(g, "ggplot")
  expect_true("GeomArea" %in% geoms)
  expect_true("GeomHline" %in% geoms)
  expect_equal(g$labels$title, "My title")
  expect_equal(g$labels$y, "Gg N")
  expect_equal(g$labels$x, "Year")
})


# plot builders: surplus sign semantics ----------------------------------------

test_that("plot_input_output clamps a negative cropland surplus to zero", {
  # inputs (5) < production (100), so the unclamped net is negative; the
  # cropland builder uses positive_only = TRUE and must clamp it to zero.
  testthat::local_mocked_bindings(
    create_n_nat_destiny = function(example = FALSE) {
      tibble::tribble(
        ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
        2000, "Spain", "Wheat", NA, "Cropland", "Synthetic", "Cropland", 5,
        2000, "Spain", "Wheat", NA, "Cropland", "Cropland", "population_food", 100
      )
    }
  )

  g <- whep::plot_input_output(example = TRUE)
  surplus <- g$data |>
    dplyr::filter(Type == "Surplus") |>
    dplyr::pull(MgN)

  expect_s3_class(g, "ggplot")
  expect_true(all(surplus == 0))
})

test_that("plot_input_output_system uses the documented factor levels", {
  g <- whep::plot_input_output_system(example = TRUE)
  input_types <- c(
    "Synthetic_fertilizer",
    "Fixation",
    "Deposition",
    "Feed_import",
    "Food_import"
  )

  expect_s3_class(g, "ggplot")
  expect_equal(
    levels(g$data$Type),
    c(input_types, "Surplus", "Feed", "Food", "Other_uses", "Export")
  )
  input_rows <- g$data |>
    dplyr::filter(Type %in% input_types)
  expect_true(all(input_rows$MgN <= 0))
})

test_that("plot_input_output_livestock keeps a negative surplus unclamped", {
  # feed inputs (10) < livestock production (100), so the signed net is
  # negative; the livestock builder uses positive_only = FALSE and must keep it.
  testthat::local_mocked_bindings(
    create_n_nat_destiny = function(example = FALSE) {
      tibble::tribble(
        ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
        2000, "Spain", "Wheat", NA, "Cropland", "Cropland", "livestock_rum", 10,
        2000, "Spain", "Bovine Meat", NA, "Livestock", "Livestock", "population_food", 100
      )
    }
  )

  g <- whep::plot_input_output_livestock(example = TRUE)
  surplus <- g$data |>
    dplyr::filter(Type == "Surplus") |>
    dplyr::pull(MgN)

  expect_s3_class(g, "ggplot")
  expect_true(any(surplus < 0))
})


# .surplus_from_totals: years present only in uses ----------------------------

test_that(".surplus_from_totals keeps years present only in uses", {
  inputs <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "A", 100
  )
  uses <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "B", 40,
    2001, "B", 30
  )

  out <- .surplus_from_totals(inputs, uses, positive_only = FALSE)
  pick <- function(y) dplyr::pull(dplyr::filter(out, Year == y), MgN)

  expect_setequal(out$Year, c(2000, 2001))
  expect_equal(pick(2000), 60)
  expect_equal(pick(2001), -30)
})


# .livestock_feed_inputs / .livestock_production ------------------------------

test_that(".livestock_feed_inputs recodes feed destinies", {
  out <- .livestock_feed_inputs(.fixture_nat_destiny())
  pick <- function(t) dplyr::pull(dplyr::filter(out, Type == t), MgN)

  # feed is selected by destiny regardless of origin, so the Outside ->
  # livestock_mono row (40) counts as monogastric feed
  expect_equal(pick("Feed_ruminants"), 100 + 120)
  expect_equal(pick("Feed_monogastric"), 40)
  expect_setequal(out$Type, c("Feed_ruminants", "Feed_monogastric"))
})

test_that(".livestock_production sums livestock output to one Type", {
  out <- .livestock_production(.fixture_nat_destiny())

  expect_equal(unique(out$Type), "Production")
  expect_equal(out$MgN, 50)
})


# ported feature: per-ha normalization ----------------------------------------

test_that(".normalize_mg_n divides totals by area when per_ha is TRUE", {
  df <- tibble::tribble(
    ~Year, ~Type, ~MgN,
    2000, "Synthetic_fertilizer", 500
  )
  lu_area <- tibble::tribble(
    ~Year, ~area_ha,
    2000, 1000
  )

  gg <- .normalize_mg_n(df, per_ha = FALSE, lu_area = NULL)
  per_ha <- .normalize_mg_n(df, per_ha = TRUE, lu_area = lu_area)

  # Gg conversion divides by 1000; per-ha is MgN * 1000 / area_ha (kg N/ha)
  expect_equal(gg$MgN, 0.5)
  expect_equal(per_ha$MgN, 500)
  expect_false("area_ha" %in% names(per_ha))
})

test_that("plot_input_output honours per_ha on real (mocked) data", {
  testthat::local_mocked_bindings(
    create_n_nat_destiny = function(example = FALSE) {
      tibble::tribble(
        ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
        2000, "Spain", "Wheat", NA, "Cropland", "Synthetic", "Cropland", 800,
        2000, "Spain", "Wheat", NA, "Cropland", "Cropland", "population_food", 300
      )
    },
    whep_read_file = function(alias) {
      tibble::tribble(
        ~Year, ~LandUse, ~Area_ygpit_ha,
        2000, "Cropland", 1000,
        2000, "Forest", 500
      )
    }
  )

  g <- whep::plot_input_output(system = "Cropland", per_ha = TRUE)

  expect_s3_class(g, "ggplot")
  expect_equal(g$labels$y, "kg N/ha")
})


# ported feature: accumulation term for semi-natural system --------------------

test_that(".calculate_n_accum nets accumulation gains against losses", {
  n_balance <- tibble::tribble(
    ~Year, ~LandUse, ~Accum_gain_AG_MgN, ~Accum_gain_BG_MgN, ~Accum_loss,
    2000, "Forest", 40, 10, 20,
    2000, "Cropland", 999, 999, 999
  )

  out <- .calculate_n_accum(n_balance, landuse = "Forest")

  expect_equal(unique(out$Type), "Accumulation")
  expect_equal(out$MgN, 40 + 10 - 20)
})

test_that("plot_input_output adds an Accumulation layer for semi-natural", {
  testthat::local_mocked_bindings(
    create_n_nat_destiny = function(example = FALSE) {
      tibble::tribble(
        ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
        2000, "Spain", "Grass", NA, "sna", "Fixation", "semi_natural_agroecosystems", 500,
        2000, "Spain", "Grass", NA, "sna", "semi_natural_agroecosystems", "livestock_rum", 300
      )
    },
    whep_read_file = function(alias) {
      tibble::tribble(
        ~Year, ~LandUse, ~Area_ygpit_ha, ~Accum_gain_AG_MgN, ~Accum_gain_BG_MgN, ~Accum_loss,
        2000, "Forest", 1000, 40, 10, 20,
        2000, "Cropland", 500, 999, 999, 999
      )
    }
  )

  g <- whep::plot_input_output(system = "semi_natural_agroecosystems")

  expect_s3_class(g, "ggplot")
  expect_true("Accumulation" %in% as.character(g$data$Type))
  expect_true("Accumulation" %in% levels(g$data$Type))
})

test_that("plot_input_output_system keeps base levels in example mode", {
  # example mode has no n_balance pin, so no Accumulation level is introduced
  g <- whep::plot_input_output_system(example = TRUE)
  expect_false("Accumulation" %in% levels(g$data$Type))
})


# ported feature: livestock feed-origin + rum/mono production breakdown --------

test_that("plot_input_output_livestock splits feed origin and production", {
  testthat::local_mocked_bindings(
    create_n_nat_destiny = function(example = FALSE) {
      tibble::tribble(
        ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
        2000, "Spain", "Wheat", NA, "Cropland", "Cropland", "livestock_rum", 50,
        2000, "Spain", "Grass", NA, "sna", "semi_natural_agroecosystems", "livestock_rum", 30,
        2000, "Spain", "Soy", NA, "Cropland", "Outside", "livestock_mono", 20,
        2000, "Spain", "Bovine Meat", NA, "Livestock", "Livestock", "population_food", 100,
        2000, "Spain", "Pork", NA, "Livestock", "Livestock", "population_food", 60
      )
    },
    whep_read_file = function(alias) {
      # stock_prod_ygps keys products as item_cbs, not Item.
      tibble::tribble(
        ~item_cbs, ~Livestock_cat,
        "Bovine Meat", "Cattle_meat",
        "Pork", "Pigs"
      )
    }
  )

  g <- whep::plot_input_output_livestock()
  types <- as.character(g$data$Type)

  expect_s3_class(g, "ggplot")
  expect_true(all(
    c("Grass_local", "Crops_local", "Imports") %in% types
  ))
  expect_true(all(
    c("Production_rum", "Production_mono") %in% types
  ))
})

test_that(".livestock_feed_by_origin buckets feed by N origin", {
  fixture <- tibble::tribble(
    ~Year, ~Origin, ~Destiny, ~MgN,
    2000, "Cropland", "livestock_rum", 50,
    2000, "semi_natural_agroecosystems", "livestock_rum", 30,
    2000, "Outside", "livestock_mono", 20,
    2000, "Livestock", "livestock_rum", 999
  )

  out <- .livestock_feed_by_origin(fixture)
  pick <- function(t) dplyr::pull(dplyr::filter(out, Type == t), MgN)

  expect_equal(pick("Crops_local"), 50)
  expect_equal(pick("Grass_local"), 30)
  expect_equal(pick("Imports"), 20)
  # the Livestock-origin distractor is not a feed origin bucket
  expect_setequal(out$Type, c("Crops_local", "Grass_local", "Imports"))
})
