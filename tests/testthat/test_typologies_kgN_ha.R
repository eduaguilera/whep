# test_typologies_kgN_ha.R — tests for R/typologies_kgN_ha.R
#
# The two entry points read `npp_ygpit` / `n_balance_ygpit_all` and the
# typology time series. Both readers are stubbed here, so the tests are
# offline; the plots the functions print go to a null device.

# Four provinces, two per typology family, so the standard deviation the
# kgN/ha lines draw is defined.
kgn_npp_fixture <- function() {
  tibble::tribble(
    ~Province_name, ~LandUse,            ~Area_ygpit_ha,
    "Lugo",         "Cropland",          200,
    "Lugo",         "Pasture_Shrubland", 300,
    "Ourense",      "Cropland",          100,
    "Ourense",      "Forest",            100,
    "Segovia",      "Cropland",          200,
    "Soria",        "Cropland",          400,
    "Soria",        "Forest",            100
  ) |>
    dplyr::mutate(Year = 2000)
}

kgn_typologies_fixture <- function() {
  tibble::tribble(
    ~province_name, ~Typology_base,
    "Lugo",         "Semi-natural agroecosystems",
    "Ourense",      "Semi-natural agroecosystems",
    "Segovia",      "Specialized cropping systems (extensive)",
    "Soria",        "Specialized cropping systems (intensive)"
  ) |>
    dplyr::mutate(year = 2000)
}

kgn_typology_colors <- function() {
  c(
    "Semi-natural agroecosystems" = "#66a61e",
    "Specialized cropping systems (extensive)" = "#FFF7C2",
    "Specialized cropping systems (intensive)" = "#F7DD5A"
  )
}

# .sum_area_by_prov -----------------------------------------------------------

test_that(".sum_area_by_prov totals every land use by default", {
  out <- whep:::.sum_area_by_prov(kgn_npp_fixture())

  expect_equal(out$Province_name, c("Lugo", "Ourense", "Segovia", "Soria"))
  expect_equal(out$Area_ha, c(500, 200, 200, 500))
})

test_that(".sum_area_by_prov restricts to the land uses it is given", {
  out <- whep:::.sum_area_by_prov(
    kgn_npp_fixture(),
    land_uses = c("Cropland", "Pasture_Shrubland")
  )

  # The forest hectares of Ourense and Soria drop out; Lugo's pasture does not.
  expect_equal(out$Area_ha, c(500, 100, 200, 400))
})

# .build_area_totals ----------------------------------------------------------

test_that(".build_area_totals converts to Mha and shares of the year", {
  colors <- kgn_typology_colors()

  out <- whep:::.build_area_totals(
    whep:::.sum_area_by_prov(kgn_npp_fixture()),
    kgn_typologies_fixture() |> dplyr::rename(Typology = Typology_base),
    colors
  )

  expect_equal(as.character(out$Typology), names(colors))
  # Lugo + Ourense are semi-natural, Segovia extensive, Soria intensive.
  expect_equal(out$Total_ha, c(700, 200, 500))
  expect_equal(out$Mha, c(700, 200, 500) / 1e6)
  # Percentages are taken within the year, so they add up to 100.
  expect_equal(sum(out$Percent_ha), 100)
  expect_equal(out$Percent_ha[[1]], 50)
  # The typology becomes a factor over the supplied palette, which is what
  # keeps the fill scale stable in a year where a class is absent.
  expect_equal(levels(out$Typology), names(colors))
})

test_that(".build_area_totals keeps unmatched provinces as an NA typology", {
  out <- whep:::.build_area_totals(
    whep:::.sum_area_by_prov(kgn_npp_fixture()),
    kgn_typologies_fixture() |>
      dplyr::filter(province_name == "Lugo") |>
      dplyr::rename(Typology = Typology_base),
    kgn_typology_colors()
  )

  # The three provinces with no typology keep their area under NA instead of
  # vanishing, so the total is still the whole country.
  expect_equal(nrow(out), 2)
  expect_true(any(is.na(out$Typology)))
  expect_equal(sum(out$Total_ha), 1400)
  expect_equal(sum(out$Percent_ha), 100)
})

# .plot_area_stacked ----------------------------------------------------------

test_that(".plot_area_stacked plots the column it was asked for", {
  skip_if_not_installed("ggplot2")

  colors <- c("Semi-natural agroecosystems" = "#66a61e")
  df <- tibble::tibble(
    Year = c(2000, 2020),
    Typology = factor("Semi-natural agroecosystems", levels = names(colors)),
    Mha = c(1, 2),
    Percent_ha = c(40, 60)
  )

  plot <- whep:::.plot_area_stacked(
    df,
    year_breaks = 2020,
    y_var = "Percent_ha",
    title = "Land area by typology (%)",
    y_label = "Share of total area (%)",
    colors = colors
  )

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Land area by typology (%)")
  expect_equal(plot$labels$y, "Share of total area (%)")
  # `y_var` selects the column, so the percentage series is drawn, not Mha.
  expect_equal(ggplot2::layer_data(plot)$y, c(40, 60))
})

# typology_area_stacked_bars --------------------------------------------------

test_that("typology_area_stacked_bars returns both totals and shares", {
  skip_if_not_installed("ggplot2")
  withr::local_pdf(nullfile())

  local_mocked_bindings(
    create_typo_ts_plot = function(...) kgn_typologies_fixture(),
    whep_read_file = function(name, ...) kgn_npp_fixture()
  )

  out <- typology_area_stacked_bars()

  expect_named(out, c("df", "p_total", "p_pct"))
  expect_equal(sum(out$df$Total_ha), 1400)
  expect_equal(sum(out$df$Percent_ha), 100)
  expect_setequal(as.character(out$df$Typology), names(kgn_typology_colors()))
  expect_s3_class(out$p_total, "ggplot")
  expect_s3_class(out$p_pct, "ggplot")
})

# typology_kgha_lines ---------------------------------------------------------

test_that("typology_kgha_lines reports N inputs per hectare by typology", {
  skip_if_not_installed("ggplot2")
  withr::local_pdf(nullfile())

  n_balance <- tibble::tribble(
    ~Province_name, ~LandUse,   ~Deposition, ~BNF, ~Synthetic,
    "Lugo",         "Cropland", 1,           2,    3,
    "Lugo",         "Forest",   10,          10,   10,
    "Ourense",      "Cropland", 1,           1,    0,
    "Segovia",      "Cropland", 1,           1,    0,
    "Soria",        "Cropland", 2,           4,    6
  ) |>
    dplyr::mutate(Year = 2000, Solid = 0, Liquid = 0, Urban = 0)

  local_mocked_bindings(
    create_typo_ts_plot = function(...) kgn_typologies_fixture(),
    whep_read_file = function(name, ...) {
      if (name == "npp_ygpit") kgn_npp_fixture() else n_balance
    }
  )

  out <- typology_kgha_lines()

  expect_named(out, c("p1", "p2", "agricultural_land", "total_land"))
  # The intensive/extensive qualifier is stripped before grouping, so the two
  # specialized cropping classes are averaged together.
  expect_setequal(
    as.character(out$agricultural_land$Typology),
    c("Semi-natural agroecosystems", "Specialized cropping systems")
  )

  # Agricultural land only: Lugo 6 MgN over 500 ha (12 kgN/ha) and Ourense
  # 2 MgN over 100 ha (20), so their mean is 16.
  expect_equal(
    out$agricultural_land |>
      dplyr::filter(Typology == "Semi-natural agroecosystems") |>
      dplyr::pull(mean_kgN),
    16
  )
  # Soria 12 MgN over 400 ha (30) against Segovia 2 over 200 (10).
  expect_equal(
    out$agricultural_land |>
      dplyr::filter(Typology == "Specialized cropping systems") |>
      dplyr::pull(mean_kgN),
    20
  )
  expect_equal(
    out$agricultural_land |>
      dplyr::filter(Typology == "Specialized cropping systems") |>
      dplyr::pull(sd_kgN),
    stats::sd(c(30, 10))
  )

  # Total land adds Lugo's 30 MgN of forest inputs over 500 ha (72 kgN/ha)
  # and spreads Soria's cropland N over its forest hectares too (24).
  expect_equal(
    out$total_land |>
      dplyr::filter(Typology == "Semi-natural agroecosystems") |>
      dplyr::pull(mean_kgN),
    41
  )
  expect_equal(
    out$total_land |>
      dplyr::filter(Typology == "Specialized cropping systems") |>
      dplyr::pull(mean_kgN),
    17
  )
})
