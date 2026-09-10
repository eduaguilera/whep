# Real-data check of WHEP's food-protein coefficients against FAOSTAT FBS's
# own implied protein density, and the generator for the offline fixture
# tests/testthat/fixtures/fbs_protein_density_2010.csv (#796).
#
# Why this is a fair oracle rather than "FAOSTAT is right". FBS publishes,
# per country and item, both `Food` (element 5142, 1000 t) and
# `Protein supply quantity` (element 671, t). It derives the protein by
# applying food-composition factors to the processed commodities actually
# eaten and dividing by the standardised primary-equivalent food quantity
# (FAO, Food Balance Sheets: A Handbook, section III,
# <https://www.fao.org/4/X9892E/X9892e03.htm>). WHEP's `food_t` for the same
# item *is* that standardised quantity, so `build_food_supply()`'s coefficient
# has to be the same construct: protein per kilogram of primary equivalent,
# not protein per kilogram of the primary commodity. A whole-grain nitrogen
# density on that mass counts the bran and germ that milling sends to feed.
#
# This needs no build: it compares coefficients, so only the
# `faostat-fbs-new` pin is read. That is why it can regenerate the fixture the
# offline suite uses.
#
# Run:  Rscript --vanilla validation/food_protein_density.R [year] [--write]

suppressMessages(pkgload::load_all(".", quiet = TRUE))

cli_args <- commandArgs(trailingOnly = TRUE)
year <- as.integer(if (length(cli_args) > 0) cli_args[[1]] else "2010")
write_fixture <- "--write" %in% cli_args
min_countries <- 20L
cli::cli_h1("Food protein density vs FAOSTAT FBS, {year}")

# FBS implied density: element 671 over element 5142, summed over the
# country-item cells where BOTH are reported. FAO aggregates (area code
# >= 5000) are excluded so the oracle is a sum of countries, not a mix of
# countries and totals.
fbs <- whep_read_file("faostat-fbs-new") |>
  dplyr::filter(
    as.integer(.data$Year) == !!year,
    as.integer(.data[["Element Code"]]) %in% c(671L, 5142L),
    as.integer(.data[["Area Code"]]) < 5000L
  ) |>
  dplyr::transmute(
    area_code = as.integer(.data[["Area Code"]]),
    item_cbs_code = as.integer(.data[["Item Code"]]),
    element = dplyr::if_else(
      as.integer(.data[["Element Code"]]) == 671L,
      "protein_t",
      "food_kt"
    ),
    value = as.numeric(.data$Value)
  ) |>
  tidyr::pivot_wider(names_from = "element", values_from = "value") |>
  dplyr::filter(
    is.finite(.data$protein_t),
    is.finite(.data$food_kt),
    .data$protein_t > 0,
    .data$food_kt > 0
  )

oracle <- fbs |>
  dplyr::summarise(
    countries = dplyr::n(),
    fbs_protein_g_kgfm = sum(.data$protein_t) /
      sum(.data$food_kt * 1000) *
      1000,
    .by = "item_cbs_code"
  ) |>
  dplyr::filter(.data$countries >= min_countries) |>
  add_item_cbs_name() |>
  dplyr::select(
    "item_cbs_code",
    "item_cbs_name",
    "fbs_protein_g_kgfm",
    "countries"
  ) |>
  dplyr::arrange(.data$item_cbs_code)

# WHEP's density, the same arithmetic build_food_supply() does on its default
# `edible_portion` basis.
whep_density <- whep::items_full |>
  dplyr::distinct(.data$item_cbs_code, .data$Name_biomass) |>
  dplyr::inner_join(
    whep::biomass_coefs |>
      dplyr::distinct(.data$Name_biomass, .keep_all = TRUE) |>
      dplyr::transmute(
        Name_biomass = .data$Name_biomass,
        nitrogen = dplyr::coalesce(
          .data$N_kgN_kgFM,
          .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM
        ) *
          dplyr::coalesce(.data$Edible_portion, 1)
      ),
    by = "Name_biomass"
  ) |>
  dplyr::transmute(
    item_cbs_code = as.integer(.data$item_cbs_code),
    Name_biomass = .data$Name_biomass,
    whep_protein_g_kgfm = .data$nitrogen * 6.25 * 1000
  ) |>
  dplyr::filter(!is.na(.data$whep_protein_g_kgfm))

paired <- dplyr::inner_join(whep_density, oracle, by = "item_cbs_code") |>
  dplyr::mutate(
    ratio = .data$whep_protein_g_kgfm / .data$fbs_protein_g_kgfm,
    gap_g_kgfm = .data$whep_protein_g_kgfm - .data$fbs_protein_g_kgfm
  )

out_of_band <- paired |>
  dplyr::filter(
    abs(.data$gap_g_kgfm) > 5,
    .data$ratio > 1.2 | .data$ratio < 0.8
  ) |>
  dplyr::arrange(dplyr::desc(abs(.data$ratio - 1)))

cli::cli_inform(c(
  "items priced by both: {nrow(paired)}",
  "*" = "outside +-20% and 5 g/kg: {nrow(out_of_band)}",
  "*" = "median ratio {round(stats::median(paired$ratio), 3)}"
))
cli::cli_inform("Out of band, worst first:")
print(
  dplyr::select(
    out_of_band,
    "item_cbs_code",
    "item_cbs_name",
    "Name_biomass",
    "whep_protein_g_kgfm",
    "fbs_protein_g_kgfm",
    "ratio",
    "countries"
  ),
  n = 40
)

wheat <- dplyr::filter(paired, .data$item_cbs_code == 2511L)
cli::cli_inform(c(
  "Wheat (2511), the #796 case:",
  "*" = "WHEP {round(wheat$whep_protein_g_kgfm, 2)} g/kg vs FBS
         {round(wheat$fbs_protein_g_kgfm, 2)} g/kg, ratio
         {round(wheat$ratio, 3)} over {wheat$countries} countries."
))

if (write_fixture) {
  path <- fs::path(
    "tests",
    "testthat",
    "fixtures",
    paste0(
      "fbs_protein_density_",
      year,
      ".csv"
    )
  )
  readr::write_csv(
    dplyr::mutate(
      oracle,
      fbs_protein_g_kgfm = round(.data$fbs_protein_g_kgfm, 4)
    ),
    path
  )
  cli::cli_inform("Wrote {.path {path}} ({nrow(oracle)} rows).")
}

cat(sprintf(
  "METRIC items=%d out_of_band=%d wheat_ratio=%.4f\n",
  nrow(paired),
  nrow(out_of_band),
  wheat$ratio
))
