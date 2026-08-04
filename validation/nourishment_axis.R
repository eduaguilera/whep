# Real-data validation of the SJOS-N nourishment ("just") axis (#450).
#
# Every test and example on this path is fixture-driven, so this script is the
# only place the axis is exercised on real inputs. It is not part of the test
# suite: it needs the `gdp-population`, commodity-balance and `faostat-fbs-new`
# pins, which CI never fetches.
#
# The cross-check is against FAOSTAT's own published per-capita supply, read
# through the SAME pin the package's `method = "faostat_fbs"` uses, so this is a
# comparison of two methods over one year rather than an eyeball against a
# remembered number.
#
# Run:  Rscript --vanilla validation/nourishment_axis.R [year]

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# Not `commandArgs(...)[1] %||% "2010"`: subscripting past the end of a
# character vector yields NA_character_, not NULL, so `%||%` never fires and a
# no-argument run silently gets year = NA and filters everything away.
cli_args <- commandArgs(trailingOnly = TRUE)
year <- as.integer(if (length(cli_args) > 0) cli_args[1] else "2010")
cli::cli_h1("Nourishment axis, {year}")

cbs_food <- get_wide_cbs() |>
  dplyr::filter(.data$year == !!year, !is.na(.data$food), .data$food > 0) |>
  dplyr::transmute(year, area_code, item_cbs_code, food_t = .data$food)
population <- read_population(years = year)

supply <- build_food_supply(
  method = "whep_native",
  data = list(cbs_food = cbs_food, population = population)
)
nourishment <- normalize_nourishment(supply)

cli::cli_h2("Coverage")
cli::cli_inform("countries: {nrow(supply)}")
print(dplyr::summarise(nourishment, n = dplyr::n(), .by = "nourish"))

cli::cli_h2("Against FAOSTAT FBS (item 2901, Grand Total)")
elements <- c(
  "Food supply (kcal/capita/day)",
  "Protein supply quantity (g/capita/day)"
)
fbs <- whep_read_file("faostat-fbs-new") |>
  dplyr::filter(
    as.integer(.data$Year) == !!year,
    .data$Element %in% elements,
    as.integer(.data[["Item Code"]]) == 2901L
  ) |>
  dplyr::transmute(
    area_code = as.integer(.data[["Area Code"]]),
    element = .data$Element,
    value = as.numeric(.data$Value)
  ) |>
  tidyr::pivot_wider(names_from = "element", values_from = "value")

compared <- supply |>
  dplyr::inner_join(fbs, by = "area_code") |>
  dplyr::mutate(
    ratio_energy = .data$energy_kcal_cap_day /
      .data[["Food supply (kcal/capita/day)"]],
    ratio_protein = .data$protein_g_cap_day /
      .data[["Protein supply quantity (g/capita/day)"]]
  )

weighted <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)])
cli::cli_inform(c(
  "matched countries: {nrow(compared)}",
  "*" = "energy  ratio median {round(stats::median(compared$ratio_energy, na.rm = TRUE), 2)}",
  "*" = "protein ratio median {round(stats::median(compared$ratio_protein, na.rm = TRUE), 2)}",
  "*" = "world (pop-weighted) WHEP {round(weighted(compared$energy_kcal_cap_day, compared$population))} kcal
         vs FBS {round(weighted(compared[['Food supply (kcal/capita/day)']], compared$population))} kcal"
))

# The axis is expected to run high while #361 is open: Edible_N_kgFM is empty
# for all 421 biomass_coefs rows, so build_food_supply() falls back to
# whole-commodity nitrogen and counts inedible mass as food.
if (stats::median(compared$ratio_energy, na.rm = TRUE) > 1.1) {
  cli::cli_warn(c(
    "!" = "Energy supply runs above FAOSTAT FBS.",
    i = "Expected while #361 is open (empty {.field Edible_*} coefficients)."
  ))
}
invisible(list(supply = supply, nourishment = nourishment, compared = compared))
