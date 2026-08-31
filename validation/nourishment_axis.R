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

# Hoisted out of the `cli` strings below: `air format` cannot reflow inside a
# string, and `line_length_linter` is off in the CI set, so a long line there is
# caught by nothing (#483).
med_energy <- stats::median(compared$ratio_energy, na.rm = TRUE)
med_protein <- stats::median(compared$ratio_protein, na.rm = TRUE)
world_whep <- round(weighted(
  compared$energy_kcal_cap_day,
  compared$population
))
world_fbs <- round(weighted(
  compared[["Food supply (kcal/capita/day)"]],
  compared$population
))
cli::cli_inform(c(
  "matched countries: {nrow(compared)}",
  "*" = "energy  ratio median {round(med_energy, 2)}",
  "*" = "protein ratio median {round(med_protein, 2)}",
  "*" = "world (pop-weighted) WHEP {world_whep} kcal vs FBS {world_fbs} kcal"
))

# The axis is expected to run high while #361 is open: Edible_N_kgFM is empty
# for all 421 biomass_coefs rows, so build_food_supply() falls back to
# whole-commodity nitrogen and counts inedible mass as food.
#
# GATED ON EITHER RATIO, AND PROTEIN IS THE ONE THAT MATTERS (#483). The
# classification printed above comes from `normalize_nourishment(supply)`, which
# defaults to `protein_g_cap_day`. Protein is the SJOS-N axis and energy the
# secondary cross-check (`R/nourishment.R`, `R/food_supply.R`). This guard used
# to test `ratio_energy` alone. Today both exceed 1.1, so it fired either way;
# a run where protein drifted above the threshold while energy stayed below it
# would have printed `Over` counts for an axis that is off, silently. Naming
# which ratio tripped keeps the two distinguishable rather than merging them.
high <- c(energy = med_energy, protein = med_protein) > 1.1
if (any(high, na.rm = TRUE)) {
  tripped <- names(high)[which(high)]
  cli::cli_warn(c(
    "!" = "{.val {tripped}} supply runs above FAOSTAT FBS.",
    i = "energy {round(med_energy, 2)}, protein {round(med_protein, 2)};
         threshold 1.1.",
    i = "Expected while #361 is open (empty {.field Edible_*} coefficients)."
  ))
}
invisible(list(supply = supply, nourishment = nourishment, compared = compared))

# ---- Per-item comparison (#500 J2.2) ---------------------------------------
#
# The Grand-Total comparison above is necessary and NOT sufficient. On the 2010
# build the axis's net excess is small, but that is cancellation: wheat and nuts
# run high while vegetables, poultry and pigmeat run low. An axis that agrees in
# total while being 30-50% wrong per item is not accurate, and for a per-country
# Under/Adequate/Over classification the item composition matters more than the
# global net. So the per-item ratios are reported, never only the net.

cli::cli_h2("Per item, against FAOSTAT FBS protein (element 671, tonnes)")

whep_item <- cbs_food |>
  whep:::.food_join_nutrition(
    whep:::.food_nutrition_lookup(
      whep::items_full,
      whep::biomass_coefs,
      "edible_portion"
    )
  ) |>
  dplyr::summarise(
    whep_protein_t = sum(.data$food_t * .data$protein_frac_kgfm, na.rm = TRUE),
    .by = c("area_code", "item_cbs_code")
  )

fbs_item <- whep_read_file("faostat-fbs-new") |>
  dplyr::filter(
    as.integer(.data$Year) == !!year,
    as.integer(.data[["Element Code"]]) == 671L
  ) |>
  dplyr::transmute(
    area_code = as.integer(.data[["Area Code"]]),
    item_cbs_code = as.integer(.data[["Item Code"]]),
    fbs_protein_t = as.numeric(.data$Value)
  ) |>
  dplyr::filter(is.finite(.data$fbs_protein_t), .data$fbs_protein_t > 0)

paired <- dplyr::inner_join(
  whep_item,
  fbs_item,
  by = c("area_code", "item_cbs_code")
)

by_item <- paired |>
  dplyr::summarise(
    countries = dplyr::n(),
    whep_t = sum(.data$whep_protein_t),
    fbs_t = sum(.data$fbs_protein_t),
    .by = "item_cbs_code"
  ) |>
  dplyr::mutate(
    conc_ratio = .data$whep_t / .data$fbs_t,
    residual_t = .data$whep_t - .data$fbs_t
  ) |>
  add_item_cbs_name() |>
  dplyr::arrange(dplyr::desc(abs(.data$residual_t)))

net_ratio <- sum(by_item$whep_t) / sum(by_item$fbs_t)
off_10 <- sum(abs(by_item$conc_ratio - 1) > 0.10)
gross <- sum(abs(by_item$residual_t))
net <- abs(sum(by_item$residual_t))

cli::cli_inform(c(
  "items compared: {nrow(by_item)}; outside +-10%: {off_10}",
  "*" = "net ratio {round(net_ratio, 4)} -- what a Grand-Total check sees",
  "*" = "gross item error {round(gross / 1e6, 2)} Mt against a net of
         {round(net / 1e6, 2)} Mt: cancellation factor
         {round(gross / max(net, 1), 1)}x"
))
cli::cli_inform("Largest absolute item residuals:")
print(utils::head(
  dplyr::select(
    by_item,
    "item_cbs_name",
    "countries",
    "conc_ratio",
    "residual_t"
  ),
  12
))

# Country spread within an item: a per-country classification cannot be read
# off a world ratio, so the dispersion is reported for the worst items.
country_spread <- paired |>
  dplyr::mutate(ratio = .data$whep_protein_t / .data$fbs_protein_t) |>
  dplyr::filter(is.finite(.data$ratio)) |>
  dplyr::summarise(
    countries = dplyr::n(),
    q25 = stats::quantile(.data$ratio, 0.25),
    median = stats::median(.data$ratio),
    q75 = stats::quantile(.data$ratio, 0.75),
    .by = "item_cbs_code"
  ) |>
  dplyr::semi_join(
    utils::head(by_item, 8),
    by = "item_cbs_code"
  ) |>
  add_item_cbs_name()
cli::cli_inform("Country ratio spread, the eight largest-residual items:")
print(country_spread)

cat(sprintf(
  paste(
    "METRIC items_compared=%d items_off_10pct=%d",
    "net_ratio=%.4f cancellation=%.1f\n"
  ),
  nrow(by_item),
  off_10,
  net_ratio,
  gross / max(net, 1)
))
