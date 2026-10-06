# Does the registered `spatialize-livestock-country-data` pin still carry the
# head counts production publishes? (issue #1274)
#
# The pin is a frozen build of Section 8 of
# `inst/scripts/prepare_spatialize_all.R`. Section 8 checks its own output
# against the production table it was built from, but it cannot see a fix to
# production that lands after the pin was uploaded: that is how the pin
# registered on 2026-09-14 kept 48.3 M equine head for 2020 while production,
# after whep#1106 restored the asses, mules and horses of countries that
# report no meat for them, carried 116.9 M.
#
# So this compares the pin, per year and species group, with
# `build_primary_production()` as it is now, grouped through the same
# `livestock_mapping.csv`. The comparison is on world totals, which the
# pin's reporting-code keying of Sudan (276/277 after 2011, against
# production's bucket 206) does not change.
#
# ## Usage
#
#   Rscript --no-init-file validation/livestock_country_heads.R
#
#   VAL_LH_YEARS      optional; comma-separated years. Default 2010,2020.
#   VAL_LH_TOLERANCE  optional; relative tolerance. Default 1e-6.
#
# Exits non-zero when any year-group pair is off by more than the tolerance,
# printing the table either way.

devtools::load_all(".", quiet = TRUE)

years <- as.integer(strsplit(Sys.getenv("VAL_LH_YEARS", "2010,2020"), ",")[[
  1
]])
tolerance <- as.numeric(Sys.getenv("VAL_LH_TOLERANCE", "1e-6"))

mapping <- readr::read_csv(
  system.file("extdata", "livestock_mapping.csv", package = "whep"),
  show_col_types = FALSE
)

production <- build_primary_production(
  start_year = min(years),
  end_year = max(years)
) |>
  dplyr::filter(
    .data$unit == "heads",
    .data$year %in% years,
    .data$value > 0
  ) |>
  dplyr::mutate(item_code = as.integer(.data$item_prod_code)) |>
  dplyr::inner_join(
    dplyr::select(mapping, "item_code", "species_group"),
    by = "item_code"
  ) |>
  dplyr::summarise(
    production = sum(.data$value),
    .by = c("year", "species_group")
  )

pin <- whep_read_file("spatialize-livestock-country-data") |>
  dplyr::filter(.data$year %in% years) |>
  dplyr::summarise(pin = sum(.data$heads), .by = c("year", "species_group"))

result <- dplyr::full_join(production, pin, by = c("year", "species_group")) |>
  dplyr::mutate(
    relative_difference = .data$pin / .data$production - 1
  ) |>
  dplyr::arrange(.data$year, .data$species_group)

print(as.data.frame(result), digits = 10)

off <- dplyr::filter(
  result,
  is.na(.data$relative_difference) |
    abs(.data$relative_difference) > tolerance
)
if (nrow(off) > 0L) {
  cli::cli_abort(
    "{nrow(off)} year-group pair{?s} differ{?s/} from production by more
     than {tolerance}."
  )
}
cli::cli_alert_success(
  "Pin heads match production in every year-group pair, within {tolerance}."
)
