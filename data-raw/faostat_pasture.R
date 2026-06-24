# Build inst/extdata/faostat_pasture.csv: per-(area_code, year) permanent
# grassland area in hectares from FAOSTAT Land Use "Permanent meadows and
# pastures" (item 6655, Element "Area", reported in 1000 ha). This is the
# FAOSTAT-statistics alternative to the LUH2-derived grassland that
# build_primary_production() produces, used by
# build_grassland_land_extension(source = "faostat_pasture") for a
# literature-comparable grass-land basis.
#
# FAOSTAT area codes are kept only where regions_full has a real country
# (non-NA iso3c), which drops regional/World aggregates and the China-351
# aggregate in favour of mainland China (41), matching WHEP's area_code
# convention.
#
# Input: the faostat-landuse pin (FAOSTAT Land Use RL domain, normalized),
# read via whep_read_file().

library(dplyr)
library(readr)
devtools::load_all(".")

valid_codes <- whep::regions_full |>
  filter(!is.na(.data$iso3c)) |>
  pull(.data$code) |>
  unique()

faostat_pasture <- whep::whep_read_file("faostat-landuse") |>
  filter(.data[["Item Code"]] == 6655, .data$Element == "Area") |>
  transmute(
    area_code = as.integer(.data[["Area Code"]]),
    year = as.integer(.data$Year),
    pasture_ha = round(.data$Value * 1000, 1)
  ) |>
  filter(!is.na(pasture_ha), pasture_ha > 0, area_code %in% valid_codes) |>
  arrange(area_code, year)

write_csv(faostat_pasture, "inst/extdata/faostat_pasture.csv")
message(
  "Wrote ",
  nrow(faostat_pasture),
  " rows to inst/extdata/faostat_pasture.csv"
)
