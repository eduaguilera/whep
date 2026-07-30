# Build inst/extdata/faostat_cropland.csv: physical cropland area (ha) per
# (area_code, year), the conserved national total for the hectare-year land
# extension (build_hayr_land_extension()).
#
# Source: FAOSTAT Land Use (RL) domain, item 6620 "Cropland" (= arable land +
# permanent crops; each hectare counted once regardless of harvests per year),
# element 5110 "Area", reported in 1000 ha and converted to ha here. Keyed by
# the FAOSTAT Area Code, which matches WHEP area_code.
#
# build_hayr_land_extension() renormalizes each (year, area_code) so that the
# occupation-weighted crop areas sum to this cropland total, de-overlapping
# cross-crop double-cropping and charging idle/fallow land to the resident
# crops by occupation weight.
#
# Input: the faostat-landuse pin (FAOSTAT Land Use RL domain, normalized),
# read via whep_read_file().

library(readr)
library(dplyr)
devtools::load_all(".")

faostat_cropland <- whep::whep_read_file("faostat-landuse") |>
  dplyr::filter(
    .data[["Item Code"]] == 6620,
    .data[["Element Code"]] == 5110
  ) |>
  dplyr::transmute(
    area_code = as.integer(.data[["Area Code"]]),
    year = as.integer(.data$Year),
    cropland_ha = .data$Value * 1000
  ) |>
  # Drop FAOSTAT aggregate regions (World, continents, economic groupings; all
  # area_code >= 5000). Real FAOSTAT country codes are < 1000 and match WHEP
  # area_code; aggregates would silently double-count if ever summed.
  dplyr::filter(!is.na(cropland_ha), cropland_ha > 0, area_code < 1000) |>
  dplyr::arrange(area_code, year)

write_csv(faostat_cropland, "inst/extdata/faostat_cropland.csv")
