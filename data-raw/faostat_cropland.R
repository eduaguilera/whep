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
# Input: FAOSTAT Land Use bulk CSV (via WHEP_FAOSTAT_LANDUSE).

library(data.table)
library(readr)
library(dplyr)
library(tidyr)
devtools::load_all(".")

source("data-raw/_faostat_landuse.R")
fao_path <- faostat_landuse_noflag()

cropland <- fread(fao_path, showProgress = FALSE)[
  `Item Code` == 6620 & `Element Code` == 5110
]

year_cols <- grep("^Y[0-9]{4}$", names(cropland), value = TRUE)

faostat_cropland <- cropland[, c("Area Code", year_cols), with = FALSE] |>
  data.table::setnames("Area Code", "area_code") |>
  tibble::as_tibble() |>
  tidyr::pivot_longer(
    dplyr::all_of(year_cols),
    names_to = "year",
    values_to = "cropland_1000ha"
  ) |>
  dplyr::transmute(
    area_code = as.integer(area_code),
    year = as.integer(sub("^Y", "", year)),
    cropland_ha = cropland_1000ha * 1000
  ) |>
  # Drop FAOSTAT aggregate regions (World, continents, economic groupings; all
  # area_code >= 5000). Real FAOSTAT country codes are < 1000 and match WHEP
  # area_code; aggregates would silently double-count if ever summed.
  dplyr::filter(!is.na(cropland_ha), cropland_ha > 0, area_code < 1000) |>
  dplyr::arrange(area_code, year)

write_csv(faostat_cropland, "inst/extdata/faostat_cropland.csv")
