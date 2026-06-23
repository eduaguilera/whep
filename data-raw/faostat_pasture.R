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
# Input: FAOSTAT Land Use bulk CSV (Inputs_LandUse_E_All_Data_NOFLAG.csv via
# WHEP_FAOSTAT_LANDUSE).

library(dplyr)
library(data.table)
library(readr)
devtools::load_all(".")

fao_path <- Sys.getenv(
  "WHEP_FAOSTAT_LANDUSE",
  "/home/usuario/LandInG/landuse/Faostat/landuse_all_data/Inputs_LandUse_E_All_Data_NOFLAG.csv"
)

valid_codes <- whep::regions_full |>
  filter(!is.na(.data$iso3c)) |>
  pull(.data$code) |>
  unique()

dt <- fread(fao_path, showProgress = FALSE)[
  `Item Code` == 6655 & Element == "Area"
]
year_cols <- grep("^Y[0-9]{4}$", names(dt), value = TRUE)

faostat_pasture <- dt |>
  tibble::as_tibble() |>
  select(area_code = `Area Code`, all_of(year_cols)) |>
  tidyr::pivot_longer(
    all_of(year_cols),
    names_to = "year",
    values_to = "pasture_1000ha"
  ) |>
  transmute(
    area_code = as.integer(area_code),
    year = as.integer(sub("^Y", "", year)),
    pasture_ha = round(pasture_1000ha * 1000, 1)
  ) |>
  filter(!is.na(pasture_ha), pasture_ha > 0, area_code %in% valid_codes) |>
  arrange(area_code, year)

write_csv(faostat_pasture, "inst/extdata/faostat_pasture.csv")
message(
  "Wrote ",
  nrow(faostat_pasture),
  " rows to inst/extdata/faostat_pasture.csv"
)
