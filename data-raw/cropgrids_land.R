# Build inst/extdata/cropgrids_land.csv: per-(area_code, item_cbs_code) crop
# (physical) area and harvested area from CROPGRIDS v1.08 (year 2020).
#
# Source: Tang et al. (2024), Scientific Data, "CROPGRIDS: a global
# geo-referenced dataset of 173 crops". figshare DOI 10.6084/m9.figshare.22491997
#   - Table_CROPGRIDSv1.08_COU.xlsx  (file 44950936): national per-crop areas
#     (sheets "Harvested area (ha)" and "Crop (physical) area (ha)").
#   - CODES.zip (file 44950360): crop name matching + the FAOSTAT 2020 extract
#     used to recover FAOSTAT item codes.
#
# The CROPGRIDS physical/harvested ratio is the per-crop multi-cropping
# correction (e.g. rice ~0.81): it is applied to WHEP harvested area in
# build_cropgrids_land_extension().

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
devtools::load_all(".")

raw_dir <- tempfile("cropgrids_")
dir.create(raw_dir)
cou <- file.path(raw_dir, "CROPGRIDS_COU.xlsx")
codes_zip <- file.path(raw_dir, "CODES.zip")
download.file(
  "https://ndownloader.figshare.com/files/44950936",
  cou,
  mode = "wb"
)
download.file(
  "https://ndownloader.figshare.com/files/44950360",
  codes_zip,
  mode = "wb"
)
unzip(codes_zip, exdir = raw_dir)
match_x <- file.path(raw_dir, "CODES", "CROPS_matching_v17.xlsx")
fao_x <- file.path(raw_dir, "CODES", "FAOSTAT_ALL_2020_edited.xlsx")

# CROPGRIDS crop (REMAP name) -> FAO item name -> FAO item code -> item_cbs_code
cg2fao <- read_excel(match_x, sheet = "Disaggregated") |>
  transmute(cg = `REMAP MRF NAME`, fao_name = `FAO NAME`) |>
  filter(!is.na(cg), !is.na(fao_name)) |>
  mutate(k = str_to_lower(str_trim(fao_name)))

fao2code <- read_excel(fao_x, sheet = 1) |>
  distinct(Item, ItemCode_FAO_) |>
  transmute(
    k = str_to_lower(str_trim(Item)),
    fao_code = as.integer(ItemCode_FAO_)
  ) |>
  filter(!is.na(fao_code)) |>
  distinct(k, .keep_all = TRUE)

code2cbs <- whep::items_prod_full |>
  transmute(
    fao_code = as.integer(item_prod_code),
    item_cbs_code = as.integer(item_cbs_code)
  ) |>
  filter(!is.na(fao_code), !is.na(item_cbs_code)) |>
  distinct(fao_code, .keep_all = TRUE)

xwalk <- cg2fao |>
  left_join(fao2code, by = "k") |>
  left_join(code2cbs, by = "fao_code") |>
  distinct(cg, item_cbs_code)

# National table: wide -> long. "Country Name" is "<ISO3>; <M49>: Name > Name";
# map the ISO3 to WHEP's FAOSTAT area_code (the M49 code is NOT the FAOSTAT one).
iso2area <- whep::regions_full |>
  filter(!is.na(code)) |>
  transmute(iso3 = polity_code, area_code = as.integer(code)) |>
  distinct(iso3, .keep_all = TRUE)

read_cou <- function(sheet, valcol) {
  read_excel(cou, sheet = sheet) |>
    rename(country = `Country Name`) |>
    select(-`Country ISO2 code`) |>
    mutate(iso3 = str_extract(country, "^[A-Za-z]{3}")) |>
    filter(!is.na(iso3)) |>
    select(-country) |>
    pivot_longer(-iso3, names_to = "cg", values_to = valcol) |>
    mutate(!!valcol := as.numeric(.data[[valcol]]))
}

cropgrids_land <- read_cou("Crop (physical) area (ha)", "physical_ha") |>
  full_join(
    read_cou("Harvested area (ha)", "harvested_ha"),
    by = c("iso3", "cg")
  ) |>
  left_join(iso2area, by = "iso3") |>
  filter(!is.na(area_code)) |>
  left_join(xwalk, by = "cg") |>
  filter(!is.na(item_cbs_code)) |>
  summarise(
    physical_ha = round(sum(physical_ha, na.rm = TRUE), 1),
    harvested_ha = round(sum(harvested_ha, na.rm = TRUE), 1),
    .by = c(area_code, item_cbs_code)
  ) |>
  filter(harvested_ha > 0) |>
  arrange(area_code, item_cbs_code)

write_csv(cropgrids_land, "inst/extdata/cropgrids_land.csv")
