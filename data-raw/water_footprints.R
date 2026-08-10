# Prepare the water-footprint coefficient pins consumed by
# build_water_extension(). None of the coefficient values are hardcoded in the
# package; they live in pinned tables registered in inst/extdata/whep_inputs.csv
# and read at runtime via whep_read_file().
#
# Three pins are needed (aliases must match build_water_extension()):
#
#   1. wfn-water-crop        crop green/blue intensities, m3 t^-1   [UPLOADED]
#   2. water-livestock-blue  livestock blue water, m3 head^-1       [UPLOADED]
#   3. water-grazing-green   grazing green water, m3 ha^-1          [UPLOADED]
#
# Sources (verified):
#   - Crops: Mialyk, O., Schyns, J.F., Booij, M.J., Su, H., Hogeboom, R.J.,
#     Berger, M. (2024). "Water footprints and crop water use of 175 individual
#     crops for 1990-2019 simulated with a global crop model." Scientific Data
#     11, 206. doi:10.1038/s41597-024-03051-3. Dataset on 4TU.ResearchData,
#     doi:10.4121/7b45bcc6-686b-404d-a910-13c87156716a (the file linked in
#     FABIO's R/12_1_ext_land_mass_water.R; FABIO's "Mekonnen & Hoekstra" code
#     comment is wrong -- the linked file is the Mialyk et al. crop-model data).
#   - Livestock: Chapagain, A.K., Hoekstra, A.Y. (2003). "Virtual water flows
#     between nations in relation to trade in livestock and livestock products."
#     Value of Water Research Report Series No. 13, UNESCO-IHE. Tables 3.8
#     (drinking) and 3.9 (service) water requirements, litre/animal/day.
#     <https://waterfootprint.org/resources/Report13.pdf>
#   - Grazing: derived from the WHEP LPJmL run (green ET of managed grassland;
#     see section 3) following Schyns, J.F., Hoekstra, A.Y., Booij, M.J.,
#     Hogeboom, R.J., Mekonnen, M.M. (2019). "Limits to the world's green water
#     resources for food, feed, fiber, timber, and bioenergy." PNAS 116(11),
#     4893-4898, doi:10.1073/pnas.1817380116. Schyns' own m3 ha^-1 values are
#     not in a public repository (shared on request); we compute the equivalent
#     from our LPJmL run instead.
#
# Uploading is manual (saco.csic.es board access via ~/whep_inputs). After
# uploading, put the printed version into the matching whep_inputs.csv row and
# re-run the whep_inputs data-raw script to rebuild the package data.

library(readr)
library(dplyr)
library(tibble)

# 1. Crop water intensities (wfn-water-crop) -----------------------------------
# The 4TU CSV has three metadata lines before the header row, so skip = 3. Keep
# only the keys and the green/blue intensity columns build_water_extension()
# uses: crop_code (FAO item code), country_code (FAO area code), year, and the
# m3 t^-1 columns wfg_m3_t (green), wfb_cr_m3_t (blue, capillary rise) and
# wfb_i_m3_t (blue, irrigation).
crop_url <- paste0(
  "https://data.4tu.nl/file/7b45bcc6-686b-404d-a910-13c87156716a/",
  "3787e536-c388-4f76-a603-9081d6748588"
)
crop_csv <- tempfile("wfn_water_crop_", fileext = ".csv")
download.file(crop_url, crop_csv, mode = "wb")

wfn_water_crop <- read_csv(crop_csv, skip = 3, show_col_types = FALSE) |>
  transmute(
    crop_code = as.integer(crop_code),
    crop_name,
    country_code = as.integer(country_code),
    country_iso3,
    year = as.integer(year),
    wfg_m3_t = as.numeric(wfg_m3_t),
    wfb_cr_m3_t = as.numeric(wfb_cr_m3_t),
    wfb_i_m3_t = as.numeric(wfb_i_m3_t)
  ) |>
  filter(!is.na(crop_code), !is.na(country_code), !is.na(year))
# upload_csv(<written wfn_water_crop>, "wfn-water-crop")  via ~/whep_inputs

# 2. Livestock blue water (water-livestock-blue) -------------------------------
# Per-animal DRINKING (Chapagain & Hoekstra 2003, Table 3.8) and SERVICE
# (Table 3.9) water requirements, litre/animal/day, adult age group, by farming
# system, mapped to whep live-animal item_cbs codes. Mixed system = mean of the
# industrial and grazing systems (the report's own convention). Converted to
# m3/head/year as (drinking + service) * 365 / 1000.
livestock_src <- tribble(
  ~item_cbs_code, ~drink_ind, ~drink_graz, ~serv_ind, ~serv_graz,
  961L, 38, 22, 11, 5, # Cattle, non-dairy (beef, adult cows)
  960L, 70, 40, 22, 5, # Cattle, dairy (milking cows 3-10 yr)
  1049L, 14, 8, 50, 25, # Pigs (adult swine)
  976L, 7.6, 6.0, 5, 5, # Sheep (adult)
  1016L, 3.8, 3.5, 5, 5, # Goats (adult)
  1053L, 0.18, 0.18, 0.09, 0.09, # Chickens, broilers (adult)
  1052L, 0.30, 0.30, 0.15, 0.15, # Chickens, layers (laying)
  1096L, 45, 45, 5, 5 # Horses (mature)
)
water_livestock_blue <- livestock_src |>
  transmute(
    item_cbs_code,
    m3_per_head = round(
      ((drink_ind + drink_graz) / 2 + (serv_ind + serv_graz) / 2) * 365 / 1000,
      4
    )
  )
# upload_csv(<written water_livestock_blue>, "water-livestock-blue") via ~/whep_inputs

# 3. Grazing green water (water-grazing-green) ---------------------------------
# Green evapotranspiration of managed grassland (m3 ha^-1) PER COUNTRY AND YEAR,
# derived from the WHEP LPJmL production run. Source variable:
# cft_consump_water_g, band "rainfed grassland" (mm/yr); mm/yr x 10 = m3/ha. The
# country value is the grassland-area-weighted mean (weight = CFTfrac of the
# same band x cell area), with cells mapped to FAO area_code via
# country_grid.parquet. Methodological reference: Schyns et al. (2019),
# <https://doi.org/10.1073/pnas.1817380116>.
#
# ANNUAL, not a climatological mean. An earlier vintage averaged 2000-2009 into
# one value per country and applied it to every year from 1850, which froze
# grazing water at one decade's climate and grassland extent and made the
# coefficient join year-free (the shape whep#669's audit exists to surface).
# The underlying cube is annual, so there is no reason to throw that away.
#
# CORRECTED GREEN = green + blue. LPJmL 6.x books infiltrating rain as blue
# (whep#710): 5.x infiltrated through infil_perc_rain() with frac_g_influx=1 and
# infil_perc_irr() with 0, and 6.x merged them into one call with the summed
# parcel while keeping the irrigation constant. Band "rainfed grassland" cannot
# receive irrigation, so its blue is exactly that misassignment and belongs back
# in green. Fixed in lbm364dl/LPJmL#3; the fix is a PURE REPARTITION -- verified
# by rebuilding the parent commit and running both binaries on one config, where
# total consumptive water moves by +0.0011% and pre(green+blue) equals
# post(green) cell by cell. So green+blue from the existing run equals what a
# rerun with the fix produces, and no rerun is needed to regenerate this pin.
#
# The band is selected by NAME, never by index: which crop a given index denotes
# is a property of how the run was configured. The run directory comes from
# WHEP_LPJML_RUN_DIR (never hardcoded), and the annual per-CFT time axis is
# decoded by read_lpjml_hydrology() rather than by hand.
#
# Occupation basis: this charges the FULL managed-grassland ET, consistent with
# the grassland land extension's "occupation" metric. NOTE the resulting global
# total (15,735 km3/yr on the 6.1.1 basis, 13,683 on 5.9.7) sits ABOVE the
# published 8,258-12,960 km3/yr range for total grazing-land ET; Schyns' "green
# WF of grazing" (2,191 km3/yr) restricts to the grazed area at the necessary
# livestock density and is far smaller. That gap is the occupation-basis choice,
# not a defect, and is open for review (whep#681, whep#116).
grass_band_name <- "rainfed grassland"
grazing_years <- 1901:2023
earth_r <- 6371007.181
d2r <- pi / 180

# country_grid.parquet is in the gitignored WHEP LPJmL_inputs tree.
country_grid <- nanoparquet::read_parquet(
  "~/WHEP/LPJmL_inputs/whep/inputs/country_grid.parquet"
) |>
  mutate(lon = round(lon, 2), lat = round(lat, 2))

# One band of one variable for one year. Read a year at a time: the per-CFT cube
# is 720 x 277 x 32 x 123, and only one band of it survives the filter.
read_grass_band <- function(var, year) {
  read_lpjml_hydrology(var, years = year) |>
    filter(band_name == grass_band_name) |>
    select(lon, lat, value)
}

grazing_water_year <- function(year) {
  green <- read_grass_band("cft_consump_water_g", year) |> rename(gv = value)
  blue <- read_grass_band("cft_consump_water_b", year) |> rename(bv = value)
  frac <- read_grass_band("cftfrac", year) |> rename(fv = value)
  green |>
    inner_join(blue, by = c("lon", "lat")) |>
    inner_join(frac, by = c("lon", "lat")) |>
    filter(!is.na(gv), !is.na(fv)) |>
    mutate(
      lon = round(lon, 2),
      lat = round(lat, 2),
      cell_area_ha = earth_r^2 *
        (0.5 * d2r) *
        (sin((lat + 0.25) * d2r) - sin((lat - 0.25) * d2r)) /
        1e4,
      grass_area_ha = pmax(fv, 0) * cell_area_ha,
      m3_per_ha = (gv + bv) * 10
    ) |>
    inner_join(country_grid, by = c("lon", "lat")) |>
    filter(grass_area_ha > 0, !is.na(area_code)) |>
    summarise(
      m3_per_ha = round(
        sum(m3_per_ha * grass_area_ha) / sum(grass_area_ha),
        1
      ),
      .by = area_code
    ) |>
    filter(m3_per_ha > 0) |>
    mutate(year = as.integer(year), .before = 1)
}

water_grazing_green <- purrr::map(grazing_years, grazing_water_year) |>
  bind_rows()
# upload_csv(<written water_grazing_green>, "water-grazing-green") via ~/whep_inputs
