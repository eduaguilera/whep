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
# Country-average green evapotranspiration of managed grassland (m3 ha^-1),
# derived from the WHEP LPJmL run (LPJmL 5.9.7, daily grazing, "our_inputs",
# global 1901-2009). Source variable: cft_consump_water_g, band 14 "rainfed
# grassland" (mm/yr), averaged over 2000-2009; mm/yr x 10 = m3/ha. The country
# value is the grassland-area-weighted mean (weight = CFTfrac band 14 x cell
# area), with cells mapped to FAO area_code via country_grid.parquet.
# Methodological reference: Schyns et al. (2019),
# <https://doi.org/10.1073/pnas.1817380116>.
#
# Occupation basis: this charges the FULL managed-grassland green ET, consistent
# with the grassland land extension's "occupation" metric (whole grassland, not
# the active-grazing sub-area). Global total ~13,700 km3/yr, within the
# 8,258-12,960 km3/yr range published for total grazing-land ET, and ~6x the
# "green WF of grazing" of Schyns (2,191 km3/yr) which restricts to the grazed
# area at the necessary livestock density. Validation: total grassland area
# 3,253 Mha matches FAO permanent pastures (~3,300 Mha).
run <- path.expand(file.path(
  "~/Nextcloud/WHEP_ERC 2025/Sources/datasets/unclassified_datasets/LPJmL",
  "LPJmL_runs",
  "global_1901-2009_spinup_200_our_inputs_grassland_livestock_npp_vegc_fix",
  "output/scenario_1"
))
grass_band <- 14L
yidx <- (2000:2009) - 1900 # time index: year 1901 = index 1
read_band_mean <- function(file, var) {
  nc <- ncdf4::nc_open(file.path(run, file))
  on.exit(ncdf4::nc_close(nc))
  arr <- ncdf4::ncvar_get(
    nc,
    var,
    start = c(1, 1, grass_band, min(yidx)),
    count = c(-1, -1, 1, length(yidx))
  )
  list(
    lon = ncdf4::ncvar_get(nc, "lon"),
    lat = ncdf4::ncvar_get(nc, "lat"),
    val = apply(arr, c(1, 2), mean, na.rm = TRUE)
  )
}
cw <- read_band_mean("cft_consump_water_g.nc", "consump_water_g")
fr <- read_band_mean("cftfrac.nc", "CFTfrac")
earth_r <- 6371007.181
d2r <- pi / 180
grid <- expand.grid(lon = cw$lon, lat = cw$lat)
cell <- tibble(
  lon = round(grid$lon, 2),
  lat = round(grid$lat, 2),
  consump_mm = as.vector(cw$val),
  frac = as.vector(fr$val)
) |>
  filter(!is.na(consump_mm)) |>
  mutate(
    cell_area_ha = earth_r^2 *
      (0.5 * d2r) *
      (sin((lat + 0.25) * d2r) - sin((lat - 0.25) * d2r)) /
      1e4,
    grass_area_ha = pmax(frac, 0) * cell_area_ha,
    m3_per_ha = consump_mm * 10
  )
# country_grid.parquet is in the gitignored WHEP LPJmL_inputs tree.
country_grid <- nanoparquet::read_parquet(
  "~/WHEP/LPJmL_inputs/whep/inputs/country_grid.parquet"
) |>
  mutate(lon = round(lon, 2), lat = round(lat, 2))
water_grazing_green <- cell |>
  inner_join(country_grid, by = c("lon", "lat")) |>
  filter(grass_area_ha > 0, !is.na(area_code)) |>
  summarise(
    m3_per_ha = round(sum(m3_per_ha * grass_area_ha) / sum(grass_area_ha), 1),
    .by = area_code
  ) |>
  filter(m3_per_ha > 0)
# upload_csv(<written water_grazing_green>, "water-grazing-green") via ~/whep_inputs
