# Prepare the water-footprint coefficient pins consumed by
# build_water_extension(). None of the coefficient values are hardcoded in the
# package; they live in pinned tables registered in inst/extdata/whep_inputs.csv
# and read at runtime via whep_read_file().
#
# Three pins are needed (aliases must match build_water_extension()):
#
#   1. wfn-water-crop      crop green/blue intensities, m3 t^-1
#   2. water-livestock-blue  livestock blue water, m3 head^-1
#   3. water-grazing-green   grazing green water, m3 ha^-1
#
# Sources (verified):
#   - Crops: Mialyk, O., Schyns, J.F., Booij, M.J., Su, H., Hogeboom, R.J.,
#     Berger, M. (2024). "Water footprints and crop water use of 175 individual
#     crops for 1990-2019 simulated with a global crop model." Scientific Data
#     11, 206. doi:10.1038/s41597-024-03051-3. Dataset on 4TU.ResearchData,
#     doi:10.4121/7b45bcc6-686b-404d-a910-13c87156716a (this is the file linked
#     in FABIO's R/12_1_ext_land_mass_water.R; despite FABIO's "Mekonnen &
#     Hoekstra" comment, the linked file is the Mialyk et al. crop-model data).
#   - Livestock: Mekonnen, M.M., Hoekstra, A.Y. (2012). "A Global Assessment of
#     the Water Footprint of Farm Animal Products." Ecosystems 15, 401-415,
#     <https://doi.org/10.1007/s10021-011-9517-8>.
#   - Grazing: FABIO's pasture-water step (input/grazing/grazing.csv); the
#     upstream provenance of those m3 ha^-1 values must be confirmed before the
#     pin is built.
#
# Uploading is manual: only someone with write access to the saco.csic.es board
# can register the pins. Use inst/scripts/prepare_upload.R (prepare_for_upload),
# then fill the printed version strings into the placeholder rows of
# inst/extdata/whep_inputs.csv (currently "PENDING_UPLOAD") and re-run the
# whep_inputs data-raw script to rebuild the package data.

library(readr)
library(dplyr)

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

# Upload (someone with board access): write wfn_water_crop to a temp CSV and
# parquet, then register it as the "wfn-water-crop" pin with the
# prepare_for_upload helper in inst scripts prepare_upload.R. Finally add the
# printed version to the wfn-water-crop row of whep_inputs.csv.

# 2. Livestock blue water (water-livestock-blue) -------------------------------
# Expected columns: item_cbs_code (live-animal CBS code, e.g. 961 non-dairy
# cattle), m3_per_head. Derive per-head blue water (drinking + servicing + feed
# processing) from Mekonnen & Hoekstra (2012) by mapping their animal categories
# to whep live-animal item_cbs codes. Coefficients NOT reproduced here pending
# extraction/verification from the source tables.

# 3. Grazing green water (water-grazing-green) ---------------------------------
# Expected columns: area_code (FAO), m3_per_ha. Port FABIO's
# input/grazing/grazing.csv once its upstream source is confirmed.
