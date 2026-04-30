# -----------------------------------------------------------------------
# download_mirca.R
#
# Downloads MIRCA2000 monthly irrigated/rainfed crop area grids.
#
# Reference:
#   Portmann, F. T., Siebert, S. & Döll, P. (2010) "MIRCA2000 — Global
#   monthly irrigated and rainfed crop areas around the year 2000: A new
#   high-resolution data set for agricultural and hydrological modeling."
#   Global Biogeochem. Cycles, 24, GB1011. doi:10.1029/2008GB003435
#
# Source: Goethe University Frankfurt
#   https://www.uni-frankfurt.de/45218023/MIRCA
#
# Files: 52 .flt.gz files (~3.5 GB total)
#   26 crop classes × 2 types (irrigated + rainfed)
#   4320 × 2160 × 12 months, 4-byte float
#
# Output: $WHEP_L_FILES_DIR/Irrigation maps_CIRCA-2000/
#
# Usage:
#   Rscript inst/scripts/download/download_mirca.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

mirca_dir <- file.path(l_files_dir, "Irrigation maps_CIRCA-2000")
if (!dir.exists(mirca_dir)) {
  dir.create(mirca_dir, recursive = TRUE)
}

base_url <- "https://www.geo.uni-frankfurt.de/45218031"

# 26 MIRCA crop classes
crop_names <- c(
  "01" = "Wheat",
  "02" = "Maize",
  "03" = "Rice",
  "04" = "Barley",
  "05" = "Rye",
  "06" = "Millet",
  "07" = "Sorghum",
  "08" = "Soybean",
  "09" = "Sunflower",
  "10" = "Potato",
  "11" = "Cassava",
  "12" = "Sugar cane",
  "13" = "Sugar beet",
  "14" = "Oil palm",
  "15" = "Rapeseed",
  "16" = "Groundnut",
  "17" = "Pulses",
  "18" = "Citrus",
  "19" = "Date palm",
  "20" = "Grape",
  "21" = "Cotton",
  "22" = "Cocoa",
  "23" = "Coffee",
  "24" = "Others perennial",
  "25" = "Fodder grasses",
  "26" = "Others annual"
)

cli::cli_h1("Downloading MIRCA2000 crop area grids")

total <- 0L
skipped <- 0L

for (crop_num in names(crop_names)) {
  for (wtype in c("rainfed", "irrigated")) {
    fname <- sprintf("crop_%s_%s_12.flt.gz", crop_num, wtype)
    fpath <- file.path(mirca_dir, fname)

    if (file.exists(fpath)) {
      skipped <- skipped + 1L
      next
    }

    url <- sprintf("%s/%s", base_url, fname)
    cli::cli_alert("Downloading {fname} ({crop_names[crop_num]}, {wtype})...")

    tryCatch({
      download.file(url, fpath, mode = "wb", quiet = TRUE)
      if (file.exists(fpath)) {
        sz <- round(file.size(fpath) / 1024 / 1024, 1)
        total <- total + 1L
        cli::cli_alert_success("  {fname} ({sz} MB)")
      } else {
        cli::cli_alert_warning("  {fname} failed")
      }
    }, error = function(e) {
      cli::cli_alert_danger("  {fname}: {conditionMessage(e)}")
    })
  }
}

if (skipped > 0) {
  cli::cli_alert_info("{skipped} files already existed (skipped)")
}
cli::cli_alert_success("Downloaded {total} files to {mirca_dir}")
cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
