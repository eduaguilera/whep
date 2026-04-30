# -----------------------------------------------------------------------
# download_earthstat_fertilizer.R
#
# Downloads EarthStat Crop Specific Fertilizer GeoTIFFs (Mueller et al.
# 2012) from Google Cloud Storage.
#
# Reference:
#   Mueller, N. D. et al. (2012) "Closing yield gaps through nutrient
#   and water management." Nature 490, 254-257.
#   doi:10.1038/nature11420
#
# Source: EarthStat (http://www.earthstat.org/), mirrored on GCS.
#   Bulk GeoTIFF zip: ~585 MB (compressed), extracts to ~1 GB
#   17 crop subdirectories with N/P/K rate rasters
#
# Output layout:
#   $WHEP_L_FILES_DIR/EarthStat - Crop Specific Fertilizers/
#     barley/
#     cassava/
#     ...
#
# Usage:
#   Rscript inst/scripts/download/download_earthstat_fertilizer.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

gcs_url <- "https://storage.googleapis.com/earthstat/FertilizerCropSpecific_Geotiff.zip"
target_dir <- file.path(l_files_dir, "EarthStat - Crop Specific Fertilizers")

cli::cli_h1("Downloading EarthStat Crop Specific Fertilizer data")

if (dir.exists(target_dir) && length(list.dirs(target_dir, recursive = FALSE)) >= 15) {
  n_crops <- length(list.dirs(target_dir, recursive = FALSE))
  cli::cli_alert_success("Already extracted: {target_dir} ({n_crops} crop directories)")
  quit(status = 0)
}

zip_path <- file.path(l_files_dir, "FertilizerCropSpecific_Geotiff.zip")

if (!file.exists(zip_path)) {
  cli::cli_alert("Downloading FertilizerCropSpecific_Geotiff.zip (~585 MB)...")
  cli::cli_alert_info("This may take 10-20 minutes.")
  download.file(gcs_url, zip_path, mode = "wb", quiet = FALSE)

  if (!file.exists(zip_path)) {
    cli::cli_abort("Download failed.")
  }
  sz <- round(file.size(zip_path) / 1024 / 1024)
  cli::cli_alert_success("Downloaded: {sz} MB")
} else {
  sz <- round(file.size(zip_path) / 1024 / 1024)
  cli::cli_alert_info("ZIP already exists ({sz} MB), extracting...")
}

cli::cli_alert("Extracting to {l_files_dir}...")
utils::unzip(zip_path, exdir = l_files_dir)

if (!dir.exists(target_dir)) {
  cli::cli_abort("Extraction did not create expected directory: {target_dir}")
}

n_crops <- length(list.dirs(target_dir, recursive = FALSE))
n_tifs <- length(list.files(target_dir, pattern = "\\.tif$", recursive = TRUE))
cli::cli_alert_success("Extracted: {n_crops} crop directories, {n_tifs} GeoTIFFs")

file.remove(zip_path)
cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
