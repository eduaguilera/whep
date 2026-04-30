# -----------------------------------------------------------------------
# download_earthstat_fertilizer.R
#
# Downloads EarthStat Crop Specific Fertilizer GeoTIFFs (Mueller et al.
# 2012) from Google Cloud Storage.
#
# Reference:
#   Mueller, N. D. et al. (2012) doi:10.1038/nature11420

download_earthstat_fertilizer <- function(dest_dir) {
  gcs_url <- "https://storage.googleapis.com/earthstat/FertilizerCropSpecific_Geotiff.zip"
  target_dir <- file.path(dest_dir, "EarthStat - Crop Specific Fertilizers")

  if (dir.exists(target_dir) &&
      length(list.dirs(target_dir, recursive = FALSE)) >= 15) {
    n_crops <- length(list.dirs(target_dir, recursive = FALSE))
    cli::cli_alert_info("EarthStat fertilizer: already extracted ({n_crops} crops)")
    return(invisible())
  }

  zip_path <- file.path(dest_dir, "FertilizerCropSpecific_Geotiff.zip")
  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading EarthStat fertilizer (~585 MB)...")
    download.file(gcs_url, zip_path, mode = "wb", method = "curl")
  }

  cli::cli_alert("Extracting...")
  utils::unzip(zip_path, exdir = dest_dir)
  file.remove(zip_path)

  n_crops <- length(list.dirs(target_dir, recursive = FALSE))
  n_tifs <- length(list.files(target_dir, pattern = "\\.tif$", recursive = TRUE))
  cli::cli_alert_success("EarthStat fertilizer: {n_crops} crops, {n_tifs} GeoTIFFs")
  invisible()
}
