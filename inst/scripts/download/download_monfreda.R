# -----------------------------------------------------------------------
# download_monfreda.R
#
# Downloads Monfreda et al. (2008) harvested area and yield GeoTIFFs
# for 175 crops from Google Cloud Storage.
#
# Reference:
#   Monfreda, C. et al. (2008) doi:10.1029/2007GB002947

download_monfreda <- function(dest_dir) {
  gcs_url <- "https://storage.googleapis.com/earthstat/HarvestedAreaYield175Crops_Geotiff.zip"
  target_dir <- file.path(
    dest_dir,
    "HarvestedAreaYield175Crops_Geotiff",
    "GeoTiff"
  )

  if (
    dir.exists(target_dir) &&
      length(list.dirs(target_dir, recursive = FALSE)) >= 170
  ) {
    n_crops <- length(list.dirs(target_dir, recursive = FALSE))
    cli::cli_alert_info("Monfreda: already extracted ({n_crops} crops)")
    return(invisible())
  }

  zip_path <- file.path(dest_dir, "HarvestedAreaYield175Crops_Geotiff.zip")
  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading Monfreda yields (~902 MB)...")
    download.file(gcs_url, zip_path, mode = "wb", method = "curl")
  }

  cli::cli_alert("Extracting...")
  utils::unzip(zip_path, exdir = dest_dir)
  file.remove(zip_path)

  # Remove macOS metadata folder
  macosx_dir <- file.path(dest_dir, "__MACOSX")
  if (dir.exists(macosx_dir)) {
    unlink(macosx_dir, recursive = TRUE)
  }

  n_crops <- length(list.dirs(target_dir, recursive = FALSE))
  n_tifs <- length(list.files(
    target_dir,
    pattern = "\\.tif$",
    recursive = TRUE
  ))
  cli::cli_alert_success("Monfreda: {n_crops} crops, {n_tifs} GeoTIFFs")
  invisible()
}
