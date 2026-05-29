# -----------------------------------------------------------------------
# download_elevation.R
#
# Downloads WorldClim 2.1 global elevation (10 arcmin).
#
# Reference:
#   Fick & Hijmans (2017) WorldClim 2, Int. J. Climatology 37(12).

download_elevation <- function(dest_dir) {
  elev_dir <- file.path(dest_dir, "WorldClim", "elevation")
  elev_path <- file.path(elev_dir, "wc2.1_10m_elev.tif")
  if (file.exists(elev_path)) {
    cli::cli_alert_info("Elevation: already exists")
    return(invisible())
  }
  if (!dir.exists(elev_dir)) {
    dir.create(elev_dir, recursive = TRUE)
  }
  zip_path <- file.path(elev_dir, "wc2.1_10m_elev.zip")
  cli::cli_alert("Downloading WorldClim elevation (~8 MB)...")
  download.file(
    "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_10m_elev.zip",
    zip_path,
    mode = "wb",
    quiet = FALSE
  )
  utils::unzip(zip_path, exdir = elev_dir)
  unlink(zip_path)
  cli::cli_alert_success("Elevation: saved to {elev_path}")
  invisible()
}
