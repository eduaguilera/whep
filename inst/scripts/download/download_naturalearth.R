# -----------------------------------------------------------------------
# download_naturalearth.R
#
# Downloads Natural Earth 10m admin-0 countries shapefile.
#
# Source: https://www.naturalearthdata.com (public domain)

download_naturalearth <- function(dest_dir) {
  ne_dir <- file.path(dest_dir, "NaturalEarth")
  shp_path <- file.path(ne_dir, "ne_10m_admin_0_countries.shp")

  if (file.exists(shp_path)) {
    cli::cli_alert_info("NaturalEarth: already exists")
    return(invisible())
  }

  if (!dir.exists(ne_dir)) dir.create(ne_dir, recursive = TRUE)
  url <- "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip"
  zip_path <- file.path(ne_dir, "ne_10m_admin_0_countries.zip")

  cli::cli_alert("Downloading Natural Earth shapefile (~5 MB)...")
  download.file(url, zip_path, mode = "wb", quiet = FALSE)
  utils::unzip(zip_path, exdir = ne_dir)
  file.remove(zip_path)
  cli::cli_alert_success("NaturalEarth: saved")
  invisible()
}
