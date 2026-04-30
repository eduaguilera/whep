# -----------------------------------------------------------------------
# download_naturalearth.R
#
# Downloads Natural Earth 10m admin-0 countries shapefile.
#
# Source: Natural Earth (public domain)
#   https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
#
# File: ne_10m_admin_0_countries.zip (~4.9 MB)
#   Extracted to: L_files/NaturalEarth/
#
# Usage:
#   Rscript inst/scripts/download/download_naturalearth.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

ne_dir <- file.path(l_files_dir, "NaturalEarth")
if (!dir.exists(ne_dir)) {
  dir.create(ne_dir, recursive = TRUE)
}

shp_path <- file.path(ne_dir, "ne_10m_admin_0_countries.shp")
url <- "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip"

cli::cli_h1("Downloading Natural Earth admin-0 shapefile")

if (file.exists(shp_path)) {
  cli::cli_alert_success("Already exists: {shp_path}")
  quit(status = 0)
}

zip_path <- file.path(ne_dir, "ne_10m_admin_0_countries.zip")

cli::cli_alert("Downloading ne_10m_admin_0_countries.zip (~4.9 MB)...")
download.file(url, zip_path, mode = "wb", quiet = FALSE)

if (!file.exists(zip_path)) {
  cli::cli_abort("Download failed: {url}")
}

cli::cli_alert("Extracting...")
utils::unzip(zip_path, exdir = ne_dir)
file.remove(zip_path)

if (file.exists(shp_path)) {
  n_files <- length(list.files(ne_dir, pattern = "ne_10m_admin_0_countries"))
  cli::cli_alert_success("Saved: {n_files} files to {ne_dir}")
} else {
  cli::cli_abort("Shapefile not found after extraction.")
}

cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
