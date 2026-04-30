# -----------------------------------------------------------------------
# download_monfreda_data.R
#
# Downloads the Monfreda et al. (2008) harvested area and yield GeoTIFFs
# for 175 crops from Google Cloud Storage.
#
# Reference:
#   Monfreda, C., N. Ramankutty, and J. A. Foley (2008), Farming the
#   planet: 2. Geographic distribution of crop areas, yields,
#   physiological types, and net primary production in the year 2000,
#   Global Biogeochem. Cycles, 22, GB1022, doi:10.1029/2007GB002947
#
# Source: EarthStat (http://www.earthstat.org/), mirrored on GCS.
#   Bulk GeoTIFF zip: ~902 MB
#   Extracted: ~2.6 GB, 172 crop directories under GeoTiff/
#
# Output layout (matching what prepare_spatialize_inputs.R,
# prepare_yield_inputs.R, and prepare_nitrogen_inputs.R expect):
#   $WHEP_LFILES_DIR/
#     HarvestedAreaYield175Crops_Geotiff/
#       GeoTiff/
#         maize/
#           maize_YieldPerHectare.tif
#           maize_HarvestedAreaHectares.tif
#           ...
#         wheat/
#         ...
#
# Usage:
#   Rscript inst/scripts/download_monfreda_data.R
# -----------------------------------------------------------------------

# ---- Configuration ----------------------------------------------------

gcs_base <- "https://storage.googleapis.com/earthstat"
zip_name <- "HarvestedAreaYield175Crops_Geotiff.zip"
gcs_url  <- paste0(gcs_base, "/", zip_name)

l_files_dir <- Sys.getenv("WHEP_LFILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_LFILES_DIR environment variable is not set.")
}

# Final extracted directory the prepare scripts look for
target_dir <- file.path(
  l_files_dir,
  "HarvestedAreaYield175Crops_Geotiff",
  "GeoTiff"
)

# ---- Main -------------------------------------------------------------

cli::cli_h1("Downloading Monfreda et al. 2008 harvested area and yield data")

if (dir.exists(target_dir) &&
    length(list.dirs(target_dir, recursive = FALSE)) >= 170) {
  n_crops <- length(list.dirs(target_dir, recursive = FALSE))
  cli::cli_alert_success(
    "Already extracted: {target_dir} ({n_crops} crop directories)"
  )
  cli::cli_alert_info("Delete this directory to re-download.")
  quit(status = 0)
}

# Download zip to L_files root
zip_path <- file.path(l_files_dir, zip_name)

if (!file.exists(zip_path)) {
  cli::cli_alert("Downloading bulk GeoTIFF zip (~902 MB)...")
  cli::cli_alert_info(
    "This may take 10-30 minutes depending on connection speed."
  )
  cli::cli_alert_info("Source: {gcs_url}")

  download.file(gcs_url, zip_path, mode = "wb", quiet = FALSE)

  if (!file.exists(zip_path)) {
    cli::cli_abort("Download failed.")
  }

  sz <- round(file.size(zip_path) / 1024 / 1024)
  cli::cli_alert_success("Downloaded: {zip_path} ({sz} MB)")
} else {
  sz <- round(file.size(zip_path) / 1024 / 1024)
  cli::cli_alert_info("ZIP already exists ({sz} MB), extracting...")
}

# Extract
cli::cli_alert("Extracting to {l_files_dir}...")
utils::unzip(zip_path, exdir = l_files_dir)

# Verify expected directory structure
if (!dir.exists(target_dir)) {
  cli::cli_abort(c(
    "Extraction did not create expected directory: {target_dir}",
    "i" = "Check the zip contents and adjust extraction manually."
  ))
}

n_crops <- length(list.dirs(target_dir, recursive = FALSE))
n_tifs <- length(list.files(target_dir, pattern = "\\.tif$", recursive = TRUE))
cli::cli_alert_success(
  "Extracted: {target_dir} ({n_crops} crop directories, {n_tifs} GeoTIFFs)"
)

# Remove zip to free space
cli::cli_alert("Removing zip file to free space...")
file.remove(zip_path)
cli::cli_alert_success("Done! Ready for prepare_spatialize_inputs.R and prepare_yield_inputs.R.")
