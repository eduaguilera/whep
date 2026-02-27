# -----------------------------------------------------------------------
# download_coello_data.R
#
# Downloads the Coello et al. (2025) global crop-specific fertilization
# dataset (Prediction_corrected.csv) from the companion GitHub repo:
#   https://github.com/STAN-UAntwerp/fertilizers_use_by_crop
#
# Reference:
#   Coello et al. (2025) "A global gridded crop-specific fertilization
#   dataset from 1961 to 2019"
#   doi:10.1038/s41597-024-04215-x
#   Data: doi:10.6084/m9.figshare.25435432
#
# The CSV contains ML-predicted, country-total-corrected N, P2O5 and
# K2O application rates (kg/ha) for 13 crop groups, 1961–2019, at
# country level.
#
# Columns:
#   FAOStat_area_code  – FAOSTAT country code (numeric)
#   Crop_Code          – Crop group code (1_1, 1_2, …, 7)
#   Year               – Year (1961–2019)
#   predicted_N_avg_app_cor    – N application rate (kg/ha)
#   predicted_P2O5_avg_app_cor – P2O5 application rate (kg/ha)
#   predicted_K2O_avg_app_cor  – K2O application rate (kg/ha)
#
# Usage:
#   Rscript inst/scripts/download_coello_data.R
# -----------------------------------------------------------------------

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
coello_dir <- file.path(l_files_dir, "Coello2025")

if (!dir.exists(coello_dir)) {
  dir.create(coello_dir, recursive = TRUE)
}

# -- Source URL ---
# The corrected predictions CSV from the companion GitHub repo.
# This is the country-level tabular data (~9 MB), not the 6 GB
# gridded TIFF archive on Figshare.
csv_url <- paste0(
  "https://raw.githubusercontent.com/STAN-UAntwerp/",
  "fertilizers_use_by_crop/main/",
  "prediction_corrected_byTotals/Prediction_corrected.csv"
)

dest_file <- file.path(coello_dir, "Prediction_corrected.csv")

if (file.exists(dest_file)) {
  cli::cli_alert_info("File already exists: {dest_file}")
  cli::cli_alert_info(
    "  Size: {round(file.size(dest_file) / 1024 / 1024, 1)} MB"
  )
  cli::cli_alert_info("  Delete to re-download.")
} else {
  cli::cli_alert("Downloading Coello et al. 2025 predictions...")
  cli::cli_alert_info("  Source: {csv_url}")

  tryCatch(
    {
      download.file(csv_url, dest_file, mode = "wb", quiet = FALSE)

      sz <- round(file.size(dest_file) / 1024 / 1024, 1)
      cli::cli_alert_success("Downloaded: {dest_file} ({sz} MB)")

      # Quick validation
      header <- readLines(dest_file, n = 1)
      cli::cli_alert_info("  Header: {header}")

      n_lines <- length(readLines(dest_file)) - 1
      cli::cli_alert_info("  Rows: {n_lines}")
    },
    error = function(e) {
      cli::cli_alert_danger("Download failed: {conditionMessage(e)}")
      if (file.exists(dest_file)) file.remove(dest_file)
    }
  )
}
