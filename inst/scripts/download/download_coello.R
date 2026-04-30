# -----------------------------------------------------------------------
# download_coello.R
#
# Downloads the Coello et al. (2025) global crop-specific fertilization
# dataset from the companion GitHub repo.
#
# Reference:
#   Coello et al. (2025) doi:10.1038/s41597-024-04215-x

download_coello <- function(dest_dir) {
  coello_dir <- file.path(dest_dir, "Coello2025")
  if (!dir.exists(coello_dir)) dir.create(coello_dir, recursive = TRUE)

  csv_url <- paste0(
    "https://raw.githubusercontent.com/STAN-UAntwerp/",
    "fertilizers_use_by_crop/main/",
    "prediction_corrected_byTotals/Prediction_corrected.csv"
  )
  dest_file <- file.path(coello_dir, "Prediction_corrected.csv")

  if (file.exists(dest_file)) {
    cli::cli_alert_info("Coello: already exists ({round(file.size(dest_file) / 1024 / 1024, 1)} MB)")
    return(invisible())
  }

  cli::cli_alert("Downloading Coello et al. 2025 predictions...")
  tryCatch({
    download.file(csv_url, dest_file, mode = "wb", quiet = FALSE)
    sz <- round(file.size(dest_file) / 1024 / 1024, 1)
    cli::cli_alert_success("Coello: {sz} MB")
  }, error = function(e) {
    cli::cli_alert_danger("Coello download failed: {conditionMessage(e)}")
    if (file.exists(dest_file)) file.remove(dest_file)
  })
  invisible()
}
