# -----------------------------------------------------------------------
# download_faostat.R
#
# Downloads FAOSTAT bulk CSVs from bulks-faostat.fao.org.

download_faostat <- function(dest_dir) {
  faostat_dir <- file.path(dest_dir, "FAOSTAT")
  if (!dir.exists(faostat_dir)) dir.create(faostat_dir, recursive = TRUE)

  base_url <- "https://bulks-faostat.fao.org/production"
  files <- c(
    "Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip",
    "Environment_LivestockManure_E_All_Data_(Normalized).zip",
    "Production_Crops_Livestock_E_All_Data_(Normalized).zip"
  )

  for (fname in files) {
    csv_name <- sub("\\.zip$", ".csv", fname)
    csv_path <- file.path(faostat_dir, csv_name)
    if (file.exists(csv_path)) {
      cli::cli_alert_info("FAOSTAT {csv_name}: already exists")
      next
    }
    url <- paste0(base_url, "/", fname)
    zip_path <- file.path(faostat_dir, fname)
    cli::cli_alert("Downloading {fname}...")
    download.file(url, zip_path, mode = "wb", quiet = FALSE)
    utils::unzip(zip_path, exdir = faostat_dir)
    file.remove(zip_path)
    cli::cli_alert_success("FAOSTAT {csv_name}: saved")
  }
  invisible()
}
