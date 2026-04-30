# -----------------------------------------------------------------------
# download_faostat.R
#
# Downloads FAOSTAT bulk CSVs needed by prepare_spatialize_all.R.
#
# Files (from https://www.fao.org/faostat/en/#data/):
#   1. Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip  (~2 MB)
#      → L_files/FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv
#   2. Environment_LivestockManure_E_All_Data_(Normalized).zip (~25 MB)
#      → L_files/FAOSTAT/Environment_LivestockManure_E_All_Data_(Normalized).csv
#   3. Production_Crops_Livestock_E_All_Data_(Normalized).zip  (~34 MB)
#      → L_files/FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).csv
#
# FAOSTAT license: CC-BY-NC-SA 3.0 IGO
#   https://www.fao.org/contact-us/terms/db-terms-of-use/en/
#
# Usage:
#   Rscript inst/scripts/download/download_faostat.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

faostat_dir <- file.path(l_files_dir, "FAOSTAT")
if (!dir.exists(faostat_dir)) {
  dir.create(faostat_dir, recursive = TRUE)
}

base_url <- "https://bulks-faostat.fao.org/production"

files <- c(
  "Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip",
  "Environment_LivestockManure_E_All_Data_(Normalized).zip",
  "Production_Crops_Livestock_E_All_Data_(Normalized).zip"
)

cli::cli_h1("Downloading FAOSTAT bulk data")

for (fname in files) {
  csv_name <- sub("\\.zip$", ".csv", fname)
  csv_path <- file.path(faostat_dir, csv_name)

  if (file.exists(csv_path)) {
    sz <- round(file.size(csv_path) / 1024 / 1024, 1)
    cli::cli_alert_success("Already exists: {csv_name} ({sz} MB)")
    next
  }

  url <- paste0(base_url, "/", fname)
  zip_path <- file.path(faostat_dir, fname)

  cli::cli_alert("Downloading {fname}...")
  download.file(url, zip_path, mode = "wb", quiet = FALSE)

  if (!file.exists(zip_path)) {
    cli::cli_abort("Download failed: {url}")
  }

  cli::cli_alert("Extracting {fname}...")
  utils::unzip(zip_path, exdir = faostat_dir)
  file.remove(zip_path)

  if (file.exists(csv_path)) {
    sz <- round(file.size(csv_path) / 1024 / 1024, 1)
    cli::cli_alert_success("Saved: {csv_name} ({sz} MB)")
  } else {
    cli::cli_alert_warning(
      "CSV not found after extraction; check zip contents."
    )
  }
}

cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
