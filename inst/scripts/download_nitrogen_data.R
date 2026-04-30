# -----------------------------------------------------------------------
# download_nitrogen_data.R
#
# Downloads freely available nitrogen datasets to L_files for use in
# prepare_nitrogen_inputs.R:
#
#   1. HaNi atmospheric N deposition (Tian et al.)
#      Source: PANGAEA, dataset 942069
#      DOI: 10.1594/PANGAEA.942069
#      Files: ndep_nhx.zip, ndep_noy.zip (~5 arcmin, annual, 1850-2020)
#      Licence: CC-BY 4.0
#
#   2. Coello et al. 2025 crop-specific N/P/K rates
#      Source: GitHub (STAN-UAntwerp)
#      File: Prediction_corrected.csv
#      (delegates to download_coello_data.R)
#
# FAOSTAT, EarthStat, EuroAgriDB, West et al. 2014, and Mueller et al.
# 2012 inputs must be downloaded manually — see prepare_nitrogen_inputs.R
# header for details.
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_LFILES_DIR")

if (!nzchar(l_files_dir)) {
  stop("WHEP_LFILES_DIR environment variable is not set.")
}

# ==== 1. HaNi atmospheric N deposition ================================

.download_hani <- function(l_files_dir) {
  cli::cli_h2("HaNi atmospheric N deposition (Tian et al.)")

  hani_dir <- file.path(l_files_dir, "HaNi")
  if (!dir.exists(hani_dir)) {
    dir.create(hani_dir, recursive = TRUE)
  }

  base_url <- "https://download.pangaea.de/dataset/942069/files"

  files <- c(
    ndep_nhx = "ndep_nhx.zip",
    ndep_noy = "ndep_noy.zip"
  )

  for (nm in names(files)) {
    fname    <- files[[nm]]
    out_path <- file.path(hani_dir, fname)

    if (file.exists(out_path)) {
      sz <- round(file.size(out_path) / 1024 / 1024, 1)
      cli::cli_alert_success("Already exists: {fname} ({sz} MB)")
      next
    }

    url <- paste0(base_url, "/", fname)
    cli::cli_alert("Downloading {fname}...")
    cli::cli_alert_info(
      "This may take several minutes depending on connection speed."
    )

    download.file(url, out_path, mode = "wb", quiet = FALSE)

    if (!file.exists(out_path)) {
      cli::cli_abort("Download failed: {url}")
    }

    sz <- round(file.size(out_path) / 1024 / 1024, 1)
    cli::cli_alert_success("Saved: {out_path} ({sz} MB)")
  }

  invisible(hani_dir)
}

# ==== 2. Coello et al. 2025 ===========================================

.download_coello <- function() {
  cli::cli_h2("Coello et al. 2025 N/P/K crop rates")
  coello_script <- system.file(
    "scripts", "download_coello_data.R",
    package = "whep"
  )
  if (!nzchar(coello_script)) {
    coello_script <- file.path(
      "inst", "scripts", "download_coello_data.R"
    )
  }
  if (file.exists(coello_script)) {
    cli::cli_alert("Running download_coello_data.R...")
    source(coello_script, local = TRUE)
  } else {
    cli::cli_alert_warning(
      "download_coello_data.R not found; skipping Coello download."
    )
  }
}

# ==== Main execution ==================================================

cli::cli_h1("Downloading nitrogen data to L_files")

.download_hani(l_files_dir)
.download_coello()

cli::cli_h1("Summary")
cli::cli_alert_info(
  "HaNi zips: {file.path(l_files_dir, 'HaNi')}"
)
cli::cli_alert_info(
  "Coello CSV: {file.path(l_files_dir, 'Coello2025', 'Prediction_corrected.csv')}"
)
cli::cli_alert_success(
  "Now run inst/scripts/prepare_nitrogen_inputs.R to generate ",
  "nitrogen_inputs.parquet and n_deposition.parquet"
)
cli::cli_alert_warning(paste(
  "Manual downloads still required for prepare_nitrogen_inputs.R:",
  "  - FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv",
  "  - FAOSTAT/Environment_LivestockManure_E_All_Data_(Normalized).csv",
  "  - FAOSTAT/Production_Crops_Livestock_E_All_Data_(Normalized).csv",
  "  - EuropeAgriDB-v1.0-results/ (optional, EU countries only)",
  "  - Manure_Westetal2014/N/ (optional gridded manure)",
  "  - EarthStat - Crop Specific Fertilizers/ (optional spatial rates)",
  "  - HarvestedAreaYield175Crops_Geotiff/GeoTiff/ (optional yields)",
  sep = "\n"
))
