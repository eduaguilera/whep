# -----------------------------------------------------------------------
# download_luh2.R
#
# Downloads LUH2 v2h NetCDF files needed by prepare_spatialize_all.R.
#
# Reference:
#   Hurtt, G. C. et al. (2020) "Harmonization of global land use change
#   and management for the period 850–2100 (LUH2) for CMIP6."
#   Geosci. Model Dev., 13, 5425-5464. doi:10.5194/gmd-13-5425-2020
#
# Source: University of Maryland
#   https://luh.umd.edu/data.shtml
#
# Files (from LUH2 v2h, historical + SSP scenarios):
#   staticData_quarterdeg.nc  (~1 MB)   — grid, cell area
#   states.nc                 (~6 GB)   — PFT fractions (c3ann, c4ann, etc.)
#   management.nc             (~1.4 GB) — irrigation fractions
#
# Not downloaded: transitions.nc (~19 GB) — not used by the pipeline.
#
# Output: $WHEP_L_FILES_DIR/LUH2/LUH2 v2h/
#
# Usage:
#   Rscript inst/scripts/download/download_luh2.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

target_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

base_url <- "https://luh.umd.edu/LUH2/LUH2_v2h"

files <- c(
  "staticData_quarterdeg.nc",
  "states.nc",
  "management.nc"
)

cli::cli_h1("Downloading LUH2 v2h data")

for (fname in files) {
  fpath <- file.path(target_dir, fname)
  if (file.exists(fpath)) {
    sz <- switch(fname,
      "staticData_quarterdeg.nc" = round(file.size(fpath) / 1024),
      round(file.size(fpath) / 1024 / 1024 / 1024, 1)
    )
    unit <- if (fname == "staticData_quarterdeg.nc") "KB" else "GB"
    cli::cli_alert_success("Already exists: {fname} ({sz} {unit})")
    next
  }

  url <- paste0(base_url, "/", fname)
  cli::cli_alert("Downloading {fname}...")
  if (fname != "staticData_quarterdeg.nc") {
    cli::cli_alert_info("This is a large file and may take 1-4 hours.")
  }

  download.file(url, fpath, mode = "wb", quiet = FALSE)

  if (!file.exists(fpath)) {
    cli::cli_abort("Download failed: {url}")
  }

  sz <- switch(fname,
    "staticData_quarterdeg.nc" = round(file.size(fpath) / 1024),
    round(file.size(fpath) / 1024 / 1024 / 1024, 1)
  )
  unit <- if (fname == "staticData_quarterdeg.nc") "KB" else "GB"
  cli::cli_alert_success("Saved: {fname} ({sz} {unit})")
}

cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
