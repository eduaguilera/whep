# -----------------------------------------------------------------------
# download_hwsd.R
#
# Downloads HWSD v2 (Harmonized World Soil Database) raster and
# attribute database, converting the MS Access .mdb to a CSV table.
#
# Reference:
#   FAO/IIASA/ISRIC/ISSCAS/JRC (2012) "Harmonized World Soil Database
#   (version 1.2)". FAO, Rome, Italy and IIASA, Laxenburg, Austria.
#
# Source: FAO GAEZ S3 bucket
#   https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD/
#
# Files:
#   HWSD2_RASTER.zip (~22 MB) → HWSD/hwsd.bil + .hdr
#   HWSD2_DB.zip    (~9 MB)  → HWSD/hwsd_data.csv (extracted from .mdb)
#
# The .mdb is converted to CSV using mdb-export (mdbtools).
# Only topsoil (layer D1) is exported — the 9 columns used by
# the soil pipeline.
#
# Output: $WHEP_L_FILES_DIR/HWSD/
#
# Usage:
#   Rscript inst/scripts/download/download_hwsd.R
# -----------------------------------------------------------------------

l_files_dir <- Sys.getenv("WHEP_L_FILES_DIR")
if (!nzchar(l_files_dir)) {
  stop("WHEP_L_FILES_DIR environment variable is not set.")
}

hwsd_dir <- file.path(l_files_dir, "HWSD")
if (!dir.exists(hwsd_dir)) {
  dir.create(hwsd_dir, recursive = TRUE)
}

base_url <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD"

cli::cli_h1("Downloading HWSD v2 soil data")

# ---- Raster ----
bil_path <- file.path(hwsd_dir, "hwsd.bil")
if (!file.exists(bil_path)) {
  zip_path <- file.path(hwsd_dir, "HWSD2_RASTER.zip")
  if (!file.exists(zip_path)) {
    cli::cli_alert("Downloading HWSD2_RASTER.zip (~22 MB)...")
    download.file(paste0(base_url, "/HWSD2_RASTER.zip"), zip_path, mode = "wb")
  }
  cli::cli_alert("Extracting raster...")
  utils::unzip(zip_path, exdir = hwsd_dir)
  file.remove(zip_path)
  cli::cli_alert_success("Raster: {bil_path}")
} else {
  cli::cli_alert_success("Raster already exists: {bil_path}")
}

# ---- Database → CSV ----
csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
if (file.exists(csv_path)) {
  cli::cli_alert_success("CSV already exists: {csv_path}")
  quit(status = 0)
}

db_zip <- file.path(hwsd_dir, "HWSD2_DB.zip")
if (!file.exists(db_zip)) {
  cli::cli_alert("Downloading HWSD2_DB.zip (~9 MB)...")
  download.file(paste0(base_url, "/HWSD2_DB.zip"), db_zip, mode = "wb")
}

tmpd <- tempfile()
dir.create(tmpd)
on.exit(unlink(tmpd, recursive = TRUE), add = TRUE)

cli::cli_alert("Extracting database...")
utils::unzip(db_zip, exdir = tmpd)
file.remove(db_zip)

mdb_file <- list.files(tmpd, pattern = "\\.mdb$", full.names = TRUE)
if (length(mdb_file) == 0) {
  stop("No .mdb file found after extraction")
}

cli::cli_alert("Exporting topsoil (D1) attributes to CSV via mdb-export...")

# Extract HWSD2_SMU header and HWSD2_LAYERS header separately,
# then join them. Or use a single mdb-export with a query.
# Simpler: export both tables and merge in R.
system2(
  "mdb-export",
  c(mdb_file, "HWSD2_SMU"),
  stdout = file.path(tmpd, "smu.csv")
)
system2(
  "mdb-export",
  c(mdb_file, "HWSD2_LAYERS"),
  stdout = file.path(tmpd, "layers.csv")
)

smu <- readr::read_csv(
  file.path(tmpd, "smu.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  name_repair = "unique_quiet"
)
layers <- readr::read_csv(
  file.path(tmpd, "layers.csv"),
  col_types = readr::cols(.default = readr::col_character()),
  name_repair = "unique_quiet"
)

# Select topsoil layer (D1) and join with SMU for SHARE
hwsd <- layers |>
  dplyr::filter(LAYER == "D1") |>
  dplyr::left_join(
    dplyr::select(smu, HWSD2_SMU_ID, SMU_SHARE = SHARE),
    by = "HWSD2_SMU_ID"
  ) |>
  dplyr::transmute(
    mu_global = as.integer(HWSD2_SMU_ID),
    share = as.numeric(SMU_SHARE),
    t_sand = as.numeric(SAND),
    t_silt = as.numeric(SILT),
    t_clay = as.numeric(CLAY),
    t_usda_tex = as.integer(TEXTURE_USDA),
    t_ph_h2o = as.numeric(PH_WATER),
    wrb2 = WRB2
  ) |>
  dplyr::filter(!is.na(t_usda_tex))

readr::write_csv(hwsd, csv_path)
sz <- round(file.size(csv_path) / 1024 / 1024, 1)
cli::cli_alert_success("Saved: hwsd_data.csv ({nrow(hwsd)} rows, {sz} MB)")
cli::cli_alert_success("Done! Ready for prepare_spatialize_all.R")
