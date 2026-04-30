# -----------------------------------------------------------------------
# download_hwsd.R
#
# Downloads HWSD v2 raster and attribute database from FAO S3,
# converting the MS Access .mdb to CSV.
#
# Reference:
#   FAO/IIASA/ISRIC/ISSCAS/JRC (2012) "Harmonized World Soil Database"

download_hwsd <- function(dest_dir) {
  hwsd_dir <- file.path(dest_dir, "HWSD")
  if (!dir.exists(hwsd_dir)) dir.create(hwsd_dir, recursive = TRUE)
  base_url <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD"

  # Raster
  bil_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(bil_path)) {
    zip_path <- file.path(hwsd_dir, "HWSD2_RASTER.zip")
    if (!file.exists(zip_path)) {
      cli::cli_alert("Downloading HWSD2 raster (~22 MB)...")
      download.file(paste0(base_url, "/HWSD2_RASTER.zip"), zip_path, mode = "wb")
    }
    utils::unzip(zip_path, exdir = hwsd_dir)
    file.remove(zip_path)
    cli::cli_alert_success("HWSD raster: saved")
  } else {
    cli::cli_alert_info("HWSD raster: already exists")
  }

  # Database → CSV
  csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
  if (file.exists(csv_path)) {
    cli::cli_alert_info("HWSD CSV: already exists")
    return(invisible())
  }

  db_zip <- file.path(hwsd_dir, "HWSD2_DB.zip")
  if (!file.exists(db_zip)) {
    cli::cli_alert("Downloading HWSD2 database (~9 MB)...")
    download.file(paste0(base_url, "/HWSD2_DB.zip"), db_zip, mode = "wb")
  }

  tmpd <- tempfile(); dir.create(tmpd)
  on.exit(unlink(tmpd, recursive = TRUE), add = TRUE)
  utils::unzip(db_zip, exdir = tmpd)
  file.remove(db_zip)

  mdb_file <- list.files(tmpd, pattern = "\\.mdb$", full.names = TRUE)
  if (length(mdb_file) == 0) stop("No .mdb file found")

  cli::cli_alert("Converting HWSD2 .mdb to CSV...")
  system2("mdb-export", c(mdb_file, "HWSD2_SMU"), stdout = file.path(tmpd, "smu.csv"))
  system2("mdb-export", c(mdb_file, "HWSD2_LAYERS"), stdout = file.path(tmpd, "layers.csv"))

  smu <- readr::read_csv(file.path(tmpd, "smu.csv"),
    col_types = readr::cols(.default = readr::col_character()), name_repair = "unique_quiet")
  layers <- readr::read_csv(file.path(tmpd, "layers.csv"),
    col_types = readr::cols(.default = readr::col_character()), name_repair = "unique_quiet")

  hwsd <- layers |>
    dplyr::filter(LAYER == "D1") |>
    dplyr::left_join(dplyr::select(smu, HWSD2_SMU_ID, SMU_SHARE = SHARE),
                     by = "HWSD2_SMU_ID") |>
    dplyr::transmute(
      mu_global = as.integer(HWSD2_SMU_ID), share = as.numeric(SMU_SHARE),
      t_sand = as.numeric(SAND), t_silt = as.numeric(SILT), t_clay = as.numeric(CLAY),
      t_usda_tex = as.integer(TEXTURE_USDA), t_ph_h2o = as.numeric(PH_WATER), wrb2 = WRB2
    ) |>
    dplyr::filter(!is.na(t_usda_tex))

  readr::write_csv(hwsd, csv_path)
  cli::cli_alert_success("HWSD CSV: {nrow(hwsd)} rows")
  invisible()
}
