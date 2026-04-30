# -----------------------------------------------------------------------
# download_hwsd.R
#
# Downloads HWSD v2 raster from FAO S3 and SQLite database from ISRIC,
# extracting topsoil attributes to CSV.
#
# Reference:
#   FAO/IIASA/ISRIC/ISSCAS/JRC (2012) "Harmonized World Soil Database"

download_hwsd <- function(dest_dir) {
  hwsd_dir <- file.path(dest_dir, "HWSD")
  if (!dir.exists(hwsd_dir)) {
    dir.create(hwsd_dir, recursive = TRUE)
  }
  base_url <- "https://s3.eu-west-1.amazonaws.com/data.gaezdev.aws.fao.org/HWSD"

  # Raster (HWSD2 zip extracts as HWSD2.bil, rename to hwsd.bil)
  bil_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(bil_path)) {
    zip_path <- file.path(hwsd_dir, "HWSD2_RASTER.zip")
    if (!file.exists(zip_path) || file.size(zip_path) == 0) {
      cli::cli_alert("Downloading HWSD2 raster (~22 MB)...")
      download.file(
        paste0(base_url, "/HWSD2_RASTER.zip"),
        zip_path,
        mode = "wb"
      )
    }
    utils::unzip(zip_path, exdir = hwsd_dir)
    file.remove(zip_path)
    # Rename HWSD2.* to hwsd.*
    for (f in list.files(hwsd_dir, pattern = "^HWSD2\\.", full.names = TRUE)) {
      file.rename(f, file.path(hwsd_dir, sub("^HWSD2", "hwsd", basename(f))))
    }
    cli::cli_alert_success("HWSD raster: saved")
  } else {
    cli::cli_alert_info("HWSD raster: already exists")
  }

  # Database → CSV (from SQLite, no mdb-export needed)
  csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
  if (file.exists(csv_path)) {
    cli::cli_alert_info("HWSD CSV: already exists")
    return(invisible())
  }

  sqlite_path <- file.path(hwsd_dir, "HWSD2.sqlite")
  if (!file.exists(sqlite_path) || file.size(sqlite_path) == 0) {
    cli::cli_alert("Downloading HWSD2 SQLite database (~1.3 GB)...")
    download.file(
      "https://www.isric.org/sites/default/files/HWSD2.sqlite",
      sqlite_path, mode = "wb"
    )
  }

  cli::cli_alert("Extracting topsoil attributes from SQLite...")
  if (!requireNamespace("RSQLite", quietly = TRUE)) install.packages("RSQLite")
  db <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  smu <- DBI::dbReadTable(db, "HWSD2_SMU") |> tibble::as_tibble()
  layers <- DBI::dbReadTable(db, "HWSD2_LAYERS") |> tibble::as_tibble()

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
  cli::cli_alert_success("HWSD CSV: {nrow(hwsd)} rows")
  invisible()
}
