# -----------------------------------------------------------------------
# download_all.R
#
# Unified download script for all WHEP spatialization pipeline inputs.
#
# Sources each download function and runs them in the correct order.
# All data is downloaded into `dest_dir`, which mirrors the L_files
# layout expected by prepare_spatialize_all.R.
#
# Usage:
#   source("inst/scripts/download_all.R")
#   download_all("~/MyData")       # download everything
#   download_all("~/MyData", c("faostat", "luh2"))  # specific datasets
#
# Available datasets:
#   naturalearth, faostat, luh2, monfreda, earthstat_fertilizer, mirca,
#   hydrology, coello, nitrogen, west_manure, global_dams, hwsd
# -----------------------------------------------------------------------

library(cli)

download_all <- function(dest_dir, datasets = NULL) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  dest_dir <- normalizePath(dest_dir)

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 3600)

  all_datasets <- c(
    "naturalearth", "luh2", "monfreda", "earthstat_fertilizer",
    "mirca", "hydrology", "coello", "nitrogen", "west_manure",
    "global_dams", "hwsd"
  )

  if (is.null(datasets)) datasets <- all_datasets
  datasets <- match.arg(datasets, all_datasets, several.ok = TRUE)

  # Source all download functions
  download_dir <- file.path("inst", "scripts", "download")
  if (!dir.exists(download_dir)) {
    download_dir <- system.file("scripts", "download", package = "whep")
  }
  for (f in list.files(download_dir, pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = TRUE)
  }

  cli::cli_h1("WHEP Data Download Pipeline")
  cli::cli_alert_info("Destination: {dest_dir}")

  for (ds in datasets) {
    cli::cli_h2("Downloading {ds}")
    switch(ds,
      naturalearth        = download_naturalearth(dest_dir),
      luh2                = download_luh2(dest_dir),
      monfreda            = download_monfreda(dest_dir),
      earthstat_fertilizer = download_earthstat_fertilizer(dest_dir),
      mirca               = download_mirca(dest_dir),
      hydrology           = download_hydrology(dest_dir),
      coello              = download_coello(dest_dir),
      nitrogen            = download_nitrogen(dest_dir),
      west_manure         = download_west_manure(dest_dir),
      global_dams         = download_global_dams(dest_dir),
      hwsd                = download_hwsd(dest_dir)
    )
  }

  cli::cli_h1("Done!")
  cli::cli_alert_success("All data downloaded to {dest_dir}")
  cli::cli_alert_info("Next: run prepare_spatialize_all.R")
  invisible()
}
