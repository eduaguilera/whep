# -----------------------------------------------------------------------
# download_population.R
#
# Downloads the ISIMIP3a gridded historical population product, which
# Section 9c of prepare_spatialize_all.R converts into LPJmL's popdens
# forcing (people per km2).
#
# This replaces the stock `popdens_HYDE3_1901_2011_bi.clm`, which ends in
# 2011. ISIMIP3a runs to 2021 and is already on the 0.5 degree grid, so no
# regridding is needed -- only a per-cell count to density conversion.
# (PIK's own LPJmL 6.0.5 input set ships an older ISIMIP2b file ending in
# 2005, so ISIMIP3a is the newest available for this input.)
#
# References:
#   ISIMIP3a socioeconomic input data, https://data.isimip.org
# -----------------------------------------------------------------------

ISIMIP_POPULATION_FILE <-
  "population_histsoc_30arcmin_annual_1901_2021.nc"

download_population <- function(dest_dir, timeout = 3600) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  pop_dir <- file.path(dest_dir, "ISIMIP", "pop")
  dir.create(pop_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(pop_dir, ISIMIP_POPULATION_FILE)
  if (file.exists(out_path)) {
    cli::cli_alert_info("Population: already exists")
    return(invisible(TRUE))
  }

  url <- paste0(
    "https://files.isimip.org/ISIMIP3a/InputData/socioeconomic/pop/",
    "histsoc/",
    ISIMIP_POPULATION_FILE
  )
  cli::cli_alert("Downloading ISIMIP3a population (~66 MB)...")
  # download.file() signals rather than returning non-zero on most failures,
  # and a truncated file would pass the skip check above on the next run.
  ok <- tryCatch(
    identical(
      as.integer(
        utils::download.file(url, out_path, mode = "wb", quiet = TRUE)
      ),
      0L
    ),
    error = function(e) {
      cli::cli_warn("Population: {conditionMessage(e)}")
      FALSE
    },
    warning = function(w) {
      cli::cli_warn("Population: {conditionMessage(w)}")
      FALSE
    }
  )
  if (!ok || !file.exists(out_path)) {
    unlink(out_path)
    cli::cli_warn("Population: download failed, skipping")
    return(invisible(FALSE))
  }
  cli::cli_alert_success("Population: saved")

  invisible(TRUE)
}
