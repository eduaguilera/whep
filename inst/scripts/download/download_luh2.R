# -----------------------------------------------------------------------
# download_luh2.R
#
# Downloads LUH2 v2h NetCDFs from luh.umd.edu.
#
# Reference:
#   Hurtt, G. C. et al. (2020) doi:10.5194/gmd-13-5425-2020

download_luh2 <- function(dest_dir) {
  target_dir <- file.path(dest_dir, "LUH2", "LUH2 v2h")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  base_url <- "https://luh.umd.edu/LUH2/LUH2_v2h"
  files <- c("staticData_quarterdeg.nc", "states.nc", "management.nc")

  for (fname in files) {
    fpath <- file.path(target_dir, fname)
    if (file.exists(fpath)) {
      unit <- if (fname == "staticData_quarterdeg.nc") "KB" else "GB"
      sz <- if (fname == "staticData_quarterdeg.nc") {
        round(file.size(fpath) / 1024)
      } else {
        round(file.size(fpath) / 1024 / 1024 / 1024, 1)
      }
      cli::cli_alert_info("LUH2 {fname}: already exists ({sz} {unit})")
      next
    }
    cli::cli_alert("Downloading LUH2 {fname}...")
    if (fname != "staticData_quarterdeg.nc") {
      cli::cli_alert_info("Large file, may take 1-4 hours")
    }
    # luh.umd.edu has an expired SSL cert; use libcurl -k, fall back to default
    dl_result <- tryCatch(
      download.file(
        paste0(base_url, "/", fname), fpath,
        mode = "wb", method = "libcurl", extra = "-k"
      ),
      error = function(e) 1L
    )
    if (dl_result != 0) {
      download.file(paste0(base_url, "/", fname), fpath, mode = "wb")
    }
    cli::cli_alert_success("LUH2 {fname}: saved")
  }
  invisible()
}
