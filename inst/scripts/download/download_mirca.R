# -----------------------------------------------------------------------
# download_mirca.R
#
# Downloads MIRCA2000 monthly crop area grids from Goethe University Frankfurt.
#
# Reference:
#   Portmann, F. T. et al. (2010) doi:10.1029/2008GB003435

download_mirca <- function(dest_dir) {
  mirca_dir <- file.path(dest_dir, "Irrigation maps_CIRCA-2000")
  if (!dir.exists(mirca_dir)) {
    dir.create(mirca_dir, recursive = TRUE)
  }

  base_url <- "https://www.geo.uni-frankfurt.de/45218031"
  crop_names <- c(
    "01" = "Wheat",
    "02" = "Maize",
    "03" = "Rice",
    "04" = "Barley",
    "05" = "Rye",
    "06" = "Millet",
    "07" = "Sorghum",
    "08" = "Soybean",
    "09" = "Sunflower",
    "10" = "Potato",
    "11" = "Cassava",
    "12" = "Sugar cane",
    "13" = "Sugar beet",
    "14" = "Oil palm",
    "15" = "Rapeseed",
    "16" = "Groundnut",
    "17" = "Pulses",
    "18" = "Citrus",
    "19" = "Date palm",
    "20" = "Grape",
    "21" = "Cotton",
    "22" = "Cocoa",
    "23" = "Coffee",
    "24" = "Others perennial",
    "25" = "Fodder grasses",
    "26" = "Others annual"
  )

  total <- 0L
  skipped <- 0L
  for (crop_num in names(crop_names)) {
    for (wtype in c("rainfed", "irrigated")) {
      fname <- sprintf("crop_%s_%s_12.flt.gz", crop_num, wtype)
      fpath <- file.path(mirca_dir, fname)
      if (file.exists(fpath)) {
        skipped <- skipped + 1L
        next
      }

      url <- paste0(base_url, "/", fname)
      tryCatch(
        {
          download.file(url, fpath, mode = "wb", quiet = TRUE)
          total <- total + 1L
        },
        error = function(e) {
          cli::cli_alert_danger("MIRCA {fname}: {conditionMessage(e)}")
        }
      )
    }
  }
  if (skipped > 0) {
    cli::cli_alert_info("MIRCA: {skipped} already existed (skipped)")
  }
  cli::cli_alert_success("MIRCA: downloaded {total} files")
  invisible()
}
