# Skip guard for the real-data HWSD smoke tests (whep#596).
#
# hwsd_data.csv is derived locally, so a partial extract -- one written before a
# column the reader now needs existed -- is an ordinary state. A guard that
# checks only that the file exists turns that into a hard suite error naming a
# dplyr internal instead of a skip, so this asks the package itself which of the
# required columns the extract is missing: the guard then states exactly the
# precondition the reader enforces and cannot drift from it.
#
# Returns the HWSD directory so a caller can pass it on; skips otherwise.
.skip_unless_hwsd_columns <- function(required) {
  hwsd_dir <- Sys.getenv("WHEP_HWSD_DIR")
  testthat::skip_if(!nzchar(hwsd_dir), "WHEP_HWSD_DIR not set.")
  testthat::skip_if(
    !file.exists(file.path(hwsd_dir, "hwsd.bil")),
    "HWSD raster hwsd.bil not available."
  )
  absent <- whep:::.hwsd_missing_columns(hwsd_dir, required)
  testthat::skip_if(
    length(absent) > 0,
    paste0(
      "Local HWSD extract lacks ",
      paste(absent, collapse = ", "),
      "; re-export it with inst/scripts/export_hwsd_attributes.R."
    )
  )
  hwsd_dir
}
