# Reads the deposited Schulte-Uebbing archive from the package cache only when
# it is already there, so the suite never downloads it. The source's own 2010
# surplus is recovered as critical + exceedance (consistent across the four
# threshold surfaces to the 0.001 kg N/ha the rasters are written with).
.real_critn_dir <- function() {
  testthat::skip_on_cran()
  dir <- whep:::.critn_cache_dir()
  testthat::skip_if_not(
    dir.exists(whep:::.critn_root_path(dir)),
    "Schulte-Uebbing archive not in the local cache"
  )
  dir
}

# Writes a tiny 3x2-cell ESRI ASCII grid (6-line header + matrix) at the
# nested archive path read_critical_n() expects for the default
# threshold "mi" and land_use "all", so the real parser is exercised
# without the off-repo Zenodo archive.
.critical_n_write_asc <- function(dir) {
  target <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Output_files",
    "Critical N surpluses"
  )
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "ncols 3",
      "nrows 2",
      "xllcorner 0",
      "yllcorner 0",
      "cellsize 0.5",
      "NODATA_value -9999",
      "10 20 -9999",
      "40 50 60"
    ),
    file.path(target, "nsur_crit_mi_all_ph.asc")
  )
  input <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Input_files"
  )
  dir.create(input, recursive = TRUE, showWarnings = FALSE)
  header <- c(
    "ncols 3",
    "nrows 2",
    "xllcorner 0",
    "yllcorner 0",
    "cellsize 0.5",
    "NODATA_value -9999"
  )
  writeLines(
    c(header, "100 200 300", "400 500 600"),
    file.path(input, "a_crop.asc")
  )
  writeLines(
    c(header, "10 20 30", "40 50 60"),
    file.path(input, "a_gr_int.asc")
  )
  writeLines(
    c(header, "1 2 3", "4 5 6"),
    file.path(input, "image_region28.asc")
  )
  invisible(dir)
}
