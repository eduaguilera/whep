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
