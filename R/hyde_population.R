# Gridded historical population from HYDE (History Database of the Global
# Environment, baseline scenario).
#
# CONFIRMED HYDE FACTS (local file inspected; do not re-guess):
# - One ZIP per calendar year: "{year}AD_pop.zip" (e.g. "1900AD_pop.zip").
#   Years before 0 use a "{year}BC_pop.zip" naming; only AD years are
#   handled here (>= 1850 is all this package needs).
# - Each ZIP holds 5 ESRI ASCII grid (.asc) files with the same year suffix:
#   popc (total population count), popd (population density), urbc (URBAN
#   population count), rurc (rural population count), uopp (urban
#   built-up-area occupancy). This reader uses urbc: it is the correct
#   variable for an urban/human-excreta-to-agriculture nitrogen stream.
# - ASC header, 6 lines in this exact order, then the data matrix:
#     ncols 4320
#     nrows 2160
#     xllcorner -180.0
#     yllcorner -90.0
#     cellsize 0.0833333
#     NODATA_value -9999.0
#   This is a 5-arcmin grid (0.0833333 deg = 1/12 deg), CONFIRMED IDENTICAL
#   grid geometry to the HaNi deposition grid in R/n_deposition.R (4320 lon x
#   2160 lat, same cellsize): the same 6x6-fine-cell-per-0.5-degree-block
#   aggregation and block-center mapping apply.
# - Row 1 of the data matrix is the NORTHERNMOST row (standard ESRI
#   convention: data starts at yllcorner + cellsize*nrows and decreases).
#   Row i (1-indexed from the top) has lat_center = 90 - cellsize*(i - 0.5);
#   column j (1-indexed from the left) has
#   lon_center = -180 + cellsize*(j - 0.5).
# - Population count is an extensive quantity (like the HaNi deposition
#   mass): aggregating 5-arcmin cells to a 0.5-degree block must SUM (not
#   average) the 36 fine cells, since a block's population is just the sum
#   of its fine cells' counts (no area normalisation needed, unlike a rate).
# - NODATA_value -9999 cells are dropped (treated as NA), never summed as a
#   literal value.
# - Local dev data dir is read from Sys.getenv("WHEP_HYDE_DIR"); never
#   hardcode an absolute path in committed code.

#' Read gridded HYDE urban population onto WHEP's grid.
#'
#' @description
#' Reads the HYDE baseline-scenario urban population count (`urbc`, native
#' 5-arcmin ESRI ASCII grid, total people per native cell) for one or more
#' calendar years and aggregates it to WHEP's 0.5-degree grid by summing the
#' 6x6 fine cells inside each 0.5-degree block, since population count is an
#' extensive quantity. Each requested year is read from its own
#' `"{year}AD_pop.zip"` archive.
#'
#' @param hyde_dir Path to the directory holding the HYDE `"{year}AD_pop.zip"`
#'   archives. Defaults to `Sys.getenv("WHEP_HYDE_DIR")`.
#' @param years Integer vector of calendar years to read (`AD`, so `>= 1`).
#'   Required: each year is a real unzip-and-parse of a ~150MB archive, so
#'   there is no default range.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `urban_pop` (total urban
#'   population in the 0.5-degree cell that year).
#' @export
#' @examples
#' read_hyde_population(example = TRUE)
read_hyde_population <- function(
  hyde_dir = NULL,
  years = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_hyde_population())
  }
  if (is.null(years)) {
    cli::cli_abort(c(
      "{.arg years} must be specified.",
      i = "Each requested year reads and parses a real ~150MB HYDE archive;
           pass the calendar year(s) you need explicitly."
    ))
  }
  .check_hyde_years(years)
  dir <- .resolve_hyde_dir(hyde_dir)
  data.table::rbindlist(lapply(years, .read_hyde_year, hyde_dir = dir)) |>
    tibble::as_tibble()
}

# ---- Private helpers --------------------------------------------------

# Resolve the HYDE data directory from the argument, else the env var.
.resolve_hyde_dir <- function(hyde_dir) {
  resolved <- hyde_dir %||% Sys.getenv("WHEP_HYDE_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No HYDE population directory available.",
      i = "Pass {.arg hyde_dir} or set {.envvar WHEP_HYDE_DIR}."
    ))
  }
  resolved
}

# HYDE ZIPs are named by AD/BC year string; only AD (>= 1) years are handled.
.check_hyde_years <- function(years) {
  if (!is.numeric(years) || any(years < 1)) {
    cli::cli_abort(c(
      "{.arg years} must be positive (AD) calendar years.",
      i = "BC-year HYDE archives (\"{{year}}BC_pop.zip\") are not handled by
           this reader."
    ))
  }
  invisible(NULL)
}

# Read one year's urban population ZIP and block-sum it to the 0.5-degree
# grid.
.read_hyde_year <- function(year, hyde_dir) {
  zip_path <- file.path(hyde_dir, paste0(year, "AD_pop.zip"))
  if (!file.exists(zip_path)) {
    cli::cli_abort("HYDE population archive not found: {.file {zip_path}}.")
  }
  arcname <- paste0("urbc_", year, "AD.asc")
  grid <- .read_hyde_asc(zip_path, arcname)
  .hyde_block_sum(grid, year)
}

# Read one ESRI ASCII grid from inside a ZIP: the 6-line header via readLines
# (cheap), then the data matrix via read.table(skip = 6) (fast, avoids
# re-parsing the header through the slower per-line path).
.read_hyde_asc <- function(zip_path, arcname) {
  header <- readLines(unz(zip_path, arcname), n = 6)
  meta <- .parse_asc_header(header)
  mat <- as.matrix(utils::read.table(
    unz(zip_path, arcname),
    skip = 6,
    header = FALSE,
    colClasses = "numeric"
  ))
  dimnames(mat) <- NULL
  list(matrix = mat, meta = meta)
}

# Parse the 6-line ESRI ASCII grid header into its named fields.
.parse_asc_header <- function(header) {
  values <- as.numeric(sub("^\\S+\\s+", "", header))
  stats::setNames(
    as.list(values),
    sub("\\s.*$", "", header)
  )
}

# Block-sum a native 5-arcmin population matrix to WHEP's 0.5-degree grid.
# Row 1 is the northernmost row (standard ESRI convention); NODATA cells are
# dropped before summing.
.hyde_block_sum <- function(grid, year) {
  meta <- grid$meta
  mat <- grid$matrix
  mat[mat == meta$NODATA_value] <- NA_real_
  n_row <- meta$nrows
  n_col <- meta$ncols
  lat <- 90 - meta$cellsize * (seq_len(n_row) - 0.5)
  lon <- -180 + meta$cellsize * (seq_len(n_col) - 0.5)
  dt <- data.table::data.table(
    lon_block = .hani_block_center(lon)[rep(seq_len(n_col), times = n_row)],
    lat_block = .hani_block_center(lat)[rep(seq_len(n_row), each = n_col)],
    urban_pop = as.vector(t(mat))
  )
  dt[,
    .(year = year, urban_pop = sum(urban_pop, na.rm = TRUE)),
    by = .(lon = lon_block, lat = lat_block)
  ]
}

# Toy fixture for a runnable example (one cell, one year).
.example_hyde_population <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~urban_pop,
    -0.25, -0.25, 2020L, 12000
  )
}
