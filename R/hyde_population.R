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
#   built-up-area occupancy). read_hyde_population() reads the three counts,
#   selected by `variable`: "total" (popc, the default), "urban" (urbc) or
#   "rural" (rurc). All three share one parse and one block sum, so
#   build_total_population_grid() (R/total_population_grid.R) reads popc
#   through the private .read_hyde_year() exactly as the reader does.
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

#' Read gridded HYDE population onto WHEP's grid.
#'
#' @description
#' Reads a HYDE baseline-scenario population count (native 5-arcmin ESRI
#' ASCII grid, people per native cell) for one or more calendar years and,
#' by default, aggregates it to WHEP's 0.5-degree grid by summing the 6x6
#' fine cells inside each 0.5-degree block, since population count is an
#' extensive quantity. Each requested year is read from its own
#' `"{year}AD_pop.zip"` archive, which holds the total (`popc`), urban
#' (`urbc`) and rural (`rurc`) counts on the same grid.
#'
#' @param hyde_dir Path to the directory holding the HYDE `"{year}AD_pop.zip"`
#'   archives. Defaults to `Sys.getenv("WHEP_HYDE_DIR")`.
#' @param years Integer vector of calendar years to read (`AD`, so `>= 1`).
#'   Required: each year is a real unzip-and-parse of a ~150MB archive, so
#'   there is no default range.
#' @param variable Which population count to read: `"total"` (default, HYDE
#'   `popc`), `"urban"` (`urbc`) or `"rural"` (`rurc`). The output column is
#'   named after it.
#' @param aggregate If `TRUE` (default), sum the fine cells to WHEP's
#'   0.5-degree grid. If `FALSE`, return the native 5-arcmin cells, keyed by
#'   their own centres, for a consumer whose target grid is not 0.5 degrees.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year` and one count column named
#'   after `variable`: `total_pop`, `urban_pop` or `rural_pop` (people in the
#'   cell that year). Cells HYDE marks as no-data are absent, never zero.
#' @export
#' @examples
#' read_hyde_population(example = TRUE)
#' read_hyde_population(variable = "urban", example = TRUE)
read_hyde_population <- function(
  hyde_dir = NULL,
  years = NULL,
  variable = c("total", "urban", "rural"),
  aggregate = TRUE,
  example = FALSE
) {
  variable <- rlang::arg_match(variable)
  column <- paste0(variable, "_pop")
  if (isTRUE(example)) {
    return(dplyr::rename(.example_hyde_population(), !!column := "pop"))
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
  years |>
    lapply(
      .read_hyde_year,
      hyde_dir = dir,
      variable = .hyde_member(variable),
      aggregate = isTRUE(aggregate)
    ) |>
    data.table::rbindlist() |>
    tibble::as_tibble() |>
    dplyr::rename(!!column := "pop")
}

# ---- Private helpers --------------------------------------------------

# The archive member prefix each `variable` reads.
.hyde_member <- function(variable) {
  c(total = "popc", urban = "urbc", rural = "rurc")[[variable]]
}

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

# Read one HYDE population count (`popc`, `urbc` or `rurc`) for one year and
# block-sum it to the 0.5-degree grid (or keep the native cells), as a neutral
# `pop` column the callers name. A member missing from the archive is refused
# by name rather than left to unz() to fail on: substituting another variable
# would change what the count IS.
.read_hyde_year <- function(year, hyde_dir, variable, aggregate = TRUE) {
  zip_path <- file.path(hyde_dir, paste0(year, "AD_pop.zip"))
  if (!file.exists(zip_path)) {
    cli::cli_abort("HYDE population archive not found: {.file {zip_path}}.")
  }
  arcname <- paste0(variable, "_", year, "AD.asc")
  members <- utils::unzip(zip_path, list = TRUE)$Name
  if (!arcname %in% members) {
    cli::cli_abort(c(
      "HYDE archive {.file {zip_path}} holds no {.file {arcname}}.",
      i = "It holds: {.file {members}}."
    ))
  }
  grid <- .read_hyde_asc(zip_path, arcname)
  if (!aggregate) {
    return(.hyde_native_cells(grid, year))
  }
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

# Block-sum a native 5-arcmin population matrix to WHEP's 0.5-degree grid, as
# a neutral `pop` count. Row 1 is the northernmost row (standard ESRI
# convention); NODATA cells are dropped before summing.
.hyde_block_sum <- function(grid, year) {
  dt <- .hyde_fine_cells(grid)
  if (nrow(dt) == 0L) {
    return(.hyde_empty())
  }
  dt[,
    .(year = year, pop = sum(pop)),
    by = .(lon = .hani_block_center(lon), lat = .hani_block_center(lat))
  ]
}

# The native 5-arcmin cells, keyed by their own centres, NODATA dropped.
.hyde_native_cells <- function(grid, year) {
  dt <- .hyde_fine_cells(grid)
  if (nrow(dt) == 0L) {
    return(.hyde_empty())
  }
  dt[, .(lon, lat, year = year, pop)]
}

# One row per fine cell holding data: its centre and its count.
.hyde_fine_cells <- function(grid) {
  meta <- grid$meta
  mat <- grid$matrix
  mat[mat == meta$NODATA_value] <- NA_real_
  n_row <- meta$nrows
  n_col <- meta$ncols
  lat <- 90 - meta$cellsize * (seq_len(n_row) - 0.5)
  lon <- -180 + meta$cellsize * (seq_len(n_col) - 0.5)
  dt <- data.table::data.table(
    lon = lon[rep(seq_len(n_col), times = n_row)],
    lat = lat[rep(seq_len(n_row), each = n_col)],
    pop = as.vector(t(mat))
  )
  dt[!is.na(pop)]
}

.hyde_empty <- function() {
  data.table::data.table(
    lon = double(),
    lat = double(),
    year = integer(),
    pop = double()
  )
}

# Toy fixture for a runnable example (one cell, one year), with the neutral
# `pop` column the reader renames after `variable`.
.example_hyde_population <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~pop,
    -0.25, -0.25, 2020L, 12000
  )
}
