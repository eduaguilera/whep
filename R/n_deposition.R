# Gridded atmospheric nitrogen deposition from HaNi (Tian et al. 2022,
# doi:10.5194/essd-14-4551-2022).
#
# CONFIRMED HaNi FACTS (local file inspected; do not re-guess):
# - ndep_nhx.nc / ndep_noy.nc, each one variable (ndep_nhx / ndep_noy) on a
#   5-arcmin native grid: lon[4320] x lat[2160] x time[171].
# - Units are grams N deposited to land WITHIN the native cell (an extensive
#   mass, not a density) -- long_name "N{HX,OY}-N deposition to land within
#   the grid cell". Aggregating to WHEP's 0.5-degree grid must therefore SUM
#   the 6x6 fine cells per 0.5-degree block (mass is additive), never average
#   them: averaging grams-per-fine-cell and dividing by one fixed area
#   constant (as an earlier exploratory script did) silently mis-weights every
#   cell away from that reference latitude, because a 5-arcmin cell's true
#   area shrinks by cos(lat) toward the poles.
# - time units "years since 1850-01-01 00:00:00", calendar noleap, so time
#   index i (0-based) is calendar year 1850 + i.
# - Local dev data dir is read from Sys.getenv("WHEP_HANI_DIR"); never
#   hardcode an absolute path in committed code.

#' Read a HaNi atmospheric nitrogen deposition species onto WHEP's grid.
#'
#' @description
#' Reads one HaNi NHx or NOy deposition NetCDF (native 5-arcmin grid, total
#' grams N deposited per native cell per year) and aggregates it to WHEP's
#' 0.5-degree grid by summing the 6x6 fine cells inside each 0.5-degree
#' block, since the source quantity is an extensive mass. Returns the summed
#' mass per 0.5-degree cell; converting to a per-hectare rate needs the true
#' cell area and is done downstream by [build_n_deposition()].
#'
#' @param species Which HaNi species to read, `"nhx"` or `"noy"`.
#' @param hani_dir Path to the directory holding `ndep_nhx.nc` and
#'   `ndep_noy.nc`. Defaults to `Sys.getenv("WHEP_HANI_DIR")`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   reads every year present in the file.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `value_g` (total grams N
#'   deposited in the 0.5-degree cell that year).
#' @export
#' @examples
#' read_n_deposition(example = TRUE)
read_n_deposition <- function(
  species = c("nhx", "noy"),
  hani_dir = NULL,
  years = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_hani_species())
  }
  species <- rlang::arg_match(species)
  rlang::check_installed("ncdf4")
  path <- file.path(
    .resolve_hani_dir(hani_dir),
    paste0("ndep_", species, ".nc")
  )
  .read_hani_nc(path, paste0("ndep_", species), years)
}

#' Build gridded atmospheric nitrogen deposition inputs.
#'
#' @description
#' Combines HaNi NHx and NOy deposition into a total nitrogen deposition rate
#' per WHEP grid cell, using the true latitude-dependent 0.5-degree cell area
#' to convert the deposited mass into a per-hectare rate, and a polity share of
#' the cell to derive the absolute mass a polity receives.
#'
#' The cell's deposited mass is split across the polities holding the cell in
#' proportion to `polity_area_ha`, the geodesic territory each holds in it, as
#' [build_polycell_support()] measures it. The transitional alternative is
#' `polity_frac`, the subcell-count share [build_cell_polity()] carries, which
#' is quantised to 1/36 of a cell; it stays selectable so the two partitions
#' can be compared, and it is what a support table carrying no
#' `polity_area_ha` is split by. Either way the split is a share of the cell,
#' so the source mass is redistributed and never created or destroyed.
#'
#' Each polity's share is then decomposed over the territory it lands on:
#' land, inland water and ice, the three separately addressable categories
#' `build_polycell_support()` carries. Deposition to freshwater is a real flux
#' on the eutrophication pathway rather than a rounding error, so the three are
#' reported side by side and the consumer chooses. A support with no category
#' columns cannot be decomposed and carries the single `"territory"` category,
#' which says the row is undecomposed rather than claiming it is land.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the inputs cover.
#' @param data Optional named list of pre-loaded inputs: `nhx` and `noy`
#'   (each `lon`, `lat`, `year`, `value_g`, falling back to
#'   [read_n_deposition()] when absent) and `cell_polity` (`lon`, `lat`,
#'   `area_code`, `cell_area_ha` and the `split` key column, required).
#' @param split Which polity share splits the cell's deposited mass:
#'   `"auto"` (default) takes `polity_area_ha` when the support carries it and
#'   `polity_frac` otherwise, `"polity_area_ha"` and `"polity_frac"` demand
#'   that key and abort when it is absent. The resolved key is recorded in the
#'   `method_polity_split` output column, so a table's split is readable from
#'   the table.
#' @param categories How each polity's share is decomposed over the territory
#'   it lands on: `"auto"` (default) decomposes when the support carries
#'   `land_area_ha`, `inland_water_ha` and `ice_area_ha` and emits the single
#'   `"territory"` category otherwise, `"land_water_ice"` demands those columns
#'   and aborts when they are absent, `"none"` keeps one undecomposed row per
#'   polycell. The resolved choice is recorded in `method_area_split`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`, `area_category`,
#'   `deposition_kgn_ha`, `deposition_n_t`, `method_deposition`,
#'   `method_polity_split` and `method_area_split`, plus the polity columns
#'   below.
#'
#'   `area_category` is `"land"`, `"inland_water"` or `"ice"` under
#'   `"land_water_ice"`, and `"territory"` under `"none"`. Summing
#'   `deposition_n_t` over the categories of a polycell recovers that
#'   polycell's whole share, so an unfiltered sum over the table is still the
#'   source mass; a consumer wanting one category **must filter**.
#'
#'   `deposition_kgn_ha` is the whole-cell mean rate: the cell's total mass
#'   over its whole area, so every polity of a cell carries the same rate on
#'   every category row and the rate is **not** conserved on re-aggregation.
#'   Only `deposition_n_t` is a mass.
#'
#'   Rows are keyed on `area_code`. `build_polycell_support()` keys on
#'   `polity_code` and does not derive the reporting vocabulary (DA-23), and
#'   `polity_area_crosswalk` folds distinct polities into one `area_code`, so a
#'   support table must be converted to one row per cell and `area_code`
#'   **before** it is passed here. That conversion is refused rather than
#'   performed silently.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_n_deposition(example = TRUE)
build_n_deposition <- function(
  years = NULL,
  data = list(),
  split = c("auto", "polity_area_ha", "polity_frac"),
  categories = c("auto", "land_water_ice", "none"),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_deposition())
  }
  split <- rlang::arg_match(split)
  categories <- rlang::arg_match(categories)
  nhx <- data$nhx %||% read_n_deposition("nhx", years = years)
  noy <- data$noy %||% read_n_deposition("noy", years = years)
  nhx <- .nd_filter_years(nhx, years)
  noy <- .nd_filter_years(noy, years)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code")
  key <- .nd_resolve_split(polity, split)
  categories <- .nd_resolve_categories(polity, categories)
  share <- .nd_category_shares(.nd_polity_share(polity, key), categories)
  .nd_assemble(nhx, noy, share, key, categories) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers --------------------------------------------------

.nd_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

# Resolve the HaNi data directory from the argument, else the env var.
.resolve_hani_dir <- function(hani_dir) {
  resolved <- hani_dir %||% Sys.getenv("WHEP_HANI_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No HaNi deposition directory available.",
      i = "Pass {.arg hani_dir} or set {.envvar WHEP_HANI_DIR}."
    ))
  }
  resolved
}

# Read one HaNi NetCDF, aggregating the native 5-arcmin grid to WHEP's
# 0.5-degree grid by summing the 36 fine cells per block (mass-conservative;
# see the file-header note on why this must be a sum, not a mean). Reads and
# aggregates one requested year at a time to bound memory on the ~9.3M-cell
# native grid.
.read_hani_nc <- function(path, netcdf_var, years) {
  if (!file.exists(path)) {
    cli::cli_abort("HaNi deposition file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  first_year <- 1850L
  n_time <- nc$dim[["time"]]$len
  available_years <- first_year + seq_len(n_time) - 1L
  time_idx <- if (is.null(years)) {
    seq_len(n_time)
  } else {
    which(available_years %in% years)
  }
  if (length(time_idx) == 0L) {
    return(tibble::tibble(
      lon = double(),
      lat = double(),
      year = integer(),
      value_g = double()
    ))
  }
  layers <- lapply(time_idx, function(ti) {
    .read_hani_year(nc, netcdf_var, lon, lat, ti, available_years[ti])
  })
  data.table::rbindlist(layers) |> tibble::as_tibble()
}

# Read one native-grid time slab and block-sum it to the 0.5-degree grid.
.read_hani_year <- function(nc, netcdf_var, lon, lat, time_idx, year) {
  slab <- ncdf4::ncvar_get(
    nc,
    netcdf_var,
    start = c(1, 1, time_idx),
    count = c(-1, -1, 1)
  )
  dt <- data.table::data.table(
    lon_block = .hani_block_center(lon)[rep(
      seq_along(lon),
      times = length(lat)
    )],
    lat_block = rep(.hani_block_center(lat), each = length(lon)),
    value_g = as.vector(slab)
  )
  dt[,
    .(year = year, value_g = sum(value_g, na.rm = TRUE)),
    by = .(lon = lon_block, lat = lat_block)
  ]
}

# Map native-grid coordinates to their enclosing 0.5-degree cell center.
.hani_block_center <- function(coord) {
  floor(coord / 0.5) * 0.5 + 0.25
}

# Validate the support and attach `polity_share`, the fraction of the cell's
# deposited mass each row takes, under the key `.nd_resolve_split()` settled on.
.nd_polity_share <- function(support, key) {
  .check_columns(support, c(key, "cell_area_ha"), "cell_polity")
  if (key == "polity_area_ha") {
    return(.nd_area_share(support))
  }
  # The transitional key. `polity_frac` is used exactly as supplied, with no
  # renormalisation, because the crosswalk already is a partition and a support
  # that is not must lose mass visibly rather than be repaired here.
  dplyr::mutate(support, polity_share = .data$polity_frac)
}

# "auto" takes the finest partition the support carries. An explicit choice is
# never silently downgraded: naming a key the support lacks aborts, so a caller
# that means to split geodesically cannot be handed crosswalk numbers instead.
.nd_resolve_split <- function(support, split) {
  if (split != "auto") {
    return(split)
  }
  if (rlang::has_name(support, "polity_area_ha")) {
    "polity_area_ha"
  } else {
    "polity_frac"
  }
}

# The geodesic split: each polity takes the cell's mass in proportion to the
# territory it holds in that cell (DA-10). Dividing by the cell's own total,
# never by the polity's own area, is what keeps this a partition -- a share per
# polycell's own hectares would let every polity of a shared cell recover the
# whole cell mass, emitting it once per polity.
.nd_area_share <- function(support) {
  .nd_check_area_values(support)
  .nd_check_area_key(support)
  support |>
    dplyr::mutate(
      polity_share = .data$polity_area_ha / sum(.data$polity_area_ha),
      .by = c("lon", "lat")
    )
}

# A cell whose territory sums to zero has no partition, and dividing by it
# would hand every polity of that cell an `NaN` share that later sums away to
# nothing. An `NA` area is worse still: it would silently delete one polity's
# claim while leaving the others' shares looking like a complete partition.
.nd_check_area_values <- function(support) {
  area <- support$polity_area_ha
  if (!is.numeric(area) || anyNA(area) || any(!is.finite(area) | area < 0)) {
    cli::cli_abort(
      "{.field cell_polity$polity_area_ha} must be finite and non-negative."
    )
  }
  empty <- dplyr::summarise(
    support,
    total = sum(.data$polity_area_ha),
    .by = c("lon", "lat")
  )
  if (any(empty$total <= 0)) {
    cli::cli_abort(c(
      "{sum(empty$total <= 0)} cell{?s} hold no territory to split.",
      i = "A cell with {.code sum(polity_area_ha) == 0} has no partition."
    ))
  }
}

# `build_polycell_support()` keys on `polity_code`, and `polity_area_crosswalk`
# folds distinct polities into one `area_code` (Sudan and South Sudan share
# 206) or leaves it `NA`. Deposition rows are keyed on `area_code`, so folding
# here would silently merge two territories' shares or pour a cell's mass into
# an unjoinable `NA` bucket. The conversion belongs at the caller's boundary
# (DA-23), so it is refused rather than performed.
.nd_check_area_key <- function(support) {
  dup <- support |>
    dplyr::count(.data$lon, .data$lat, .data$area_code, name = "n_rows") |>
    dplyr::filter(.data$n_rows > 1L | is.na(.data$area_code))
  if (nrow(dup) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg cell_polity} must hold one row per cell and {.field area_code}.",
    x = "{nrow(dup)} cell-{.field area_code} group{?s} {?is/are} duplicated
         or {.val NA}.",
    i = "Convert {.field polity_code} to {.field area_code} before calling;
         deposition rows are keyed on {.field area_code} and will not fold
         two polities into one silently."
  ))
}

# The DA-3 territory decomposition, in output-label order. `polity_area_ha` is
# their sum, which is what makes the three shares a partition of the polity's
# claim on the cell rather than three independent multipliers.
.nd_category_cols <- function() {
  c(
    land = "land_area_ha",
    inland_water = "inland_water_ha",
    ice = "ice_area_ha"
  )
}

# "auto" decomposes whenever the support can be decomposed. An explicit choice
# is never silently downgraded, exactly as for the split key: asking for
# "land_water_ice" without the columns aborts in .nd_category_shares() rather
# than quietly returning one undecomposed row per polycell, which would look
# identical to a decomposition whose water and ice happened to be zero.
.nd_resolve_categories <- function(support, categories) {
  if (categories != "auto") {
    return(categories)
  }
  if (all(rlang::has_name(support, .nd_category_cols()))) {
    "land_water_ice"
  } else {
    "none"
  }
}

# Expand each polycell into one row per territory category, carrying
# `category_frac`, the fraction of that polycell's own claim the category
# holds. Multiplying `polity_share` by it keeps the cell's mass exactly
# partitioned: the fractions sum to 1 per polycell (S-A1), so the categories
# sum back to the polycell's share and the polycells sum back to the cell.
.nd_category_shares <- function(support, categories) {
  if (categories == "none") {
    return(dplyr::mutate(
      support,
      area_category = "territory",
      category_frac = 1
    ))
  }
  cols <- .nd_category_cols()
  .check_columns(support, c(unname(cols), "polity_area_ha"), "cell_polity")
  .nd_check_category_values(support, cols)
  .nd_check_category_sum(support, cols)
  support |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(unname(cols)),
      names_to = "area_category",
      values_to = "category_area_ha"
    ) |>
    dplyr::mutate(
      area_category = names(cols)[match(.data$area_category, cols)],
      # A polycell holding no territory takes no mass either (its
      # `polity_share` is 0), so its categories are all 0 and the ratio is
      # 0/0. Left as NaN it would poison every downstream sum with NA.
      category_frac = dplyr::if_else(
        .data$polity_area_ha > 0,
        .data$category_area_ha / .data$polity_area_ha,
        0
      )
    ) |>
    dplyr::select(-"category_area_ha")
}

# A negative or non-finite category is not a share of anything: it would let
# one category borrow mass from another while the three still summed to the
# polycell's territory, so the conservation check alone would not see it.
.nd_check_category_values <- function(support, cols) {
  bad <- purrr::map_lgl(unname(cols), function(col) {
    value <- support[[col]]
    !is.numeric(value) || anyNA(value) || any(!is.finite(value) | value < 0)
  })
  if (!any(bad)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{.field cell_polity${unname(cols)[bad]}} must be finite and
     non-negative."
  )
}

# S-A1: land + inland water + ice is the polity's territory in the cell. A
# support that breaks it is not a decomposition, and splitting by it would
# hand the cell's mass out in shares that do not add up to the polycell's own.
.nd_check_category_sum <- function(support, cols) {
  total <- Reduce(`+`, lapply(unname(cols), function(col) support[[col]]))
  gap <- abs(total - support$polity_area_ha) /
    pmax(support$polity_area_ha, .Machine$double.xmin)
  if (max(gap, 0) <= 1e-9) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg cell_polity} area categories must sum to
     {.field polity_area_ha}.",
    x = "{sum(gap > 1e-9)} row{?s} differ by more than 1e-9 relative
         (worst {sprintf('%.3g', max(gap))}).",
    i = "{.field {unname(cols)}} decompose {.field polity_area_ha}; they are
         not independent of it."
  ))
}

# The categories a consumer's scope resolves to, given how the table was
# decomposed. Read from the table's own `method_area_split` so a consumer
# cannot hardcode a label that silently stops matching.
#
# DA-14, decided 2026-08-06: "territory" -- land AND inland water AND ice -- is
# the default, and it is a scientific position rather than a conservative one.
# Nitrogen deposited on a lake or a glacier still drives indirect N2O and still
# reaches the eutrophication pathway, so the impact terms have to account for
# it; charging only the land share would discard real flux. "land" stays
# selectable for the purposes that want the terrestrial surface alone.
.nd_scope_categories <- function(scope, method) {
  emitted <- .nd_emitted_categories(.nd_check_area_method(method))
  if (scope == "territory") {
    return(emitted)
  }
  if (!"land" %in% emitted) {
    cli::cli_abort(c(
      "Scope {.val land} needs a decomposed territory.",
      x = "The deposition table records
           {.code method_area_split = \"none\"}, which emits only
           {.val {emitted}}.",
      i = "Pass a {.arg cell_polity} carrying {.field land_area_ha},
           {.field inland_water_ha} and {.field ice_area_ha}, or use scope
           {.val territory}."
    ))
  }
  "land"
}

# What a decomposition puts in `area_category`. "none" says the row is
# undecomposed territory rather than claiming it is land, so the two are never
# confusable after the fact.
.nd_emitted_categories <- function(method) {
  if (method == "none") {
    "territory"
  } else {
    names(.nd_category_cols())
  }
}

.nd_check_area_method <- function(method) {
  method <- unique(method)
  if (length(method) != 1L || !method %in% c("none", "land_water_ice")) {
    cli::cli_abort("Unknown {.field method_area_split}: {.val {method}}.")
  }
  method
}

# Combine NHx + NOy mass, convert to a per-hectare rate using the true cell
# area, and split that mass across the cell's polities by `polity_share`.
.nd_assemble <- function(nhx, noy, polity, key, categories) {
  total <- dplyr::full_join(
    nhx,
    noy,
    by = c("lon", "lat", "year"),
    suffix = c("_nhx", "_noy")
  ) |>
    dplyr::mutate(
      value_g_total = dplyr::coalesce(.data$value_g_nhx, 0) +
        dplyr::coalesce(.data$value_g_noy, 0)
    )
  dplyr::inner_join(total, polity, by = c("lon", "lat")) |>
    dplyr::mutate(
      deposition_kgn_ha = .data$value_g_total / 1000 / .data$cell_area_ha,
      # `cell_area_ha` divides here and multiplies straight back, so it
      # cancels and the whole-cell area over-count cannot reach the mass. The
      # round trip is kept, not simplified away, so that substituting a land
      # area on one side alone stays a visible break rather than a silent
      # ~10% fall in every deposition total.
      deposition_n_t = .data$deposition_kgn_ha *
        .data$cell_area_ha *
        .data$polity_share *
        .data$category_frac /
        1000,
      method_deposition = "hani",
      method_polity_split = key,
      method_area_split = categories
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "area_category",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition",
      "method_polity_split",
      "method_area_split"
    )
}

# Toy fixture for a runnable example (one native-grid deposition species).
.example_hani_species <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 30800000
  )
}

# Toy fixture for the runnable example (one cell, one polity, one year, its
# territory 90% land / 8% inland water / 2% ice).
.example_n_deposition <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~area_category, ~deposition_kgn_ha,
    ~deposition_n_t, ~method_deposition, ~method_polity_split,
    ~method_area_split,
    -0.25, -0.25, 1L, 2020L, "land", 15, 41.58, "hani", "polity_area_ha",
    "land_water_ice",
    -0.25, -0.25, 1L, 2020L, "inland_water", 15, 3.696, "hani",
    "polity_area_ha", "land_water_ice",
    -0.25, -0.25, 1L, 2020L, "ice", 15, 0.924, "hani", "polity_area_ha",
    "land_water_ice"
  ) |>
    .add_reporting_polity_columns()
}
