# EMEP MSC-W nitrogen deposition, and a year-resolved correction of HaNi's
# European trend toward it (whep#1121).
#
# CONFIRMED EMEP FACTS (local files inspected; do not re-guess):
# - EMEP01 rv5.6, 2025 reporting round: one file per year named
#   EMEP01_rv5.6_year.<YYYY>met_<YYYY>emis_rep2025.nc, ~78 MB each, on a
#   regular 0.1-degree grid (lon -30..90, lat 30..82) whose centres sit at
#   x.x5, so each WHEP 0.5-degree block nests exactly 5x5 of them.
# - Reduced (NHx) deposition is DDEP_RDN_m2Grid + WDEP_RDN, oxidised (NOy) is
#   DDEP_OXN_m2Grid + WDEP_OXN, all in mgN/m2 over the whole grid cell, so
#   x 0.01 gives kg N/ha. The `_m2Grid` dry terms are the grid-average ones;
#   `_m2Seminat` and `_m2Water_D` are ecosystem-specific and must not be
#   summed with them.
# - Unlike HaNi (grams deposited to LAND within a cell, an extensive mass),
#   EMEP is a density over the whole cell, sea included. Comparing a HaNi
#   mass divided by whole-cell area with an EMEP density therefore reads low
#   in every coastal cell, which is why the correction below works on the
#   trend, where a fixed land fraction cancels, and not on the level.

#' Read EMEP MSC-W atmospheric nitrogen deposition onto WHEP's grid.
#'
#' @description
#' Reads the yearly EMEP MSC-W model results (EMEP01 rv5.6, 2025 reporting
#' round) for one nitrogen species and aggregates the 0.1-degree grid to
#' WHEP's 0.5-degree grid. NHx is `DDEP_RDN_m2Grid + WDEP_RDN` and NOy is
#' `DDEP_OXN_m2Grid + WDEP_OXN`, dry plus wet. The source is a density
#' (mgN/m2 over the whole grid cell), so it is converted to kg N/ha
#' (x 0.01) and aggregated by plain mean: the 0.1-degree grid nests exactly
#' 5x5 inside each 0.5-degree block and the cell area varies by under 0.2 per
#' mille inside a block.
#'
#' EMEP is the reference [correct_n_deposition()] corrects HaNi's European
#' trend toward. It is a regional product (domain lon -30 to 90, lat 30 to 82)
#' and is not a substitute for HaNi anywhere else.
#'
#' The files are third-party model output and are read from local disk:
#' `inst/scripts/download/download_emep.R` fetches them from the Norwegian
#' Meteorological Institute THREDDS server into `<dest_dir>/EMEP/`, and
#' `WHEP_EMEP_DIR` points there.
#'
#' Simpson, D. *et al.* (2012). The EMEP MSC-W chemical transport model --
#' technical description. *Atmospheric Chemistry and Physics* 12(16),
#' 7825-7865. \doi{10.5194/acp-12-7825-2012}
#'
#' @param species Which species to read, `"nhx"` or `"noy"`.
#' @param emep_dir Path to the directory holding the yearly EMEP files.
#'   Defaults to `Sys.getenv("WHEP_EMEP_DIR")`.
#' @param years Optional integer vector of calendar years to read. `NULL`
#'   reads every year with a file in `emep_dir`. A requested year with no file
#'   is not read; the years actually returned are the ones in the output.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year` and `deposition_kgn_ha` (the
#'   species' grid-mean deposition rate over the whole 0.5-degree cell).
#' @export
#' @examples
#' read_emep_deposition(example = TRUE)
read_emep_deposition <- function(
  species = c("nhx", "noy"),
  emep_dir = NULL,
  years = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_emep_species())
  }
  species <- rlang::arg_match(species)
  rlang::check_installed("ncdf4")
  files <- .emep_files(.resolve_emep_dir(emep_dir), years)
  purrr::map2(
    files$path,
    files$year,
    \(path, year) .read_emep_year(path, .emep_species_vars(species), year)
  ) |>
    data.table::rbindlist() |>
    tibble::as_tibble() |>
    .ensure_emep_schema()
}

#' Correct HaNi's European deposition trend toward EMEP.
#'
#' @description
#' Rescales one HaNi species, country by country and year by year, so that
#' its trajectory inside the EMEP core countries follows EMEP's while its
#' level over a reference period stays HaNi's own. This is the year-resolved
#' correction whep#1121 asks for and is **opt-in**: nothing in the package
#' calls it, and [build_n_deposition()] keeps reading plain HaNi unless a
#' corrected field is injected through its `data` argument.
#'
#' @section What is wrong with HaNi over Europe:
#' Over cells at least 98% land in the 39 EMEP core countries, HaNi falls
#' from 10.6 to 8.6 kg N/ha/yr between 1990 and 2019 (-19%) while EMEP falls
#' from 14.6 to 8.3 (-43%), so the HaNi/EMEP ratio climbs from 0.73 to 1.04
#' (`validation/n_deposition_emep.R`). HaNi is too flat over exactly the
#' period in which European emission controls halved deposition.
#'
#' @section The correction:
#' For country \eqn{c} and year \eqn{y} the factor is
#' \deqn{f_{c,y} = \frac{E_{c,y} / \bar{E}_c}{H_{c,y} / \bar{H}_c}}
#' where \eqn{H} and \eqn{E} are the HaNi and EMEP masses summed over the
#' country's cells and the bars are their means over `reference_years`. Every
#' HaNi cell of that country-year is multiplied by \eqn{f_{c,y}}. Three
#' properties follow, and the tests pin each:
#'
#' * **Year-resolved.** Every year has its own factor, so the three years
#'   2011-2013 in which HaNi sits above EMEP are lowered while 1990 is raised.
#'   A single scale factor anchored on one year cannot do both.
#' * **Trend, not level.** The mean of the corrected mass over
#'   `reference_years` equals HaNi's, and HaNi's spatial pattern inside a
#'   country is kept. EMEP's level is never imposed: HaNi books mass deposited
#'   to land while EMEP is a density over the whole cell, so their levels
#'   differ by the land fraction in every coastal cell, and that difference
#'   cancels in a ratio of ratios but not in a ratio.
#' * **Bounded in time and space.** Only country-years in the EMEP core list
#'   that have both products are touched; every other row is returned as
#'   read and keeps `method_deposition = "hani"`. In particular **nothing
#'   before 1990 is corrected** (EMEP rv5.6 starts in 1990), so a corrected
#'   series steps at 1989-1990 by that year's factor -- about +40% over the
#'   core countries for the two species together. How to carry the correction
#'   back in time is the open part of whep#1121 and is not decided here.
#'
#' `reference_years` defaults to 2010-2019, the last decade the two products
#' overlap, in which their core-country ratio stays within 0.89-1.03. It is a
#' choice, not a sourced value (assumed, unverified): it says which period's
#' HaNi level is trusted. A decade rather than one year, because the ratio is
#' not monotone and a single anchor year would carry its own noise into every
#' other year.
#'
#' Each border cell is assigned to the country holding its largest
#' `polity_frac`, so it is counted once when the factors are formed and gets
#' one factor.
#'
#' @param hani One HaNi species as returned by [read_n_deposition()]: `lon`,
#'   `lat`, `year`, `value_g` and `method_deposition`, which must be `"hani"`
#'   on every row so that a field cannot be corrected twice. It must cover
#'   `reference_years`.
#' @param emep The same species from [read_emep_deposition()]: `lon`, `lat`,
#'   `year` and `deposition_kgn_ha`. It must cover `reference_years`.
#' @param cell_polity The cell-polity table, as [build_cell_polity()] returns
#'   it: `lon`, `lat`, `area_code`, `polity_frac` and `cell_area_ha`.
#' @param method `"emep_trend"` (default) applies the correction; `"none"`
#'   returns `hani` unchanged, so a pipeline can switch it off without a
#'   second code path.
#' @param reference_years Integer years over which the corrected mass keeps
#'   HaNi's mean level. Defaults to `2010:2019`.
#' @param area_codes Integer `area_code`s of the countries to correct. `NULL`
#'   (default) takes the 39 EMEP core countries, the European countries the
#'   EMEP model is built to represent. Its domain also reaches Central Asia
#'   and the Arabian peninsula, where the two products disagree by factors of
#'   2-5 in both directions, which is a domain-edge artifact rather than a
#'   HaNi bias, so those countries are not corrected by default.
#' @return `hani` with `value_g` rescaled on corrected rows, those rows
#'   stamped `method_deposition = "hani_emep_trend"`, and a
#'   `deposition_correction` column holding the factor applied (`1` on every
#'   untouched row).
#' @export
#' @examples
#' correct_n_deposition(
#'   hani = read_n_deposition(example = TRUE),
#'   emep = read_emep_deposition(example = TRUE),
#'   cell_polity = tibble::tribble(
#'     ~lon, ~lat, ~area_code, ~polity_frac, ~cell_area_ha,
#'     -0.25, -0.25, 79L, 1, 300000
#'   ),
#'   reference_years = 2020L
#' )
correct_n_deposition <- function(
  hani,
  emep,
  cell_polity,
  method = c("emep_trend", "none"),
  reference_years = 2010:2019,
  area_codes = NULL
) {
  method <- rlang::arg_match(method)
  if (method == "none") {
    return(dplyr::mutate(hani, deposition_correction = 1))
  }
  .ndc_check_hani(hani)
  check_inputs_supplied(
    emep,
    c(emep = "deposition_kgn_ha"),
    details = c(
      i = "An EMEP field that is identically zero or missing leaves nothing
           to correct toward; check {.envvar WHEP_EMEP_DIR} and the years
           read."
    )
  )
  cells <- .ndc_cells(cell_polity, area_codes %||% .emep_core_area_codes())
  factors <- .ndc_country_totals(hani, emep, cells) |>
    .ndc_trend_factors(reference_years)
  .ndc_apply(hani, cells, factors)
}

# ---- Private helpers --------------------------------------------------

# The countries EMEP is built to represent, by ISO3. The model domain reaches
# Xinjiang and the Arabian peninsula, where HaNi and EMEP disagree by factors
# of 3-5 in BOTH directions; correcting there would read the domain edge as a
# bias. Same list as `validation/n_deposition_emep.R`.
.emep_core_iso3 <- function() {
  c(
    "ALB",
    "AUT",
    "BEL",
    "BGR",
    "BIH",
    "BLR",
    "CHE",
    "CYP",
    "CZE",
    "DEU",
    "DNK",
    "ESP",
    "EST",
    "FIN",
    "FRA",
    "GBR",
    "GRC",
    "HRV",
    "HUN",
    "IRL",
    "ISL",
    "ITA",
    "LTU",
    "LUX",
    "LVA",
    "MDA",
    "MKD",
    "MLT",
    "MNE",
    "NLD",
    "NOR",
    "POL",
    "PRT",
    "ROU",
    "SRB",
    "SVK",
    "SVN",
    "SWE",
    "UKR"
  )
}

.emep_core_area_codes <- function() {
  whep::regions_full |>
    dplyr::filter(.data$iso3c %in% .emep_core_iso3()) |>
    dplyr::pull("code") |>
    unique()
}

.resolve_emep_dir <- function(emep_dir) {
  resolved <- emep_dir %||% Sys.getenv("WHEP_EMEP_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No EMEP deposition directory available.",
      i = "Pass {.arg emep_dir} or set {.envvar WHEP_EMEP_DIR}; see
           {.file inst/scripts/download/download_emep.R}."
    ))
  }
  resolved
}

# One file per year; the met year and the emission year are the same in the
# trend runs, and a file pairing two different years is not one of them.
.emep_files <- function(emep_dir, years) {
  pattern <- "^EMEP01_rv5\\.6_year\\.([0-9]{4})met_([0-9]{4})emis_rep2025\\.nc$"
  name <- list.files(emep_dir, pattern = pattern)
  parts <- stringr::str_match(name, pattern)
  tibble::tibble(
    path = file.path(emep_dir, name),
    year = as.integer(parts[, 2]),
    emis_year = as.integer(parts[, 3])
  ) |>
    dplyr::filter(
      .data$year == .data$emis_year,
      is.null(years) | .data$year %in% years
    ) |>
    dplyr::arrange(.data$year)
}

.emep_species_vars <- function(species) {
  switch(
    species,
    nhx = c("DDEP_RDN_m2Grid", "WDEP_RDN"),
    noy = c("DDEP_OXN_m2Grid", "WDEP_OXN")
  )
}

# 0.1-degree centres sit at x.x5, so flooring onto the 0.5-degree lattice is
# exact and each block collects exactly 25 of them.
.emep_block_centre <- function(coord) {
  floor(coord / 0.5) * 0.5 + 0.25
}

.read_emep_year <- function(path, vars, year) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  total <- Reduce(`+`, lapply(vars, \(v) ncdf4::ncvar_get(nc, v)))
  dt <- data.table::data.table(
    lon = rep(.emep_block_centre(lon), times = length(lat)),
    lat = rep(.emep_block_centre(lat), each = length(lon)),
    deposition_kgn_ha = as.vector(total) * 0.01
  )
  dt <- dt[!is.na(deposition_kgn_ha)]
  dt[,
    .(year = year, deposition_kgn_ha = mean(deposition_kgn_ha)),
    by = .(lon, lat)
  ]
}

.ensure_emep_schema <- function(x) {
  ensure_columns(
    x,
    tibble::tibble(
      lon = double(),
      lat = double(),
      year = integer(),
      deposition_kgn_ha = double()
    )
  ) |>
    dplyr::mutate(year = as.integer(.data$year))
}

# Correcting a field that is not plain HaNi would compound two corrections
# and stamp the result as one.
.ndc_check_hani <- function(hani) {
  tag <- if (rlang::has_name(hani, "method_deposition")) {
    hani$method_deposition
  }
  if (is.null(tag) || anyNA(tag) || any(tag != "hani")) {
    cli::cli_abort(
      c(
        "{.arg hani} must carry {.code method_deposition == \"hani\"} on every
         row.",
        i = "Pass the output of {.fun read_n_deposition}; a field already
             corrected or supplied from elsewhere is not corrected again."
      ),
      class = "whep_deposition_not_hani"
    )
  }
  invisible(hani)
}

# One country per cell: the polity holding the largest share, so a border cell
# is counted once in the country totals and gets one factor.
.ndc_cells <- function(cell_polity, area_codes) {
  cell_polity |>
    dplyr::slice_max(
      .data$polity_frac,
      n = 1,
      by = c("lon", "lat"),
      with_ties = FALSE
    ) |>
    dplyr::filter(.data$area_code %in% area_codes) |>
    dplyr::select("lon", "lat", "area_code", "cell_area_ha")
}

# HaNi and EMEP mass per country-year over the cells where both are positive.
# A zero in either is an out-of-domain cell, not a measurement of no
# deposition: HaNi is zero outside its land mask and EMEP outside its grid.
.ndc_country_totals <- function(hani, emep, cells) {
  hani |>
    dplyr::inner_join(cells, by = c("lon", "lat")) |>
    dplyr::inner_join(
      dplyr::select(emep, "lon", "lat", "year", "deposition_kgn_ha"),
      by = c("lon", "lat", "year")
    ) |>
    dplyr::mutate(
      emep_g = .data$deposition_kgn_ha * .data$cell_area_ha * 1000
    ) |>
    dplyr::filter(.data$value_g > 0, .data$emep_g > 0) |>
    dplyr::summarise(
      hani_g = sum(.data$value_g),
      emep_g = sum(.data$emep_g),
      .by = c("area_code", "year")
    )
}

.ndc_trend_factors <- function(totals, reference_years) {
  reference <- .ndc_reference(totals, reference_years)
  totals |>
    dplyr::inner_join(reference, by = "area_code") |>
    dplyr::mutate(
      deposition_correction = (.data$emep_g / .data$emep_ref) /
        (.data$hani_g / .data$hani_ref)
    ) |>
    dplyr::select("area_code", "year", "deposition_correction")
}

# Each country's mean mass over the reference years. A country short of any
# reference year would be anchored on a different period from its neighbours,
# so that aborts rather than averaging whatever is there.
.ndc_reference <- function(totals, reference_years) {
  in_ref <- dplyr::filter(totals, .data$year %in% reference_years)
  short <- in_ref |>
    dplyr::summarise(n_years = dplyr::n(), .by = "area_code") |>
    dplyr::filter(.data$n_years < length(unique(reference_years)))
  missing <- setdiff(unique(totals$area_code), in_ref$area_code)
  bad <- c(short$area_code, missing)
  if (nrow(totals) == 0L || length(bad) > 0L) {
    .ndc_abort_reference(bad, reference_years)
  }
  dplyr::summarise(
    in_ref,
    hani_ref = mean(.data$hani_g),
    emep_ref = mean(.data$emep_g),
    .by = "area_code"
  )
}

.ndc_abort_reference <- function(bad, reference_years) {
  cli::cli_abort(
    c(
      "HaNi and EMEP do not both cover every reference year.",
      x = "{cli::qty(length(bad))}Area code{?s} short of a reference year:
           {.val {bad}}.",
      x = if (length(bad) == 0L) "No country has both products at all.",
      i = "Read both products for {.arg reference_years}
           ({min(reference_years)}-{max(reference_years)}) as well as the
           years to correct."
    ),
    class = "whep_deposition_reference"
  )
}

.ndc_apply <- function(hani, cells, factors) {
  hani |>
    dplyr::left_join(
      dplyr::select(cells, "lon", "lat", "area_code"),
      by = c("lon", "lat")
    ) |>
    dplyr::left_join(factors, by = c("area_code", "year")) |>
    dplyr::mutate(
      corrected = !is.na(.data$deposition_correction),
      # An uncorrected row is returned as read and keeps its "hani" stamp:
      # outside the core countries and outside the overlap years there is no
      # reference to correct toward, and a factor of one declares that.
      deposition_correction = dplyr::coalesce(.data$deposition_correction, 1),
      value_g = .data$value_g * .data$deposition_correction,
      method_deposition = dplyr::if_else(
        .data$corrected,
        "hani_emep_trend",
        .data$method_deposition
      )
    ) |>
    dplyr::select(-"area_code", -"corrected")
}

# Toy fixture for a runnable example (one cell, one year, one species).
.example_emep_species <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~deposition_kgn_ha,
    -0.25, -0.25, 2020L, 1.1
  )
}
