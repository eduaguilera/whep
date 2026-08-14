# Within-country dispersion of habitual intake, for the SJOS-N nourishment
# floor.
#
# A population whose MEAN intake equals its mean requirement leaves half its
# members short. Turning a requirement into a supply threshold therefore needs a
# dispersion allowance, and WHEP's shipped floor carried one as an unsourced
# constant of 1.35 (whep#753) which the author has confirmed was a preliminary
# presentation figure.
#
# FAOSTAT publishes the quantity that replaces it: Suite of Food Security
# Indicators item 21058, the coefficient of variation of habitual caloric
# consumption, 169 areas and 2001-2024.
#
# TWO THINGS ABOUT 21058 THAT MUST NOT BE FORGOTTEN, both documented at the
# point of use rather than only in the PR:
#
# 1. It is an ENERGY dispersion and the axis is protein. The transfer rests on
#    USDA WWEIA NHANES, where protein and energy intake log-SDs agree within
#    +-6% (ratio 1.006-1.146, mean 1.062 across 8 country-rounds). That is n = 1
#    country for the mechanism and 8 for the ratio.
# 2. It is not a measured dispersion. FAO builds it as the CV of FITTED values
#    from a regression of calories on log income, quadrature-summed with a
#    requirement term, and names the discarded remainder "excess variability".
#    Against the empirical household CV from the very surveys it is built from
#    it sits at a median 0.574, and against a direct usual-intake dispersion it
#    is roughly 1.5x smaller (8-country median k = 1.477). So the default
#    UNDERSTATES dispersion, and therefore the floor, by a known and signed
#    amount. `estimand = "calibrated"` applies that k; it is not the default,
#    because k varies 1.43-1.95 and every comparable country sits at CV
#    0.17-0.27 while the series runs to 0.37.

#' Build the within-country intake dispersion.
#'
#' @description
#' Returns the log-scale standard deviation of habitual intake per country and
#' year, the input a lognormal shortfall model needs. The coefficient of
#' variation comes from FAOSTAT Suite of Food Security Indicators item 21058 and
#' is converted with `sigma = sqrt(log(1 + cv^2))`, which is the conversion FAO
#' performs itself; treating a raw CV as a log-SD errs by about 1.5% at a CV of
#' 0.25.
#'
#' `nutrient = "protein"` (default) scales the energy-based dispersion by 1.062,
#' the mean protein-to-energy log-SD ratio measured across eight countries with
#' deconvolved usual-intake surveys. `"energy"` leaves it unscaled and is about
#' 6% low for a protein floor.
#'
#' `estimand = "faostat"` (default) takes item 21058 on its own scale and
#' carries its understatement as a documented signed bias. `"calibrated"`
#' multiplies by 1.477, the median ratio of survey usual-intake dispersion to
#' this series across eight countries; it improves the level and rests on a
#' calibration whose countries all sit in the lower half of the CV range.
#'
#' Item 21058 begins in 2001. `temporal = "hold_constant"` (default) carries
#' each area's earliest published value back over earlier years, which is FAO's
#' own documented rule for unsupported country-years, and stamps those rows
#' accordingly. A covariate hindcast was specified and tested against this and
#' did not beat it: it wins RMSE by 2.7% and loses MAE over 1,332 country-years.
#'
#' @param data Named list of injected inputs. `habitual_cv` (`year`,
#'   `area_code`, `cv`) overrides the FAOSTAT read.
#' @param years Optional integer vector of years to return. Years before the
#'   series begins are filled per `temporal`.
#' @param nutrient `"protein"` (default) or `"energy"`.
#' @param estimand `"faostat"` (default) or `"calibrated"`.
#' @param temporal `"hold_constant"` (default) or `"observed_only"`, which
#'   returns nothing before the series begins rather than carrying it back.
#' @return A tibble keyed by `year`, `area_code` with `cv`, `sigma`,
#'   `method_dispersion` and `method_cv_year`, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_intake_dispersion(
#'   data = list(
#'     habitual_cv = tibble::tribble(
#'       ~year, ~area_code, ~cv,
#'       2001L, 10L,        0.25,
#'       2002L, 10L,        0.24
#'     )
#'   ),
#'   years = 2000:2002
#' )
build_intake_dispersion <- function(
  data = list(),
  years = NULL,
  nutrient = c("protein", "energy"),
  estimand = c("faostat", "calibrated"),
  temporal = c("hold_constant", "observed_only")
) {
  nutrient <- rlang::arg_match(nutrient)
  estimand <- rlang::arg_match(estimand)
  temporal <- rlang::arg_match(temporal)
  cv <- data$habitual_cv %||% read_habitual_cv()
  .check_columns(cv, c("year", "area_code", "cv"), "data$habitual_cv")

  cv |>
    .id_extend(years, temporal) |>
    .id_sigma(nutrient, estimand) |>
    dplyr::mutate(
      method_dispersion = paste(nutrient, estimand, sep = "_")
    ) |>
    .add_reporting_polity_columns()
}

#' Read FAOSTAT's coefficient of variation of habitual caloric consumption.
#'
#' @description
#' Reads item 21058 from the FAOSTAT Suite of Food Security Indicators bulk
#' download, resolved to WHEP area codes. The file is located in order: the
#' `dir` argument, then `WHEP_FAOSTAT_FS_DIR`, then a cache under
#' `rappdirs::user_cache_dir("whep")`, downloading on first use.
#'
#' As with the UN WPP reader, the size and MD5 recorded here are WHEP's own:
#' FAOSTAT publishes no checksum for its bulk files, so a mismatch means the
#' upstream release changed, not necessarily that the download failed.
#'
#' @param years Optional integer vector of years to keep.
#' @param data Optional pre-read table, bypassing the file. Used by the tests.
#' @param dir Optional directory holding the bulk zip.
#' @return A tibble with `year`, `area_code`, `cv`.
#' @export
#' @examples
#' read_habitual_cv(
#'   data = tibble::tribble(
#'     ~`Area Code`, ~`Item Code`, ~Year, ~Value,
#'     2L,           21058L,       2010L, 0.25
#'   )
#' )
read_habitual_cv <- function(years = NULL, data = NULL, dir = NULL) {
  raw <- data %||% .fs_read_file(dir)
  .fs_parse(raw, years)
}

# ---- Private helpers -------------------------------------------------------

# The measured protein-to-energy log-SD ratio, mean of eight countries with
# deconvolved usual-intake surveys (range 1.006-1.146). Applied to the log-SD,
# not to the CV, because that is the quantity the ratio was measured on.
.id_protein_energy_ratio <- function() 1.062

# Median ratio of survey usual-intake log-SD to the log-SD implied by item
# 21058, across the same eight countries (range 1.432-1.953 on protein).
.id_calibration_k <- function() 1.477

.id_sigma <- function(cv, nutrient, estimand) {
  scale <- 1
  if (nutrient == "protein") {
    scale <- scale * .id_protein_energy_ratio()
  }
  if (estimand == "calibrated") {
    scale <- scale * .id_calibration_k()
  }
  dplyr::mutate(cv, sigma = sqrt(log(1 + .data$cv^2)) * scale)
}

# Carry each area's earliest published value back over years the series does not
# cover. FAO's own rule for unsupported country-years, and a covariate hindcast
# did not beat it. The filled rows are stamped so they are never mistaken for
# observations.
.id_extend <- function(cv, years, temporal) {
  observed <- dplyr::mutate(cv, method_cv_year = "faostat_observed")
  if (is.null(years)) {
    return(observed)
  }
  if (temporal == "observed_only") {
    return(dplyr::filter(observed, .data$year %in% years))
  }
  earliest <- observed |>
    dplyr::slice_min(.data$year, n = 1L, by = "area_code") |>
    dplyr::select("area_code", first_year = "year", first_cv = "cv")
  back <- tidyr::expand_grid(
    year = as.integer(years),
    dplyr::select(earliest, "area_code", "first_year", "first_cv")
  ) |>
    dplyr::filter(.data$year < .data$first_year) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      cv = .data$first_cv,
      method_cv_year = "hold_constant"
    )
  dplyr::bind_rows(dplyr::filter(observed, .data$year %in% years), back) |>
    dplyr::arrange(.data$area_code, .data$year)
}

.fs_file_name <- function() {
  "Food_Security_Data_E_All_Data_(Normalized).zip"
}

.fs_url <- function() {
  paste0("https://bulks-faostat.fao.org/production/", .fs_file_name())
}

# WHEP-recorded, not publisher-published. See read_habitual_cv()'s docs.
.fs_bytes <- function() 2214673
.fs_md5 <- function() "d76b9f8489f4437ff4b3e71c35a6e918"

.fs_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "faostat_fs")
}

.resolve_fs_dir <- function(dir = NULL) {
  resolved <- dir %||% Sys.getenv("WHEP_FAOSTAT_FS_DIR")
  if (.has_path(resolved)) {
    return(resolved)
  }
  .fs_cache_dir()
}

.fs_read_file <- function(dir = NULL, download = .fs_download) {
  resolved <- .resolve_fs_dir(dir)
  path <- file.path(resolved, .fs_file_name())
  if (!.fs_manifest_ok(path)) {
    path <- download(resolved)
  }
  # The archive carries the data table plus four code lookups. Take the
  # normalized data file BY NAME, and abort rather than guess if it is not
  # uniquely identifiable: picking by position would silently hand back a code
  # list if FAOSTAT ever reorders the archive.
  members <- utils::unzip(path, list = TRUE)$Name
  wanted <- grep("Normalized\\)\\.csv$", members, value = TRUE)
  if (length(wanted) != 1L) {
    cli::cli_abort(c(
      "Expected one normalized data file in the FAOSTAT archive.",
      i = "Found {length(wanted)}."
    ))
  }
  extracted <- utils::unzip(path, files = wanted, exdir = tempfile())
  readr::read_csv(extracted, show_col_types = FALSE, progress = FALSE)
}

.fs_manifest_ok <- function(path) {
  file.exists(path) &&
    identical(
      as.numeric(unname(file.info(path)$size)),
      as.numeric(.fs_bytes())
    ) &&
    identical(unname(tools::md5sum(path)), .fs_md5())
}

.fs_download <- function(dir, fetch = .fs_fetch) {
  path <- file.path(dir, .fs_file_name())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_info("Downloading the FAOSTAT food-security bulk (2 MB)...")
  fetch(.fs_url(), path)
  if (!.fs_manifest_ok(path)) {
    unlink(path)
    expected_bytes <- .fs_bytes()
    expected_md5 <- .fs_md5()
    cli::cli_abort(c(
      "The FAOSTAT food-security download does not match WHEP's manifest.",
      i = "Expected {.val {expected_bytes}} bytes, MD5 {.val {expected_md5}}.",
      i = "FAOSTAT publishes no checksum, so this manifest is WHEP's own: a
           mismatch means the upstream release changed.",
      i = "Set {.envvar WHEP_FAOSTAT_FS_DIR} to a directory holding a
           known-good copy to bypass the download."
    ))
  }
  path
}

.fs_fetch <- function(url, path) {
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  invisible(path)
}

# Item 21058 only. FAOSTAT's food-security file carries dozens of indicators in
# one long table, so an unfiltered read would silently mix, say, undernourishment
# prevalence into a dispersion column.
.fs_parse <- function(raw, years) {
  .check_columns(
    raw,
    c("Area Code", "Item Code", "Year", "Value"),
    "the FAOSTAT food-security table"
  )
  out <- tibble::as_tibble(raw) |>
    # The bulk file mixes numeric item codes with a few non-numeric ones, so
    # the coercion warns on every real read. Those rows are not 21058 and the
    # NA they become is filtered here anyway; suppressing keeps a production
    # read quiet without changing which rows survive.
    dplyr::filter(
      suppressWarnings(as.integer(.data[["Item Code"]])) == 21058L
    ) |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      fao_area_code = as.integer(.data[["Area Code"]]),
      cv = as.numeric(.data$Value)
    ) |>
    dplyr::filter(is.finite(.data$cv), .data$cv > 0)
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  dplyr::rename(out, area_code = "fao_area_code")
}
