# verify_polycell_support.R
#
# Re-derives every number quoted as evidence for build_polycell_support()
# (plan `plans/2026-08-03-polycell-spatial-support.md`, task T-A4). The numbers
# were first measured interactively; this script exists so anyone with the same
# inputs can reproduce them, and so a later change to the polity table, the
# water layer, the ice layer or the LUH2 vintage shows up as a moved number
# rather than as silent drift.
#
# What it reports:
#   L    the three input layers against the plan's EA9/EA10 measurements.
#   S-A1 the three area categories sum to polity_area_ha.
#   S-A2 re-aggregation to the polity polygon, AT A YEAR. Never summed across
#        intervals: the table is interval-keyed, so summing a polity over every
#        interval counts a cell once per epoch and inflates it. Reported at a
#        historical year, at the modern year, and then over EVERY interval at a
#        year inside its own validity, which is the only view that shows the
#        whole exception list: three of its five members are live in neither
#        1900 nor 2015, and none of the five is live in 2015.
#   S-A4 the global land denominator against the whole-cell base.
#   DA-12 the deployed crosswalk, today's producer and the polycell footprint.
#   DA-13 that the transitional shim is GONE. Until C9 this asserted the shim
#        reproduced build_cell_polity() bit-for-bit; it now aborts if a
#        `polity_frac` column, a crosswalk-only padding row or an unmeasured
#        row has come back.
#   DA-15 whole-table completeness on every runtime, plus the coverage classes
#        and pieces measured with terra against the reference-runtime census in
#        `.vps_expected_census()`.
#   DA-19 inland water clamped to the polycell's territory, and the cells the
#        water layer and the polycells do not share.
#   S-A9/S-A11 the LUH2 reconciliation, in BOTH directions, and the
#        unclaimed-land magnitude.
#   O    cells holding more territory than the cell (duplicate polygons).
#   Q-P6 the four orphan cells.
#
# Run:
#   Rscript inst/scripts/verify_polycell_support.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_LPJML_INPUT_DIR      the parent of GLWD/, as download_hydrology.R
#                             lays it out. Optional; inland water is skipped
#                             when unset.
#   WHEP_NATURALEARTH_DIR     ne_10m_glaciated_areas/. Optional; ice is
#                             skipped when unset.
#   WHEP_LUH2_DIR             staticData_quarterdeg.nc. Optional; the DA-5
#                             reconciliation and S-A11 are skipped when unset.
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. Optional; the
#                             DA-12 reconciliation is skipped when unset. The
#                             DA-13 check always runs -- it is about the
#                             producer's own output, not about the crosswalk.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`, and this repository has
# its own (issue #456). Run with R_ENVIRON_USER pointing at the user file, or
# export the variables in the shell.

.vps_h <- function(x) cli::cli_h2(x)

.vps_codes_from_env <- function() {
  codes <- Sys.getenv("WHEP_VPS_POLITY_CODES", "")
  if (!nzchar(codes)) {
    return(NULL)
  }
  stringr::str_split_1(codes, ",") |> stringr::str_trim()
}

.vps_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) value else NULL
}

# A subset must be held against the request made BEFORE the build. Deriving its
# expected membership from the returned support is circular: an empty build
# then expects zero and reports success. The preflight also makes retired,
# aggregate, missing, empty and invalid requested polities explicit rather than
# silently treating them as a clean zero-row verification.
.vps_polities <- function(path = file.path("data", "polities.rda")) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Cannot load the verified polity snapshot: {.file {path}} is absent."
    )
  }
  store <- new.env(parent = emptyenv())
  objects <- load(path, envir = store)
  if (!identical(objects, "polities") || !inherits(store$polities, "sf")) {
    cli::cli_abort(
      "{.file {path}} must contain exactly one {.cls sf} object named
       {.val polities}; found {.val {objects}}."
    )
  }
  store$polities
}

.vps_geometries <- function(requested) {
  # Read the same repository file `.vps_snapshot_gate()` fingerprints. Using
  # `whep::polities` here would let an older installed namespace pass the source
  # file's blob gate and then build a different object, especially on a subset
  # where the whole-table census is intentionally skipped.
  geometries <- .vps_polities()
  if (is.null(requested)) {
    return(geometries)
  }
  keep <- geometries$polity_code %in% requested
  attrs <- sf::st_drop_geometry(geometries)[keep, , drop = FALSE]
  # Construct from the intact full-table sfc explicitly. On the pinned snapshot
  # `get_polity_geometries(codes)` loses the sfc class on its geometry column,
  # which is a separate accessor defect and made the advertised fast verifier
  # subset fail before it could check anything.
  sf::st_sf(attrs, geometry = sf::st_geometry(geometries)[keep])
}

.vps_subset_codes <- function(requested, geometries) {
  if (is.null(requested)) {
    return(NULL)
  }
  requested <- sort(unique(requested[!is.na(requested) & nzchar(requested)]))
  if (length(requested) == 0L) {
    cli::cli_abort("The requested polity subset contains no non-empty codes.")
  }
  returned <- sort(unique(as.character(geometries$polity_code)))
  missing <- setdiff(requested, returned)
  unexpected <- setdiff(returned, requested)
  if (length(missing) > 0L || length(unexpected) > 0L) {
    cli::cli_abort(c(
      "The geometry lookup did not preserve the requested polity subset.",
      "x" = "Missing: {.val {missing}}.",
      "x" = "Unexpected: {.val {unexpected}}."
    ))
  }
  prepared <- whep:::.pcs_prepare_polities(geometries)
  usable <- sort(unique(prepared$polity_code[
    prepared$coverage_status %in% c("has_geometry", "s2_repaired")
  ]))
  unavailable <- setdiff(requested, usable)
  if (length(unavailable) > 0L) {
    cli::cli_abort(
      "Requested polities are not live, non-aggregate, interval-valid readable
       geometries and cannot form a verification subset: {.val {unavailable}}."
    )
  }
  usable
}

.vps_subset_gate <- function(expected_codes, support) {
  if (is.null(expected_codes)) {
    return(invisible(NULL))
  }
  got <- sort(unique(as.character(support$polity_code)))
  missing <- setdiff(expected_codes, got)
  unexpected <- setdiff(got, expected_codes)
  if (length(missing) > 0L || length(unexpected) > 0L) {
    cli::cli_abort(c(
      "The built support does not match the requested usable polity subset.",
      "x" = "Missing: {.val {missing}}.",
      "x" = "Unexpected: {.val {unexpected}}."
    ))
  }
  cli::cli_alert_success(
    "Subset identity matches all {length(expected_codes)} requested usable
     polit{?y/ies}: {.val {expected_codes}}."
  )
  invisible(NULL)
}

# Engine selection is a property of the geometry stack as well as the input
# blob. s2 validity can turn on ULP-level degeneracy, so the exact terra census
# below is authoritative only on this complete reference-runtime fingerprint.
.vps_reference_runtime <- function() {
  c(
    os = "Windows 11 x64",
    R = "4.5.2",
    platform = "x86_64-w64-mingw32",
    sf = "1.0.22",
    s2 = "1.1.9",
    terra = "1.8.80",
    GEOS = "3.13.1"
  )
}

.vps_runtime <- function() {
  os <- if (.Platform$OS.type == "windows") {
    sub(" \\(build [^)]+\\)$", "", utils::win.version())
  } else {
    paste(
      unname(Sys.info()[c("sysname", "release", "machine")]),
      collapse = " "
    )
  }
  c(
    os = os,
    R = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    sf = as.character(utils::packageVersion("sf")),
    s2 = as.character(utils::packageVersion("s2")),
    terra = as.character(utils::packageVersion("terra")),
    GEOS = unname(sf::sf_extSoftVersion()[["GEOS"]])
  )
}

.vps_runtime_text <- function(runtime) {
  paste(names(runtime), unname(runtime), sep = " ", collapse = "; ")
}

# The census comments name a Git blob, so the executable pin checks that blob
# rather than treating its row count as an identity. This script is a
# repository verification harness and is intentionally run from the repository
# root; aborting when either the data file or Git is unavailable is safer than
# silently weakening an identity gate to aggregate counts.
.vps_polities_blob <- function(path = file.path("data", "polities.rda")) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Cannot fingerprint the polity snapshot: {.file {path}} is absent.
       Run this verifier from the WHEP repository root."
    )
  }
  git <- Sys.which("git")
  if (!nzchar(git)) {
    cli::cli_abort(
      "Cannot fingerprint {.file {path}} because {.command git} is unavailable."
    )
  }
  blob <- suppressWarnings(system2(
    git,
    c("hash-object", path),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(blob, "status")
  if (
    (!is.null(status) && status != 0L) ||
      length(blob) != 1L ||
      !grepl("^[0-9a-f]{40}$", blob)
  ) {
    cli::cli_abort(
      "Could not read a Git blob identity for {.file {path}}: {paste(blob,
        collapse = ' ')}."
    )
  }
  unname(blob[[1]])
}

# ---- Inputs -----------------------------------------------------------------

.vps_water <- function() {
  if (is.null(.vps_env("WHEP_LPJML_INPUT_DIR"))) {
    cli::cli_alert_warning("WHEP_LPJML_INPUT_DIR unset: no inland water.")
    return(NULL)
  }
  water <- whep::read_glwd_water()
  cli::cli_alert_info(
    "GLWD: {nrow(water)} cells (EA10 67,420),
     {sum(water$water_frac > 0)} wet (EA10 32,358),
     {round(sum(water$water_frac * whep:::.cell_area_ha_lat(water$lat)) / 1e8, 4)}
     Mkm2 (EA10 2.4759)."
  )
  water
}

.vps_ice <- function() {
  if (is.null(.vps_env("WHEP_NATURALEARTH_DIR"))) {
    cli::cli_alert_warning("WHEP_NATURALEARTH_DIR unset: no ice.")
    return(NULL)
  }
  ice <- whep::read_glaciated_areas()
  cli::cli_alert_info(
    "ne_10m_glaciated_areas: {nrow(ice)} usable features,
     {sum(ice$s2_repaired)} repaired planar-side,
     {nrow(attr(ice, 'unrepaired'))} still s2-invalid."
  )
  ice
}

.vps_luh2 <- function() {
  if (is.null(.vps_env("WHEP_LUH2_DIR"))) {
    cli::cli_alert_warning("WHEP_LUH2_DIR unset: no DA-5 validation layer.")
    return(NULL)
  }
  luh2 <- whep::read_luh2_terrestrial()
  cli::cli_alert_info(
    "LUH2 {attr(luh2, 'luh2_vintage')}: {nrow(luh2)} cells,
     {round(sum(luh2$terrestrial_ha) / 1e9, 4)} Gha (EA1/EA2 12.9931)."
  )
  luh2
}

.vps_crosswalk <- function() {
  if (is.null(.vps_env("WHEP_POLITY_FRACTION_PATH"))) {
    cli::cli_alert_warning(
      "WHEP_POLITY_FRACTION_PATH unset: no DA-12 crosswalk reconciliation."
    )
    return(NULL)
  }
  whep::build_cell_polity()
}

# ---- Sections ---------------------------------------------------------------

.vps_identity <- function(polycells) {
  .vps_h("S-A1: the three categories sum to polity_area_ha")
  residual <- polycells$land_area_ha +
    polycells$inland_water_ha +
    polycells$ice_area_ha -
    polycells$polity_area_ha
  cli::cli_text(
    "max relative residual {max(abs(residual) / polycells$polity_area_ha)}
     (DA-18 1e-9); negative land rows {sum(polycells$land_area_ha < 0)};
     negative water rows {sum(polycells$inland_water_ha < 0)}."
  )
}

# Re-aggregation is checked AT A YEAR. The polity's own polygon area is the
# reference, taken through the same planar repair the producer applies.
.vps_reaggregation <- function(polycells, year) {
  .vps_h(paste0("S-A2: re-aggregation at ", year))
  got <- whep::expand_polycell_years(polycells, year) |>
    dplyr::summarise(
      got_ha = sum(.data$polity_area_ha),
      terra_pieces = sum(.data$area_engine == "terra"),
      .by = "polity_code"
    )
  comparison <- got |>
    dplyr::inner_join(.vps_own_areas(got$polity_code), by = "polity_code") |>
    dplyr::mutate(rel = abs(.data$got_ha - .data$own_ha) / .data$own_ha) |>
    dplyr::arrange(dplyr::desc(.data$rel))
  if (nrow(comparison) == 0L) {
    cli::cli_alert_info("No assessed polities are active in {year}.")
    return(invisible(comparison))
  }
  cli::cli_text(
    "{nrow(comparison)} polities: max {signif(max(comparison$rel), 3)},
     median {signif(stats::median(comparison$rel), 3)},
     over 1e-6: {sum(comparison$rel > 1e-6)}."
  )
  print(utils::head(as.data.frame(comparison), 8), digits = 6)
  invisible(comparison)
}

# The S-A2 exception list, checked over EVERY interval at a year inside its
# own validity rather than at one calendar year. On the reference runtime
# (Windows 11 x64, R 4.5.2, sf 1.0-22, s2 1.1.9, terra 1.8-80, GEOS 3.13.1),
# measured across all 666 clipped polities in polities 753 / 4f1fa941
# (`data/polities.rda` at git blob
# 4f1fa9415736b7d8f4b42e26b8b8809a286e70e3): max 2.8976e-05 and nine
# above 1e-6. All nine carry pieces the spherical engine could not read, so
# their residual is the terra/s2 engine substitution and nothing else; a polity
# appearing here without terra pieces is a new defect. None is live in 2015,
# which is why a single current-year check misses them. Exact membership is
# runtime-dependent; the property checked below is adaptive.
#
# THE EXPECTED SET IS DERIVED FROM THE BUILD, not listed by name. The criterion
# is the engine substitution, so the polities that may legitimately exceed the
# tolerance are exactly those carrying an `area_engine == "terra"` piece -- and
# on the shipped table and reference runtime the two sets coincide exactly, 9
# for 9. A hardcoded list
# cannot say that, and it rots: the previous one named GRC-1830-1913, which the
# 753-row `whep::polities` marks `superseded`, so DA-7's live filter drops it
# before the clip and the name could never match again.
#
# Derivation alone would be circular -- an expectation read off the build
# agrees with the build by construction -- so `.vps_expected_census()` pins the
# census the derivation rests on and `.vps_census_gate()` fails loudly the
# moment the polity table moves under it. The two together keep the property
# the list existed for: the set cannot grow unnoticed.
.vps_exception_list <- function(polycells, whole_table, expected_codes = NULL) {
  .vps_h("S-A2: the exception list, over every interval at its own year")
  # ONE interval per polycell, never a total over interval rows. Summing every
  # row of a polity counts each of its cells once per epoch. GRC-1881-1913 has
  # 63 polycells but more interval rows; summing those rows inflates its area,
  # while the probe below counts each piece once. A gate built on the interval
  # sum fires on polities that are correct.
  #
  # The probe is the polity's own earliest interval start, which every one of
  # its polycells covers because the splits partition the polity's validity.
  measured <- polycells |>
    dplyr::inner_join(
      dplyr::summarise(
        polycells,
        probe_year = min(.data$start_year),
        .by = "polity_code"
      ),
      by = "polity_code"
    ) |>
    dplyr::filter(
      .data$start_year <= .data$probe_year,
      .data$probe_year < .data$end_year
    ) |>
    dplyr::summarise(
      got_ha = sum(.data$polity_area_ha),
      polycells = dplyr::n(),
      terra_pieces = sum(.data$area_engine == "terra"),
      .by = c("polity_code", "probe_year")
    )
  assessed <- measured |>
    dplyr::inner_join(
      .vps_own_areas(measured$polity_code),
      by = "polity_code"
    ) |>
    dplyr::mutate(rel = abs(.data$got_ha - .data$own_ha) / .data$own_ha)
  comparison <- assessed |>
    dplyr::filter(.data$rel > 1e-6) |>
    dplyr::arrange(dplyr::desc(.data$rel))
  print(as.data.frame(comparison), digits = 6)
  # SET EQUALITY against the derived expectation, in both directions and as
  # counts. "Within the expected set" passed just as happily on an EMPTY list,
  # so a relaxed tolerance or a broken measurement read as success; the
  # machine-checkable lines below cannot, because the vacuity guard runs first
  # and asks whether anything was measured at all.
  expected <- sort(unique(
    polycells$polity_code[polycells$area_engine %in% "terra"]
  ))
  unexpected <- setdiff(comparison$polity_code, expected)
  quiet <- setdiff(expected, comparison$polity_code)
  cli::cli_text(
    "exception list: over tolerance {nrow(comparison)}, carrying terra
     {length(expected)}, over tolerance WITHOUT terra {length(unexpected)},
     terra but UNDER tolerance {length(quiet)}."
  )
  .vps_exception_alerts(
    measured,
    assessed,
    comparison,
    polycells,
    expected,
    quiet,
    whole_table,
    expected_codes
  )
  invisible(comparison)
}

# Every way this gate can read green while measuring nothing, made loud. Exact
# equality between terra carriers and over-tolerance polities is a property of
# the pinned runtime, not of the blob alone: T-A15 showed that ULP-level s2
# validity changes the fallback membership across geometry stacks. Elsewhere we
# still require a complete, finite assessment and forbid every over-tolerance
# polity that lacks a terra piece, but do not require every terra piece to push
# its polity above the threshold. The engine substitution does have a
# platform-independent magnitude envelope: over the global 0.5-degree grid the
# measured extrema of `terra/s2 - 1` are -0.447% and +0.888%. The 1% bound below
# rounds the larger magnitude outward, permits any platform-specific mix of the
# two engines, and still rejects the 10.07% missing-piece defect this gate was
# introduced to catch.
.vps_exception_alerts <- function(
  measured,
  assessed,
  comparison,
  polycells,
  expected,
  quiet,
  whole_table,
  expected_codes = NULL,
  runtime = .vps_runtime()
) {
  polities <- if (whole_table) {
    .vps_expected_census()$clipped_polities
  } else {
    if (is.null(expected_codes)) {
      cli::cli_abort(
        "A subset exception check requires the independently preserved
         requested/usable polity codes."
      )
    }
    length(expected_codes)
  }
  unexpected <- setdiff(comparison$polity_code, expected)
  exact_runtime <- identical(runtime, .vps_reference_runtime())
  issues <- character()
  if (nrow(measured) != polities) {
    issues <- c(
      issues,
      paste0(
        "The probe covered ",
        nrow(measured),
        " of ",
        polities,
        " expected polities."
      )
    )
  }
  if (nrow(assessed) != polities) {
    issues <- c(
      issues,
      paste0(
        "The own-area join assessed ",
        nrow(assessed),
        " of ",
        polities,
        " expected polities."
      )
    )
  }
  bad_measured <- !is.finite(measured$got_ha) |
    measured$got_ha <= 0 |
    !is.finite(measured$polycells) |
    measured$polycells < 1 |
    !is.finite(measured$terra_pieces) |
    measured$terra_pieces < 0
  bad_assessed <- !is.finite(assessed$own_ha) |
    assessed$own_ha <= 0 |
    !is.finite(assessed$rel) |
    assessed$rel < 0
  excessive <- which(is.finite(assessed$rel) & assessed$rel > 1e-2)
  if (any(bad_measured) || any(bad_assessed)) {
    issues <- c(
      issues,
      paste0(
        sum(bad_measured),
        " probe rows and ",
        sum(bad_assessed),
        " own-area rows are non-finite or outside their physical bounds."
      )
    )
  }
  if (length(excessive) > 0L) {
    issues <- c(
      issues,
      paste0(
        length(excessive),
        " polity residual",
        if (length(excessive) == 1L) "" else "s",
        " exceed the 1% terra/s2 substitution envelope (max ",
        format(max(assessed$rel[excessive]), digits = 6L),
        ")."
      )
    )
  }
  if (
    exact_runtime &&
      nrow(comparison) == 0L &&
      length(expected) > 0L
  ) {
    issues <- c(
      issues,
      paste0(
        "Nothing exceeds the tolerance while ",
        length(expected),
        " polities carry terra pieces."
      )
    )
  }
  if (length(unexpected) > 0L) {
    issues <- c(
      issues,
      paste0(
        "Over tolerance without a terra piece: ",
        paste(unexpected, collapse = ", "),
        "."
      )
    )
  }
  if (exact_runtime && length(quiet) > 0L) {
    issues <- c(
      issues,
      paste0(
        "Carries terra pieces but is under the tolerance: ",
        paste(quiet, collapse = ", "),
        "."
      )
    )
  }
  if (length(issues) > 0L) {
    cli::cli_abort(c(
      "The S-A2 exception gate failed.",
      stats::setNames(issues, rep("x", length(issues)))
    ))
  }
  if (!exact_runtime) {
    cli::cli_alert_success(
      "All {polities} polities have finite measurements; exact exception
       membership is not pinned off the reference runtime, and all
       {nrow(comparison)} over-tolerance polities carry terra pieces."
    )
    return(invisible(NULL))
  }
  cli::cli_alert_success(
    "Exception list is exactly the {length(expected)} terra-carrying
     polit{?y/ies}: {.val {expected}}."
  )
  invisible(NULL)
}

.vps_own_areas <- function(codes) {
  polities <- .vps_geometries(codes)
  fixed <- whep:::.s2_repair(sf::st_geometry(polities))
  usable <- fixed$status != "invalid"
  own_ha <- rep(NA_real_, nrow(polities))
  own_ha[usable] <- as.numeric(sf::st_area(fixed$geom[usable])) / 1e4
  tibble::tibble(polity_code = polities$polity_code, own_ha = own_ha)
}

.vps_denominator <- function(polycells, year) {
  .vps_h(paste0("S-A4: the global denominator at ", year))
  year_rows <- whep::expand_polycell_years(polycells, year)
  cells <- dplyr::distinct(year_rows, .data$cell_id, .data$cell_area_ha)
  cli::cli_text(
    "land {round(sum(year_rows$land_area_ha) / 1e9, 4)} Gha against a whole-cell
     base of {round(sum(cells$cell_area_ha) / 1e9, 4)} Gha
     (ratio {round(sum(cells$cell_area_ha) / sum(year_rows$land_area_ha), 4)});
     inland water {round(sum(year_rows$inland_water_ha) / 1e8, 4)} Mkm2;
     ice {round(sum(year_rows$ice_area_ha) / 1e8, 4)} Mkm2."
  )
}

.vps_footprints <- function(support, crosswalk) {
  .vps_h("DA-12: the three footprints")
  print(as.data.frame(attr(support, "footprints")))
  if (is.null(crosswalk)) {
    return(invisible(NULL))
  }
  diff <- attr(support, "footprint_diff")
  cli::cli_text(
    "build_cell_polity() {nrow(crosswalk)} rows;
     {nrow(diff)} cell-area_code pairs on which the three footprints disagree."
  )
}

# DA-13, flipped at C9. This check used to print "identical: TRUE" for the shim
# against `build_cell_polity()`. The shim is gone, so the check is now that it
# has NOT come back -- and it ABORTS rather than reporting, because a diagnostic
# that prints a false line is exactly what a re-introduced shim would produce.
# It runs on the support built WITH `data$crosswalk` supplied, which is the
# input that used to produce the shim -- checking a support built without one
# would pass on a path where the shim could not have appeared anyway.
.vps_shim_removed <- function(support) {
  .vps_h("DA-13: the transitional shim is gone")
  if (rlang::has_name(support, "polity_frac")) {
    cli::cli_abort(
      "The support carries {.field polity_frac}: the DA-13 shim is back."
    )
  }
  if (any(support$coverage_status == "crosswalk_only")) {
    cli::cli_abort(
      "{sum(support$coverage_status == 'crosswalk_only')} crosswalk-only
       padding row{?s}: the DA-13 shim is back."
    )
  }
  if (anyNA(support$polity_code) || anyNA(support$land_area_ha)) {
    cli::cli_abort(
      "{sum(is.na(support$polity_code))} row{?s} carry no {.field polity_code}
       and {sum(is.na(support$land_area_ha))} no {.field land_area_ha}: every
       row of this table must be a measured polycell."
    )
  }
  cli::cli_alert_success(
    "No shim column, no padding row; sum(land_area_ha) =
     {round(sum(support$land_area_ha) / 1e9, 4)} Gha over
     {nrow(support)} interval rows, not NA."
  )
}

.vps_coverage <- function(support, whole_table, expected_codes = NULL) {
  .vps_h("DA-15: polygon coverage and substituted area engines")
  diagnostic <- attr(support, "coverage")
  if (is.null(diagnostic)) {
    if (whole_table) {
      cli::cli_abort(
        "The whole-table build has no {.field coverage} diagnostic. On the
         pinned snapshot at least the 28 {.val no_geometry} rows require that
         diagnostic on every runtime; absence cannot be interpreted as an
         all-readable table."
      )
    }
    # The producer deliberately omits this attribute when every prepared row is
    # `has_geometry`. Use the independent pre-build membership, never the output
    # row count that an empty subset could make agree with itself.
    if (is.null(expected_codes)) {
      cli::cli_abort(
        "A subset coverage check requires the independently preserved
         requested/usable polity codes."
      )
    }
    coverage <- tibble::tibble(
      coverage_status = "has_geometry",
      n = as.integer(length(expected_codes))
    )
  } else {
    coverage <- dplyr::count(diagnostic, coverage_status)
  }
  clipped_polities <- if (whole_table) {
    dplyr::n_distinct(support$polity_code)
  } else {
    length(expected_codes)
  }
  print(as.data.frame(coverage))
  terra_measured <- attr(support, "terra_measured")
  if (is.null(terra_measured)) {
    cli::cli_alert_success("Every piece was measured by the spherical engine.")
    .vps_census_gate(
      coverage,
      terra_measured,
      clipped_polities,
      whole_table
    )
    return(invisible(NULL))
  }
  cli::cli_text(
    "{dplyr::n_distinct(terra_measured$polycell_id)} polycells measured with
     terra::expanse() over {nrow(terra_measured)} interval rows:
     {round(sum(terra_measured$polity_area_ha) / 1e6, 4)} Mha."
  )
  print(as.data.frame(dplyr::summarise(
    terra_measured,
    pieces = dplyr::n(),
    ha = sum(.data$polity_area_ha),
    .by = "polity_code"
  )))
  .vps_census_gate(
    coverage,
    terra_measured,
    clipped_polities,
    whole_table
  )
}

# THE PINNED WHOLE-TABLE CENSUS, measured on `whep::polities` at **753 rows**,
# `data/polities.rda` at git blob **4f1fa9415736b7d8f4b42e26b8b8809a286e70e3**.
# The snapshot is part of the pin, not context. The exact s2/terra split is also
# pinned to the reference runtime because T-A15 proved that piece readability
# varies by platform. On another runtime the snapshot/live/clipped counts still
# run, while exact engine counts are reported but not compared.
#
# whep#734 supplied this 753-row snapshot: archipelago geometry, plus Aruba and
# the Holy See. Both new rows carry a polygon, so `live_rows` 692 -> 694,
# `has_geometry` and `clipped_polities` 664 -> 666, and `no_geometry` is
# unchanged at 28. The census below was re-measured on that snapshot rather
# than adjusted to fit: the terra figures came back IDENTICAL -- 21 polycells
# across 9 polities holding 1,429,276.70 ha, to the hectare -- which is worth
# recording, because they were first measured under the Windows reference
# runtime and re-measured here under Linux. T-A15's platform caveat is real but
# did not bite on this snapshot.
# `.vps_exception_list()` derives its expectation from the build, which cannot
# notice a build that MOVED -- a derived expectation agrees with its own build
# by construction. This is the half that can: it names what the derivation
# rests on, so a refreshed polity table fails here, by number, instead of
# quietly re-deriving a different answer and calling it a pass.
#
# `terra_polycells` is the DISTINCT polycell count, not the interval-row count:
# the interval split subdivides a polycell in time and would inflate a row
# count without any piece changing. On this vintage they happen to coincide at
# 21, and pinning the stable one is what keeps that a coincidence rather than a
# hidden assumption.
#
# Re-measure this and `.pcs_measure_pieces()`'s comment in
# `R/polycell_support.R` together; they are the same numbers, and letting them
# drift apart is what left GRC-1830-1913 in a hardcoded list here after the
# upstream re-sync had already marked it `superseded`.
.vps_expected_census <- function() {
  list(
    snapshot_blob = "4f1fa9415736b7d8f4b42e26b8b8809a286e70e3",
    snapshot_rows = 753L,
    live_rows = 694L,
    clipped_polities = 666L,
    runtime = .vps_reference_runtime(),
    coverage = c(
      has_geometry = 666L,
      no_geometry = 28L,
      s2_repaired = 0L,
      s2_invalid = 0L
    ),
    terra_polycells = 21L,
    terra_polities = 9L,
    terra_ha = 1429276.70
  )
}

.vps_snapshot_gate <- function() {
  expected <- .vps_expected_census()$snapshot_blob
  got <- .vps_polities_blob()
  if (!identical(got, expected)) {
    cli::cli_abort(
      "The polity snapshot MOVED: {.file data/polities.rda} is Git blob
       {.val {got}}, pinned as {.val {expected}}. Re-measure the whole-table
       census before running this verifier."
    )
  }
  cli::cli_alert_success(
    "Polity snapshot matches pinned Git blob {.val {expected}}."
  )
  invisible(NULL)
}

.vps_census_gate <- function(
  coverage,
  terra_measured,
  clipped_polities,
  whole_table,
  runtime = .vps_runtime()
) {
  if (!whole_table) {
    cli::cli_alert_info(
      "Subset run: the whole-table census pin is not checked, because a subset
       legitimately measures less."
    )
    return(invisible(NULL))
  }
  expected <- .vps_expected_census()
  got <- .vps_measured_census(
    coverage,
    terra_measured,
    clipped_polities,
    runtime
  )
  exact_runtime <- identical(got$runtime, expected$runtime)
  moved <- .vps_census_diff(expected, got, exact_runtime)
  if (length(moved) > 0L) {
    cli::cli_abort(
      "The whole-table census MOVED against the pin in
     {.val {names(moved)}}. Re-measure, then update BOTH
     {.fn .vps_expected_census} and the {.fn .pcs_measure_pieces} comment in
     {.file R/polycell_support.R}: {paste(moved, collapse = '; ')}."
    )
  }
  if (!exact_runtime) {
    got_runtime <- .vps_runtime_text(got$runtime)
    reference_runtime <- .vps_runtime_text(expected$runtime)
    cli::cli_alert_info(
      "Snapshot completeness matches, but the exact engine census is skipped:
       runtime {.val {got_runtime}} differs from the reference
       {.val {reference_runtime}}."
    )
    return(invisible(NULL))
  }
  cli::cli_alert_success(
    "Whole-table census matches the reference-runtime pin:
     {got$terra_polycells} terra polycells, {round(got$terra_ha)} ha,
     {got$terra_polities} polities; coverage
     {paste(names(got$coverage), got$coverage, collapse = ', ')}."
  )
  invisible(NULL)
}

.vps_measured_census <- function(
  coverage,
  terra_measured,
  clipped_polities,
  runtime = .vps_runtime()
) {
  classes <- c("has_geometry", "no_geometry", "s2_repaired", "s2_invalid")
  counts <- stats::setNames(
    as.integer(coverage$n[match(classes, coverage$coverage_status)]),
    classes
  )
  counts[is.na(counts)] <- 0L
  list(
    snapshot_blob = .vps_polities_blob(),
    snapshot_rows = nrow(.vps_polities()),
    live_rows = sum(counts),
    clipped_polities = as.integer(clipped_polities),
    runtime = runtime,
    coverage = counts,
    terra_polycells = if (is.null(terra_measured)) {
      0L
    } else {
      dplyr::n_distinct(terra_measured$polycell_id)
    },
    terra_polities = if (is.null(terra_measured)) {
      0L
    } else {
      dplyr::n_distinct(terra_measured$polity_code)
    },
    terra_ha = if (is.null(terra_measured)) {
      0
    } else {
      sum(terra_measured$polity_area_ha)
    }
  )
}

# Hectares are compared to the pin's own precision (0.01 ha), which is far
# tighter than any real movement and far looser than float noise over 21 terms.
.vps_census_diff <- function(expected, got, exact_runtime) {
  moved <- character()
  for (nm in c(
    "snapshot_blob",
    "snapshot_rows",
    "live_rows",
    "clipped_polities"
  )) {
    if (!identical(got[[nm]], expected[[nm]])) {
      moved[nm] <- paste0(
        nm,
        " pinned ",
        expected[[nm]],
        ", measured ",
        got[[nm]]
      )
    }
  }
  if (!exact_runtime) {
    return(moved)
  }
  for (nm in names(expected$coverage)) {
    if (!identical(got$coverage[[nm]], expected$coverage[[nm]])) {
      moved[nm] <- paste0(
        nm,
        " pinned ",
        expected$coverage[[nm]],
        ", measured ",
        got$coverage[[nm]]
      )
    }
  }
  for (nm in c("terra_polycells", "terra_polities")) {
    if (!identical(got[[nm]], expected[[nm]])) {
      moved[nm] <- paste0(
        nm,
        " pinned ",
        expected[[nm]],
        ", measured ",
        got[[nm]]
      )
    }
  }
  if (abs(got$terra_ha - expected$terra_ha) > 0.01) {
    moved["terra_ha"] <- paste0(
      "terra_ha pinned ",
      format(expected$terra_ha, nsmall = 2L),
      ", measured ",
      format(round(got$terra_ha, 2L), nsmall = 2L)
    )
  }
  moved
}

.vps_water_clamp <- function(support, polycells, year) {
  .vps_h(paste0("DA-19: inland water clamped to the territory, ", year))
  excess <- attr(support, "water_excess")
  if (is.null(excess)) {
    return(invisible(NULL))
  }
  keys <- whep::expand_polycell_years(polycells, year) |>
    dplyr::distinct(.data$polycell_id, .data$start_year)
  at_year <- dplyr::semi_join(
    excess,
    keys,
    by = c("polycell_id", "start_year")
  )
  cli::cli_text(
    "{nrow(excess)} clamped polycells over all intervals
     ({round(sum(excess$water_excess_ha) / 1e8, 4)} Mkm2);
     {nrow(at_year)} at {year}
     ({round(sum(at_year$water_excess_ha) / 1e8, 4)} Mkm2)."
  )
}

.vps_unassigned <- function(support, polycells, luh2, year) {
  .vps_h(paste0("S-A9/S-A11: LUH2 reconciliation at ", year))
  if (is.null(luh2)) {
    return(invisible(NULL))
  }
  claimed <- sum(whep::expand_polycell_years(polycells, year)$land_area_ha)
  cli::cli_text(
    "claimed land {round(claimed / 1e9, 4)} Gha against LUH2
     {round(sum(luh2$terrestrial_ha) / 1e9, 4)} Gha
     ({round(100 * (claimed / sum(luh2$terrestrial_ha) - 1), 2)}%)."
  )
  # Both directions. Reporting only the shortfall reconciles the overshoot away
  # by construction, which is the silent reconciliation DA-5 forbids.
  disagreement <- attr(support, "unassigned") |>
    dplyr::filter(.data$start_year <= year, year < .data$end_year)
  cli::cli_text(
    "at {year}: {sum(disagreement$unassigned_land_ha > 0)} cells under-claim
     {round(sum(disagreement$unassigned_land_ha) / 1e6, 2)} Mha;
     {sum(disagreement$over_claimed_land_ha > 0)} cells over-claim
     {round(sum(disagreement$over_claimed_land_ha) / 1e6, 2)} Mha."
  )
}

# The diagnostic is interval-grain, so a row is a cell-INTERVAL, not a cell.
# Both are reported, and the wet side is also sliced, because the two answer
# different questions: how many cells are unreached in some epoch, and how many
# at the year in hand.
.vps_water_unmatched <- function(support, year) {
  .vps_h("EA10: cells the water layer and the polycells do not share")
  unmatched <- attr(support, "water_unmatched")
  if (is.null(unmatched) || nrow(unmatched) == 0L) {
    cli::cli_alert_success("The two footprints coincide.")
    return(invisible(NULL))
  }
  print(as.data.frame(dplyr::summarise(
    unmatched,
    cell_intervals = dplyr::n(),
    distinct_cells = dplyr::n_distinct(.data$lon, .data$lat),
    whole_cell_gha = round(sum(.data$cell_area_ha, na.rm = TRUE) / 1e9, 4),
    .by = "side"
  )))
  wet <- unmatched |>
    dplyr::filter(
      .data$side == "water_cell_without_polycell",
      .data$start_year <= year,
      year < .data$end_year
    )
  cli::cli_text(
    "wet cells no polycell reaches at {year}:
     {nrow(dplyr::distinct(wet, .data$lon, .data$lat))}."
  )
}

.vps_overlap <- function(support, polycells, year) {
  .vps_h(paste0("Duplicate polygons: territory beyond the cell, ", year))
  overlap <- attr(support, "overlap")
  if (is.null(overlap)) {
    cli::cli_alert_success("No cell holds more territory than the cell.")
    return(invisible(NULL))
  }
  at_year <- whep::expand_polycell_years(polycells, year) |>
    dplyr::summarise(
      territory_ha = sum(.data$polity_area_ha),
      .by = c("cell_id", "cell_area_ha")
    ) |>
    dplyr::filter(.data$territory_ha > .data$cell_area_ha * (1 + 1e-4))
  cli::cli_text(
    "{nrow(overlap)} cell-intervals overall; {nrow(at_year)} cells at {year},
     excess {round(sum(at_year$territory_ha - at_year$cell_area_ha) / 1e6, 2)}
     Mha, worst ratio
     {round(max(at_year$territory_ha / at_year$cell_area_ha), 4)}."
  )
}

.vps_orphans <- function(polycells, year) {
  .vps_h("Q-P6: the four orphan cells")
  orphans <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    -75.75, 22.25, 12L,
    98.25, 11.25, 28L,
    131.75, -4.75, 101L,
    -80.75, -33.75, 40L
  )
  year_rows <- whep::expand_polycell_years(polycells, year)
  found <- orphans |>
    dplyr::left_join(
      dplyr::summarise(
        year_rows,
        polycells = dplyr::n(),
        land_ha = sum(.data$land_area_ha),
        .by = c("lon", "lat")
      ),
      by = c("lon", "lat")
    )
  print(as.data.frame(found))
}

# ---- Run --------------------------------------------------------------------

# `polity_codes` restricts the build to a subset. The default is the whole
# table, which is the production call: on polities 753 / 4f1fa941 under the
# reference runtime, the polity clip alone is 3,843 s over 666 polities and
# 414,479 measured pieces, all retained after the 1e-6 ha area floor.
# Budget hours rather than the "about an hour" this note used to claim. A subset
# runs in minutes and is what makes it practical to EXECUTE this script after
# editing it. `inst/scripts/` is under no test, so an unexecuted change here is
# unverified by construction, which is exactly how a broken S-A2 gate once
# shipped from this file. Set WHEP_VPS_POLITY_CODES to a comma-separated list
# to drive it from the shell.
#
# Whether the run is the WHOLE TABLE is carried to the census and exception
# gates, because a subset legitimately measures less. Checking whole-table
# counts on it would train a reader to ignore the alert that catches drift.
.vps_main <- function(
  year = 2015L,
  historical_year = 1900L,
  polity_codes = .vps_codes_from_env()
) {
  rlang::check_installed(c("sf", "terra"))
  .vps_snapshot_gate()
  whole_table <- is.null(polity_codes)
  geometries <- .vps_geometries(polity_codes)
  expected_codes <- .vps_subset_codes(polity_codes, geometries)
  water <- .vps_water()
  ice <- .vps_ice()
  luh2 <- .vps_luh2()
  crosswalk <- .vps_crosswalk()
  cli::cli_alert_info("Building the polycell support table...")
  support <- whep::build_polycell_support(
    geometries = geometries,
    water = water,
    ice = ice,
    data = list(luh2 = luh2, crosswalk = crosswalk, crosswalk_year = year)
  )
  .vps_subset_gate(expected_codes, support)
  .vps_shim_removed(support)
  # Every row is a measured polycell since C9 removed the padding, so this is
  # the whole table rather than a filtered view of it.
  polycells <- support
  cli::cli_alert_success(
    "{nrow(support)} interval rows
     ({dplyr::n_distinct(polycells$polycell_id)} polycells)."
  )
  .vps_identity(polycells)
  .vps_reaggregation(polycells, historical_year)
  .vps_reaggregation(polycells, year)
  .vps_exception_list(polycells, whole_table, expected_codes)
  .vps_denominator(polycells, year)
  .vps_footprints(support, crosswalk)
  .vps_coverage(support, whole_table, expected_codes)
  .vps_water_clamp(support, polycells, year)
  .vps_water_unmatched(support, year)
  .vps_unassigned(support, polycells, luh2, year)
  .vps_unassigned(support, polycells, luh2, historical_year)
  .vps_overlap(support, polycells, year)
  .vps_orphans(polycells, year)
  cli::cli_alert_success("Done.")
  invisible(support)
}

.vps_main()
