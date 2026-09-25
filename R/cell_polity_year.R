# The year-aware cell support behind `build_cell_polity(year = )` (whep#1196).
#
# The year-invariant crosswalk keys every cell on today's reporting area, so a
# national row reported by a polity that no longer exists -- the USSR (228),
# Czechoslovakia (51), the Yugoslav SFR (248), Serbia and Montenegro (186) --
# has no cell at all. This support is read instead from the polycell support
# (`read_polycell_support()`), at the interval covering the driven year, so in
# 1961 the former-USSR cells carry 228. That is the same read the carbon path
# makes at its fixed 2015 snapshot (`.carbon_cell_support()`), done at the
# year being built.
#
# Three rules decided with the maintainer on whep#1196 sit on top of the read,
# because the polity grid and FAOSTAT's reporting units do not line up
# everywhere. Each is an explicit row of
# `inst/extdata/polity_cell_support_map.csv`, never inferred:
#
#   aggregate_member    An aggregate FAOSTAT reports whose members the support
#                       carries separately: its cells are the union of its
#                       members' cells in its reporting years (Belgium-
#                       Luxembourg 15; Viet Nam 237 to 1974; Yemen 249 in 1961).
#   contained_fold      A polity with no FAOSTAT row that year folds into the
#                       reporting unit containing it (the Baltic and
#                       Azerbaijan SSRs into the USSR to 1990; the fifteen USSR
#                       successors in 1991; North Macedonia into 248 in 1991).
#   constant_territory  A reporting unit whose predecessors the support does
#                       not carry: its modern territory's cells are used
#                       (Yemen 249 in 1962-1989, on the cells of YEM-1990-2025).
#
# And one rule for cells the support labels twice. The support is documented as
# a partition, but some cells carry an aggregate on top of its members (Germany
# over East and West Germany, the Federation of Rhodesia and Nyasaland over its
# three members) or two claimants. The land share's denominator is the cell's
# land, so such a cell hands each polity roughly half: measured on the shipped
# pin, Germany's 1961 cropland support fell from 12.54 to 6.54 Mha. In an
# OVERLAPPING cell, a polity that has no area code, no national data in the
# year (`reporting_areas`), or duplicates the container it folds into is
# therefore removed from the denominator. Two claimants that both report keep
# their halves; they are listed in the `overlap_kept` attribute.

# How far a cell's summed territory may exceed its area before the cell counts
# as labelled twice. Measured on the shipped `polycell_support` pin at 2010, the
# geodesic territory of a partitioned cell differs from the latitude formula's
# cell area by at most ~1e-5 of it (99th percentile 9.5e-6); 1e-4 sits an order
# of magnitude above that noise and below every real overlap (537 cells over it
# at 2010 against 30,858 over 1e-9).
.cpy_overlap_tolerance <- function() 1e-4

.cpy_rules <- function() {
  c("aggregate_member", "contained_fold", "constant_territory")
}

# The recorded mapping. Read from the installed package so the tarball, the
# source checkout and every consumer read one table.
.cell_polity_support_map <- function() {
  path <- system.file(
    "extdata",
    "polity_cell_support_map.csv",
    package = "whep",
    mustWork = TRUE
  )
  map <- utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      start_year = as.integer(.data$start_year),
      end_year = as.integer(.data$end_year)
    )
  .cpy_check_map(map)
}

.cpy_check_map <- function(map) {
  .check_columns(
    map,
    c("polity_code", "area_code", "start_year", "end_year", "rule"),
    "polity_cell_support_map"
  )
  bad <- !map$rule %in% .cpy_rules() |
    is.na(map$area_code) |
    !(map$start_year < map$end_year)
  if (any(bad)) {
    cli::cli_abort(c(
      "{.file polity_cell_support_map.csv} has {sum(bad)} invalid row{?s}.",
      x = "First: {.val {map$polity_code[bad][1]}}."
    ))
  }
  map
}

# Validate the year-aware arguments, then build. `reporting_areas` is required:
# without it the de-duplication has nothing to test, and a silently weaker
# support would be the result.
.cell_polity_at_year <- function(year, reporting_areas, area_key, version) {
  .cpy_check_args(year, reporting_areas)
  read_polycell_support(version = version) |>
    .cell_polity_year_support(as.integer(year), reporting_areas, area_key)
}

.cpy_check_args <- function(year, reporting_areas) {
  if (
    length(year) != 1L ||
      !is.numeric(year) ||
      is.na(year) ||
      year != round(year)
  ) {
    cli::cli_abort(
      "{.arg year} must be one whole year; the support is read per year.",
      class = "whep_cell_polity_year"
    )
  }
  if (length(reporting_areas) == 0L || !is.numeric(reporting_areas)) {
    cli::cli_abort(
      c(
        "{.arg reporting_areas} is required with {.arg year}.",
        i = "Pass the area codes that carry national data in {year}: an
             overlapping cell drops the polities that have none from its share
             denominator."
      ),
      class = "whep_cell_polity_reporting_areas"
    )
  }
}

.cell_polity_year_support <- function(
  support,
  year,
  reporting_areas,
  area_key
) {
  .check_columns(
    support,
    c(
      "lon",
      "lat",
      "polity_code",
      "area_code",
      "start_year",
      "end_year",
      "cell_area_ha",
      "polity_area_ha",
      "land_area_ha"
    ),
    "polycell_support"
  )
  map <- .cpy_active_map(.cell_polity_support_map(), year)
  rows <- support |>
    .cpy_rows_at_year(map, year) |>
    .cpy_key(map, area_key) |>
    .cpy_mark_removed(as.integer(reporting_areas))
  out <- .cpy_shares(rows, area_key)
  attr(out, "deduplicated") <- .cpy_removed(rows)
  attr(out, "overlap_kept") <- .cpy_overlap_kept(rows)
  .cpy_inform(rows, year)
  out
}

.cpy_active_map <- function(map, year) {
  dplyr::filter(map, .data$start_year <= year, year < .data$end_year)
}

# The polycells valid at `year`, plus the cells of any polity a
# constant_territory row places outside its own interval. A polity already
# valid at `year` is not added twice.
#
# The injected polity is read at its own first year, not in full: the support
# splits a polycell's rows at every breakpoint a neighbour introduces, so
# taking every row would count such a polycell once per piece. On the shipped
# pin one of YEM-1990-2025's 193 polycells is split at 1993.
.cpy_rows_at_year <- function(support, map, year) {
  valid <- .carbon_support_at_year(support, year)
  constant <- map$polity_code[map$rule == "constant_territory"]
  injected <- support |>
    dplyr::filter(
      .data$polity_code %in% constant,
      !.data$polity_code %in% valid$polity_code
    )
  if (nrow(injected) == 0L) {
    return(valid)
  }
  first <- injected |>
    dplyr::filter(
      .data$start_year == min(.data$start_year),
      .by = "polity_code"
    )
  dplyr::bind_rows(valid, first)
}

# Key every row: the polity's reporting area code (the pinned code where the
# crosswalk resolves none, as `.carbon_rekey_area_code()` does), then the
# recorded mapping, then, under "polity_area", the bucket the national tables
# are aggregated on.
.cpy_key <- function(rows, map, area_key) {
  resolved <- .polity_reporting_area_code(rows$polity_code)
  keyed <- rows |>
    dplyr::mutate(
      area_code = dplyr::coalesce(resolved, as.integer(.data$area_code))
    ) |>
    dplyr::left_join(
      dplyr::select(map, "polity_code", map_code = "area_code", "rule"),
      by = "polity_code"
    ) |>
    dplyr::mutate(
      area_code = dplyr::coalesce(.data$map_code, .data$area_code),
      polity_rule = dplyr::coalesce(.data$rule, "reporting"),
      grid_area_code = .data$area_code
    ) |>
    dplyr::select(-"map_code", -"rule")
  if (area_key == "grid") {
    return(keyed)
  }
  lookup <- .cell_polity_bucket_lookup()
  bucket <- lookup$polity_area_code[match(keyed$area_code, lookup$area_code)]
  keyed$area_code <- dplyr::coalesce(bucket, keyed$area_code)
  keyed
}

# Flag, per cell, the rows an overlapping cell takes out of its denominator.
.cpy_mark_removed <- function(rows, reporting_areas) {
  if (anyNA(rows$land_area_ha) || any(rows$land_area_ha < 0)) {
    cli::cli_abort(
      "{.field polycell_support$land_area_ha} must be non-negative and
       non-missing."
    )
  }
  tolerance <- .cpy_overlap_tolerance()
  rows |>
    dplyr::mutate(
      no_data = is.na(.data$area_code) |
        !.data$area_code %in% reporting_areas
    ) |>
    dplyr::mutate(
      overlap = sum(.data$polity_area_ha) >
        dplyr::first(.data$cell_area_ha) * (1 + tolerance),
      container = .data$polity_rule == "contained_fold" &
        .data$area_code %in%
          .data$area_code[
            .data$polity_rule != "contained_fold" & !.data$no_data
          ],
      .by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      removed = .data$overlap & (.data$no_data | .data$container),
      removed_reason = dplyr::case_when(
        !.data$removed ~ NA_character_,
        is.na(.data$area_code) ~ "no_area_code",
        .data$no_data ~ "no_national_data",
        .default = "duplicates_container"
      )
    )
}

# The land share over the rows the cell keeps. The denominator includes the
# land of kept rows with no area code, so an unkeyable polity's hectares are
# never handed to its neighbour; those rows then leave the table.
.cpy_shares <- function(rows, area_key) {
  out <- rows |>
    dplyr::mutate(
      denominator = sum(.data$land_area_ha[!.data$removed]),
      .by = c("lon", "lat")
    ) |>
    dplyr::filter(
      !.data$removed,
      !is.na(.data$area_code),
      .data$denominator > 0
    ) |>
    dplyr::summarise(
      land_ha = sum(.data$land_area_ha),
      denominator = dplyr::first(.data$denominator),
      grid_area_code = paste(
        sort(unique(.data$grid_area_code)),
        collapse = "+"
      ),
      polity_rule = paste(sort(unique(.data$polity_rule)), collapse = "+"),
      .by = c("lon", "lat", "area_code")
    ) |>
    dplyr::mutate(
      polity_frac = .data$land_ha / .data$denominator,
      cell_area_frac = .data$polity_frac,
      cell_area_ha = .cell_area_ha_lat(.data$lat),
      method_cell_polity = "year_aware"
    )
  keep <- c(
    "lon",
    "lat",
    "area_code",
    "polity_frac",
    "cell_area_ha",
    "cell_area_frac",
    if (area_key == "polity_area") "grid_area_code",
    "polity_rule",
    "method_cell_polity"
  )
  dplyr::select(out, dplyr::all_of(keep))
}

.cpy_removed <- function(rows) {
  rows |>
    dplyr::filter(.data$removed) |>
    dplyr::summarise(
      cells = dplyr::n_distinct(.data$lon, .data$lat),
      land_area_ha = sum(.data$land_area_ha),
      .by = c("polity_code", "area_code", "removed_reason")
    ) |>
    dplyr::arrange(dplyr::desc(.data$land_area_ha))
}

# Overlapping cells where more than one reporting polity keeps its share: two
# claimants that both carry national data, left at their halves.
.cpy_overlap_kept <- function(rows) {
  rows |>
    dplyr::filter(.data$overlap, !.data$removed, !is.na(.data$area_code)) |>
    dplyr::mutate(
      n_kept = dplyr::n_distinct(.data$area_code),
      .by = c("lon", "lat")
    ) |>
    dplyr::filter(.data$n_kept > 1L) |>
    dplyr::summarise(
      cells = dplyr::n_distinct(.data$lon, .data$lat),
      land_area_ha = sum(.data$land_area_ha),
      .by = c("polity_code", "area_code")
    ) |>
    dplyr::arrange(dplyr::desc(.data$land_area_ha))
}

.cpy_inform <- function(rows, year) {
  mapped <- dplyr::filter(rows, .data$polity_rule != "reporting")
  removed <- dplyr::filter(rows, .data$removed)
  cli::cli_inform(c(
    i = "Year-aware cell support at {year}: {nrow(mapped)} polycell{?s} keyed
         by the recorded mapping ({dplyr::n_distinct(mapped$polity_code)}
         polit{?y/ies}).",
    i = "{nrow(removed)} polycell{?s}
         ({round(sum(removed$land_area_ha) / 1e6, 2)} Mha of land) removed from
         overlapping cells' share denominators."
  ))
}
