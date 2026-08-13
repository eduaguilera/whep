# Year-validity of a year-less cell -> polity crosswalk -----------------------
#
# `data$cell_polity` is a present-day rasterization with NO year dimension
# (whep#460, whep#579), while polity validity IS year-scoped. So a cell carrying
# `area_code` 52 (Azerbaijan) is labelled that in 1901 as readily as in 2009,
# and `.add_reporting_polity_columns()` then resolves 1901 to the nearest
# period, `AZE-1991-2025`, a state that did not exist. That substitution is
# already recorded as `mapping_status == "out_of_span"` inside
# `.add_polity_columns_dt()`, but the column is dropped from published outputs,
# so without these helpers it is silent.
#
# MEASURED on the real 58,795-cell `spatialize-country-grid` (178 area codes):
# 3,181 of 30,438 (area_code, year) pairs over 1850-2020, 22 of 178 area codes,
# 15,100 of 58,795 cells -- the post-Soviet and post-Yugoslav successors, plus
# Sudan and South Sudan. Even a modern-only module is exposed: over 2000-2020
# it is still 34 pairs over codes 272, 273, 276 and 277.
#
# The rows are usually not wrong about the physics; the cells are real
# territory and the quantity is per cell. Only the polity NAME is anachronistic.
# Dropping them therefore deletes valid data, which is why `"keep"` is the
# default everywhere this is offered (whep#462, whep#675).

# Every entry point spells the argument the same way --
# `polity_validity = c("keep", "flag", "drop")`, default `"keep"` -- and routes
# through one of the two helpers below, so the ~8 of them cannot drift apart.
#
# Report -- and, per `polity_validity`, drop or flag -- the rows whose
# (area_code, year) resolves to a polity that did not exist in that year, then
# attach the reporting-polity columns. For consumers whose tail already is
# `.add_reporting_polity_columns()`; the flag comes from the resolver's own
# `mapping_status`, exactly as whep#674 shipped it.
.resolve_polity_validity <- function(table, polity_validity) {
  status <- if (polity_validity == "flag") "flag" else NULL
  .apply_polity_validity(table, polity_validity) |>
    .add_reporting_polity_columns(mapping_status = status)
}

# The same three modes for a table that carries no reporting-polity columns:
# there is no `mapping_status` to derive the flag from, so `attach_flag = TRUE`
# attaches it here from the same gap set instead. The column name and the
# warning are identical either way -- that is the whole point of one helper.
.apply_polity_validity <- function(
  table,
  polity_validity,
  attach_flag = FALSE
) {
  gaps <- .polity_validity_gaps(table)
  .warn_polity_validity(table, gaps, polity_validity)
  if (polity_validity == "drop" && nrow(gaps) > 0L) {
    return(dplyr::anti_join(table, gaps, by = c("area_code", "year")))
  }
  if (polity_validity == "flag" && attach_flag) {
    return(.mark_polity_validity(table, gaps))
  }
  table
}

# The (area_code, year) pairs of `table` whose polity is a nearest-period
# stand-in. Resolved on the DISTINCT pairs, not the rows: a gridded table is
# millions of rows over at most a few thousand pairs, and
# `polity_coverage_gaps()` resolves whatever it is handed. Using the exported
# diagnostic rather than a second hand-rolled lookup is what guarantees the
# report describes the rows `.add_reporting_polity_columns()` really substituted.
#
# SCOPED TO THE STAND-INS, deliberately. whep#763 made the diagnostic also
# report the back-cast anchor class -- a pre-anchor row labelled with the polity
# live in the anchor year -- which is 12,208 `(area, year)` cells against the
# 2,301 stand-in pairs of a real `get_primary_production()`. Those two are not
# the same defect and must not share a fate: `polity_validity = "drop"` deleting
# every pre-1961 row of the 140 areas whose anchor polity starts later is a
# modelling decision about the back-cast convention itself (whep#748), not the
# year-less cell crosswalk this argument exists for. Widening it here would move
# published values under `"drop"` and `"flag"`, which whep#763 explicitly does
# not do.
.polity_validity_gaps <- function(table) {
  if (!all(c("area_code", "year") %in% names(table))) {
    return(tibble::tibble(area_code = integer(0), year = integer(0)))
  }
  dplyr::distinct(table, area_code, year) |>
    polity_coverage_gaps() |>
    dplyr::filter(.data$gap_kind != "backcast_anchor") |>
    dplyr::select(area_code, year)
}

# The per-row logical `"flag"` promises, for a table with no polity tail.
.mark_polity_validity <- function(table, gaps) {
  if (!all(c("area_code", "year") %in% names(table)) || nrow(gaps) == 0L) {
    return(dplyr::mutate(table, reporting_polity_out_of_span = FALSE))
  }
  flagged <- dplyr::mutate(gaps, reporting_polity_out_of_span = TRUE)
  table |>
    dplyr::left_join(flagged, by = c("area_code", "year")) |>
    dplyr::mutate(
      reporting_polity_out_of_span = !is.na(
        .data$reporting_polity_out_of_span
      )
    )
}

# Name what the stand-ins are. The message says whether the rows were kept,
# flagged or dropped, so a log line is self-explanatory about which of the
# three ran.
.warn_polity_validity <- function(table, gaps, polity_validity) {
  if (nrow(gaps) == 0L) {
    return(invisible(NULL))
  }
  n_rows <- nrow(dplyr::semi_join(table, gaps, by = c("area_code", "year")))
  codes <- sort(unique(gaps$area_code))
  fate <- c(
    keep = "kept as-is",
    flag = "kept and flagged in reporting_polity_out_of_span",
    drop = "dropped"
  )[[polity_validity]]
  cli::cli_warn(c(
    "!" = "{n_rows} row{?s} over {length(codes)} area code{?s} resolve to a
      polity that did not exist in that row's year (years
      {min(gaps$year)}-{max(gaps$year)}); they are {fate}.",
    i = "The cell-polity crosswalk has no year dimension, so an early cell
      carries its present-day territory. Area codes: {codes}.",
    i = "{.fn polity_coverage_gaps} names the polity each one landed on;
      {.code polity_validity = \"drop\"} removes them."
  ))
}
