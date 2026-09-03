#' Resolve one source per container from the admin-shares table
#'
#' @description
#' Choose, for every `(area_code, level, item_prod_code, year)` the
#' admin-shares table covers, the single source whose reported units set
#' the within-country shape, and record why it won. Operates on **observed**
#' rows only: gap filling is a separate step that runs after resolution, so
#' an `interpolated` or `carried` row here is an error, not an input.
#'
#' The result is a list of five tibbles rather than one table with
#' attributes, because every piece is itself evidence a later task reads,
#' and an attribute does not survive a write to parquet or CSV.
#'
#' @section Precedence:
#' Within one candidate group -- `(area_code, level, item_prod_code,
#' indicator_used, year)` -- each source is one candidate. Candidates are
#' ranked, best first, by:
#'
#' 1. **Override**: a source named for this container in `overrides` wins
#'    wherever it is a candidate, and every group it reaches is recorded
#'    `resolution_rule = "override"` -- including a group where the named
#'    source had no rival, so an override's footprint can be read off the
#'    coverage report. This is the per-country escape hatch the grain rule
#'    requires: an override may keep a coarser source over a finer one.
#' 2. **Grain**, finer first: `"admin1" < "admin2" < "admin3"`, compared
#'    through `match()` on that vocabulary, exactly as
#'    [admin_shares_schema()] declares it. Grain beats tier, under decision
#'    4 as amended at lock: public first at equal or finer grain, in-house
#'    where no public source of equal grain exists. So Spain keeps its 53
#'    NUTS-3 provinces (tier 2, `"admin2"`) over Eurostat's NUTS-2 (tier 1,
#'    `"admin1"`). A candidate reporting at several grains at once is
#'    ranked on its **coarsest**, and warns.
#' 3. **Tier**, lower first.
#' 4. **Run length**, longer first: the contiguous run of that source
#'    around that year, defined below.
#' 5. **Source name**, ascending in the C locale, with a warning naming the
#'    tied sources. This is the deterministic last resort, never a
#'    scientific rule.
#'
#' Before any of that, the indicator in force is chosen per `(area_code,
#' level, item_prod_code, year)` by decision 8's order of acceptance --
#' `"area_harvested"`, `"area_planted_or_sown"`, `"area_main"`,
#' `"area_cultivated"`, then `"production"` only where no area exists.
#' Rows of a losing indicator go to `dropped` with `drop_reason =
#' "indicator_precedence"`. `"yield"` is in the table's vocabulary but
#' never binds an allocation (a share of a ratio is not a share), so a
#' `"yield"` row is dropped as `"indicator_never_binds"`.
#'
#' @section Contiguous run:
#' The run length of source `s` at year `y` is the number of years in the
#' maximal block of **consecutive** years containing `y` in which `s`
#' supplies at least one row for the same `(area_code, level,
#' item_prod_code, indicator_used)` -- the candidate group's columns except
#' `year`. So a source present in 1990:2000 has run length 11 at every one
#' of those years; if it is absent in 1997, its run length is 7 at 1996 and
#' 3 at 1998. The run is measured on the rows that actually reach ranking,
#' after `constraint_exclude` and after indicator precedence: a long run of production
#' years says nothing about the continuity of an area series, and a year
#' withheld for validation is not evidence of coverage.
#'
#' @section Seams:
#' One row per `(area_code, level, item_prod_code)` series, seam year and
#' `seam_kind`:
#'
#' - `"start"`: the series' first resolved year. `value_to` is the source
#'   it starts on.
#' - `"source_switch"`, `"grain_switch"`, `"nuts_version_switch"`,
#'   `"indicator_switch"`: the resolved value differs from the previous
#'   **resolved** year of that series, which is the previous year present,
#'   not necessarily `year - 1`.
#' - `"coverage_change"`: the set of reporting units differs from the
#'   previous resolved year -- a unit entering or leaving reporting.
#'
#' A missing value and a present one count as a change; two missing values
#' do not.
#'
#' @section Coverage report:
#' One row per `(area_code, level, item_prod_code, year)`, carrying the
#' resolved source, tier, grain, indicator, NUTS version and rule, the
#' number of reporting units, the units themselves as a `"|"`-joined string
#' of `level_polity_code` sorted in the C locale (a string, not a
#' list-column, so the table writes to parquet and CSV unchanged;
#' unresolved units appear as `"<NA>"` and count as one member), and
#' `coverage_change`, and `not_shipped`, which marks resolved rows whose
#' source family is withheld from the pin board (T23).
#'
#' @param shares Admin-shares table, conforming to [admin_shares_schema()]
#'   except that `source` joins the key: a multi-source union is exactly
#'   what resolution consumes, and the contract's key holds within one
#'   source. Every row must have `treatment_year == "observed"`.
#' @param overrides Optional tibble of forced choices, with columns
#'   `area_code`, `source` and optionally `item_prod_code` (`NA`, or the
#'   column absent, means every item of that container). An item-specific
#'   row beats a container-wide one. A row that never forces a winner
#'   warns.
#' @param not_shipped Character vector of `source` labels whose family is
#'   withheld from the pin board (plan T23/T24); the coverage report flags
#'   every resolved row whose source is one of them. Empty by default.
#' @param constraint_exclude Optional named list of years to withhold, keyed by
#'   `area_code` as a character name, such as `list("840" = 1961:1989)`.
#'   Those container-years leave the constraint set and are returned in
#'   `excluded`, never silently dropped. This is the leave-years-out
#'   validation switch.
#'
#' @return A list of five tibbles:
#'
#' - `shares`: the winning rows, each exactly as supplied plus
#'   `resolved_source`, `resolved_tier`, `resolved_grain` and
#'   `resolution_rule` (one of `"override"`, `"grain"`, `"tier"`,
#'   `"run_length"`, `"source_name"`, `"single_candidate"`).
#' - `seams`: the seam list described above.
#' - `coverage`: the coverage report described above.
#' - `excluded`: the rows `constraint_exclude` withheld, in the input's shape.
#' - `dropped`: the losing rows, in the input's shape plus `drop_reason`
#'   (`"indicator_never_binds"`, `"indicator_precedence"`, `"override"`,
#'   `"grain"`, `"tier"`, `"run_length"`, `"source_name"`).
#'
#' Every input row appears in exactly one of `shares`, `excluded` and
#' `dropped`.
#'
#' @export
#'
#' @examples
#' # Spain 2010: Eurostat NUTS-2 (tier 1, admin1) against the in-house
#' # NUTS-3 provinces (tier 2, admin2). Grain beats tier, so tier 2 wins.
#' rows <- tibble::tibble(
#'   area_code = 724L,
#'   level_polity_code = c("ES-N2-A", "ES-N2-B", "ES-N3-1", "ES-N3-2"),
#'   level = 1L,
#'   item_prod_code = 15L,
#'   indicator_used = "area_harvested",
#'   year = 2010L,
#'   value = c(100, 200, 120, 180),
#'   share = c(1 / 3, 2 / 3, 0.4, 0.6),
#'   source = rep(c("Eurostat_apro_cpshr", "ES_provinces"), each = 2),
#'   tier = rep(c(1L, 2L), each = 2),
#'   grain = rep(c("admin1", "admin2"), each = 2),
#'   concept_break = FALSE,
#'   nuts_version = rep(c("2021", "2016"), each = 2),
#'   source_native_id = NA_character_,
#'   source_native_name = NA_character_,
#'   source_id = rep(c("Eurostat_apro_cpshr", "ES_provinces"), each = 2),
#'   source_version = NA_character_,
#'   recorded_at = "2026-01-01T00:00:00Z",
#'   treatment_year = "observed",
#'   value_flag = NA_character_
#' )
#' resolved <- resolve_admin_shares(rows)
#' resolved$coverage
#' resolved$dropped
resolve_admin_shares <- function(
  shares,
  overrides = NULL,
  constraint_exclude = NULL,
  not_shipped = character()
) {
  shares <- .assert_resolvable_shares(shares)
  overrides <- .parse_admin_overrides(overrides)
  split <- .split_excluded_shares(shares, constraint_exclude)
  by_indicator <- .apply_indicator_precedence(split$kept)
  ranked <- .rank_admin_candidates(by_indicator$kept, overrides)
  coverage <- .admin_coverage_report(ranked$winners, not_shipped)

  list(
    shares = ranked$winners,
    seams = .admin_seam_list(coverage),
    coverage = coverage,
    excluded = split$excluded,
    dropped = dplyr::bind_rows(by_indicator$dropped, ranked$dropped)
  )
}

# Vocabularies and keys --------------------------------------------------

# Ascending fineness, the order [admin_shares_schema()] declares for
# `grain`. Compared through `match()` because the schema stores `grain` as
# character: `check_table_schema()` has no ordered-factor type.
.admin_grain_levels <- function() {
  c("admin1", "admin2", "admin3")
}

.admin_grain_rank <- function(grain) {
  match(grain, .admin_grain_levels())
}

# Decision 8 (plan, "Decisions", signed off at lock 2026-09-02): the order
# in which indicators are accepted as the binding constraint. "yield" is in
# the table's vocabulary but absent here on purpose -- the plan makes the
# yield contrast a reconciliation diagnostic, "never the allocation".
.admin_indicator_order <- function() {
  c(
    "area_harvested",
    "area_planted_or_sown",
    "area_main",
    "area_cultivated",
    "production"
  )
}

.admin_group_cols <- function() {
  c("area_code", "level", "item_prod_code", "indicator_used", "year")
}

.admin_run_cols <- function() {
  c("area_code", "level", "item_prod_code", "indicator_used")
}

.admin_series_cols <- function() {
  c("area_code", "level", "item_prod_code")
}

.admin_seam_cols <- function() {
  c(
    .admin_series_cols(),
    "seam_year",
    "seam_kind",
    "previous_year",
    "value_from",
    "value_to"
  )
}

# Input validation -------------------------------------------------------

# The contract keyed on `source` as well: within one source the contract's
# key holds, and a multi-source union is what resolution exists to consume.
.admin_resolve_schema <- function() {
  schema <- admin_shares_schema()
  schema$key <- c(schema$key, "source")
  schema
}

.assert_resolvable_shares <- function(shares) {
  if (!is.data.frame(shares)) {
    cli::cli_abort(
      "{.arg shares} must be a data frame, not
       {.obj_type_friendly {shares}}.",
      class = "whep_error_admin_resolve_input"
    )
  }
  shares <- tibble::as_tibble(shares)
  assert_table_schema(shares, .admin_resolve_schema(), arg = "shares")
  .abort_unobserved_shares(shares)
  ensure_columns(shares, admin_shares_prototype())
}

.abort_unobserved_shares <- function(shares) {
  treatment <- shares$treatment_year
  other <- sort(unique(treatment[treatment != "observed"]))
  if (length(other) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg shares} must hold observed rows only.",
      "x" = "{.field treatment_year} also takes {.val {other}}.",
      "i" = "Gap filling runs after resolution, never as an input to it."
    ),
    class = "whep_error_admin_not_observed"
  )
}

# Exclusion --------------------------------------------------------------

.split_excluded_shares <- function(shares, constraint_exclude) {
  withheld <- .parse_exclude_years(constraint_exclude)
  if (nrow(withheld) == 0L) {
    return(list(kept = shares, excluded = shares[0L, ]))
  }
  keys <- c("area_code", "year")
  list(
    kept = dplyr::anti_join(shares, withheld, by = keys),
    excluded = dplyr::semi_join(shares, withheld, by = keys)
  )
}

.parse_exclude_years <- function(constraint_exclude) {
  # An empty list is a caller withholding nothing, not a malformed
  # argument: a configuration that builds the list programmatically will
  # hand one over whenever no container is being held out.
  if (
    is.null(constraint_exclude) ||
      (is.list(constraint_exclude) && length(constraint_exclude) == 0L)
  ) {
    return(tibble::tibble(area_code = integer(), year = integer()))
  }
  .abort_bad_exclude_shape(constraint_exclude)
  codes <- suppressWarnings(as.integer(names(constraint_exclude)))
  if (anyNA(codes)) {
    cli::cli_abort(
      c(
        "{.arg constraint_exclude} must be keyed by {.field area_code}.",
        "x" = "{.val {names(constraint_exclude)[is.na(codes)]}} {?is/are} not a whole
               number."
      ),
      class = "whep_error_admin_exclude"
    )
  }
  purrr::map2(codes, constraint_exclude, .admin_exclude_rows) |>
    dplyr::bind_rows() |>
    dplyr::distinct()
}

.abort_bad_exclude_shape <- function(constraint_exclude) {
  named <- is.list(constraint_exclude) &&
    !is.null(names(constraint_exclude)) &&
    !any(is.na(names(constraint_exclude)) | names(constraint_exclude) == "")
  if (named) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg constraint_exclude} must be a list named by {.field area_code}.",
      "i" = "For example {.code list(\"840\" = 1961:1989)}."
    ),
    class = "whep_error_admin_exclude"
  )
}

.admin_exclude_rows <- function(code, years) {
  whole <- is.numeric(years) && !anyNA(years) && all(years == trunc(years))
  if (!whole) {
    cli::cli_abort(
      c(
        "Each {.arg constraint_exclude} element must be whole years.",
        "x" = "Container {.val {code}} was given
               {.obj_type_friendly {years}}."
      ),
      class = "whep_error_admin_exclude"
    )
  }
  tibble::tibble(area_code = as.integer(code), year = as.integer(years))
}

# Indicator precedence ---------------------------------------------------

.apply_indicator_precedence <- function(shares) {
  if (nrow(shares) == 0L) {
    return(list(kept = shares, dropped = .dropped_shares_prototype()))
  }
  accepted <- .admin_indicator_order()
  never <- shares |>
    dplyr::filter(!indicator_used %in% accepted) |>
    dplyr::mutate(drop_reason = "indicator_never_binds")
  ranked <- shares |>
    dplyr::filter(indicator_used %in% accepted) |>
    dplyr::mutate(indicator_rank = match(indicator_used, accepted)) |>
    dplyr::mutate(
      keep_indicator = indicator_rank == min(indicator_rank),
      .by = dplyr::all_of(c(.admin_series_cols(), "year"))
    )

  list(
    kept = .strip_indicator_columns(dplyr::filter(ranked, keep_indicator)),
    dropped = dplyr::bind_rows(
      never,
      ranked |>
        dplyr::filter(!keep_indicator) |>
        .strip_indicator_columns() |>
        dplyr::mutate(drop_reason = "indicator_precedence")
    )
  )
}

.strip_indicator_columns <- function(shares) {
  dplyr::select(shares, -indicator_rank, -keep_indicator)
}

# Candidate ranking ------------------------------------------------------

.rank_admin_candidates <- function(shares, overrides) {
  if (nrow(shares) == 0L) {
    return(list(
      winners = .resolved_shares_prototype(),
      dropped = .dropped_shares_prototype()
    ))
  }
  ranked <- shares |>
    .admin_candidate_table() |>
    .match_admin_overrides(overrides) |>
    .add_admin_run_lengths() |>
    .rank_candidate_groups()
  .warn_admin_name_ties(ranked)
  .warn_unused_overrides(ranked, overrides)
  .split_admin_winners(shares, ranked)
}

# One row per candidate group and source. `tier` is a property of the
# source, so more than one within a candidate is a contradiction in the
# input, not something to resolve; `grain` may legitimately vary, and is
# summarised to the candidate's coarsest (see the file header).
.admin_candidate_table <- function(shares) {
  candidates <- shares |>
    dplyr::summarise(
      n_tiers = dplyr::n_distinct(tier),
      n_grains = dplyr::n_distinct(grain),
      grain_rank = min(.admin_grain_rank(grain)),
      # Last, and deliberately so: `summarise()` evaluates its expressions
      # in order and each one sees the columns the earlier ones created, so
      # counting distinct tiers after collapsing `tier` to its minimum
      # would count one tier however many the input carried.
      tier = min(tier),
      .by = dplyr::all_of(c(.admin_group_cols(), "source"))
    )
  .abort_mixed_candidate_tier(candidates)
  .warn_mixed_candidate_grain(candidates)
  dplyr::select(candidates, -n_tiers, -n_grains)
}

.abort_mixed_candidate_tier <- function(candidates) {
  offenders <- dplyr::filter(candidates, n_tiers > 1L)
  if (nrow(offenders) == 0L) {
    return(invisible(NULL))
  }
  sources <- sort(unique(offenders$source), method = "radix")
  cli::cli_abort(
    c(
      "Each source must carry one {.field tier} per candidate group.",
      "x" = "{.val {sources}} carr{?ies/y} more than one.",
      "i" = "A tier is a property of the source, not of the row."
    ),
    class = "whep_error_admin_tier_conflict"
  )
}

.warn_mixed_candidate_grain <- function(candidates) {
  offenders <- dplyr::filter(candidates, n_grains > 1L)
  if (nrow(offenders) == 0L) {
    return(invisible(NULL))
  }
  sources <- sort(unique(offenders$source), method = "radix")
  cli::cli_warn(c(
    "{nrow(offenders)} candidate{?s} report{?s/} at more than one grain.",
    "i" = "{.val {sources}} {?is/are} ranked on the coarsest grain
           reported; the finer units are kept, not dropped."
  ))
}

# `run_id` increments whenever the year gap to the previous row of the same
# source is not 1, so the rows sharing a `run_id` are exactly one maximal
# block of consecutive years. The arrange is load-bearing: `diff()` reads
# the rows in the order it finds them.
.add_admin_run_lengths <- function(candidates) {
  candidates |>
    dplyr::arrange(
      area_code,
      level,
      item_prod_code,
      indicator_used,
      source,
      year,
      .locale = "C"
    ) |>
    dplyr::mutate(
      run_id = cumsum(c(1L, as.integer(diff(year) != 1L))),
      .by = dplyr::all_of(c(.admin_run_cols(), "source"))
    ) |>
    dplyr::mutate(
      run_length = dplyr::n(),
      .by = dplyr::all_of(c(.admin_run_cols(), "source", "run_id"))
    ) |>
    dplyr::select(-run_id)
}

.rank_candidate_groups <- function(candidates) {
  candidates |>
    dplyr::arrange(
      area_code,
      level,
      item_prod_code,
      indicator_used,
      year,
      dplyr::desc(forced),
      dplyr::desc(grain_rank),
      tier,
      dplyr::desc(run_length),
      source,
      .locale = "C"
    ) |>
    dplyr::mutate(
      candidate_rank = dplyr::row_number(),
      resolution_rule = .admin_winner_rule(
        forced,
        grain_rank,
        tier,
        run_length
      ),
      drop_reason = .admin_drop_reason(forced, grain_rank, tier, run_length),
      tied_top = grain_rank == grain_rank[1] &
        tier == tier[1] &
        run_length == run_length[1],
      .by = dplyr::all_of(.admin_group_cols())
    )
}

# Called once per candidate group, on that group's rows in rank order:
# position 1 is the winner and position 2 the runner-up it had to beat.
# An override is reported ahead of "single_candidate" so that every group
# an override reached is visible, including the ones where the source it
# names had no rival.
.admin_winner_rule <- function(forced, grain_rank, tier, run_length) {
  if (forced[1]) {
    return("override")
  }
  if (length(forced) == 1L) {
    return("single_candidate")
  }
  if (grain_rank[1] > grain_rank[2]) {
    return("grain")
  }
  if (tier[1] < tier[2]) {
    return("tier")
  }
  if (run_length[1] > run_length[2]) {
    return("run_length")
  }
  "source_name"
}

# Why each candidate lost to position 1. The winner's own entry is computed
# too, and then discarded by the rank filter.
.admin_drop_reason <- function(forced, grain_rank, tier, run_length) {
  dplyr::case_when(
    forced[1] ~ "override",
    grain_rank < grain_rank[1] ~ "grain",
    tier > tier[1] ~ "tier",
    run_length < run_length[1] ~ "run_length",
    .default = "source_name"
  )
}

.split_admin_winners <- function(shares, ranked) {
  keys <- c(.admin_group_cols(), "source")
  winners <- shares |>
    dplyr::inner_join(
      ranked |>
        dplyr::filter(candidate_rank == 1L) |>
        dplyr::select(dplyr::all_of(c(keys, "resolution_rule"))),
      by = keys,
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      resolved_source = source,
      resolved_tier = tier,
      resolved_grain = grain
    ) |>
    dplyr::relocate(resolution_rule, .after = resolved_grain)

  list(
    winners = winners,
    dropped = dplyr::inner_join(
      shares,
      ranked |>
        dplyr::filter(candidate_rank > 1L) |>
        dplyr::select(dplyr::all_of(c(keys, "drop_reason"))),
      by = keys,
      relationship = "many-to-one"
    )
  )
}

# Overrides --------------------------------------------------------------

.parse_admin_overrides <- function(overrides) {
  prototype <- tibble::tibble(
    area_code = integer(),
    item_prod_code = integer(),
    source = character()
  )
  if (is.null(overrides)) {
    return(prototype)
  }
  .abort_bad_override_shape(overrides)
  parsed <- overrides |>
    tibble::as_tibble() |>
    ensure_columns(prototype, extra = "drop") |>
    dplyr::distinct()
  .abort_bad_override_values(parsed)
  parsed
}

.abort_bad_override_shape <- function(overrides) {
  required <- c("area_code", "source")
  if (is.data.frame(overrides) && all(rlang::has_name(overrides, required))) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg overrides} must be a table with {.field area_code} and
       {.field source}.",
      "i" = "{.field item_prod_code} is optional; absent or missing means
             every item of that container."
    ),
    class = "whep_error_admin_overrides"
  )
}

.abort_bad_override_values <- function(overrides) {
  incomplete <- is.na(overrides$area_code) | is.na(overrides$source)
  # `duplicated()` rather than a grouped count: this is a key check on a
  # user-supplied policy table, not an aggregation of data, and a grouped
  # count would put a year-free territorial grouping on the whep#669
  # ledger for something that reads no value.
  repeated <- duplicated(overrides[, c("area_code", "item_prod_code")])
  if (!any(incomplete) && !any(repeated)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg overrides} must name one source per container and item.",
      "x" = "{sum(incomplete)} row{?s} miss{?es/} {.field area_code} or
             {.field source}.",
      "x" = "{sum(repeated)} row{?s} repeat{?s/} a container-item pair."
    ),
    class = "whep_error_admin_overrides"
  )
}

.match_admin_overrides <- function(candidates, overrides) {
  numbered <- dplyr::mutate(overrides, override_id = dplyr::row_number())
  specific <- numbered |>
    dplyr::filter(!is.na(item_prod_code)) |>
    dplyr::select(
      area_code,
      item_prod_code,
      forced_source = source,
      override_id
    )
  general <- numbered |>
    dplyr::filter(is.na(item_prod_code)) |>
    dplyr::select(
      area_code,
      forced_source_all = source,
      override_id_all = override_id
    )

  candidates |>
    dplyr::left_join(
      specific,
      by = c("area_code", "item_prod_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      general,
      by = "area_code",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      override_id = dplyr::coalesce(override_id, override_id_all),
      forced_source = dplyr::coalesce(forced_source, forced_source_all),
      forced = !is.na(forced_source) & source == forced_source
    ) |>
    dplyr::select(-forced_source_all, -override_id_all)
}

.warn_unused_overrides <- function(ranked, overrides) {
  used <- ranked$override_id[ranked$candidate_rank == 1L & ranked$forced]
  unused <- overrides[!seq_len(nrow(overrides)) %in% unique(used), ]
  if (nrow(unused) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{nrow(unused)} {.arg overrides} row{?s} forced no winner.",
    "i" = "Container{?s}: {.val {unique(unused$area_code)}}.",
    "i" = "Source{?s}: {.val {unique(unused$source)}}.",
    "i" = "Either the source is no candidate there, or a more specific
           override covers it."
  ))
}

.warn_admin_name_ties <- function(ranked) {
  tied <- dplyr::filter(ranked, resolution_rule == "source_name" & tied_top)
  if (nrow(tied) == 0L) {
    return(invisible(NULL))
  }
  groups <- dplyr::distinct(
    tied,
    dplyr::pick(dplyr::all_of(.admin_group_cols()))
  )
  sources <- sort(unique(tied$source), method = "radix")
  cli::cli_warn(c(
    "{nrow(groups)} candidate group{?s} tied on grain, tier and run
     length.",
    "i" = "Broken by source name, keeping the first of {.val {sources}}.",
    "i" = "This is determinism, not evidence: an override settles it."
  ))
}

# Coverage and seams -----------------------------------------------------

.admin_coverage_report <- function(winners, not_shipped = character()) {
  not_shipped <- as.character(not_shipped)
  if (nrow(winners) == 0L) {
    return(.admin_coverage_prototype())
  }
  winners |>
    dplyr::summarise(
      resolved_source = .admin_join_unique(resolved_source),
      resolved_tier = min(resolved_tier),
      resolved_grain = .admin_join_unique(resolved_grain),
      resolved_indicator = .admin_join_unique(indicator_used),
      resolved_nuts_version = .admin_join_unique(nuts_version),
      resolution_rule = .admin_join_unique(resolution_rule),
      n_units_reporting = dplyr::n_distinct(level_polity_code),
      reporting_units = .admin_join_units(level_polity_code),
      .by = dplyr::all_of(c(.admin_series_cols(), "year"))
    ) |>
    dplyr::arrange(area_code, level, item_prod_code, year) |>
    dplyr::mutate(
      has_previous = dplyr::row_number() > 1L,
      not_shipped = resolved_source %in% not_shipped,
      coverage_change = has_previous &
        .seam_changed(dplyr::lag(reporting_units), reporting_units),
      .by = dplyr::all_of(.admin_series_cols())
    ) |>
    dplyr::select(-has_previous)
}

.admin_join_unique <- function(values) {
  present <- as.character(values[!is.na(values)])
  if (length(present) == 0L) {
    return(NA_character_)
  }
  paste(sort(unique(present), method = "radix"), collapse = "|")
}

.admin_join_units <- function(codes) {
  units <- unique(dplyr::coalesce(as.character(codes), "<NA>"))
  paste(sort(units, method = "radix"), collapse = "|")
}

.seam_changed <- function(previous, current) {
  both <- !is.na(previous) & !is.na(current)
  (both & previous != current) | xor(is.na(previous), is.na(current))
}

.admin_seam_list <- function(coverage) {
  if (nrow(coverage) == 0L) {
    return(.admin_seams_prototype())
  }
  ordered <- dplyr::arrange(coverage, area_code, level, item_prod_code, year)
  switches <- purrr::map2(
    c(
      "resolved_source",
      "resolved_grain",
      "resolved_nuts_version",
      "reporting_units",
      "resolved_indicator"
    ),
    c(
      "source_switch",
      "grain_switch",
      "nuts_version_switch",
      "coverage_change",
      "indicator_switch"
    ),
    .admin_switch_seams,
    coverage = ordered
  )
  dplyr::bind_rows(.admin_start_seams(ordered), dplyr::bind_rows(switches)) |>
    dplyr::arrange(
      area_code,
      level,
      item_prod_code,
      seam_year,
      seam_kind,
      .locale = "C"
    )
}

.admin_start_seams <- function(coverage) {
  coverage |>
    dplyr::slice_min(
      year,
      n = 1L,
      by = dplyr::all_of(.admin_series_cols())
    ) |>
    dplyr::mutate(
      seam_year = year,
      seam_kind = "start",
      previous_year = NA_integer_,
      value_from = NA_character_,
      value_to = as.character(resolved_source)
    ) |>
    dplyr::select(dplyr::all_of(.admin_seam_cols()))
}

.admin_switch_seams <- function(column, kind, coverage) {
  coverage |>
    dplyr::mutate(switch_value = as.character(coverage[[column]])) |>
    dplyr::mutate(
      previous_year = dplyr::lag(year),
      value_from = dplyr::lag(switch_value),
      has_previous = dplyr::row_number() > 1L,
      .by = dplyr::all_of(.admin_series_cols())
    ) |>
    dplyr::filter(has_previous & .seam_changed(value_from, switch_value)) |>
    dplyr::mutate(
      seam_year = year,
      seam_kind = kind,
      value_to = switch_value
    ) |>
    dplyr::select(dplyr::all_of(.admin_seam_cols()))
}

# Output prototypes ------------------------------------------------------

.resolved_shares_prototype <- function() {
  dplyr::mutate(
    admin_shares_prototype(),
    resolved_source = character(),
    resolved_tier = integer(),
    resolved_grain = character(),
    resolution_rule = character()
  )
}

.dropped_shares_prototype <- function() {
  dplyr::mutate(admin_shares_prototype(), drop_reason = character())
}

.admin_coverage_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    year = integer(),
    resolved_source = character(),
    resolved_tier = integer(),
    resolved_grain = character(),
    resolved_indicator = character(),
    resolved_nuts_version = character(),
    resolution_rule = character(),
    n_units_reporting = integer(),
    reporting_units = character(),
    not_shipped = logical(),
    coverage_change = logical()
  )
}

.admin_seams_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    seam_year = integer(),
    seam_kind = character(),
    previous_year = integer(),
    value_from = character(),
    value_to = character()
  )
}
