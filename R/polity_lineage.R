#' Resolve a national row onto the polity its grid support carries that year
#'
#' @description
#' The national tables and the cell support are keyed on **different polity
#' vintages**. A 1961 harvested-area row is keyed `area_code` 185, whose
#' reporting polity is `RUS-1991-2014`; a year-aware read of the polycell
#' support holds `F228-1945-1991` (the USSR) in 1961 and no Russian polity at
#' all. The row then matches no cell and
#' `build_gridded_landuse()`'s missing-reporter guard drops its whole national
#' total.
#'
#' This is the **national-side lineage step**: for every `(area_code, year)` it
#' walks the `predecessor` edges of [polities] from the row's reporting polity
#' back to the polity the supplied `support` actually carries in that year, and
#' records which rule answered. It moves no value and allocates nothing --
#' splitting a predecessor's cells between its successors is the consumer's
#' job, and this function only says **which polity's cells** a row belongs on.
#'
#' Measured on the live `polycell_support` pin (`20260907T111653Z-e654d`) and
#' the `spatialize-country-areas` pin, over the 1961 crop tables: 27 of 194
#' reporting areas carry 164.2 Mha of harvested area (17.2% of the world) that
#' a year-aware support cannot place. Under `basis = "historical_polity"` the
#' lineage places 21 of them, 156.5 Mha, leaving a 7.7 Mha (0.81%) residue
#' that is **not** a lineage failure: 0.13 Mha is four Rest-of-region
#' reporting buckets, which are aggregate polities the support excludes by
#' construction under either vintage, and 7.6 Mha is Viet Nam and Yemen, whose
#' reporting areas resolve to aggregate polities (`F237-1954-1975`,
#' `F249-1918-1990`) whose members the support carries separately -- splitting
#' those between members needs a share rule this function does not invent.
#'
#' @section Which vintage binds is a choice, not a fact:
#' Two bases are defensible and they differ numerically, so `basis` selects
#' between them and the answer is stamped on every row in
#' `method_polity_lineage`. They are alternatives, never a silent fallback.
#'
#' \describe{
#'   \item{`"historical_polity"`}{The default and the more rigorous. A row is
#'     understood as a share of the entity that reported in that year -- the
#'     FAOSTAT back-series is a modern-territory reconstruction of it -- and is
#'     placed on that entity's cells. Conserves every national total against a
#'     year-aware support; leaves the successor's share of the predecessor's
#'     cells for the consumer to decide.}
#'   \item{`"constant_territory"`}{The row keeps its reporting polity, i.e.
#'     modern borders wherever a territory changed. Simpler, and what a
#'     present-day snapshot support does implicitly, but it keeps the
#'     attribution error at every succession: 164.2 Mha of 1961 harvested area
#'     has no cell to land on.}
#' }
#'
#' @section The support is an input, and its supply is asserted:
#' `support` is required and is checked for rows covering every year in
#' `national`, because a lineage resolved against [polities] alone can be
#' confidently wrong. `polities` carries two overlapping Yugoslav intervals,
#' `F248-1920-1991` and `F248-1947-1991`, and the published support emitted
#' only the second: a walk that consulted the edges alone would answer
#' `F248-1920-1991`, satisfy every totals check, and still find no cell. An
#' edgeless `polities` is refused for the same reason -- it would return every
#' anchor unchanged and reconcile perfectly.
#'
#' @param national A tibble of national rows with at least `area_code` and
#'   `year`. Any other column is carried through untouched; no row is added,
#'   dropped or reordered.
#' @param support A tibble of the polity intervals the spatial support actually
#'   holds cells for, with `polity_code`, `start_year` and `end_year`.
#'   `start_year` is inclusive and `end_year` exclusive, the [polities]
#'   convention. `read_polycell_support()` returns a superset of these columns.
#' @param basis Which polity vintage binds a row, `"historical_polity"`
#'   (default) or `"constant_territory"`. See *Which vintage binds is a
#'   choice, not a fact*.
#' @param polities The polity table carrying the `predecessor` edges the walk
#'   follows; defaults to [polities]. Supplied as an argument so a caller can
#'   pin a vintage rather than inherit whichever one the package ships.
#'
#' @return `national` with three columns added: `lineage_polity_code`, the
#'   polity whose cells the row belongs on, `lineage_polity_name`, and
#'   `method_polity_lineage`, one of `"anchor"` (the reporting polity is
#'   itself carried at that year), `"predecessor"` (a predecessor is),
#'   `"sibling_interval"` (a different interval of the polity the walk landed
#'   on is -- the support and `polities` disagree about the interval),
#'   `"constant_territory"`, or `"unresolved"`. An unresolved row keeps `NA`
#'   rather than being dropped, so the gap stays visible, and is warned about
#'   with condition class `whep_lineage_unresolved`.
#'
#' @export
#'
#' @examples
#' national <- tibble::tribble(
#'   ~area_code, ~year, ~harvested_area_ha,
#'          185, 1961L,           85049155,
#'          185, 2015L,           45000000
#' )
#' support <- tibble::tribble(
#'   ~polity_code, ~start_year, ~end_year,
#'   "F228-1945-1991",      1945L,     1991L,
#'   "RUS-2014-2025",       2014L,     2025L
#' )
#' resolve_polity_lineage(national, support)
resolve_polity_lineage <- function(
  national,
  support,
  basis = c("historical_polity", "constant_territory"),
  polities = whep::polities
) {
  basis <- rlang::arg_match(basis)
  .check_columns(national, c("area_code", "year"), "national")
  .check_columns(
    support,
    c("polity_code", "start_year", "end_year"),
    "support"
  )
  years <- sort(unique(as.integer(national$year)))
  .check_lineage_support(support, years)
  .check_lineage_edges(polities, basis)

  pairs <- .lineage_anchor_pairs(national)
  resolved <- if (basis == "constant_territory") {
    .lineage_constant_territory(pairs)
  } else {
    .lineage_walk(pairs, support, polities)
  }
  .lineage_attach(national, resolved, polities)
}

# --- Private helpers ----------------------------------------------------------

# How far back the walk follows `predecessor` before giving up. The longest
# real chain on the shipped polities is four steps (a post-Yugoslav successor
# through F248-1991-1992 and F248-1920-1991); the cap exists so a cycle
# introduced upstream terminates instead of hanging.
.lineage_max_depth <- function() 12L

# An empty support is not a support. Without this the function would answer
# every row "unresolved" and look like a lineage that found nothing, which is
# indistinguishable from a lineage that was never given anything to find --
# the shape a zero-filled optional layer already produced once in this package.
.check_lineage_support <- function(support, years) {
  if (nrow(support) == 0L) {
    cli::cli_abort(
      c(
        "{.arg support} holds no polity interval.",
        x = "A lineage resolved against an empty support cannot distinguish
             a polity the grid does not carry from a grid that carries
             nothing.",
        i = "Pass the intervals the spatial support really holds, e.g.
             {.fun read_polycell_support}."
      ),
      class = "whep_lineage_support_empty"
    )
  }
  covered <- purrr::map_lgl(
    years,
    \(yr) any(support$start_year <= yr & support$end_year > yr)
  )
  if (all(covered)) {
    return(invisible(NULL))
  }
  gaps <- years[!covered]
  cli::cli_abort(
    c(
      "{.arg support} carries no polity interval covering
       {length(gaps)} of the {length(years)} year{?s} in {.arg national}.",
      x = "Uncovered: {.val {gaps}}.",
      i = "Every row in those years would resolve to {.val {NA}}, which reads
           as a lineage gap rather than as a support that was never supplied."
    ),
    class = "whep_lineage_support_year"
  )
}

# The `predecessor` edges are the walk's only input. A `polities` table without
# them returns every anchor unchanged and reconciles perfectly -- an invariant
# that holds by construction cannot detect the missing input, so the input is
# asserted instead.
.check_lineage_edges <- function(polities, basis) {
  .check_columns(
    polities,
    c("polity_code", "polity_name", "start_year", "end_year", "predecessor"),
    "polities"
  )
  if (basis == "constant_territory") {
    return(invisible(NULL))
  }
  n_edges <- sum(!is.na(polities$predecessor))
  if (n_edges > 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg polities} carries no {.field predecessor} edge.",
      x = "With no edge the walk returns every reporting polity unchanged,
           which is {.code basis = \"constant_territory\"} under another
           name.",
      i = "Rebuild the polity tables, or ask for
           {.code basis = \"constant_territory\"} explicitly."
    ),
    class = "whep_lineage_no_edges"
  )
}

# The distinct (area_code, year) pairs and the reporting polity each resolves
# to. Resolved on the pairs rather than the rows: a national table is millions
# of rows over a few thousand pairs, and the answer is a property of the pair.
.lineage_anchor_pairs <- function(national) {
  pairs <- national |>
    dplyr::distinct(area_code, year) |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      year = as.integer(year)
    )
  pairs |>
    .add_reporting_polity_columns() |>
    dplyr::transmute(
      pair_id = dplyr::row_number(),
      area_code,
      year,
      code = reporting_polity_code
    )
}

# `basis = "constant_territory"`: the reporting polity stands, whether or not
# the support carries it. That is the status quo faithfully reproduced -- the
# row keeps modern borders and the consumer's own missing-reporter guard is
# what reports the 164.2 Mha of 1961 harvested area with nowhere to land. Only
# a row with no reporting polity at all is `"unresolved"`.
.lineage_constant_territory <- function(pairs) {
  pairs |>
    dplyr::transmute(
      pair_id,
      area_code,
      year,
      lineage_polity_code = code,
      method_polity_lineage = dplyr::if_else(
        is.na(code),
        "unresolved",
        "constant_territory"
      )
    )
}

# Breadth-first over the `predecessor` edges. A `while` rather than a map: the
# frontier is the whole vector of unresolved pairs at one depth, so each pass
# is vectorised and only the depth is iterated.
.lineage_walk <- function(pairs, support, polities) {
  frontier <- dplyr::transmute(
    pairs,
    pair_id,
    year,
    code,
    depth = 0L,
    path = "0"
  )
  hits <- .lineage_empty_hits()
  depth <- 0L
  while (nrow(frontier) > 0L && depth <= .lineage_max_depth()) {
    step <- .lineage_resolve_frontier(frontier, support)
    hits <- dplyr::bind_rows(hits, step$hits)
    frontier <- .lineage_expand(step$pending, polities)
    depth <- depth + 1L
  }
  .lineage_pick(hits, pairs)
}

.lineage_empty_hits <- function() {
  tibble::tibble(
    pair_id = integer(0),
    lineage_polity_code = character(0),
    method_polity_lineage = character(0),
    depth = integer(0),
    path = character(0)
  )
}

# Split one depth's frontier into the candidates the support carries and those
# it does not.
.lineage_resolve_frontier <- function(frontier, support) {
  carried <- .lineage_carried(frontier, support)
  hit <- !is.na(carried$code)
  hits <- frontier[hit, , drop = FALSE] |>
    dplyr::transmute(
      pair_id,
      lineage_polity_code = carried$code[hit],
      method_polity_lineage = .lineage_method(
        depth,
        carried$is_sibling[hit]
      ),
      depth,
      path
    )
  list(hits = hits, pending = frontier[!hit, , drop = FALSE])
}

# `"sibling_interval"` takes precedence over `"predecessor"` because it is the
# surprising answer: the support and `polities` disagree about which interval
# of one polity exists, and a reader must see that rather than a plain
# succession.
.lineage_method <- function(depth, is_sibling) {
  dplyr::case_when(
    is_sibling ~ "sibling_interval",
    depth == 0L ~ "anchor",
    .default = "predecessor"
  )
}

# For each candidate `(code, year)`, the polity code the support carries at
# that year: the candidate itself, or -- when the support holds a different
# interval of the same polity family -- that interval instead.
.lineage_carried <- function(candidates, support) {
  live <- support |>
    dplyr::distinct(polity_code, start_year, end_year) |>
    dplyr::mutate(
      live_code = polity_code,
      family = .lineage_family(polity_code)
    )
  self <- .lineage_match(candidates, live, "polity_code", "code")
  sibling <- .lineage_match(
    dplyr::mutate(candidates, family = .lineage_family(code)),
    live,
    "family",
    "family"
  )
  tibble::tibble(
    code = dplyr::coalesce(self, sibling),
    is_sibling = is.na(self) & !is.na(sibling)
  )
}

# The polity family a code belongs to: its identity with the `-start-end`
# interval suffix removed. `polities` keys one entity's successive periods on
# the same stem, which is what makes the sibling lookup a code join rather than
# a name one.
.lineage_family <- function(code) {
  stringr::str_remove(code, "-[0-9]+-[0-9]+$")
}

# The lowest-sorting live polity code matching `candidates` on `key`, per
# candidate row. Deterministic by code so the answer cannot depend on row
# order, and `NA` where nothing matches.
.lineage_match <- function(candidates, live, live_key, cand_key) {
  by <- rlang::set_names(live_key, cand_key)
  matched <- candidates |>
    dplyr::mutate(.lineage_row = dplyr::row_number()) |>
    dplyr::inner_join(
      live,
      by = by,
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(live_code),
      start_year <= year,
      end_year > year
    ) |>
    dplyr::arrange(.lineage_row, live_code) |>
    dplyr::distinct(.lineage_row, .keep_all = TRUE)
  out <- rep(NA_character_, nrow(candidates))
  out[matched$.lineage_row] <- matched$live_code
  out
}

# One depth step: replace each pending candidate by its predecessors. A polity
# with several predecessors expands to one candidate each, ranked by the order
# the edge lists them, so the pick below is deterministic.
.lineage_expand <- function(pending, polities) {
  if (nrow(pending) == 0L) {
    return(pending)
  }
  edges <- polities |>
    dplyr::distinct(polity_code, predecessor) |>
    dplyr::filter(!is.na(predecessor))
  pending |>
    dplyr::inner_join(edges, by = c("code" = "polity_code")) |>
    dplyr::mutate(code = stringr::str_split(predecessor, ";")) |>
    tidyr::unnest_longer(code, indices_to = "edge_rank") |>
    dplyr::transmute(
      pair_id,
      year,
      code = stringr::str_trim(code),
      depth = depth + 1L,
      path = paste0(path, "-", .lineage_pad(edge_rank))
    ) |>
    dplyr::filter(!is.na(code), code != "")
}

# Zero-padded so `path` sorts as a lexicographic path rather than a string in
# which "10" precedes "2".
.lineage_pad <- function(rank) {
  formatC(as.integer(rank), width = 3L, flag = "0")
}

# The nearest answer per pair: fewest predecessor steps, then the edge order.
.lineage_pick <- function(hits, pairs) {
  picked <- hits |>
    dplyr::arrange(pair_id, depth, path) |>
    dplyr::distinct(pair_id, .keep_all = TRUE) |>
    dplyr::select(pair_id, lineage_polity_code, method_polity_lineage)
  pairs |>
    dplyr::select(pair_id, area_code, year) |>
    dplyr::left_join(picked, by = "pair_id") |>
    dplyr::mutate(
      method_polity_lineage = dplyr::coalesce(
        method_polity_lineage,
        "unresolved"
      )
    )
}

# Join the per-pair answer back onto every row and name the polity. The join is
# on `(area_code, year)` integers, never on a label, and cannot change the row
# count: `resolved` is one row per distinct pair of `national`.
.lineage_attach <- function(national, resolved, polities) {
  names_by_code <- polities |>
    dplyr::select("polity_code", "polity_name") |>
    dplyr::distinct(polity_code, .keep_all = TRUE)
  out <- national |>
    dplyr::mutate(
      .lineage_area = as.integer(area_code),
      .lineage_year = as.integer(year)
    ) |>
    dplyr::left_join(
      dplyr::select(resolved, -"pair_id"),
      by = c(".lineage_area" = "area_code", ".lineage_year" = "year")
    ) |>
    dplyr::left_join(
      names_by_code,
      by = c("lineage_polity_code" = "polity_code")
    ) |>
    dplyr::rename(lineage_polity_name = polity_name) |>
    dplyr::select(-".lineage_area", -".lineage_year")
  .warn_lineage_unresolved(out)
  out
}

.warn_lineage_unresolved <- function(out) {
  gaps <- out |>
    dplyr::filter(method_polity_lineage == "unresolved") |>
    dplyr::distinct(area_code, year)
  if (nrow(gaps) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(as.integer(gaps$area_code)))
  cli::cli_warn(
    c(
      paste0(
        "{nrow(gaps)} {.field (area_code, year)} pair{?s} resolve to no ",
        "polity the support carries."
      ),
      x = "{cli::qty(length(codes))}{length(codes)} area_code{?s}:
           {.val {codes}}.",
      i = "They keep {.val {NA}} rather than being dropped. A reporting area
           whose polity is an aggregate needs a share rule to reach the
           members the support holds; this step does not invent one."
    ),
    class = "whep_lineage_unresolved"
  )
}
