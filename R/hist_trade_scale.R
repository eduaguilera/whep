# The `"correct"` setting of `hist_trade_scale` (whep#1085, whep#1117).
#
# The historical trade screen in build_cbs.R flags a pre-1961 row of the
# `historical-trade-*` pins when a single reporter's flow exceeds the largest
# WORLD flow FAOSTAT records for the same item. The flagged rows are not one
# population. Most of their mass is the USA block over roughly 1900-1960, where
# some values are exactly ten times too large, interleaved year to year with
# correct values of the same series (USA cotton lint exports, item 767, run
# 1,524 / 16,437 / 2,160 / 25,291 kt). Others are not a mass under any factor:
# USA item 831, "Tobacco products nes", has no partner that books the flow at
# all after 1909.
#
# This file sorts each flagged row into one of three classes, using only the
# two pins themselves, so each row is judged on its own evidence:
#
#   * "x10" -- a provable ten-fold slip. All four must hold:
#       1. dividing by 10 brings the row within the world bound;
#       2. the row as published is larger than the whole partner side of the
#          pins (the sum of the opposite flow over every other reporter, the
#          same item and year), so it is impossible against its own mirror;
#       3. divided by 10 it is not: it becomes at most the whole partner side;
#       4. it sits one power of ten above the clean neighbours of its own
#          series: log10(value / neighbour) rounds to 1, i.e. the ratio lies
#          in [10^0.5, 10^1.5]. A clean neighbour is a row of the same
#          reporter, item and element within 5 years that is below the world
#          bound and no larger than its own year's partner side.
#   * "not_mass" -- no mirror is consistent with the row as a mass: no other
#     reporter books the opposite flow for that item-year at all, or the row is
#     still larger than the whole partner side after dividing by 10. This is
#     item 831's signature. It can also be a partner side the pins cover thinly
#     (USA natural rubber imports, 836, against exports booked by few
#     producers); either way no factor is demonstrated, so none is applied.
#   * "unexplained" -- everything else: a flagged row whose published value is
#     consistent with its mirror, or that has no clean neighbour, or whose
#     neighbour ratio is not one power of ten.
#
# `"correct"` divides the "x10" rows by 10 and drops the other two classes.
# The factor 10 is the one demonstrated by the data (median log10 ratio to the
# clean neighbours is 1.02 over the rows it selects on the real pins,
# whep#1117); no other factor is ever applied, and a row is corrected only where all four
# conditions hold for that row.
#
# The 5-year neighbour window is an assumption, not a sourced value
# ("assumed, unverified"). Measured on the real pins at 1850-2023: a 3-, 5- and
# 10-year window select 149, 175 and 188 rows (852.3, 1,002.0 and 1,068.8 Mt as
# published). The two other thresholds are not tolerances: the world bound is
# measured from FAOSTAT and the neighbour band is "rounds to one power of ten".

# The only conversion factor this screen ever applies. It is the factor
# whep#1117 measured in the pins, not a chosen value.
.hist_trade_x10_factor <- function() {
  10
}

# Half-width, in years, of the window a flagged row's clean neighbours are
# taken from. Assumed, unverified -- see the sensitivity above.
.hist_trade_neighbour_years <- function() {
  5L
}

# Classify every row of `dt` (the screened historical trade, both elements,
# with `world_max` attached) and return it with `hist_trade_class`: `NA` for a
# row within the world bound, else one of "x10", "not_mass", "unexplained",
# plus the evidence columns `mirror` and `neighbour`.
.classify_hist_trade_scale <- function(dt) {
  out <- data.table::copy(data.table::as.data.table(dt))
  factor <- .hist_trade_x10_factor()
  out[, flagged := !is.na(world_max) & value > world_max]
  out[, mirror := .hist_trade_mirror(out)]
  out[, neighbour := .hist_trade_clean_neighbour(out)]
  out[, hist_trade_class := .hist_trade_class_of(out, factor)]
  out[, flagged := NULL]
  out[]
}

# The per-row rule itself, over the columns `.classify_hist_trade_scale()`
# has attached. Evaluated in this order: a row that fails the mirror is
# `"not_mass"` before its neighbours are looked at.
.hist_trade_class_of <- function(dt, factor) {
  no_mirror <- dt$mirror <= 0 | dt$value / factor > dt$mirror
  powers <- abs(log10(dt$value / dt$neighbour) - log10(factor))
  proven <- dt$value / factor <= dt$world_max &
    dt$value > dt$mirror &
    !is.na(powers) &
    powers <= 0.5
  data.table::fcase(
    !dt$flagged , NA_character_ ,
    no_mirror   , "not_mass"    ,
    proven      , "x10"         ,
    default = "unexplained"
  )
}

# The partner side of each row: the opposite flow (import for an export, and
# the reverse) of the same item and year, summed over every OTHER reporter in
# the pins. Zero when no other reporter books it.
.hist_trade_mirror <- function(dt) {
  opposite <- c(export = "import", import = "export")
  totals <- dt[,
    .(total = sum(value, na.rm = TRUE)),
    by = c("item_code_trade", "element", "year")
  ]
  own <- dt[, c("iso3c", "item_code_trade", "element", "year", "value")]
  data.table::setnames(own, "value", "own")
  keyed <- dt[, c("iso3c", "item_code_trade", "element", "year")]
  keyed[, element := unname(opposite[element])]
  keyed[totals, total := i.total, on = c("item_code_trade", "element", "year")]
  keyed[
    own,
    own := i.own,
    on = c("iso3c", "item_code_trade", "element", "year")
  ]
  data.table::fcoalesce(keyed$total, 0) - data.table::fcoalesce(keyed$own, 0)
}

# Median of each row's clean neighbours: rows of the same reporter, item and
# element within `.hist_trade_neighbour_years()` that are within the world
# bound and no larger than their own year's partner side. `NA` where there is
# none. Needs `mirror` already on `dt`.
.hist_trade_clean_neighbour <- function(dt) {
  window <- .hist_trade_neighbour_years()
  series <- c("iso3c", "item_code_trade", "element")
  clean <- dt[
    (is.na(world_max) | value <= world_max) &
      value > 0 &
      mirror > 0 &
      value <= mirror,
    c(series, "year", "value"),
    with = FALSE
  ]
  data.table::setnames(clean, c("year", "value"), c("near_year", "near"))
  rows <- dt[, c(series, "year"), with = FALSE]
  rows[, row_id := .I]
  pairs <- clean[
    rows,
    on = series,
    allow.cartesian = TRUE,
    nomatch = NULL
  ][abs(near_year - year) <= window]
  medians <- pairs[, .(near = stats::median(near)), by = "row_id"]
  rows[medians, near := i.near, on = "row_id"]
  rows$near
}

# Apply `"correct"`: divide the "x10" rows by the demonstrated factor and drop
# the "not_mass" and "unexplained" ones. Rows within the bound are untouched.
.correct_hist_trade_scale <- function(classified) {
  out <- classified[
    is.na(hist_trade_class) | hist_trade_class == "x10"
  ]
  out[
    hist_trade_class == "x10",
    value := value / .hist_trade_x10_factor()
  ]
  out[, c("hist_trade_class", "mirror", "neighbour") := NULL]
  out[]
}

# One row per flagged pin row, saying what the chosen setting did with it:
# the published and the used value, the action, and for `"correct"` the class
# and the evidence behind it. Attached to the build output as the
# `hist_trade_scale_log` attribute.
.hist_trade_scale_log <- function(classified, method) {
  flagged <- classified[!is.na(world_max) & value > world_max]
  if (!"hist_trade_class" %in% names(flagged)) {
    flagged[, `:=`(
      hist_trade_class = NA_character_,
      mirror = NA_real_,
      neighbour = NA_real_
    )]
  }
  action <- .hist_trade_scale_action(flagged$hist_trade_class, method)
  used <- data.table::fcase(
    action == "kept"                         ,
    flagged$value                            ,
    action == "divided_by_10"                ,
    flagged$value / .hist_trade_x10_factor() ,
    default = 0
  )
  tibble::tibble(
    year = as.integer(flagged$year),
    iso3c = flagged$iso3c,
    item_code_trade = as.integer(flagged$item_code_trade),
    element = flagged$element,
    value_published = flagged$value,
    value_used = used,
    world_max = flagged$world_max,
    mirror = flagged$mirror,
    neighbour = flagged$neighbour,
    hist_trade_class = flagged$hist_trade_class,
    hist_trade_scale_action = action,
    method_hist_trade_scale = method
  )
}

.hist_trade_scale_action <- function(hist_trade_class, method) {
  if (method != "correct") {
    action <- if (method == "report") "kept" else "dropped"
    return(rep(action, length(hist_trade_class)))
  }
  data.table::fcase(
    hist_trade_class == "x10"      ,
    "divided_by_10"                ,
    hist_trade_class == "not_mass" ,
    "dropped_not_mass"             ,
    default = "dropped_unexplained"
  )
}

# Say what `"correct"` did, class by class.
.inform_hist_trade_correct <- function(log) {
  if (nrow(log) == 0L) {
    return(invisible(log))
  }
  by_class <- log |>
    dplyr::summarise(
      n = dplyr::n(),
      mt = sum(.data$value_published) / 1e6,
      .by = "hist_trade_scale_action"
    ) |>
    dplyr::arrange(.data$hist_trade_scale_action)
  lines <- paste0(
    by_class$hist_trade_scale_action,
    ": ",
    by_class$n,
    " rows, ",
    round(by_class$mt, 1),
    " Mt as published"
  )
  names(lines) <- rep("*", length(lines))
  cli::cli_inform(
    c(
      "i" = paste0(
        "{.arg hist_trade_scale} is {.val correct}: each flagged row was ",
        "classified on its own mirror and neighbours (whep#1085)."
      ),
      lines,
      "i" = paste0(
        "Per-row provenance is in the {.field hist_trade_scale_log} ",
        "attribute of the output."
      )
    ),
    class = "whep_hist_trade_correct"
  )
  invisible(log)
}
