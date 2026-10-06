# Carry the reporting identity across a build's reductions (whep#707).
#
# `.aggregate_to_polities()` emits the reporting identity where it creates the
# fold, and `.add_reporting_polity_columns()` keeps a carried identity instead of
# resolving it a second time, after checking that it still describes the key
# (whep#670). Neither shipped build used to hand the tail one to keep. Every
# reduction between the fold and the output selects a fixed column set: in the
# production chain `.combine_primary_raw()`'s grouped sum and the three
# `.build_*()` helpers beside it, in the CBS `.cbs_combine_sources()`, the wide
# `.test_cbs()` pivot and `.format_cbs_output()`'s grouped sum, and the wide
# CBS pivot after that. Measured on a real 2009-2011 build of `main`, the tail
# took the carried path 0 times and re-resolved 147,040 production rows, 309,700
# long CBS rows and 61,819 wide CBS rows from scratch.
#
# The identity is PARKED around those reductions rather than threaded through
# them, the way `.production_flag_lookup()` parks `fao_flag` and
# `.extract_source_lookup()` parks `source`. Threading it means adding it to
# each reduction's `by =`, which is the shape of whep#563: on one key, a row
# whose identity is NA (a branch the fold never saw, such as fodder or LUH2
# grassland) and a row whose identity is not are then never summed, and no
# value moves to say so. Parking cannot do that. The identity is a function of
# `(area_code, year)` -- `.add_polity_columns_dt()` keeps one match per row, the
# latest-starting period, so a key cannot resolve two ways -- and it is parked at
# that grain and written back with an update-join, which can neither add, drop
# nor reorder a row.
#
# Parking is not a choice of grain. Every output row is keyed on `year` and
# `area_code`, so writing the identity back by that pair labels the row with the
# polity of its own year -- the polity-period of whep#1192 -- and grouping an
# output on `(year, area_code)` or on `(year, area_code, reporting_polity_code)`
# gives the same groups. The steps where the two grains WOULD differ are the
# ones that run along the year axis (the QC series, the yield and fodder
# gap-fills), which group on `area_code` across a handover. They are untouched
# here, so they still do.

# The identity `frames` carry, one row per `(area_code, year)`.
#
# A frame that lacks any of the identity columns contributes nothing, and
# neither does a row whose `reporting_polity_code` is NA: the fold leaves NA
# where the bucket's own code resolves to no polity that year, and resolving
# that key afresh is what the tail would publish there. Two frames that give one
# key two different answers cannot both be right, so the key is dropped (it is
# resolved afresh too) and the disagreement is reported rather than settled by
# whichever frame came first.
.park_polity_identity <- function(frames) {
  cols <- .polity_identity_park_cols()
  frames <- purrr::keep(
    frames,
    \(x) is.data.frame(x) && all(cols %in% names(x))
  )
  parked <- frames |>
    purrr::map(\(x) .polity_identity_of(x, cols)) |>
    data.table::rbindlist(use.names = TRUE)
  if (nrow(parked) == 0L) {
    return(parked)
  }
  parked <- unique(parked)
  .drop_conflicting_identity(parked)
}

# Write the parked identity back onto the rows of `df` that lack one, keyed on
# `(area_code, year)`.
#
# Keys the park does not hold -- rows that came from no fold, such as LUH2
# grassland for an area FAOSTAT does not report, or a back-cast year -- are
# resolved here, on their distinct keys, by the same helper the tail uses. That
# is what makes the carry complete, so the tail keeps it instead of resolving
# the whole frame because one key was missing.
#
# Only rows without an identity are written. A row that carries one keeps it,
# so the tail's own check still sees what was carried: overwriting it by key
# would hide a row re-keyed since. And "the columns exist" is not read as "the
# frame is carried", because a `bind_rows()` of a carrying frame and a
# non-carrying one leaves the columns present and NA on the second part.
#
# Nothing parked means no fold identity reached this point, and `df` is
# returned untouched for the tail to resolve, as before. The class of `df` is
# kept, so this can sit in a pipeline without changing what the next step
# receives.
.attach_polity_identity <- function(df, parked) {
  cols <- .reporting_polity_cols()
  keyed <- all(c("area_code", "year") %in% names(df))
  if (is.null(parked) || nrow(parked) == 0L || !keyed) {
    return(df)
  }
  dt <- data.table::as.data.table(df)
  # A partial set is a stale one: an identity is kept only when all of it came.
  partial <- intersect(cols, names(dt))
  if (length(partial) > 0L && length(partial) < length(cols)) {
    dt[, (partial) := NULL]
  }
  holes <- if (all(cols %in% names(dt))) {
    is.na(dt$reporting_polity_code)
  } else {
    rep(TRUE, nrow(dt))
  }
  if (!any(holes)) {
    return(df)
  }
  keys <- .polity_identity_keys(dt$area_code[holes], dt$year[holes])
  identity <- data.table::rbindlist(
    list(
      parked[keys, on = c("area_code", "year"), nomatch = NULL],
      .resolve_unparked_identity(keys, parked)
    ),
    use.names = TRUE
  )
  identity[, .polity_hole := TRUE]
  dt[, .polity_hole := holes]
  # The keys are integer here and may be double in `dt` (`year` is, in both
  # builds' outputs); data.table matches the two without coercing `dt`'s own.
  dt[
    identity,
    on = c("area_code", "year", ".polity_hole"),
    (cols) := mget(paste0("i.", cols))
  ]
  dt[, .polity_hole := NULL]
  .restore_frame_class(dt, df)
}

.polity_identity_park_cols <- function() {
  c("area_code", "year", .reporting_polity_cols())
}

# The distinct identity rows of one frame, on integer keys so frames that store
# `year` as double and as integer park into one table.
.polity_identity_of <- function(x, cols) {
  # Only the six columns are copied, not the frame: production is 6.3 million
  # rows over the full span.
  out <- unique(data.table::as.data.table(as.list(x)[cols]))
  out <- out[!is.na(reporting_polity_code)]
  out[, `:=`(
    area_code = as.integer(area_code),
    year = as.integer(year),
    polity_area_code = as.integer(polity_area_code)
  )]
  out
}

.drop_conflicting_identity <- function(parked) {
  conflicted <- unique(
    parked[duplicated(parked, by = c("area_code", "year")), .(area_code, year)]
  )
  if (nrow(conflicted) == 0L) {
    return(parked)
  }
  examples <- utils::head(
    paste0("area ", conflicted$area_code, ", ", conflicted$year),
    3L
  )
  n_keys <- nrow(conflicted)
  cli::cli_warn(
    c(
      "{n_keys} {cli::qty(n_keys)}key{?s} carr{?ies/y} two different reporting
       polities in the frames parked for the tail.",
      "i" = "{cli::qty(n_keys)}Resolving {?it/them} afresh, which is what the
             tail has always published.",
      "i" = "First: {.val {examples}}."
    ),
    class = "whep_warn_polity_identity_conflict"
  )
  parked[!conflicted, on = c("area_code", "year")]
}

.polity_identity_keys <- function(area_code, year) {
  unique(data.table::data.table(
    area_code = as.integer(area_code),
    year = as.integer(year)
  ))
}

# The identity of the keys no parked frame carried, from the published resolver.
.resolve_unparked_identity <- function(keys, parked) {
  unparked <- keys[!parked, on = c("area_code", "year")]
  cols <- .polity_identity_park_cols()
  if (nrow(unparked) == 0L) {
    return(parked[0L, cols, with = FALSE])
  }
  resolved <- data.table::as.data.table(
    .add_reporting_polity_columns(unparked)
  )
  resolved[, cols, with = FALSE]
}

.restore_frame_class <- function(dt, like) {
  if (data.table::is.data.table(like)) {
    return(dt)
  }
  out <- if (tibble::is_tibble(like)) {
    tibble::as_tibble(dt)
  } else {
    as.data.frame(dt)
  }
  # See `.order_reporting_polity_cols()`: the over-allocation pointer survives
  # the conversion and makes an unchanged frame compare unequal.
  attr(out, ".internal.selfref") <- NULL
  out
}
