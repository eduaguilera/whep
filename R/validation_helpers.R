# Internal helpers backing the inst/scripts validation figures. Kept as
# pure, stateless functions so the conservation-scoring and join logic can
# be unit-tested independently of the plotting scripts (which need large
# gridded parquet inputs to run).

# Relative error (%) of a gridded total against a reference total, with
# correct handling of the zero-reference case. A country the reference
# says has no animals/area but that carries spurious gridded mass is a
# genuine conservation failure, not a perfect match, so it scores `Inf`.
# Only a true zero-against-zero pair scores `0`.
.conservation_rel_error <- function(gridded, reference) {
  dplyr::case_when(
    reference > 0 ~ abs(gridded - reference) / reference * 100,
    gridded > 0 ~ Inf,
    .default = 0
  )
}

# Join gridded totals to their country reference keeping *every* country
# from both sides (`full_join`), so a reference country with no gridded
# output -- a total mass leak, the most severe conservation failure -- is
# retained and flagged instead of silently dropped by an inner join.
# Numeric columns named in `fill` have their post-join `NA`s (the absent
# side) set to `0` so downstream error scoring treats them as leaks.
.join_conservation <- function(gridded, reference, by, fill = NULL) {
  joined <- dplyr::full_join(gridded, reference, by = by)
  if (!is.null(fill)) {
    joined <- joined |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(fill), \(x) tidyr::replace_na(x, 0))
      )
  }
  joined
}

# Bridge FABIO's published region space onto WHEP's `polity_area_code`
# buckets, so `inst/scripts/compare_fabio.R` compares like with like.
#
# FABIO's `regions.csv`/`io_codes.csv` `code` column IS a FAOSTAT-style area
# code -- code 1 is Armenia, 999 is Rest of World -- not the sequential
# 1..192 matrix row index the block layout suggests (whep#264). So the two
# spaces look directly joinable, and 185 of the 192 codes are. What a raw
# join gets wrong is the minority where the two *bucketings* disagree, and it
# gets it wrong silently: an `inner_join()` drops a non-matching code without
# a word while still printing "Matched N sectors".
#
# - FABIO enumerates 276 Sudan and 277 South Sudan; WHEP folds both into
#   bucket 206 Sudan (former) (see `folded_reporting_areas()`). All three
#   codes drop out, taking real tonnage with them.
# - WHEP publishes every reporting Rest-of-World member under its own code
#   (whep#459), while FABIO keeps them inside its single 999 row. Those WHEP
#   codes drop out, and the two 999 rows -- which no longer cover the same
#   territories -- match each other and are compared as if they did.
# - FABIO carries composite historical regions (BLX, CSK, SCG, SUN, YUG)
#   under codes of their own, which a modern WHEP year has no counterpart for.
#
# The bridge keys both sides in WHEP bucket space by ISO3 identity and pools
# whatever FABIO does not enumerate into its Rest-of-World code. That is the
# only assignment leaving BOTH sides' totals intact: every matrix row keeps a
# key, so nothing leaves the comparison without being named.
#
# `fabio_regions` is FABIO's `regions.csv` (columns `code`, `iso3c`);
# `whep_area_codes` are the `area_code` values the WHEP io-model labels carry;
# `crosswalk` defaults to the run's polity crosswalk.
#
# Returns one row per (side, area_code):
# - `side`: `"fabio"` or `"whep"`.
# - `area_code`: that side's own code.
# - `compare_area_code`: the shared key to group and join on.
# - `bridge_kind`: `"direct"` (same code both sides), `"fabio_fold"` (several
#   FABIO regions into one WHEP bucket), `"pooled_into_row"` (a WHEP code
#   FABIO keeps inside Rest of World), `"whep_fold"` (a WHEP reporting area
#   summed into another bucket) or `"unmatched"` (a FABIO region with no
#   WHEP counterpart at all -- the only kind that still leaves the join, and
#   now named instead of vanishing). Every WHEP area always keeps a key.
.fabio_area_bridge <- function(
  fabio_regions,
  whep_area_codes,
  crosswalk = NULL
) {
  required <- c("code", "iso3c")
  missing <- required[!rlang::has_name(fabio_regions, required)]
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg fabio_regions} is missing {.field {missing}}.")
  }
  areas <- sort(unique(as.integer(whep_area_codes)))
  if (length(areas) == 0L) {
    cli::cli_abort("{.arg whep_area_codes} must name at least one area.")
  }

  row_code <- .fabio_row_code(fabio_regions)
  cw <- crosswalk %||% .polity_crosswalk()
  keyed <- .fabio_bridge_fabio_side(fabio_regions, cw)

  # Both sides resolve through the SAME crosswalk, or the two halves of a fold
  # part company: a WHEP frame can still carry reporting area 276 alongside the
  # bucket 206 it is summed into, and keying 276 on itself would pool Sudan
  # into Rest of World while FABIO's own 276 went to 206.
  bucket <- .fabio_bridge_whep_buckets(cw, areas)
  pooled <- !bucket %in% keyed$compare_area_code
  matched <- c(bucket, if (any(pooled)) row_code)

  whep_side <- tibble::tibble(
    side = "whep",
    area_code = areas,
    compare_area_code = dplyr::if_else(pooled, row_code, bucket),
    bridge_kind = dplyr::case_when(
      pooled ~ "pooled_into_row",
      bucket != areas ~ "whep_fold",
      .default = "direct"
    )
  )

  keyed |>
    dplyr::mutate(
      bridge_kind = dplyr::case_when(
        !.data$compare_area_code %in% matched ~ "unmatched",
        .data$compare_area_code != .data$area_code ~ "fabio_fold",
        .default = "direct"
      )
    ) |>
    dplyr::bind_rows(whep_side) |>
    dplyr::arrange(.data$side, .data$area_code)
}

# FABIO's own Rest-of-World code, the pool every territory its region list
# does not enumerate lands in. Identified by its published `iso3c` `"ROW"`
# rather than by hardcoding 999, and required: without it there is nowhere to
# put a WHEP area FABIO does not name, and pooling would invent a code.
.fabio_row_code <- function(fabio_regions) {
  row_code <- unique(as.integer(fabio_regions$code[
    !is.na(fabio_regions$iso3c) & fabio_regions$iso3c == "ROW"
  ]))
  if (length(row_code) != 1L) {
    cli::cli_abort(c(
      "{.arg fabio_regions} must hold exactly one {.val ROW} region.",
      "x" = "Found {length(row_code)}."
    ))
  }
  row_code
}

# The FABIO half of `.fabio_area_bridge()`: resolve each FABIO region to the
# WHEP bucket that carries its ISO3, falling back to its own code when WHEP
# has no area for that ISO3 at all.
.fabio_bridge_fabio_side <- function(fabio_regions, crosswalk) {
  iso_bucket <- tibble::as_tibble(crosswalk) |>
    dplyr::filter(!is.na(.data$area_iso3c), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      iso3c = .data$area_iso3c,
      whep_bucket = as.integer(.data$polity_area_code)
    )

  tibble::as_tibble(fabio_regions) |>
    dplyr::distinct(area_code = as.integer(.data$code), iso3c = .data$iso3c) |>
    dplyr::left_join(iso_bucket, by = "iso3c", relationship = "many-to-one") |>
    dplyr::transmute(
      side = "fabio",
      area_code = .data$area_code,
      compare_area_code = dplyr::coalesce(
        .data$whep_bucket,
        .data$area_code
      )
    )
}

# The bucket each WHEP area's rows are summed into, for `.fabio_area_bridge()`.
# A frame can carry a reporting area and the bucket that folds it side by side
# (206 Sudan (former) beside 276 Sudan), so an area's OWN code is not always
# the key its values belong under. An area the crosswalk does not know keeps
# its own code, which then either matches a FABIO region or gets pooled.
.fabio_bridge_whep_buckets <- function(crosswalk, areas) {
  lookup <- tibble::as_tibble(crosswalk) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      area_code = as.integer(.data$area_code),
      bucket = as.integer(.data$polity_area_code)
    )
  clashes <- lookup |>
    dplyr::count(.data$area_code) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(clashes) > 0L) {
    bad <- clashes$area_code
    cli::cli_abort(
      "{.arg crosswalk} folds area{?s} {.val {bad}} into more than one bucket."
    )
  }
  dplyr::coalesce(lookup$bucket[match(areas, lookup$area_code)], areas)
}
