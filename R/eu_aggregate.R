# The EU aggregate that `inst/scripts/compare_fabio_footprints.R` reports on
# used to be a 28-element ISO3 literal in the script. It is not one list, it is
# two independent choices, and only one of them is this project's to make.
#
# `regions_full$EU27` marks 29 ISO3 codes. Twenty-seven are the member states
# that still exist. The other two are dissolved: `BLX` (Belgium-Luxembourg,
# polities to 1999) and `CSK` (Czechoslovakia, polities to 1993). That is
# coherent rather than stale -- the flag means "territory now inside the EU27",
# which is what a long time series needs -- and it is the part the published
# data can supply, so it is derived here instead of retyped.
#
# Whether an aggregate wants those predecessors is not cosmetic, and both
# models the FABIO comparison lines up agree on that. FABIO books Belgium and
# Luxembourg under `BLX` and Czechia and Slovakia under `CSK` for the years
# before those successions, and so does WHEP's own CBS: in 1986 codes 15 and 51
# carry 118 and 106 rows while 255, 256, 167 and 199 carry exactly zero, and in
# 2000 and 2013 it is the other way round. So an aggregate built from member
# states alone reads all four of them as zero in the 1986 benchmark year on
# BOTH sides and normally in 2000 and 2013. That is a year-dependent
# membership, not a method, which is why the territory bases are the defaults.
#
# `GBR` is the one code no published table here supplies. `regions_full` carries
# an EU27 flag and no EU28 flag, so "the member states before the United Kingdom
# left" is a membership fact stated by hand, on purpose, in one place
# (`.eu_withdrawn_member_iso3()`), and selected through `basis` rather than
# assumed. Whether the FABIO comparison should report EU28 or EU27 is an open
# question for the project, not a defect to be fixed here (whep#421).
#
# Extancy is read from `whep::polities` rather than listed: an ISO3 is a current
# state when the database still has it open at the last year the database
# covers. `polity_end_year` is exclusive at a succession and inclusive at an
# open end (#577), so the last year a code carries is the right test and
# `BLX` (1999) and `CSK` (1993) fall out of it on their own.

.eu_aggregate_iso3 <- function(
  basis = c("eu28_territory", "eu27_territory", "eu28_states", "eu27_states")
) {
  basis <- rlang::arg_match(basis)
  flagged <- .eu27_flagged_iso3()
  codes <- if (stringr::str_ends(basis, "territory")) {
    flagged
  } else {
    .extant_iso3(flagged)
  }
  if (stringr::str_starts(basis, "eu28")) {
    codes <- union(codes, .eu_withdrawn_member_iso3())
  }
  sort(codes)
}

# The ISO3 codes the published EU27 flag marks, predecessors included.
.eu27_flagged_iso3 <- function() {
  flagged <- whep::regions_full |>
    dplyr::filter(.data$EU27 %in% TRUE, !is.na(.data$iso3c)) |>
    dplyr::pull(.data$iso3c) |>
    unique()
  if (length(flagged) == 0L) {
    cli::cli_abort(
      "{.field regions_full$EU27} marks no ISO3 code; the EU aggregate
       cannot be derived."
    )
  }
  flagged
}

# Members of `iso3c` that the polities database still has open at its last year.
.extant_iso3 <- function(iso3c) {
  last_open <- .iso3_last_polity_year()
  unknown <- setdiff(iso3c, last_open$iso3_code)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.val {unknown}} {?is/are} not {?an ISO3 code/ISO3 codes} the polities
       database knows."
    )
  }
  open_at <- max(last_open$last_year)
  iso3c[iso3c %in% last_open$iso3_code[last_open$last_year >= open_at]]
}

# `whep::polities` is an sf object, so its geometry column has to be dropped
# before dplyr will group it.
.iso3_last_polity_year <- function() {
  tibble::tibble(
    iso3_code = whep::polities$iso3_code,
    end_year = whep::polities$end_year
  ) |>
    dplyr::filter(!is.na(.data$iso3_code), !is.na(.data$end_year)) |>
    dplyr::summarise(
      last_year = max(.data$end_year),
      .by = "iso3_code"
    )
}

# The single membership fact no published table in this package states: the
# United Kingdom was the 28th member state and withdrew on 31 January 2020.
.eu_withdrawn_member_iso3 <- function() {
  "GBR"
}
