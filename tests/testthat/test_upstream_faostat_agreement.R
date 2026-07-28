# whep-polities runs its own FAOSTAT matching pipeline
# (pipelines/faostat-era-matching), whose state file records, per FAOSTAT
# reporting area and year range, the polity that area's data belongs to. This
# package builds the SAME mapping independently, by joining area codes to polity
# families on an ISO3-shaped prefix.
#
# Two authorities for one question. These tests compare them.
#
# The good news, and the invariant worth keeping: there are ZERO true conflicts.
# No area has both repos claiming a polity for overlapping years and disagreeing
# about which. Where they differ, this package is less COMPLETE, never
# contradictory — and that gap is baselined below rather than asserted away.
#
# Skips when the upstream checkout is absent, like test_polities_upstream_
# contract.R: an unavailable second opinion is not evidence of agreement.

aliases_path <- function() {
  Sys.getenv(
    "WHEP_POLITIES_FAOSTAT_ALIASES",
    unset = path.expand(
      "~/whep-polities/pipelines/faostat-era-matching/state/faostat_aliases.csv"
    )
  )
}

read_aliases <- function() {
  path <- aliases_path()
  testthat::skip_if_not(
    file.exists(path),
    paste0("upstream FAOSTAT alias state not found at ", path)
  )
  a <- utils::read.csv(path, stringsAsFactors = FALSE)
  a <- a[a$match_status == "matched" & !is.na(suppressWarnings(as.integer(a$area_code))), ]
  a$area_code <- as.integer(a$area_code)
  a
}

# Areas where upstream names a specific polity but this package folds the area
# into an aggregate (ROW-1850-2023 and the continental "Other" buckets). These
# are small territories AND several substantial ones — Syria, Swaziland, North
# Macedonia, New Caledonia — whose data lands in Rest of World here while a real
# polity for them exists upstream. Tracked, not fixed: moving an area out of ROW
# changes published aggregates, so it is a deliberate decision, not a cleanup.
FOLDED_INTO_AGGREGATE <- c(
  5L, 6L, 17L, 42L, 47L, 61L, 64L, 65L, 69L, 85L, 87L, 88L, 125L, 135L, 140L,
  142L, 153L, 154L, 160L, 161L, 180L, 182L, 187L, 190L, 192L, 205L, 209L, 212L,
  239L, 240L, 299L
)

# Areas whose COLONIAL-ERA polity this package cannot reach, because the
# crosswalk maps an area to exactly one polity prefix and upstream gives the
# colonial and modern polities DIFFERENT prefixes: Angola is ANG-1905-1975 then
# AGO-1975-2025, Zimbabwe SRH-1953-1964 then ZWE-*, Botswana BEC then BWA,
# Zambia NRH then ZMB, Sudan SUD then SDN, Viet Nam F237 then VNM, Yemen F249
# then YEM. A single prefix cannot reach both, so pre-independence FAOSTAT years
# resolve to the modern polity instead. See whep#387.
DIFFERENT_ERA <- c(7L, 20L, 181L, 206L, 237L, 249L, 251L)

classify <- function() {
  a <- read_aliases()
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_code), ]
  cw$area_code <- as.integer(cw$area_code)

  out <- lapply(seq_len(nrow(a)), function(k) {
    r <- a[k, ]
    cands <- cw[cw$area_code == r$area_code, ]
    if (nrow(cands) == 0) return(data.frame(area_code = r$area_code, kind = "absent"))
    if (r$target_polity_code %in% cands$polity_code) {
      return(data.frame(area_code = r$area_code, kind = "agree"))
    }
    if (all(!is.na(cands$polity_type) & cands$polity_type == "aggregate")) {
      return(data.frame(area_code = r$area_code, kind = "folded"))
    }
    overlapping <- cands[
      !is.na(cands$polity_start_year) & !is.na(cands$polity_end_year) &
        cands$polity_end_year > r$year_start & cands$polity_start_year < r$year_end,
    ]
    data.frame(
      area_code = r$area_code,
      kind = if (nrow(overlapping) == 0) "different_era" else "conflict"
    )
  })
  do.call(rbind, out)
}

test_that("no area conflicts with upstream on overlapping years", {
  cls <- classify()
  conflicts <- sort(unique(cls$area_code[cls$kind == "conflict"]))

  # The strong invariant. A conflict means both repos claim a polity for the same
  # area over the same years and name DIFFERENT ones — a genuine contradiction
  # between the source of truth and its consumer, not merely a gap.
  expect_equal(
    length(conflicts), 0L,
    info = paste0(
      "areas where this package and whep-polities name different polities for ",
      "overlapping years: ", paste(conflicts, collapse = ", ")
    )
  )
})

test_that("every upstream-matched area is present in the crosswalk", {
  cls <- classify()
  absent <- sort(unique(cls$area_code[cls$kind == "absent"]))
  expect_equal(
    length(absent), 0L,
    info = paste0(
      "areas upstream matched to a polity that this crosswalk does not carry ",
      "at all: ", paste(absent, collapse = ", ")
    )
  )
})

test_that("the known completeness gaps do not grow", {
  cls <- classify()

  for (kind in c("folded", "different_era")) {
    baseline <- if (kind == "folded") FOLDED_INTO_AGGREGATE else DIFFERENT_ERA
    observed <- sort(unique(cls$area_code[cls$kind == kind]))

    # New entries fail: a gap appearing is a regression, whether from an upstream
    # split or a change here.
    expect_equal(
      length(setdiff(observed, baseline)), 0L,
      info = paste0(
        "NEW ", kind, " areas not in the baseline: ",
        paste(setdiff(observed, baseline), collapse = ", ")
      )
    )
    # Entries that have been resolved must be removed from the baseline, so it
    # shrinks as the gap closes and cannot quietly license a regression later.
    expect_equal(
      length(setdiff(baseline, observed)), 0L,
      info = paste0(
        "baseline lists ", kind, " areas that now resolve correctly — remove ",
        "them: ", paste(setdiff(baseline, observed), collapse = ", ")
      )
    )
  }
})
