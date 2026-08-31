# The gate whep#669 asks for: a join keyed on a territory but not on a year has
# to be classified, and the list of them can only shrink.
#
# Measured on the commit this arrived at: 163 joins in the installed namespace
# key on a territory column, 106 of them carry `year` and 57 do not. Those 57
# are mostly not wrong numbers -- a single-year scope, a table with no time
# dimension, an identity lookup or a diagnostic -- but before this file nothing
# said so, and the difference between a decision and an oversight was
# invisible. One of them (`.polity_code_from_labels`) was a real defect, and
# saying so is what the classification is for -- whep#698 then fixed it, which
# is the first time the ratchet moved DOWN: 58 back to 57.
#
# The audit reads the NAMESPACE, not `R/`, because `R/` is not shipped to where
# tests run under `R CMD check` while the parsed bodies always are.

test_that("every year-free territorial join is classified", {
  audit <- whep:::.territorial_joins()
  found <- audit |>
    dplyr::filter(!.data$has_year) |>
    dplyr::count(.data$owner, .data$join_fn, .data$key, name = "n")
  baseline <- whep:::.territorial_join_baseline() |>
    dplyr::select("owner", "join_fn", "key", "n")

  # Compared as labels rather than as counts, so a failure names the join that
  # has to be classified instead of only saying how many there are. A new one
  # here means: give the key a `year`, or add a row to
  # `.territorial_join_baseline()` saying why the join means the same thing in
  # 1850 and in 2023.
  unclassified <- dplyr::anti_join(
    found,
    baseline,
    by = c("owner", "join_fn", "key")
  )
  expect_equal(paste(unclassified$owner, unclassified$key), character(0))

  # The other direction is what makes the list shrink: a classified join that
  # gained a year, or disappeared, has to leave the baseline with it.
  stale <- dplyr::anti_join(
    baseline,
    found,
    by = c("owner", "join_fn", "key")
  )
  expect_equal(paste(stale$owner, stale$key), character(0))

  counts <- dplyr::inner_join(
    found,
    baseline,
    by = c("owner", "join_fn", "key"),
    suffix = c("_found", "_baseline")
  )
  expect_equal(counts$n_found, counts$n_baseline)
})

test_that("the enumerated baseline can only shrink", {
  baseline <- whep:::.territorial_join_baseline()
  # Lower this number when a join is fixed. It rises only for a join that had
  # to be added to make something MORE year-aware, and only with the reason
  # written into the row -- never to admit a new year-blind read. 57 was the
  # count on the commit that introduced the gate; 58 added
  # `.resolve_all_area_years`, the first-reported-year bound that turned
  # `polity_bucket_coverage()` from a 65-year report into a 14-year one;
  # `.handed_over_polity_codes` added the succession self-join that stops
  # `.polity_join_end_year()` widening a period into the first year of the
  # successor upstream recorded only in the inverse direction (whep#683);
  # whep#698 then took the ratchet DOWN for the first time by keying the
  # pre-1962 proxy fill on the reporting bucket, deleting
  # `.polity_code_from_labels()`.
  #
  # 59 is where whep#709 lands on top of those. It is a rise in the COUNT and a
  # fall in what matters: the pre-1962 extension stopped keying on the `area`
  # label in four places and `.attach_cbs_area_label` attaches it once instead,
  # so the count gains that one lookup while the label-keyed surface this file's
  # third test guards drops to a single redundant join. Read the two together --
  # this number alone would have called that change a regression.
  #
  # 58 is the second fall: `.nbx_image_region` left too, because the deposited
  # Schulte-Uebbing crosswalk carries IMAGE membership on the canonical cell
  # key, so the gridded boundary no longer reaches IMAGE through a year-free
  # country join at all.
  #
  # 59 is `.attach_mapping_source`, measured -- `sum(.territorial_joins() |>
  # filter(!has_year) |> count(...) |> pull(n))` reads 59 on this commit against
  # 58 before it. It is a rise in the count for a join that keys on the polity
  # PERIOD, i.e. on the year-scoped identity itself, and that exists only to
  # report which authority each resolution rests on (whep#740). The instrument
  # that measures year-blindness costs one row on the ledger it measures.
  #
  # 60 and 61 arrived independently -- the carbon fold and whep#761 each added
  # one year-free row and each recorded itself as "60". Both are kept because
  # both are real; the cap is measured on the merge, not assumed from a sum.
  #
  # `.carbon_warn_fold` is not a new year-blind READ at all -- both sides of it
  # are the same already-year-filtered carbon support, and it selects warning
  # text rather than a value.
  #
  # `.land_in_polygons` is the clearest case yet of the rise that is a fall.
  # The pre-1962 back-cast estimates production as `ha * t_ha`; the yield half
  # was already historical and the AREA half was measured on present-day
  # borders (whep#761). Making it historical means resolving
  # (area_code, year) -> polity_code unfloored and then reading that polity's
  # polygon -- and the second step keys on `polity_code` alone, because a
  # polity code already carries its own period. One year-free row bought a land
  # series that varies with the map. Everything downstream of it carries `year`.
  # 62: whep#423 adds exactly ONE year-free join, `.plu_bind_pasture_backcast`,
  # and it buys the same thing `.land_in_polygons` did. FAOSTAT land use starts
  # in 1961, so anchoring grassland on it alone would step the gridded series at
  # that year; carrying the FAO 1961 level backwards on LUH2's own national
  # trend removes the step. The join that does it keys on `area_code` because
  # its anchor side is already filtered to 1961 -- fixed, not missing.
  #
  # It did not cost four. The producer first grew four year-free joins, three of
  # which were duplicating machinery the cropland back-cast already had: the
  # ISO3 bridge is now shared (`.luh2_bridge_iso3c`, extracted from
  # `.read_luh2_cft`, so that row moved rather than multiplied), the LUH2 anchor
  # became a grouped lookup instead of a second merge, and the toy fixture's
  # polity tail is built in place instead of joined.
  #
  # 65 is the loss wedge's three on top of that (whep#500, whep#753), and they
  # are another rise that is not a regression: Gustavsson's Annex 1 is a single
  # 2011 snapshot with no time dimension at all, so the region a country's loss
  # rates come from CANNOT be year-keyed. Keying it on the year would be the
  # defect, not the fix -- it would leave every successor area (Ethiopia,
  # Sudan) without the pre-partition region Annex 1 actually assigns it. The
  # count is three and not four because `.lw_weight` reads the assignment once
  # and carries `method_region` through its grouping instead of joining twice.
  #
  # Both numbers are measured on the merge, never summed from the two sides:
  # `sum(.territorial_joins() |> filter(!has_year) |> count(owner, join_fn,
  # key) |> pull(n))` reads 65 here, and the enumerated baseline reads 65 with
  # it (61 rows).
  #
  # whep#691 leaves the number at 65 and is still the ratchet moving: the
  # destiny-share skeleton join did not go away, it stopped naming the `area`
  # label, so one row changed key rather than leaving. The count says nothing
  # about that; the third test does, and it now reads EMPTY.
  #
  # 66 is the dependency-sovereign fallback, and it is a rise that BUYS BACK a
  # territory rather than spending one. `.dependency_sovereign_iso3()` found a
  # crown dependency's sovereign by asking which reporting area SHARED its
  # `polity_code`. That relation exists only while the dependency has no polity
  # of its own, and the 2026-08-25 whep-polities re-sync gave Sint Maarten
  # `SXM-2010-2025` -- an upstream improvement -- so nothing shared its polity,
  # the join dropped it, and its LUH2 land went from counted under `NLD` to
  # counted nowhere. The second join reads `legacy_polity_prefix` instead, the
  # same ISO3-stem-to-bucket bridge `.read_fodder_euadb()` already uses, and it
  # names the sovereign whether or not the dependency has its own polity. It
  # cannot be year-keyed for the reason the class says: a present-day sovereign
  # has no time dimension, and the dependency's own period is chosen before this
  # join runs. It fires only where the first route found nothing, so it cannot
  # move an answer that route still gives -- the two agree on all five it does.
  #
  # 67 since whep#264: `.fabio_bridge_fabio_side()` resolves FABIO's published
  # region list to WHEP buckets by ISO3. It is the first `diagnostic` row that
  # buys a comparison rather than a value -- `inst/scripts/compare_fabio.R` was
  # joining the two region spaces raw, which dropped Sudan entirely and matched
  # two Rest-of-World residuals covering different territories against each
  # other. One year-free identity join makes that comparison honest, and no
  # published number passes through it.
  #
  # 69 since whep#884: `.off_window_area_keys()` and `.reported_bucket_years()`
  # each attach an area's own reporting window before testing a year against
  # it. Both are the instrument that catches a year-blind AREA CODE -- FishStat
  # keys Belgium 255 from 1976, sixteen years before the vocabulary reports that
  # code -- and neither can be year-keyed, because the window is what the year
  # is being compared with. Both rises are the shape `.resolve_all_area_years`
  # already records above.
  expect_lte(sum(baseline$n), 69L)
  expect_true(all(nzchar(baseline$why)))
  # `label_identity` and `label_redundant` are deliberately absent: they
  # classified one join each, the ones whep#698 and whep#691 removed. Putting
  # either back means arguing again that a label may be a key.
  expect_true(all(
    baseline$class %in%
      c(
        "single_year",
        "time_invariant",
        "identity_lookup",
        "diagnostic"
      )
  ))
})

test_that("every year-free territorial grouping is classified", {
  # whep#692, the other half of whep#669. Measured on the commit this arrived
  # at: 279 GROUPING keys in the installed namespace mention a territory
  # column, 208 of them carry `year` and 71 do not, in 67 distinct signatures.
  # Before this test all 71 were unclassified -- a `.by = c(area_code,
  # item_cbs_code)` that collapses 1961 into 2023 was indistinguishable from
  # one sitting inside a per-year scope.
  audit <- whep:::.territorial_groupings()
  found <- audit |>
    dplyr::filter(!.data$has_year) |>
    dplyr::count(.data$owner, .data$group_fn, .data$key, name = "n")
  baseline <- whep:::.territorial_grouping_baseline() |>
    dplyr::select("owner", "group_fn", "key", "n")

  # The ratchet, in the same shape as the join gate above: a new year-free
  # territorial group means either put `year` in the key, or add a row to
  # `.territorial_grouping_baseline()` saying which of the five reasons a
  # year-free group is allowed to have applies.
  unclassified <- dplyr::anti_join(
    found,
    baseline,
    by = c("owner", "group_fn", "key")
  )
  expect_equal(paste(unclassified$owner, unclassified$key), character(0))

  stale <- dplyr::anti_join(
    baseline,
    found,
    by = c("owner", "group_fn", "key")
  )
  expect_equal(paste(stale$owner, stale$key), character(0))

  counts <- dplyr::inner_join(
    found,
    baseline,
    by = c("owner", "group_fn", "key"),
    suffix = c("_found", "_baseline")
  )
  expect_equal(counts$n_found, counts$n_baseline)

  # 71 was the count on the commit that introduced this gate, measured the same
  # way the join cap is: `sum(.territorial_groupings() |> filter(!has_year) |>
  # count(owner, group_fn, key) |> pull(n))`. Lower it when a group gains a
  # year; raise it only with the reason written into the row.
  #
  # 72 since whep#758: `.pcs_abort_interval_overlap()` compares each polycell
  # interval with the previous one inside `(cell_id, polity_code)`, which is a
  # year-free territorial group by necessity -- the previous interval only
  # exists while the group holds the whole sequence.
  #
  # 73 since whep#787: `.reporting_periods()` reduces the crosswalk's rows for
  # one (area_code, polity_code) period to that period's reporting span, so
  # `year` is the axis being reduced over, not a missing key.
  #
  # 77 since whep#264: four grouping keys, two on each side of the FABIO-to-WHEP
  # region bridge, all `diagnostic` -- they build and guard a comparison key for
  # `inst/scripts/compare_fabio.R` and no published number passes through them.
  # Each side collapses polity PERIODS on purpose, because the question is which
  # bucket carries a territory at all; a year in either key would multiply one
  # FABIO region into one row per period and defeat the `many-to-one` join and
  # the one-bucket-per-area guard that follow. The four are the price of not
  # comparing two different Rest-of-World residuals against each other.
  full <- whep:::.territorial_grouping_baseline()
  #
  # 79 since whep#884: `.area_reporting_windows()` reduces the crosswalk's
  # periods to one window per area and `.off_window_area_years()` reduces an
  # area's off-window rows to the span they cover. Both are `year_axis` -- the
  # year is the thing being reduced over, so putting it in the key returns the
  # year itself, which is the same reason `.area_first_reported_year` is on
  # this ledger.
  expect_lte(sum(full$n), 81L)
  expect_true(all(nzchar(full$why)))
  expect_true(all(
    full$class %in%
      c(
        "single_year",
        "time_invariant",
        "identity_lookup",
        "diagnostic",
        # The two verdicts a group needs and a join does not.
        "year_axis",
        "row_wise"
      )
  ))
})

test_that("a year-free grouping label always comes with its code", {
  # The grouping analogue of the join label rule below, and deliberately
  # weaker: adding a label BESIDE the code it is functionally determined by
  # only splits groups further, so it cannot merge two territories. A label
  # WITHOUT a code can -- that is whep#589's shape, where a shared display name
  # diluted Syria's livestock 12-fold.
  #
  # This is a one-entry expectation rather than an empty set, which is weaker
  # than the join rule and says why in the row: `.zero_proxy_land_areas()`
  # groups on `area_key`, a column whose NAME is chosen by its caller, and
  # `.fill_pre_faostat()` still falls back to `"area"` when the LUH2 land table
  # carries no `area_code` (whep#584). A SECOND one fails here.
  offenders <- whep:::.territorial_groupings() |>
    dplyr::filter(!.data$has_year, .data$has_label, !.data$has_code) |>
    dplyr::pull(.data$owner) |>
    sort()
  expect_equal(offenders, ".zero_proxy_land_areas")
})

test_that("no year-free join keys on the area LABEL at all", {
  # Keying on `area` rather than `area_code` is the shape behind whep#589 (a
  # shared label diluted Syria's livestock by 12x) and whep#563. whep#698
  # removed the load-bearing one and whep#691 the last redundant one
  # (`.interpolate_destiny_shares`), so the set is now EMPTY and a new one
  # cannot be classified into existence -- it has to be keyed on the code.
  labelled <- whep:::.territorial_joins() |>
    dplyr::filter(!.data$has_year, .data$has_label) |>
    dplyr::pull(.data$owner) |>
    sort()
  expect_equal(labelled, character(0))
})

# The detector has to be able to fail. These run it over fixture functions
# rather than the package, so they say what it sees rather than what the
# package happens to contain.

test_that("the audit sees a year-free territorial join in any nesting", {
  ns <- rlang::new_environment()
  ns$plain <- function(x, y) {
    dplyr::left_join(x, y, by = c("area_code", "item_cbs_code"))
  }
  ns$nested <- function(x, y) {
    x |>
      dplyr::mutate(z = 1) |>
      (\(d) dplyr::inner_join(d, y, by = "area_code"))()
  }
  ns$data_table <- function(x, y) x[y, on = c("area", "area_code")]

  audit <- whep:::.territorial_joins(ns)

  expect_setequal(audit$owner, c("plain", "nested", "data_table"))
  expect_true(all(!audit$has_year))
  expect_equal(
    audit$has_label,
    c(TRUE, FALSE, FALSE),
    ignore_attr = TRUE
  )
})

test_that("the audit ignores joins that carry a year or no territory", {
  ns <- rlang::new_environment()
  ns$with_year <- function(x, y) {
    dplyr::left_join(x, y, by = c("area_code", "year"))
  }
  ns$no_territory <- function(x, y) {
    dplyr::left_join(x, y, by = c("item_cbs_code", "element"))
  }
  ns$not_a_join <- function(x) dplyr::filter(x, .data$area_code == 1L)

  audit <- whep:::.territorial_joins(ns)

  expect_equal(audit$owner, "with_year")
  expect_true(audit$has_year)
})

test_that("the grouping audit sees every shape of year-free group", {
  ns <- rlang::new_environment()
  ns$summarised <- function(x) {
    dplyr::summarise(x, v = sum(.data$v), .by = c(area_code, item_cbs_code))
  }
  ns$windowed <- function(x) {
    x |> dplyr::mutate(share = .data$v / sum(.data$v), .by = "area_code")
  }
  ns$deduped <- function(x) dplyr::distinct(x, area_code, .keep_all = TRUE)
  ns$data_table <- function(x) x[, .(v = sum(v)), by = .(area, area_code)]
  ns$grouped <- function(x) dplyr::group_by(x, .data$area_code, .add = TRUE)

  audit <- whep:::.territorial_groupings(ns)

  expect_setequal(
    audit$owner,
    c("summarised", "windowed", "deduped", "data_table", "grouped")
  )
  expect_true(all(!audit$has_year))
  expect_equal(
    audit$owner[audit$has_label],
    "data_table"
  )
  expect_true(all(audit$has_code))
})

test_that("the grouping audit ignores year-keyed and non-territorial groups", {
  ns <- rlang::new_environment()
  ns$with_year <- function(x) {
    dplyr::summarise(x, v = sum(.data$v), .by = c(area_code, year))
  }
  ns$no_territory <- function(x) {
    dplyr::summarise(x, v = sum(.data$v), .by = "item_cbs_code")
  }
  # A whep series helper: its `.by` is the series identity and the year is its
  # own argument, so it is not a grouping key this audit should see.
  ns$series <- function(x) {
    whep::fill_linear(x, v, time_col = year, .by = c("area_code"))
  }
  # A data.table JOIN, which the other detector owns.
  ns$joined <- function(x, y) x[y, on = c("area_code")]

  audit <- whep:::.territorial_groupings(ns)

  expect_equal(audit$owner, "with_year")
  expect_true(audit$has_year)
})

test_that("a renaming key is audited on both vocabularies", {
  ns <- rlang::new_environment()
  ns$renaming <- function(x, y) {
    dplyr::left_join(x, y, by = c("iso3" = "area_iso3c"))
  }
  ns$computed <- function(x, y, keys) dplyr::left_join(x, y, by = keys)

  audit <- whep:::.territorial_joins(ns)

  expect_equal(audit$owner, "renaming")
  expect_equal(audit$key, "iso3, area_iso3c")
})
