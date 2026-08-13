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
  # 60 and 61 arrived independently -- this branch and whep#761 each added one
  # year-free row and each recorded itself as "60". Both are kept because both
  # are real; the cap is 61, measured on the merge, not 59 + 1 + 1 assumed.
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
  expect_lte(sum(baseline$n), 61L)
  expect_true(all(nzchar(baseline$why)))
  # `label_identity` is deliberately absent: it classified exactly one join,
  # the one whep#698 removed. Putting it back means arguing again that a label
  # may be an identity.
  expect_true(all(
    baseline$class %in%
      c(
        "single_year",
        "time_invariant",
        "identity_lookup",
        "diagnostic",
        "label_redundant"
      )
  ))
})

test_that("no year-free join keys on the area LABEL beyond those registered", {
  # Keying on `area` rather than `area_code` is the shape behind whep#589 (a
  # shared label diluted Syria's livestock by 12x) and whep#563. ONE survives,
  # redundant (whep#691): whep#698 removed the load-bearing one. A second must
  # not appear unnoticed.
  labelled <- whep:::.territorial_joins() |>
    dplyr::filter(!.data$has_year, .data$has_label) |>
    dplyr::pull(.data$owner) |>
    sort()
  registered <- whep:::.territorial_join_baseline() |>
    dplyr::filter(.data$class == "label_redundant") |>
    dplyr::pull(.data$owner) |>
    sort()
  expect_equal(labelled, registered)
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
