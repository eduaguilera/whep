# test_tobacco_leaf_use.R — tests for R/tobacco_leaf_use.R (whep#1390)

# Two country-years of the tobacco chain in the `.extract_fao()` layout,
# every link balancing on its own, as FAO reports it. The Netherlands-like
# area 150 grows no leaf: it imports 94, exports 28 and books 66 as leaf
# `other_uses`, 54 of which become 831 that is exported again. The
# Ukraine-like area 230 reports more product (55 + 1.5) than leaf use (30),
# so a 1:1 leaf content cannot hold there.
.tobacco_fixture <- function() {
  tibble::tribble(
    ~area_code, ~item_cbs_code, ~element,          ~value,
    150L,       826L,           "import",          94,
    150L,       826L,           "export",          28,
    150L,       826L,           "other_uses",      66,
    150L,       831L,           "production",      54,
    150L,       831L,           "import",          10,
    150L,       831L,           "export",          64,
    230L,       826L,           "production",      2,
    230L,       826L,           "import",          28,
    230L,       826L,           "other_uses",      30,
    230L,       828L,           "production",      55,
    230L,       828L,           "export",          26,
    230L,       828L,           "other_uses",      29,
    230L,       829L,           "production",      1.5,
    230L,       829L,           "other_uses",      1.5,
    203L,       836L,           "other_uses",      7
  ) |>
    dplyr::mutate(
      year = 2019L,
      area = as.character(area_code),
      item_cbs = "x",
      unit = "TRUE",
      fao_flag = "A"
    ) |>
    data.table::as.data.table()
}

.tobacco_booked <- function(cbs_new) {
  whep:::.get_fiber_tobacco(
    cbs_new,
    tibble::tribble(
      ~item_code_trade, ~item_cbs,
      826L,             "Tobacco",
      828L,             "Tobacco",
      829L,             "Tobacco",
      831L,             "Tobacco",
      836L,             "Rubber"
    ),
    tibble::tribble(
      ~item_cbs, ~item_cbs_code,
      "Tobacco", 2671L,
      "Rubber",  2672L
    )
  ) |>
    dplyr::filter(item_cbs == "Tobacco") |>
    tidyr::pivot_wider(
      id_cols = area_code,
      names_from = element,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::arrange(area_code)
}

# The stock change the aggregated Tobacco item needs to close, positive
# when stock is withdrawn. Product production is not booked (whep#1276).
.tobacco_gap <- function(wide) {
  wide$other_uses + wide$export - wide$production - wide$import
}

test_that("as_published leaves the leaf's other_uses double-counted", {
  cbs_new <- .tobacco_fixture()
  out <- whep:::.cbs_tobacco_leaf_use(cbs_new, "as_published")

  expect_equal(out, cbs_new)
  # The phantom withdrawal of whep#1390: the leaf made into product is a use
  # once as leaf and again as the product exported or consumed.
  expect_equal(.tobacco_gap(.tobacco_booked(out)), c(54, 56.5))
})

test_that("one_to_one nets product production off the leaf's other_uses", {
  out <- whep:::.cbs_tobacco_leaf_use(.tobacco_fixture(), "one_to_one")
  wide <- .tobacco_booked(out)

  expect_equal(wide$other_uses, c(12, 0 + 29 + 1.5))
  # Area 150 closes. Area 230's products outweigh its leaf use, the leaf use
  # is floored at zero, and the remainder is left visible as a gap rather
  # than booked as a negative use.
  expect_equal(.tobacco_gap(wide), c(0, 26.5))
  expect_true(all(out$value >= 0))
})

test_that("one_to_one touches only the leaf's other_uses", {
  cbs_new <- .tobacco_fixture()
  out <- whep:::.cbs_tobacco_leaf_use(cbs_new, "one_to_one")
  is_leaf_use <- out$item_cbs_code == 826L & out$element == "other_uses"

  key <- c("area_code", "item_cbs_code", "element")
  untouched <- dplyr::inner_join(
    cbs_new[!(item_cbs_code == 826L & element == "other_uses")],
    out[!is_leaf_use],
    by = key
  )
  expect_equal(nrow(untouched), nrow(cbs_new) - 2L)
  expect_equal(untouched$value.x, untouched$value.y)
  # A netted value is WHEP's, not FAOSTAT's, so it carries no FAOSTAT flag.
  expect_true(all(is.na(out$fao_flag[is_leaf_use])))
  # The input is not modified in place.
  expect_equal(cbs_new$value[3], 66)
})

test_that("one_to_one leaves an area with no manufacture alone", {
  cbs_new <- .tobacco_fixture()[
    !(item_cbs_code %in% c(828L, 829L, 831L) & element == "production")
  ]
  out <- whep:::.cbs_tobacco_leaf_use(cbs_new, "one_to_one")

  expect_equal(
    out[order(area_code, item_cbs_code, element)],
    cbs_new[order(area_code, item_cbs_code, element)]
  )
})

test_that("as_published is the default", {
  expect_equal(whep:::.tobacco_leaf_use_choices()[[1]], "as_published")
})

test_that("build_commodity_balances validates tobacco_leaf_use", {
  expect_error(
    build_commodity_balances(example = TRUE, tobacco_leaf_use = "half"),
    class = "rlang_error"
  )
  expect_warning(
    build_commodity_balances(
      .fixed_data = tibble::tibble(
        year = c(2010L, 2011L),
        area = "Spain",
        area_code = 203L,
        item_cbs = "Wheat and products",
        item_cbs_code = 2511L,
        element = "import",
        value = c(1, 2),
        source = "FAOSTAT_trade"
      ),
      tobacco_leaf_use = "one_to_one"
    ),
    "tobacco_leaf_use"
  )
})
