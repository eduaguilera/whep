# whep#1215: the CBS residue items are fresh matter of a crop mix, and
# "Other crop residues" (2106) used to be converted to feed dry matter with one
# item coefficient of 0.90 whatever the crops behind it.

.residue_dm_fixture <- function() {
  # Spain 2010: fresh tomato haulm (DM 0.132) and fresh pea straw (DM 0.90),
  # both booked to 2106, plus wheat straw (DM 0.866) booked to 2105.
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_crop, ~item_cbs_code_residue, ~value,
    ~value_dm,
    2010L, 203L,       2601L,               2106L,                  1000,
    132,
    2010L, 203L,       2547L,               2106L,                  1000,
    900,
    2010L, 203L,       2511L,               2105L,                  500,
    433
  )
}

testthat::test_that(".residue_feed_kgdm is the dry matter of the feed itself", {
  res <- .residue_dm_fixture()
  out <- whep:::.residue_feed_kgdm(res)

  pointblank::expect_col_exists(
    out,
    c("year", "area_code", "item_cbs_code", "residue_kgdm_kgfm")
  )
  testthat::expect_equal(
    anyDuplicated(out[c("year", "area_code", "item_cbs_code")]),
    0L
  )

  # The identity it must satisfy: feed DM / feed FM, with each crop's feed
  # share taken from the same split the CBS `feed` element is built from.
  split <- whep:::.residue_recovered_split(res, warn = FALSE)
  other <- split[split$item_cbs_code_residue == 2106L, ]
  expected <- sum(other$feed_dm_t * other$value_dm / other$value) /
    sum(other$feed_dm_t)
  got <- out$residue_kgdm_kgfm[out$item_cbs_code == 2106L]
  testthat::expect_equal(got, expected)
  # Bounded by its crops' own contents, and well below the old flat 0.90.
  testthat::expect_gt(got, 0.132)
  testthat::expect_lt(got, 0.90)

  straw <- out$residue_kgdm_kgfm[out$item_cbs_code == 2105L]
  testthat::expect_equal(straw, 433 / 500)
})

testthat::test_that(".residue_feed_kgdm leaves Firewood and empty input out", {
  res <- .residue_dm_fixture() |>
    dplyr::mutate(item_cbs_code_residue = 2107L)
  out <- whep:::.residue_feed_kgdm(res)
  testthat::expect_equal(nrow(out), 0L)
  pointblank::expect_col_exists(out, "residue_kgdm_kgfm")
})

testthat::test_that(".residue_feed_kgdm refuses residues with no dry matter", {
  res <- dplyr::select(.residue_dm_fixture(), -"value_dm")
  testthat::expect_error(
    whep:::.residue_feed_kgdm(res),
    class = "whep_residue_no_dry_matter"
  )
})

testthat::test_that("residue feed availability uses the crop-mix content", {
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed,
    2010L, 203L,       2106L,          1000,
    2010L, 203L,       2591L,          1000
  )
  kgdm <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~residue_kgdm_kgfm,
    2010L, 203L,       2106L,          0.5
  )
  flat <- whep:::.build_feed_avail_national(cbs)
  mixed <- whep:::.build_feed_avail_national(cbs, residue_kgdm = kgdm)

  # 1000 t fresh at the mix's 0.5 kg DM/kg, less the 0.9 feed-loss factor.
  testthat::expect_equal(
    mixed$avail_dm_t[mixed$item_cbs_code == 2106L],
    1000 * 0.9 * 0.5
  )
  # The item's single coefficient is what the old path applied.
  coef <- whep::biomass_coefs$Product_kgDM_kgFM[
    whep::biomass_coefs$Name_biomass == "Other crop residues"
  ]
  testthat::expect_equal(
    flat$avail_dm_t[flat$item_cbs_code == 2106L],
    1000 * 0.9 * coef
  )
  # Non-residue items are untouched.
  testthat::expect_equal(
    mixed$avail_dm_t[mixed$item_cbs_code == 2591L],
    flat$avail_dm_t[flat$item_cbs_code == 2591L]
  )
})

testthat::test_that("residue feed the table does not cover is named", {
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed,
    2010L, 203L,       2106L,          1000,
    2010L, 79L,        2106L,          250
  )
  kgdm <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~residue_kgdm_kgfm,
    2010L, 203L,       2106L,          0.5
  )
  testthat::expect_warning(
    out <- whep:::.build_feed_avail_national(cbs, residue_kgdm = kgdm),
    "250 t fresh"
  )
  testthat::expect_equal(nrow(out), 2L)
})

testthat::test_that("the reshape returns residue intake to its fresh mass", {
  # Availability divided fresh feed into dry matter with the mix's content, so
  # the reshape must multiply back with the same content, or a tonne of residue
  # leaves the balance as one mass and returns as another.
  result <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed_group, ~intake_dm_t,
    2010L, 203L,       2106L,          "residues",  45
  )
  item_kgdm <- tibble::tibble(item_cbs_code = 2106L, product_kgdm_kgfm = 0.9)
  kgdm <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~residue_kgdm_kgfm,
    2010L, 203L,       2106L,          0.5
  )
  out <- whep:::.intake_to_fresh_matter(result, item_kgdm, kgdm)
  testthat::expect_equal(out$intake, 90)
  testthat::expect_equal(out$intake_dry_matter, 45)
  flat <- whep:::.intake_to_fresh_matter(result, item_kgdm)
  testthat::expect_equal(flat$intake, 50)
})
