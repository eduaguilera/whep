## Bedding straw traced into the managed manure carbon and nitrogen (whep#1005)
##
## The defect: WHEP derives excreted carbon from volatile solids, which is
## right for excreta, and then stores solid manure at a carbon loss and no
## carbon gain. So the applied C:N of a solid-storage stream comes out BELOW
## the excreted C:N, for every species and every diet:
##
##   applied C:N (Solid Storage) = excreted C:N x (1 - 0.420) / (applied_n /
##                                 n_stream) = excreted C:N x 0.879
##
## while `bio_coefs` puts cattle `Solid` -- a bedded farmyard manure -- at
## 20.163 ABOVE `Excreta` 19.065. Bedding carbon is the missing term.

.bedding_intake <- function() {
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category,
    ~item_cbs_code, ~feed_quality, ~intake_dm_t,
    2020L, "203", NA, "Cattle_milk", 2513L, "high_quality", 100,
    2020L, "203", NA, "Cattle_milk", NA, "grass", 500
  )
}

.bedding_supply <- function(dm = 1000, c_t = 440, n_t = 5) {
  tibble::tibble(
    year = 2020L,
    territory = "203",
    bedding_dm_t = dm,
    bedding_c_t = c_t,
    bedding_n_t = n_t
  )
}

.bedding_applied_cn <- function(split) {
  applied <- whep::apply_management_losses(split)
  dplyr::transmute(
    applied,
    mms_type = .data$mms_type,
    cn = .data$applied_c / .data$applied_n
  )
}

testthat::test_that("bedding lifts stored solid manure above excreta C:N", {
  excretion <- whep::estimate_n_excretion(.bedding_intake())
  excreted_cn <- excretion$c_excretion / excretion$n_excretion
  split <- whep::split_manure_management(excretion)

  bare <- .bedding_applied_cn(split)
  solid_bare <- bare$cn[bare$mms_type == "Solid Storage"]
  # The defect, pinned: without bedding, storing solid manure LOWERS its C:N.
  testthat::expect_lt(solid_bare, excreted_cn)

  bedded <- split |>
    whep::add_manure_bedding(.bedding_supply()) |>
    .bedding_applied_cn()
  solid_bedded <- bedded$cn[bedded$mms_type == "Solid Storage"]
  testthat::expect_gt(solid_bedded, excreted_cn)
})

testthat::test_that("grazing manure keeps its excreta-only composition", {
  excretion <- whep::estimate_n_excretion(.bedding_intake())
  split <- whep::split_manure_management(excretion)
  bare <- .bedding_applied_cn(split)
  bedded <- split |>
    whep::add_manure_bedding(.bedding_supply()) |>
    .bedding_applied_cn()
  grazing <- function(x) x$cn[x$mms_type == "Pasture/Range/Paddock"]
  testthat::expect_equal(grazing(bedded), grazing(bare))

  # Liquid systems cannot be bedded either.
  liquid <- function(x) x$cn[x$mms_type == "Liquid/Slurry"]
  testthat::expect_equal(liquid(bedded), liquid(bare))
})

testthat::test_that("all the bedding lands, and only on litter systems", {
  # Cattle and poultry together, so both IPCC litter systems are reachable:
  # "Poultry Manure" is WHEP's deep-litter system.
  mixed <- dplyr::bind_rows(
    .bedding_intake(),
    tibble::tibble(
      year = 2020L,
      territory = "203",
      sub_territory = NA,
      livestock_category = "Poultry",
      item_cbs_code = 2513L,
      feed_quality = "high_quality",
      intake_dm_t = 80
    )
  )
  out <- mixed |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management() |>
    whep::add_manure_bedding(.bedding_supply())

  testthat::expect_equal(sum(out$n_bedding), 5)
  testthat::expect_equal(sum(out$c_bedding), 440)
  testthat::expect_equal(sum(out$dm_bedding), 1000)
  placed <- unique(out$mms_type[out$n_bedding > 0])
  testthat::expect_setequal(placed, c("Solid Storage", "Poultry Manure"))
})

testthat::test_that("with_daily_spread moves bedding but not its total", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management()
  ipcc <- whep::add_manure_bedding(split, .bedding_supply())
  wide <- whep::add_manure_bedding(
    split,
    .bedding_supply(),
    options = list(mms_bedding = "with_daily_spread")
  )
  testthat::expect_equal(sum(wide$n_bedding), sum(ipcc$n_bedding))
  solid <- function(x) sum(x$n_bedding[x$mms_type == "Solid Storage"])
  testthat::expect_lt(solid(wide), solid(ipcc))
  testthat::expect_gt(
    sum(wide$n_bedding[wide$mms_type == "Daily Spread"]),
    0
  )
  testthat::expect_true(all(wide$method_bedding_mms == "with_daily_spread"))
})

testthat::test_that("bedding N escapes the management losses (IPCC Eq 10.34)", {
  # IPCC 2019 Vol. 4 Ch. 10 Eq. 10.34 puts NbeddingMS outside (1 - FracLossMS),
  # so bedding nitrogen raises applied_n one-for-one and raises no loss term.
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management()
  bare <- whep::apply_management_losses(split)
  bedded <- whep::apply_management_losses(
    whep::add_manure_bedding(split, .bedding_supply())
  )
  testthat::expect_equal(sum(bedded$applied_n) - sum(bare$applied_n), 5)
  for (loss in c("n_volatilized", "n_leached", "n2o_direct_n", "n2_n")) {
    testthat::expect_equal(sum(bedded[[loss]]), sum(bare[[loss]]))
  }
})

testthat::test_that("bedding C:N choice moves applied carbon, not nitrogen", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management() |>
    whep::add_manure_bedding(.bedding_supply())
  same <- whep::apply_management_losses(split)
  none <- whep::apply_management_losses(
    split,
    options = list(bedding_c_loss = "none")
  )
  testthat::expect_equal(sum(none$applied_n), sum(same$applied_n))
  testthat::expect_gt(sum(none$applied_c), sum(same$applied_c))
  testthat::expect_true(all(same$method_bedding_c == "same_as_excreta"))
  testthat::expect_true(all(none$method_bedding_c == "none"))

  # Carbon is conserved either way: applied + lost = excreted + bedded.
  supplied <- sum(split$c_stream) + sum(split$c_bedding)
  testthat::expect_equal(sum(same$applied_c) + sum(same$c_lost), supplied)
  testthat::expect_equal(sum(none$applied_c) + sum(none$c_lost), supplied)
})

testthat::test_that("volatile solids stay excreta-only when manure is bedded", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management()
  bare <- whep::apply_management_losses(split)
  bedded <- whep::apply_management_losses(
    whep::add_manure_bedding(split, .bedding_supply())
  )
  testthat::expect_equal(sum(bedded$applied_vs), sum(bare$applied_vs))
  testthat::expect_equal(sum(bedded$vs_destroyed), sum(bare$vs_destroyed))
})

testthat::test_that("a split with no bedding columns is numerically untouched", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management()
  zero <- whep::add_manure_bedding(split, .bedding_supply(0, 0, 0))
  bare <- whep::apply_management_losses(split)
  filled <- whep::apply_management_losses(zero)
  cols <- c("applied_n", "applied_c", "applied_vs", "c_lost", "vs_destroyed")
  for (col in cols) {
    testthat::expect_equal(filled[[col]], bare[[col]])
  }
})

testthat::test_that("bedding with nowhere to land is reported, not dropped", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management() |>
    dplyr::filter(.data$mms_type == "Pasture/Range/Paddock")
  testthat::expect_warning(
    out <- whep::add_manure_bedding(split, .bedding_supply()),
    "no litter-using manure stream"
  )
  testthat::expect_equal(sum(out$n_bedding), 0)
})

testthat::test_that("add_manure_bedding refuses a malformed supply", {
  split <- .bedding_intake() |>
    whep::estimate_n_excretion() |>
    whep::split_manure_management()
  bad <- dplyr::select(.bedding_supply(), -"bedding_c_t")
  testthat::expect_error(
    whep::add_manure_bedding(split, bad),
    "bedding_c_t"
  )
  testthat::expect_error(
    whep::add_manure_bedding(split, .bedding_supply(), list(mms_bedding = "x")),
    class = "rlang_error"
  )
})

## ---- build_residue_bedding_supply ----------------------------------------

.bedding_destiny <- function(bedding_fraction = 0.2, dm = 1000) {
  tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = dm,
    region_krausmann = "Western Europe",
    region_un_sub = "Southern Europe",
    year = 2020L,
    territory = "203"
  ) |>
    whep::calculate_residue_destinies(bedding_fraction = bedding_fraction)
}

testthat::test_that("the bedding supply uses the bio_coefs residue coefs", {
  coefs <- whep::whep_coef_table("bio_coefs")
  wheat <- coefs[as.character(coefs$item_prod_code) == "15", ]
  out <- whep::build_residue_bedding_supply(.bedding_destiny())
  dm <- .bedding_destiny()$residue_bedding_dm_t

  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_equal(out$bedding_dm_t, dm)
  testthat::expect_equal(out$bedding_c_t, dm * wheat$residue_c_kgdm[1])
  testthat::expect_equal(out$bedding_n_t, dm * wheat$residue_n_kgdm[1])
})

testthat::test_that("a supply built from no bedding at all is refused", {
  # The identity a reconciliation would check -- the three destinies still sum
  # to the residue -- holds perfectly with bedding switched off, because zero
  # satisfies a sum. Only a supplied-check can see the absence (whep#1034).
  none <- .bedding_destiny(bedding_fraction = 0)
  expect_supplied_guard(
    identity = isTRUE(all.equal(
      none$residue_feed_dm_t +
        none$residue_bedding_dm_t +
        none$residue_burn_dm_t +
        none$residue_soil_dm_t,
      none$residue_dm_t
    )),
    guard = whep::build_residue_bedding_supply(none)
  )
})

testthat::test_that("bedding straw with no known composition is named", {
  unknown <- .bedding_destiny()
  unknown$item_prod_code <- "999999"
  testthat::expect_warning(
    out <- whep::build_residue_bedding_supply(unknown),
    "999999"
  )
  testthat::expect_identical(nrow(out), 0L)
})

testthat::test_that("build_residue_bedding_supply needs its key columns", {
  testthat::expect_error(
    whep::build_residue_bedding_supply(
      dplyr::select(.bedding_destiny(), -"territory")
    ),
    "territory"
  )
})

testthat::test_that("the supply keeps sub_territory when the input has one", {
  gridded <- dplyr::mutate(.bedding_destiny(), sub_territory = "0.25_51.75")
  out <- whep::build_residue_bedding_supply(gridded)
  testthat::expect_true(rlang::has_name(out, "sub_territory"))
  testthat::expect_identical(out$sub_territory, "0.25_51.75")
})
