test_that("species taxonomy bridge covers every livestock_category", {
  bridge <- whep:::.species_taxonomy_bridge()
  crosswalk <- whep:::.livestock_crosswalk()

  # Unique key, no missing rows.
  expect_equal(dplyr::n_distinct(bridge$livestock_category), nrow(bridge))
  expect_setequal(
    bridge$livestock_category,
    unique(crosswalk$livestock_category)
  )
})

test_that("bridge keys join to real IPCC coefficient rows (no silent default)", {
  bridge <- whep:::.species_taxonomy_bridge()
  retention <- whep::ipcc_tier2_n_retention
  ash <- whep::ipcc_tier2_manure_ash
  excretion <- whep::ipcc_2019_n_excretion
  mms <- whep::regional_mms_distribution

  # N-retention: every bo_category resolves; values are the real ones, not the
  # 0.07 coalesce default the string-match path produced for swine/poultry.
  ret <- bridge |>
    dplyr::left_join(retention, by = c("bo_category" = "category"))
  expect_false(anyNA(ret$n_retention_frac))
  lookup <- function(cat) {
    ret$n_retention_frac[match(cat, ret$livestock_category)]
  }
  expect_equal(lookup("Cattle_milk"), 0.20)
  expect_equal(lookup("Cattle_meat"), 0.07)
  expect_equal(lookup("Pigs"), 0.30)
  expect_equal(lookup("Poultry"), 0.30)
  expect_equal(lookup("Horses"), 0.05)

  # Ash and default-Nex keyed by species_gen / excretion_category.
  ash_j <- bridge |>
    dplyr::left_join(ash, by = c("species_gen" = "category"))
  expect_false(anyNA(ash_j$ash_percent))
  expect_true(all(bridge$excretion_category %in% excretion$category))

  # Every category has at least one global MMS-distribution row.
  mms_global <- mms |> dplyr::filter(region == "Global")
  expect_true(all(bridge$species_gen %in% mms_global$species))
})

test_that("bridge species_group uses the gridding vocabulary", {
  bridge <- whep:::.species_taxonomy_bridge()
  proxy <- whep:::.default_species_proxy()
  expect_true(all(bridge$species_group %in% proxy$species_group))
})

test_that("feed N lookup resolves nitrogen for nearly all feed items", {
  lk <- whep:::.feed_n_content_lookup(whep::items_full, whep::biomass_coefs)
  expect_equal(dplyr::n_distinct(lk$item_cbs_code), nrow(lk))

  feed_items <- tibble::as_tibble(whep::items_full) |>
    dplyr::filter(!is.na(feedtype_graniv) | !is.na(feedtype_grazers))
  fl <- dplyr::filter(
    lk,
    item_cbs_code %in% as.integer(feed_items$item_cbs_code)
  )
  expect_gt(mean(!is.na(fl$feed_n_kgn_kgdm)), 0.95)

  # Non-negative; <= 0.5 accommodates urea / non-protein-N feed additives.
  vals <- lk$feed_n_kgn_kgdm[!is.na(lk$feed_n_kgn_kgdm)]
  expect_true(all(vals >= 0 & vals <= 0.5))
})

test_that("forage N default is a plausible grazed-forage value", {
  fn <- whep:::.forage_n_kgn_kgdm()
  expect_gte(fn, 0.01)
  expect_lte(fn, 0.03)
})
