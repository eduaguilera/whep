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

test_that("feed N lookup resolves nitrogen for feed items via bio_coefs", {
  lk <- whep:::.feed_n_content_lookup()
  expect_equal(dplyr::n_distinct(lk$item_cbs_code), nrow(lk))

  feed_items <- tibble::as_tibble(whep::items_full) |>
    dplyr::filter(!is.na(feedtype_graniv) | !is.na(feedtype_grazers))
  fl <- dplyr::filter(
    lk,
    item_cbs_code %in% as.integer(feed_items$item_cbs_code)
  )
  # bio_coefs now carries the feed byproducts; only the junk Name_biomass="0"
  # item fails to resolve.
  expect_gt(mean(!is.na(fl$feed_n_kgn_kgdm)), 0.99)

  # Non-negative; <= 0.5 accommodates urea / non-protein-N feed additives.
  vals <- lk$feed_n_kgn_kgdm[!is.na(lk$feed_n_kgn_kgdm)]
  expect_true(all(vals >= 0 & vals <= 0.5))
})

test_that("forage N default is a plausible grazed-forage value", {
  fn <- whep:::.forage_n_kgn_kgdm()
  expect_gte(fn, 0.01)
  expect_lte(fn, 0.03)
})

test_that("manure loss fractions cover the engine MMS and are in [0, 1]", {
  loss <- whep:::.manure_loss_fractions()
  expect_equal(
    dplyr::n_distinct(loss[c("mms_type", "animal_category")]),
    nrow(loss)
  )
  expect_true(all(loss$frac_gas_ms >= 0 & loss$frac_gas_ms <= 1))
  expect_true(all(loss$frac_leach_ms >= 0 & loss$frac_leach_ms <= 1))

  pick <- function(m, a) {
    loss$frac_gas_ms[loss$mms_type == m & loss$animal_category == a]
  }
  expect_equal(pick("Pasture/Range/Paddock", "Dairy Cattle"), 0)
  expect_equal(pick("Daily Spread", "Swine"), 0.07)
  expect_equal(pick("Solid Storage", "Swine"), 0.45)
  expect_equal(pick("Solid Storage", "Dairy Cattle"), 0.30)
})

test_that("every MMS x loss-category the engine can emit has a loss fraction", {
  bridge <- whep:::.species_taxonomy_bridge()
  loss <- whep:::.manure_loss_fractions()
  mms <- whep::regional_mms_distribution |>
    dplyr::filter(region == "Global", fraction > 0)

  combos <- bridge |>
    dplyr::distinct(species_gen, loss_category) |>
    dplyr::inner_join(
      dplyr::distinct(mms, species, mms_type),
      by = c("species_gen" = "species"),
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      loss,
      by = c("mms_type", "loss_category" = "animal_category")
    )
  expect_false(anyNA(combos$frac_gas_ms))
})

test_that("manure C:N is reused from bio_coefs per species x manure type", {
  cn <- whep:::.manure_cn_coefs()
  expect_true(all(
    c("species", "manure_type", "cn_ratio", "humified_c_kgc") %in% names(cn)
  ))
  expect_gt(nrow(cn), 20)
  expect_false(anyNA(cn$cn_ratio))
  expect_true(all(cn$cn_ratio > 3 & cn$cn_ratio < 60))
  expect_true(all(c("Solid", "Liquid", "Excreta") %in% cn$manure_type))
  expect_true(all(
    c("Cattle", "Pigs", "Poultry", "Sheep", "Goats", "Horses", "Rabbits") %in%
      cn$species
  ))
})

test_that("N2:N2O ratio and climate-zone cuts match verified IPCC values", {
  expect_equal(whep:::.n2_to_n2o_ratio(), 3)
  z <- whep:::.climate_zone_from_mat(c(5, 10, 12, 18, 25, NA))
  expect_equal(z, c("Cool", "Cool", "Temperate", "Temperate", "Warm", NA))
})

test_that("source registry is well-formed and uses the reliability vocabulary", {
  reg <- whep:::.manure_to_soil_sources()
  expect_true(all(
    c("source_id", "citation", "identifier", "used_for", "reliability") %in%
      names(reg)
  ))
  expect_equal(dplyr::n_distinct(reg$source_id), nrow(reg))
  expect_false(anyNA(reg$citation))
  expect_true(all(
    reg$reliability %in% c("verified", "derived", "consensus", "placeholder")
  ))
})
