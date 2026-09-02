# Crop groups for the soil carbon balance: the Spain_Hist convention as a
# label function. Offline throughout; the shipped vocabulary is package data.

.voc <- function() {
  tibble::tribble(
    ~item_prod_code, ~Herb_Woody, ~Name_biomass,
    15L, "Herbaceous", "Wheat",
    56L, "Herbaceous", "Maize",
    260L, "Woody", "Olives",
    560L, "Woody", "Grapes",
    534L, "Woody", "Peaches_nectarines",
    999L, NA_character_, NA_character_
  )
}

testthat::test_that("herbaceous crops pool, per irrigation regime", {
  out <- whep::soc_crop_group(
    c(15L, 56L, 15L, 56L),
    irrigated = c(FALSE, FALSE, TRUE, TRUE),
    vocabulary = .voc()
  )
  testthat::expect_identical(
    out,
    c(
      "cropland_rainfed_herbaceous",
      "cropland_rainfed_herbaceous",
      "cropland_irrigated_herbaceous",
      "cropland_irrigated_herbaceous"
    )
  )
})

testthat::test_that("woody crops keep their species, per irrigation regime", {
  out <- whep::soc_crop_group(
    c(260L, 260L, 560L, 534L),
    irrigated = c(FALSE, TRUE, FALSE, FALSE),
    vocabulary = .voc()
  )
  testthat::expect_identical(
    out,
    c(
      "cropland_rainfed_olives",
      "cropland_irrigated_olives",
      "cropland_rainfed_grapes",
      "cropland_rainfed_peaches_nectarines"
    )
  )
})

testthat::test_that("every label keeps the cropland prefix the balance keys on", {
  out <- whep::soc_crop_group(
    c(15L, 260L),
    irrigated = c(TRUE, FALSE),
    vocabulary = .voc()
  )
  testthat::expect_true(all(whep:::.soc_is_cropland(out)))
  testthat::expect_true(whep:::.soc_is_cropland("cropland"))
  testthat::expect_true(whep:::.soc_is_cropland("Cropland"))
  testthat::expect_false(whep:::.soc_is_cropland("grassland"))
  testthat::expect_false(whep:::.soc_is_cropland("natural"))
})

testthat::test_that("the irrigation flag recycles over the codes", {
  out <- whep::soc_crop_group(
    c(15L, 260L),
    irrigated = TRUE,
    vocabulary = .voc()
  )
  testthat::expect_identical(
    out,
    c("cropland_irrigated_herbaceous", "cropland_irrigated_olives")
  )
})

testthat::test_that("an unclassified crop aborts and is named", {
  # Pooling an unclassified crop into a default group would move carbon
  # between groups with nothing recording it.
  testthat::expect_error(
    whep::soc_crop_group(c(15L, 999L), vocabulary = .voc()),
    "999"
  )
  testthat::expect_error(
    whep::soc_crop_group(123456L, vocabulary = .voc()),
    "123456"
  )
})

testthat::test_that("species slugs are stable across spellings", {
  testthat::expect_identical(
    whep:::.soc_species_slug(c(
      "Peaches_nectarines",
      "Peaches nectarines",
      "Olives "
    )),
    c("peaches_nectarines", "peaches_nectarines", "olives")
  )
})

testthat::test_that("the shipped vocabulary classifies every area-bearing crop", {
  # items_prod_full carries Herb_Woody per item_prod_code. The three named
  # gaps are derived products with no harvested area of their own; nothing
  # that reaches the carbon-input layer may be unclassified.
  voc <- whep::items_prod_full
  primary <- voc[!is.na(voc$item_prod_code) & voc$group == "Primary crops", ]
  gaps <- primary$item_prod[
    is.na(primary$Herb_Woody) | primary$Herb_Woody == ""
  ]
  gaps <- gaps[!is.na(gaps)]
  testthat::expect_setequal(gaps, c("Palm kernels", "Palm oil", "Cotton seed"))
  # And the real vocabulary resolves a real olive and a real wheat. The
  # species slug comes from Name_biomass ("Olive"), the column Spain_Hist's
  # Cat_SOC is built from, not from the names_cats key ("Olives").
  testthat::expect_identical(
    whep::soc_crop_group(c(15L, 260L), irrigated = FALSE),
    c("cropland_rainfed_herbaceous", "cropland_rainfed_olive")
  )
})

testthat::test_that("no package file tests land_use against the literal cropland", {
  # Every cropland predicate on the carbon and nitrogen paths goes through
  # .soc_is_cropland(), so a crop group is cropland everywhere without being
  # enumerated. A literal comparison would silently drop every grouped
  # hectare from whichever term it guards. The LUH2 support itself is keyed
  # on the plain label and is exempt (n_balance_inputs.R filters `support`).
  files <- list.files(testthat::test_path("..", "..", "R"), full.names = TRUE)
  if (length(files) == 0L) {
    files <- list.files(system.file("R", package = "whep"), full.names = TRUE)
  }
  testthat::skip_if(length(files) == 0L, "package sources not available")
  hits <- purrr::map(files, \(f) {
    lines <- readLines(f, warn = FALSE)
    idx <- which(grepl(
      "str_to_lower\\(.*land_use.*\\) == \"cropland\"",
      lines
    ))
    if (length(idx)) paste0(basename(f), ":", idx) else character()
  }) |>
    unlist()
  testthat::expect_identical(hits, character())
})
