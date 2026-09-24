# `regime_yield_crop_mapping` gives every primary crop item the two sources
# the irrigated:rainfed regime yield ratio needs (plan decision D15): a
# SPAM2010 crop for the level and an LPJmL CFT for the anomaly. An item with
# no row, or with a code neither source knows, would leave that crop without
# a ratio -- silently, as a join miss -- so these tests pin coverage and
# vocabulary rather than individual rows.

# The 42 SPAM2010 v2.0 crop codes, as listed in the dataset's own
# ReadMe_v2r0_Global.txt (doi:10.7910/DVN/PRFF8V).
.spam2010_crops <- function() {
  c(
    "whea",
    "rice",
    "maiz",
    "barl",
    "pmil",
    "smil",
    "sorg",
    "ocer",
    "pota",
    "swpo",
    "yams",
    "cass",
    "orts",
    "bean",
    "chic",
    "cowp",
    "pige",
    "lent",
    "opul",
    "soyb",
    "grou",
    "cnut",
    "oilp",
    "sunf",
    "rape",
    "sesa",
    "ooil",
    "sugc",
    "sugb",
    "cott",
    "ofib",
    "acof",
    "rcof",
    "coco",
    "teas",
    "toba",
    "bana",
    "plnt",
    "trof",
    "temf",
    "vege",
    "rest"
  )
}

# SPAM2010's nine multi-crop aggregates, plus "rape" (rapeseed and mustard).
.spam2010_aggregates <- function() {
  c(
    "ocer",
    "orts",
    "opul",
    "ooil",
    "ofib",
    "trof",
    "temf",
    "vege",
    "rest",
    "rape"
  )
}

# The items that must be mapped: every item cft_mapping spatializes (gridded
# land use, carbon chain) and every "Primary crops" item of items_prod_full,
# which holds every item build_primary_production() gives harvested area
# (the N balance's crop universe), less fallow, which grows no crop.
.regime_yield_universe <- function() {
  codes <- whep::items_prod_full |>
    dplyr::filter(.data$group == "Primary crops") |>
    dplyr::pull("item_prod_code")
  # One "Primary crops" row is keyed by the name "Fallow", not a code.
  primary <- suppressWarnings(as.integer(codes))
  primary <- primary[!is.na(primary) & primary != 3003L]
  sort(union(whep::cft_mapping$item_prod_code, primary))
}

# The crop stands of inst/extdata/lpjml_cft_bands.csv (spelled with
# underscores, as cft_mapping spells them): twelve crop CFTs plus "others".
# Grassland and the two bioenergy stands are not crops.
.lpjml_crop_cfts <- function() {
  path <- system.file("extdata", "lpjml_cft_bands.csv", package = "whep")
  crops <- utils::read.csv(path, stringsAsFactors = FALSE)$crop
  not_crops <- c("grassland", "biomass grass", "biomass tree")
  crops[!is.na(crops) & nzchar(crops) & !crops %in% not_crops] |>
    unique() |>
    stringr::str_replace_all(" ", "_")
}

testthat::test_that("every crop item has a SPAM crop and an LPJmL CFT", {
  map <- whep::regime_yield_crop_mapping
  universe <- .regime_yield_universe()

  testthat::expect_setequal(map$item_prod_code, universe)
  testthat::expect_false(anyDuplicated(map$item_prod_code) > 0)
  testthat::expect_false(anyNA(map$spam_crop))
  testthat::expect_false(anyNA(map$lpjml_cft))
})

testthat::test_that("SPAM crops are SPAM2010 codes", {
  map <- whep::regime_yield_crop_mapping
  tokens <- unlist(stringr::str_split(map$spam_crop, "[+|]"))

  testthat::expect_true(all(tokens %in% .spam2010_crops()))
  # Anything else would be a separator T12e cannot read.
  testthat::expect_true(all(stringr::str_detect(
    map$spam_crop,
    "^[a-z]{4}([+|][a-z]{4})*$"
  )))
})

testthat::test_that("each joined SPAM crop carries the basis that reads it", {
  map <- whep::regime_yield_crop_mapping
  pooled <- map |>
    dplyr::filter(
      stringr::str_detect(.data$spam_crop, stringr::fixed("+")),
      .data$spam_basis == "direct"
    )
  composite <- map |>
    dplyr::filter(.data$spam_basis == "composite_weighted")
  dominance <- map |>
    dplyr::filter(.data$spam_basis == "product_dominance")
  grasses <- c(638L, 639L, 645L, 651L, 996L)
  legumes <- c(640L, 641L, 643L)

  # `+` on a direct row: SPAM's own split of one FAO item, summed.
  testthat::expect_setequal(pooled$item_prod_code, c(79L, 656L))
  testthat::expect_setequal(pooled$spam_crop, c("pmil+smil", "acof+rcof"))
  # `+` on a composite row: area-weighted mean of per-crop ratios (D18, D19).
  testthat::expect_setequal(composite$item_prod_code, c(grasses, legumes))
  testthat::expect_equal(
    composite$spam_crop[composite$item_prod_code %in% grasses],
    rep("whea+barl+ocer", length(grasses))
  )
  testthat::expect_equal(
    composite$spam_crop[composite$item_prod_code %in% legumes],
    rep("bean+chic+cowp+pige+lent+opul+rest", length(legumes))
  )
  # `|`: one of two per country, by product dominance (D17).
  testthat::expect_setequal(dominance$item_prod_code, c(772L, 776L))
  testthat::expect_equal(dominance$spam_crop, c("ooil|ofib", "ooil|ofib"))
  # No other row joins codes.
  joined <- map |> dplyr::filter(stringr::str_detect(.data$spam_crop, "[+|]"))
  testthat::expect_equal(
    nrow(joined),
    nrow(pooled) + nrow(composite) + nrow(dominance)
  )
  testthat::expect_false(any(stringr::str_detect(
    pooled$spam_crop,
    stringr::fixed("|")
  )))
  testthat::expect_false(any(stringr::str_detect(
    composite$spam_crop,
    stringr::fixed("|")
  )))
})

testthat::test_that("LPJmL CFTs are crop bands the regime layer reads", {
  map <- whep::regime_yield_crop_mapping

  testthat::expect_true(all(map$lpjml_cft %in% .lpjml_crop_cfts()))
  # Where cft_mapping classifies an item, the CFT is its cft_lpjml, unchanged.
  joined <- map |>
    dplyr::inner_join(whep::cft_mapping, by = "item_prod_code")
  testthat::expect_equal(nrow(joined), nrow(whep::cft_mapping))
  testthat::expect_equal(joined$lpjml_cft, joined$cft_lpjml)
})

testthat::test_that("basis columns use the declared vocabulary", {
  map <- whep::regime_yield_crop_mapping
  basis <- c("direct", "direct_aggregate", "group_proxy", "proxy")
  spam_only <- c("composite_weighted", "product_dominance")

  testthat::expect_true(all(map$spam_basis %in% c(basis, spam_only)))
  testthat::expect_true(all(map$lpjml_basis %in% basis))
  # A SPAM aggregate is never a crop's own code, and vice versa.
  aggr <- map |> dplyr::filter(.data$spam_basis == "direct_aggregate")
  testthat::expect_true(all(aggr$spam_crop %in% .spam2010_aggregates()))
  own <- map |>
    dplyr::filter(
      .data$spam_basis == "direct",
      !stringr::str_detect(.data$spam_crop, stringr::fixed("+"))
    )
  testthat::expect_false(any(
    own$spam_crop %in% setdiff(.spam2010_aggregates(), "rape")
  ))
  # LPJmL's catch-all stand is never a crop's own CFT.
  others <- map |> dplyr::filter(.data$lpjml_cft == "others")
  testthat::expect_true(all(others$lpjml_basis != "direct"))
})

testthat::test_that("every non-direct assignment carries a rationale", {
  map <- whep::regime_yield_crop_mapping
  non_direct <- map |>
    dplyr::filter(.data$spam_basis != "direct" | .data$lpjml_basis != "direct")

  testthat::expect_gt(nrow(non_direct), 0)
  testthat::expect_true(all(
    !is.na(non_direct$rationale) & nzchar(non_direct$rationale)
  ))
  # Every open choice has been decided (plan D17-D19).
  testthat::expect_false(any(
    stringr::str_detect(map$rationale, "UNDECIDED|PLACEHOLDER"),
    na.rm = TRUE
  ))
})
