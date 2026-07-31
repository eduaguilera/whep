.test_trade_items <- function() {
  tibble::tribble(
    ~item, ~Name_biomass, ~group,
    "Wheat and products", "Wheat", "Primary crops",
    "Beef and Buffalo Meat", "Cattle_meat", "Livestock products",
    "Straw", "Straw", "Crop residues",
    "Grassland", "Grass", "Grass",
    "Freshwater Fish", "Fish", "Fish",
    "Alcohol, Non-Food", "Alcohol", "Additives"
  )
}

.test_trade_biomass <- function() {
  tibble::tribble(
    ~Name_biomass,
    ~Product_kgDM_kgFM,
    ~Product_kgN_kgDM,
    ~Residue_kgDM_kgFM,
    ~Residue_kgN_kgDM,
    "Wheat", 0.87, 0.02, 0.9, 0.005,
    "Cattle_meat", 0.3, 0.04, NA, NA,
    "Straw", 0.9, 0.005, 0.9, 0.005,
    "Grass", 0.25, 0.02, 0.3, 0.015,
    "Fish", 0.25, 0.03, NA, NA,
    "Alcohol", 1.0, 0.0, NA, NA
  )
}


# .sum_provincial_net ---------------------------------------------------------

test_that(".sum_provincial_net nets exports against imported origin", {
  npd <- tibble::tribble(
    ~year, ~item, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Wheat and products", "Cropland", "Cropland", "export", 100,
    2000, "Wheat and products", "Cropland", "Outside", "population_food", 30,
    2000, "Wheat and products", "Cropland", "Cropland", "population_food", 500
  )

  out <- .sum_provincial_net(npd, character(0), character(0))

  # Domestic-to-domestic flows are neither exports nor imports.
  expect_equal(out$net_prov, 70)
})

test_that(".sum_provincial_net drops excluded boxes and items", {
  npd <- tibble::tribble(
    ~year, ~item, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Wheat and products", "Cropland", "Cropland", "export", 100,
    2000, "Freshwater Fish", "Fish", "Cropland", "export", 999,
    2000, "Straw", "Cropland", "Cropland", "export", 555
  )

  out <- .sum_provincial_net(npd, "Fish", "Straw")

  expect_equal(nrow(out), 1)
  expect_equal(out$item, "Wheat and products")
  expect_equal(out$net_prov, 100)
})

test_that(".sum_provincial_net sums provinces so internal flows cancel", {
  # A exports 100 to B, which imports it: the national net must be zero.
  npd <- tibble::tribble(
    ~year, ~item, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Wheat and products", "Cropland", "Cropland", "export", 100,
    2000, "Wheat and products", "Cropland", "Outside", "population_food", 100
  )

  out <- .sum_provincial_net(npd, character(0), character(0))

  expect_equal(out$net_prov, 0)
})

test_that(".sum_provincial_net assumes exports never carry imported origin", {
  # The case_when tests destiny == "export" first, so a re-export row would be
  # counted as an export and never as an import. No such row exists in the
  # model today; pin the assumption so it fails loudly if that changes.
  npd <- tibble::tribble(
    ~year, ~item, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Wheat and products", "Cropland", "Outside", "export", 100
  )

  out <- .sum_provincial_net(npd, character(0), character(0))

  # Documents the current behaviour: counted once, as an export.
  expect_equal(out$net_prov, 100)
})


# .convert_trade_fm_to_n -------------------------------------------------------

test_that(".convert_trade_fm_to_n applies product coefficients", {
  trade <- tibble::tribble(
    ~Year, ~Item, ~Element, ~value_fm,
    2000, "Wheat and products", "Export", 1000
  )

  out <- .convert_trade_fm_to_n(
    trade,
    .test_trade_items(),
    .test_trade_biomass()
  )

  expect_equal(out$value_n, 1000 * 0.87 * 0.02)
  expect_named(out, c("Year", "Item", "Element", "value_n"))
})

test_that(".convert_trade_fm_to_n uses residue coefficients for grass and wood", {
  trade <- tibble::tribble(
    ~Year, ~Item, ~Element, ~value_fm,
    2000, "Grassland", "Export", 1000
  )

  out <- .convert_trade_fm_to_n(
    trade,
    .test_trade_items(),
    .test_trade_biomass()
  )

  expect_equal(out$value_n, 1000 * 0.3 * 0.015)
})

test_that(".convert_trade_fm_to_n falls back to product coefficients when residue coefs are missing", {
  items <- tibble::tribble(
    ~item, ~Name_biomass, ~group,
    "Firewood", "Average wood", "Primary crops"
  )
  biomass <- tibble::tribble(
    ~Name_biomass,
    ~Product_kgDM_kgFM,
    ~Product_kgN_kgDM,
    ~Residue_kgDM_kgFM,
    ~Residue_kgN_kgDM,
    "Average wood", 0.5, 0.004, NA, NA
  )
  trade <- tibble::tribble(
    ~Year, ~Item, ~Element, ~value_fm,
    2000, "Firewood", "Export", 1000
  )

  out <- .convert_trade_fm_to_n(trade, items, biomass)

  expect_equal(out$value_n, 1000 * 0.5 * 0.004)
})


# .default_excluded_trade_items ------------------------------------------------

test_that(".default_excluded_trade_items excludes grass, residues and internal feed", {
  out <- .default_excluded_trade_items(.test_trade_items())

  expect_true(all(c("Straw", "Grassland") %in% out))
  expect_true(all(c("Acorns", "Fodder mix", "Fodder legumes") %in% out))
  expect_false("Wheat and products" %in% out)
})


# .classify_item_category -----------------------------------------------------

test_that(".classify_item_category splits livestock from crop", {
  out <- .classify_item_category(
    c("Wheat and products", "Beef and Buffalo Meat"),
    .test_trade_items()
  )

  cats <- stats::setNames(out$category, out$item)
  expect_equal(cats[["Wheat and products"]], "Crop")
  expect_equal(cats[["Beef and Buffalo Meat"]], "Livestock")
  expect_named(out, c("item", "category"))
})


# .sum_national_flows ---------------------------------------------------------

test_that(".sum_national_flows reports gross exports and imports separately", {
  nnd <- tibble::tribble(
    ~year, ~item, ~box, ~origin, ~destiny, ~mg_n,
    2000, "Wheat and products", "Cropland", "Cropland", "export", 100,
    2000, "Wheat and products", "Cropland", "Outside", "population_food", 40,
    2000, "Wheat and products", "Cropland", "Cropland", "population_food", 10
  )

  out <- .sum_national_flows(nnd, character(0), character(0))

  expect_equal(out$export, 100)
  expect_equal(out$import, 40)
})


# .combine_trade_flow_sources -------------------------------------------------

test_that(".combine_trade_flow_sources restricts the model to raw item-years", {
  national <- tibble::tribble(
    ~year, ~item, ~export, ~import,
    1900, "Wheat and products", 10, 5,
    1901, "Wheat and products", 11, 6
  )
  raw <- tibble::tribble(
    ~year, ~item, ~export, ~import,
    1900, "Wheat and products", 12, 4
  )

  out <- .combine_trade_flow_sources(national, raw, .test_trade_items())

  # Only 1900 is reported, for both sources, split into Export and Import.
  expect_setequal(out$year, 1900)
  expect_setequal(out$source, c("FAO (raw)", "WHEP model"))
  expect_setequal(out$flow, c("Export", "Import"))
  expect_equal(nrow(out), 4)
  expect_setequal(out$category, "Crop")
})


# .read_raw_trade_data --------------------------------------------------------

test_that(".read_raw_trade_data returns the packaged Spain trade extract", {
  out <- .read_raw_trade_data()

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Element", "Item", "Year", "value_fm"))
  expect_setequal(unique(out$Element), c("Export", "Import"))
  expect_type(out$Year, "integer")
  expect_false(anyNA(out$value_fm))
  expect_gt(nrow(out), 0)
  # The extract is the historical pre-FAOSTAT window.
  expect_lte(max(out$Year), 1960)
})
