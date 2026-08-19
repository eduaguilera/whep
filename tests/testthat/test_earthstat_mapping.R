# `inst/extdata/earthstat_mapping.csv` and `.earthstat_fertilizer_mapping()`,
# the two EarthStat -> FAOSTAT crosswalks the spatialization reads.
#
# Both are offline artifacts: a packaged CSV and a `tribble()` at script
# scope. Neither the 902 MB EarthStat archive nor a pin is touched here.
#
# What these tests exist to stop is an omission passing for a decision. The
# crosswalk covered 169 of the archive's 175 crop layers, and one of the six
# it missed was BARLEY -- a major cereal, and about 23% of the cropland
# carbon that #363 reports as dropped for want of a crop-pattern cell. It was
# invisible precisely because a layer nobody had mapped and a layer
# deliberately left out looked identical: an absent row in one case, and in
# the other a row with a blank code and no reason given.

# The archive's own crop list, from the CROPNAME column of
# METADATA_HarvestedAreaYield175Crops_June2018.pdf, shipped inside
# HarvestedAreaYield175Crops_Geotiff.zip. Monfreda, C., N. Ramankutty and
# J. A. Foley (2008), Farming the planet: 2, Global Biogeochem. Cycles 22,
# GB1022, doi:10.1029/2007GB002947.
.earthstat_archive_layers <- function() {
  c(
    "abaca",
    "agave",
    "alfalfa",
    "almond",
    "aniseetc",
    "apple",
    "apricot",
    "areca",
    "artichoke",
    "asparagus",
    "avocado",
    "bambara",
    "banana",
    "barley",
    "bean",
    "beetfor",
    "berrynes",
    "blueberry",
    "brazil",
    "broadbean",
    "buckwheat",
    "cabbage",
    "cabbagefor",
    "canaryseed",
    "carob",
    "carrot",
    "carrotfor",
    "cashew",
    "cashewapple",
    "cassava",
    "castor",
    "cauliflower",
    "cerealnes",
    "cherry",
    "chestnut",
    "chickpea",
    "chicory",
    "chilleetc",
    "cinnamon",
    "citrusnes",
    "clove",
    "clover",
    "cocoa",
    "coconut",
    "coffee",
    "coir",
    "cotton",
    "cowpea",
    "cranberry",
    "cucumberetc",
    "currant",
    "date",
    "eggplant",
    "fibrenes",
    "fig",
    "flax",
    "fonio",
    "fornes",
    "fruitnes",
    "garlic",
    "ginger",
    "gooseberry",
    "grape",
    "grapefruitetc",
    "grassnes",
    "greenbean",
    "greenbroadbean",
    "greencorn",
    "greenonion",
    "greenpea",
    "groundnut",
    "gums",
    "hazelnut",
    "hemp",
    "hempseed",
    "hop",
    "jute",
    "jutelikefiber",
    "kapokfiber",
    "kapokseed",
    "karite",
    "kiwi",
    "kolanut",
    "legumenes",
    "lemonlime",
    "lentil",
    "lettuce",
    "linseed",
    "lupin",
    "maize",
    "maizefor",
    "mango",
    "mate",
    "melonetc",
    "melonseed",
    "millet",
    "mixedgrain",
    "mixedgrass",
    "mushroom",
    "mustard",
    "nutmeg",
    "nutnes",
    "oats",
    "oilpalm",
    "oilseedfor",
    "oilseednes",
    "okra",
    "olive",
    "onion",
    "orange",
    "papaya",
    "pea",
    "peachetc",
    "pear",
    "pepper",
    "peppermint",
    "persimmon",
    "pigeonpea",
    "pimento",
    "pineapple",
    "pistachio",
    "plantain",
    "plum",
    "popcorn",
    "poppy",
    "potato",
    "pulsenes",
    "pumpkinetc",
    "pyrethrum",
    "quince",
    "quinoa",
    "ramie",
    "rapeseed",
    "rasberry",
    "rice",
    "rootnes",
    "rubber",
    "rye",
    "ryefor",
    "safflower",
    "sesame",
    "sisal",
    "sorghum",
    "sorghumfor",
    "sourcherry",
    "soybean",
    "spicenes",
    "spinach",
    "stonefruitnes",
    "strawberry",
    "stringbean",
    "sugarbeet",
    "sugarcane",
    "sugarnes",
    "sunflower",
    "swedefor",
    "sweetpotato",
    "tangetc",
    "taro",
    "tea",
    "tobacco",
    "tomato",
    "triticale",
    "tropicalnes",
    "tung",
    "turnipfor",
    "vanilla",
    "vegetablenes",
    "vegfor",
    "vetch",
    "walnut",
    "watermelon",
    "wheat",
    "yam",
    "yautia"
  )
}

.earthstat_crosswalk <- function() {
  path <- system.file("extdata", "earthstat_mapping.csv", package = "whep")
  skip_if_not(nzchar(path) && file.exists(path))
  # read.csv, not fread: the item names carry commas and quoted prose, and a
  # fread round trip doubles embedded quotes (see CLAUDE.md).
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = character())
}

test_that("the crosswalk has a row for every archive crop layer", {
  crosswalk <- .earthstat_crosswalk()
  layers <- .earthstat_archive_layers()

  expect_length(layers, 175L)
  expect_setequal(crosswalk$earthstat_name, layers)
  expect_equal(anyDuplicated(crosswalk$earthstat_name), 0L)
})

test_that("barley is mapped, to Barley", {
  crosswalk <- .earthstat_crosswalk()

  barley <- crosswalk[crosswalk$earthstat_name == "barley", ]

  expect_equal(nrow(barley), 1L)
  expect_equal(barley$item_prod_code, 44L)
  expect_equal(barley$item_prod_name, "Barley")
})

# The invariant that keeps a blank code meaningful: a code and a reason are
# exact complements, so no row can be silently unmapped and none can carry a
# reason it does not need.
test_that("every unmapped row states why, and no mapped row does", {
  crosswalk <- .earthstat_crosswalk()

  has_code <- !is.na(crosswalk$item_prod_code) &
    nzchar(trimws(as.character(crosswalk$item_prod_code)))
  has_reason <- nzchar(trimws(crosswalk$unmapped_reason))

  expect_equal(has_reason, !has_code)
  expect_true(all(
    crosswalk$unmapped_reason[has_reason] %in%
      c("no_fao_crop_name", "unmapped")
  ))
})

test_that("every mapped item code exists in items_prod", {
  crosswalk <- .earthstat_crosswalk()
  mapped <- crosswalk[!is.na(crosswalk$item_prod_code), ]

  expect_true(all(mapped$item_prod_code %in% whep::items_prod$item_prod_code))
})

# The names are carried alongside the codes purely for readability, which is
# how they drift: nothing reads them, so a stale one is invisible.
test_that("the carried item names agree with items_prod", {
  crosswalk <- .earthstat_crosswalk()
  mapped <- crosswalk[!is.na(crosswalk$item_prod_code), ]

  expected <- whep::items_prod$item_prod_name[
    match(mapped$item_prod_code, whep::items_prod$item_prod_code)
  ]

  expect_equal(mapped$item_prod_name, expected)
})

.source_prepare_spatialize()

test_that("the fertilizer mapping resolves to real items_prod codes", {
  skip_if_not(exists(".earthstat_fertilizer_mapping", mode = "function"))
  fert <- .earthstat_fertilizer_mapping()

  unknown <- setdiff(fert$item_prod_code, whep::items_prod$item_prod_code)

  expect_equal(nrow(fert), 17L)
  # Failed before the fix on cassava 340 and cotton 274, neither of which is
  # an item_prod_code at all.
  expect_equal(unknown, integer())
})

# The two crosswalks live 900 lines apart in one file and key the same
# EarthStat names into the same code space, so the only thing keeping them
# consistent is this test. Seven of the seventeen disagreed: potato was coded
# 328 ("Seed cotton, unginned"), oilpalm 217 ("Cashew nuts, in shell"),
# rapeseed 223 ("Pistachios, in shell"), sugarcane 780 ("Jute, raw or
# retted") and sunflower 222 ("Walnuts, in shell").
test_that("the fertilizer mapping agrees with the crosswalk", {
  skip_if_not(exists(".earthstat_fertilizer_mapping", mode = "function"))
  crosswalk <- .earthstat_crosswalk()
  fert <- .earthstat_fertilizer_mapping()

  expected <- crosswalk$item_prod_code[
    match(fert$earthstat_fert_name, crosswalk$earthstat_name)
  ]

  # Every fertilizer crop is one of the archive's own layers.
  expect_false(anyNA(expected))
  expect_equal(fert$item_prod_code, as.integer(expected))
})

# Which of the 175 metadata names actually SHIP as a raster directory is a
# separate axis from whether WHEP maps them. The zip served today extracts 172:
# coir, gums and popcorn are absent from it, verified by a fresh download on
# 2026-08-19 (902,458,036 bytes). They exist as rasters in an older
# distribution, so this is the served archive being incomplete against its own
# metadata, not the crops failing to exist.
test_that("the crosswalk records which layers ship in the raster archive", {
  crosswalk <- .earthstat_crosswalk()

  expect_type(crosswalk$in_raster_archive, "logical")
  expect_false(anyNA(crosswalk$in_raster_archive))
  expect_setequal(
    crosswalk$earthstat_name[!crosswalk$in_raster_archive],
    c("coir", "gums", "popcorn")
  )
  expect_equal(sum(crosswalk$in_raster_archive), 172L)
})

# The download-side half of the barley failure. `download_monfreda.R` used to
# accept any extraction with >= 170 of the crop directories, which is how a
# 169-crop copy came to be the thing `earthstat_mapping.csv` was built from. A
# count cannot say WHICH layer is absent, and that is the only useful fact.
.monfreda_env <- function() {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "download",
    "download_monfreda.R"
  )
  if (!file.exists(path)) {
    path <- system.file(
      "scripts",
      "download",
      "download_monfreda.R",
      package = "whep"
    )
  }
  skip_if_not(nzchar(path) && file.exists(path))
  env <- new.env(parent = globalenv())
  source(path, local = env)
  env
}

.shipping_layers <- function() {
  crosswalk <- .earthstat_crosswalk()
  crosswalk$earthstat_name[crosswalk$in_raster_archive]
}

# A crop archive on disk, minus whichever layers the caller wants absent. Built
# under `tempdir()`, which R clears at session end, so no cleanup dance and no
# dependency on withr (which this package does not declare).
.fake_monfreda_dir <- function(absent = character()) {
  root <- tempfile("monfreda-")
  dir.create(root, recursive = TRUE)
  for (crop in setdiff(.shipping_layers(), absent)) {
    dir.create(file.path(root, crop))
  }
  root
}

test_that("the guard expects the layers that ship, not all 175 names", {
  env <- .monfreda_env()

  # Expecting 175 would make this warn on every correct download, which is
  # exactly how a guard comes to be ignored.
  expect_setequal(env$.monfreda_expected_crops(), .shipping_layers())
  expect_length(env$.monfreda_expected_crops(), 172L)
})

test_that("a complete extraction passes silently", {
  env <- .monfreda_env()

  root <- .fake_monfreda_dir()

  expect_length(list.dirs(root, recursive = FALSE), 172L)
  expect_equal(env$.monfreda_missing(root), character())
  expect_silent(env$.monfreda_check_complete(root))
})

test_that("the guard names the absent crop layers", {
  env <- .monfreda_env()
  absent <- c("barley", "greencorn", "hempseed")

  root <- .fake_monfreda_dir(absent)

  # 169 of 172 -- the exact state that produced the missing barley row.
  expect_length(list.dirs(root, recursive = FALSE), 169L)
  expect_setequal(env$.monfreda_missing(root), absent)
  expect_message(env$.monfreda_check_complete(root), "barley")
})

# 170 directories is where the old count guard turned on, so this is the case
# worth pinning: two absent layers used to read as a complete archive.
test_that("the guard rejects what the old count guard passed", {
  env <- .monfreda_env()
  absent <- c("barley", "wheat")

  root <- .fake_monfreda_dir(absent)

  expect_length(list.dirs(root, recursive = FALSE), 170L)
  expect_setequal(env$.monfreda_missing(root), absent)
})
