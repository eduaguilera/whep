# `.earthstat_fertilizer_mapping()` maps each EarthStat crop-specific fertilizer
# raster to an `item_prod_code`. It lives at script scope in
# inst/scripts/prepare_spatialize_all.R, so this file sources the script; under
# `R CMD check` the script is not in the build (`^inst/scripts$` is in
# `.Rbuildignore`, whep#402) and these tests skip. They DO run in the
# `offline-tests` job, which calls `devtools::test()` against the checkout.
#
# Why a value check and not an existence check (#889): seven of the seventeen
# codes were wrong, and only two of the seven were detectable by existence.
# `cassava` 340 and `cotton` 274 are absent from `items_prod`, but `oilpalm`
# 217, `potato` 328, `rapeseed` 223, `sugarcane` 780 and `sunflower` 222 are all
# real item codes -- cashews, seed cotton, pistachios, jute and walnuts. A code
# that exists and names another crop joins successfully to the wrong crop, which
# is the more dangerous half. So the binding assertion below compares against
# `inst/extdata/earthstat_mapping.csv`, the same script's own answer for the
# same raster on the harvested-area layer.

.source_prepare_spatialize()

testthat::test_that("every fertilizer raster agrees with the harvested-area crosswalk", {
  .need_spatialize_helper(".earthstat_fertilizer_mapping")
  fert <- .earthstat_fertilizer_mapping()
  area <- utils::read.csv(
    system.file("extdata", "earthstat_mapping.csv", package = "whep"),
    stringsAsFactors = FALSE
  )

  shared <- merge(
    fert,
    area[, c("earthstat_name", "item_prod_code")],
    by.x = "earthstat_fert_name",
    by.y = "earthstat_name",
    suffixes = c("_fert", "_area")
  )
  # A positive control on the join itself: an empty `shared` would make the
  # comparison below pass while comparing nothing.
  testthat::expect_gt(nrow(shared), 10L)
  disagree <- shared[shared$item_prod_code_fert != shared$item_prod_code_area, ]
  testthat::expect_equal(
    nrow(disagree),
    0L,
    info = paste(
      "rasters keyed differently by the two mappings:",
      paste(disagree$earthstat_fert_name, collapse = ", ")
    )
  )
})

testthat::test_that("every fertilizer code is an item that exists", {
  .need_spatialize_helper(".earthstat_fertilizer_mapping")
  fert <- .earthstat_fertilizer_mapping()
  # Kept alongside the comparison above rather than replaced by it: a raster the
  # harvested-area crosswalk does not carry has nothing to be compared against,
  # and `barley` was exactly that case until #876 added its row.
  testthat::expect_equal(
    setdiff(fert$item_prod_code, whep::items_prod$item_prod_code),
    integer(0)
  )
})

testthat::test_that("the fertilizer mapping covers the rasters EarthStat ships rates for", {
  .need_spatialize_helper(".earthstat_fertilizer_mapping")
  fert <- .earthstat_fertilizer_mapping()
  # Asserted by name, so a raster silently disappearing from the mapping is a
  # failure here rather than a quietly smaller pattern layer.
  testthat::expect_setequal(
    fert$earthstat_fert_name,
    c(
      "barley",
      "cassava",
      "cotton",
      "groundnut",
      "maize",
      "millet",
      "oilpalm",
      "potato",
      "rapeseed",
      "rice",
      "rye",
      "sorghum",
      "soybean",
      "sugarbeet",
      "sugarcane",
      "sunflower",
      "wheat"
    )
  )
  testthat::expect_false(any(duplicated(fert$earthstat_fert_name)))
  testthat::expect_false(any(duplicated(fert$item_prod_code)))
})

testthat::test_that(".read_west_manure_local shares the one mapping", {
  .need_spatialize_helper(".earthstat_fertilizer_mapping")
  # `.read_west_manure_local()` carried an INDEPENDENT copy of the same 17-crop
  # tribble with the same seven wrong codes (#889), so fixing one would have
  # left the manure-N layer misattributing. It now reads the shared mapping and
  # only renames the key column. Asserted on the source text because the reader
  # is a closure inside `prepare_spatialize_all()` and cannot be called without
  # its L-files directory.
  src <- readLines(.prepare_spatialize_path(), warn = FALSE)
  assignments <- grep("crop_map <- ", src, value = TRUE)
  # Two readers key on this vocabulary -- the EarthStat fertilizer one and the
  # West manure one. On `main` the first already read the shared mapping and the
  # second kept its own copy, which is precisely why fixing the shared one alone
  # would have left manure N misattributed. So the invariant is that EVERY call
  # site reads it, not that there is only one.
  testthat::expect_gte(length(assignments), 2L)
  testthat::expect_true(all(grepl(
    ".earthstat_fertilizer_mapping()",
    assignments,
    fixed = TRUE
  )))
  # And no second tribble reintroduces the vocabulary: the crop names appear in
  # exactly one tribble.
  testthat::expect_equal(sum(grepl("~west_crop", src, fixed = TRUE)), 0L)
})
