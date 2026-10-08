# Offline tests for the production table Section 2 of
# inst/scripts/prepare_spatialize_all.R builds the `spatialize-country-areas`
# pin from.
#
# Issue whep#1367: production folds Sudan 276 and South Sudan 277 into bucket
# 206 in every year. Built from that fold, `.redistribute_predecessors()` split
# 206 back by LUH2 cropland share, replacing the split FAOSTAT itself reports
# from 2012 on. Section 2 now reads the un-folded table, as Section 8 does.

.source_prepare_spatialize()

.country_areas_buckets <- function() {
  tibble::tribble(
    ~bucket, ~last_year, ~member,
    206L,    2011L,      276L,
    206L,    2011L,      277L
  )
}

test_that("Section 2 keeps FAOSTAT's successor split after the bucket ends", {
  .need_spatialize_helper(".crop_reporting_areas")
  # The un-folded production: 206 up to 2011, and from 2012 both the
  # successors FAOSTAT reports and 206's 2011 area carried flat beside them.
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,    ~value,
    2011L,      206L,            "83", "ha",       100,
    2012L,      206L,            "83", "ha",       100,
    2012L,      206L,            "83", "tonnes",    90,
    2012L,      276L,            "83", "ha",        80,
    2012L,      277L,            "83", "ha",        20
  )

  out <- .crop_reporting_areas(prod, .country_areas_buckets())

  expect_equal(out$area_code[out$year == 2011L], 206L)
  # The carried bucket is gone in every unit, so 2012 is not counted twice.
  expect_setequal(out$area_code[out$year == 2012L], c(276L, 277L))
  expect_equal(sum(out$value[out$unit == "ha" & out$year == 2012L]), 100)
})

test_that("Section 2 refuses a production table in the default fold", {
  .need_spatialize_helper(".crop_reporting_areas")
  folded <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2011L,      206L,            "83", "ha",     100,
    2012L,      206L,            "83", "ha",     100
  )
  # Dropping 206 after 2011 here would delete Sudan's crops outright.
  expect_error(
    .crop_reporting_areas(folded, .country_areas_buckets()),
    class = "whep_crop_folded_production"
  )
  expect_error(
    .crop_reporting_areas(folded, .country_areas_buckets()),
    "Section 2"
  )
})

test_that("a production table that ends before the bucket is untouched", {
  .need_spatialize_helper(".crop_reporting_areas")
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit, ~value,
    2010L,      206L,            "83", "ha",     100,
    2010L,        1L,            "15", "ha",      50
  )
  expect_identical(.crop_reporting_areas(prod, .country_areas_buckets()), prod)
})
