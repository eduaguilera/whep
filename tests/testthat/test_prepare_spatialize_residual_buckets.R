# Tests for `.check_no_residual_buckets()` in
# inst/scripts/prepare_spatialize_all.R, the Section 2 guard that keeps the
# continent and world residual buckets (901-906 "<Continent> Other", 999 RoW)
# out of `country_areas.parquet` (whep#656). The helper lives at script scope,
# so the script is sourced first, as the other prepare_spatialize tests do.

.source_prepare_spatialize()

.rb_crop_areas <- function(codes) {
  tibble::tibble(
    year = 1900L,
    area_code = as.integer(codes),
    item_prod_code = 15L,
    harvested_area_ha = 100
  )
}

test_that("the residual buckets are read from the crosswalk, all seven", {
  .need_spatialize_helper(".residual_bucket_codes")
  # Pinned by name, not only by count, so a renumbered bucket or a crosswalk
  # that stops carrying one fails here instead of passing the guard silently.
  buckets <- .residual_bucket_codes()
  expect_setequal(buckets, c(901L, 902L, 903L, 904L, 905L, 906L, 999L))
  names <- whep::polity_area_crosswalk |>
    dplyr::filter(.data$area_code %in% buckets) |>
    dplyr::distinct(.data$area_name) |>
    dplyr::pull()
  expect_true(all(c("Latin America Other", "Oceania Other", "RoW") %in% names))
})

test_that("a country table with no residual bucket passes unchanged", {
  .need_spatialize_helper(".check_no_residual_buckets")
  crop_areas <- .rb_crop_areas(c(87L, 135L, 153L))
  expect_identical(.check_no_residual_buckets(crop_areas), crop_areas)
})

test_that("a residual bucket in the country table aborts", {
  .need_spatialize_helper(".check_no_residual_buckets")
  # The June 2026 pin carried 904 Latin America Other and 906 Oceania Other in
  # place of Guadeloupe, Martinique, New Caledonia and the rest. A bucket has
  # no country_grid cell, so its area was dropped from the grid while the
  # country table still summed it: the guard must refuse that table.
  crop_areas <- .rb_crop_areas(c(87L, 904L, 906L))
  expect_error(
    .check_no_residual_buckets(crop_areas),
    class = "whep_crop_residual_bucket"
  )
  expect_error(.check_no_residual_buckets(crop_areas), "904")
})

test_that("Rest of World 999 is refused like the continent buckets", {
  .need_spatialize_helper(".check_no_residual_buckets")
  expect_error(
    .check_no_residual_buckets(.rb_crop_areas(999L)),
    class = "whep_crop_residual_bucket"
  )
})
