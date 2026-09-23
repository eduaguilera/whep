# Runs `.read_production()` with every reader and every stage outside the
# yield chain stubbed, recording the years each stage was handed (whep#834).
#
# The stubbed `.compute_yields()` keeps the one property that made the real
# chain window-dependent: it fills a yield from the nearest year that has one,
# however far away. Its only anchors are 1995 and 2020, both well outside a
# 2010 window plus the read margin.
.run_stubbed_read_production <- function(start_year, end_year) {
  seen <- new.env()
  testthat::local_mocked_bindings(
    .read_cbs_production = function(years = NULL, elements = NULL) {
      if (is.null(elements)) {
        seen$cbs_window <- years
      } else {
        seen$cbs_chain <- years
        seen$cbs_elements <- elements
      }
      out <- data.table::as.data.table(.stub_fao_rows(years))
      attr(out, ".cb_extracts") <- list(fbs_new = .stub_fao_rows(years))
      out
    },
    .read_fao_crop_liv = function(years = NULL) {
      seen$fao <- years
      .stub_fao_rows(years)
    },
    .build_fodder = function(fao_crop_liv, years = NULL) {
      seen$fodder <- years
      fao_crop_liv[0, ]
    },
    .build_livestock_stocks = function(fao_combined, years = NULL) {
      seen$stocks <- years
      fao_combined[0, ]
    },
    .build_livestock_slaughter = function(fao_combined) {
      seen$slaughter <- sort(unique(fao_combined$year))
      fao_combined[0, ]
    },
    .combine_primary_raw = function(fao_combined, fao_liv_all) fao_combined,
    .compute_yields = function(primary_raw, cbs_prod_raw) {
      primary_raw |>
        dplyr::select("year", "area_code") |>
        dplyr::left_join(.stub_yield_anchors(), by = "year") |>
        whep::fill_linear(yield_c, .by = "area_code")
    },
    .assemble_production_raw = function(yield_all, stocks = NULL, ...) {
      seen$assembled <- sort(unique(yield_all$year))
      seen$stocks_assembled <- sort(unique(stocks$year))
      tibble::as_tibble(yield_all)
    },
    .prepare_historical_production = function(historical_data, years) {
      tibble::tibble()
    },
    .read_land_areas = function(years = NULL, ...) NULL,
    .read_int_yields = function(years = NULL) NULL,
    .historical_land_wide = function(land_method, years) NULL,
    .extend_historical = function(df, ...) df,
    .build_grassland = function(land_areas) tibble::tibble(),
    .production_flag_lookup = function(df) NULL,
    .add_historical_yields = function(df, int_yields) df,
    .finalise_primary = function(df) df,
    .attach_production_flags = function(df, flags) df,
    .package = "whep"
  )
  out <- suppressMessages(whep:::.read_production(start_year, end_year))
  list(out = out, seen = seen)
}

# One row per FAOSTAT year (1961-2023) inside the requested read.
.stub_fao_rows <- function(years) {
  span <- if (is.null(years)) 1961L:2023L else intersect(years, 1961L:2023L)
  tibble::tibble(year = span, area_code = 1L, value = 1)
}

.stub_yield_anchors <- function() {
  tibble::tribble(
    ~year, ~yield_c,
    1995L, 1,
    2020L, 4
  )
}
