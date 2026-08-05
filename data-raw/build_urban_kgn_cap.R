# Regenerate inst/extdata/balances/urban_kgn_cap_reference.csv (Module C,
# Task C3): the per-capita urban-nitrogen-to-agriculture rate at each
# urban_n_reference benchmark year, i.e. that year's urban_n_gg converted to
# kg and divided by that year's Spain urban population.
#
# This is a ONE-OFF, real-data-dependent script. It is NOT sourced by any
# build pipeline (data-raw/balance_coefficients.R reads the already-committed
# CSV, it does not call this script) and it is NOT run during R CMD check or
# CI: it needs local WHEP_HYDE_DIR (real HYDE population archives) and
# WHEP_POLITY_FRACTION_PATH (the cached cell_polity_fraction.parquet
# whep::build_cell_polity() reads), neither of which is available in the
# package sandbox. Run this manually to regenerate the CSV when better
# inputs are available.
#
# PROVENANCE OF THE COMMITTED CSV: Spain's urban population denominator is,
# for each urban_n_reference benchmark year covered by a local HYDE archive,
# whep::build_cell_polity() (Task C6's real global cell_polity crosswalk,
# lon/lat/area_code/polity_frac/cell_area_ha from
# WHEP_POLITY_FRACTION_PATH), filtered to area_code == 203L (Spain's FAOSTAT
# area code; whep::build_cell_polity() returns area_code as the integer
# FAOSTAT code, not an ISO3 string), inner-joined to
# whep::read_hyde_population() (real HYDE baseline-scenario urban population,
# WHEP_HYDE_DIR), weighted by polity_frac per cell (matching every other
# build_cell_polity() consumer in this package, e.g. R/feed_lpjml.R,
# R/n_deposition.R) to avoid overcounting population in cells that straddle
# Spain's border with Portugal, France or Morocco, and summed to a national
# total. Because build_cell_polity() covers Spain's whole grid footprint
# (unlike the earlier World Bank series,
# which only started at 1960), the 1860, 1900 and 1950 benchmark years now
# get their own genuine HYDE-derived ratio for the first time.
#
# The local HYDE baseline mirror (WHEP_HYDE_DIR) only extends through 2017
# ("NetCDF_full1500-2017" release): it has no 2018/2020/2022 archives, so
# those three benchmark years cannot get a HYDE-derived ratio from this
# local data. For exactly those three years the script keeps the
# already-committed World Bank SP.URB.TOTL-derived rate (real, verified
# figures, just not HYDE) rather than dropping them back to an undocumented
# fill_linear carry-forward gap. Every row in the final CSV is real data,
# just not all from the same source; re-running this script against a HYDE
# mirror that extends past 2017 would let those years switch to HYDE too.

hyde_dir <- Sys.getenv("WHEP_HYDE_DIR")
if (!nzchar(hyde_dir)) {
  stop(
    "Set WHEP_HYDE_DIR to a local directory holding the HYDE ",
    "\"{year}AD_pop.zip\" archives before running this script."
  )
}

spain_cell_polity <- whep::build_cell_polity() |>
  dplyr::filter(area_code == 203L)

benchmark_years <- whep::urban_n_reference$year
hyde_years <- benchmark_years[
  file.exists(file.path(hyde_dir, paste0(benchmark_years, "AD_pop.zip")))
]

spain_urban_pop <- whep::read_hyde_population(
  hyde_dir = hyde_dir,
  years = hyde_years
) |>
  dplyr::inner_join(spain_cell_polity, by = c("lon", "lat")) |>
  dplyr::filter(area_code == 203L) |>
  dplyr::summarise(
    urban_pop = sum(urban_pop * polity_frac),
    .by = "year"
  )

hyde_rows <- whep::urban_n_reference |>
  dplyr::inner_join(spain_urban_pop, by = "year") |>
  dplyr::transmute(
    year,
    urban_kgn_cap = urban_n_gg * 1e6 / urban_pop
  )

# Years with no local HYDE archive (currently 2018, 2020, 2022) keep the
# already-committed World Bank-derived rate: real data, just a different
# real source, not fabricated.
world_bank_fallback_years <- setdiff(benchmark_years, hyde_years)
existing_csv <- here::here(
  "inst",
  "extdata",
  "balances",
  "urban_kgn_cap_reference.csv"
) |>
  readr::read_csv(show_col_types = FALSE)
world_bank_rows <- existing_csv |>
  dplyr::filter(year %in% world_bank_fallback_years)

urban_kgn_cap_reference <- dplyr::bind_rows(hyde_rows, world_bank_rows) |>
  dplyr::arrange(year)

readr::write_csv(
  urban_kgn_cap_reference,
  here::here("inst", "extdata", "balances", "urban_kgn_cap_reference.csv")
)
