# Regenerate inst/extdata/balances/urban_kgn_cap_reference.csv (Module C,
# Task C3): the per-capita urban-nitrogen-to-agriculture rate at each
# urban_n_reference benchmark year, i.e. that year's urban_n_gg converted to
# kg and divided by that year's Spain urban population.
#
# This is a ONE-OFF, real-data-dependent script. It is NOT sourced by any
# build pipeline (data-raw/balance_coefficients.R reads the already-committed
# CSV, it does not call this script) and it is NOT run during R CMD check or
# CI: it needs local WHEP_HYDE_DIR (real HYDE population archives) and a real
# Spain cell_polity crosswalk (lon, lat, area_code covering Spain's grid
# cells), neither of which is available in the package sandbox. Run this
# manually to regenerate the CSV when better inputs are available.
#
# PROVENANCE OF THE CURRENTLY COMMITTED CSV: this script's real HYDE path
# could not be exercised when the CSV was built (no local WHEP_HYDE_DIR in
# that environment either). The committed
# inst/extdata/balances/urban_kgn_cap_reference.csv was instead built from
# World Bank SP.URB.TOTL Spain urban population (verified figures, not
# HYDE), for exactly the urban_n_reference benchmark years the World Bank
# series covers (1990, 2000, 2008, 2016, 2018, 2020, 2022). The World Bank
# series only starts at 1960, so the 1860, 1900 and 1950 benchmark years
# have NO verified urban-population denominator and are DELIBERATELY
# ABSENT from urban_kgn_cap_reference.csv (not fabricated): build_urban_n()
# falls back to fill_linear's carry-backward for those years, which is a
# documented limitation (the 1990 rate is used for all years <= 1990), not a
# silent wrong number. Re-running this script against real HYDE data would
# let 1860/1900/1950 get their own genuine ratios and remove that
# limitation.

hyde_dir <- Sys.getenv("WHEP_HYDE_DIR")
if (!nzchar(hyde_dir)) {
  stop(
    "Set WHEP_HYDE_DIR to a local directory holding the HYDE ",
    "\"{year}AD_pop.zip\" archives before running this script."
  )
}

# A real Spain cell_polity crosswalk (lon, lat, area_code) must be supplied
# here; whep does not ship one. See R/water_balance.R / R/n_deposition.R for
# the crosswalk's schema (lon, lat, area_code[, polity_frac, cell_area_ha]).
spain_cell_polity <- stop(
  "Supply a real Spain cell_polity crosswalk (lon, lat, area_code == \"ESP\")",
  " before running this script."
)

benchmark_years <- whep::urban_n_reference$year

spain_urban_pop <- whep::read_hyde_population(
  hyde_dir = hyde_dir,
  years = benchmark_years
) |>
  dplyr::inner_join(spain_cell_polity, by = c("lon", "lat")) |>
  dplyr::filter(area_code == "ESP") |>
  dplyr::summarise(urban_pop = sum(urban_pop), .by = "year")

urban_kgn_cap_reference <- whep::urban_n_reference |>
  dplyr::inner_join(spain_urban_pop, by = "year") |>
  dplyr::transmute(
    year,
    urban_kgn_cap = urban_n_gg * 1e6 / urban_pop
  )

readr::write_csv(
  urban_kgn_cap_reference,
  here::here("inst", "extdata", "balances", "urban_kgn_cap_reference.csv")
)
