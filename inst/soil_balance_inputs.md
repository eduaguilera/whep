# Soil-balance module inputs (water / carbon / nitrogen)

The gridded soil-balance modules (`build_water_balance()`,
`build_carbon_balance()`, `calculate_soc_dynamics()`, `build_nitrogen_balance()`
and their readers) consume large raster/tabular source datasets that are **not
bundled with the package** (multi-GB). Each reader locates its data through an
environment variable and **aborts with an instruction if the variable is
unset**.

You only need these for **real runs**. The examples and the test suite use small
built-in fixtures (`example = TRUE`, or injected `data = ...`), so package
checks and `?`-examples run without any of these variables set.

## Environment variables

| Variable | Read by | Expected contents |
|---|---|---|
| `WHEP_LPJML_RUN_DIR` | `read_lpjml_hydrology()`, `read_lpjml_npp()`, `build_water_balance()` | An LPJmL run output directory holding the monthly NetCDFs: `mseepage.nc`, `mtransp.nc`, `mevap.nc`, `minterc.nc`, `mprec.nc`, `mrain.nc`, `mirrig.nc`, `mrunoff.nc`, `mdischarge.nc`, `mswc.nc`, `mcft_nir.nc`. |
| `WHEP_CRU_DIR` | `read_cru_climate()` | A CRU TS directory with `cru_ts<version>.<years>.<var>.dat.nc` files (`tmp`, `pet`, `pre`, `tmn`, `tmx`, `frs`, …). Any CRU TS version is accepted; the newest present is used. |
| `WHEP_HWSD_DIR` | `read_soil_ph()`, `read_soil_hydraulic()` | HWSD soil directory with `hwsd_data.csv` and `hwsd.bil`. |
| `WHEP_LUH2_DIR` | `read_luh2_landuse()` | LUH2 v2h directory with the gridded `states.nc` product (Hurtt et al. 2020). |
| `WHEP_HYDE_DIR` | `read_hyde_population()` | HYDE historical population directory. |
| `WHEP_HANI_DIR` | `build_n_deposition()` | HaNi nitrogen-deposition directory. |
| `WHEP_WIND_DIR` | `read_lpjml_wind()` | Directory with the wind input (`wind_gswp3-w5e5.txt`). |
| `WHEP_CROP_PATTERNS_PATH` | `n_balance_spatialize.R` | Path to the crop-patterns grid. |
| `WHEP_POLITY_FRACTION_PATH` | `n_balance_spatialize.R` | Path to the per-cell polity-fraction grid. |
| `WHEP_TYPE_CROPLAND_PATH` | `n_balance_spatialize.R` | Path to the LUH2-type cropland grid. |

## Source attribution

These are original third-party products (CRU, LPJmL, LUH2, HYDE, HaNi, HWSD).
Obtain them from their original providers; the package does not redistribute
them. See the `@source` of each reader for the citation.
