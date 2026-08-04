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
| `WHEP_LPJML_RUN_DIR` | `read_lpjml_hydrology()`, `read_lpjml_npp()`, `build_water_balance()` | An LPJmL run output directory holding the monthly NetCDFs: `mseepage.nc`, `mtransp.nc`, `mevap.nc`, `minterc.nc`, `mprec.nc`, `mrain.nc`, `mirrig.nc`, `mrunoff.nc`, `mdischarge.nc`, `mswc.nc`, `mcft_nir.nc`. **See the year-coverage warning below.** |
| `WHEP_CRU_DIR` | `read_cru_climate()` | A CRU TS directory with `cru_ts<version>.<years>.<var>.dat.nc` files (`tmp`, `pet`, `pre`, `tmn`, `tmx`, `frs`, …). Any CRU TS version is accepted; the newest present is used. |
| `WHEP_HWSD_DIR` | `read_soil_ph()`, `read_soil_hydraulic()` | HWSD **v1.2** soil directory with `hwsd_data.csv` and `hwsd.bil`. The 2023 HWSD2 release is not wired (see #343). |
| `WHEP_LUH2_DIR` | `read_luh2_landuse()` | **Fallback only** — the states grid normally comes from the `luh2_v2h_states` pin, and this variable is read only when that pin cannot be fetched. LUH2 directory with the gridded `states.nc` product (Hurtt et al. 2020). The base v2h release covers 850-2015; the annually-updated Global Carbon Budget variants extend further (the pinned payload is LUH2-GCB2022, 850-2022), so check `states.nc`'s own time axis rather than assuming 2015. Whichever source is read, its vintage is recorded on the result — see below. |
| `WHEP_HYDE_DIR` | `read_hyde_population()` | HYDE historical population directory. |
| `WHEP_HANI_DIR` | `build_n_deposition()` | HaNi nitrogen-deposition directory. |
| `WHEP_WIND_DIR` | `read_lpjml_wind()` | Directory with the wind input (`wind_gswp3-w5e5.txt`). |
| `WHEP_CROP_PATTERNS_PATH` | `n_balance_spatialize.R` | Path to the crop-patterns grid. |
| `WHEP_POLITY_FRACTION_PATH` | `n_balance_spatialize.R` | Path to the per-cell polity-fraction grid. |
| `WHEP_TYPE_CROPLAND_PATH` | `n_balance_spatialize.R` | Path to the LUH2-type cropland grid. |

## LPJmL year coverage — read this before requesting a year

An LPJmL run only has output for years its **forcing** covered. LPJmL does
not error when it is configured to simulate past the end of its climate
input: it writes all-NA fields, and derived variables (notably deep
seepage) can come out physically impossible. The readers then propagate
NaN rather than aborting, so a bad year looks like a quiet zero downstream
(issue #340).

Consequences:

- `read_lpjml_hydrology()`, `read_lpjml_npp()` and `get_soc_climate_drivers()`
  are only valid for years the run's forcing actually covered.
- Runs forced with CRU TS 3.10/3.20 (every WHEP run before the 4.09 rebuild)
  are valid **through 2009 only**, whatever their `.nc.json` metadata claims.
  `global_1901-2018_spinup_200_our_inputs` is the trap: its metadata says
  `lastyear 2018`, but it was forced with the same 2009-ending CRU files, so
  2010+ is empty and its `mseepage` is corrupt. Do not use it for drainage.
- Always confirm the run's coverage before requesting a year. The generating
  forcings and their end years are recorded in the run's
  `configurations/config_*.json`; `prepare_spatialize_all.R` (Section 9d)
  provides `report_forcing_end_years()` for the input side.

## LUH2 vintage — which v2h tree produced a result

Several LUH2 v2h trees are in circulation and they are **different products**:
the base CMIP6 release (`source_id` `LUH2 v2h`, 1166 yearly steps, 850-2015) and
the annually-reissued Global Carbon Budget variants (`UofMD-landState-LUH2-GCB2022`,
1173 steps, 850-2022). They reproduce different residual statistics, so a result
is not interpretable without knowing which one it came from.

`read_luh2_landuse()` therefore reads the registered `luh2_v2h_states` pin
first — whose payload is the GCB2022 `states.nc` — and treats `WHEP_LUH2_DIR` as
a fallback. Either way it records the vintage on the result:

```r
out <- read_luh2_landuse(resolution = "grid", years = 2000L)
get_provenance(out)
#> input_alias      input_version            input_origin input_source_id
#> luh2_v2h_states  20260701T083449Z-582d8   pin          UofMD-landState-LUH2-GCB2022
#> ... plus input_first_year / input_last_year
```

`input_version` is the pinned version and is `NA` for a local read, whose
identity is `input_source_id` instead. The calendar span comes from the file's
own time axis, so both trees read correctly without a hardcoded end year.

## Source attribution

These are original third-party products (CRU, LPJmL, LUH2, HYDE, HaNi, HWSD).
Obtain them from their original providers; the package does not redistribute
them. See the `@source` of each reader for the citation.
